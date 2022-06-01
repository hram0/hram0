import json

MIN_DATA_REGISTERS = 2
MIN_ZETA = 1 # zeta is the gap between blocks which can detect overflows
NUM_SPECIAL_REGISTERS = 2 # pc and n

ZERO_TRIPLE = (0, 0, 0)

STATE_WAIT = 0
STATE_HALT = 1
STATE_ERROR = 2
STATE_FETCH = 3
STATE_EXEC_HLT = 4
STATE_EXEC_PUT = 5
STATE_EXEC_ADD = 6
STATE_EXEC_SUB = 7
STATE_EXEC_LOD = 8
STATE_EXEC_STO = 9
STATE_EXEC_BRN = 10
STATE_EXEC_CAL = 11
STATE_EXEC_RET = 12
STATE_EXEC_MAL = 13
STATE_EXEC_FRE = 14

TYPE_CONSTANT = 0
TYPE_REGISTER = 1
TYPE_LABEL = 2


# (Mnemonic, Sigma, Signature) descriptor triples ordered by opcode
instr_descs = [('hlt', STATE_EXEC_HLT, []),
               ('put', STATE_EXEC_PUT, [TYPE_CONSTANT, TYPE_REGISTER]),
               ('add', STATE_EXEC_ADD, [TYPE_REGISTER]*3),
               ('sub', STATE_EXEC_SUB, [TYPE_REGISTER]*3),
               ('lod', STATE_EXEC_LOD, [TYPE_REGISTER]*2),
               ('sto', STATE_EXEC_STO, [TYPE_REGISTER]*2),
               ('brn', STATE_EXEC_BRN, [TYPE_REGISTER, TYPE_LABEL]),
               ('cal', STATE_EXEC_CAL, [TYPE_LABEL]),
               ('ret', STATE_EXEC_RET, []),
               ('mal', STATE_EXEC_MAL, [TYPE_REGISTER]*2),
               ('fre', STATE_EXEC_FRE, [TYPE_REGISTER])]


def print_registers(r):
    print(', '.join(['r%d: %d' % (i, r[i]) for i in range(len(r))]))


class ProgramState(object):

    def __init__(self, r, mem, blks, e, stack, copy=True):
        assert e >= 0

        if copy:
            self.registers = list(r)
            self.memory = dict(mem)
            self.blocks = dict(blks)
            self.call_stack = list(stack)
        else:
            self.registers = r
            self.memory = mem
            self.blocks = blks
            self.call_stack = stack
        self.heap_start = e

    def copy(self):
        return ProgramState(self.registers, self.memory, self.blocks,
                            self.heap_start, self.call_stack)

    
class MachineState(object):
    code = []
    data = []
    operands = ZERO_TRIPLE

    def __init__(self, sigma, c, d, t, ps, copy=True):
        self.sigma = sigma
        self.operands = t
        if copy:
            self.code = list(c)
            self.data = list(d)
            self.program_state = ps.copy()
        else:
            self.code = c
            self.data = d
            self.program_state = ps


class Instruction(object):

    def __init__(self, opcode, mnemonic, signature):
        self.opcode = opcode
        self.mnemonic = mnemonic
        self.num_operands = len(signature)
        self.signature = signature

    def execute(self, machine_state):
        pass


    def validate(self, code, index, context=None):
        return index >= 0 and index + 1 + self.num_operands <= len(code) and \
               code[index] == self.opcode


class HRAM0(object):

    class MInstruction(Instruction):

        # func is the instruction evaluation function and vpred is the
        # validation predicate
        def __init__(self, opcode, mnemonic, signature, sigma, machine, func,
                     vpred=None):
            super(HRAM0.MInstruction, self).__init__(opcode, mnemonic,
                                                       signature)
            self._machine = machine
            self._func = func
            self._vpred = vpred
            self.sigma = sigma

        def execute(self, machine_state):
            return self._func(machine_state)

        def validate(self, code, index, context=None):
            if not super(HRAM0.MInstruction, self).validate(code, index,
                                                            context):
                return False
            
            if self._vpred is not None:
                return self._vpred(code, index, context)

            i = 0
            valid = True
            while valid and i < self.num_operands - 1:
                reg = code[index + 1 + i]
                valid = reg >= -2 and reg < self._machine.num_registers
                i += 1

            if valid and self.num_operands > 0:
                last_operand = code[index + self.num_operands]
                valid = last_operand >= 0 and \
                    last_operand < self._machine.num_data_registers
            return valid
    
    def __init__(self, params):
        # num_data_registers == rho and zeta is the size of empty
        # region at end of allocated block to detect overflows

        assert 'rho' in params and 'zeta' in params

        num_data_registers = params['rho']
        zeta = params['zeta']

        assert num_data_registers >= MIN_DATA_REGISTERS
        assert zeta >= MIN_ZETA

        self.zeta = zeta        
        self.num_data_registers = num_data_registers
        self.num_registers = num_data_registers + NUM_SPECIAL_REGISTERS
        self._pc_index = num_data_registers
        self._n_index = num_data_registers + 1
        initial_r = [0] * self.num_registers
        initial_ps = ProgramState(initial_r, {}, {}, 0, [], copy=False)
        self.state = MachineState(STATE_WAIT, [], [], ZERO_TRIPLE, initial_ps,
                                  copy=False)

        self.instructions = []
        self.instructions_by_sigma = dict()
        
        opcode = 0
        for desc in instr_descs:
            (mnemonic, sigma, signature) = desc

            fname = '_exec_' + mnemonic
            assert hasattr(self, fname)
            func = getattr(self, fname)

            vpred_name = '_validate_' + mnemonic
            vpred = None
            if hasattr(self, vpred_name):
                vpred = getattr(self, vpred_name)

            instr = HRAM0.MInstruction(opcode, mnemonic, signature, sigma,
                                         self, func, vpred)
            self.instructions.append(instr)
            self.instructions_by_sigma[sigma] = instr
            
            opcode += 1


    def advance(self):
        sigma = self.state.sigma

        if sigma in [STATE_WAIT, STATE_HALT, STATE_ERROR]:
            return False

        if sigma is STATE_FETCH:
            self._fetch()
        elif sigma in self.instructions_by_sigma:
            instr = self.instructions_by_sigma[sigma]
            self.state = instr.execute(self.state)
        return True

    def validate_code(self, code):
        # valid_targets is the set of addresses that are instruction boundaries
        # and therefore valid branch/call targets
        valid_targets = set()

        address = 0
        len_code = len(code)
        while address < len_code:
            opcode = code[address]
            if opcode < 0 or opcode >= len(self.instructions):
                return False

            valid_targets.add(address)
            address += 1 + self.instructions[opcode].num_operands
        if address != len_code:
            return False
        valid_targets.add(address)

        # Now we do another pass and validate each instruction
        address = 0
        while address < len_code:
            opcode = code[address]
            instr = self.instructions[opcode]
            if not instr.validate(code, address, valid_targets):
                return False
            address += 1 + instr.num_operands
        return True

    def load(self, code, data):
        if not self.validate_code(code):
            raise 'Invalid program'
        initial_r = [0] * self.num_registers
        initial_ps = ProgramState(initial_r, {}, {}, 0, [], copy=False)

        self.state = MachineState(STATE_WAIT, code, data, ZERO_TRIPLE,
                                  initial_ps)

    def run(self, inp):
        n = len(inp)
        init_mem_contents = self.state.data + inp
        e = len(init_mem_contents)        
        M = {i:init_mem_contents[i] for i in range(e)}        
        B = dict()
        r = [0] * self.num_registers
        r[self._n_index] = n
        ps = ProgramState(r, M, B, e, [], copy=False)
        self.state = MachineState(STATE_FETCH, self.state.code,
                                  self.state.data, ZERO_TRIPLE, ps, copy=False)



    ###########################################################################
    ## Private
    ###########################################################################

    def _fetch(self):
        ms = self.state
        assert ms.sigma is STATE_FETCH

        ps = ms.program_state
        r = ps.registers

        pc = r[self._pc_index]
        assert pc >= 0 and pc <= len(ms.code)

        if pc == len(ms.code):
            self.state = MachineState(STATE_HALT, ms.code, ms.data,
                                      ZERO_TRIPLE, ps, copy=False)
            return

        opcode = ms.code[pc]
        assert opcode >= 0 and opcode < len(self.instructions)

        instr = self.instructions[opcode]
        assert pc + 1 + instr.num_operands <= len(ms.code)
        
        operands_ = [0, 0, 0]
        for i in range(instr.num_operands):
            operands_[i] = ms.code[pc + 1 + i]

        pc_ = pc + 1 + instr.num_operands
        r_ = list(r)
        r_[self._pc_index] = pc_
        ps_ = ProgramState(r_, ps.memory, ps.blocks, ps.heap_start,
                           ps.call_stack, copy=False)
        self.state = MachineState(instr.sigma, ms.code, ms.data, operands_,
                                  ps_, copy=False)
        
            
        
    
    def _exec_hlt(self, ms):
        return MachineState(STATE_HALT, ms.code, ms.data, ZERO_TRIPLE,
                            ms.program_state, copy=False)

    def _exec_put(self, ms):
        ps = ms.program_state
        r = ps.registers
        r_ = list(r)    
        t = ms.operands
        
        r_[t[1]] = t[0]
        ps_ = ProgramState(r_, ps.memory, ps.blocks, ps.heap_start,
                           ps.call_stack, copy=False)
        
        return MachineState(STATE_FETCH, ms.code, ms.data, ZERO_TRIPLE,
                            ps_, copy=False)


    def _validate_put(self, code, index, context):
        dest_operand = code[index + 2]
        return dest_operand >= 0 and dest_operand < self.num_data_registers

    def _exec_add(self, ms):
        ps = ms.program_state
        r = ps.registers
        r_ = list(r)
        t = ms.operands
            
        r_[t[2]] = r[t[0]] + r[t[1]]
        ps_ = ProgramState(r_, ps.memory, ps.blocks, ps.heap_start,
                           ps.call_stack, copy=False)    

        return MachineState(STATE_FETCH, ms.code, ms.data, ZERO_TRIPLE,
                            ps_, copy=False)


    def _exec_sub(self, ms):
        ps = ms.program_state
        r = ps.registers
        r_ = list(r)    
        t = ms.operands
            
        r_[t[2]] = r[t[1]] - r[t[0]]
        ps_ = ProgramState(r_, ps.memory, ps.blocks, ps.heap_start,
                           ps.call_stack, copy=False)    

        return MachineState(STATE_FETCH, ms.code, ms.data, ZERO_TRIPLE,
                            ps_, copy=False)


    def _exec_lod(self, ms):
        ps = ms.program_state
        r = ps.registers
        r_ = list(r)
        M = ps.memory
        B = ps.blocks
        t = ms.operands
        sigma_ = STATE_ERROR

        if r[t[0]] in M:
            r_[t[1]] = M[r[t[0]]]
            sigma_ = STATE_FETCH
        else:
            for (start, end) in B.items():
                if r[t[0]] >= start and r[t[0]] < end:
                    r_[t[1]] = 0
                    sigma_ = STATE_FETCH


        ps_ = ProgramState(r_, M, ps.blocks, ps.heap_start, ps.call_stack,
                           copy=False)    
            
        return MachineState(sigma_, ms.code, ms.data, ZERO_TRIPLE, ps_,
                            copy=False)


    def _exec_sto(self, ms):
        ps = ms.program_state
        r = ps.registers
        M = ps.memory
        B = ps.blocks
        M_ = dict(M)
        t = ms.operands
        sigma_ = STATE_ERROR

        if r[t[1]] in M:
            M_[r[t[1]]] = r[t[0]]
            sigma_ = STATE_FETCH
        else:
            for (start, end) in B.items():
                if r[t[1]] >= start and r[t[1]] < end:
                    M_[r[t[1]]] = r[t[0]]
                    sigma_ = STATE_FETCH


        ps_ = ProgramState(r, M_, ps.blocks, ps.heap_start, ps.call_stack,
                           copy=False)

            
        return MachineState(sigma_, ms.code, ms.data, ZERO_TRIPLE, ps_,
                            copy=False)


    def _exec_brn(self, ms):
        ps = ms.program_state
        r = ps.registers
        r_ = list(r)
        t = ms.operands

        if r[t[0]] < 0:
            r_[self._pc_index] = t[1]
    
        ps_ = ProgramState(r_, ps.memory, ps.blocks, ps.heap_start,
                           ps.call_stack, copy=False)    
    
        return MachineState(STATE_FETCH, ms.code, ms.data, ZERO_TRIPLE, ps_,
                            copy=False)

    def _validate_brn(self, code, index, context):
        valid_targets = context
        cond_operand = code[index + 1]
        target_operand = code[index + 2]
        return cond_operand >= -2 and cond_operand < self.num_registers and \
            target_operand in valid_targets

    def _exec_cal(self, ms):
        ps = ms.program_state
        r = ps.registers
        r_ = list(r)
        s = ps.call_stack
        s_ = list(s)    
        t = ms.operands

        s_.append(r[self._pc_index])
        r_[self._pc_index] = t[0]
    
        ps_ = ProgramState(r_, ps.memory, ps.blocks, ps.heap_start, s_,
                           copy=False)    
    
        return MachineState(STATE_FETCH, ms.code, ms.data, ZERO_TRIPLE, ps_,
                            copy=False)

    def _validate_cal(self, code, index, context):
        valid_targets = context
        target_operand = code[index + 1]
        return target_operand in valid_targets

    

    def _exec_ret(self, ms):
        ps = ms.program_state
        r = ps.registers
        r_ = list(r)
        s = ps.call_stack
        s_ = list(ps.call_stack)
        t = ms.operands
        sigma_ = STATE_HALT

        if len(s) > 0:
            a = s_.pop()
            r_[self._pc_index] = a
            sigma_ = STATE_FETCH
    
        ps_ = ProgramState(r_, ps.memory, ps.blocks, ps.heap_start, s_,
                           copy=False)    
    
        return MachineState(sigma_, ms.code, ms.data, ZERO_TRIPLE, ps_,
                            copy=False)


    def _exec_mal(self, ms):
        ps = ms.program_state
        r = ps.registers
        r_ = list(r)
        M = ps.memory
        M_ = dict(M)
        B = ps.blocks
        B_ = dict(B)
        e = ps.heap_start
        e_ = e
        t = ms.operands

        if r[t[0]] > 0:
            e_ = e + r[t[0]] + self.zeta
            B_[e] = e + r[t[0]]
            #M_.update({i:0 for i in range(e, e_)})
            r_[t[1]] = e

    
        ps_ = ProgramState(r_, M_, B_, e_, ps.call_stack, copy=False)    
    
        return MachineState(STATE_FETCH, ms.code, ms.data, ZERO_TRIPLE, ps_,
                            copy=False)

    def _exec_fre(self, ms):
        ps = ms.program_state
        r = ps.registers
        M = ps.memory
        M_ = dict(M)
        B = ps.blocks
        B_ = dict(B)
        t = ms.operands

        if r[t[0]] in B:
            del B_[r[t[0]]]
            for i in range(r[t[0]], B[r[t[0]]]):
                if i in M:
                    del M_[i]
    
        ps_ = ProgramState(r, M_, B_, ps.heap_start, ps.call_stack,
                           copy=False)    
    
        return MachineState(STATE_FETCH, ms.code, ms.data, ZERO_TRIPLE, ps_,
                            copy=False)

class Program(object):

    def __init__(self, code, data, encode_func, decode_func):
        self.code = list(code)
        self.data = list(data)
        self._encode = encode_func
        self._decode = decode_func

    def encode_input(self, x):
        return self._encode(x)

    def decode_output(self, M, n):
        return self._decode(M, n)


class DynamicMemory(object):

    class Iterator(object):

        def __init__(self, dmem):
            self._dmem = dmem
            self._addr = 0
            self._block_i = None

        def __iter__(self):
            return self


        def __next__(self):
            if self._addr < self._dmem.size:
                if self._addr < self._dmem.static_data_size:
                    a = self._addr
                    self._addr += 1
                    return (a, self._dmem[a])

                if self._addr == self._dmem.static_data_size:
                    self._block_i = 0
                    block = self._dmem.blocks[self._block_i]
                    start, end = block
                    self._addr = start
                elif self._addr == self._dmem.blocks[self._block_i][1]:
                    self._block_i += 1
                    block = self._dmem.blocks[self._block_i]
                    start, end = block
                    self._addr = start                    

                a = self._addr
                self._addr += 1
                return (a, self._dmem[a])
            else:
                raise StopIteration()
                    

    def __init__(self, memory, static_data_size, blocks):
        self._memory = memory
        self.static_data_size = static_data_size
        self.blocks = list(blocks.items())
        self.blocks.sort(key=lambda b: b[0])

        size = 0
        for (start, end) in blocks.items():
            if end > size:
                size = end

        self.size = size
        


    def __getitem__(self, addr):
        if addr in self._memory:
            return self._memory[addr]

        for (start, end) in self.blocks:
            if addr >= start and addr < end:
                return 0

        return None

    def __contains__(self, addr):
        if addr in self._memory:
            return True

        for (start, end) in self.blocks:
            if addr >= start and addr < end:
                return True

        return False
    
    
    def __iter__(self):
        return DynamicMemory.Iterator(self)


class Executor(object):

    def __init__(self, machine):
        self.program = None
        self._machine = machine

    def execute(self, program, inp):
        if self.program != program:
            self._machine.load(program.code, program.data)
            self.program = program
        x = self.program.encode_input(inp)
        n = len(x)
        d = len(program.data) + n # size of static data

        self._machine.run(x)
        nstates = 0
        while self._machine.advance():
            nstates += 1

        ms = self._machine.state
        if ms.sigma is STATE_ERROR:
            raise 'Program terminated in error: bad memory access'

        assert ms.sigma is STATE_HALT
        ps = ms.program_state
        mem = ps.memory
        dmem = DynamicMemory(mem, d, ps.blocks)
        y = self.program.decode_output(dmem, n)

        return y


HRAM0S_RHO = 14
HRAM0S_ZETA = 10
HRAM0S_PARAMS = {'rho':HRAM0S_RHO, 'zeta':HRAM0S_ZETA}

