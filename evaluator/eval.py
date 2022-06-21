# TODO: much refactoring needed; areas of untidy sprawling code.

import sys
import hram
import json
from hram import HRAM0


def print_menu(exit_cmds, halted=False):
    print("Menu\n--------------------------")
    print("m - Print menu\n")

    if not halted:
        print("b - Set breakpoint\n")
        print("i - Show next instruction\n")
        print("u - Unset breakpoint\n")
        print("l - List breakpoints\n")

    for (cmd, desc) in exit_cmds.items():
        print("%s - %s\n" % (cmd, desc))
    print("r0 | r1 | r2... etc.| pc | n - Print register value\n")
    print("0 | 1 | 2 ... (any integer) - Print contents of memory address\n")
        
            
    

def inspect_loop(machine, exit_cmds={'q':'Quit'}, breakpoints=set()):
    state = machine.state
    ps = state.program_state
    static_data_size = len(state.data) + ps.registers[-1]
    M = hram.DynamicMemory(ps.memory, static_data_size, ps.blocks)

    halted = machine.state.sigma in [hram.STATE_HALT, hram.STATE_ERROR]
    if not halted:
        print("%d:\n\t%s" % (machine.state.program_state.registers[-2],
                             machine.disassemble_next_instruction()))
    s = input(">: ").strip().lower()
    while s not in exit_cmds:

        # Certain options are only handled if machine halted
        done = False
        if not halted:
            if s == 'i':
                print("%d:\n\t%s" % (machine.state.program_state.registers[-2],
                                     machine.disassemble_next_instruction()))
                done = True
            elif s == 'b':
                addr = input('Breakpoint address: ')
                if not addr.isdecimal():
                    print('Invalid address')            
                else:
                    breakpoints.add(int(addr))
                done = True
            elif s == 'u':
                addr = input('Unset breakpoint at address: ')
                if not addr.isdecimal():
                    print('Invalid address')
                elif int(addr) in breakpoints:
                    breakpoints.remove(int(addr))
                else:
                    print('No breakpoint found at address %d' % addr)
                done = True
            elif s == 'l':
                addrs = list(breakpoints)
                addrs.sort()
                for addr in addrs:
                    print(addr)
                done = True

        # If we have not yet handled an option (i.e. done == False) then
        # see if the remining options have been selected
        if not done:
            if s == 'm':
                print_menu(exit_cmds, halted)
            elif s.isdecimal():
                a = int(s)
                if a < 0 or a not in M:
                    print("Address %d is invalid" % a)
                else:
                    print(M[a])
            elif len(s) > 0 and (s[0] == 'r' and s[1:].isdecimal()) or \
                 s == 'pc' or s == 'n':
                regn = 0
                if s == 'n':
                    regn = -1
                elif s == 'pc':
                    regn = -2
                else:
                    regn = int(s[1:])
                print(ps.registers[regn])
            else:
                print('Invalid Input')
        s = input(">: ").strip().lower()
    return s


if __name__ == "__main__":
    
    machine = HRAM0(hram.HRAM0S_PARAMS)

    encode = lambda x: [int(s) for s in x.split(',') if len(s) > 0]
    decode = lambda M, n: M

    if len(sys.argv) < 3:
        print("usage: python3 %s <cmd> <input-file>" % sys.argv[0])
        exit(1)

    cmd = sys.argv[1]
    in_filename = sys.argv[2]
    
    with open(in_filename) as in_f:
        obj = json.load(in_f)
        assert 'code' in obj
        code = obj['code']
        data = []
        if 'data' in obj:
            data = obj['data']

        prog = hram.Program(code, data, encode, decode)

        if not machine.validate_code(prog.code):
            print('Invalid program')
            exit(1)
        
        if cmd == 'run':
            x_str = input("Program Input (x) - comma-separated array of integers: ").strip()
            x = encode(x_str)
            n = len(x)    
            
            executor = hram.Executor(machine)

            M = executor.execute(prog, x_str)
            inspect_loop(machine)
        elif cmd == 'step':
            x_str = input("Program Input (x) - comma-separated array of integers: ").strip()
            x = encode(x_str)
            n = len(x)    
            
            machine.load(prog.code, prog.data)

            d = len(prog.data) + n # size of static data
            machine.run(x)
            do_quit = False
            cmds = {'s':'Step (advance)',
                    'c':'Continue execution until breakpoint/halt',
                    'q':'Quit'
                    }
            try:
                cmd = None
                at_breakpoint = False
                breakpoints = set()
                while not do_quit:
                    cmd = inspect_loop(machine, cmds, breakpoints)
                    if cmd == 's':
                        do_quit = not machine.step_cycle()
                    elif cmd == 'c':
                        while machine.step_cycle():
                            ps = machine.state.program_state
                            pc = ps.registers[-2]
                            if pc in breakpoints:
                                at_breakpoint = True
                                break
                        do_quit = not at_breakpoint
                    elif cmd == 'q':
                        do_quit = True
                if cmd != 'q':
                    if machine.state.sigma is hram.STATE_HALT:
                        print('Halted')
                    else:
                        print('Error state')
                    inspect_loop(machine, {'q':'Quit'})
            except hram.ExecutionException as e:
                print(e)
        
        elif cmd == 'disasm':
            print("BEGIN DATA")
            print("dat, %d\t%s" % (len(data),
                                    ".".join([str(elem) for elem in data])))
            print("END DATA")
            print("BEGIN CODE")
            for address in machine.code_offsets(code):
                print('%d:\n\t%s' % (address, machine.disassemble_instruction(code, address)))
            print("END CODE")
