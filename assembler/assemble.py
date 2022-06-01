#!/usr/bin/python3

import sys
import hram
from hram import TYPE_CONSTANT, TYPE_REGISTER, TYPE_LABEL
import re
import json


TOKEN_CONSTANT = 0
TOKEN_DATA_REGISTER = 1
TOKEN_SPECIAL_REGISTER = 2
TOKEN_IDENT = 3
TOKEN_MNEMONIC = 4
TOKEN_KEYWORD = 5


OPERAND_CONSTANT = 0
OPERAND_REGISTER = 1
OPERAND_SYMBOL = 2

OP_INSTRUCTION = 0
OP_MACRO = 1

class AssemblerException(Exception):
    def __init__(self, error_str):
        self.error_str = error_str

class ResolverException(Exception):
    def __init__(self, error_str):
        self.error_str = error_str

class DataVar(object):

    def __init__(self, name, size, data, index):
        self.name = name
        self.size = size
        self.data = data
        self.index = index

class Operand(object):

    def __init__(self, operand_type):
        self.operand_type = operand_type

    def eval(self):
        pass

    def encode(self):
        t, v = self.eval()
        return v


class Constant(Operand):

    def __init__(self, value):
        super(Constant, self).__init__(OPERAND_CONSTANT)
        self.value = value

    def eval(self):
        return (TYPE_CONSTANT, self.value)

class Symbol(Operand):

    def __init__(self, label, resolver=None):
        super(Symbol, self).__init__(OPERAND_SYMBOL)
        self.symbol_str = label
        self.resolver = resolver

    def eval(self):
        return self.resolve()

    def resolve(self):
        if self.resolver is None:
            error_str = 'Failed to resolve label %s'  % self.symbol_str
            raise ResolverException(error_str)
        else:
            return self.resolver(self.symbol_str)
        

class Register(Operand):

    def __init__(self, reg_num):
        super(Register, self).__init__(OPERAND_REGISTER)
        self.number = reg_num

    def eval(self):
        return (TYPE_REGISTER, self.number)


class Operation(object):

    def __init__(self, op_type, operands):
        self.op_type = op_type
        self.operands = operands

    def encode(self):
        pass


class InstructionOp(Operation):

    def __init__(self, instr, operands):
        super(InstructionOp, self).__init__(OP_INSTRUCTION, operands)
        self.instruction = instr

    def encode(self):
        encoding = []
        encoding.append(self.instruction.opcode)

        for operand in self.operands:
            encoding.append(operand.encode())

        return encoding


class MacroOp(Operation):

    def __init__(self, macro, operands):
        super(MacroOp, self).__init__(OP_MACRO, operands)
        self.macro = macro
        


class Token(object):

    def __init__(self, t_type, value):
        self.token_type = t_type
        self.value = value

    def __eq__(self, other):
        return self.token_type == other.token_type and \
            self.value == other.value

class Block(object):

    def __init__(self, ops, labels):
        self.ops = ops
        self.labels = labels

    def is_reduced(self):
        return all([op.op_type is OP_INSTRUCTION for op in self.ops])

    def encode(self):
        encoding = []
        for op in self.ops:
            encoding.extend(op.encode())
        return encoding

class Macro(object):

    def __init__(self, name, arity, block):
        self.name = name
        self.arity = arity
        self.block = block

def is_data_register(token):
    return token.token_type is TOKEN_DATA_REGISTER

def is_special_register(token):
    return token.token_type is TOKEN_SPECIAL_REGISTER

def is_register(token):
    return is_data_register(token) or is_special_register(token)

def is_mnemonic(token, mnemonic=None):
    return token.token_type is TOKEN_MNEMONIC \
        and (mnemonic is None or token.value == mnemonic)

def is_constant(token):
    return token.token_type is TOKEN_CONSTANT

def is_ident(token, ident=None):
    return token.token_type is TOKEN_IDENT \
        and (ident is None or token.value == ident)

def is_operand(token):
    return is_constant(token) or is_ident(token) or is_register(token)

def is_keyword(token, keyword=None):
    return token.token_type is TOKEN_KEYWORD \
        and (keyword is None or token.value == keyword)

def match_kw(tokens, keywords):
    return len(tokens) >= len(keywords) and \
        tokens[:len(keywords)] == [Token(TOKEN_KEYWORD, kw) for kw in keywords]

def is_target(token):
    return is_ident(token) or (is_constant(token) and token.value >= 0)


def match_type(ty, operand):
    mapping = {OPERAND_CONSTANT:TYPE_CONSTANT,
               OPERAND_REGISTER:TYPE_REGISTER}

    if operand.operand_type in mapping:
        return mapping[operand.operand_type] == ty

    if operand.operand_type is OPERAND_SYMBOL:
        (t, v) = operand.resolve()
        return t == ty

    return False

    
class Assembler(object):
    class AInstruction(hram.Instruction):
        def __init__(self, opcode, mnemonic, signature,
                     parse_pred=None):
            super(Assembler.AInstruction, self).__init__(opcode, mnemonic,
                                                         signature)
            self._parse_pred = parse_pred


        def parse(self, tokens):
            if len(tokens) < 1 + self.num_operands or \
               not is_mnemonic(tokens[0], self.mnemonic):
                return False

            if self._parse_pred is not None:
                return self._parse_pred(tokens)

            return all([is_ident(t) or is_register(t) for t in tokens[1:-1]] \
              + [is_ident(t) or is_data_register(t) for t in tokens[1:][-1:]])

        def extract_operands(self, tokens):
            operands = []
            for token in tokens:
                operand = None
                if is_constant(token):
                    operand = Constant(token.value)
                elif token.token_type is TOKEN_IDENT:
                    operand = Symbol(token.value)
                elif is_data_register(token) or is_special_register(token):
                    operand = Register(token.value)
                else:
                    raise AssemblerException('Unknown operand')
                operands.append(operand)
            return operands

    class AMacro(Macro):
        def __init__(self, name, arity, block):
            super(Assembler.AMacro, self).__init__(name, arity, block)


        def parse(self, tokens):
            if len(tokens) < 1 + self.arity or \
               not is_ident(tokens[0], self.name):
                return False

            return all([is_operand(t) for t in tokens[1:]])

        def extract_operands(self, tokens):
            operands = []
            for token in tokens:
                operand = None
                if is_constant(token):
                    operand = Constant(token.value)
                elif token.token_type is TOKEN_IDENT:
                    operand = Symbol(token.value)
                elif is_data_register(token) or is_special_register(token):
                    operand = Register(token.value)
                else:
                    raise AssemblerException('Unknown operand')
                operands.append(operand)
            return operands
        
            

    def __init__(self, params):
        num_data_registers = params['rho']
        assert num_data_registers >= hram.MIN_DATA_REGISTERS

        self.num_data_registers = num_data_registers
        self.num_registers = num_data_registers + hram.NUM_SPECIAL_REGISTERS
        self._pc_index = num_data_registers
        self._n_index = num_data_registers + 1
        self._constant_regex = re.compile('^(\-)?\d+$')
        self._register_regex = re.compile('^[rR]\d+$')
        self._ident_regex = re.compile('^&?([a-zA-Z0-9_]+)(\[(\d+)\])?$')
        
        self.instructions = dict()
        self.keywords = ['begin', 'end', 'code', 'macro', 'data', 'constants']
        self._global_labels = dict()
        
        opcode = 0
        for desc in hram.instr_descs:
            (mnemonic, sigma, signature) = desc

            pred_name = '_parse_' + mnemonic
            parse_pred = None
            if hasattr(self, pred_name):
                parse_pred = getattr(self, pred_name)

            instr = Assembler.AInstruction(opcode, mnemonic, signature,
                                           parse_pred)
            self.instructions[mnemonic] = instr
            
            opcode += 1


    
    def assemble(self, f):
        tokens_list = []
        line_no = 0
        for line in f:
            line = line.strip()
            if len(line) > 0 and not line.startswith('#'):
                parts = line.split('#')
                tokens = self._tokenize(parts[0])
                tokens_list.append(tokens)
            line_no += 1

        self._macros = dict()
        self._data = []
        self._global_vars = dict()
        self._constants = dict()

        begin_index = 0

        section, sec_attrs, end_index = self._extract_section(tokens_list,
                                                              begin_index)
        while section != 'code' and end_index < len(tokens_list):
            if section == 'data':
                if len(self._global_vars) > 0:
                    raise AssemblerException("Cannot have more than one data section")
                d_tokens_list = tokens_list[begin_index + 1:end_index]
                
                self._data, self._global_vars = self._parse_data(d_tokens_list)
            elif section == 'constants':
                if len(self._constants) > 0:
                    raise AssemblerException("Cannot have more than one constants section")
                c_tokens_list = tokens_list[begin_index + 1:end_index]
                
                array, self._constants = self._parse_data(c_tokens_list)
                    
            
            elif section == 'macro':
                m_tokens_list = tokens_list[begin_index + 1:end_index]
                macro = self._parse_macro(m_tokens_list, sec_attrs)
                self._macros[macro.name] = macro
            else:
                raise AssemblerException("Unknown section type")

            begin_index = end_index + 1
            section, sec_attrs, end_index = self._extract_section(tokens_list,
                                                                  begin_index)
        if section != 'code':
            raise AssemblerException("Coud not find code section")

        c_tokens_list = tokens_list[begin_index + 1:end_index]
        self._main_block = self._parse_block(c_tokens_list)
        self._global_labels = self._main_block.labels
        self._main_block = self._reduce_block(self._main_block)
        return {'code':self._main_block.encode(), 'data':self._data}
        

    ###########################################################################
    # Private methods
    ###########################################################################
    
    def _tokenize(self, line):
        parts = [s.strip() for s in line.split(',')]
        tokens = []
        if len(parts) == 1 and parts[0][:-1].isidentifier() \
           and parts[0][-1] == ':':
            label = parts[0].lower()
            tokens.append(Token(TOKEN_IDENT, label))
        else:           
            for part in parts:
                for s in part.split():
                    t = s.strip().lower()
                    if self._constant_regex.match(t):
                        tokens.append(Token(TOKEN_CONSTANT, int(t)))
                    elif self._register_regex.match(t):
                        r = int(t[1:])
                        tokens.append(Token(TOKEN_DATA_REGISTER, r))
                    elif t in self.instructions:
                        tokens.append(Token(TOKEN_MNEMONIC, t))
                    elif t in ['pc', 'n']:
                        mapping = {'pc':-2,
                                   'n':-1}
                        tokens.append(Token(TOKEN_SPECIAL_REGISTER,
                                            mapping[t]))
                    elif t in self.keywords:
                        tokens.append(Token(TOKEN_KEYWORD, t))
                    elif self._ident_regex.match(t):
                        tokens.append(Token(TOKEN_IDENT, t))
                    else:
                        tokens.append(Token(None, None))
        return tokens


    def _parse_macro(self, tokens_list, attr_tokens=[]):
        if len(attr_tokens) <= 0 or not is_ident(attr_tokens[0]) or \
           not attr_tokens[0].value.isidentifier():
            raise AssemblerException("Expected macro name")

        name = attr_tokens[0].value
        arity = 0
        if len(attr_tokens) > 1 and is_constant(attr_tokens[1]):
            arity = attr_tokens[1].value

        block = self._parse_block(tokens_list)
        
        return Assembler.AMacro(name, arity, block)

    def _parse_data(self, tokens_list):
        data = []
        dvars = dict()
        var_n = 0
        index = 0

        for tokens in tokens_list:
            if len(tokens) < 2:
                raise AssemblerException("Invalid declaration of data variable number %d in data section" % var_n)

            if not is_ident(tokens[0]) or not tokens[0].value.isidentifier():
                raise AssemblerException("Illegal data variable name")

            name = tokens[0].value

            if not is_constant(tokens[1]) or tokens[1].value <= 0:
                raise AssemblerException("Invalid data variable size")

            size = tokens[1].value
            array = [0] * size

            num_spec_elems = len(tokens) - 2
            if num_spec_elems > size:
                raise AssemblerException("Number of specified elements of data variable exceeds its size")

            for i in range(num_spec_elems):
                token = tokens[i + 2]
                if not is_constant(token):
                    raise AssemblerException("Data element number of %d of variale %d is invalid" % (i, var_n))

                array[i] = token.value

            dvars[name] = DataVar(name, size, data, index)
            data.extend(array)
            index += size
            var_n += 1

        return (data, dvars)
        

    def _parse_block(self, tokens_list):
        address = 0
        ops = []
        labels = dict()
        for tokens in tokens_list:
            if len(tokens) == 0:
                raise AssemblerException('Unknown input at address %d' % address)
            token = tokens[0]
            if is_mnemonic(token):
                instr = self.instructions[token.value]
                if not instr.parse(tokens):
                    raise AssemblerException('Bad instruction at address %d' % address)
                operands = instr.extract_operands(tokens[1:])
                op = InstructionOp(instr, operands)
                ops.append(op)
                address += 1 + instr.num_operands
            elif token.token_type is TOKEN_IDENT and token.value in self._macros:
                # TODO: some refactoring here given similarity with parsing
                # of instruction above
                macro = self._macros[token.value]
                if not macro.parse(tokens):
                    raise AssemblerException('Bad macro use at address %d'\
                                             % address)
                operands = macro.extract_operands(tokens[1:])
                op = MacroOp(macro, operands)
                ops.append(op)
                address += 1 + macro.arity
            elif token.token_type is TOKEN_IDENT and token.value[-1] == ':':
                label = token.value[:-1]
                labels[label] = address
            else:
                raise AssemblerException('Improperly placed token (%s)' % token.value)

        block = Block(ops, labels)
        return block

    def _extract_section(self, tokens_list, begin_index=0):
        index = begin_index

        if index >= len(tokens_list) or len(tokens_list[index]) < 2 or \
           not match_kw(tokens_list[index], ["begin"]) or \
           not is_keyword(tokens_list[index][1]):
            raise AssemblerException("Expected section on line %d" % index)

        section = tokens_list[index][1].value
        attributes = tokens_list[index][2:]
        index += 1

        while index < len(tokens_list) and \
              not match_kw(tokens_list[index], ("end", section)):
                index += 1

        if index >= len(tokens_list):
            raise AssemblerException('Section %s not terminated' % section)

        end_index = index

        return (section, attributes, end_index)


    def _resolve(self, symbol_str):
        error_str = 'Failed to resolve symbol %s' % symbol_str
        
        if len(symbol_str) < 1:
            raise ResolverException(error_str)
        
        if symbol_str in self._global_labels:
            return (TYPE_LABEL, self._global_labels[symbol_str])

        is_address = len(symbol_str) >= 2 and symbol_str[0] == '&'
        if is_address:
            symbol_str = symbol_str[1:]

        is_array_access = symbol_str[-1] == ']'
        index = 0
        ident = symbol_str
        if is_array_access:
            parts = symbol_str[:-1].split('[')
            ident = parts[0]
            index = int(parts[1])

        if ident in self._global_vars:
            var = self._global_vars[ident]
            
            if index >= var.size:
                raise ResolverException('Array access to %s out of bounds' % ident)
            if is_address:
                return (TYPE_CONSTANT, var.index + index)
            else:
                return (TYPE_CONSTANT, var.data[var.index + index])

        if ident in self._constants:
            var = self._constants[ident]
            
            if index >= var.size:
                raise ResolverException('Array access to %s out of bounds' % ident)
            if is_address:
                raise ResolverException("Cannot get address of a constant")
            
            return (TYPE_CONSTANT, var.data[var.index + index])
            
            
        if ident == 'x' and is_address:
            return (TYPE_CONSTANT, len(self._data) + index)

        raise ResolverException(error_str)
                
            
    

    def _reduce_block(self, block, start_address=0, args=dict()):
        red_ops = []
        red_labels = {label:block.labels[label] + start_address \
                      for label in block.labels}

        if block is self._main_block:
            self._global_labels = red_labels

        def resolve(symbol_str):
            if symbol_str in red_labels:
                return (TYPE_LABEL, red_labels[symbol_str])            
            elif symbol_str in args:
                operand = args[symbol_str]
                return operand.eval()
            else:
                return self._resolve(symbol_str)

        address = start_address
        for op in block.ops:
            red_operands = []
            for operand in op.operands:
                if operand.operand_type is OPERAND_SYMBOL:
                    red_operands.append(Symbol(operand.symbol_str,
                                               resolver=resolve))
                else:
                    red_operands.append(operand)

            if op.op_type is OP_INSTRUCTION:
                red_op = InstructionOp(op.instruction, red_operands)
                signature = op.instruction.signature
                index = 0

                # Sanity check: ensure type of operand agrees with instruction
                # signature
                for operand in red_operands:
                    ty = signature[index]
                    if not match_type(ty, operand):
                        raise AssemblerException('Operand %d does not match type' % index)
                    index += 1
                red_ops.append(red_op)
                address += 1 + len(red_operands)
            elif op.op_type is OP_MACRO:
                macro = op.macro
                noperands = len(red_operands)
                args = {('args[%d]' % i):red_operands[i] \
                        for i in range(noperands)}
                red_block = self._reduce_block(macro.block, address,
                                               args)
                assert red_block.is_reduced()

                macro_start_addr = address
                for red_op in red_block.ops:
                    assert red_op.op_type is OP_INSTRUCTION
                    red_ops.append(red_op)
                    address += 1 + len(red_op.operands)
                macro_end_addr = address

                macro_addr_delta = macro_end_addr - macro_start_addr

                red_label_strs = list(red_labels.keys())
                for label in red_label_strs:
                    if red_labels[label] > macro_start_addr:
                        red_labels[label] += macro_addr_delta
                        if label in self._global_labels:
                            self._global_labels[label] = red_labels[label]
                        
            
        
        return Block(red_ops, red_labels)
    


    def _parse_put(self, tokens):
        return (is_ident(tokens[1]) or is_constant(tokens[1])) and \
            (is_ident(tokens[2]) or is_data_register(tokens[2]))

    def _parse_brn(self, tokens):
        return is_register(tokens[1]) and is_target(tokens[2])

    def _parse_cal(self, tokens):
        return is_target(tokens[1])




if len(sys.argv) < 2:
    print("usage: python3 %s <input-file>" % sys.argv[0])
    exit(1)

in_filename = sys.argv[1]
assembler = Assembler(hram.HRAM0S_PARAMS)
with open(in_filename) as in_f:
    obj = assembler.assemble(in_f)

    in_filename_parts = in_filename.split('.')
    out_filename = '.'.join(in_filename_parts[:-1]) + '.prg'

    with open(out_filename, 'w') as out_f:
        json.dump(obj, out_f)

exit(0)
