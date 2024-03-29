'''
This code is generating the tranlstaion from assembly -> machine code
'''
from SymbolTable import symbol_table

class Code:
    dest_table = {
        '': '000',
        'M': '001',
        'D': '010',
        'MD': '011',
        'A': '100',
        'AM': '101',
        'AD': '110',
        'AMD': '111',
    }

    comp_table = {
        '0': '0101010',
        '1': '0111111',
        '-1': '0111010',
        'D': '0001100',
        'A': '0110000',
        'M': '1110000',
        '!D': '0001101',
        '!A': '0110001',
        '!M': '0110001',
        'D+1': '0011111',
        'A+1': '0110111',
        'M+1': '1110111',
        'D-1': '0001110',
        'A-1': '0110010',
        'M-1': '1110010',
        'D+A': '0000010',
        'D+M': '1000010',
        'D-A': '0010011',
        'D-M': '1010011',
        'A-D': '0000111',
        'M-D': '1000111',
        'D&A': '0000000',
        'D&M': '1000000',
        'D|A': '0010101',
        'D|M': '1010101',
    }

    jmp_table = {
        '': '000',
        'JGT': '001',
        'JGE': '011',
        'JLT': '100',
        'JNE': '101',
        'JLE': '110',
        'JMP': '111',
    }

    def __init__(self):
        self.a = None

    def set_a(self, a):
        # Translate to binary
        try:
            binary_number = bin(int(a))[2:]
            leading_zeros = ''.join( ['0' for i in range(16 - len(binary_number))])
            return leading_zeros + binary_number
        except ValueError:
            binary_number = bin(int(symbol_table[a]))[2:]
            leading_zeros = ''.join( ['0' for i in range(16 - len(binary_number))])
            return leading_zeros + binary_number



    def comp(self, command):
        comp = self.comp_table[command]
        return comp

    def dest(self, command):
        dest = self.dest_table[command]
        return dest

    def jump(self, command):
        jmp = self.jmp_table[command]
        return jmp