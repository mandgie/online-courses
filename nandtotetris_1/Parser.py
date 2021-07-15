'''
This code isparsing the fields

'''

from SymbolTable import symbol_table

import argparse
class Parser:

    def __init__(self):
        self.counter = 0
        self.cmd = None
        self.line_length = None
        self.file = None
        self.a = None
        self.memory_location = 16
        self.lines = self._open_file()

    def _open_file(self):
        parser = argparse.ArgumentParser(description="Get assembly file input")
        parser.add_argument("input_file", type=str)
        args = parser.parse_args()
        self.output_file = args.input_file.replace('.asm', '.hack')
        self.file = open(args.input_file, "r")
        lines = []
        for line in self.file:
            lines.append(line)
        self.file.close()

        lines = list(self._clean_whitespaces(lines))
        print("length of lines: ", len(lines))
        lines = list(self._remove_comments(lines))
        print("lines: ", lines)
        self._add_label_symbols(lines)

        lines = list(self._remove_label_symbols(lines))
        self.line_length = len(lines)
        self._add_var_symbols(lines)
        return iter(lines)

    def _output_file(self, filename):
        file_ext = '.Hack'
        filename.split('.')[0]

    def _clean_whitespaces(self, text_lists):
        for line in text_lists:
            a = line.split()
            if a:
                yield a[0]

    def _remove_comments(self, text_lists):
        for line in text_lists:
            if line != '//':
                yield line

    def _add_label_symbols(self, text_lists):
        nr_of_labels = 0
        for index, value in enumerate(text_lists):
            if value.startswith('('):
                new_label_symbol = value[1:-1]
                symbol_table[new_label_symbol] = index - nr_of_labels
                nr_of_labels += 1
                print('added new symbol: ', new_label_symbol)
            
    def _remove_label_symbols(self, text_lists):
        for line in text_lists:
            if not line.startswith('('):
                yield line

    def _add_var_symbols(self, text_lists):
        for line in text_lists:
            if line.startswith('@'):
                address = line.split('@')[1]
                try:
                    int(address)
                except ValueError:
                    if address not in symbol_table:
                        symbol_table[address] = self.memory_location
                        print('added new symbol: ', address)
                        self.memory_location += 1

    def a_instruction(self):
        cmd = next(self.lines)
        self.cmd = cmd
        self.counter += 1
        if cmd.startswith('@'):
            self.a = cmd.split('@')[1]
            return True

    def comp(self):
        try:
            if ';' in self.cmd:
                comp = self.cmd.split(';')[0]
                return comp
            elif '=' in self.cmd:
                comp = self.cmd.split('=')[1]
                return comp
            else:
                return ""

        except:
            return ""

    def dest(self):
        try:
            if '=' in self.cmd:
                dest = self.cmd.split('=')[0]
                return dest
            else:
                return ""
        except:
            return ""

    def jump(self):
        try:
            dest = self.cmd.split(';')[1]
            return dest
        except:
            return ""

    def done(self):
        print(self.counter)
        print("LineL: ", self.line_length)
        if self.counter == self.line_length:
            return True
        else:
            return False

    def out(self):
        f = open(self.output_file, "w")
        return f
        #f.write(line + '\n')
