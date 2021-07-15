from Parser import Parser
from Code import Code

# Tmp import
from SymbolTable import symbol_table

def main():
    # Run code inside of here
    parser = Parser()
    code = Code()

    f = open("output.Hack", "w")
    f = parser.out()
    # Writing output file
    while not parser.done():
        if parser.a_instruction():
            a = code.set_a(parser.a)
            f.write(a + '\n')

        else:
            c = parser.comp()
            d = parser.dest()
            j = parser.jump()

            cc = code.comp(c)
            dd = code.dest(d)
            jj = code.jump(j)

            string_out = "111" + cc + dd + jj
            f.write(string_out + '\n')


            
    print(symbol_table)





    # Writing output file


    
    



"""
# Parse the file line by line
input_file = 'demofile.txt'

# Initilize symbol table
symbol_table = {
    'R0': '0',
    'R1': '1',
    'R2': '2',
    'R0': '3',
    'R1': '4',
    'R2': '5',
    'R0': '6',
    'R1': '7',
    'R2': '8',
    'R0': '9',
    'R1': '10',
    'R2': '11',
    'R0': '12',
    'R1': '13',
    'R2': '14',
    'R1': '13',
    'R2': '15',
}

dest_table = {

}

def get_text(filename):
    f = open(filename, "r")
    lines = []
    for line in f:
        lines.append(line)

    f.close()
    return lines

lines = get_text(input_file)

def clean_whitespaces(text_lists):
    for line in text_lists:
        a = line.split()
        if a:
            yield a[0]

def remove_comments(text_lists):
    for line in text_lists:
        if line != '//':
            yield line


    
lines = list(clean_whitespaces(lines))
lines = list(remove_comments(lines))

print(lines)

# Check if c or a


# write to new file
f = open("output.Hack", "w")

def write_output(text_list):
    for line in text_list:
        f.write(line + '\n')


write_output(lines)
"""