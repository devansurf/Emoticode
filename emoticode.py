import ply.lex as lex
import ply.yacc as yacc
import sys

#-- Lexical Analysis --

DEBUG = False
prompt = "Enter < 1 > for terminal\nEnter < 2 > for running file\n"
#Convert inputs into tokens, run logic and compare valid syntax
reserved = {
    'else' : 'ELSE',
    'while' : 'WHILE',
}
tokens = [
    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUALS',
    'LPAREN',
    'RPAREN',
    'PRINT',
]+ list(reserved.values())
#PLY looks for the the syntax 't_' to register what a token will look like

# 'r' is for raw strings

t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='
t_LPAREN = r'\('
t_RPAREN = r'\)'

t_PRINT = r'\🖨️'
t_IF = r'\🤔'
t_THEN = r'\👶'
t_END = r'\💀'

t_ignore = r' '


#Function order matters! Lexer compares the functions from top to bottom.

def t_FLOAT(t):
    #Regular expression with a length greater than one in front and behind a point
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    #Regular expression for any value of length greater than 1
    r'\d+'
    t.value = int(t.value)
    return t

def t_NAME(t):
    #Regular expression, variable name has to start with a character that is not a number
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

def t_COMMENT(t):
     r'\💬.*'
     pass
     # No return value. Token discarded

def t_NEWLINE(t):
     r'\n+'
     t.lexer.lineno += len(t.value)

def t_error(t):
    print("Illegal characters!")
    t.lexer.skip(1)

lexer = lex.lex()

#'precedence' removes ambiguity, make sure it is written exactly as shown
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE')
)
#-- Parsing --

#Start of parser Tree
def p_emoticode(p):
    '''
    emoti : expression
         | var_assign
         | print
         | empty
    '''
    run(p[1])

def p_print(p):
    '''
    print : PRINT LPAREN expression RPAREN
    '''
    p[0] = ('print', p[3])

def p_var_assign(p):
    '''
    var_assign : NAME EQUALS expression
    '''
    p[0] = ('=', p[1], p[3])

def p_expression(p):
    #recursive expression, allows expressions such as 1+2+3+4...+n
    '''
    expression : expression PLUS expression
               | expression DIVIDE expression
               | expression MULTIPLY expression
               | expression MINUS expression
    '''
    #expression tree
    p[0] = (p[2], p[1], p[3])

def p_expression_var(p):
    '''
    expression : NAME
    '''
    #Expression will be evaluated to whatever the value is
    p[0] = ('var', p[1])

def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    #Expression will be evaluated to whatever the value is
    p[0] = p[1]

def p_expression_parenthesis(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    #remove parentheses, evaluate anything inside parenthesis as seperate expression
    p[0] = p[2]

def p_error(p):
    print("Syntax Error Found!")

def p_empty(p):
    '''
    empty :
    '''
    p[0] = None


#Start of input
parser = yacc.yacc()
env = {}

def run(p):
    global env
    #runs parse
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run (p[2])
        elif p[0] == '-':
            return run(p[1]) - run (p[2])
        elif p[0] == '*':
            return run(p[1]) * run (p[2])
        elif p[0] == '/':
            return run(p[1]) / run (p[2])
        elif p[0] == 'print':
            return print(run(p[1]))

        elif p[0] == '=':
            env[p[1]] = run(p[2])
            if DEBUG:
                print(env)
        elif p[0] == 'var':
            if p[1] not in env:
                return 'Undeclared variable found!'
            else:
                return env[p[1]]
    else:
        return p

i = input(prompt)

while not (int(i) == 1 or int(i) == 2):
    print("\nInvalid input!\n")
    i = input(prompt)

i = int(i)
if i == 1:
    while True:
        try: 
            s = input('📥 >> : ')
        except EOFError:
            break
        parser.parse(s)
else:
    filename = input("Input the file name (Ex: test.ec):\n")
    if DEBUG:
        print("\nDEBUG IS ON.\n")
    with open(filename,"r") as f:
        data = f.readlines() # readlines() returns a list of items, each item is a line in your file
        for ln in data:
            parser.parse(ln)

