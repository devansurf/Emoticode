import ply.lex as lex
import ply.yacc as yacc
import sys

#-- Lexical Analysis --

#Convert inputs into tokens, run logic and compare valid syntax
tokens = [
    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUALS'
]
#PLY looks for the the syntax 't_' to register what a token will look like

# 'r' is for raw strings
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='

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
    t.type = 'NAME'
    return t

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
    calc : expression
         | var_assign
         | empty
    '''
    print(run(p[1]))

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
        elif p[0] == '=':
            env[p[1]] = run(p[2])
            print(env)
        elif p[0] == 'var':
            if p[1] not in env:
                return 'Undeclared variable found!'
            else:
                return env[p[1]]
    else:
        return p
        
while True:
    try: 
        s = input('>> ')
    except EOFError:
        break
    parser.parse(s)