from re import T
import ply.lex as lex
import ply.yacc as yacc
import sys

#Constants
WAIT_UNTIL_END = "WAIT_UNTIL_END"
#Memory

lnNum = 0
env = {}

#stacks
goto = [] 
state = []
func_calls = []

#Key Value pairs
lnGoto = {}             #Key -> trigger line Num, Value -> goto Line number
funcGoto = {}           #Key -> Func Name, Value -> starting line number

#-- Lexical Analysis --

DEBUG = T
prompt = "Enter < 1 > for terminal\nEnter < 2 > for running file\n"

def peek_stack(stack):
    if stack:
        return stack[-1]
    else:
        return -1

#Convert inputs into tokens, run logic and compare valid syntax
reserved = {
    'else' : 'ELSE',
}
tokens = [
    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'ASSIGN',
    'LESSTHAN',
    'GREATERTHAN',
    'EQUALS',
    'NOTEQUALS',
    'LPAREN',
    'RPAREN',
    'IF',
    'WHILE',
    'THEN',
    'END',
    'FUNC',
    'COMMA',
    'STRING',
    'PRINT',
]+ list(reserved.values())
#PLY looks for the the syntax 't_' to register what a token will look like

# 'r' is for raw strings

t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_ASSIGN = r'\='
t_LESSTHAN = r'\<'
t_GREATERTHAN = r'\>'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r'\,'

t_PRINT = r'\🖨️'
t_WHILE = r'\🔁'
t_IF = r'\🤔'
t_THEN = r'\👶'
t_END = r'\💀'
t_FUNC = r'\#️⃣'

t_ignore = r' '


#Function order matters! Lexer compares the functions from top to bottom.

# def t_char(t):
#    r'\'([^\\\n]|(\\.))*?\''

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = str(t.value).replace('"', '')
    return t

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

def t_EQUALS(t):
    #Only passes ==
    r'(?<!=)==(?!=)'
    return t

def t_NOTEQUALS(t):
    #This regex passes both == and !=
    r'(?<![!=])[!=]=(?!=)'
    return t
    
def t_COMMENT(t):
     r'\💬.*'
     pass
     # No return value. Token discarded

# Define a rule so we can track line numbers
def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    #print("line number " + str(t.lexer.lineno))

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
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
    emoti : code
          | conditional
          | while
          | end
    '''
    p[0] = p[1]

def p_code(p):
    #Exists to execute other blocks without running emoticode
    '''
     code : expression
          | var_assign
          | print
          | empty
    '''
    global state
    #Do not run until parser finds END
    if peek_stack(state) == WAIT_UNTIL_END:
        p[0] = None
    else:
        p[0] = p[1]



def p_var_assign(p):
    '''
    var_assign : NAME ASSIGN expression
    '''
    p[0] = ('=', p[1], p[3])

def p_expression(p):
    #recursive expression, allows expressions such as 1+2+3+4...+n
    '''
    expression : expression PLUS expression
               | expression DIVIDE expression
               | expression MULTIPLY expression
               | expression MINUS expression
               | expression LESSTHAN expression
               | expression GREATERTHAN expression
               | expression EQUALS expression
               | expression NOTEQUALS expression
    '''
    # line  = p.lineno(2)        # line number of the PLUS token
    # index = p.lexpos(2)        # Position of the PLUS token
    # print("line number is: " + str(line))
    # print("lex pos is: " + str(index))
    #expression tree
    p[0] = (p[2], p[1], p[3])


def p_function(p):
    '''
    function : FUNC NAME LPAREN param RPAREN THEN
    '''
    #Check if there is a request to run the function
    if peek_stack(func_calls) == str(p[2]):
        print("do something")
    else:
        #register the function in the dictionary
        funcGoto[str(p[2])] = lnNum
        #state.append(WAIT_UNTIL_END)

def p_conditional(p):
    '''
    conditional : IF expression THEN
    '''

    global state
    #If WAIT_UNTIL_END before conditional, then add to the stack and ignore this step
    if not run(p[2]) or peek_stack(state) == WAIT_UNTIL_END:
        state.append(WAIT_UNTIL_END) 
    else:
        p[0] = None

def p_while(p):
    '''
    while : WHILE expression THEN
    '''

    global state
    global goto
    #If false, skip the loop
    if not run(p[2]) or peek_stack(state) == WAIT_UNTIL_END:
        state.append(WAIT_UNTIL_END)
    #If true, run the block and run the check again
    else: 
        #Add while line# to goto stack for later reference
        goto.append(lnNum)
        p[0] = None

def p_end(p):  
    '''
    end : END
    '''

    global state
    if peek_stack(state) != -1:
        state.pop()
    p[0] = p[1]

def p_param_multi(p):
    '''
    param : param COMMA param
    '''
    p[0] = ('param', p[1], p[3])
    
def p_param_single(p):
    '''
    param : expression
    '''
    p[0] = ('param', p[1])

def p_expression_var(p):
    '''
    expression : NAME
    '''
    #Expression will be evaluated to whatever the value is
    p[0] = ('var', p[1])

def p_expression_int_float_string(p):
    '''
    expression : INT
               | FLOAT
               | STRING
    '''
    #Expression will be evaluated to whatever the value is
    p[0] = p[1]

def p_expression_parenthesis(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    #remove parentheses, evaluate anything inside parenthesis as seperate expression
    p[0] = p[2]

def p_print(p):
    '''
    print : PRINT LPAREN expression RPAREN
    '''
    p[0] = ('print', p[3])

def p_error(p):
    print("Syntax Error Found!")

def p_empty(p):
    '''
    empty :
    '''
    p[0] = None

#Start of input
parser = yacc.yacc()

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
        elif p[0] == '<':
            return run(p[1]) < run (p[2])
        elif p[0] == '>':
            return run(p[1]) > run (p[2])
        elif p[0] == '==':
            return run(p[1]) == run (p[2])
        elif p[0] == '!=':
            return run(p[1]) != run (p[2])

        elif p[0] == 'print':
            return print("Output: " + str(run(p[1])))

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


filename = input("Input the file name (Ex: test.ec , loop.ec):\n")
if DEBUG:
    print("\nDEBUG IS ON.\n")
with open(filename,"r") as f:
    data = f.readlines() # readlines() returns a list of items, each item is a line in your file
  
    while lnNum < len(data):
            
        result = parser.parse(data[lnNum])
        run(result)
        lnNum += 1

        gotoLn = peek_stack(goto)
        

        if result and gotoLn > 0:
            
            #Resolve any WAIT states before goto's
            if peek_stack(state) != -1:
                state.pop()

            #Goto's trigger on END
            elif result[0] == "💀":
                #modify lnNum, start from the goto line
                lnNum = gotoLn
                goto.pop()
        
            
    #Potential logic to detect blocks (incomplete)
   # for lnNum in range(goto.pop(), p.lineno(1)):
        #     lexer.input(data[lnNum])
        #     while 1:
        #         tok = lexer.token()
        #         if not tok: break