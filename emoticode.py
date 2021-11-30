from re import T
import ply.lex as lex
import ply.yacc as yacc
import sys

#Constants
WAIT_UNTIL_END = "WAIT_UNTIL_END"
FUNCTION_CALL = "FUNCTION_CALL"
#Memory

lnNum = 0
blockLevel = 0
env = {}

#stacks
goto = [] 
prev = []
state = []
envStack = []
func_calls = []
func_return = []
func_blockLevel = []
passed_args = []

#Key Value pairs
funcGoto = {}           #Key -> Func Name, Value -> starting line number

#-- Lexical Analysis --

DEBUG = False
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
    'RETURN',
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

t_WHILE = r'\🔁'
t_IF = r'\🤔'
t_THEN = r'\👶'
t_END = r'\💀'
t_FUNC = r'\#️⃣'
t_RETURN = r'\↩️'
t_PRINT = r'\🖨️'

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

def t_error(t):
    print("Illegal character on line "+ str(t.lexer.lineno) + " '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()


#'precedence' removes ambiguity
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
          | function
          | end
    '''
    p[0] = p[1]

def p_code(p):
    #Exists to execute other blocks without running emoticode
    '''
     code : expression
          | var_assign
          | return
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
 
    #expression tree
    p[0] = (p[2], p[1], p[3])


def p_function(p):
    '''
    function : FUNC NAME LPAREN args RPAREN THEN
    '''
    #Check if there is a request to run the function
    p[0] = ('function', p[2], p[4])

def p_conditional(p):
    '''
    conditional : IF expression THEN
    '''

    p[0] = ('cond', p[2])

def p_while(p):
    '''
    while : WHILE expression THEN
    '''
    global state
    global goto
    #If false, skip the loop
    p[0] = ('while', p[2])

def p_end(p):  
    '''
    end : END
    '''

    p[0] = ('end', p[1])

def p_return(p):
    '''
    return : RETURN expression
           | RETURN empty

    '''
    p[0] = ('return', p[2])


def p_args(p):
    '''
    args : args COMMA args
    '''
    #needs existing variables names to be passed as args
    # if not create new vars with the pass values
    p[0] = ('args', p[1], p[3])

def p_args_single(p):
    '''
    args : NAME
         | empty
    '''
    p[0] = ('args', p[1], None)

def p_callfunc(p):
    '''
    callfunc : NAME LPAREN param RPAREN
    '''
    #If the result for the function was resolved, return the result
    if peek_stack(func_return) != -1:
        #Remove and return from funcEnv
        p[0] = func_return.pop()
    else:
        p[0] = ('callfunc', p[1], p[3])


def p_param(p):
    '''
    param : param COMMA param
    '''
    p[0] = ('param', p[1], p[3])

def p_params_single(p):
    '''
    param : expression
          | empty
    '''
    p[0] = ('param', p[1], None)


def p_expression_var(p):
    '''
    expression : NAME
    '''
    #Expression will be evaluated to whatever the value is
    p[0] = ('var', p[1])


def p_expression_data(p):
    '''
    expression : INT
               | FLOAT
               | STRING
               | callfunc
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

parser = yacc.yacc()

def run(p):
    global env
    global blockLevel
    global func_calls
    global func_return
    global goto
    global lnNum

    #runs parse
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run(p[2])
        elif p[0] == '-':
            return run(p[1]) - run(p[2])
        elif p[0] == '*':
            return run(p[1]) * run(p[2])
        elif p[0] == '/':
            return run(p[1]) / run(p[2])
        elif p[0] == '<':
            return run(p[1]) < run(p[2])
        elif p[0] == '>':
            return run(p[1]) > run(p[2])
        elif p[0] == '==':
            return run(p[1]) == run(p[2])
        elif p[0] == '!=':
            return run(p[1]) != run(p[2])
        elif p[0] == 'print':
            output = str(run(p[1]))
            if output != "None":
                return print(">> " + output)

        elif p[0] == 'while':
            blockLevel +=1 
            if not run(p[1]) or peek_stack(state) == WAIT_UNTIL_END:
                #If true, run the block and run the check again
                state.append(WAIT_UNTIL_END)
            else: 
                #Add while line# to goto stack for later reference
                goto.append(lnNum)

        elif p[0] == 'function':
            blockLevel += 1
            if peek_stack(func_calls) == str(p[1]):
                #Function starts
                func_blockLevel.append(blockLevel)
                func_calls.pop()
                #Run args
                run(p[2])
            else:
                #register the function in the dictionary
                funcGoto[str(p[1])] = lnNum
                state.append(WAIT_UNTIL_END)

        elif p[0] == 'cond':
            #If WAIT_UNTIL_END before conditional, then add to the stack and ignore this step
            blockLevel += 1
            if not run(p[1]) or peek_stack(state) == WAIT_UNTIL_END:
                state.append(WAIT_UNTIL_END) 
    
        elif p[0] == 'callfunc':
            #append a goto back to the line where the function was called
            prev.append(lnNum)
            #append the function's goto line number to the goto stack
            goto.append(funcGoto[p[1]]) 
            #append a new function call, immediately activates goto. Pass function name
            func_calls.append(p[1])
            #Run the params
            run(p[2])

        elif p[0] == "param":
            #last param inserted first, pops last
            param2 = run(p[2])
            param1 = run(p[1])

            #Pass none empty parameters
            if param2 is not None:
                passed_args.append(param2)
            if param1 is not None:
                passed_args.append(param1)
            

        elif p[0] == "args":
            #create vars in a scope
            argName1 = run(p[1])
            if argName1 is not None:
                envStack.append({argName1 : passed_args.pop()})

            argName2 = run(p[2])
            if argName2 is not None:
                envStack.append({argName2 : passed_args.pop()})

        elif p[0] == 'return':
            #exit function
            blockLevel = peek_stack(func_blockLevel)-1
            #Append the expression result to the return stack
            func_return.append(run(p[1]))

        elif p[0] == 'end':
            blockLevel -= 1
            if peek_stack(state) != -1:
                state.pop()
            run(p[1])

        elif p[0] == '=':
            env[p[1]] = run(p[2])
            if DEBUG:
                print(env)
                
        elif p[0] == 'var':
            envScope = peek_stack(envStack)
            
            #Prioritize global vars
            if p[1] in env:
                return env[p[1]]

            elif envScope != -1:
                if p[1] in envScope:
                    #print(envScope[p[1]])
                    #only look at variables within the top of the stack (scope)
                    return envScope[p[1]]
            else:
                return 0
    else:
        return p


filename = input("Input the file name (Ex: test.ec , loop.ec, func.ec):\n")
if DEBUG:
    print("\nDEBUG IS ON.\n")
with open(filename,"r") as f:
    # readlines() returns a list of items, each item is a line in your file
    data = f.readlines() 
  
    while lnNum < len(data):
        
        result = parser.parse(data[lnNum])
        run(result)
        lnNum += 1


        if peek_stack(func_calls) != -1: #if a function was called, jump to it
            lnNum = goto.pop()

        #Start of GOTO logic -> handles jumps between lines
        gotoLn = peek_stack(goto)
      
        #Handle loops
        if result and gotoLn > 0:
            #Goto's trigger on END when inside a loop
            if result[0] == 'end':
                #Resolve any WAIT states before goto's
                if peek_stack(state) != -1:
                    state.pop()
                else: 
                    #modify lnNum, start from the goto line
                    lnNum = goto.pop()

        #Handle functions
        prevLn = peek_stack(prev)
        peekBlock = peek_stack(func_blockLevel)
        if result and prevLn > 0:
            #If exited block level, that means function was exited
            if blockLevel < peekBlock:
                lnNum = prev.pop()
                func_blockLevel.pop()
                #release variables in function scope
                envStack.pop()
            
                    
                  
          