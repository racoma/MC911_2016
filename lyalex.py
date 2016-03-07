# ------------------------------------------------------------
#                            Lexer
#              
#                Raissa Machado e Andre Brandao
#
# ------------------------------------------------------------

#Para testar as expressoes regulares: https://regex101.com/

import sys
import ply.lex as lex

f = open(sys.argv[-1])

reserved = {
   'if' : 'IF',
   'then' : 'THEN',
   'else' : 'ELSE',
   'while' : 'WHILE',
   'syn' : 'SYN',
   'dcl' : 'DCL',
   'print' : 'PRINT',
   'array' : 'ARRAY', 
   'by' : 'BY',
   'chars' : 'CHARS',
   'do' : 'DO',
   'down' : 'DOWN', 
   'elseif' : 'ELSIF', 
   'end' : 'END', 
   'exit' : 'EXIT',
   'fi' : 'FI',
   'for' : 'FOR', 
   'in' : 'IN', 
   'loc' : 'LOC', 
   'type' : 'TYPE', 
   'od' : 'OD', 
   'proc' : 'PROC', 
   'ref' : 'REF', 
   'result' : 'RESULT', 
   'return' : 'RETURN', 
   'returns' : 'RETURNS',
   'syn' : 'SYN', 
   'to' : 'TO'
}

# List of token names.
tokens = [
   'ID',
   'PLUS',
   'MINUS',
   'TIMES',
   'DIVIDE',
   'ASSIGN',
   'SEMI',
   'LPAREN',
   'RPAREN',
   'ICONST',
   'CCONST',
   'SCONST',
] + list(reserved.values())

# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_ASSIGN  = r'='
t_SEMI    = r';'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

#Numbers rule
def t_ICONST(t):
    r'\d+'
    t.value = int(t.value)    
    return t

#Constants rule
def t_CCONST(t):
    r"\'([^\\\n]|(\\.))*?\'"   
    return t

#Strings rule
def t_SCONST(t):
    r'\"([^\\\n]|(\\.))*?\"'   
    return t

#Line numbers track
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

#Ignores spaces and tabs
t_ignore  = ' \t'

#Ignores comments of type #
t_ignore_COMMENT1 = r'\#.*'

#Ignores comments of type /* */
t_ignore_COMMENT2 = r'\/\*(.|[\r\n])*?\*\/'

#Ignores comments of type //
t_ignore_COMMENT3 = r'\/\/.*'


#Column Computation
def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
	last_cr = 0
    column = (token.lexpos - last_cr) + 1
    return column


#Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


#Build the lexer
lexer = lex.lex()

lex.input(f.read())

for tok in iter(lex.token, None):
    print repr(tok.type), repr(tok.value)
    
    
    
    
    
