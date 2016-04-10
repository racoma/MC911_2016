# ------------------------------------------------------------ #
#                            Lexer                             #
#                                                              #
#                Raissa Machado e Andre Brandao                #
#                                                              #
# ------------------------------------------------------------ #

#Para testar as expressoes regulares: https://regex101.com/

import sys
import ply.lex as lex

#f = open(sys.argv[-1])

reserved = {
   'if' : 'IF',
   'then' : 'THEN',
   'else' : 'ELSE',
   'while' : 'WHILE',
   'syn' : 'SYN',
   'dcl' : 'DCL',
   'array' : 'ARRAY', 
   'by' : 'BY',
   'chars' : 'CHARS',
   'do' : 'DO',
   'down' : 'DOWN', 
   'elsif' : 'ELSIF', 
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
   'to' : 'TO',
   'bool': 'BOOL',
   'char': 'CHAR',
   'false': 'FALSE',
   'int': 'INT',
   'length': 'LENGTH',
   'lower': 'LOWER',
   'null': 'NULL',
   'num': 'NUM',
   'pred': 'PRED',
   'print': 'PRINT',
   'read': 'READ',
   'succ': 'SUCC',
   'true': 'TRUE',
   'upper': 'UPPER',
}

# List of token names.
tokens = [

# Literals
   'ICONST',
   'CCONST',
   'SCONST',
   'ID',

# Operators
   'PLUS',
   'MINUS',
   'TIMES',
   'DIVIDE',
   'AND',
   'OR',
   'NOT',
   'EQ',
   'NEQ',
   'GT',
   'GE',
   'LT',
   'LE',
   'MOD',
   'ARROW',
   'CONCAT',
   
# Assignments
   'ASSIGN',

# Delimiters
   'SEMI',
   'COMMA',
   'COLON',
   'LPAREN',
   'RPAREN',
   'LBRACKET',
   'RBRACKET',
] + list(reserved.values())

# Operators
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_AND  	  = r'&&'
t_OR      = r'\|\|'
t_NOT     = r'!'
t_EQ      = r'=='
t_NEQ     = r'!='
t_GT      = r'>'
t_GE      = r'>='
t_LT      = r'<'
t_LE      = r'<='
t_IN      = r'IN'
t_MOD     = r'%'
t_ARROW   = r'->'
t_CONCAT  = r'&'


# Assignments
t_ASSIGN  = r'='

# Dividers
t_SEMI    = r';'
t_COMMA   = r'\,'
t_COLON  = r'\:'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'


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


#Error Unterminated string
def t_error_USTRING(t):
    r'\"([^\\\n]|(\\.))*?'
    print("%s: Unterminated string" % t.lineno)
    t.lexer.skip(1)

#Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)
    
#############################################################################
#Check for unterminated strings

states = (
  ('COMMENT', 'exclusive'),
)

def t_COMMENT(t):
    r'\/\*'
    t.lexer.code_start = t.lineno   
    t.lexer.push_state("COMMENT")

def t_COMMENT_string(t):
   r'\"([^\\\n]|(\\.))*?\"'
   pass

def t_COMMENT_char(t):
   r'\"([^\\\n]|(\\.))*?\"'
   pass

t_COMMENT_ignore = " \t\n"

def t_COMMENT_error(t):
    t.lexer.skip(1)

def t_COMMENT_end(t):
    r'(.|[\r\n])*?\*\/'
    t.lexer.pop_state()

def t_COMMENT_eof(t):
    print("%s: Unterminated comment" % t.lexer.code_start) 

############################################################################


#Build the lexer
lexer = lex.lex()


#lex.input(f.read())

#for tok in iter(lex.token, None):
 #  print repr(tok.type), repr(tok.value)
    
