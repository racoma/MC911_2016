# Yacc example

import ply.yacc as yacc

# Get the token map from the lexer.  This is required.
from lyalex import tokens


######################################################

def p_program(p):
  """ program : statement_list """
  p[0] = p[1]

def p_statement_list(p):
  """ statement_list : statement
                     | statement_list statement
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

def p_statement(p):
  """ statement : declaration_statement
                | synonym_statement
                | newmode_statement
                | procedure_statement
                | action_statement
  """
  p[0] = p[1]

######################################################

def p_declaration_statement(p):
  ' declaration_statement : DCL declaration_list SEMI'
  p[0] = DCL(p[2])

def p_declaration_list(p):
  """ declaration_list : declaration
                       | declaration_list COMMA declaration
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_declaration(p):
  """ declaration : identifier_list mode initialization_opt
  """
  p[0] = [p[1], p[2]] if len(p) == 3 else [p[1], p[2], p[3]]

def p_initialization(p):
  ' initialization : ASSIGN expression'
  p[0] = ASSIGN(p[2])

def p_identifier_list(p):
  """ identifier_list : identifier
                       | identifier_list COMMA identifier
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_identifier(p):
  ' identifier : ID '
  p[0] = ID(p[1])

######################################################

def p_synonym_statement(p):
  ' synonym_statement : SYN synonym_list SEMI'
  p[0] = SYN(p[2])

def p_synonym_list(p):
  """ synonym_list : synonym_definition
                   | synonym_list COMMA synonym_definition
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_synonym_definition(p):
  ' synonym_definition : identifier_list mode_opt ASSIGN constant_expression'
  p[0] = [p[1], p[2], p[4]] if len(p) == 5 else [p[1], p[3]]

def p_constant_expression(p):
  ' constant_expression : expression'
  p[0] = p[1]

######################################################

def p_newmode_statement(p):
  ' newmode_statement : TYPE newmode_list'
  p[0] = TYPE(p[1])

def p_newmode_list(p):
  """ newmode_list : mode_definition
                   | newmode_list COMMA mode_definition
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_mode_definition(p):
  ' mode_definition : identifier_list ASSIGN mode '
  p[0] = [p[1], p[3]]

def p_mode(p):
  """ mode : mode_name
           | discrete_mode
           | reference_mode
           | composite_mode
  """
  p[0] = p[1]

def p_discrete_mode(p):
  """ discrete_mode : integer_name
                    | boolean_mode
                    | character_mode
                    | discrete_range_mode
  """
  p[0] = p[1]

def p_integer_mode(p):
  ' integer_mode : INT '
  p[0] = p[1]

def p_boolean_mode(p):
  ' boolean_mode : BOOL '
  p[0] = p[1]

def p_character_mode(p):
  ' character_mode : CHAR '
  p[0] = p[1]

def p_discrete_range_mode(p):
  """ discrete_range_mode : discrete_mode_name LPAREN literal_range RPAREN
                          | discrete_mode LPAREN literal_range RPAREN
  """
  p[0] = [p[1], p[3]]

def p_discrete_mode_name(p):
  ' discrete_mode_name : identifier '
  p[0] = p[1]

def p_literal_range(p):
  ' literal_range : lower_bound COLON upper_bound '
  p[0] = [p[1], p[3]]

def p_lower_bound(p):
  ' lower_bound : integer_literal '
  p[0] = p[1]

def p_upper_bound(p):
  ' upper_bound : integer_literal '
  p[0] = p[1]

def p_reference_mode(p):
  ' reference_mode : REF mode '
  p[0] = REF(p[2])

def p_composite_mode(p):
  """ composite_mode : string_mode
                     | array_mode
  """
  p[0] = p[1]

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")

# Build the parser
parser = yacc.yacc()

while True:
   try:
       s = raw_input('calc > ')
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   print(result)