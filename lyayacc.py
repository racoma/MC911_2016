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
  """ declaration : identifier_list mode initialization
                  | identifier_list mode
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

# ######################################################

def p_synonym_statement(p):
  ' synonym_statement : SYN synonym_list SEMI'
  p[0] = SYN(p[2])

def p_synonym_list(p):
  """ synonym_list : synonym_definition
                   | synonym_list COMMA synonym_definition
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_synonym_definition(p):
  """ synonym_definition : identifier_list mode ASSIGN constant_expression
                         | identifier_list ASSIGN constant_expression

  """
  p[0] = [p[1], p[2], p[4]] if len(p) == 5 else [p[1], p[3]]

def p_constant_expression(p):
  ' constant_expression : expression'
  p[0] = p[1]

# ######################################################

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

def p_string_mode(p):
  ' string_mode : CHARS LBRACKET string_length RBRACKET '
  p[0] = p[3]

def p_string_length(p):
  ' string_length : integer_literal '
  p[0] = p[1]

def p_array_mode(p):
  """ array_mode : ARRAY LBRACKET index_mode RBRACKET
                 | ARRAY LBRACKET index_mode COMMA index_mode_list RBRACKET
  """
  p[0] = [p[3]] if len(p) == 5 else p[3] + [p[5]]

def p_index_mode(p):
  """ index_mode : discrete_mode
                 | literal_range
  """
  p[0] = p[1]

def p_index_mode_list(p):
  """ index_mode_list : index_mode
                      | index_mode_list COMMA index_mode
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_element_mode(p):
  """ element_mode : mode
  """
  p[0] = p[1]


# ######################################################

def p_location(p):
  """ location : location_name
               | dereferenced_reference
               | string_element
               | string_slice
               | array_element
               | array_slice
               | call_action
  """
  p[0] = p[1]

def p_dereferenced_reference(p):
  """ dereferenced_reference : primitive_value ARROW
  """
  p[0] = p[1]

def p_string_element(p):
  """ string_element : string_location LBRACKET start_element COLON right_element RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

def p_start_element(p):
  """ start_element : integer_expression
  """
  p[0] = p[1]

def p_string_slice(p):
  """ string_slice : string_location LBRACKET left_element COLON right_element RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

def p_left_element(p):
  """ left_element : integer_expression
  """
  p[0] = p[1]

def p_right_element(p):
  """ right_element : integer_expression
  """
  p[0] = p[1]

def p_array_element(p):
  """ array_element : string_location LBRACKET expression_list RBRACKET
  """
  p[0] = [p[1], p[3]]

def p_expression_list(p):
  """ expression_list : expression
                      | expression_list COMMA expression
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_array_slice(p):
  """ array_slice : array_location LBRACKET lower_element COLON upper_element RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

def p_lower_element(p):
  """ lower_element : expression
  """
  p[0] = p[1]

def p_upper_element(p):
  """ upper_element : expression
  """
  p[0] = p[1]

def p_primitive_value(p):
  """  primitive_value : literal
                       | value_array_element
                       | value_array_slice
                       | parenthesized_expression
  """
  p[0] = p[1]

def p_location_contents(p):
  """ location_contents : location
  """
  p[0] = p[1]

def p_value_name(p):
  """ value_name : synonym_name
                 | value_enumeration_name
  """
  p[0] = p[1]

def p_value_enumeration_name(p):
  """ value_enumeration_name : identifier
  """
  p[0] = p[1]

# ######################################################

def p_literal(p):
  """  literal : integer_literal
               | boolean_literal
               | character_literal
               | empty_literal
               | character_string_literal
  """
  p[0] = p[1]

def p_integer_literal(p):
  ' integer_literal : ICONST '
  p[0] = p[1]

def p_boolean_literal(p):
  """ boolean_literal : FALSE
                      | TRUE
  """
  p[0] = p[1]

def p_character_literal(p):
  """ character_literal : CCONST
  """
  p[0] = p[1]

def p_empty_literal(p):
  """ empty_literal : NULL
  """
  p[0] = None

def p_character_string_literal(p):
  """ character_string_literal : SCONST
  """
  p[0] = p[1]

# ######################################################

def p_value_array_element(p):
  """ value_array_element : array_primitive_value LBRACKET expression_list RBRACKET
  """
  p[0] = [p[1], p[3]]

def p_value_array_slice(p):
  """ value_array_slice : array_primitive_value LBRACKET lower_element COLON upper_element RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

def p_array_primitive_value(p):
  """ array_primitive_value : primitive_value
  """
  p[0] = p[1]

def p_parenthesized_expression(p):
  """ parenthesized_expression : LPAREN expression RPAREN
  """
  p[0] = p[2]

def p_expression(p):
  """ expression : operand0
                 | conditional_expression
  """
  p[0] = p[1]

def p_conditional_expression(p):
  """ conditional_expression : IF boolean_expression then_expression else_expression FI
                             | IF boolean_expression then_expression elsif_expression else_expression FI
  """
  p[0] = [p[2], p[3], p[4]] if len(p) == 6 else [p[2], p[3], p[4], p[5]]

def p_boolean_expression(p):
  """ boolean_expression : expression
  """
  p[0] = p[1]

def p_then_expression(p):
  """ then_expression : THEN expression
  """
  p[0] = p[1]

def p_else_expression(p):
  """ else_expression : ELSE expression
  """
  p[0] = p[1]

def p_elsif_expression(p):
  """ elsif_expression : ELSIF boolean_expression then_expression
                       | elsif_expression ELSIF boolean_expression then_expression
  """
  p[0] = [p[2], p[3]] if len(p) == 4 else p[1] + [p[3], p[4]]



# ######################################################

def p_operand0(p):
  """ operand0 : operand1
               | operand0 operator1 operand1
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2], p[3]]

def p_operator1(p):
  """ operator1 : relational_operator
                | membership_operator
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