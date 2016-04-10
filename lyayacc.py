# ------------------------------------------------------------ #
#                           Parser                             #
#                                                              #
#                Raissa Machado e Andre Brandao                #
#                                                              #
# ------------------------------------------------------------ #

import ply.yacc as yacc
from lyalex import tokens


########################  BLOCO 1  #############################

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

########################  BLOCO 2  #############################

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

########################  BLOCO 3  #############################

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

########################  BLOCO 4  #############################

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
  """ discrete_mode : integer_mode
                    | boolean_mode
                    | character_mode
                    | discrete_range_mode
  """
  p[0] = p[1]

def p_integer_mode(p):
  ' integer_mode : ICONST '
  p[0] = p[1]

def p_boolean_mode(p):
  ' boolean_mode : BOOL '
  p[0] = BOOL(None)

def p_character_mode(p):
  ' character_mode : CHAR '
  p[0] = p[1]

def p_discrete_range_mode(p):
  """ discrete_range_mode : discrete_mode_name LPAREN literal_range RPAREN
                          | discrete_mode LPAREN literal_range RPAREN
  """
  p[0] = [p[1], p[3]]

def p_mode_name(p):
  ' mode_name : identifier '
  p[0] = p[1]

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
  """ array_mode : ARRAY LBRACKET index_mode RBRACKET element_mode
                 | ARRAY LBRACKET index_mode COMMA index_mode_list RBRACKET element_mode
  """
  p[0] = [p[3]] if len(p) == 5 else p[3] + [p[5]]

def p_index_mode_list(p):
  """ index_mode_list : index_mode
                      | index_mode_list COMMA index_mode
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_index_mode(p):
  """ index_mode : discrete_mode
                 | literal_range
  """
  p[0] = p[1]

def p_element_mode(p):
  """ element_mode : mode
  """
  p[0] = p[1]


########################  BLOCO 5  #############################

def p_location(p):
  """ location : identifier
               | dereferenced_reference
               | string_element
               | string_slice
               | array_element
               | array_slice
               | call_action
  """
  p[0] = p[1]

def p_dereferenced_reference(p):
  """ dereferenced_reference : location ARROW
  """
  p[0] = p[1]

def p_string_element(p):
  """ string_element : identifier LBRACKET start_element COLON ICONST RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

def p_start_element(p):
  """ start_element : ICONST
  """
  p[0] = p[1]

def p_string_slice(p):
  """ string_slice : identifier LBRACKET ICONST COLON ICONST RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

def p_array_element(p):
  """ array_element : identifier LBRACKET expression_list RBRACKET
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

def p_array_location(p):
  """ array_location : location
  """
  p[0] = p[1]

def p_lower_element(p):
  """ lower_element : expression
  """
  p[0] = p[1]

def p_upper_element(p):
  """ upper_element : expression
  """
  p[0] = p[1]

########################  BLOCO 6  #############################

def p_primitive_value(p):
  """  primitive_value : literal
                       | value_array_element
                       | value_array_slice
                       | parenthesized_expression
  """
  p[0] = p[1]

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

# def p_location_contents(p):
#   """ location_contents : location
#   """
#   p[0] = p[1]

# def p_value_name(p):
#   """ value_name : synonym_name
#                  | value_enumeration_name
#   """
#   p[0] = p[1]

# def p_value_enumeration_name(p):
#   """ value_enumeration_name : identifier
#   """
#   p[0] = p[1]


########################  BLOCO 7  #############################

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
  """ expression : binop
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



########################  BLOCO 8  #############################

def p_binop(p):
  """ binop   : operand
              | binop AND binop
              | binop OR binop
              | binop EQ binop
              | binop NEQ binop
              | binop GT binop
              | binop GE binop
              | binop LT binop
              | binop LE binop
              | binop PLUS binop
              | binop MINUS binop
              | binop TIMES binop
              | binop DIVIDE binop
              | binop MOD binop
              | binop NOT binop
              | binop IN binop
              | binop CONCAT binop
  """
  if len(p) == 2:
	p[0] = p[1]
  else:
  	p[0] = [p[2], p[1], p[3]]

def p_operand(p):
  """ operand : MINUS operand1
  	      | NOT operand1
              | integer_literal
  """
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = [p[1], p[2]]

def p_operand1(p):
  """ operand1 : location
               | referenced_location
               | primitive_value
  """
  p[0] = p[1]

def p_referenced_location(p):
  """ referenced_location : ARROW location
  """
  p[0] = p[2]
  
########################### BLOCO 9 ##################################
def p_action_statement(p):
  """ action_statement : label_id COLON action SEMI
                       | action SEMI
  """
  if len(p) == 3:
    p[0] = p[1]
  else:
    p[0] = [p[1], p[3]]

def p_label_id(p):
  """ label_id : identifier
  """
  p[0] = p[1]

def p_action(p):
  """ action : if_action
  	     | do_action
             | assignment_action
             | call_action
             | exit_action
             | return_action
             | result_action
  """
  p[0] = p[1]

def p_assignment_action(p):
  """ assignment_action : location assigning_operator expression
  """
  p[0] = [p[1], p[2], p[3]]

def p_assigning_operator(p):
  """ assigning_operator : PLUS ASSIGN
                         | MINUS ASSIGN
                         | TIMES ASSIGN
                         | DIVIDE ASSIGN
                         | MOD ASSIGN
                         | CONCAT ASSIGN
                         | ASSIGN
  """
  p[0] = [p[1], p[2]] if len(p) == 3 else p[1]

########################### BLOCO 10 ##################################

def p_if_action(p):
  """ if_action : IF boolean_expression then_clause else_clause FI
                | IF boolean_expression then_clause FI
  """
  if len(p) == 6:
    p[0] = [p[2], p[3], p[4]]
  else:
    p[0] = [p[2], p[3]]
    
def p_then_clause(p):
  """ then_clause : THEN action_statement_list
  """
  p[0] = p[2]
  
def p_else_clause(p):
  """ else_clause : ELSE action_statement_list
                  | ELSIF boolean_expression then_clause else_clause
                  | ELSIF boolean_expression then_clause
  """
  if len(p) == 3:
    p[0] = p[2]
  elif len(p) == 5:
    p[0] = [p[2], p[3], p[4]]
  else:
    p[0] = [p[2], p[3]]

########################### BLOCO 11 ##################################
def p_do_action(p):
  """ do_action : DO control_part SEMI action_statement_list OD
                | DO action_statement_list OD
  """
  if len(p) == 4:
    p[0] = p[2]
  else:
    p[0] = [p[2], p[4]]

def p_control_part(p):
  """ control_part : for_control while_control
                   | while_control
  """
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = [p[1], p[2]]

def p_for_control(p):
  """ for_control : FOR iteration
  """
  p[0] = p[2]
  
def p_iteration(p):
  """ iteration : step_enumeration
                | range_enumeration
  """  
  p[0] = p[1]

def p_step_enumeration(p):
  """ step_enumeration : loop_counter ASSIGN start_value step_value DOWN end_value 
                       | loop_counter ASSIGN start_value DOWN end_value
                       | loop_counter ASSIGN start_value step_value end_value
                       | loop_counter ASSIGN start_value end_value
  """
  if len(p) == 5:
    p[0] = [p[1], p[2], p[3], p[4]]
  elif(p[4] == 'DOWN' and len(p) == 5):
    p[0] = [p[1], p[2], p[3], p[5]]
  elif len(p) == 5:
    p[0] = [p[1], p[2], p[3], p[4], p[5]]
  else:
    p[0] = [p[1], p[2], p[3], p[4], p[6]]

def p_loop_counter(p):
  """ loop_counter : identifier
  """
  p[0] = p[1]

def p_start_value(p):
  """ start_value : discrete_expression
  """ 
  p[0] = p[1]

def p_step_value(p):
  """ step_value : BY ICONST
  """ 
  p[0] = p[2]

def p_end_value(p):
  """ end_value : TO discrete_expression
  """
  p[0] = p[2]
  
def p_discrete_expression(p):
  """ discrete_expression : expression
  """  
  p[0] = p[1]
  
def p_range_enumeration(p):
  """ range_enumeration : loop_counter DOWN IN discrete_mode_name
                        | loop_counter IN discrete_mode_name
  """  
  if len(p) == 5:
    p[0] = [p[1], p[4]]
  else:
    p[0] = [p[1], p[3]]
    
def p_while_control(p):
  """ while_control : WHILE boolean_expression
  """
  p[0] = p[2]
  
########################### BLOCO 12 ##################################
def p_call_action(p):
  """ call_action : procedure_call
                  | builtin_call
  """
  p[0] = p[1]

def p_procedure_call(p):
  """ procedure_call : procedure_name LPAREN parameter_list RPAREN
                     | procedure_name LPAREN RPAREN
  """
  if len(p) == 4:
    p[0] = [p[1], p[3]]
  else:
    p[0] = p[1]

def p_parameter_list(p):
  """ parameter_list : parameter
                     | parameter_list COMMA parameter
  """ 
  p[0] = p[1]

def p_parameter(p):
  """ parameter : expression """
  p[0] = p[1]

def p_procedure_name(p):
  """ procedure_name : identifier """
  p[0] = p[1]

def p_exit_action(p):
  """ exit_action : EXIT label_id """
  p[0] = p[2]

def p_return_action(p):
  """ return_action : RETURN result 
                    | RETURN
  """
  if len(p) == 2:
	p[0] = none
  else:
	p[0] = p[2]

def p_result_action(p):
  """ result_action : RESULT result """
  p[0] = p[2]

def p_result(p):
  """ result : expression """
  p[0] = p[1]

def p_builtin_call(p):
  """ builtin_call : builtin_name LPAREN parameter_list RPAREN
                   | builtin_name LPAREN RPAREN
  """
  if len(p) == 4:
    p[0] = p[1]
  else:
    p[0] = [p[1], p[3]]

def p_builtin_name(p):
  """ builtin_name : NUM
                   | PRED
                   | SUCC
                   | UPPER
                   | LOWER
                   | LENGTH
                   | READ
                   | PRINT
  """

  p[0] = p[1]

########################### BLOCO 13 ##################################
def p_procedure_statement(p):
  """ procedure_statement : label_id COLON procedure_definition SEMI """
  p[0] = [p[1], p[3]]

def p_procedure_definition(p):
  """ procedure_definition : PROC LPAREN formal_parameter_list RPAREN result_spec SEMI action_statement action_statement_list END 
                          | PROC LPAREN formal_parameter_list RPAREN result_spec SEMI action_statement END
                          | PROC LPAREN RPAREN SEMI action_statement action_statement_list END
                          | PROC LPAREN RPAREN SEMI action_statement END
  """
  if len(p) == 10:
    p[0] = [p[3], p[5], p[7], p[8]]
  elif len(p) == 8:
    p[0] = [p[3], p[5], p[7]]
  elif len(p) == 7:
    p[0] = [p[5], p[6]]
  else:
    p[0] = p[5]

def p_action_statement_list(p):
  """ action_statement_list : action_statement
                           | action_statement_list COMMA action_statement
  """
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = [p[1], p[3]]

def p_formal_parameter_list(p):
  """ formal_parameter_list : formal_parameter
                           | formal_parameter_list COMMA formal_parameter

  """
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = [p[1], p[3]]

def p_formal_parameter(p):
  """ formal_parameter : identifier_list parameter_spec """
  p[0] = [p[1], p[2]]

def p_parameter_spec(p):
  """ parameter_spec : mode attribute
                    | mode
  """
  if len(p)== 2:
    p[0] = [p[1], p[2]]
  else:
    p[0] = p[1]

def p_result_spec(p):
  """ result_spec : RETURNS LPAREN mode attribute RPAREN 
                 | RETURNS LPAREN mode RPAREN
  """
  if len(p) == 4:
    p[0] = p[3]
  else:
    p[0] = [p[3], p[4]]

def p_attribute(p):
  """ attribute : LOC """
  p[0] = LOC(None)



########################### BLOCO 14 ##################################
#                                                                     #
#                  No need, done in the lexer                         #
#                                                                     #
#######################################################################


  
# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")


# Build the parser
parser = yacc.yacc()

