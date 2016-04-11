# ------------------------------------------------------------ #
#                           Parser                             #
#                                                              #
#                Raissa Machado e Andre Brandao                #
#                                                              #
# ------------------------------------------------------------ #

import sys
import ply.yacc as yacc
from lyalex import tokens
import ast

f = open(sys.argv[-1])

precedence = (
	('left', 'CONCAT'),
	('left', 'NOT'),
	('left', 'IN'),
        ('left', 'OR'),
        ('left', 'AND'),
        ('left', 'EQ', 'NEQ'),
        ('left', 'GT', 'GE', 'LT', 'LE'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE', 'MOD')
    )

########################  BLOCO 1  #############################

def p_program(p):
  """ program : statement_list """
  p[0] = Program(p[1])

def p_statement_list(p):
  """ statement_list : statement
                     | statement_list statement
  """
  if len(p) == 2:
    p[0] = Statement_list([p[1]])
  else:
    p[1].statements.append(p[2])
    p[0] = p[1]

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
  """ declaration_statement : DCL declaration_list SEMI
  """
  if len(p) == 4:
    p[0] = DCL_statement(p[2])
  else:
    p[0] = DCL_statement(p[2])

def p_declaration_list(p):
  """ declaration_list : declaration
                       | declaration_list COMMA declaration
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_declaration(p):
  """ declaration : identifier_list mode initialization
                  | identifier_list mode
  """
  p[0] = Declaration(p[1], p[2]) if len(p) == 3 else Declaration(p[1], p[2], p[3])

def p_initialization(p):
  ' initialization : ASSIGN expression'
  p[0] = p[2]

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
  p[0] = p[2]

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
  p[0] = Constant(p[1])

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
  ' mode_definition : identifier_list mode '
  p[0] = [p[1], p[2]]

def p_mode(p):
  """ mode : mode_name
           | discrete_mode
           | reference_mode
           | composite_mode
  """
  p[0] = Mode(p[1])

def p_discrete_mode(p):
  """ discrete_mode : integer_mode
                    | boolean_mode
                    | character_mode
                    | discrete_range_mode
  """
  p[0] = DiscreteMode(p[1])

def p_integer_mode(p):
  ' integer_mode : INT '
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
  ' literal_range : ICONST COLON ICONST '
  p[0] = [p[1], p[3]]

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
  ' string_length : ICONST '
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
  """ location : dereferenced_reference
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
  """ string_element : identifier LBRACKET ICONST RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

def p_string_slice(p):
  """ string_slice : identifier LBRACKET ICONST COLON ICONST RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

def p_array_element(p):
  """ array_element : location LBRACKET expression_list RBRACKET
  """
  p[0] = [p[1], p[3]]

def p_expression_list(p):
  """ expression_list : expression
                      | expression_list COMMA expression
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_array_slice(p):
  """ array_slice : location LBRACKET ICONST COLON ICONST RBRACKET
  """
  p[0] = [p[1], p[3], p[5]]

########################  BLOCO 6  #############################

def p_primitive_value(p):
  """  primitive_value : literal
                       | value_array_element
                       | value_array_slice
                       | parenthesized_expression
  """
  p[0] = p[1]

def p_literal(p):
  """  literal : ICONST
               | FALSE
               | TRUE
               | CCONST
               | NULL
               | SCONST
  """
  p[0] = Constant(p[1])


########################  BLOCO 7  #############################

def p_value_array_element(p):
  """ value_array_element : array_primitive_value LBRACKET expression_list RBRACKET
  """
  p[0] = [p[1], p[3]]

def p_value_array_slice(p):
  """ value_array_slice : array_primitive_value LBRACKET expression COLON expression RBRACKET
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
  p[0] = Expr(p[1])

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
  	p[0] = Binop(p[2], p[1], p[3])

def p_operand(p):
  """ operand : MINUS operand1
  	      | NOT operand1
              | primitive_value
              | identifier
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
  """ action_statement : identifier COLON action SEMI
                       | action SEMI
  """
  if len(p) == 3:
    p[0] = ActionStmt(None, p[1])
  else:
    p[0] = ActionStmt(p[1], p[3])

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
                        | identifier assigning_operator expression
  """
  p[0] = Assignment(p[1], p[2], p[3])

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
  """ then_clause : THEN action_statement
  		  | THEN 
  """
  if len(p) == 3:
    p[0] = [p[1], p[2]]
  else:
    p[0] = p[1]
  
def p_else_clause(p):
  """ else_clause : ELSE action_statement
  		  | ELSE
                  | ELSIF boolean_expression then_clause else_clause
                  | ELSIF boolean_expression then_clause
  """
  if len(p) == 2:
    p[0] = p[1]
  elif len(p) == 3:
    p[0] = p[2]
  elif len(p) == 5:
    p[0] = [p[2], p[3], p[4]]
  else:
    p[0] = [p[2], p[3]]

########################### BLOCO 11 ##################################
def p_do_action(p):
  """ do_action : DO control_part SEMI action_statement OD
                | DO control_part SEMI OD
                | DO action_statement OD
  """
  if len(p) == 4:
    p[0] = [p[1], p[2], p[3]]
  elif len(p) == 5:
    p[0] = [p[1], p[2], p[3], p[4]]
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
  """ procedure_call : identifier LPAREN parameter_list RPAREN
                     | identifier LPAREN RPAREN
  """
  if len(p) == 4:
    p[0] = [p[1], p[3]]
  else:
    p[0] = p[1]

def p_parameter_list(p):
  """ parameter_list : expression
                     | parameter_list COMMA expression
  """ 
  p[0] = p[1]

def p_exit_action(p):
  """ exit_action : EXIT identifier """
  p[0] = [p[1], p[2]]

def p_return_action(p):
  """ return_action : RETURN expression 
                    | RETURN
  """
  if len(p) == 2:
	p[0] = none
  else:
	p[0] = p[2]

def p_result_action(p):
  """ result_action : RESULT expression """
  p[0] = [p[1], p[2]]

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
  """ procedure_statement : identifier COLON procedure_definition SEMI """
  p[0] = Procedure(p[1], p[3])

def p_procedure_definition(p):
  """ procedure_definition : PROC LPAREN formal_parameter_list RPAREN result_spec SEMI statement_list END 
  			                   | PROC LPAREN formal_parameter_list RPAREN SEMI statement_list END 
                           | PROC LPAREN RPAREN SEMI statement_list END
  """
  if len(p) == 9:
    p[0] = ProcDef(p[3], p[5], p[7])
  elif len(p) == 8:
    p[0] = ProcDef(p[3], None, p[6])
  else:
    p[0] = ProcDef(None, p[5], None)


def p_formal_parameter_list(p):
  """ formal_parameter_list : formal_parameter
                            | formal_parameter_list COMMA formal_parameter

  """
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = p[1] + p[3]

def p_formal_parameter(p):
  """ formal_parameter : identifier_list parameter_spec """
  p[0] = FormalParam(p[1], p[2])

def p_result_spec(p):
  """ result_spec : RETURNS LPAREN parameter_spec RPAREN """
  p[0] = [p[1], p[3]]
    
def p_parameter_spec(p):
  """ parameter_spec : mode LOC
                     | mode
  """
  if len(p)== 3:
    p[0] = [p[1], p[2]]
  else:
    p[0] = p[1]

####################################### AST ###############################################


	 

###########################################################################################
  
# Error rule for syntax errors
def p_error(p):
    if p:
   	print("Syntax error at '%s'" % repr(p))
    	#print "Syntax error at token ", p.type
    	print "\n"
    	parser.errok()
   
#def p_error(p):
 #   if p:
 #	print("Syntax error at '%s'" % repr(p))
  #	print "\n"
    # Read ahead looking for a terminating ";"
   # while 1:
    #    tok = parser.token()             # Get the next token
     #   if not tok or tok.type == 'SEMI': break
   # parser.errok()


class Node(object):
    __slots__ = ()
    """ Abstract base class for AST nodes.
    """
    def children(self):
        """ A sequence of all children that are Nodes
        """
        pass

    def show(self, buf=sys.stdout, offset=0, attrnames=False, nodenames=False, showcoord=False, _my_node_name=None):
        """ Pretty print the Node and all its attributes and
            children (recursively) to a buffer.

            buf:
                Open IO buffer into which the Node is printed.

            offset:
                Initial offset (amount of leading spaces)

            attrnames:
                True if you want to see the attribute names in
                name=value pairs. False to only see the values.

            nodenames:
                True if you want to see the actual node names
                within their parents.

            showcoord:
                Do you want the coordinates of each Node to be
                displayed.
        """
        lead = ' ' * offset
        if nodenames and _my_node_name is not None:
            buf.write(lead + self.__class__.__name__+ ' <' + _my_node_name + '>: ')
        else:
            buf.write(lead + self.__class__.__name__+ ': ')

        if self.attr_names:
            if attrnames:
                nvlist = [(n, getattr(self,n)) for n in self.attr_names]
                attrstr = ', '.join('%s=%s' % nv for nv in nvlist)
            else:
                vlist = [getattr(self, n) for n in self.attr_names]
                attrstr = ', '.join('%s' % v for v in vlist)
            buf.write(attrstr)

        if showcoord:
            buf.write(' (at %s)' % self.coord)
        buf.write('\n')

        for (child_name, child) in self.children():
            child.show(
                buf,
                offset=offset + 2,
                attrnames=attrnames,
                nodenames=nodenames,
                showcoord=showcoord,
                _my_node_name=child_name)


class NodeVisitor(object):
    """ A base NodeVisitor class for visiting c_ast nodes.
        Subclass it and define your own visit_XXX methods, where
        XXX is the class name you want to visit with these
        methods.

        For example:

        class ConstantVisitor(NodeVisitor):
            def __init__(self):
                self.values = []

            def visit_Constant(self, node):
                self.values.append(node.value)

        Creates a list of values of all the constant nodes
        encountered below the given node. To use it:

        cv = ConstantVisitor()
        cv.visit(node)

        Notes:

        *   generic_visit() will be called for AST nodes for which
            no visit_XXX method was defined.
        *   The children of nodes for which a visit_XXX was
            defined will not be visited - if you need this, call
            generic_visit() on the node.
            You can use:
                NodeVisitor.generic_visit(self, node)
        *   Modeled after Python's own AST visiting facilities
            (the ast module of Python 3.0)
    """

    def visit(self, node):
        """ Visit a node.
        """
        method = 'visit_' + node.__class__.__name__
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        """ Called if no explicit visitor function exists for a
            node. Implements preorder visiting of the node.
        """
        for c_name, c in node.children():
            self.visit(c)


# class Expr: pass

class Program(Node):
    def __init__(self, statement_list):
        self.type = "program"
        self.statement_list = statement_list
    attr_names = ()

    def children(self):
      nodelist = []
      if self.statement_list is not None: nodelist.append(("statement_list", self.statement_list))
      return tuple(nodelist)

class Statement_list(Node):
    def __init__(self, statements):
        self.type = "statement_list"
        self.statements = statements
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.statements or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)
        
class Statement(Node):
    def __init__(self, statement):
        self.type = "statement"
        self.statement = statement
    attr_names = ()

    def children(self):
      nodelist = []
      if self.statement is not None: nodelist.append(("statement", self.statement))
      return tuple(nodelist)
        
class DCL_statement(Node):
    def __init__(self, declarations):
        self.type = "DCL_statement"
        self.declarations = declarations
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.declarations or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)
      

class Declaration(Node):
    def __init__(self, identifier_list, mode, initialization=None):
        self.type = "Declaration"
        self.identifier_list = identifier_list
        self.mode = mode
        self.initialization = initialization
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.identifier_list or []):
          nodelist.append(("exprs[%d]" % i, child))
      if self.initialization is not None: nodelist.append(("initialization", self.initialization))
      if self.mode is not None: nodelist.append(("mode", self.mode))
      return tuple(nodelist)

class ID(Node):
    def __init__(self, char):
        self.type = "ID"
        self.char = char
    attr_names = ("char",)

    def children(self):
        nodelist = []
        return tuple(nodelist)

class Mode(Node):
    def __init__(self, mode):
        self.type = "Mode"
        self.mode = mode
    attr_names = ()

    def children(self):
        nodelist = []
        if self.mode is not None: nodelist.append(("mode", self.mode))
        return tuple(nodelist)

class DiscreteMode(Node):
    def __init__(self, mode):
        self.type = "DiscreteMode"
        self.mode = mode
    attr_names = ("mode",)

    def children(self):
        nodelist = []
        return tuple(nodelist)

class Expr(Node):
    def __init__(self, exp):
        self.type = "Expr"
        self.exp = exp
    attr_names = ()

    def children(self):
        nodelist = []
        if self.exp is not None: nodelist.append(("exp", self.exp))
        return tuple(nodelist)

class Constant(Node):
    def __init__(self, exp):
        self.type = "Constant"
        self.exp = exp
    attr_names = ("exp",)

    def children(self):
        nodelist = []
        return tuple(nodelist)

class Procedure(Node):
    def __init__(self, identifier, procedure):
        self.type = "Procedure"
        self.identifier = identifier
        self.procedure = procedure
    attr_names = ()

    def children(self):
        nodelist = []
        if self.identifier is not None: nodelist.append(("identifier", self.identifier))
        if self.procedure is not None: nodelist.append(("procedure", self.procedure))
        return tuple(nodelist)

class ProcDef(Node):
    def __init__(self, formal_parameter_list, result_spec, statement_list):
        self.type = "Procedure"
        self.formal_parameter_list = formal_parameter_list
        self.result_spec = result_spec
        self.statement_list = statement_list

    attr_names = ()

    def children(self):
        nodelist = []
        if self.statement_list is not None: nodelist.append(("statement_list", self.statement_list))
        for i, child in enumerate(self.formal_parameter_list or []):
          nodelist.append(("exprs[%d]" % i, child))
        return tuple(nodelist)

class FormalParam(Node):
    def __init__(self, id_list, param_spec):
        self.type = "formal_param"
        self.id_list = id_list
        self.param_spec = param_spec
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.id_list or []):
          nodelist.append(("exprs[%d]" % i, child))
      if self.param_spec is not None: nodelist.append(("param_spec", self.param_spec))
      return tuple(nodelist)

class ActionStmt(Node):
    def __init__(self, identifier, action):
        self.type = "action_statement"
        self.identifier = identifier
        self.action = action
    attr_names = ()

    def children(self):
      nodelist = []
      if self.identifier is not None: nodelist.append(("identifier", self.identifier))
      if self.action is not None: nodelist.append(("action", self.action))
      return tuple(nodelist)

class Assignment(Node):
    def __init__(self, location, op, expr):
        self.type = "assignment"
        self.location = location
        self.op = op
        self.expr = expr
    attr_names = ("op",)

    def children(self):
      nodelist = []
      if self.location is not None: nodelist.append(("location", self.location))
      if self.expr is not None: nodelist.append(("expr", self.expr))
      return tuple(nodelist)

class Binop(Node):
    def __init__(self, op, left, right):
        self.type = "assignment"
        self.left = left
        self.op = op
        self.right = right
    attr_names = ("op",)

    def children(self):
      nodelist = []
      if self.left is not None: nodelist.append(("left", self.left))
      if self.right is not None: nodelist.append(("right", self.right))
      return tuple(nodelist)

# Build the parser
parser = yacc.yacc()

p = parser.parse(f.read(),tracking=True)

p.show()