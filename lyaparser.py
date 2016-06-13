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
    p[0] = StmtList([p[1]])
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
    p[0] = DclStmt(p[2])
  else:
    p[0] = DclStmt(p[2])

def p_declaration_list(p):
  """ declaration_list : declaration
                       | declaration_list COMMA declaration
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

def p_declaration(p):
  """ declaration : identifier_list mode initialization
                  | identifier_list mode
  """
  p[0] = Decl(p[1], p[2]) if len(p) == 3 else Decl(p[1], p[2], p[3])

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
  p[0] = ID(p[1], lineno=p.lineno(1))


########################  BLOCO 3  #############################

def p_synonym_statement(p):
  ' synonym_statement : SYN synonym_list SEMI'
  p[0] = p[2]

def p_synonym_list(p):
  """ synonym_list : synonym_definition
                   | synonym_list COMMA synonym_definition
  """
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = p[1] + [p[3]]

def p_synonym_definition(p):
  """ synonym_definition : identifier_list mode ASSIGN constant_expression
                         | identifier_list ASSIGN constant_expression
  """
  if len(p) == 5:
    p[0] = Syn(p[1], p[4], p[2])
  else:
    p[0] = Syn(p[1], p[3])

def p_constant_expression(p):
  ' constant_expression : expression'
  p[0] = p[1]

########################  BLOCO 4  #############################

def p_newmode_statement(p):
  ' newmode_statement : TYPE newmode_list SEMI'
  p[0] = Type(p[2])

def p_newmode_list(p):
  """ newmode_list : mode_definition
                   | newmode_list COMMA mode_definition
  """
  if len(p) == 2:
      p[0] = [p[1]]
  else:
      p[0] = p[1] + [p[3]]

def p_mode_definition(p):
  ' mode_definition : identifier_list ASSIGN mode '
  p[0] = ModeDef(p[1], p[2], p[3])

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
  if isinstance(p[1], int) or isinstance(p[1], bool) or isinstance(p[1], str):
    p[0] = DiscreteMode(p[1], None, lineno=p.lineno(1))
  else:
    p[0] = DiscreteMode(None, p[1], lineno=p.lineno(1))

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
  p[0] = DiscreteRange(p[1], p[3])

def p_mode_name(p):
  ' mode_name : identifier '
  p[0] = p[1]

def p_discrete_mode_name(p):
  ' discrete_mode_name : identifier '
  p[0] = p[1]

def p_literal_range(p):
  ' literal_range : lower_bound COLON upper_bound '
  p[0] = Range(p[1], p[3])

def p_reference_mode(p):
  ' reference_mode : REF mode '
  p[0] = Ref(p[2])

def p_composite_mode(p):
  """ composite_mode : string_mode
                     | array_mode
  """
  p[0] = p[1]

def p_string_mode(p):
  ' string_mode : CHARS LBRACKET string_length RBRACKET '
  p[0] = StrLen(p[3])

def p_string_length(p):
  ' string_length : ICONST '
  p[0] = p[1]

def p_array_mode(p):
  """ array_mode : ARRAY LBRACKET index_mode RBRACKET element_mode
                 | ARRAY LBRACKET index_mode COMMA index_mode_list RBRACKET element_mode
  """
  if len(p) == 6:
    p[0] = ArrayMode(p[3], p[5])
  else:
    p[0] =  + ArrayMode(p[3]+[p[5]], p[7])

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
               | location LBRACKET lower_bound COLON upper_bound RBRACKET
               | location ARROW
               | location LBRACKET expression_list RBRACKET
               | call_action
  """
  if len(p) == 5:
    p[0] = Array(p[1], p[3])
  elif len(p) == 3:
    p[0] = DerefRef(p[1], p[2])
  elif len(p) == 7:
    p[0] = ArraySlice(p[1], p[3], p[5])
  else:
    p[0] = p[1]

def p_lower_bound(p):
  """ lower_bound : expression """
  p[0] = p[1]

def p_upper_bound(p):
  """ upper_bound : expression """
  p[0] = p[1]

def p_expression_list(p):
  """ expression_list : expression
                      | expression_list COMMA expression
  """
  p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]


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
  if isinstance(p[1], int):
    p[0] = Constant(p[1], p.lineno(1), "int")
  elif p[1] == 'true' or p[1] == 'false':
    p[0] = Constant(p[1], p.lineno(1), "bool")
  elif p[1][0] == "\'":
    p[0] = Constant(p[1], p.lineno(1), "char")
  elif isinstance(p[1], str):
    p[0] = Constant(p[1], p.lineno(1), "string")
  else:
    p[0] = Constant(p[1], p.lineno(1), "null")

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
  if len(p) == 6:
   p[0] = CondExpr(p[2], p[3], p[4], None)
  else:
   p[0] = CondExpr(p[2], p[3], p[5], p[4])

def p_boolean_expression(p):
  """ boolean_expression : expression
  """
  p[0] = BoolExpr(p[1])

def p_then_expression(p):
  """ then_expression : THEN expression
  """
  p[0] = ThenExpr(p[2])

def p_else_expression(p):
  """ else_expression : ELSE expression
  """
  p[0] = ElseExpr(p[2])

def p_elsif_expression(p):
  """ elsif_expression : ELSIF boolean_expression then_expression
                       | elsif_expression ELSIF boolean_expression then_expression
  """
  # if len(p) == 4:
  #   p[0] = Elsif(p[2], p[3], None)
  # else:
  #   p[0] = Elsif(p[3], p[4], p[1])
  if len(p) == 4: # single initializer
      expr = Elsif(p[2], p[3])
      p[0] = ElsifList([expr])
  else:
      expr = Elsif(p[3], p[4])
      p[1].exprs.append(expr)
      p[0] = p[1]


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
    p[0] = Binop(p[2], p[1], p[3], p.lineno(1))

def p_operand(p):
  """ operand : MINUS operand1
              | NOT operand1
              | operand1
  """
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = Operand(p[1], p[2])

def p_operand1(p):
  """ operand1 : location
               | referenced_location
               | primitive_value
  """
  p[0] = p[1]

def p_referenced_location(p):
  """ referenced_location : ARROW location
  """
  p[0] = RefLoc(p[1], p[2])

########################### BLOCO 9 ##################################
def p_action_statement(p):
  """ action_statement : label_id COLON action SEMI
                       | action SEMI
  """
  if len(p) == 3:
    p[0] = ActionStmt(None, p[1])
  else:
    p[0] = ActionStmt(p[1], p[3])

def p_label_id(p):
  """ label_id : identifier
  """
  p[0] = p[1]

def p_action_statement_list(p):
  """ action_statement_list : action_statement
                            | action_statement_list action_statement
  """
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = p[1] + [p[2]]

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
    p[0] = IfAction(p[2], p[3], p[4])
  else:
    p[0] = IfAction(p[2], p[3])

def p_then_clause(p):
  """ then_clause : THEN action_statement_list
        | THEN
  """
  if len(p) == 3:
    p[0] = ThenClause(p[2])
  else:
    p[0] = ThenClause(None)

def p_else_clause(p):
  """ else_clause : ELSE action_statement_list
        | ELSE
  """
  if len(p) == 2:
    p[0] = ElseClause(None)
  else:
    p[0] = ElseClause(p[2])

########################### BLOCO 11 ##################################
def p_do_action(p):
  """ do_action : DO control_part SEMI action_statement_list OD
                | DO control_part SEMI OD
                | DO action_statement_list OD
  """
  if len(p) == 4:
    p[0] = DoAction(None, p[2])
  elif len(p) == 5:
    p[0] = DoAction(p[2], None)
  else:
    p[0] = DoAction(p[2], p[4])

def p_control_part(p):
  """ control_part : for_control while_control
                   | for_control
                   | while_control
  """
  if len(p) == 3:
    p[0] = Control(p[1], p[2])
  elif isinstance(p[1], ForControl):
    print ("passou no for_control")
    p[0] = Control(p[1], None)
  else: 
    print ("passou no while_control")  
    p[0] = Control(None, p[1])

def p_for_control(p):
  """ for_control : FOR iteration
  """
  p[0] = ForControl(p[2])

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
    p[0] = StepEnum(p[1], p[2], p[3], p[4])
  elif(p[4] == 'DOWN' and len(p) == 5):
    p[0] = StepEnum(p[1], p[2], p[3], p[5])
  elif len(p) == 6:
    p[0] = StepEnum(p[1], p[2], p[3], p[5], p[4])
  else:
    p[0] = StepEnum(p[1], p[2], p[3], p[6], p[4])

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
  """ range_enumeration : loop_counter DOWN IN discrete_mode
                        | loop_counter IN discrete_mode
  """
  if len(p) == 5:
    p[0] = RangeEnum(p[1], p[4])
  else:
    p[0] = RangeEnum(p[1], p[3])

def p_while_control(p):
  """ while_control : WHILE boolean_expression
  """
  p[0] = WhileControl(p[2])

########################### BLOCO 12 ##################################
def p_call_action(p):
  """ call_action : procedure_call
                  | builtin_call
  """
  p[0] = p[1]

def p_procedure_call(p):
  """ procedure_call : discrete_mode_name LPAREN parameter_list RPAREN
                     | discrete_mode_name LPAREN RPAREN
  """
  if len(p) == 5:
    p[0] = ProcCall(p[1], p[3])
  else:
    p[0] = ProcCall(p[1], None)

def p_parameter_list(p):
  """ parameter_list : expression
                     | parameter_list COMMA expression
  """
  if len(p) == 2:
    p[0] = Param([p[1]])
  else:
    p[1].param.append(p[3])
    p[0] = p[1]



def p_exit_action(p):
  """ exit_action : EXIT identifier """
  p[0] = Exit(p[2])

def p_return_action(p):
  """ return_action : RETURN expression
                    | RETURN
  """
  if len(p) == 2:
    p[0] = Return(None)
  else:
    p[0] = Return(p[2])

def p_result_action(p):
  """ result_action : RESULT expression"""
  p[0] = Result(p[2])

def p_builtin_call(p):
  """ builtin_call : builtin_name LPAREN parameter_list RPAREN
                   | builtin_name LPAREN RPAREN
  """
  if len(p) == 4:
    p[0] = Call(p[1])
  else:
    p[0] = Call(p[1], p[3])

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

  p[0] = Func(p[1])

########################### BLOCO 13 ##################################
def p_procedure_statement(p):
  """ procedure_statement : label_id COLON procedure_definition SEMI """
  p[0] = ProcStmt(p[1], p[3])

def p_procedure_definition(p):
  """ procedure_definition : PROC LPAREN formal_parameter_list RPAREN result_spec SEMI statement_list END
                           | PROC LPAREN formal_parameter_list RPAREN SEMI statement_list END
                           | PROC LPAREN RPAREN result_spec SEMI statement_list END
                           | PROC LPAREN RPAREN SEMI statement_list END
  """
  if len(p) == 9:
    p[0] = ProcDef(p[3], p[5], p[7], lineno=p.lineno(1))
  elif len(p) == 8 and p[3] != ")":
    p[0] = ProcDef(p[3], None, p[6], lineno=p.lineno(1))
  elif len(p) == 8:
    p[0] = ProcDef(None, p[4], p[6], lineno=p.lineno(1))
  else:
    p[0] = ProcDef(None, None, p[5], lineno=p.lineno(1))


def p_formal_parameter_list(p):
  """ formal_parameter_list : formal_parameter
                            | formal_parameter_list COMMA formal_parameter

  """
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = p[1] + [p[3]]

def p_formal_parameter(p):
  """ formal_parameter : identifier_list parameter_spec """
  p[0] = FormalParam(p[1], p[2])

def p_result_spec(p):
  """ result_spec : RETURNS LPAREN parameter_spec RPAREN """
  p[0] = Returns(p[3])

def p_parameter_spec(p):
  """ parameter_spec : mode LOC
                     | mode
  """
  if len(p)== 3:
    p[0] = ParamSpec(p[1], p[2])
  else:
    p[0] = ParamSpec(p[1], None)

###################################### Error ##############################################

# Error rule for syntax errors
def p_error(p):
    if p:
      print("Syntax error at '%s'" % repr(p))
      print("\n")
      parser.errok()
      

####################################### AST ###############################################

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

        if showcoord and hasattr(self, 'lineno'):
            buf.write(' (at line: %s)' % self.lineno)
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

class StmtList(Node):
    def __init__(self, statements):
        self.type = "stmtlist"
        self.statements = statements
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.statements or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)

class DclStmt(Node):
    def __init__(self, declarations):
        self.type = "DclStmt"
        self.declarations = declarations
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.declarations or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)


class Decl(Node):
    def __init__(self, identifier_list, mode, initialization=None):
        self.type = "Decl"
        self.identifier_list = identifier_list
        self.mode = mode
        self.initialization = initialization
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.identifier_list or []):
          nodelist.append(("exprs[%d]" % i, child))
      if self.mode is not None: nodelist.append(("mode", self.mode))
      if self.initialization is not None: nodelist.append(("initialization", self.initialization))
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

class ParamSpec(Node):
  def __init__(self, mode, attr):
      self.type = "paramspec"
      self.mode = mode
      self.attr = attr
  attr_names = ("attr",)

  def children(self):
    nodelist = []
    if self.mode is not None: nodelist.append(("mode", self.mode))
    return tuple(nodelist)


class ID(Node):
    def __init__(self, char, lineno):
        self.ttype = "ID"
        self.lineno = lineno
        self.char = char
        self.loc = None
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
    def __init__(self, type, mode, lineno):
        self.type = "DiscreteMode"
        self.type = type
        self.mode = mode
        self.lineno = lineno
    attr_names = ("type",)

    def children(self):
        nodelist = []
        if self.mode is not None: nodelist.append(("mode", self.mode))
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

class BoolExpr(Node):
    def __init__(self, exp):
        self.type = "Expr"
        self.exp = exp
    attr_names = ()

    def children(self):
        nodelist = []
        if self.exp is not None: nodelist.append(("exp", self.exp))
        return tuple(nodelist)

class Constant(Node):
    def __init__(self, exp, lineno, type):
        self.ttype = "Constant"
        self.exp = exp
        self.type = type
        self.lineno = lineno
    attr_names = ("exp", "type",)

    def children(self):
        nodelist = []
        return tuple(nodelist)

class ProcStmt(Node):
    def __init__(self, identifier, procedure):
        self.type = "ProcStmt"
        self.identifier = identifier
        self.procedure = procedure
    attr_names = ()

    def children(self):
        nodelist = []
        if self.identifier is not None: nodelist.append(("identifier", self.identifier))
        if self.procedure is not None: nodelist.append(("procedure", self.procedure))
        return tuple(nodelist)

class ProcDef(Node):
    def __init__(self, formal_parameter_list, result_spec, statement_list, lineno):
        self.type = "Procedure"
        self.formal_parameter_list = formal_parameter_list
        self.result_spec = result_spec
        self.statement_list = statement_list
        self.lineno = lineno

    attr_names = ("")

    def children(self):
        nodelist = []
        for i, child in enumerate(self.formal_parameter_list or []):
          nodelist.append(("exprs[%d]" % i, child))
        if self.result_spec is not None: nodelist.append(("result_spec", self.result_spec))
        if self.statement_list is not None: nodelist.append(("statement_list", self.statement_list))
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



class ActionStmt_List(Node):
    def __init__(self, actions):
        self.type = "action_statement_list"
        self.actions = actions
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.action or []):
        nodelist.append(("exprs[%d]" % i, child))
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
    def __init__(self, op, left, right, lineno):
        self.ttype = "binop"
        self.left = left
        self.op = op
        self.right = right
        self.lineno = lineno
    attr_names = ("op",)

    def children(self):
      nodelist = []
      if self.left is not None: nodelist.append(("left", self.left))
      if self.right is not None: nodelist.append(("right", self.right))
      return tuple(nodelist)

class Call(Node):
    def __init__(self, op, param=None):
        self.type = "call"
        self.op = op
        self.param = param
    attr_names = ()

    def children(self):
      nodelist = []
      if self.op is not None: nodelist.append(("op", self.op))
      if self.param is not None: nodelist.append(("param", self.param))
      return tuple(nodelist)

class Func(Node):
    def __init__(self, op):
        self.type = "func"
        self.op = op
    attr_names = ("op",)

    def children(self):
      nodelist = []
      return tuple(nodelist)

class ProcCall(Node):
    def __init__(self, op, param):
        self.type = "proccall"
        self.op = op
        self.param = param
    attr_names = ()

    def children(self):
      nodelist = []
      if self.op is not None: nodelist.append(("op", self.op))
      if self.param is not None: nodelist.append(("param", self.param))
      return tuple(nodelist)

class Param(Node):
    def __init__(self, param):
        self.type = "param"
        self.param = param
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.param or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)


class IfAction(Node):
    def __init__(self, bool_exp, then_c, else_c=None):
        self.type = "ifaction"
        self.bool_exp = bool_exp
        self.then_c = then_c
        self.else_c = else_c
    attr_names = ()

    def children(self):
      nodelist = []
      if self.bool_exp is not None: nodelist.append(("bool_exp", self.bool_exp))
      if self.then_c is not None: nodelist.append(("then_c", self.then_c))
      if self.else_c is not None: nodelist.append(("else_c", self.else_c))
      return tuple(nodelist)

class ThenClause(Node):
    def __init__(self, action_list):
        self.type = "thenclause"
        self.action_list = action_list
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.action_list or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)

class Result(Node):
    def __init__(self, expr):
        self.type = "result"
        self.expr = expr
    attr_names = ()

    def children(self):
      nodelist = []
      if self.expr is not None: nodelist.append(("expr", self.expr))
      return tuple(nodelist)

class ElseClause(Node):
    def __init__(self, action_list):
        self.type = "elseclause"
        self.action_list = action_list
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.action_list or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)

class DoAction(Node):
    def __init__(self, control, action_list):
        self.type = "doaction"
        self.action_list = action_list
        self.control = control
    attr_names = ()

    def children(self):
      nodelist = []
      if self.control is not None: nodelist.append(("control", self.control))
      for i, child in enumerate(self.action_list or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)

class Control(Node):
    def __init__(self, forcontrol, whilecontrol):
        self.ttype = "control"
        self.forcontrol = forcontrol 
        self.whilecontrol = whilecontrol
    attr_names = ()

    def children(self):
      nodelist = []
      if self.forcontrol is not None: nodelist.append(("forcontrol", self.forcontrol))
      if self.whilecontrol is not None: nodelist.append(("whilecontrol", self.whilecontrol))
      return tuple(nodelist)

class ForControl(Node):
    def __init__(self, iteration):
        self.ttype = "forcontrol"
        self.iteration = iteration
    attr_names = ()

    def children(self):
      nodelist = []
      if self.iteration is not None: nodelist.append(("iteration", self.iteration))
      return tuple(nodelist)

class WhileControl(Node):
    def __init__(self, bool_exp):
        self.ttype = "whilecontrol"
        self.bool_exp = bool_exp
    attr_names = ()

    def children(self):
      nodelist = []
      if self.bool_exp is not None: nodelist.append(("bool_exp", self.bool_exp))
      return tuple(nodelist)

class Returns(Node):
    def __init__(self, param):
        self.type = "returns"
        self.param = param
    attr_names = ()

    def children(self):
      nodelist = []
      if self.param is not None: nodelist.append(("param", self.param))
      return tuple(nodelist)

class Return(Node):
    def __init__(self, param):
        self.type = "return"
        self.param = param
    attr_names = ()

    def children(self):
      nodelist = []
      if self.param is not None: nodelist.append(("param", self.param))
      return tuple(nodelist)

class Operand(Node):
    def __init__(self, op, ident):
        self.type = "operand"
        self.op = op
        self.ident = ident
    attr_names = ("op",)

    def children(self):
      nodelist = []
      if self.ident is not None: nodelist.append(("ident", self.ident))
      return tuple(nodelist)

class ArrayMode(Node):
    def __init__(self, index_mode, element_mode):
        self.type = "arraymode"
        self.index_mode = index_mode
        self.element_mode = element_mode
    attr_names = ()

    def children(self):
      nodelist = []
      if self.index_mode is not None: nodelist.append(("index_mode", self.index_mode))
      if self.element_mode is not None: nodelist.append(("element_mode", self.element_mode))
      return tuple(nodelist)

class Array(Node):
    def __init__(self, location, expr):
        self.type = "array"
        self.location = location
        self.expr = expr
    attr_names = ()

    def children(self):
      nodelist = []
      if self.location is not None: nodelist.append(("location", self.location))
      for i, child in enumerate(self.expr or []):
          nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)

class Range(Node):
    def __init__(self, i1, i2):
        self.type = "range"
        self.i1 = i1
        self.i2 = i2
    attr_names = ()

    def children(self):
      nodelist = []
      if self.i1 is not None: nodelist.append(("i1", self.i1))
      if self.i2 is not None: nodelist.append(("i2", self.i2))
      return tuple(nodelist)

class StepEnum(Node):
    def __init__(self, loop, assign, start, end, step=None):
        self.type = "stepenum"
        self.loop = loop
        self.assign = assign
        self.start = start
        self.end = end
        self.step = step
    attr_names = ('assign',)

    def children(self):
      nodelist = []
      if self.loop is not None: nodelist.append(("loop", self.loop))
      if self.start is not None: nodelist.append(("start", self.start))
      if self.end is not None: nodelist.append(("end", self.end))
      if self.step is not None: nodelist.appstep(("step", self.end))
      return tuple(nodelist)

class Exit(Node):
    def __init__(self, ident):
        self.type = "exit"
        self.ident = ident
    attr_names = ()

    def children(self):
      nodelist = []
      if self.ident is not None: nodelist.append(("ident", self.ident))
      return tuple(nodelist)

class Syn(Node):
    def __init__(self, ident, expr, mode=None):
        self.type = "syn"
        self.ident = ident
        self.expr = expr
        self.mode = mode
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.ident or []):
        nodelist.append(("exprs[%d]" % i, child))
      if self.mode is not None: nodelist.append(("mode", self.mode))
      if self.expr is not None: nodelist.append(("expr", self.expr))
      return tuple(nodelist)

class Type(Node):
    def __init__(self, newmode):
        self.type = "type"
        self.newmode = newmode
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.newmode or []):
        nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)

class ModeDef(Node):
    def __init__(self, idents, assign, mode):
        self.type = "modedef"
        self.idents = idents
        self.mode = mode
        self.assign = assign
    attr_names = ("assign",)

    def children(self):
      nodelist = []
      for i, child in enumerate(self.idents or []):
        nodelist.append(("exprs[%d]" % i, child))
      if self.mode is not None: nodelist.append(("mode", self.mode))
      return tuple(nodelist)

class DiscreteRange(Node):
    def __init__(self, discr, liter):
        self.type = "discreterange"
        self.discr = discr
        self.liter = liter
    attr_names = ()

    def children(self):
      nodelist = []
      if self.discr is not None: nodelist.append(("discr", self.discr))
      if self.liter is not None: nodelist.append(("liter", self.liter))

      return tuple(nodelist)

class RangeEnum(Node):
    def __init__(self, loop, mode):
        self.type = "rangeenum"
        self.loop = loop
        self.mode = mode
    attr_names = ()

    def children(self):
      nodelist = []
      if self.loop is not None: nodelist.append(("loop", self.loop))
      if self.mode is not None: nodelist.append(("mode", self.mode))

      return tuple(nodelist)

class Ref(Node):
    def __init__(self, mode):
        self.type = "ref"
        self.mode = mode
    attr_names = ()

    def children(self):
      nodelist = []
      if self.mode is not None: nodelist.append(("mode", self.mode))

      return tuple(nodelist)

class RefLoc(Node):
    def __init__(self, ref, loc):
        self.type = "refloc"
        self.ref = ref
        self.loc = loc
    attr_names = ("ref", )

    def children(self):
      nodelist = []
      if self.loc is not None: nodelist.append(("loc", self.loc))

      return tuple(nodelist)

class StrLen(Node):
    def __init__(self, slen):
        self.type = "strlen"
        self.slen = slen
        self.type = "string"
    attr_names = ("slen", "type")

    def children(self):
      nodelist = []
      return tuple(nodelist)

class CondExpr(Node):
    def __init__(self, boolex, thenex, elseex, elsifex):
        self.type = "condexpr"
        self.boolex = boolex
        self.thenex = thenex
        self.elseex = elseex
        self.elsifex = elsifex
    attr_names = ()

    def children(self):
      nodelist = []
      if self.boolex is not None: nodelist.append(("boolex", self.boolex))
      if self.thenex is not None: nodelist.append(("thenex", self.thenex))
      if self.elsifex is not None: nodelist.append(("elsifex", self.elsifex))
      if self.elseex is not None: nodelist.append(("elseex", self.elseex))

      return tuple(nodelist)

class ElsifList(Node):
    def __init__(self, exprs):
        self.type = "elsiflist"
        self.exprs = exprs
    attr_names = ()

    def children(self):
      nodelist = []
      for i, child in enumerate(self.exprs or []):
        nodelist.append(("exprs[%d]" % i, child))
      return tuple(nodelist)

class Elsif(Node):
    def __init__(self, boolex, thenex):
        self.type = "elsif"
        self.boolex = boolex
        self.thenex = thenex
    attr_names = ()

    def children(self):
      nodelist = []
      if self.boolex is not None: nodelist.append(("boolex", self.boolex))
      if self.thenex is not None: nodelist.append(("thenex", self.thenex))

      return tuple(nodelist)

class ThenExpr(Node):
    def __init__(self, exp):
        self.type = "thenexpr"
        self.exp = exp
    attr_names = ()

    def children(self):
        nodelist = []
        if self.exp is not None: nodelist.append(("exp", self.exp))
        return tuple(nodelist)

class ElseExpr(Node):
    def __init__(self, exp):
        self.type = "elseexpr"
        self.exp = exp
    attr_names = ()

    def children(self):
        nodelist = []
        if self.exp is not None: nodelist.append(("exp", self.exp))
        return tuple(nodelist)

class DerefRef(Node):
    def __init__(self, location, arrow):
        self.type = "DerefRef"
        self.location = location
        self.arrow = arrow
    attr_names = ("arrow",)

    def children(self):
        nodelist = []
        if self.location is not None: nodelist.append(("location", self.location))
        return tuple(nodelist)

class ArraySlice(Node):
    def __init__(self, location, lower, upper):
        self.type = "arrayslice"
        self.location = location
        self.lower = lower
        self.upper = upper
    attr_names = ()

    def children(self):
        nodelist = []
        if self.location is not None: nodelist.append(("location", self.location))
        if self.lower is not None: nodelist.append(("lower", self.lower))
        if self.upper is not None: nodelist.append(("upper", self.upper))
        return tuple(nodelist)

# Build the parser
parser = yacc.yacc()

p = parser.parse(f.read(),tracking=True)

p.show(showcoord = True)
