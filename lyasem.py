import sys
from lyaparser import *

#################################### Symbol Table ########################################      

class SymbolTable(dict):
    def __init__(self, decl=None):
        super().__init__()
        self.decl = decl
    def insert(self, name, value):
        self[name] = value
    def lookup(self, name):
        return self.get(name, None)
    def return_type(self):
        if self.decl:
            return self.decl.mode
        return None

#################################### Types ##################################################

class ExprType(object):
    def __init__(self, name, bin_ops=set(), un_ops=set()):
        self.name = name
        self.bin_ops = bin_ops
        self.un_ops = un_ops

IntType = ExprType("int",
    set(('PLUS', 'MINUS', 'TIMES', 'DIVIDE',
         'LE', 'LT', 'EQ', 'NE', 'GT', 'GE', 'MOD')),
    set(('PLUS', 'MINUS', 'NOT', 'IN')),
    )
BoolType = ExprType("bool",
    set(('AND', 'OR', 'EQ', 'NE')),
    set(('NOT',))
    )
CharType = ExprType("char",
    set(('PLUS', 'MINUS')),
    set(('PLUS', 'MINUS', 'IN', 'CONCAT')),
    )
StringType = ExprType("string",
    set(('PLUS',)),
    set(('CONCAT')),
    )

######################################### Scopes ###############################################

class Environment(object):
    def __init__(self):
        self.stack = []
        self.root = SymbolTable()
        self.stack.append(self.root)
        self.root.update({
            "int": IntType,
            "bool": BoolType,
            "char": CharType,
            "string": StringType
        })
    def push(self, enclosure):
        self.stack.append(SymbolTable(decl=enclosure))
    def pop(self):
        self.stack.pop()
    def peek(self):
        return self.stack[-1]
    def scope_level(self):
        return len(self.stack)
    def insert_local(self, name, value):
        self.peek().insert(name, value)
    def insert_root(self, name, value):
        self.root.insert(name, value)
    def lookup(self, name):
        for scope in reversed(self.stack):
            hit = scope.lookup(name)
            if hit is not None:
                return hit
        return None
    def find(self, name):
        if name in self.stack[-1]:
            return True
        else:
            return False

######### ERROR #######

def error(lineno, message, filename=None):
    print("{}: {}".format(lineno, message))

######################################### Visitor ###############################################


class Visitor(NodeVisitor):
    """
    Program Visitor class. This class uses the visitor pattern as
    described in lya_ast.py.   It’s define methods of the form
    visit_NodeName() for each kind of AST node that we want to process.
    Note: You will need to adjust the names of the AST nodes if you
    picked different names.
    """
    def __init__(self):
        self.environment = Environment()
        self.typemap = {
            "int": IntType,
            "char": CharType,
            "string": StringType,
            "bool": BoolType
        }
    def raw_type_unary(self, node, op, val):
        if hasattr(val, "type"):
            if op not in val.type.unary_ops:
                error(node.lineno,
                      "Unary operator {} not supported".format(op))
            return val.type
    def raw_type_binary(self, node, op, left, right):
        if hasattr(left, "type") and hasattr(right, "type"):
            if left.type != right.type:
                error(node.lineno,
                "Binary operator {} does not have matching types".format(op))
                return left.type
            errside = None
            print (op)
            if op not in left.type.bin_ops:
                errside = "LHS"
            if op not in right.type.bin_ops:
                errside = "RHS"
            if errside is not None:
                error(node.lineno,
                      "Binary operator {} not supported on {} of expression".format(op, errside))
        return left.type

    def visit_Program(self,node):
        self.environment.push(node)
        node.environment = self.environment
        node.symtab = self.environment.peek()
        # Visit all of the statements
        for stmts in node.statement_list.statements: self.visit(stmts)
    def visit_Syn(self, node):
        # Visit all of the synonyms
        for syn in node.syns:
            self.visit(syn)
    def visit_UnaryExpr(self,node):
        self.visit(node.expr)
        # Make sure that the operation is supported by the type
        raw_type = self.raw_type_unary(node, node.op, node.expr)
        # Set the result type to the same as the operand
        node.raw_type = raw_type
    def visit_Binop(self,node):
        # Make sure left and right operands have the same type
        # Make sure the operation is supported
        self.visit(node.left)
        self.visit(node.right)
        raw_type = self.raw_type_binary(node, node.op, node.left, node.right)
        # Assign the result type
        node.raw_type = raw_type


def check_errors(node):
    visitor = Visitor()
    visitor.visit(node)

check_errors(p)