import sys
from lyaparser import *
from lyalex import tokens

#################################### Symbol Table ########################################      

class SymbolTable(dict):
    def __init__(self, decl=None):
        super().__init__()
        self.decl = decl
    def insert(self, name, value, scope, number=None):
        self[name] = {"node": value, "scope": scope, "number": number}

    def lookup(self, name):
        return self.get(name, None)
    def return_type(self):
        if self.decl:
            return self.decl.mode
        return None

#################################### Types ##################################################

class ExprType(object):
    def __init__(self, name, bin_ops=set(), un_ops=set(), bin_opc=set(), un_opc=set()):
        self.name = name
        self.bin_ops = bin_ops
        self.un_ops = un_ops or set()
        self.bin_opc = bin_opc or set()
        self.un_opc = un_opc or set()

IntType = ExprType("int",
    set(('+', '-', '*', '/', '<=', '<', '==', '!=', '>', '>=', '%')),
    set(('+', '-')),	
    bin_opc={'+' : 'add', '-' : 'sub', '*' : 'mul', '/' : 'div', '<=' : 'leq', '<' : 'les', '==' : 'eq', '!=' : 'neq', '>' : 'grt', '>=': 'geq', '%' : 'mod'},
    un_opc={'+' : 'add', '-' : 'sub'},
    )

CharType = ExprType("char",
    set(('==', '!=', '>', '>=', '<', '<=')),
    bin_opc={'==' : 'eq', '!=' : 'neq', '>' : 'grt', '>=' : 'geq', '<' : 'les', '<=' : 'leq'},
    )

StringType = ExprType("string",
    set(('&', '==', '!=', '+')),
    bin_opc={'&' : 'cct', '==' : 'eq', '!=' : 'neq', '+' : 'add'},
    )

BoolType = ExprType("bool",
    set(('&&', '||', '==', '!=')),
    set(('!')),
    bin_opc={'&&' : 'and', '||' : 'or', '==' : 'eq', '!=' : 'neq'},
    un_opc={'!' : 'dif'}
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
    def pushSymtab(self, symtab):
        self.stack.append(symtab)
    def pop(self):
        self.stack.pop()
    def peek(self):
        return self.stack[-1]
    def scope_level(self):
        return len(self.stack)
    def insert_local(self, name, value):
        self.peek().insert(name, value, self.scope_level()-1)
    def insert_root(self, name, value):
        self.root.insert(name, value, self.scope_level()-1)
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
    def countVars(self):
        count = 0
        for value, obj in self.peek().items():
            if not isinstance(obj, ExprType):
                if isinstance(obj["node"], Decl):
                    count += 1
        return count

########################################### ERROR ###############################################

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
            if op not in val.type.un_ops:
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
            if op not in left.type.bin_ops:
                errside = "LHS"
            if op not in right.type.bin_ops:
                errside = "RHS"
            if errside is not None:
                error(node.lineno,
                      "Binary operator {} not supported on {} of expression".format(op, errside))
        return left.type

    def visit_Program(self,node):
        # self.environment.push(node)
        node.environment = self.environment
        node.symtab = self.environment.peek()
        # Visit all of the statements
        for stmts in node.statement_list.statements:
            self.visit(stmts)
            if isinstance(stmts, Assignment):
                self.environment.insert_local(stmts.location.char, stmts.expr)
            # print(node.symtab)
        # print(len(self.environment.peek())

    def visit_Syn(self, node):
        node.scope_level = self.environment.scope_level()
        # 2. Add an entry to the symbol table
        for i, child in enumerate(node.ident or []):
            self.environment.insert_local(child.char, node)
        self.visit(node.expr)
        node.type = node.expr.exp.type
        # for i, child in enumerate(node.ident or []):
        #     e = self.environment.lookup(child.char)
        #     print(e.type.name)

    def visit_Operand(self,node):
        self.visit(node.expr)
        # Make sure that the operation is supported by the type
        raw_type = self.raw_type_unary(node, node.op, node.expr)
        # Set the result type to the same as the operand
        node.type = raw_type

    def visit_Binop(self,node):
        # Make sure left and right operands have the same type
        # Make sure the operation is supported
        self.visit(node.left)
        self.visit(node.right)
        raw_type = self.raw_type_binary(node, node.op, node.left, node.right)
        # Assign the result type
        node.type = raw_type
        print(node.type)
        node.scope_level = self.environment.scope_level()

    def visit_Constant(self,node):
        nodetype = self.typemap.get(node.type, None)
        if nodetype is None:
            error(node.lineno, "Type {} is not supported".format(node.type))
        node.type = nodetype

    def visit_ID(self, node):
        e = self.environment.lookup(node.char)

        if e is not None and e["node"] is not None:
            node.type = e["node"].type
        else:
            node.type = None

    def visit_Array(self, node):
        self.visit(node.location)
        e = self.environment.lookup(node.location.char)["node"]

        if e is not None:
            node.type = e.type
        else:
            node.type = None

    def visit_ArrayMode(self, node):
        nodetype = self.typemap.get(node.element_mode.mode.type, None)
        node.type = nodetype

    def visit_Array2Mode(self, node):
        nodetype = self.typemap.get(node.element_mode.mode.type, None)
        node.type = nodetype

    def visit_Mode(self, node):
        self.visit(node.mode)
        node.type = node.mode.type

    def visit_Ref(self, node):
        self.visit(node.mode)
        node.type = node.mode.type

    def visit_Result(self, node):
        self.visit(node.expr)
        node.scope_level = self.environment.scope_level()

    def visit_RefLoc(self, node):
        self.visit(node.loc)
        node.type = node.loc.type

    def visit_Expr(self, node):
        self.visit(node.exp)
        node.type = node.exp.type

    def visit_ProcStmt(self, node):
        node.scope_level = self.environment.scope_level()
        self.environment.push(node)
        self.environment.insert_local(node.identifier.char, node)
        self.visit(node.procedure)

        if(node.procedure.type != None):
            node.type = node.procedure.type
        else:
            node.type = None

        node.symtab = self.environment.peek()
        # print(node.symtab)
        self.environment.insert_root(node.identifier.char, node)
        self.environment.pop()


    def visit_ProcCall(self, node):
        self.visit(node.op)
        node.type = node.op.type

    def visit_CondExpr(self, node):
        self.visit(node.thenex.exp)
        node.type = node.thenex.exp.type

    def visit_ProcDef(self, node):
        node.symtab = self.environment.peek()
        if node.result_spec != None:
            self.visit(node.result_spec.param.mode)
            node.type = node.result_spec.param.mode.type
        else:
            node.type = None

        if node.formal_parameter_list != None:
            for i, child in enumerate(node.formal_parameter_list or []):
                self.visit(child)

        if node.result_spec != None:
            self.visit(node.result_spec)

        for stmts in node.statement_list.statements:
            self.visit(stmts)

    def visit_Returns(self, node):
        node.scope_level = self.environment.scope_level()
        self.environment.insert_local("_ret", node)
        
    def visit_Return(self, node):
        node.scope_level = self.environment.scope_level()
        self.environment.insert_local("_ret", node)
        
    def visit_FormalParam(self, node):
        for i, child in enumerate(node.id_list or []):
            self.environment.insert_local(child.char, node)
        self.visit(node.param_spec)
        node.type = node.param_spec.type
        node.scope_level = self.environment.scope_level()

    def visit_ParamSpec(self, node):
        self.visit(node.mode)
        node.type = node.mode.type

    def visit_Assignment(self,node):
        if isinstance(node.location, DerefRef) or isinstance(node.location, ArraySlice) or isinstance(node.location, Array):
            var = self.environment.lookup(node.location.location.char)
        elif isinstance(node.location, ProcCall):
            var = self.environment.lookup(node.location.op.char)
        else:
            var = self.environment.lookup(node.location.char)
        # # else:
        #     return
        if var is None:
            error(node.location.lineno, "Error. '{}' is not defined".format(node.location.char))
        self.visit(node.expr)

        self.visit(node.location)
        if hasattr(node.location, "type") and node.location.type != None and hasattr(node.expr, "type") and node.expr.type != None:
            print(node.location)
            declared_type = node.location.type.name
            value_type = node.expr.type.name
            if declared_type != value_type:
                error(node.location.lineno, "Cannot assign {} to {}".format(value_type, declared_type))
        node.scope_level = self.environment.scope_level()

    def visit_Decl(self,node):
        for i, child in enumerate(node.identifier_list or []):
            # Adiciona a tabela
            self.environment.insert_local(child.char, node)
        # Verifica se tem o mesmo tipo
        self.visit(node.mode)
        if hasattr(node.mode, "type"):
            node.type = node.mode.type

        # Testa se a declaration tem um assignment
        if node.initialization:
            self.visit(node.initialization.exp)
            if hasattr(node.initialization.exp, "type"):
                declared_type = node.type.name
                value_type = node.initialization.exp.type.name
                if declared_type != value_type:
                    error(node.identifier_list[0].lineno, "Cannot assign {} to {}".format(value_type, declared_type))
        node.scope_level = self.environment.scope_level()

    def visit_DiscreteMode(self,node):
        nodetype = self.environment.lookup(node.type)
        # if not isinstance(nodetype, ExprType):
        #     error(node.lineno, "{} is not a valid type".format(node.type))
        #     return
        node.type = nodetype

    def visit_StrLen(self,node):
        nodetype = self.environment.lookup(node.type)
        if not isinstance(nodetype, ExprType):
            error(node.lineno, "{} is not a valid type".format(node.type))
            return
        node.type = nodetype

    def visit_Type(self, node):
        for i, child in enumerate(node.newmode or []):
            self.visit(child)

    def visit_ModeDef(self, node):
        for i, child in enumerate(node.idents or []):
            # Adiciona a tabela
            self.environment.insert_local(child.char, node)
        self.visit(node.mode)
        node.type = node.mode.type
        
    def visit_WhileControl(self,node):
        node.scope_level = self.environment.scope_level()
        self.visit(node.bool_exp)
    
    '''    
    def visit_FormalParam(self,node):
        node.scope_level = self.environment.scope_level()
    '''            

def check_errors(node):
    visitor = Visitor()
    visitor.visit(node)

check_errors(p)
