'''
LVM code is only allowed to use the following operators:
    (’ldc’, k)     # Load constant sp=sp+1;  M[sp]=k
    (’ldv’, i, j)  # Load value sp=sp+1;  M[sp]=M[D[i]+j]
    (’ldr’, i, j)  # Load reference sp=sp+1;  M[sp]=D[i]+j
    (’stv’, i, j)  # Store value M[D[i]+j]=M[sp];  sp=sp-1
    (’lrv’, i, j)  # Load reference value sp=sp+1;  M[sp]=M[M[D[i]+j]]
    (’srv’, i, j)  # Store reference value M[M[D[i]+j]]=M[sp];  sp=sp-1
    (’add’)        # Add M[sp-1]=M[sp-1] + M[sp];  sp=sp-1
    (’sub’)        # Subtract M[sp-1]=M[sp-1] - M[sp];  sp=sp-1
    (’mul’)        # Multiply M[sp-1]=M[sp-1] * M[sp];  sp=sp-1
    (’div’)        # Division M[sp-1]=M[sp-1] / M[sp];  sp=sp-1
    (’mod’)        # Modulus M[sp-1]=M[sp-1] % M[sp];  sp=sp-1
    (’neg’)        # Negate M[sp]= -M[sp]
    (’and’)        # Logical And M[sp-1]=M[sp-1] and M[sp];  sp=sp-1
    (’lor’)        # Logical Or M[sp-1]=M[sp-1] or M[sp];  sp=sp-1
    (’not’)        # Logical Not M[sp]= not M[sp]
    (’les’)        # Less M[sp-1]=M[sp-1] < M[sp];  sp=sp-1
    (’leq’)        # Less or Equal M[sp-1]=M[sp-1] <= M[sp];  sp=sp-1
    (’grt’)        # Greater M[sp-1]=M[sp-1] > M[sp];  sp=sp-1
    (’gre’)        # Greater or Equal M[sp-1]=M[sp-1] >= M[sp];  sp=sp-1
    (’equ’)        # Equal M[sp-1]=M[sp-1] == M[sp];  sp=sp-1
    (’neq’)        # Not Equal M[sp-1]=M[sp-1] != M[sp];  sp=sp-1
    (’jmp’, p)     # Jump pc=p
    (’jof’, p)     # Jump on False (if not M[sp]: pc=p) (else: pc=pc+1) (sp=sp-1)
    (’alc’, n)     # Allocate memory sp=sp+n
    (’dlc’, n)     # Deallocate memory sp=sp-n
    (’cfu’, p)     # Call Function sp=sp+1; M[sp]=pc+1; pc=p
    (’enf’, k)     # Enter Function sp=sp+1; M[sp]=D[k]; D[k]=sp+1
    (’ret’, k, n)  # Return from Function D[k]=M[sp]; pc=M[sp-1]; sp=sp-(n+2)
    (’idx’, k)     # Index M[sp-1]=M[sp-1] + M[sp] * k sp=sp-1
    (’grc’)        # Get(Load) Reference Contents M[sp]=M[M[sp]]
    (’lmv’, k)     # Load multiple values 
    (’smv’, k)     # Store multiple Values 
    (’sts’, k)     # Store string constant on reference 
    (’rdv’)        # Read single Value sp=sp+1;  M[sp]=input()
    (’rds’)        # Read String and store it on stack reference 
    (’prv’)        # Print Value print(M[sp]); sp=sp-1
    ('prt', k)     # Print Multiple Values print(M[sp-k+1:sp+1]); sp-=(k-1)
    (’prc’, i)     # Print String constant print(H(i),end="")
    (’prs’)        # Print contents of a string location 
    (’stp’)        # Start Program sp=-1; D[0]=0
    (’lbl’, i)     # No operation (define the label index i)
    (’end’)        # Stop execution

'''

import sys
from lyasem import *
import lyasem
import lyablock
import lyaparser
from collections import defaultdict


class GenerateCode(lyaparser.NodeVisitor):

    def __init__(self):
        super(GenerateCode, self).__init__()

        self.temp_count = 0
        
        # dictionary for counting var numbers per scope
        self.number = defaultdict(int)
        # dictionary for storing global var number 
        self.vardict = defaultdict(int)
        # dictionary for storing funciton number
        self.func = defaultdict(int)
        # dictionary for storing label numbers
        self.labeldict = defaultdict(int)
        
        ''' The generated code (list of tuples)'''
        self.code = lyablock.BasicBlock()
        self.start_block = self.code

        ''' A list of external declarations (and types)'''
        self.externs = []

        self.countLabels = 0
        
    #Funcao para contar variaveis por escopo
    def var_scope(self,scope):
        '''
        Increments count per scope
        '''
        count = "%d" % (self.number[scope])
        self.number[scope] += 1
        return count
        
    def param_number(self,varnumber):
        paramnumber = - varnumber
        count = "%d" % (paramnumber)
        return count                 
        
    def new_temp(self):
        #Creates new temporary variable of a given type
        name = "%d" % (self.temp_count)
        self.temp_count += 1
        return name

    def numVariables(self, node):
        count = 0
        for value, obj in node.symtab.items():
            if isinstance(obj, lyaparser.Decl):
                count += 1
        return count

    def visit_Program(self,node):
        # Reset the sequence of instructions and temporary count
        #self.code = lyablock.BasicBlock()
        self.program = self.code
        self.environment = node.environment
        inst = "('stp')"
        self.code.append(inst)
        # Allocate variables
        inst = ('alc', self.numVariables(node))
        self.code.append(inst)
        # Visit all statements in the program
        for stmts in node.statement_list.statements:
            self.visit(stmts)
        inst = ('dlc', self.numVariables(node))
        self.code.append(inst)
        inst2 = "('end')"
        self.code.append(inst2)


    def visit_Decl(self,node):
        node.scope_level = self.environment.scope_level()
        for i, child in enumerate(node.identifier_list or []):
            #count var number
            nvar = self.var_scope(node.scope_level)
            #stores var number
            self.vardict[child.char] = nvar
            self.visit(child)
            #inst = "('stv', {}, {})".format(node.scope_level-1, child.gen_location)
            #self.code.append(inst)
            if node.initialization:
                 self.visit(node.initialization.exp)
                 inst = "('stv', {}, {})".format(node.scope_level-1, nvar)
                 self.code.append(inst)            
    
    def visit_FormalParam(self,node):
        node.scope_level = self.environment.scope_level()
        for i, child in enumerate(node.id_list or []):
            #count param number
            nparam = self.param_number(self.number[node.scope_level])
            #stores param number
            self.vardict[child.char] = nparam
            self.visit(child)
                   

    def visit_Constant(self,node):
        target = self.new_temp()        
        node.gen_location = target
        inst = "('ldc', {})".format(node.exp)
        self.code.append(inst)
   
    
    def visit_Assignment(self,node):
        self.visit(node.expr)
        self.visit(node.location)
        #get var number
        nvar = self.vardict[node.location.char]
        inst = "('stv', {}, {})".format(node.scope_level-1,nvar)
        self.code.append(inst)
     
    '''             
    def visit_Expr(self, node):
        target = self.new_temp()   
        self.visit(node.exp)
        inst2 = "('ldv', {}, {})".format(target, node.exp)
        self.code.append(inst2)
        node.gen_location = target
    '''        
        
    def visit_ID(self, node):
        target = self.new_temp()
        #inst = "('ldv', {}, {})".format(target,node.char)
        #self.code.append(inst)
        node.gen_location = target

    def visit_Syn(self, node):   
        self.visit(node.expr)
        
    def visit_BoolExpr(self, node):
        target = self.new_temp()
        node.gen_location = target
    

    def visit_Binop(self,node):
        print(self.vardict)
        
        nvar = self.vardict[node.left.char]
        inst = "('ldv', {}, {})".format(node.scope_level-2,nvar)
        self.code.append(inst)
        nvar = self.vardict[node.right.char]
        inst = "('ldv', {}, {})".format(node.scope_level-1,nvar)
        self.code.append(inst)
        self.visit(node.left)
        self.visit(node.right)
        # print(node.type)
        opcode = node.type.bin_opc[node.op]
        inst = "('" + opcode + "')"
        self.code.append(inst)

    def visit_Operand(self,node):
        self.visit(node.expr)
        target = self.new_temp()
        opcode = node.type.un_opc[node.op]
        inst = "('" + opcode + "')"
        #inst2 = "('ldv', {}, {})".format(target, node.ident)
        #self.code.append(inst2)
        self.code.append(inst)
        node.gen_location = target
        
    def visit_Func(self,node):
        if (node.op == 'read'):
            inst = "('rdv')"
            self.code.append(inst)
        if (node.op == 'print'):
            inst = "('prv')"
            self.code.append(inst)        

    ##### PROC #####

    def visit_ProcStmt(self, node):
        self.visit(node.identifier)   
        self.countLabels += 1
        self.labeldict[node.identifier.char] = self.countLabels
        aux = self.countLabels + 1    
        inst = "('jmp', {})".format(aux)
        self.code.append(inst)

        self.visit(node.procedure)

    #ideia: talvez criar outro dict para labels
    #precisa subir o enf para o procstmt e consequentemente o lbl e alc 
    def visit_ProcDef(self, node):
        print (self.labeldict)
        inst = "('lbl', %d)" % self.countLabels
        self.code.append(inst) 
        inst = "('enf', %d)" % self.countLabels
        self.code.append(inst)
        inst = ('alc', self.numVariables(node))
        self.code.append(inst)
        
        for i, child in enumerate(node.formal_parameter_list or []):
            self.visit(child)
            
        for stmts in node.statement_list.statements:
            self.visit(stmts)
            
        inst = "('ret', %d, %d)" % (self.countLabels,self.numVariables(node))
        self.code.append(inst)
        self.countLabels += 1
        inst = "('lbl', %d)" % self.countLabels
        self.code.append(inst)
        
    def visit_ProcCall(self, node):
        self.visit(node.op)
        self.visit(node.param)
        inst = "('cfu', %d)" % self.labeldict[node.op.char]
        self.code.append(inst)
        
    def visit_Param(self, node):
        for i, child in enumerate(node.param or []):
             inst = "('ldv', 0, {})".format(self.vardict[child.exp.char])
             self.code.append(inst)                

    def visit_DoAction(self, node):
        self.countLabels += 1
        inst = "('lbl', %d)" % self.countLabels
        self.code.append(inst)
        inst2 = "('jmp', %d)" % self.countLabels
        self.countLabels += 1

        self.visit(node.control)
	#inserir visita ao action_list

        inst = "('jof', %d)" % self.countLabels
        self.code.append(inst)
        self.code.append(inst2)
        instf = "('lbl', %d)" % self.countLabels
        self.code.append(inst)	

    def visit_IfAction(self, node):
     
        self.countLabels += 1
        inst2 = "('lbl', %d)" % self.countLabels

        self.visit(node.then_c)
        self.visit(node.else_c)

        inst = "('jof', %d)" % self.countLabels
        self.code.append(inst)
        self.code.append(inst2)
    
    
    def visit_WhileControl(self, node):
        var = self.var_scope(node.scope_level)
        inst = "('ldv', {}, {})".format(var,var)
        self.code.append(inst)
        #self.visit(node.BoolExpr)

        inst = "('jof', %d)" % self.countLabels
        self.code.append(inst)
    

class JumpGenerator(lyablock.BlockVisitor):
    def visit_BasicBlock(self,block):
        # print("Block:[%s]" % block)
        for inst in block.instructions:
            print("    %s" % (inst,))
        print("")

    def visit_IfBlock(self,block):
        # Emit a conditional jump around the if-branch
        #inst = ('if', block.condvar)
        #block.append(inst)
        self.visit_BasicBlock(block)
        if block.falsebranch:
            pass
            # Emit a jump around the else-branch (if there is one)
            #inst = ('else',)
            #block.truebranch.append(inst)
        self.visit(block.truebranch)
        if block.falsebranch:
            self.visit(block.falsebranch)

    def visit_WhileBlock(self,block):
        # Emit a conditional jump around the if-branch
        #inst = ('while', block.condvar)
        #block.append(inst)
        self.visit_BasicBlock(block)
        self.visit(block.truebranch)

def gen_code(node):
    #adicionar teste sem erros lyasem
    gen = GenerateCode()
    gen.visit(node)
    return gen.code

# lyasem.check_errors(lyaparser.p)
code = gen_code(p)
JumpGenerator().visit(code)
