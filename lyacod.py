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
        
        ''' The generated code (list of tuples)'''
        self.code = lyablock.BasicBlock()
        self.start_block = self.code

        ''' A list of external declarations (and types)'''
        self.externs = []
        
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
        # self.code = lyablock.BasicBlock()
        self.program = self.code
        self.environment = node.environment
        self.temp_count = 0
        inst = "('stp')"
        self.code.append(inst)
        # Allocate variables
        inst = ('alc', self.numVariables(node))
        self.code.append(inst)
        # Visit all of the statements in the program
        for stmts in node.statement_list.statements:
            self.visit(stmts)
        inst2 = "('end')"
        self.code.append(inst2)


    def visit_Decl(self,node):
        node.scope_level = self.environment.scope_level()
        if node.initialization:
            self.visit(node.initialization.exp)
        for i, child in enumerate(node.identifier_list or []):
            self.visit(child)
            inst = "('stv', {}, {})".format(node.scope_level-1, child.gen_location)
            self.code.append(inst)


    def visit_Constant(self,node):
        inst = "('ldc', {})".format(node.exp)
        self.code.append(inst)

    def visit_ID(self, node):
        target = self.new_temp()
        # inst = "('ldv', {}, {})".format(node.char, target)
        # self.code.append(inst)
        node.gen_location = target

    def visit_Syn(self, node):   
        self.visit(node.expr)

    def visit_Binop(self,node):
        self.visit(node.left)
        self.visit(node.right)

        # Create the opcode and append to list
        # print(node.type)
        opcode = node.type.bin_opc[node.op]
        inst = "('" + opcode + "')"
        self.code.append(inst)

    def visit_Operand(self,node):
        self.visit(node.expr)
        target = self.new_temp()
        opcode = node.type.un_opc[node.op]
        inst = "('" + opcode + "')"
        inst2 = "('ldv', {}, {})".format(node.ident, target)
        self.code.append(inst2)
        self.code.append(inst)
        node.gen_location = target


    ##### PROC #####

    def visit_ProcStmt(self, node):
        self.visit(node.identifier)
        inst = "('jmp', {})".format(node.identifier.gen_location)
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

