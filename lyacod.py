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
        # dictionary for storing global var number
        self.varloc = defaultdict(str)
        # dictionary for counting var numbers per scope
        self.number = defaultdict(int)
        # dictionary to store variables scope
        self.scopedict = defaultdict(int)
        # dictionary for storing funciton number
        self.func = defaultdict(int)
        # dictionary for storing label numbers
        self.labeldict = defaultdict(int)

        ''' The generated code (list of tuples)'''
        self.code = lyablock.BasicBlock()
        self.start_block = self.code

        ''' A list of external declarations (and types)'''
        self.externs = []

        # Dictionary to save Procs and params
        self.procs = {}

        self.countLabels = 0
        self.param_init = 0

    def insert_param(self, proc, loc):
        if proc in self.procs:
            count = len(self.procs[proc])
            self.procs[proc].update({count: loc})
        else:
            count = 0
            self.procs[proc] = {count: loc}

    def insert_ret(self, proc, loc):
        if proc in self.procs:
            self.procs[proc].update({"_ret": loc})
        else:
            self.procs[proc] = {"_ret": loc}
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

    def which_code(self, op):
        return IntType.bin_opc[op[0]]

    def visit_Program(self,node):
        # Reset the sequence of instructions and temporary count
        #self.code = lyablock.BasicBlock()
        self.program = self.code
        self.environment = node.environment
        inst = "["
        self.code.append(inst)
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
        inst = "]"
        self.code.append(inst)


    def visit_Decl(self,node):
        #print(self.vardict)
        #print(self.scopedict)
        # node.scope_level = self.environment.scope_level()
        #print(node.scope_level)
        for i, child in enumerate(node.identifier_list or []):
            #count var number
            nvar = self.var_scope(node.scope_level)
            #stores var number
            self.vardict[child.char] = nvar
            self.scopedict[child.char] = node.scope_level
            self.visit(child)
            #inst = "('stv', {}, {})".format(node.scope_level-1, child.gen_location)
            #self.code.append(inst)
            if node.initialization:
                 if(node.initialization.exp.ttype == "ID"):
                    nvar2 = self.vardict[node.initialization.exp.char]
                    varscop = self.scopedict[node.initialization.exp.char]
                    if self.varloc[node.initialization.exp.char] == "loc":
                        inst = "('lrv', {}, {})".format(varscop-1,nvar2)
                    else:
                        inst = "('ldv', {}, {})".format(varscop-1,nvar2)
                    self.code.append(inst)
                 self.visit(node.initialization.exp)
                 inst = "('stv', {}, {})".format(node.scope_level-1, nvar)
                 self.code.append(inst)

    def visit_FormalParam(self,node):
        # node.scope_level = node.scope_level-1
        # print("FormalParam", node.scope_level)
        # print("Param_init", self.param_init)
        for i, child in enumerate(node.id_list or []):
            self.varloc[child.char] = node.param_spec.attr
            #count var number
            # nvar = self.var_scope(node.scope_level-1, param=1)
            #count param number
            # nparam = self.param_number(self.number[node.scope_level-1]-1)
            nparam = self.param_number(self.param_init)
            self.param_init += 1
            #stores param number
            self.vardict[child.char] = nparam
            self.scopedict[child.char] = node.scope_level
            self.visit(child)


    def visit_Constant(self,node):
        target = self.new_temp()
        node.gen_location = target
        inst = "('ldc', {})".format(node.exp)
        self.code.append(inst)


    def visit_Assignment(self,node):
        self.visit(node.expr)
        self.visit(node.location)

        if(len(node.op) == 2):

            var = node.expr.exp
            if (var.ttype == 'ID'):
                if self.varloc[var.char] == "loc":
                    code = 'lrv'
                else:
                    code = 'ldv'
                nvar = self.vardict[var.char]
                varscop = self.scopedict[var.char]
                inst = "('{}', {}, {})".format(code, varscop-1,nvar)
                self.code.append(inst)
            elif (var.ttype == 'Constant'):
                inst = "('ldc', {})".format(var.exp)
                self.code.append(inst)

            if self.varloc[node.location.char] == "loc":
                code = 'lrv'
            else:
                code = 'ldv'
            nvar2 = self.vardict[node.location.char]
            varscop = self.scopedict[node.location.char]
            inst = "('{}', {}, {})".format(code, varscop-1,nvar2)
            self.code.append(inst)

            opcode = self.which_code(node.op)
            inst = "('" + opcode + "')"
            self.code.append(inst)
        else:
            if isinstance(node.expr.exp, RefLoc):
                var = node.expr.exp.loc
                nvar = self.vardict[var.char]
                varscop = self.scopedict[var.char]
                inst = "('ldr', {}, {})".format(varscop-1,nvar)
                self.code.append(inst)
            elif isinstance(node.expr.exp, ID):
                nvar2 = self.vardict[node.expr.exp.char]
                varscop = self.scopedict[node.expr.exp.char]
                inst = "('ldv', {}, {})".format(varscop-1,nvar2)
                self.code.append(inst)

        ret = None
        if isinstance(node.expr.exp, ProcCall):
            params = self.procs[node.expr.exp.op.char]
            ret = params["_ret"]
            print(ret)

        if self.varloc[node.location.char] == "loc" or ret == "loc":
            code = 'srv'
        else:
            code = 'stv'
        #get var number
        nvar = self.vardict[node.location.char]
        inst = "('{}', {}, {})".format(code, node.scope_level-1,nvar)
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
        # target = self.new_temp()
        # node.gen_location = target
        #print(self.vardict)
        #print(self.scopedict)
        if node.exp is not None:
            self.visit(node.exp)
            if(hasattr(node.exp.exp, "ttype") and node.exp.exp.ttype == "ID"):
                nvar = self.vardict[node.exp.exp.char]
                varscop = self.scopedict[node.exp.exp.char]
                inst = "('ldv', {}, {})".format(varscop-1,nvar)
                self.code.append(inst)

    def visit_Binop(self,node):
        #print(self.vardict)
        #print(self.scopedict)
        #ver melhor a questao do escopo como target aqui, talvez salvar no dict o escopo da var?

        if (node.left.ttype == 'ID'):
            nvar = self.vardict[node.left.char]
            varscop = self.scopedict[node.left.char]
            inst = "('ldv', {}, {})".format(varscop-1,nvar)
            self.code.append(inst)
        elif (node.left.ttype == 'Constant'):
            inst = "('ldc', {})".format(node.left.exp)
            self.code.append(inst)

        if (node.right.ttype == 'ID'):
            nvar = self.vardict[node.right.char]
            varscop = self.scopedict[node.right.char]
            #print("Varscope", varscop, node.right.char)
            inst = "('ldv', {}, {})".format(varscop-1,nvar)
            self.code.append(inst)
        elif (node.right.ttype == 'Constant'):
            inst = "('ldc', {})".format(node.right.exp)
            self.code.append(inst)

        # self.visit(node.left)
        # self.visit(node.right)
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

        # Pega qual o valor da proxima variavel para parametros
        self.param_init = self.number[node.scope_level]

        self.visit(node.procedure)

        # Salva parametros no dictionary para checar na ProcCall
        if node.procedure.formal_parameter_list != None:
            formalparams = node.procedure.formal_parameter_list
            for i, fp in enumerate(formalparams or []):
                for i, child in enumerate(fp.id_list or []):
                    self.insert_param(node.identifier.char, fp.param_spec.attr)


        if node.procedure.result_spec is not None and node.procedure.result_spec.param is not None:
            self.insert_ret(node.identifier.char, node.procedure.result_spec.param.attr)
        print("ProcStmt",self.procs)
    #ideia: talvez criar outro dict para labels
    #precisa subir o enf para o procstmt e consequentemente o lbl e alc
    def visit_ProcDef(self, node):
        li = self.countLabels
        if node.result_spec is not None:
                self.countLabels += 1
                lp = "('lbl', %d)" % self.countLabels

        ret = "('ret', %d, %d)" % (li,self.numVariables(node))
        self.countLabels += 1
        nlf = self.countLabels

        inst = "('jmp', %d)"% nlf
        self.code.append(inst)

        inst = "('lbl', %d)" % li
        self.code.append(inst)
        inst = "('enf', %d)" % li
        self.code.append(inst)
        inst = ('alc', self.numVariables(node))
        self.code.append(inst)

        for i, child in enumerate(node.formal_parameter_list or []):
            self.visit(child)

        if node.result_spec is not None:
            self.visit(node.result_spec)

        for stmts in node.statement_list.statements:
            self.visit(stmts)

        if node.result_spec is not None:
                self.code.append(lp)
        if(self.numVariables(node) > 0):
            dlc = "('dlc', %d)" % (self.numVariables(node))
            self.code.append(dlc)
        self.code.append(ret)
        lf = "('lbl', %d)" % nlf
        self.code.append(lf)

    def visit_ProcCall(self, node):
        self.visit(node.op)
        # self.visit(node.param)

        params = self.procs[node.op.char]
        print(params)

        # Visita todas as Expr dos Params
        for i, expr in reversed(list(enumerate(node.param.param or []))):
            self.visit(expr.exp)
            if (isinstance(expr.exp, ID)):
                if params[i] == "loc":
                    code = 'ldr'
                else:
                    code = 'ldv'
                varscop = self.scopedict[expr.exp.char]
                self.code.append("('{}', {}, {})".format(code,varscop-1,self.vardict[expr.exp.char]) )

            elif isinstance(expr.exp, RefLoc):
                var = expr.exp.loc
                nvar = self.vardict[var.char]
                varscop = self.scopedict[var.char]
                inst = "('ldr', {}, {})".format(varscop-1,nvar)
                self.code.append(inst)

        inst = "('cfu', %d)" % self.labeldict[node.op.char]
        self.code.append(inst)

    def visit_Call(self, node):

        if(node.op.op == 'print'):
            for i, expr in enumerate(node.param.param or []):
                if (isinstance(expr.exp, ID)):
                    varscop = self.scopedict[expr.exp.char]
                    self.code.append("('ldv', {}, {})".format(varscop-1,self.vardict[expr.exp.char]) )

                elif isinstance(expr.exp, RefLoc):
                    var = expr.exp.loc
                    nvar = self.vardict[var.char]
                    varscop = self.scopedict[var.char]
                    inst = "('ldr', {}, {})".format(varscop-1,nvar)
                    lista.append([inst, self.vardict[var.char]])

                    self.visit(node.op)
                else:
                    self.visit(expr.exp)

                self.visit(node.op)

        elif(node.op.op != 'read'):
             self.visit(node.param)

        if(node.op.op == 'read'):
             self.visit(node.op)
             child = node.param.param[0]
             varscop = self.scopedict[child.exp.char]
             inst = "('stv', {}, {})".format(varscop-1,self.vardict[child.exp.char])
             self.code.append(inst)

    def visit_Param(self, node):
        lista = []
        for i, child in enumerate(node.param or []):

            if (hasattr(child.exp, "ttype") and child.exp.ttype == 'ID'):
                 varscop = self.scopedict[child.exp.char]
                 lista.append( ["('ldv', {}, {})".format(varscop-1,self.vardict[child.exp.char]), self.vardict[child.exp.char] ] )

            elif isinstance(child.exp, RefLoc):
                var = child.exp.loc
                nvar = self.vardict[var.char]
                varscop = self.scopedict[var.char]
                inst = "('ldr', {}, {})".format(varscop-1,nvar)
                lista.append([inst, self.vardict[var.char]])

            self.visit(child)

        lista = sorted(lista, key=lambda ins: ins[1], reverse=True)
        for inst, i in lista:
            print(lista)
            self.code.append(inst)

    def visit_DoAction(self, node):

        if ((node.control.whilecontrol is not None) and (node.control.forcontrol is None)):
                print ("while")
                self.countLabels += 1
                inst = "('lbl', %d)" % self.countLabels
                self.code.append(inst)
                inst2 = "('jmp', %d)" % self.countLabels
                self.countLabels += 1
                dlabel = self.countLabels
                self.visit(node.control)

                for i, child in enumerate(node.action_list or []):
                    self.visit(child)

                self.code.append(inst2)
                inst = "('lbl', %d)" % dlabel
                self.code.append(inst)

        elif ((node.control.whilecontrol is None) and (node.control.forcontrol is not None)):
                inst = "('ldc', {})".format(node.control.forcontrol.iteration.start.exp.exp)
                self.code.append(inst)
                inst = "('stv', 0, {})".format(self.vardict[node.control.forcontrol.iteration.start.exp])
                self.code.append(inst)
                self.countLabels += 1
                self.labeldict[node.control.ttype] = self.countLabels
                inst = "('lbl', %d)" % self.countLabels
                self.code.append(inst)

                for i, child in enumerate(node.action_list or []):
                    self.visit(child)

                inst = "('ldv', 0, {})".format(self.vardict[node.control.forcontrol.iteration.start.exp])
                self.code.append(inst)
                inst = "('ldc', 1)"
                self.code.append(inst)
                inst = "('add')"
                self.code.append(inst)
                inst = "('stv', 0, {})".format(self.vardict[node.control.forcontrol.iteration.start.exp])
                self.code.append(inst)
                inst = "('ldv', 0, {})".format(self.vardict[node.control.forcontrol.iteration.start.exp])
                self.code.append(inst)
                inst = "('ldc', {})".format (node.control.forcontrol.iteration.end.exp.exp)
                self.code.append(inst)
                inst = "('leq')"
                self.code.append(inst)
                self.countLabels += 1
                inst = "('jof', %d)"% self.countLabels
                self.code.append(inst)
                inst = "('jmp', {})".format(self.labeldict[node.control.ttype])
                self.code.append(inst)
                inst = "('lbl', %d)" % self.countLabels
                self.code.append(inst)

        else:
                inst = "('ldc', {})".format(node.control.forcontrol.iteration.start.exp.exp)
                self.code.append(inst)
                inst = "('stv', 0, {})".format(self.vardict[node.control.forcontrol.iteration.start.exp])
                self.code.append(inst)
                self.countLabels += 1
                self.labeldict[node.control.ttype] = self.countLabels
                inst = "('lbl', %d)" % self.countLabels
                self.code.append(inst)
                self.countLabels += 1
                self.labeldict['od'] = self.countLabels
                inst = "('ldv', 0, {})".format(self.vardict[node.control.whilecontrol.bool_exp.exp.exp.char])
                self.code.append(inst)
                inst = "('jof', %d)"% self.labeldict['od']
                self.code.append(inst)

                for i, child in enumerate(node.action_list or []):
                    self.visit(child)

                #update for
                inst = "('ldv', 0, {})".format(self.vardict[node.control.forcontrol.iteration.start.exp])
                self.code.append(inst)
                inst = "('ldc', 1)"
                self.code.append(inst)
                inst = "('add')"
                self.code.append(inst)
                inst = "('stv', 0, {})".format(self.vardict[node.control.forcontrol.iteration.start.exp])
                self.code.append(inst)
                #test for
                inst = "('ldv', 0, {})".format(self.vardict[node.control.forcontrol.iteration.start.exp])
                self.code.append(inst)
                inst = "('ldc', {})".format (node.control.forcontrol.iteration.end.exp.exp)
                self.code.append(inst)
                inst = "('leq')"
                self.code.append(inst)
                self.countLabels += 1
                inst = "('jof', %d)"% self.labeldict['od']
                self.code.append(inst)
                inst = "('jmp', {})".format(self.labeldict[node.control.ttype])
                self.code.append(inst)

                inst = "('lbl', %d)" % self.labeldict['od']
                self.code.append(inst)


    def visit_IfAction(self, node):
        self.visit(node.bool_exp)
        inst = "('jof', %d)" % (self.countLabels+1)
        self.code.append(inst)
        if node.then_c is not None:
            self.visit(node.then_c)
            if node.else_c is not None:
                label = self.countLabels+2
                inst = "('jmp', {})".format(label)
                self.code.append(inst)
        if node.else_c is not None:
            self.visit(node.else_c)
        self.countLabels += 1
        inst2 = "('lbl', %d)" % self.countLabels
        self.code.append(inst2)

    def visit_ThenClause(self, node):
        for i, child in enumerate(node.action_list or []):
            self.visit(child)

    def visit_ElseClause(self, node):
        self.countLabels += 1
        inst2 = "('lbl', %d)" % self.countLabels
        self.code.append(inst2)
        for i, child in enumerate(node.action_list or []):
            self.visit(child)


    def visit_WhileControl(self, node):

        # inst = "('ldv', {}, {})".format(node.scope_level-1,self.vardict[node.bool_exp.exp.exp.char])
        # self.code.append(inst)
        self.visit(node.bool_exp)

        inst = "('jof', %d)" % self.countLabels
        self.code.append(inst)

    def visit_ActionStmt(self, node):

        if node.identifier is not None:
            self.countLabels += 1
            self.labeldict[node.identifier.char] = self.countLabels
            flabel = "('lbl', %d)" % self.countLabels

        if node.action is not None:
            self.visit(node.action)

        if node.identifier is not None:
            self.code.append(flabel)

    def visit_Result(self, node):
        if isinstance(node.expr.exp, DerefRef):
            varscop = self.scopedict[node.expr.exp.location.char]
            inst = "('lrv', {}, {})".format(varscop-1,self.vardict[node.expr.exp.location.char])
        elif isinstance(node.expr.exp, ID):
            varscop = self.scopedict[node.expr.exp.char]
            if self.varloc["_ret"] == "loc":
                code = 'ldr'
            else:
                code = 'ldv'
            # print(node.expr.exp.char)
            inst = "('{}', {}, {})".format(code, varscop-1,self.vardict[node.expr.exp.char])
        self.code.append(inst)

        inst = "('stv', {}, {})".format(node.scope_level-1,self.vardict["_ret"])
        self.code.append(inst)

    def visit_Returns(self, node):
        nparam = self.param_number(self.param_init)
        self.param_init += 1
        #stores param number
        self.vardict["_ret"] = nparam
        self.scopedict["_ret"] = node.scope_level

        # save return loc if location
        if node.param is not None:
            self.varloc["_ret"] = node.param.attr
        print(self.vardict)

    def visit_Exit(self, node):
        inst = "('jmp', {})".format(self.labeldict[node.ident.char])
        self.code.append(inst)




class JumpGenerator(lyablock.BlockVisitor):
    def visit_BasicBlock(self,block):
        # print("Block:[%s]" % block)
        for inst in block.instructions:
            print("    %s," % (inst,))
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
