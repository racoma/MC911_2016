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
        self.arraytype = 'false'
        self.readtype = 'false'
        # dictionary for counting var numbers per scope
        self.number = defaultdict(int)
        # dictionary for storing global var number
        self.varloc = defaultdict(str)
        # dictionary for storing label numbers
        self.labeldict = defaultdict(int)
        # dictionary for storing array bounds
        self.varmode = defaultdict(int)

        ''' The generated code (list of tuples)'''
        self.code = lyablock.BasicBlock()
        self.start_block = self.code

        # Dictionary to save Procs and params
        self.procs = {}

        self.bounds = {}

        self.countLabels = 0
        self.param_init = -3

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

    def insert_bounds(self, array, lower, upper):
        if array in self.bounds:
            self.bounds[array].update({"l2": lower})
            self.bounds[array].update({"u2": upper})
        else:
            self.bounds[array] = {"l1": lower}
            self.bounds[array].update({"u1": upper})
            
    #Funcao para contar variaveis por escopo
    def var_scope(self,scope, crange):
        count = self.number[scope]
        self.number[scope] = self.number[scope] + crange + 1
        return count

    # def param_number(self,varnumber):
    #     paramnumber = - varnumber
    #     count = "%d" % (paramnumber)

    #     return count

    def new_temp(self):
        #Creates new temporary variable of a given type
        name = "%d" % (self.temp_count)
        self.temp_count += 1
        return name

    def numVariables(self, node):
        count = 0
        for value, obj in node.symtab.items():
            if not isinstance(obj, ExprType):
                obj = obj["node"]

            if isinstance(obj, lyaparser.Decl) and isinstance(obj.mode.mode, ID):
                modedef = self.environment.lookup(obj.mode.mode.char)["node"]
                self.insert_bounds(modedef.idents[0].char, modedef.mode.mode.index_mode.i1.exp.exp, modedef.mode.mode.index_mode.i2.exp.exp)
                crange = modedef.mode.mode.index_mode.i2.exp.exp - modedef.mode.mode.index_mode.i1.exp.exp           
                count = count + crange   
                count += 1

            elif isinstance(obj, lyaparser.Decl) and not isinstance(obj.mode.mode, ArrayMode) and not isinstance(obj.mode.mode, Array2Mode) :
                count += 1
            elif isinstance(obj, lyaparser.Decl) and isinstance(obj.mode.mode, ArrayMode) :
                self.insert_bounds(obj.identifier_list[0].char, obj.mode.mode.index_mode.i1.exp.exp, obj.mode.mode.index_mode.i2.exp.exp)
                crange = obj.mode.mode.index_mode.i2.exp.exp - obj.mode.mode.index_mode.i1.exp.exp           
                count = count + crange
                count += 1
                # print(obj.mode.mode)
            elif isinstance(obj, lyaparser.Decl) and isinstance(obj.mode.mode, Array2Mode) :
                count2 = 1
                for i, child in enumerate(obj.mode.mode.index_mode or []):
                    self.insert_bounds(obj.identifier_list[0].char, child.i1.exp.exp, child.i2.exp.exp)
                    crange = child.i2.exp.exp - child.i1.exp.exp + 1
                    count2 *= crange

                count += count2
            # elif isinstance(obj, lyaparser.ModeDef):
            #     self.insert_bounds(obj.idents[0].char, obj.mode.mode.index_mode.i1.exp.exp, obj.mode.mode.index_mode.i2.exp.exp)
            #     crange = obj.mode.mode.index_mode.i2.exp.exp - obj.mode.mode.index_mode.i1.exp.exp           
            #     count = count + crange   
        return count

    def which_code(self, op):
        return IntType.bin_opc[op[0]]

    def visit_Program(self,node):
        # Reset the sequence of instructions and temporary count
        #self.code = lyablock.BasicBlock()
        self.program = self.code
        self.environment = node.environment
        # inst = "["
        # self.code.append(inst)
        inst = ('stp',)
        self.code.append(inst)
        # Allocate variables
        inst = ('alc', self.numVariables(node))
        self.code.append(inst)
        # Visit all statements in the program
        for stmts in node.statement_list.statements:
            self.visit(stmts)
        inst = ('dlc', self.numVariables(node))
        self.code.append(inst)
        inst2 = ('end',)
        self.code.append(inst2)
        # inst = "]"
        # self.code.append(inst)
        print("Program", node.symtab)


    def visit_Decl(self,node):
        #print(self.vardict)
        #print(self.scopedict)
        # node.scope_level = self.environment.scope_level()
        #print(node.scope_level)
        for i, child in enumerate(node.identifier_list or []):
            #count var number
            if node.mode is not None:
                 if isinstance(node.mode.mode, ID):
                    bounds = self.bounds[node.mode.mode.char]
                    crange = bounds['u1'] - bounds['l1']
                    nvar = self.var_scope(node.scope_level, crange)
                    vmode = node.mode.mode.char
                    for i, child in enumerate(node.identifier_list or []):
                        self.varmode[child.char] = vmode   
                 elif isinstance(node.mode.mode, ArrayMode):
                    bounds = self.bounds[node.identifier_list[0].char]
                    crange = bounds['u1'] - bounds['l1']
                    # print (bounds)
                    nvar = self.var_scope(node.scope_level, crange)
                 elif isinstance(node.mode.mode, Array2Mode):
                    bounds = self.bounds[node.identifier_list[0].char]
                    # print(bounds)
                    crange1 = bounds["u1"] - bounds["l1"] + 1
                    crange2 = bounds["u2"] - bounds["l2"] + 1
                    crange = crange1*crange2 - 1
                    nvar = self.var_scope(node.scope_level, crange)
                 else:
                    nvar = self.var_scope(node.scope_level, 0)
            #stores var number
            var = self.environment.lookup(child.char)
            var["number"] = nvar
            # self.scopedict[child.char] = node.scope_level
            self.visit(child)
            #inst = "('stv', {}, {})".format(node.scope_level-1, child.gen_location)
            #self.code.append(inst)
            if node.initialization:
                 if(node.initialization.exp.ttype == "ID"):
                    nvar2 = self.environment.lookup(node.initialization.exp.char)["number"]
                    scope = var["scope"]
                    if self.varloc[node.initialization.exp.char] == "loc":
                        inst = ('lrv', scope, nvar2)
                    else:
                        inst = ('ldv', scope, nvar2)
                    self.code.append(inst)
                 self.visit(node.initialization.exp)
                 inst = ('stv', node.scope_level-1, nvar)
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
            nparam = self.param_init
            self.param_init -= 1
            #stores param number
            var = self.environment.lookup(child.char)
            var["number"] = nparam
            self.visit(child)

    def visit_Range(self,node):
        pass

    def visit_Constant(self,node):
        target = self.new_temp()
        node.gen_location = target
        if node.exp == 'false':
            node.exp = 0
        elif node.exp == 'true':
            node.exp = 1
        inst = ('ldc', node.exp)
        self.code.append(inst)

    def visit_Array(self,node):
        var = self.environment.lookup(node.location.char)
        scope = var["scope"]
        num = var["number"]
        if self.arraytype == 'false':
            inst = ('ldr', scope, num)
        else:
            inst = ('ldv', scope, num)
        self.code.append(inst)       
        count = 0
        for i, child in enumerate(node.expr or []):
            self.visit(child)
            if (child.exp.ttype == 'ID'):
                var = self.environment.lookup(child.exp.char)
                scope = var["scope"]
                num = var["number"]
                inst = ('ldv', scope, num)
                self.code.append(inst)        

            if self.arraytype == 'false' and self.readtype == 'false':        
                 bounds = self.bounds[node.location.char]   
            else:        
                 tmode = self.varmode[node.location.char]
                 bounds = self.bounds[tmode]   
            inst = ('ldc', bounds['l1'])
            self.code.append(inst)      
            inst = ('sub',)
            self.code.append(inst)              

            if(len(bounds) > 2 and count == 0):
                a = int(bounds['u2'])
                inst = ('idx', a)
                self.code.append(inst)  
            else:    
                a = 1
                inst = ('idx', a)
                self.code.append(inst)            

            count += 1

    def visit_Assignment(self,node):
        if (isinstance(node.location, ProcCall) or node.location.ttype == 'array'):
            self.visit(node.location)
            self.visit(node.expr)
        else:
            self.visit(node.expr)
            self.visit(node.location)

        if(len(node.op) == 2):

            var = node.expr.exp
            if (var.ttype == 'ID'):
                if self.varloc[var.char] == "loc":
                    code = 'lrv'
                else:
                    code = 'ldv'
                nvar = self.environment.lookup(var.char)["number"]
                varscop = self.environment.lookup(var.char)["scope"]
                inst = ('{}'.format(code), varscop, nvar)
                self.code.append(inst)
            elif (var.ttype == 'Constant'):
                inst = ('ldc', var.exp)
                self.code.append(inst)

            if self.varloc[node.location.char] == "loc":
                code = 'lrv'
            else:
                code = 'ldv'
            nvar2 = self.environment.lookup(node.location.char)["number"]
            varscop = self.environment.lookup(node.location.char)["scope"]
            inst = ('{}'.format(code), varscop, nvar2)
            self.code.append(inst)

            opcode = self.which_code(node.op)
            inst = ('{}'.format(opcode),)
            self.code.append(inst)
        else:
            if isinstance(node.expr.exp, RefLoc):
                var = node.expr.exp.loc
                nvar = self.environment.lookup(var.char)["number"]
                varscop = self.environment.lookup(var.char)["scope"]
                inst = ('ldr', varscop, nvar)
                self.code.append(inst)
            elif isinstance(node.expr.exp, ID):
                nvar2 = self.environment.lookup(node.expr.exp.char)["number"]
                varscop = self.environment.lookup(node.expr.exp.char)["scope"]
                inst = ('ldv', varscop, nvar2)
                self.code.append(inst)

        ret = None
        if isinstance(node.expr.exp, ProcCall):
            params = self.procs[node.expr.exp.op.char]
            ret = params["_ret"]
            print(ret)

        if isinstance(node.location, ProcCall):
            #get function number
            nvar = self.labeldict[node.location.op.char]
            inst = ('smv', nvar)
            self.code.append(inst)

        elif (node.location.ttype != 'array'):
            if self.varloc[node.location.char] == "loc" or ret == "loc":
                code = 'srv'
            else:
                code = 'stv'
            #get var number
            nvar = self.environment.lookup(node.location.char)["number"]
            inst = ('{}'.format(code), node.scope_level-1, nvar)
            self.code.append(inst)
        
        elif (node.location.ttype == 'array'):          
            inst = ('smv', 1)
            self.code.append(inst)                

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
                nvar = self.environment.lookup(node.exp.exp.char)["number"]
                varscop = self.environment.lookup(node.exp.exp.char)["scope"]
                inst = ('ldv', varscop, nvar)
                self.code.append(inst)

    def visit_Binop(self,node):
        #print(self.vardict)
        #print(self.scopedict)
        #ver melhor a questao do escopo como target aqui, talvez salvar no dict o escopo da var?

        if (node.left.ttype == 'ID'):
            nvar = self.environment.lookup(node.left.char)["number"]
            varscop = self.environment.lookup(node.left.char)["scope"]
            inst = ('ldv', varscop, nvar)
            self.code.append(inst)
        elif (node.left.ttype == 'Constant'):
            inst = ('ldc', node.left.exp)
            self.code.append(inst)
        elif (node.left.ttype == 'array'):
            self.arraytype = 'true'
            self.visit(node.left)
            self.arraytype = 'false'
            inst = "('grc')"
            self.code.append(inst)
        if (node.right.ttype == 'ID'):
            nvar = self.environment.lookup(node.right.char)["number"]
            varscop = self.environment.lookup(node.right.char)["scope"]
            #print("Varscope", varscop, node.right.char)
            inst = ('ldv', varscop, nvar)
            self.code.append(inst)
        elif (node.right.ttype == 'Constant'):
            inst = ('ldc', node.right.exp)
            self.code.append(inst)
        elif (node.right.ttype == 'array'):
            self.arraytype = 'true'
            self.visit(node.right)
            self.arraytype = 'false'
            inst = "('grc')"
            self.code.append(inst)
        # self.visit(node.left)
        # self.visit(node.right)
        print(node.type)
        opcode = node.type.bin_opc[node.op]
        inst = ('{}'.format(opcode),)
        self.code.append(inst)

    def visit_Operand(self,node):
        self.visit(node.expr)
        target = self.new_temp()
        opcode = node.type.un_opc[node.op]
        inst = ('{}'.format(opcode),)
        #inst2 = "('ldv', {}, {})".format(target, node.ident)
        #self.code.append(inst2)
        self.code.append(inst)
        node.gen_location = target

    def visit_Func(self,node):
        if (node.op == 'read'):
            inst = ('rdv',)
            self.code.append(inst)
        if (node.op == 'print'):
            inst = ('prv',)
            self.code.append(inst)

    ##### PROC #####

    def visit_ProcStmt(self, node):
        self.visit(node.identifier)
        self.countLabels += 1
        self.labeldict[node.identifier.char] = self.countLabels


        self.param_init = -3

        self.environment.pushSymtab(node.symtab)
        # Pega qual o valor da proxima variavel para parametros

        self.visit(node.procedure)

        # Salva parametros no dictionary para checar na ProcCall
        if node.procedure.formal_parameter_list != None:
            formalparams = node.procedure.formal_parameter_list
            for i, fp in enumerate(formalparams or []):
                for i, child in enumerate(fp.id_list or []):
                    self.insert_param(node.identifier.char, fp.param_spec.attr)


        if node.procedure.result_spec is not None and node.procedure.result_spec.param is not None:
            self.insert_ret(node.identifier.char, node.procedure.result_spec.param.attr)

        #print(node.symtab)
        self.environment.pop()
        # print("ProcStmt",self.procs)
    #ideia: talvez criar outro dict para labels
    #precisa subir o enf para o procstmt e consequentemente o lbl e alc
    def visit_ProcDef(self, node):
        li = self.countLabels
        if node.result_spec is not None:
                self.countLabels += 1
                jumplb = self.countLabels
                lp = ('lbl', self.countLabels)
       
        self.countLabels += 1
        nlf = self.countLabels

        inst = ('jmp', nlf)
        self.code.append(inst)

        inst = ('lbl', li)
        self.code.append(inst)
        inst = ('enf', li)
        self.code.append(inst)
        inst = ('alc', self.numVariables(node))
        if self.numVariables(node) >0:
            self.code.append(inst)
        
        nparam = 0
        for i, child in enumerate(node.formal_parameter_list or []):
            self.visit(child)
            for i, child2 in enumerate(child.id_list or []):
                nparam += 1

        if node.result_spec is not None:
            self.visit(node.result_spec)

        for stmts in node.statement_list.statements:
            self.visit(stmts)
            if(stmts.ttype == 'action_statement') and (stmts.action.ttype == 'return'):
                inst = ('jmp', jumplb)
                self.code.append(inst)
        if node.result_spec is not None:
                self.code.append(lp)
        if(self.numVariables(node) > 0):
            dlc = ('dlc', self.numVariables(node))
            self.code.append(dlc)
        

        ret = ('ret', li, nparam)
        self.code.append(ret)
        lf = ('lbl', nlf)
        self.code.append(lf)


    def visit_ProcCall(self, node):
        self.visit(node.op)
        # self.visit(node.param)

        params = self.procs[node.op.char]
        print(params)

        if ("_ret" in params):
                inst = ('alc', 1)
                self.code.append(inst)            
        # Visita todas as Expr dos Params
        for i, expr in reversed(list(enumerate(node.param.param or []))):
            self.visit(expr.exp)
            if (isinstance(expr.exp, ID)):
                if params[i] == "loc":
                    code = 'ldr'
                else:
                    code = 'ldv'
                varscop = self.environment.lookup(expr.exp.char)["scope"]
                self.code.append( ('{}'.format(code), varscop,  self.environment.lookup(expr.exp.char)["number"]) )

            elif isinstance(expr.exp, RefLoc):
                var = expr.exp.loc
                nvar = self.environment.lookup(var.char)["number"]
                varscop = self.environment.lookup(var.char)["scope"]
                inst = ('ldr', varscop, nvar)
                self.code.append(inst)

        inst = ('cfu', self.labeldict[node.op.char])
        self.code.append(inst)

    def visit_Call(self, node):

        if(node.op.op == 'print'):
            for i, expr in enumerate(node.param.param or []):
                if (isinstance(expr.exp, ID)):
                    varscop = self.environment.lookup(expr.exp.char)["scope"]
                    self.code.append( ('ldv', varscop, self.environment.lookup(expr.exp.char)["number"]) )

                elif isinstance(expr.exp, RefLoc):
                    var = expr.exp.loc
                    nvar = self.environment.lookup(var.char)["number"]
                    varscop = self.environment.lookup(var.char)["scope"]
                    inst = ('ldr', varscop, nvar)
                    lista.append([inst, self.environment.lookup(var.char)["number"]])

                    self.visit(node.op)
                    
                elif isinstance(expr.exp, Array):
                    self.visit(expr.exp)
                    inst = ('grc',)
                    self.code.append(inst)
                                
                else:
                    self.visit(expr.exp)

                self.visit(node.op)

        elif(node.op.op != 'read'):
             self.visit(node.param)

        if(node.op.op == 'read'):            
             child = node.param.param[0]
             if (child.exp.ttype != 'array'):
                 self.visit(node.op)
                 varscop = self.environment.lookup(child.exp.char)["scope"]
                 inst = ('stv', varscop, self.environment.lookup(child.exp.char)["number"])
             else:
                 self.readtype = 'true'
                 self.visit(node.param)
                 varscop = self.environment.lookup(child.exp.location.char)["scope"]
                 inst = ('smv', 1)
                 self.visit(node.op)
             self.code.append(inst)

    def visit_Param(self, node):
        lista = []
        for i, child in enumerate(node.param or []):

            if (hasattr(child.exp, "ttype") and child.exp.ttype == 'ID'):
                 varscop = self.environment.lookup(child.exp.char)["scope"]
                 lista.append( [('ldv', varscop, self.environment.lookup(child.exp.char)["number"]), self.environment.lookup(child.exp.char)["number"] ] )

            elif isinstance(child.exp, RefLoc):
                var = child.exp.loc
                nvar = self.environment.lookup(var.char)["number"]
                varscop = self.environment.lookup(var.char)["scope"]
                inst = ('ldr', varscop, nvar)
                lista.append([inst, self.environment.lookup(var.char)["scope"]])

            self.visit(child)

        lista = sorted(lista, key=lambda ins: ins[1], reverse=True)
        for inst, i in lista:
            self.code.append(inst)

    def visit_DoAction(self, node):
        if ((node.control.whilecontrol is not None) and (node.control.forcontrol is None)):
                self.countLabels += 1
                inst = ('lbl', self.countLabels)
                self.code.append(inst)
                inst2 = ('jmp', self.countLabels)
                self.countLabels += 1
                dlabel = self.countLabels
                self.visit(node.control)
 
                for i, child in enumerate(node.action_list or []):
                    self.visit(child)
 
                self.code.append(inst2)
                inst = ('lbl', dlabel)
                self.code.append(inst)
 
        elif ((node.control.whilecontrol is None) and (node.control.forcontrol is not None)):
                inst = ('ldc', node.control.forcontrol.iteration.start.exp.exp)
                self.code.append(inst)
                inst = ('stv', 0, self.environment.lookup(node.control.forcontrol.iteration.loop.char)["number"])
                self.code.append(inst)
                self.countLabels += 1
                self.labeldict[node.control.ttype] = self.countLabels
                inst = ('lbl', self.countLabels)
                self.code.append(inst)
                instjmp = ('jmp', self.countLabels)
                self.countLabels += 1
                instlabel = ('lbl',self.countLabels)
                instjof = ('jof', self.countLabels)
 
                for i, child in enumerate(node.action_list or []):
                    self.visit(child)
 
                inst = ('ldv', 0, self.environment.lookup(node.control.forcontrol.iteration.loop.char)["number"])
                self.code.append(inst)
                inst = ('ldc', 1)
                self.code.append(inst)
                inst = ('add',)
                self.code.append(inst)
                inst = ('stv', 0, self.environment.lookup(node.control.forcontrol.iteration.loop.char)["number"])
                self.code.append(inst)
                inst = ('ldv', 0, self.environment.lookup(node.control.forcontrol.iteration.loop.char)["number"])
                self.code.append(inst)
                inst = ('ldc', node.control.forcontrol.iteration.end.exp.exp)
                self.code.append(inst)
                inst = ('leq',)
                self.code.append(inst)
 
                self.code.append(instjof)
 
                self.code.append(instjmp)
                self.code.append(instlabel)
 
        else:
                inst = ('ldc', node.control.forcontrol.iteration.start.exp.exp)
                self.code.append(inst)
                inst = ('stv', 0, self.environment.lookup(node.control.forcontrol.iteration.start.exp)["number"])
                self.code.append(inst)
                self.countLabels += 1
                self.labeldict[node.control.ttype] = self.countLabels
                inst = ('lbl', self.countLabels)
                self.code.append(inst)
                self.countLabels += 1
                self.labeldict['od'] = self.countLabels
                inst = ('ldv', 0, self.environment.lookup(node.control.whilecontrol.bool_exp.exp.exp.char)["number"])
                self.code.append(inst)
                inst = ('jof', self.labeldict['od'])
                self.code.append(inst)
 
                for i, child in enumerate(node.action_list or []):
                    self.visit(child)
 
                #update for
                inst = ('ldv', 0, self.environment.lookup(node.control.forcontrol.iteration.loop.char)["number"])
                self.code.append(inst)
                inst = ('ldc', 1)
                self.code.append(inst)
                inst = ('add',)
                self.code.append(inst)
                inst = ('stv', 0, self.environment.lookup(node.control.forcontrol.iteration.loop.char)["number"])
                self.code.append(inst)
                #test for
                inst = ('ldv', 0, self.environment.lookup(node.control.forcontrol.iteration.loop.char)["number"])
                self.code.append(inst)
                inst = ('ldc', node.control.forcontrol.iteration.end.exp.exp)
                self.code.append(inst)
                inst = ('leq',)
                self.code.append(inst)
                self.countLabels += 1
                inst = ('jof', self.labeldict['od'])
                self.code.append(inst)
                inst = ('jmp', self.labeldict[node.control.ttype])
                self.code.append(inst)
 
                inst = ('lbl', self.labeldict['od'])
                self.code.append(inst)
        # print (self.vardict)

    def visit_IfAction(self, node):
        self.visit(node.bool_exp)
        inst = ('jof', self.countLabels+1 )
        self.code.append(inst)
        if node.then_c is not None:
            self.visit(node.then_c)
            if node.else_c is not None:
                label = self.countLabels+2
                inst = ('jmp', label)
                self.code.append(inst)
        if node.else_c is not None:
            self.visit(node.else_c)
        self.countLabels += 1
        inst2 = ('lbl', self.countLabels)
        self.code.append(inst2)

    def visit_ThenClause(self, node):
        for i, child in enumerate(node.action_list or []):
            self.visit(child)

    def visit_ElseClause(self, node):
        self.countLabels += 1
        inst2 = ('lbl', self.countLabels) 
        self.code.append(inst2)
        for i, child in enumerate(node.action_list or []):
            self.visit(child)


    def visit_WhileControl(self, node):
        
        if (node.bool_exp.exp.exp.ttype == 'binop'):
                self.visit(node.bool_exp)
        elif (node.bool_exp.exp.exp.ttype == 'ID'):
                inst = ('ldv', node.scope_level-1, self.environment.lookup(node.bool_exp.exp.exp.char)["number"])
                self.code.append(inst)
        # self.visit(node.bool_exp)

        inst = ('jof', self.countLabels)
        self.code.append(inst)

    def visit_ActionStmt(self, node):

        if node.identifier is not None:
            self.countLabels += 1
            self.labeldict[node.identifier.char] = self.countLabels
            flabel = ('lbl', self.countLabels) 

        if node.action is not None:
            self.visit(node.action)

        if node.identifier is not None:
            self.code.append(flabel)

    def visit_Result(self, node):
        if isinstance(node.expr.exp, DerefRef):
            varscop = self.environment.lookup(node.expr.exp.location.char)["scope"]
            inst = ('lrv', varscop, self.environment.lookup(node.expr.exp.location.char)["number"])
        elif isinstance(node.expr.exp, ID):
            varscop = self.environment.lookup(node.expr.exp.char)["scope"]
            if self.varloc["_ret"] == "loc":
                code = 'ldr'
            else:
                code = 'ldv'
            # print(node.expr.exp.char)
            inst = ('{}'.format(code), varscop, self.environment.lookup(node.expr.exp.char)["number"])
        self.code.append(inst)

        nvar = self.environment.lookup("_ret")["number"]
        inst = ('stv', node.scope_level-1, nvar)
        self.code.append(inst)

    def visit_Returns(self, node):
        nparam = self.param_init
        self.param_init -= 1
        #stores param number
        var = self.environment.lookup("_ret")
        var["number"] = nparam

        # save return loc if location
        if node.param is not None:
            self.varloc["_ret"] = node.param.attr
        # print(self.vardict)

    def visit_Return(self, node):
        inst = ('ldv', node.scope_level-1, self.environment.lookup(node.param.exp.char)["number"])
        self.code.append(inst)
        var = self.environment.lookup("_ret")
        nparam = var['number']
        inst = ('stv', node.scope_level-1, nparam)
        self.code.append(inst)
        
    def visit_Exit(self, node):
        inst = ('jmp', self.labeldict[node.ident.char])
        self.code.append(inst)


class JumpGenerator(lyablock.BlockVisitor):
    def visit_BasicBlock(self,block):
        # print("Block:[%s]" % block)
        print("[")
        for inst in block.instructions:
            print("    %s," % (inst,))
        print("]")
        print("")



def gen_code(node):
    #adicionar teste sem erros lyasem
    gen = GenerateCode()
    gen.visit(node)
    return gen.code

# lyasem.check_errors(lyaparser.p)
code = gen_code(p)
JumpGenerator().visit(code)
