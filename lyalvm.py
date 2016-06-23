import sys
from lyacod import *

class LVM:
    def __init__(self):
        global pc, sp, D, M, H

        H = [] # Colocar constantes (string)
        self.code = []
        self.labels = {} # guarda o ID do label e o PC correspondente
        self.changepc = ['jmp', 'jof', 'cfu', 'ret']
        D = 10 * [None]
        M = 10000 * [None]
        pc = 0
        sp = 0

    def run(self):
        global pc, sp, D, M
        self.code = code.instructions
        funcs = Funcs()

        print("RUNNING CODE...")

        # Primeiro percorremos as instrucoes e mudamos o valor do label para o PC correspondente
        for i, v in enumerate(self.code):
            if v[0] == 'lbl':
                self.labels[v[1]] = i
                self.code[i] = (v[0], i)


        # Percorremos de novo, mas agora mudamos o valor dos JUMPS e CFUs
        for i, v in enumerate(self.code):
            if v[0] == 'jmp' or v[0] == 'jof' or v[0] == 'cfu':
                # print(self.labels)
                new = self.labels[v[1]]
                self.code[i] = (v[0], new)

        # Percorrer cada instrução, executando cada uma e alterando os dados correspondentes
        while True:
            instruction = self.code[pc][0]
            # print(instruction)
            if instruction == 'end':
                print("CODE EXECUTED.")
                return
            else:
                getattr(funcs, "eval_"+instruction)(self.code[pc]) # Chama funcao correspondente

                if instruction not in self.changepc:
                    pc += 1



class Funcs:
    def eval_ldc(self, code):
        global sp, M
        k = code[1]
        sp += 1
        M[sp] = k

    def eval_ldv(self, code):
        global sp, D, M
        i = code[1]
        j = code[2]

        sp += 1
        M[sp]=M[D[i]+j]

    def eval_ldr(self, code):
        global sp, D, M
        i = code[1]
        j = code[2]
        sp=sp+1
        M[sp]=D[i]+j

    def eval_stv(self, code):
        global sp, D, M
        i = code[1]
        j = code[2]

        M[D[i]+j]=M[sp]
        sp=sp-1

    def eval_lrv(self, code):
        global sp, D, M
        i = code[1]
        j = code[2]

        sp=sp+1
        M[sp]=M[M[D[i]+j]]

    def eval_srv(self, code):
        global sp, D, M
        i = code[1]
        j = code[2]

        M[M[D[i]+j]]=M[sp]
        sp=sp-1

    def eval_add(self, code):
        global sp, M

        M[sp-1]=M[sp-1] + M[sp]
        sp=sp-1

    def eval_sub(self, code):
        global sp, M
        M[sp-1]=M[sp-1] - M[sp]
        sp=sp-1

    def eval_mul(self, code):
        global sp, M
        M[sp-1]=M[sp-1] * M[sp]
        sp=sp-1

    def eval_div(self, code):
        global sp, M
        M[sp-1]=M[sp-1] / M[sp]
        sp=sp-1

    def eval_mod(self, code):
        global sp, M
        M[sp-1]=M[sp-1] % M[sp]
        sp=sp-1

    def eval_neg(self, code):
        global sp, M
        M[sp]= -M[sp]

    def eval_and(self, code):
        global sp, M
        M[sp-1]=M[sp-1] and M[sp]
        sp=sp-1

    def eval_lor(self, code):
        global sp, M
        M[sp-1]=M[sp-1] or M[sp]
        sp=sp-1

    def eval_not(self, code):
        global sp, M
        M[sp]= not M[sp]

    def eval_les(self, code):
        global sp, M
        M[sp-1]=M[sp-1] < M[sp]
        sp=sp-1

    def eval_leq(self, code):
        global sp, M
        M[sp-1]=M[sp-1] <= M[sp]
        sp=sp-1

    def eval_grt(self, code):
        global sp, M
        M[sp-1]=M[sp-1] > M[sp]
        sp=sp-1

    def eval_gre(self, code):
        global sp, M
        M[sp-1]=M[sp-1] >= M[sp]
        sp=sp-1

    def eval_equ(self, code):
        global sp, M
        M[sp-1]=M[sp-1] == M[sp]
        sp=sp-1

    def eval_neq(self, code):
        global sp, M
        M[sp-1]=M[sp-1] != M[sp]
        sp=sp-1

    def eval_jmp(self, code):
        global pc
        pc = code[1]

    def eval_jof(self, code):
        global sp, M, pc
        p = code[1]
        if not M[sp]:
            pc=p
        else:
            pc=pc+1
        sp=sp-1

    def eval_alc(self, code):
        global sp
        n = code[1]
        sp = sp + n

    def eval_dlc(self, code):
        global sp
        n = code[1]
        sp = sp - n

    def eval_cfu(self, code):
        global sp, pc, M

        sp +=1
        M[sp]=pc+1
        pc = code[1]

    def eval_enf(self, code):
        global sp, D, M
        k = code[1]

        sp +=1
        M[sp]=D[k]
        D[k]=sp+1

    def eval_ret(self, code):
        global sp, pc, D, M
        k = code[1]
        n = code[2]

        D[k]=M[sp]
        pc=M[sp-1]
        sp=sp-(n+2)

    def eval_idx(self, code):
        global sp, M
        k = code[1]

        M[sp-1]=M[sp-1] + M[sp] * k
        sp=sp-1

    def eval_grc(self, code):
        global sp, M
        M[sp]=M[M[sp]]

    def eval_lmv(self, code):
        global sp, M
        k = code[1]

        t=M[sp]
        M[sp:sp+k]=M[t:t+k]
        sp += (k-1)

    def eval_smv(self, code):
        global sp, M
        k = code[1]

        t = M[sp-k]
        M[t:t+k] =M[sp-k+1:sp+1]
        sp -= (k+1)

    def eval_smr(self, code):
        global sp, M
        k = code[1]

        t1 = M[sp-1]
        t2 = M[sp]
        M[t1:t1+k] = M[t2:t2+k]
        sp -= 1


    def eval_sts(self, code):
        global sp, M, H
        k = code[1]
        pass##############
        adr=M[sp]
        M[adr]=len(H[k])
        for c in H[k]:
           adr=adr+1
           M[adr]=c;
        sp=sp-1

    def eval_rdv(self, code):
        global sp, M

        sp=sp+1
        M[sp]=int(input())

    def eval_rds(self, code):
        global sp, M

        str=input()
        adr=M[sp]
        M[adr] = len(str)
        for k in str:
           adr=adr+1
           M[adr]=k
        sp=sp-1


    def eval_prv(self, code):
        global sp, M
        print(M[sp]);
        sp=sp-1

    def eval_prt(self, code):
        global sp, M
        k = code[1]
        print(M[sp-k+1:sp+1])
        sp-=(k-1)

    def eval_prc(self, code):
        global H
        i = code[1]
        print(H(i),end="")

    def eval_prs(self, code):
        global sp, M

        adr = M[sp]
        len = M[adr]
        for i in range(0,len):
           adr = adr + 1
           print(M(adr),end="")
        sp=sp-1

    def eval_stp(self, code):
        global sp, D
        D[0] = 0
        sp = -1

    def eval_lbl(self, code):
        pass

    def eval_end(self, code):
        pass

lvm = LVM()
lvm.run()

'''

passar pelos labels e substituir por PC
passar pelos jumps e substituir pelo valor trocado do label

WHILE TRUE percorrendo todas as instrucoes
    se END - sp = -1
    sai


'''