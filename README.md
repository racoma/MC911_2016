# MC911_2016

Second fase: semantic processing and code generation for LVM, the Lya Virtual Machine.
Main files: lyalex.py lyaparser.py lyasem.py lyacod.py

Running:

python3 lyacod.py exemple.lya

*Tests OK:*

- ex_proc.lya (existe um dlc que não está na resolução do professor, mas acredito que está certo)
- ex_do_for.lya
- do_while.lya (duvida com um 'ldc', true)
- for_while.lya
- ex_results.lya
- locs.lya 
- references.lya
- arrays.lya
- returning_location.lya 
- arrays_02.lya
- bubble.lya


*Tests NOK:*
- 2dim_arrays.lya  --     ('idx', 5) ---> ('idx', 10) na resolução
- slices_n_builtins.lya



*REFACTOR*
- Feito troca de vardict e scopedict para a enviroment stack do lyasym
- Consertado param_init

*Implementado LVM*
