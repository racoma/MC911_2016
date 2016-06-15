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
- locs.lya - (duvida com um 'ret' e um 'alc')
- references.lya - (duvida com um 'ret' e um 'alc')
- arrays.lya


*Tests NOK:*

- returning_location.lya 
	- problema com numeracao de variaveis (acho que a variavel b de 'dcl b bool = false;' deve valer 2, e a de 'b bool' deve valer 3)
	- problemas com 'ret' e 'alc'
- arrays_02.lya
- 2dim_arrays.lya
- slices_n_builtins.lya
