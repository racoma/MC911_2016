# MC911_2016

Second fase: semantic processing and code generation for LVM, the Lya Virtual Machine.
Main files: lyalex.py lyaparser.py lyasem.py lyacod.py

Running:

python3 lyacod.py exemple.lya

Tests OK:

ex_proc.lya
ex_do_for.lya
do_while.lya
for_while.lya
ex_results.lya

Tests NO:

locs.lya
slices_n_builtins.lya
returning_location.lya
references.lya
arrays.lya
arrays_02.lya
2dim_arrays.lya
