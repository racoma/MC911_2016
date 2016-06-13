#!/bin/bash
#Declare array with 4 elements
ARRAY=( 'ex_proc.lya' 'ex_do_for.lya' 'do_while.lya' 'for_while.lya' 'ex_results.lya' 
		'locs.lya' 'references.lya')
# get number of elements in the array
ELEMENTS=${#ARRAY[@]}

# echo each element in array 
# for loop
for (( i=0;i<$ELEMENTS;i++)); do
    echo "Running for:" ${ARRAY[${i}]}
    python3 lyacod.py ${ARRAY[${i}]} > ./results/${ARRAY[${i}]}.out
done 