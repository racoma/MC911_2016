#!/bin/bash
#Declare array with 4 elements
ARRAY=( 'ex_proc.lya' 'ex_do_for.lya' 'do_while.lya' 'for_while.lya' 'ex_results.lya' 
		'locs.lya' 'references.lya' 'arrays.lya' 'arrays_02.lya' '2dim_arrays.lya' 'returning_location.lya' 'bubble.lya')
# get number of elements in the array
ELEMENTS=${#ARRAY[@]}

# echo each element in array 
# for loop
for (( i=0;i<$ELEMENTS;i++)); do
    echo "Running for:" ${ARRAY[${i}]}
    python3 lyacod.py ./examples/${ARRAY[${i}]} > ./results/${ARRAY[${i}]}.out
    diff ./results/${ARRAY[${i}]}.out ./results/${ARRAY[${i}]}.res > ./results/diff/${ARRAY[${i}]}
done 
