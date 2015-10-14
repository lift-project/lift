#!/bin/bash

# Generate the index files needed for the native executor

if [ $# -eq 0 ]
  then
    echo "No arguments supplied, using default foldername"
	HIGH_LEVEL=lambdas
  else
	HIGH_LEVEL=$1
fi

LOW_LEVEL="${HIGH_LEVEL}Lower"

find ${HIGH_LEVEL} -type f -and -iname "[0-9a-f]*" > ${HIGH_LEVEL}/index
cut -d '/' -f 4 ${HIGH_LEVEL}/index > lambda_list
for i in `cat lambda_list`; do find ${LOW_LEVEL}/${i}/ -type f -and -iname "[0-9a-f]*" > ${LOW_LEVEL}/${i}/index; done
rm lambda_list
