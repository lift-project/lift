#!/bin/bash

# Generate the index files needed for the native executor

find lambdas -type f -and -iname "[0-9a-f]*" > lambdas/index
cut -d '/' -f 4 lambdas/index > lambda_list
for i in `cat lambda_list`; do find lower/${i}/ -type f -and -iname "[0-9a-f]*" > lower/${i}/index; done
