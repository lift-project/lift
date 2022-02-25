#!/usr/bin/env bash

set -e
set -o pipefail

# Activate debug
# set -x


# tee ~/Downloads/test.txt | sed 's/Arr//g' | sed 's/s=//g' | sed 's/c=//g' | sed 's/float,//g' | sed -E 's/([0-9]+),[0-9]+/\1/g' 
sed 's/@/\n/g' | sed 's/£/  /g' | sed 's/Arr//g' | sed 's/s=//g' | sed 's/c=//g' | sed 's/float,//g' | sed -E 's/([0-9]+),[0-9]+/\1/g' 

# vim tmp.txt
# rm tmp.txt

