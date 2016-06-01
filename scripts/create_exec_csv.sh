#!/bin/bash

FILE=$1

#FILE=/home/s1042579/Documents/apart/rewriting/mmTransposeAClFiltered/cdcac67540d97b676facdbd40b293b9940699f5a471b1060ef330eea2253dc55/15129a55241ca6992f1adc9094197758f5ae9acf9a2c659e28d416c5f52e363f.cl

M_VAR="v_0_0"
N_VAR="v_1_1"
#K_VAR="v_4_4"

M_SIZE=2048
N_SIZE=2048
#K_SIZE=512

REPLACEPATTERN_M="s/$M_VAR/$M_SIZE/g"
REPLACEPATTERN_N="s/$N_VAR/$N_SIZE/g"
#REPLACEPATTERN_N="s/$N_VAR/$N_SIZE/g"

KERNEL_HASH=$(basename "$FILE" | cut -d . -f 1)
DIR=$(dirname "$FILE")
CSV_FILE="${DIR}/exec_${M_SIZE}_${K_SIZE}_${N_SIZE}.csv"

L_SIZES=$(grep Local $FILE | cut  -d : -f 2)
G_SIZES=$(grep Global $FILE | cut -d : -f 2 | sed "$REPLACEPATTERN_M" | sed "$REPLACEPATTERN_N" | sed 's/\/^/\//g' | tr ',' '\n' | xargs -I {} -n 1 bash -c 'echo "$(($1))"' -- {} | paste -sd ',' -)

echo $CSV_FILE
echo "0,${G_SIZES},${L_SIZES},${KERNEL_HASH},0,0"
