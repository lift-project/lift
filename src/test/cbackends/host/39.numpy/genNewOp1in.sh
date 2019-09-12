#!/usr/bin/env bash 

set -e
set -o pipefail


# Activate debug
# set -x

if [ -f $1.cpp ]; then
	echo "already generated, neglect"
	sleep 1
else
	sed 's/sqrt/'"$1"'/g' sqrt.cpp > $1.cpp
	echo "change data please ..."
	sleep 1
	nvim "+normal 10G26|" $1.cpp
fi

if [ -f $1.py ]; then
	echo "already generated, neglect"
	sleep 1
else
	sed 's/sqrt/'"$1"'/g' ./numpy/sqrt.py > ./numpy/$1.py
	chmod +x ./numpy/$1.py
	echo "change to the same data please ..."
	sleep 1
	nvim "+normal 6G14|" ./numpy/$1.py
fi

echo "add compilation commands please ..."
sleep 1
nvim "+normal G7k" genMain.sh

echo "now executing ..."
sleep 1
./genMain.sh
