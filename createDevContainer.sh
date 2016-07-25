#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

docker create -it -v $DIR:/lift/ -v ${HOME}/.ssh:/root/.ssh lift-dev
