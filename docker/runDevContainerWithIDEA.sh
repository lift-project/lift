#!/bin/bash

ip=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
if command -v xhost > /dev/null 2>&1; then
    xhost=$(command -v xhost)
    xhost + $ip
    docker run -d -e DISPLAY=$ip:0 -v /tmp/.X11-unix/:/tmp/.X11-unix -v ${HOME}/.ssh:/root/.ssh lift:dev-with-idea
elif [ -f /usr/X11/bin/xhost ]; then
    /usr/X11/bin/xhost + $ip
    docker run -d -e DISPLAY=$ip:0 -v /tmp/.X11-unix/:/tmp/.X11-unix -v ${HOME}/.ssh:/root/.ssh lift:dev-with-idea
else
    echo "Couldn't find xhost command"
fi
