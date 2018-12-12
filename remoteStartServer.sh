ssh avus "cd /home/s1569687/lift && COMPILEONLY=yes REDIRECT_OUTPUT=/dev/pts/1 SERVERMODE=$1 screen -S lift_server -d -m /home/s1569687/lift/startServer.sh"
sleep 1
