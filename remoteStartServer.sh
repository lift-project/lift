ssh avus "cd /home/s1569687/lift && $1 REDIRECT_OUTPUT=$2 $3 screen -S lift_server -d -m /home/s1569687/lift/startServer.sh" 
sleep 1
