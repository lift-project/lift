export ROOTDIR=/home/shunya/naums/hipeac2020/
export TRIALS=3

echo "Setting frequency 767000000..."
sudo $ROOTDIR/set_gpu_freq.sh 767000000
echo "Now frequency is `/home/shunya/gpu_freq.sh`"

chmod +x runOne.sh
chmod +x deinit_experiment_env.sh

export EXPERIMENT_INIT_DATETIME="$(date +'%Y-%m-%d-%H-%M-%S')"

echo "Temperature before execution: `cat /sys/class/thermal/thermal_zone0/temp`"