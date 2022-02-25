#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N RUN_LABEL
#$ -wd /exports/eddie/scratch/s1569687/eddie_logs/START_DATE
#$ -l h_rt=4:00:00
#$ -l h_vmem=8G
#  These options are:
#  job name: -N
#  use the current working directory: -cwd
#  runtime limit of N minutes: -l h_rt
#  memory limit of x Gbyte: -l h_vmem
. /etc/profile.d/modules.sh
module load java/jdk/1.8.0

let "initial_seed = $SGE_TASK_ID + 501"

# Iterate through all layers
#n_vgg_layers=9
#let "layer = ($SGE_TASK_ID - 1) % $n_vgg_layers"
## Skip some layers
#if [ "$layer" -eq 8 ]
#then
#    layer=10
#fi
#if [ "$layer" -eq 7 ]
#then
#    layer=8
#fi
#if [ "$layer" -eq 6 ]
#then
#    layer=7
#fi
layer=8
PAR_MAPPINGS_CSV=""

# date_start="$(date +'%d.%m.%Y_%H.%M.%S')"

ulimit -v
lscpu 

# todo:  -Xmx 2G
java -Dexperiment_label=RUN_LABEL_$SGE_TASK_ID -Dfile.encoding=UTF-8 -classpath "/home/s1569687/eddie_package/classpath/test-classes:/home/s1569687/eddie_package/classpath/classes:/home/s1569687/eddie_package/classpath/logback-classic-1.1.7.jar:/home/s1569687/eddie_package/classpath/logback-core-1.1.7.jar:/home/s1569687/eddie_package/classpath/jackson-annotations-2.9.8.jar:/home/s1569687/eddie_package/classpath/jackson-core-2.9.8.jar:/home/s1569687/eddie_package/classpath/jackson-databind-2.9.8.jar:/home/s1569687/eddie_package/classpath/jackson-datatype-jdk8-2.9.8.jar:/home/s1569687/eddie_package/classpath/jackson-datatype-jsr310-2.9.8.jar:/home/s1569687/eddie_package/classpath/cpprof-java-1.3.0.jar:/home/s1569687/eddie_package/classpath/nscala-time_2.11-2.16.0.jar:/home/s1569687/eddie_package/classpath/scopt_2.11-4.0.0-RC2.jar:/home/s1569687/eddie_package/classpath/protobuf-java-2.6.1.jar:/home/s1569687/eddie_package/classpath/junit-interface-0.11.jar:/home/s1569687/eddie_package/classpath/play-functional_2.11-2.7.3.jar:/home/s1569687/eddie_package/classpath/play-json_2.11-2.7.3.jar:/home/s1569687/eddie_package/classpath/scala-logging_2.11-3.9.2.jar:/home/s1569687/eddie_package/classpath/commons-cli-1.4.jar:/home/s1569687/eddie_package/classpath/VectorGraphics2D-0.13.jar:/home/s1569687/eddie_package/classpath/automaton-1.11-8.jar:/home/s1569687/eddie_package/classpath/jline-2.12.1.jar:/home/s1569687/eddie_package/classpath/joda-time-2.10.1.jar:/home/s1569687/eddie_package/classpath/junit-4.11.jar:/home/s1569687/eddie_package/classpath/trove4j-3.0.3.jar:/home/s1569687/eddie_package/classpath/choco-sat-1.0.2.jar:/home/s1569687/eddie_package/classpath/choco-solver-4.10.1.jar:/home/s1569687/eddie_package/classpath/cutoffseq-1.0.5.jar:/home/s1569687/eddie_package/classpath/hamcrest-core-1.3.jar:/home/s1569687/eddie_package/classpath/jfreesvg-2.0.jar:/home/s1569687/eddie_package/classpath/jgrapht-core-1.3.1.jar:/home/s1569687/eddie_package/classpath/jheaps-0.10.jar:/home/s1569687/eddie_package/classpath/joda-convert-1.2.jar:/home/s1569687/eddie_package/classpath/xchart-3.5.4.jar:/home/s1569687/eddie_package/classpath/scala-parser-combinators_2.11-1.1.2.jar:/home/s1569687/eddie_package/classpath/scala-xml_2.11-1.0.4.jar:/home/s1569687/eddie_package/classpath/scala-compiler-2.11.8.jar:/home/s1569687/eddie_package/classpath/scala-library-2.11.8.jar:/home/s1569687/eddie_package/classpath/scala-reflect-2.11.8.jar:/home/s1569687/eddie_package/classpath/test-interface-1.0.jar:/home/s1569687/eddie_package/classpath/scalacheck_2.11-1.14.0.jar:/home/s1569687/eddie_package/classpath/slf4j-api-1.7.26.jar:/home/s1569687/eddie_package/classpath/macro-compat_2.11-1.1.1.jar:/home/s1569687/eddie_package/classpath" rewriting.guided.ConvExploration --path_to_settings /home/s1569687/eddie_package/compileOnEddieRunOnBoard.json --net_name vgg --compile_for_board --overwrite_compiled_files --enforce_solution_uniqueness_through_cstr --old_advanced_test_harness --compilation_timeout 10 --total_tune_points 2000 --tune_points_batch_size 200 --from_layer ${layer} --to_layer ${layer} --initial_seed ${initial_seed} --lambda_dir_subroot generated_files_RUN_LABEL_START_DATE ${PAR_MAPPINGS_CSV} --job_id $SGE_TASK_ID --choose_local_size_deterministically