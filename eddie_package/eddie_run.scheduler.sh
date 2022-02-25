#!/usr/bin/env bash
start_date="$(date +'%d.%m.%Y_%H.%M.%S')"
run_label=refactored_cstrs_for_cc22_artifact

cp eddie_run.template.sh eddie_run.instance.sh
sed -i "s~START_DATE~${start_date}~g" eddie_run.instance.sh
sed -i "s~START_DATE~${start_date}~g" src/test/resources/logback-test.xml
sed -i "s~START_DATE~${start_date}~g" classpath/test-classes/resources/logback-test.xml
sed -i "s~START_DATE~${start_date}~g" classpath/test-classes/logback-test.xml

sed -i "s~RUN_LABEL~${run_label}~g" eddie_run.instance.sh
sed -i "s~RUN_LABEL~${run_label}~g" src/test/resources/logback-test.xml
sed -i "s~RUN_LABEL~${run_label}~g" classpath/test-classes/resources/logback-test.xml
sed -i "s~RUN_LABEL~${run_label}~g" classpath/test-classes/logback-test.xml

mkdir -p /exports/eddie/scratch/s1569687/eddie_logs/${start_date}
mkdir -p /exports/eddie/scratch/s1569687/src_archives/src_archive_${start_date}_${run_label}

rsync -a --info=progress2 src-lib.tar.gz /exports/eddie/scratch/s1569687/src_archives/src_archive_${start_date}_${run_label}/

qsub -t 1-2048 eddie_run.instance.sh