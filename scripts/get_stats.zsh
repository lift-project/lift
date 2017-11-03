#!/usr/bin/env zsh

ALL_BENCHMARKS=$argv

mkdir -p stats

for BENCHMARK_FOLDER in $ALL_BENCHMARKS
do

  BENCHMARK=$(sed 's/Cl//g' <<< $BENCHMARK_FOLDER)

  echo $BENCHMARK

  STATS_FILES=$(find $BENCHMARK_FOLDER -name "stats*" | cut -d / -f 3 | sort  | uniq | sed '/^$/d')
  STATS_FILES=("${(@f)$(echo $STATS_FILES)}")

  mkdir -p stats/$BENCHMARK

  for SIZE in $STATS_FILES
  do
    DESTINATION=stats/$BENCHMARK/$SIZE
    cat $BENCHMARK_FOLDER/stats_header.csv > $DESTINATION
    find $BENCHMARK_FOLDER -name $SIZE | xargs cat >> $DESTINATION
  done
done
