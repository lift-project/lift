#!/usr/bin/env zsh

mkdir -p stats

for BENCHMARK_FOLDER in "$@"
do

  BENCHMARK=$(sed 's/Cl//g' <<< $BENCHMARK_FOLDER)

  echo $BENCHMARK

  SIZES=$(find $BENCHMARK_FOLDER -name "stats*" | cut -d / -f 3 | sort  | uniq | sed -e '/^$/d' -e 's/stats//g')
  SIZES=("${(@f)$(echo $SIZES)}")

  mkdir -p stats/$BENCHMARK

  for SIZE in $SIZES
  do
    STATS_FILENAME=stats$SIZE
    STATS_DESTINATION=stats/$BENCHMARK/$STATS_FILENAME

    cat $BENCHMARK_FOLDER/stats_header.csv > $STATS_DESTINATION
    find $BENCHMARK_FOLDER -name $STATS_FILENAME | xargs cat >> $STATS_DESTINATION

    USERFUNS_FILENAME=userfuns$SIZE
    USERFUNS_DESTINATION=stats/$BENCHMARK/$USERFUNS_FILENAME
    find $BENCHMARK_FOLDER -name $USERFUNS_FILENAME | xargs cat > $USERFUNS_DESTINATION
  done
done
