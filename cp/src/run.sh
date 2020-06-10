#!/usr/bin/env bash

IN=../instances
OUT=../out

for input in $IN/*.inp; do
    output=$(basename $input .inp).out
    if timeout 60 ./p < $input > $OUT/$output; then
      echo "ok!"
      ./checker < $OUT/$output
    else
      echo "time out!"
      rm -f $OUT/$output
    fi
done
