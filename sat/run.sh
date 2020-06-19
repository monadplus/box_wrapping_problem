#!/usr/bin/env bash

for input in ./instances/*.inp; do
    output=$(basename $input .inp).out
    if timeout 60 ./sat < $input > ./out/$output; then
      echo $output
      ./checker < ./out/$output
    else
      echo "time out!"
      rm -f ./out/$output
    fi
done
