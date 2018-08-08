#!/bin/bash

directories=("./0001-0020" "./0021-0040" "./0041-0060" "./0061-0080" "./0081-0100" "./0101-0150")

for directory in ${directories[*]}
do
    cd "$directory"

    for entry in "."/*
    do
      if [ ${entry: -3} == ".hs" ]
      then
        echo -en "\n<--------> TASK: " "$entry" " <-------->\n"
        date && runhaskell "$entry" && date
      fi
    done

    cd ..
done
