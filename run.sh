#!/bin/bash

directories=("./0001-0050" "./0051-0100")

for directory in ${directories[*]}
do
    cd "$directory"

    for entry in "."/*
    do
      echo -en "\n<--------> TASK: " "$entry" " <-------->\n"
      date && runhaskell "$entry" && date
    done

    cd ..
done
