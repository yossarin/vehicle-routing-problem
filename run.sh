#!/bin/bash

#  This script looks for all the files that start with params*.txt,
#  and runs the hmo-project executable using the params*.txt files
#  as parameters for each run. The results are saved in the same
#  directory as $FILENAME.res

for P in `ls params*.txt`
do
    echo " Runnig with parameters $P"
    ./haskell/dist/build/hmo-project/hmo-project in.txt $P +RTS -N -RTS > $P.res
    echo " Done for $P"
    tail -n 1 $P.res
done
