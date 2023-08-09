#!/bin/bash

if [ $1 == "rslts" ]; then
   COMMAND="rslts unfold"
   FILE="rslts"
   N=(1 2 3 4 5 10 20 30 40 50 60 70 80 90 100 200 300 400)
elif [ $1 == "rsltc" ]; then
   COMMAND="rsltc -unfrtt"
   FILE="rsltc"
   N=(1 2 3 4 5 10 20 30 40 50 60 70)
else
   echo "$1 is not valid..."
   exit 1
fi

for j in {0..10}
do
  echo "n,real,user,sys,mRSS" > "${FILE}_s_${j}.csv"
  echo "n,real,user,sys,mRSS" > "${FILE}_t_${j}.csv"
  for i in "${N[@]}"
  do
    VARIANT=""
    for k in $(seq 0 $i)
    do
      VARIANT="${VARIANT} | t${k}"
    done
    sed "s/\TheBigN/$i/g" SimpleRailSegmentId_base.rsl > SimpleRailSegmentId.rsl
    sed "s/\TheVariant/$VARIANT/g" SimpleRailTrainId_base.rsl > SimpleRailTrainId.rsl
    /usr/bin/time -f "$i,%e,%U,%S,%M" -a -o "${FILE}_s_${j}.csv" $COMMAND SimpleRailSegmentId.rsl
    /usr/bin/time -f "$i,%e,%U,%S,%M" -a -o "${FILE}_t_${j}.csv" $COMMAND SimpleRailTrainId.rsl
  done
  echo "Done ${j}"
done
