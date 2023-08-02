#!/bin/bash

# Set the command you want to time
if [ $1 == "rslts" ]; then
   COMMAND="rslts unfold SimpleRailBig.rsl"
   FILE="rslts"
elif [ $1 == "rsltc" ]; then
   COMMAND="rsltc -unfrtt SimpleRailBig.rsl"
   FILE="rsltc"
else
   echo "$1 is not valid..."
   exit 1
fi

# Set array to enumerate
N=(1 2 3 4 5 10 20 30 40 50 60 70 80 90 100 200 300 400)

# Write header to the CSV file


# Run the command 10 times and append output to the CSV file
for j in {0..3}
do
   LFILE="${FILE}_${j}.csv"
   echo "n,real,user,sys,mRSS" > $LFILE
   # for i in "${N[@]}"
   # do
   #    sed "s/\TheBigN/$i/g" SimpleRailBig_base.rsl > SimpleRailBig.rsl 
   #    /usr/bin/time -f "$i,%e,%U,%S,%M" -a -o $LFILE $COMMAND
   # done
done

# cp $FILE /mnt/c/thesis/


