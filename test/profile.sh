#!/bin/bash
nsamples=1000
sleeptime=0

echo "start" > profile.txt

pid=$(pidof R)

for x in $(seq 1 $nsamples)
	
  do
	echo $x
    gdb -ex "set pagination 0" -ex "thread apply all bt" -batch -p $pid >> profile.txt
    sleep $sleeptime
  done


