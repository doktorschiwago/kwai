#!/bin/bash

#awk '
#  BEGIN { s = ""; } 
#  /^Thread/ { print s; s = ""; } 
#  /^\#/ { if (s != "" ) { s = s "," $4} else { s = $4 } } 
#  END { print s }' profile.txt | \
#sort | uniq -c | sort -r -n -k 1,1

./profile2b.py | sort | uniq -c | sort -r -n -k 1,1 | less
