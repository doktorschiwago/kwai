file /usr/lib/R/bin/exec/R
set exec-wrapper /usr/lib/R/bin/R
set args -f test15.R

directory ~/unison/dev/kwai/llvm_source/r-base-3.2.2/src/main/

set logging file profile.txt
set logging on
set pagination off
r
