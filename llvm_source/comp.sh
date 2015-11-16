#!/bin/sh
#clang-3.7 -Wno-unused-value -I/usr/share/R/include -I r-base-*/src/include -g -S -emit-llvm  out.c
clang-3.7 -Wno-unused-value -D USE_RINTERNALS -I r-base-*/src/include -g -S -emit-llvm  out.c 

