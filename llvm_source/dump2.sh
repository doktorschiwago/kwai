#!/bin/sh
clang -E -I/usr/share/R/include -I r-base-*/src/include r-base-*/src/main/eval.c  | less

