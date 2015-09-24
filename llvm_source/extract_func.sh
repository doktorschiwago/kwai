#!/bin/sh

llvm-extract -S -o=out2.ll -func=do_subset2_dflt R.bin.linked.bc
