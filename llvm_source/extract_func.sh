#!/bin/sh

llvm-extract-3.7 -S -o=extract.ll -func=cmp_relop -func=cmp_arith2 -func=getPrimitive --glob=.str.56.1526 --glob=.str.160.1597 --glob=.str.161.1598 --glob=.str.162.1599 -func=do_logic --glob=.str.2476 --func=lunary --glob=.str.13.2490 --func=lbinary --func=binaryLogic --glob=.str.12.2484 --func=binaryLogic2 --glob=.str.8.2486 --glob=.str.10.2488 --glob=.str.9.2487 --glob=.str.14.2491 --glob=.str.11.2489 R.bin.linked.bc
#opt-3.7 -S -o extract2.ll -strip-debug extract.ll
llvm-link-3.7 -S -o complete.ll extract.ll out.ll
