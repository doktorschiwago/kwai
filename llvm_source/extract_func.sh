#!/bin/sh

llvm-extract-3.7 -S -o=extract.ll -func=cmp_relop -func=cmp_arith2 -func=getPrimitive --glob=.str.56.1526 --glob=.str.160.1597 --glob=.str.161.1598 --glob=.str.162.1599 -func=do_logic --glob=.str.2476 --func=lunary --glob=.str.13.2490 --func=lbinary --func=binaryLogic --glob=.str.12.2484 --func=binaryLogic2 --glob=.str.8.2486 --glob=.str.10.2488 --glob=.str.9.2487 --glob=.str.14.2491 --glob=.str.11.2489 -func=asLogicalNoNA --glob=.str.119.1624 --glob=.str.120.1625 --glob=.str.121.1626 --glob=.str.122.1627 R.bin.linked.bc
llvm-link-3.7 -S -o complete0.ll extract.ll out.ll
opt-3.7 -O2 -S -o complete.ll complete0.ll
