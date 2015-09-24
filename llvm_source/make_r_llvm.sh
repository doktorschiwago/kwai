#!/bin/sh
apt-get source r-base

rm r-base*.diff.gz
rm r-base*.dsc
rm r-base*.orig.tar.gz

cd $(ls -1 | grep "r-base")

../configure2

make

cp src/main/R.bin.linked.bc ../

cd ..

rm -rf $(ls -1 | grep "r-base")
