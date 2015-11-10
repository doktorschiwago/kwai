#!/bin/sh

DOWNLOAD_SOURCE=0
DELETE_SOURCE=0
while getopts ":a" opt; do
  case $opt in
    --download-source)
      DOWNLOAD_SOURCE=1
      ;;
    --delete-source)
      DELETE_SOURCE=1
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ;;
  esac
done

#only download if requested
if [ $DOWNLOAD_SOURCE -eq 1 ]; then
	apt-get source r-base
	rm r-base*.diff.gz
	rm r-base*.dsc
	rm r-base*.orig.tar.gz
fi

R_PATH=$(ls -1d */ | grep "r-base")

echo "Checking R Source in $R_PATH"

if [ ! -d "$R_PATH" ]; then
	echo "could not find R source!"
	exit
fi



cd $R_PATH

../configure2

make clean

make

cp src/main/R.bin.linked.bc ../

cd ..

if [ $DELETE_SOURCE -eq 1 ]; then
	rm -rf $R_PATH
fi
