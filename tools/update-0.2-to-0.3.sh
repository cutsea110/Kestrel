#!/bin/sh
IFS="
"

for d in `ls -F|grep /|sed -e 's/\/$//'`
do
  echo $d;
  cd $d;

  for f in `ls -F`
  do
    echo $f;
    mv "$f" "Key {unKey = $f}";
  done

  cd ..;
  mv "$d" "Key {unKey = $d}";
done

