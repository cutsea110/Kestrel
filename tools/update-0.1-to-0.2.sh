#!/bin/sh
for d in `ls -F|grep /`
do
  echo $d;
  cd $d;

  for f in `ls -F`
  do
    echo $f;
    mv $f "PersistInt64 $f";
  done

  cd ..;
  mv $d "PersistInt64 $d";
done

