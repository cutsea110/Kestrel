#!/bin/sh
IFS="
"

rm -rf _thumbnail;

for d in `ls -F|grep /|sed -e 's/\/$//'`
do
  echo $d;
  cd $d;

  for f in `ls -F`
  do
    echo $f;
    newf=`expr $f : "Key {unKey = PersistInt64 \(.*\)}"`;
    mv "$f" $newf
  done

  cd ..;
  newd=`expr $d : "Key {unKey = PersistInt64 \(.*\)}"`
  mv "$d" $newd
done

echo @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
echo please generate thumbnails again!
echo access to http://localost/system-batch
echo @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
