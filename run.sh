#!/bin/sh

echo
echo "##### Match gen/device #####"
echo

./prettier 180 4 _
./prettier 80 4 _
./prettier 30 4 _
./prettier 15 4 _
./prettier 5 4 _

echo
echo "##### Mismatch gen/device short #####"
echo

./prettier 30 4 _ 8
./prettier 15 4 _ 8
./prettier 5 4 _ 8

echo
echo "##### Mismatch gen/device long #####"
echo

./prettier 30 8 _ 4
./prettier 15 8 _ 4
./prettier 5 8 _ 4
