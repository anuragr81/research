#!/bin/bash

if [ ! -z "$1" ] ; then
INPUT=$1
else
 echo "Usage : $0 <FILENAME>"
 exit 1
fi

if [ ! -f $INPUT ] ;then
 echo "File : $INPUT is invalid"
 exit 1
fi

sed -e 's/\ /,/' $INPUT | sed -e 's/\ /,/'| sed -e 's/\ $//' | sed -e 's/\ /_/g' | sed -e 's/[_]*contin_numeric//' | sed -e 's/[_]*discrete_numeric//' | sed -e 's/\-//g' | tr '[:upper:]' '[:lower:]' | sed -e 's/[_][_]*/_/g'

