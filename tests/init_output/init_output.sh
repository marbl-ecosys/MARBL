#!/bin/bash

#------------------------------------------------------------------------------

build() {
  CWD=$PWD
  cd ../../drivers
  make $COMPILER
  if [ $? -ne 0 ]; then
    exit
  fi
  cd $CWD
}

#------------------------------------------------------------------------------

run() {
  ../../exe/marbl.exe < $NAMELIST
}

#------------------------------------------------------------------------------

if [ -z $1 ]; then
  COMPILER=gnu
else
  COMPILER=$1
fi
if [ -z $2 ]; then
  NAMELIST=marbl_in
else
  NAMELIST=$2
fi
build
run

