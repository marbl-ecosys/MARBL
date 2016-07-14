#!/bin/bash

build() {
  CWD=$PWD
  compiler=$1
  module purge
  module load compiler/$compiler
  cd ../../src
  make $compiler
  cd $CWD
}

pause() {
  read -n1 -r -p "Press any key to continue..." key
}

build intel
pause
build nag
pause
build gnu
