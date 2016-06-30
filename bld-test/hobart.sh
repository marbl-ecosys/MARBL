#!/bin/bash

build() {
  compiler=$1
  rm -f ../obj/* ../include/*
  module purge
  module load compiler/$compiler
  make $compiler
}

pause() {
  read -n1 -r -p "Press any key to continue..." key
}

build intel
pause
build nag
pause
build gnu
