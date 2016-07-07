#!/bin/bash

build() {
  compiler=$1
  module purge
  module load $compiler
  make $compiler
}

pause() {
  read -n1 -r -p "Press any key to continue..." key
}

build intel
pause
build gnu
