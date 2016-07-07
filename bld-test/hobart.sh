#!/bin/bash

build() {
  compiler=$1
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
