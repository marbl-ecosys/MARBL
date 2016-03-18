#!/bin/bash

build() {
  compiler=$1
  rm -f ../obj/* ../include/*
  module purge
  module load compiler/$compiler
  make $compiler
}

build intel
build nag
build gnu
