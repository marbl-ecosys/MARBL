#!/bin/bash

build() {
  compiler=$1
  rm -f ../obj/* ../include/*
  module purge
  module load $compiler
  make $compiler
}

build intel
build gnu
