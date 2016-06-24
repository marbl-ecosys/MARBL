#!/bin/bash

#------------------------------------------------------------------------------

usage() {
  echo "$0 [-h] [-compiler COMPILER] [-namelist NML]"
  echo "  -h                     | Show usage"
  echo "  -compiler COMPILER     | Select compiler (default:gnu)"
  echo "                         | Options: gnu, intel, pgi, nag, cray"
}
#------------------------------------------------------------------------------

parse_input() {

  COMPILER=gnu
  while [ $# -gt 0 ]; do
    case $1 in
      -h|--help)
        usage
        exit 0
      ;;
      -build|-compiler)
        shift
        COMPILER=$1
      ;;
      *)
        echo "Invalid option: $1"
        usage
        exit 1
    esac
    shift
  done

}

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
  ../../exe/marbl.exe < marbl_in
}

#------------------------------------------------------------------------------

parse_input $@
build
run

