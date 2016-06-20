#!/bin/bash

#------------------------------------------------------------------------------

usage() {
  echo "$0 [-h] [-compiler COMPILER] [-namelist NML]"
  echo "  -h                     | Show usage"
  echo "  -compiler COMPILER     | Select compiler (default:gnu)"
  echo "                         | Options: gnu, intel, pgi, nag, cray"
  echo "  -namelist NML          | Select namelist (default:marbl_in)"
}
#------------------------------------------------------------------------------

parse_input() {

  COMPILER=gnu
  NAMELIST=marbl_in
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
      -nml|-namelist)
        shift
        NAMELIST=$1
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
  ../../exe/marbl.exe < $NAMELIST
}

#------------------------------------------------------------------------------

parse_input $@
build
run

