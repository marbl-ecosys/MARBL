#!/bin/bash
# Quick script to check source code for lines exceeded specified length
# Two uses:
# 1) NAG compiler requires lines no longer than 132 characters
# 2) At some point, we may want to enforce a max line length in coding guidelines

# -----------------------------------------------------------------------------

usage() {
  echo "./check_line_length.sh [-n N] [-s] [-d DIR]"
  echo ""
  echo "-n N    |    check for lines exceeding N characters (default=132)"
  echo "-s      |    silent mode - suppress output to stdout"
  echo "        |                  (rely on return code to detect problem)"
  echo "-d DIR  |    compare code in directory DIR"
}

# -----------------------------------------------------------------------------

check_length() {
  LINEMAX=$((LINELIMIT+1))
  sed "s/!.*//" $file | grep -Hn ".\{$LINEMAX\}"
}

# -----------------------------------------------------------------------------

parse_args() {

# DEFAULT SETTINGS
SILENT=FALSE
LINELIMIT=132
SRC_DIR=../src

  while [ $# -gt 0 ]; do
    case $1 in
      -n )
        if [ $# -eq 1 ]; then
          echo "ERROR: you must provide number of lines with the -n option"
          exit -1
        fi
        LINELIMIT=$2
        shift
      ;;
      -s|--silent )
        SILENT=TRUE
      ;;
      -d|--dir|--src_dir )
        if [ $# -eq 1 ]; then
          echo "ERROR: you must provide directory with the -d option"
          exit -1
        fi
        SRC_DIR=$2
        shift
      ;;
      -h|--help )
        usage
        exit 0
      ;;
      * )
        echo "ERROR: $1 is not a valid option"
        usage
        exit -1
      ;;
    esac
    shift
  done
}

# -----------------------------------------------------------------------------

process_results() {
  if [ "$FOUND" == "TRUE" ]; then
    if [ "$SILENT" != "TRUE" ]; then
      echo "Test FAILED: found at least one file exceeding $LINELIMIT character limit"
    fi
    exit 1
  else
    if [ "$SILENT" != "TRUE" ]; then
      echo "Test PASSED: no lines exceeded $LINELIMIT characters"
    fi
    exit 0
  fi
}

# -----------------------------------------------------------------------------

parse_args $@ 

FOUND=FALSE
FILELIST=""
if [ -d ${SRC_DIR} ]; then
  FILELIST=`find ${SRC_DIR} -name "*.F90"`
fi
if [ -z "$FILELIST" ] && [ "$SILENT" != "TRUE" ]; then
  echo "WARNING: no .F90 files found in ${SRC_DIR}"
fi
for file in $FILELIST
do
  if [[ ! -z `check_length` ]]; then
    FOUND=TRUE
    if [ "$SILENT" != "TRUE" ]; then
      BASEFILE=`basename $file`
      check_length | sed "s/(standard input)/$BASEFILE/"
      echo ""
    fi
  fi
done

process_results
