#!/bin/bash
# Quick script to check source code for lines exceeded specified length
# Two uses:
# 1) NAG compiler requires lines no longer than 132 characters
# 2) At some point, we may want to enforce a max line length in coding guidelines

# -----------------------------------------------------------------------------

usage() {
  echo "./check_line_length.sh [-n N] [-s]"
  echo ""
  echo "-n N    |    check for lines exceeding N characters (default=132)"
  echo "-s      |    silent mode - suppress output to stdout"
  echo "        |                  (rely on return code to detect problem)"
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
      -s )
        SILENT=TRUE
      ;;
      -h )
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
for file in ../src/*.F90
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
