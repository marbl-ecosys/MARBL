#!/bin/bash

# Convert shell return code to "PASS" or "FAIL"
# (0 = PASS, all other return codes = FAIL)
function check_return() {
  if [ $1 -eq 0 ]; then
    echo "PASS"
  else
    echo "FAIL"
  fi

}

#################################################

# Output test results
function print_status() {
  TEST_CNT=$((TEST_CNT+1))
  if [ "${STATUS}" == "FAIL" ]; then
    FAIL_CNT=$((FAIL_CNT+1))
  fi
  echo "${TEST_CNT}. $1: ${STATUS}"
}

#################################################

###############
# Global Vars #
###############

MARBL_ROOT=`(cd ..; pwd -P)`
OUTFILE=${MARBL_ROOT}/.test.out
TEST_CNT=0
FAIL_CNT=0
echo "Test Results:" > $OUTFILE

#########
# TESTS #
#########

# Code consistency check
cd ${MARBL_ROOT}/MARBL_tools
echo "$ ./code_consistency.py"
./code_consistency.py
STATUS=$(check_return $?)
print_status "CodeConsistency.py" >> $OUTFILE

# Run pylint (if installed)
command -v pylint 2>&1 > /dev/null
if [ $? -eq 0 ]; then
  cd ${MARBL_ROOT}/MARBL_tools
  echo "$ pylint --rcfile=pylintrc code_consistency.py netcdf_comparison.py"
  pylint --rcfile=pylintrc code_consistency.py netcdf_comparison.py
  STATUS=$(check_return $?)
  print_status "pylint" >> $OUTFILE
fi

# Convert YAML to JSON
cd ${MARBL_ROOT}/MARBL_tools
echo "$ ./yaml_to_json.py"
./yaml_to_json.py
STATUS=$(check_return $?)
print_status "yaml_to_json.py" >> $OUTFILE

# Check to see if JSON changed
cd ${MARBL_ROOT}/defaults/json
echo "$ git diff --exit-code ."
git diff --exit-code .
STATUS=$(check_return $?)
print_status "JSON is unchanged" >> $OUTFILE

# Generate a settings file (python)
cd ${MARBL_ROOT}/MARBL_tools
echo "$ ./MARBL_generate_settings_file.py"
./MARBL_generate_settings_file.py
STATUS=$(check_return $?)
print_status "MARBL_generate_settings_file.py" >> $OUTFILE

# Test MARBL_generate_diagnostics_file.py
cd ${MARBL_ROOT}/MARBL_tools
echo "$ ./MARBL_generate_diagnostics_file.py"
./MARBL_generate_diagnostics_file.py
STATUS=$(check_return $?)
print_status "MARBL_generate_diagnostics_file.py" >> $OUTFILE

# Clean Fortran Code
cd ${MARBL_ROOT}/tests/driver_src
echo "$ make clean"
make clean
STATUS=$(check_return $?)
print_status "make clean" >> $OUTFILE

# Build libmarbl.a
cd ${MARBL_ROOT}/tests/bld_tests
echo "$ ./bld_lib.py --no_pause"
./bld_lib.py --no_pause
STATUS=$(check_return $?)
print_status "bld_lib.py --no_pause" >> $OUTFILE

# Build stand-alone executable (only if library built successfully)
if [ "${STATUS}" == "PASS" ]; then
  cd ${MARBL_ROOT}/tests/bld_tests
  echo "$ ./bld_exe.py --no_pause"
  ./bld_exe.py --no_pause
  STATUS=$(check_return $?)
  print_status "bld_exe.py --no_pause" >> $OUTFILE
fi

# Only test Fortran executable if build was successful
if [ "${STATUS}" == "PASS" ]; then
  # get_put unit test
  cd ${MARBL_ROOT}/tests/unit_tests/get_put
  echo "$ ./get_put.py"
  ./get_put.py
  STATUS=$(check_return $?)
  print_status "get_put.py" >> $OUTFILE

  # marbl_utils unit test
  cd ${MARBL_ROOT}/tests/unit_tests/utils_routines
  echo "$ ./marbl_utils.py"
  ./marbl_utils.py
  STATUS=$(check_return $?)
  print_status "marbl_utils.py" >> $OUTFILE

  # Initialize MARBL
  cd ${MARBL_ROOT}/tests/regression_tests/init
  echo "$ ./init.py"
  ./init.py
  STATUS=$(check_return $?)
  print_status "init.py" >> $OUTFILE

  # Initialize MARBL, clean up memory, initialize again
  cd ${MARBL_ROOT}/tests/regression_tests/init-twice
  echo "$ ./init-twice.py"
  ./init-twice.py
  STATUS=$(check_return $?)
  print_status "init-twice.py" >> $OUTFILE

  # Generate a settings file (Fortran)
  cd ${MARBL_ROOT}/tests/regression_tests/gen_input_file
  echo "$ ./gen_input_file.py"
  ./gen_input_file.py
  STATUS=$(check_return $?)
  print_status "gen_input_file.py" >> $OUTFILE

  # Initialize MARBL, compute surface fluxes and interior tendencies
  cd ${MARBL_ROOT}/tests/regression_tests/compute_cols
  echo "$ ./compute_cols.py"
  ./compute_cols.py
  STATUS=$(check_return $?)
  print_status "compute_cols.py" >> $OUTFILE

  # Same test, but with num_inst = 2 instead of 1
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/tests/regression_tests/compute_cols
    echo "$ ./compute_cols.py -n test_2inst.nml"
    ./compute_cols.py -n test_2inst.nml
    STATUS=$(check_return $?)
    print_status "compute_cols.py -n test_2inst.nml" >> $OUTFILE
  fi

  # Same test, but with num_inst = 5 instead of 1 or 2
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/tests/regression_tests/compute_cols
    echo "$ ./compute_cols.py -n test_5inst.nml"
    ./compute_cols.py -n test_5inst.nml
    STATUS=$(check_return $?)
    print_status "compute_cols.py -n test_5inst.nml" >> $OUTFILE
  fi

  # Compare 1-inst, 2-inst and 5-inst output
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/MARBL_tools
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/compute_cols
    # We use "--strict exact" because we want these two files to be identical
    # When we introduce a baseline comparison, we will use "--strict loose"
    echo "$ ./netcdf_comparison.py -b ${HIST_ROOT}/history_1inst.nc -n ${HIST_ROOT}/history_2inst.nc --strict exact"
    ./netcdf_comparison.py -b ${HIST_ROOT}/history_1inst.nc -n ${HIST_ROOT}/history_2inst.nc --strict exact
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (2 inst vs 1 inst)" >> $OUTFILE

    echo "$ ./netcdf_comparison.py -b ${HIST_ROOT}/history_1inst.nc -n ${HIST_ROOT}/history_5inst.nc --strict exact"
    ./netcdf_comparison.py -b ${HIST_ROOT}/history_1inst.nc -n ${HIST_ROOT}/history_5inst.nc --strict exact
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (5 inst vs 1 inst)" >> $OUTFILE
  fi

  # Print all diagnostics MARBL can provide
  cd ${MARBL_ROOT}/tests/regression_tests/requested_diags
  echo "$ ./requested_diags.py"
  ./requested_diags.py
  STATUS=$(check_return $?)
  print_status "requested_diags.py" >> $OUTFILE

  # Print all forcings MARBL requires without multiple PAR subcols
  cd ${MARBL_ROOT}/tests/regression_tests/requested_forcings
  echo "$ ./requested_forcings.py"
  ./requested_forcings.py
  STATUS=$(check_return $?)
  print_status "requested_forcings.py" >> $OUTFILE

  # Print all forcings MARBL requires with multiple PAR subcols
  cd ${MARBL_ROOT}/tests/regression_tests/requested_forcings
  echo "$ ./requested_forcings.py -n test_with_PAR.nml"
  ./requested_forcings.py -n test_with_PAR.nml
  STATUS=$(check_return $?)
  print_status "requested_forcings.py -n test_with_PAR.nml" >> $OUTFILE

  # Print all restoring fields being requested
  cd ${MARBL_ROOT}/tests/regression_tests/requested_restoring
  echo "$ ./requested_restoring.py"
  ./requested_restoring.py -i ${MARBL_ROOT}/tests/input_files/settings/marbl_with_restore.input
  STATUS=$(check_return $?)
  print_status "requested_restoring.py" >> $OUTFILE

  # Print all tracers MARBL computes tendencies for
  cd ${MARBL_ROOT}/tests/regression_tests/requested_tracers
  echo "$ ./requested_tracers.py"
  ./requested_tracers.py
  STATUS=$(check_return $?)
  print_status "requested_tracers.py" >> $OUTFILE

  # Initialize MARBL (with MPI)
  cd ${MARBL_ROOT}/tests/regression_tests/init
  echo "$ ./init.py --mpitasks 2"
  ./init.py --mpitasks 2
  STATUS=$(check_return $?)
  print_status "init.py --mpitasks 2" >> $OUTFILE
fi

echo "----"
cat $OUTFILE
rm -f $OUTFILE
echo ""
echo "${TEST_CNT} tests were run, and $FAIL_CNT failed."
exit ${FAIL_CNT}

