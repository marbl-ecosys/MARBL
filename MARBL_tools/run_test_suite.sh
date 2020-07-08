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
  HEAD="   "
  TAIL=""
  if [ "${STATUS}" == "FAIL" ]; then
    FAIL_CNT=$((FAIL_CNT+1))
    HEAD="***"
    TAIL=" ***"
  fi
  echo "${HEAD} ${TEST_CNT}. $1: ${STATUS}${TAIL}"
}

#################################################

###############
# Global Vars #
###############

MARBL_ROOT=`(cd ..; pwd -P)`
RESULTS_CACHE=${MARBL_ROOT}/MARBL_tools/.test_suite.cache
TEST_CNT=0
FAIL_CNT=0
echo "Test Results:" > ${RESULTS_CACHE}

#########
# TESTS #
#########

# Code consistency check
cd ${MARBL_ROOT}/MARBL_tools
(set -x ; ./code_consistency.py)
STATUS=$(check_return $?)
print_status "CodeConsistency.py" >> ${RESULTS_CACHE}

# Run pylint (if installed)
command -v pylint 2>&1 > /dev/null
if [ $? -eq 0 ]; then
  cd ${MARBL_ROOT}/MARBL_tools
  (set -x ; pylint --rcfile=pylintrc code_consistency.py netcdf_comparison.py)
  STATUS=$(check_return $?)
  print_status "pylint" >> ${RESULTS_CACHE}
fi

# Convert YAML to JSON
cd ${MARBL_ROOT}/MARBL_tools
(set -x ; ./yaml_to_json.py)
STATUS=$(check_return $?)
print_status "yaml_to_json.py" >> ${RESULTS_CACHE}

# Check to see if JSON changed
cd ${MARBL_ROOT}/defaults/json
(set -x ; git diff --exit-code .)
STATUS=$(check_return $?)
print_status "JSON is unchanged" >> ${RESULTS_CACHE}

# Generate a settings file (python)
cd ${MARBL_ROOT}/MARBL_tools
(set -x ; ./MARBL_generate_settings_file.py)
STATUS=$(check_return $?)
print_status "MARBL_generate_settings_file.py" >> ${RESULTS_CACHE}

# Test MARBL_generate_diagnostics_file.py
cd ${MARBL_ROOT}/MARBL_tools
(set -x ; ./MARBL_generate_diagnostics_file.py)
STATUS=$(check_return $?)
print_status "MARBL_generate_diagnostics_file.py" >> ${RESULTS_CACHE}

# Clean Fortran Code
cd ${MARBL_ROOT}/tests/driver_src
(set -x ; make clean)
STATUS=$(check_return $?)
print_status "make clean" >> ${RESULTS_CACHE}

# Build libmarbl.a
cd ${MARBL_ROOT}/tests/bld_tests
(set -x ; ./bld_lib.py --no_pause)
STATUS=$(check_return $?)
print_status "bld_lib.py --no_pause" >> ${RESULTS_CACHE}

# Build stand-alone executable (only if library built successfully)
if [ "${STATUS}" == "PASS" ]; then
  cd ${MARBL_ROOT}/tests/bld_tests
  (set -x ; ./bld_exe.py --no_pause)
  STATUS=$(check_return $?)
  print_status "bld_exe.py --no_pause" >> ${RESULTS_CACHE}
fi

# Only test Fortran executable if build was successful
if [ "${STATUS}" == "PASS" ]; then
  # get_put unit test
  cd ${MARBL_ROOT}/tests/unit_tests/get_put
  (set -x ; ./get_put.py)
  STATUS=$(check_return $?)
  print_status "get_put.py" >> ${RESULTS_CACHE}

  # marbl_utils unit test
  cd ${MARBL_ROOT}/tests/unit_tests/utils_routines
  (set -x ; ./marbl_utils.py)
  STATUS=$(check_return $?)
  print_status "marbl_utils.py" >> ${RESULTS_CACHE}

  # Initialize MARBL
  cd ${MARBL_ROOT}/tests/regression_tests/init
  (set -x ; ./init.py)
  STATUS=$(check_return $?)
  print_status "init.py" >> ${RESULTS_CACHE}

  # Initialize MARBL, clean up memory, initialize again
  cd ${MARBL_ROOT}/tests/regression_tests/init-twice
  (set -x ; ./init-twice.py)
  STATUS=$(check_return $?)
  print_status "init-twice.py" >> ${RESULTS_CACHE}

  # Generate a settings file (Fortran)
  cd ${MARBL_ROOT}/tests/regression_tests/gen_settings_file
  (set -x ; ./gen_settings_file.py)
  STATUS=$(check_return $?)
  print_status "gen_settings_file.py" >> ${RESULTS_CACHE}

  # Initialize MARBL, compute surface fluxes and interior tendencies
  cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
  (set -x ; ./call_compute_subroutines.py)
  STATUS=$(check_return $?)
  print_status "call_compute_subroutines.py" >> ${RESULTS_CACHE}

  # Same test, but with num_inst = 2 instead of 1
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./call_compute_subroutines.py -n test_2inst.nml)
    STATUS=$(check_return $?)
    print_status "call_compute_subroutines.py -n test_2inst.nml" >> ${RESULTS_CACHE}
  fi

  # Same test, but with num_inst = 5 instead of 1 or 2
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./call_compute_subroutines.py -n test_5inst.nml)
    STATUS=$(check_return $?)
    print_status "call_compute_subroutines.py -n test_5inst.nml" >> ${RESULTS_CACHE}
  fi

  # Compare 1-inst, 2-inst and 5-inst output
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/MARBL_tools
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    # We use "--strict exact" because we want these two files to be identical
    # When we introduce a baseline comparison, we will use "--strict loose"
    (set -x ; ./netcdf_comparison.py -b ${HIST_ROOT}/history_1inst.nc -n ${HIST_ROOT}/history_2inst.nc --strict exact)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (2 inst vs 1 inst)" >> ${RESULTS_CACHE}

    (set -x ; ./netcdf_comparison.py -b ${HIST_ROOT}/history_1inst.nc -n ${HIST_ROOT}/history_5inst.nc --strict exact)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (5 inst vs 1 inst)" >> ${RESULTS_CACHE}

    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst vs baseline)" >> ${RESULTS_CACHE}
  fi

  # Print all diagnostics MARBL can provide
  cd ${MARBL_ROOT}/tests/regression_tests/requested_diags
  (set -x ; ./requested_diags.py)
  STATUS=$(check_return $?)
  print_status "requested_diags.py" >> ${RESULTS_CACHE}

  # Print all forcings MARBL requires without multiple PAR subcols
  cd ${MARBL_ROOT}/tests/regression_tests/requested_forcings
  (set -x ; ./requested_forcings.py)
  STATUS=$(check_return $?)
  print_status "requested_forcings.py" >> ${RESULTS_CACHE}

  # Print all forcings MARBL requires with multiple PAR subcols
  cd ${MARBL_ROOT}/tests/regression_tests/requested_forcings
  (set -x ; ./requested_forcings.py -n test_with_PAR.nml)
  STATUS=$(check_return $?)
  print_status "requested_forcings.py -n test_with_PAR.nml" >> ${RESULTS_CACHE}

  # Print all restoring fields being requested
  cd ${MARBL_ROOT}/tests/regression_tests/requested_restoring
  (set -x ; ./requested_restoring.py)
  STATUS=$(check_return $?)
  print_status "requested_restoring.py" >> ${RESULTS_CACHE}

  # Print all tracers MARBL computes tendencies for
  cd ${MARBL_ROOT}/tests/regression_tests/requested_tracers
  (set -x ; ./requested_tracers.py)
  STATUS=$(check_return $?)
  print_status "requested_tracers.py" >> ${RESULTS_CACHE}

  # Initialize MARBL (with MPI)
  cd ${MARBL_ROOT}/tests/regression_tests/init
  (set -x ; ./init.py --mpitasks 2)
  STATUS=$(check_return $?)
  print_status "init.py --mpitasks 2" >> ${RESULTS_CACHE}
fi

echo "----"
cat ${RESULTS_CACHE}
rm -f ${RESULTS_CACHE}
echo ""
echo "${TEST_CNT} tests were run, and $FAIL_CNT failed."
exit ${FAIL_CNT}

