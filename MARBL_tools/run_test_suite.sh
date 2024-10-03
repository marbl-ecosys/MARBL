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
    echo "${FAIL_CNT}. $1" >> ${FAILURE_CACHE}
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
FAILURE_CACHE=${MARBL_ROOT}/MARBL_tools/.test_suite.failures.cache
TEST_CNT=0
FAIL_CNT=0
echo "Test Results:" > ${RESULTS_CACHE}
echo "Failed Tests:" > ${FAILURE_CACHE}

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
  (set -x ; pylint --rcfile=pylintrc code_consistency.py netcdf_comparison.py netcdf_metadata_check.py)
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

# Generate settings files from every JSON file (python)
cd ${MARBL_ROOT}/MARBL_tools
(set -x ; ./MARBL_generate_settings_file.py)
STATUS=$(check_return $?)
print_status "MARBL_generate_settings_file.py" >> ${RESULTS_CACHE}
for shortname in cesm2.0 cesm2.1 cesm2.1+cocco latest latest+cocco; do
  (set -x ; ./MARBL_generate_settings_file.py -f ../defaults/json/settings_${shortname}.json -o marbl_${shortname}.settings)
  STATUS=$(check_return $?)
  print_status "MARBL_generate_settings_file.py (${shortname})" >> ${RESULTS_CACHE}
done

# Test MARBL_generate_diagnostics_file.py
cd ${MARBL_ROOT}/MARBL_tools
(set -x ; ./MARBL_generate_diagnostics_file.py --diag-mode none -o marbl.diags.none)
STATUS=$(check_return $?)
print_status "MARBL_generate_diagnostics_file.py --diag-mode none -o marbl.diags.none" >> ${RESULTS_CACHE}
(set -x ; ./MARBL_generate_diagnostics_file.py --diag-mode minimal -o marbl.diags.minimal)
STATUS=$(check_return $?)
print_status "MARBL_generate_diagnostics_file.py --diag-mode minimal marbl.diags.minimal" >> ${RESULTS_CACHE}
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
  # Initialize MARBL with settings from tests/input_files/settings/
  for settingsfile in `find ../../input_files/settings -type f`; do
    if [ "`basename $settingsfile`" == "marbl_with_ladjust_bury_coeff.settings" ]; then
      continue
    fi
    (set -x ; ./init.py -s $settingsfile)
    STATUS=$(check_return $?)
    print_status "init.py ($(basename ${settingsfile}))" >> ${RESULTS_CACHE}
  done
  # Initialize MARBL with settings generated from every JSON file
  for shortname in cesm2.0 cesm2.1 cesm2.1+cocco latest latest+cocco; do
    if [ -f ../../../MARBL_tools/marbl_${shortname}.settings ]; then
      (set -x ; ./init.py -s ../../../MARBL_tools/marbl_${shortname}.settings)
      STATUS=$(check_return $?)
      print_status "init.py (${shortname})" >> ${RESULTS_CACHE}
    fi
  done

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

  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/MARBL_tools
    # Compare netCDF output to baseline
    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst (cgs) vs baseline (cgs))" >> ${RESULTS_CACHE}

    # Compare netCDF metadata to JSON file
    (set -x ; ./netcdf_metadata_check.py)
    STATUS=$(check_return $?)
    print_status "netCDF metadata check" >> ${RESULTS_CACHE}
  fi

  # Initialize MARBL (with 4p2z), compute surface fluxes and interior tendencies
  cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
  (set -x ; ./call_compute_subroutines.py -s ../../input_files/settings/marbl_with_4p2z_cgs.settings)
  STATUS=$(check_return $?)
  print_status "call_compute_subroutines.py -s ../../input_files/settings/marbl_with_4p2z_cgs.settings" >> ${RESULTS_CACHE}

  if [ "${STATUS}" == "PASS" ]; then
    # Compare netCDF output to baseline
    cd ${MARBL_ROOT}/MARBL_tools
    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history_4p2z.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst (cgs, 4p2z) vs baseline (cgs, 4p2z))" >> ${RESULTS_CACHE}

    # Compare netCDF metadata to JSON file
    (set -x ; ./netcdf_metadata_check.py -s ../tests/input_files/settings/marbl_with_4p2z_cgs.settings -f ../defaults/json/settings_latest+4p2z.json)
    STATUS=$(check_return $?)
    print_status "netCDF metadata check (4p2z)" >> ${RESULTS_CACHE}
  fi

  # Initialize MARBL (with 4p2z), compute surface fluxes and interior tendencies in mks instead of cgs
  cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
  (set -x ; ./call_compute_subroutines.py -s ../../input_files/settings/marbl_with_4p2z_mks.settings -u mks)
  STATUS=$(check_return $?)
  print_status "call_compute_subroutines.py -s ../../input_files/settings/marbl_with_4p2z_mks.settings -u mks" >> ${RESULTS_CACHE}

  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/MARBL_tools
    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history_4p2z.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst (mks, 4p2z) vs baseline (cgs, 4p2z))" >> ${RESULTS_CACHE}
  fi

  # Initialize MARBL (with abio tracers), compute surface fluxes and interior tendencies
  cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
  (set -x ; ./call_compute_subroutines.py -s ../../input_files/settings/marbl_with_abio_only.settings)
  STATUS=$(check_return $?)
  print_status "call_compute_subroutines.py -s ../../input_files/settings/marbl_with_abio_only.settings" >> ${RESULTS_CACHE}

  if [ "${STATUS}" == "PASS" ]; then
    # Compare netCDF output to baseline
    cd ${MARBL_ROOT}/MARBL_tools
    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history_with_abio_only.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst (cgs, with abio) vs baseline (cgs, with abio))" >> ${RESULTS_CACHE}

    # Compare netCDF metadata to JSON file
    (set -x ; ./netcdf_metadata_check.py -s ../tests/input_files/settings/marbl_with_abio_only.settings)
    STATUS=$(check_return $?)
    print_status "netCDF metadata check (abio only)" >> ${RESULTS_CACHE}
  fi

  # Initialize MARBL (with abio tracers), compute surface fluxes and interior tendencies in mks instead of cgs
  cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
  (set -x ; ./call_compute_subroutines.py -s ../../input_files/settings/marbl_with_abio_only.settings -u mks)
  STATUS=$(check_return $?)
  print_status "call_compute_subroutines.py -s ../../input_files/settings/marbl_with_abio_only.settings -u mks" >> ${RESULTS_CACHE}

  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/MARBL_tools
    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history_with_abio_only.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst (mks, with abio) vs baseline (cgs, with abio))" >> ${RESULTS_CACHE}
  fi

  # Initialize MARBL (with ciso tracers), compute surface fluxes and interior tendencies
  cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
  (set -x ; ./call_compute_subroutines.py -s ../../input_files/settings/marbl_with_ciso.settings)
  STATUS=$(check_return $?)
  print_status "call_compute_subroutines.py -s ../../input_files/settings/marbl_with_ciso.settings" >> ${RESULTS_CACHE}

  if [ "${STATUS}" == "PASS" ]; then
    # Compare netCDF output to baseline
    cd ${MARBL_ROOT}/MARBL_tools
    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history_with_ciso.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst (cgs, with ciso) vs baseline (cgs, with ciso))" >> ${RESULTS_CACHE}

    # Compare netCDF metadata to JSON file
    (set -x ; ./netcdf_metadata_check.py -s ../tests/input_files/settings/marbl_with_ciso.settings)
    STATUS=$(check_return $?)
    print_status "netCDF metadata check (ciso)" >> ${RESULTS_CACHE}
  fi

  # Initialize MARBL (with ciso tracers), compute surface fluxes and interior tendencies in mks instead of cgs
  cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
  (set -x ; ./call_compute_subroutines.py -s ../../input_files/settings/marbl_with_ciso.settings -u mks)
  STATUS=$(check_return $?)
  print_status "call_compute_subroutines.py -s ../../input_files/settings/marbl_with_ciso.settings -u mks" >> ${RESULTS_CACHE}

  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/MARBL_tools
    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history_with_ciso.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst (mks, with ciso) vs baseline (cgs, with ciso))" >> ${RESULTS_CACHE}
  fi

  # Initialize MARBL, compute surface fluxes and interior tendencies in mks instead of cgs
  cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
  (set -x ; ./call_compute_subroutines.py --unit_system mks)
  STATUS=$(check_return $?)
  print_status "call_compute_subroutines.py --unit_system mks" >> ${RESULTS_CACHE}

  # Same test, but with num_inst = 2 instead of 1
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./call_compute_subroutines.py --unit_system mks -n test_2inst.nml)
    STATUS=$(check_return $?)
    print_status "call_compute_subroutines.py --unit_system mks -n test_2inst.nml" >> ${RESULTS_CACHE}
  fi

  # Same test, but with num_inst = 5 instead of 1 or 2
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    (set -x ; ./call_compute_subroutines.py --unit_system mks -n test_5inst.nml)
    STATUS=$(check_return $?)
    print_status "call_compute_subroutines.py --unit_system mks -n test_5inst.nml" >> ${RESULTS_CACHE}
  fi

  # Compare 1-inst, 2-inst and 5-inst output
  if [ "${STATUS}" == "PASS" ]; then
    cd ${MARBL_ROOT}/MARBL_tools
    HIST_ROOT=${MARBL_ROOT}/tests/regression_tests/call_compute_subroutines
    # We use "--strict exact" because we want these two files to be identical
    # When we introduce a baseline comparison, we will use "--strict loose"
    (set -x ; ./netcdf_comparison.py -b ${HIST_ROOT}/history_1inst.nc -n ${HIST_ROOT}/history_2inst.nc --strict exact)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (2 inst vs 1 inst, mks)" >> ${RESULTS_CACHE}

    (set -x ; ./netcdf_comparison.py -b ${HIST_ROOT}/history_1inst.nc -n ${HIST_ROOT}/history_5inst.nc --strict exact)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (5 inst vs 1 inst, mks)" >> ${RESULTS_CACHE}

    BASE_ROOT=${MARBL_ROOT}/tests/input_files/baselines
    (set -x ; ./netcdf_comparison.py -b ${BASE_ROOT}/call_compute_subroutines.history.nc -n ${HIST_ROOT}/history_1inst.nc --strict loose)
    STATUS=$(check_return $?)
    print_status "netCDF Comparison (1 inst (mks) vs baseline (cgs))" >> ${RESULTS_CACHE}
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

  # Print all fields MARBL needs running means of
  cd ${MARBL_ROOT}/tests/regression_tests/bury_coeff
  (set -x ; ./bury_coeff.py)
  STATUS=$(check_return $?)
  print_status "bury_coeff.py" >> ${RESULTS_CACHE}

  # Print all output_for_GCM variables
  cd ${MARBL_ROOT}/tests/regression_tests/available_output
  (set -x ; ./available_output.py)
  STATUS=$(check_return $?)
  print_status "available_output.py" >> ${RESULTS_CACHE}

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
if [ ${FAIL_CNT} -gt 0 ]; then
  cat ${FAILURE_CACHE}
fi
rm -f ${FAILURE_CACHE}
exit ${FAIL_CNT}

