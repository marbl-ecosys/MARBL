#!/usr/bin/env python

"""
    Usage:
        $ ./netcdf_comparison.py --baseline BASELINE_FILE --compare NEW_FILE

    Compare two netcdf files. For each variable, flag
    1. Variables that are present in one file but not the other
    2. Variables where the data type doesn't match across files
    3. Variables where the dimensions don't match across files
    4. Variables with array members where the baseline is 0 and the new file is not
    5. Variables where the relative difference between the two files large
       -- NOTE: we do not flag points where the relative difference is large but
                the absolute difference is smaller than 1e-12 (there are MARBL
                diagnostics that should be identically equal to 0 but are O(1e-19)
                due to taking the difference between two numbers that are very close
                to each other)
"""

import logging

##################

def _init(baseline, new_file):
    import xarray as xr

    logger = logging.getLogger(__name__)
    logger.info("Comparing %s to the baseline %s\n", new_file, baseline)
    ds_base = xr.open_dataset(baseline)
    ds_new = xr.open_dataset(new_file)
    fail = False

    # Compare file headers
    header_fail, ds_base, ds_new = _header_test(ds_base, ds_new)
    fail = fail or header_fail

    return fail, ds_base, ds_new

##################

def netcdf_comparison_exact(baseline, new_file):
    """
        Compare baseline to new_file using xarray
        Will only pass if two files are identical (bfb)
        (run with --strict=exact)
    """
    header_fail, ds_base, ds_new = _init(baseline, new_file)

    # Compare remaining variables
    return header_fail or _variable_check_exact(ds_base, ds_new)

##################

def netcdf_comparison_loose(baseline, new_file, rtol=1e-12, atol=1e-16, thres=1e-16):
    """
        Compare baseline to new_file using xarray
        Will pass if two files are within specified tolerance
        (run with --strict=loose)
    """
    header_fail, ds_base, ds_new = _init(baseline, new_file)

    # Compare remaining variables
    return header_fail or _variable_check_loose(ds_base, ds_new, rtol, atol, thres)

##################

def _header_test(ds_base, ds_new):
    """
        Are the file headers the same? Check for:
            1. Variables in one file but not the other
            2. Variables that are different types
            3. Variables that are different dimensions
    """
    logger = logging.getLogger(__name__)
    fail = False
    failed_vars = []

    # 1. Any variables in one file but not the other?
    base_vars = ds_base.variables
    new_vars = ds_new.variables
    common_vars = set(base_vars) & set(new_vars)
    base_vars = list(set(base_vars) - common_vars)
    new_vars = list(set(new_vars) - common_vars)
    if base_vars:
        fail = True
        ds_base = ds_base.drop(base_vars)
        logger.info("The following variables are in the baseline file but not the new file:")
        base_vars.sort()
        for var in base_vars:
            logger.info("* %s", var)
        logger.info("")
    if new_vars:
        fail = True
        ds_new = ds_new.drop(new_vars)
        logger.info("The following variables are in the new file but not the baseline file:")
        new_vars.sort()
        for var in new_vars:
            logger.info("* %s", var)
        logger.info("")

    # 2. Can variables be compared?
    common_vars = list(common_vars)
    common_vars.sort()
    for var in common_vars:
        err_messages = []
        # (a) Are they the same type?
        if ds_base[var].dtype != ds_new[var].dtype:
            err_messages.append(
                "Variable is {} in baseline and {} in new file".format(
                    ds_base[var].dtype, ds_new[var].dtype
                )
            )
        # (b) Do the dimension names match?
        common_dims = set(ds_base[var].dims) & set(ds_new[var].dims)
        if list(set(ds_base[var].dims) - common_dims) + list(set(ds_new[var].dims) - common_dims):
            err_messages.append(
                "Baseline dimensions are {} and new file dimensions are {}".format(
                    ds_base[var].dims, ds_new[var].dims
                )
            )
        # (c) Do the dimension sizes match?
        if ds_base[var].data.shape != ds_new[var].data.shape:
            err_messages.append(
                "Baseline dimensions are {} and new file dimensions are {}".format(
                    ds_base[var].data.shape, ds_new[var].data.shape
                )
            )

        # Report errors
        if _report_errs(var, err_messages):
            fail = True
            failed_vars.append(var)

    ds_base = ds_base.drop(failed_vars)
    ds_new = ds_new.drop(failed_vars)
    return fail, ds_base, ds_new # return re-assigned ds_base and ds_new

##################

def _variable_check_exact(ds_base, ds_new):
    """
        Assumes both datasets contain the same variables with the same dimensions
        Checks if two datasets are identical:
        1. Are NaNs in the same place?
        2. Are non-NaN values identical?
    """

    import numpy as np

    var_check_fail = False
    common_vars = list(set(ds_base.variables) & set(ds_new.variables))
    common_vars.sort()

    for var in common_vars:
        err_messages = []
        # (1) Are NaNs in the same place?
        mask_base = ~np.isnan(ds_base[var].data)
        mask_new = ~np.isnan(ds_new[var].data)
        if np.any(mask_base ^ mask_new):
            err_messages.append('NaNs are not in same place')

        # (2) Compare non-NaN values
        base_data = ds_base[var].data[mask_base]
        new_data = ds_new[var].data[mask_base]
        if np.any(base_data != new_data):
            rel_err = np.where(base_data != 0, np.abs(new_data - base_data), 0) / \
                      np.where(base_data != 0, np.abs(base_data), 1)
            abs_err = np.abs(new_data - base_data)
            err_messages.append("Values are not the same everywhere\n{}".format(
                "    Max relative error: {}\n    Max absolute error: {}".format(np.max(rel_err),
                                                                                np.max(abs_err))
            ))

        var_check_fail = _report_errs(var, err_messages) or var_check_fail

    return var_check_fail

##################

def _variable_check_loose(ds_base, ds_new, rtol, atol, thres):
    """
        Assumes both datasets contain the same variables with the same dimensions
        Checks:
        1. If baseline is 0 at a given point, then ds_new must be 0 as well
        2. Absolute vs relative error:
           i. If 0 < |baseline value| <= thres then want absolute difference < atol
           ii. If |baseline value| > thres, then want relative difference < rtol
    """
    import numpy as np

    var_check_fail = False
    common_vars = list(set(ds_base.variables) & set(ds_new.variables))
    common_vars.sort()

    for var in common_vars:
        err_messages = []

        # (1) Are NaNs in the same place?
        if np.any(np.isnan(ds_base[var].data) ^ np.isnan(ds_new[var].data)):
            err_messages.append('NaNs are not in same place')
        else:
            mask = ~np.isnan(ds_base[var].data)
            # (2) compare everywhere that baseline is 0 (well, <= thres)
            if np.any(np.where(np.abs(ds_base[var].data[mask]) <= thres,
                               np.abs(ds_new[var].data[mask]) > thres, False)):
                err_messages.append('Baseline is 0 at some indices where new data is non-zero')

            # (3) Compare everywhere that |baseline| is > thres
            base_data = np.where(np.abs(ds_base[var].data[mask]) > thres,
                                 ds_base[var].data[mask],
                                 0)
            new_data = np.where(np.abs(ds_base[var].data[mask]) > thres,
                                ds_new[var].data[mask],
                                0)
            rel_err = np.where(base_data != 0, np.abs(new_data - base_data), 0) / \
                      np.where(base_data != 0, np.abs(base_data), 1)
            if np.any(rel_err > rtol):
                err_messages.append("Max relative error ({}) exceeds {}".format(np.max(rel_err),
                                                                                rtol))

            # (4) Compare everywhere that 0 < |baseline| <= thres
            base_data = np.where((ds_base[var].data[mask] != 0) &
                                 (np.abs(ds_base[var].data[mask]) <= thres),
                                 ds_base[var].data[mask], 0)
            new_data = np.where((ds_base[var].data[mask] != 0) &
                                (np.abs(ds_base[var].data[mask]) <= thres),
                                ds_new[var].data[mask], 0)
            abs_err = np.abs(new_data - base_data)
            if np.any(abs_err > atol):
                err_messages.append("Max absolute error ({}) exceeds {}".format(np.max(abs_err),
                                                                                atol))

        var_check_fail = _report_errs(var, err_messages) or var_check_fail

    return var_check_fail

##################

def _report_errs(var, err_messages):
    """
        err_messages is a list of all accumulated errors
    """
    logger = logging.getLogger(__name__)
    if err_messages:
        logger.info("Variable: %s", var)
        for err in err_messages:
            logger.info("... %s", err)
        logger.info("")
        return True
    return False

def _parse_args():
    """ Parse command line arguments
    """

    import argparse

    parser = argparse.ArgumentParser(description="Compare two netCDF files using xarray and numpy",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    # Baseline for comparison
    parser.add_argument('-b', '--baseline', action='store', dest='baseline', required=True,
                        help='Baseline for comparison')

    # File to compare to baseline
    parser.add_argument('-n', '--new-file', action='store', dest='new_file', required=True,
                        help="File to compare to baseline")

    # Tolerances
    parser.add_argument('--strict', choices=['exact', 'loose'], required=True,
                        help="Should files be bit-for-bit [exact] or within some tolerance [loose]")

    parser.add_argument('-r', '--rtol', action='store', dest='rtol', default=1e-12, type=float,
                        help="Maximum allowable relative tolerance (only if strict=loose)")

    parser.add_argument('-a', '--atol', action='store', dest='atol', default=1e-16, type=float,
                        help="Maximum allowable absolute tolerance (only if strict=loose)")

    parser.add_argument('-t', '--thres', action='store', dest='thres', default=1e-16, type=float,
                        help="Threshold to switch from abs tolerance to rel (only if strict=loose)")

    return parser.parse_args()

if __name__ == "__main__":
    import os

    # Set up logging
#    logging.basicConfig(format='%(levelname)s (%(funcName)s): %(message)s', level=logging.INFO)
    logging.basicConfig(format='%(message)s', level=logging.INFO)
    LOGGER = logging.getLogger(__name__)

    args = _parse_args() # pylint: disable=invalid-name
    if args.strict == 'loose':
        if netcdf_comparison_loose(args.baseline, args.new_file, args.rtol, args.atol, args.thres):
            LOGGER.error("Differences found between files!")
            os.sys.exit(1)
        LOGGER.info("PASS: All variables match and are within specified tolerance.")
    if args.strict == 'exact':
        if netcdf_comparison_exact(args.baseline, args.new_file):
            LOGGER.error("Differences found between files!")
            os.sys.exit(1)
        LOGGER.info("PASS: All variables match and have exactly the same values.")
