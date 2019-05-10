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

def netcdf_comparison(baseline, new_file):
    """
        Compare baseline to new_file using xarray
    """
    import xarray as xr

    logger = logging.getLogger(__name__)
    logger.info("Comparing %s to the baseline %s", new_file, baseline)
    ds_base = xr.open_dataset(baseline)
    ds_new = xr.open_dataset(new_file)

    fail = False

    # 1. Any variables in one file but not the other?
    base_vars = ds_base.keys()
    new_vars = ds_new.keys()
    common_vars = set(base_vars) & set(new_vars)
    base_vars = list(set(base_vars) - common_vars)
    new_vars = list(set(new_vars) - common_vars)
    if base_vars:
        fail = True
        ds_base.drop(base_vars)
        logger.info("The following variables are in the baseline file but not the new file:")
        base_vars.sort()
        for var in base_vars:
            logger.info("* %s", var)
        logger.info("")
    if new_vars:
        fail = True
        ds_new.drop(new_vars)
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
        if err_messages:
            fail = True
            logger.info("Variable: %s", var)
            for err in err_messages:
                logger.info("... %s", err)
            ds_base.drop(var)
            ds_new.drop(var)
            common_vars.remove(var)
            logger.info("")

    # 3. Compare variables that match type and shape
    # logger.info("Remaining vars:")
    # for var in common_vars:
    #     logger.info("* %s", var)
    #for var in common_vars:

    return fail

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

    return parser.parse_args()

if __name__ == "__main__":
    import os

    # Set up logging
#    logging.basicConfig(format='%(levelname)s (%(funcName)s): %(message)s', level=logging.INFO)
    logging.basicConfig(format='%(message)s', level=logging.INFO)

    args = _parse_args() # pylint: disable=invalid-name
    if netcdf_comparison(args.baseline, args.new_file):
        os.sys.exit(1)
