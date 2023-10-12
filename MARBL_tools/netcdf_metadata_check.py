#!/usr/bin/env python

"""
Compare the units, longname and vertical grid for each variable in a MARBL history file to what is
defined in the diagnostics YAML file.

usage: netcdf_metadata_check.py [-h] [-n NETCDF_FILE] [-j DEFAULT_DIAGNOSTICS_FILE]
                                [-f DEFAULT_SETTINGS_FILE] [-s SETTINGS_FILE_IN] [-u {cgs,mks}]

Compare metadata in netCDF file to a JSON file

optional arguments:
  -h, --help            show this help message and exit
  -n NETCDF_FILE, --netcdf-file NETCDF_FILE
                        netCDF file to read metadata from (default: /Users/mlevy/NO_BACKUP/codes/MAR
                        BL/tests/regression_tests/call_compute_subroutines/history_1inst.nc)
  -j DEFAULT_DIAGNOSTICS_FILE, --default_diagnostics_file DEFAULT_DIAGNOSTICS_FILE
                        Location of JSON-formatted MARBL diagnostics configuration file (default:
                        /Users/mlevy/NO_BACKUP/codes/MARBL/defaults/json/diagnostics_latest.json)
  -f DEFAULT_SETTINGS_FILE, --default_settings_file DEFAULT_SETTINGS_FILE
                        Location of JSON-formatted MARBL settings configuration file (default:
                        /Users/mlevy/NO_BACKUP/codes/MARBL/defaults/json/settings_latest.json)
  -s SETTINGS_FILE_IN, --settings_file_in SETTINGS_FILE_IN
                        A file that overrides values in settings JSON file (default: /Users/mlevy/NO
                        _BACKUP/codes/MARBL/tests/input_files/settings/marbl_with_o2_consumption_sca
                        lef.settings)
  -u {cgs,mks}, --unit_system {cgs,mks}
                        Unit system for parameter values (default: cgs)
"""

##################

def _parse_args():
    """ Parse command line arguments
    """

    import argparse

    parser = argparse.ArgumentParser(description="Compare metadata in netCDF file to a JSON file",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    # File to compare to baseline
    # (default is $MARBLROOT/tests/regression_tests/call_compute_subroutines/history_1inst.nc)
    default = os.path.join(marbl_root,
                           "tests",
                           "regression_tests",
                           "call_compute_subroutines",
                           "history_1inst.nc"
                          )
    parser.add_argument("-n", "--netcdf-file", action="store", dest="netcdf_file",
                        default=default, help="netCDF file to read metadata from")

    # Command line argument to point to JSON diagnostics file
    # (default is $MARBLROOT/defaults/json/diagnostics_latest.json)
    default=os.path.join(marbl_root, "defaults", "json", "diagnostics_latest.json")
    parser.add_argument("-j", "--default_diagnostics_file", action="store",
                        dest="default_diagnostics_file", default=default,
                        help="Location of JSON-formatted MARBL diagnostics configuration file")

    # Command line argument to point to JSON settings file
    # (default is $MARBLROOT/defaults/json/settings_latest.json)
    default=os.path.join(marbl_root, "defaults", "json", "settings_latest.json")
    parser.add_argument("-f", "--default_settings_file", action="store",
                        dest="default_settings_file", default=default,
                        help="Location of JSON-formatted MARBL settings configuration file")

    # Command line argument to specify a settings file which would override the JSON
    # (default matches that used my call_compute_subroutines test)
    default = os.path.join(marbl_root,
                           "tests",
                           "input_files",
                           "settings",
                           "marbl_with_o2_consumption_scalef.settings"
                          )
    parser.add_argument("-s", "--settings_file_in", action="store", dest="settings_file_in",
                        default=default, help="A file that overrides values in settings JSON file")

    # Command line argument to where to write the settings file being generated
    parser.add_argument("-u", "--unit_system", action="store", dest="unit_system", default="cgs",
                        choices=["cgs", "mks"], help="Unit system for parameter values")

    return parser.parse_args()

##################

if __name__ == "__main__":
    import logging
    import os
    import sys
    import xarray as xr

    # We need marbl_root in python path so we can import MARBL_tools from generate_settings_file()
    marbl_root = os.path.abspath(os.path.join(os.path.dirname(sys.argv[0]), ".."))
    sys.path.append(marbl_root)
    from MARBL_tools import MARBL_settings_class, MARBL_diagnostics_class, abort, LogFormatter

    # Set up logging
    logger = logging.getLogger("__name__")
    handler = logging.StreamHandler()
    handler.setFormatter(LogFormatter())
    logger.setLevel(logging.DEBUG)
    logger.addHandler(handler)

    # Parse command line arguments
    args = _parse_args()
    if args.settings_file_in == "None":
        args.settings_file_in = None

    # Read netcdf file and generate diagnostics object
    ds = xr.open_dataset(args.netcdf_file)
    DefaultSettings = MARBL_settings_class(args.default_settings_file,
                                           "settings_file",
                                           None,
                                           args.settings_file_in,
                                           args.unit_system
                                          )
    MARBL_diagnostics = MARBL_diagnostics_class(args.default_diagnostics_file,
                                                DefaultSettings,
                                                args.unit_system
                                               )
    json_in = MARBL_diagnostics.diagnostics_dict

    # construct list of diagnostics generated by the driver
    driver_vars = ["zt", "zw"]
    for tracer in DefaultSettings.tracers_dict:
        driver_vars.append(tracer)
        driver_vars.append(f"STF_{tracer}")
        driver_vars.append(f"J_{tracer}")

    diff_found = False
    for var in ds.variables:
        # skip diagnostics generated by the driver
        if var in driver_vars or var.startswith("output_for_GCM"):
            continue

        # Note difference if variable is in netcdf file but not JSON
        if var not in json_in:
            logger.info("Can not find %s in %s!", var, args.default_diagnostics_file)
            diff_found = True
            continue

        # Construct dictionaries to compare units, long_name, and vertical grid
        netcdf_dict = {}
        netcdf_dict["units"] = str(ds[var].attrs["units"])
        netcdf_dict["longname"] = ds[var].attrs["long_name"]
        if "num_levels" in ds[var].dims:
            netcdf_dict["vertical_grid"] = "layer_avg"
        else:
            netcdf_dict["vertical_grid"] = "none"
        json_dict = {key: str(json_in[var][key]) for key in ["units", "longname", "vertical_grid"]}

        # Construct dictionary comparing each of the elements of the above dicts
        differences = {key: netcdf_dict[key] != value for (key, value) in json_dict.items()}

        # Report any differences
        if any(differences.values()):
            diff_found = True
            logger.info("Differences in %s:", var)
            if differences["units"]:
                logger.info("* JSON units: %s", json_dict["units"])
                logger.info("* netcdf units: %s", netcdf_dict["units"])
            if differences["longname"]:
                logger.info("* JSON long name: %s", json_dict["longname"])
                logger.info("* netcdf long name: %s", netcdf_dict["longname"])
            if differences["vertical_grid"]:
                logger.info("* JSON vertical grid: %s", json_dict["vertical_grid"])
                logger.info("* netcdf dimensions: %s", ds[var].dims)

if diff_found:
    logger.error("Differences found between JSON and netCDF metadata!")
    abort(1)
else:
    logger.info("No differences found between JSON and netCDF metadata")
