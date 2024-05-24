#!/usr/bin/env python

"""
Generate a MARBL settings file containing values for all MARBL settings, as defined
in a JSON file (and optional additional settings file).

This file can be run as a command line script or imported as part of the MARBL_tools module.
To use from a module, all arguments are required in the call

generate_settings_file(MARBL_settings, settings_file_out)

From the command line
---------------------

usage: MARBL_generate_settings_file.py [-h] [-f DEFAULT_SETTINGS_FILE]
                                       [-s {GCM,settings_file}] [-g GRID]
                                       [-i SETTINGS_FILE_IN]
                                       [-o SETTINGS_FILE_OUT]

Generate a MARBL settings file from a JSON file

optional arguments:
  -h, --help            show this help message and exit
  -f DEFAULT_SETTINGS_FILE, --default_settings_file DEFAULT_SETTINGS_FILE
                        Location of JSON-formatted MARBL configuration file
                        (default: $MARBLROOT/defaults/json/settings_latest.json)
  -s {GCM,settings_file}, --saved_state_vars_source {GCM,settings_file}
                        Source of initial value for saved state vars that can
                        come from GCM or settings file (default:
                        settings_file)
  -g GRID, --grid GRID  Some default values are grid-dependent (default: None)
  -i SETTINGS_FILE_IN, --settings_file_in SETTINGS_FILE_IN
                        A file that overrides values in JSON (default: None)
  -o SETTINGS_FILE_OUT, --settings_file_out SETTINGS_FILE_OUT
                        Name of file to be written (default: marbl.settings)
  -u {cgs,mks}, --unit_system {cgs,mks}
                        Unit system for parameter values (default: cgs)
"""

#######################################

def generate_settings_file(MARBL_settings, settings_file_out):
    """ Produce a valid MARBL settings file from a JSON parameter file
    """

    fout = open(settings_file_out,"w")
    # Sort variables by subcategory
    written_any = False
    for subcat_name in MARBL_settings.get_subcategory_names():
        header = "! %s\n" % subcat_name.split('. ')[1]
        var_values = []
        for varname in MARBL_settings.get_settings_dict_variable_names(subcat_name):
            var_values.append("%s = %s\n" % (varname, MARBL_settings.settings_dict[varname]['value']))
        if len(var_values) > 0:
            if written_any:
                fout.write("\n")
            written_any = True
            fout.write(header)
            for line in var_values:
                fout.write(line)
    fout.close()

#######################################

def _parse_args(marbl_root):
    """ Parse command line arguments
    """

    import argparse

    parser = argparse.ArgumentParser(description="Generate a MARBL settings file from a JSON file",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    # Command line argument to point to JSON file (default is $MARBLROOT/defaults/json/settings_latest.json)
    parser.add_argument('-f', '--default_settings_file', action='store', dest='default_settings_file',
                        default=os.path.join(marbl_root, 'defaults', 'json', 'settings_latest.json'),
                        help='Location of JSON-formatted MARBL configuration file')

    # Is the GCM providing initial bury coefficients via saved state?
    parser.add_argument('-s', '--saved_state_vars_source', action='store', dest='saved_state_vars_source',
                        default='settings_file', choices = set(('settings_file', 'GCM')),
                        help="Source of initial value for saved state vars that can come from GCM or settings file")

    # Command line flag to turn off the base biotic tracers
    parser.add_argument('--disable-base-bio', action='store_false', dest='base_bio_on',
                        help='Turn off base biotic tracer module (default is on)')

    # Command line flag to turn on the abiotic DIC tracers
    parser.add_argument('--enable-abio-dic', action='store_true', dest='abio_dic_on',
                        help='Turn on abiotic DIC tracer module (default is off)')

    # Command line argument to specify resolution (default is None)
    parser.add_argument('-g', '--grid', action='store', dest='grid',
                        help='Some default values are grid-dependent')

    # Command line argument to specify a settings file which would override the JSON
    parser.add_argument('-i', '--settings_file_in', action='store', dest='settings_file_in', default=None,
                        help='A file that overrides values in JSON')

    # Command line argument to where to write the settings file being generated
    parser.add_argument('-o', '--settings_file_out', action='store', dest='settings_file_out', default='marbl.settings',
                        help='Name of file to be written')

    # Command line argument to where to write the settings file being generated
    parser.add_argument('-u', '--unit_system', action='store', dest='unit_system', default='cgs',
                        choices=['cgs', 'mks'], help='Unit system for parameter values')

    return parser.parse_args()

#######################################

if __name__ == "__main__":
    # We need marbl_root in python path so we can import MARBL_tools from generate_settings_file()
    import sys, os
    marbl_root = os.path.abspath(os.path.join(os.path.dirname(sys.argv[0]), '..'))
    sys.path.append(marbl_root)

    # Parse command-line arguments (marbl_root is used to set default for JSON file location)
    args = _parse_args(marbl_root)

    # Set up logging
    import logging
    logging.basicConfig(format='%(levelname)s (%(funcName)s): %(message)s', level=logging.DEBUG)

    from MARBL_tools import MARBL_settings_class
    DefaultSettings = MARBL_settings_class(args.default_settings_file,
                                           args.saved_state_vars_source,
                                           args.base_bio_on,
                                           args.abio_dic_on,
                                           args.grid,
                                           args.settings_file_in,
                                           args.unit_system)

    # Write the settings file
    generate_settings_file(DefaultSettings, args.settings_file_out)
