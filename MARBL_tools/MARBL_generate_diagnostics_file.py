#!/usr/bin/env python

"""
Generate a MARBL diagnostics file containing frequencies for all MARBL diagnostics
available from a particular MARBL_settings_class object (as defined in a JSON file).

This file can be run as a command line script or imported as part of the MARBL_tools module.
To use from a module, all arguments are required in the call

generate_diagnostics_file(MARBL_settings, diagnostics_file_out)

From the command line
---------------------

usage: MARBL_generate_diagnostics_file.py [-h] [-f DEFAULT_SETTINGS_FILE]
                                          [-j DEFAULT_DIAGNOSTICS_FILE]
                                          [-s {GCM,settings_file}] [-g GRID]
                                          [-i SETTINGS_FILE_IN]
                                          [-o DIAGNOSTICS_FILE_OUT]

Generate a MARBL settings file from a JSON file

optional arguments:
  -h, --help            show this help message and exit
  -f DEFAULT_SETTINGS_FILE, --default_settings_file DEFAULT_SETTINGS_FILE
                        Location of JSON-formatted MARBL settings
                        configuration file (default: $MARBLROOT/
                        defaults/json/settings_latest.json)
  -j DEFAULT_DIAGNOSTICS_FILE, --default_diagnostics_file DEFAULT_DIAGNOSTICS_FILE
                        Location of JSON-formatted MARBL diagnostics
                        configuration file (default: $MARBLROOT/
                        defaults/json/diagnostics_latest.json)
  -s {GCM,settings_file}, --saved_state_vars_source {GCM,settings_file}
                        Source of initial value for saved state vars that can
                        come from GCM or settings file (default:
                        settings_file)
  -g GRID, --grid GRID  Some default values are grid-dependent (default: None)
  -i SETTINGS_FILE_IN, --settings_file_in SETTINGS_FILE_IN
                        A file that overrides values in settings JSON file
                        (default: None)
  -o DIAGNOSTICS_FILE_OUT, --diagnostics_file_out DIAGNOSTICS_FILE_OUT
                        Name of file to be written (default: marbl.diags)

"""

#######################################

def generate_diagnostics_file(MARBL_diagnostics, diagnostics_file_out, append=False):
    """ Produce a list of MARBL diagnostic frequencies and operators from a JSON parameter file
    """

    import logging
    logger = logging.getLogger(__name__)

    if not append:
        try:
            fout = open(diagnostics_file_out,"w")
        except:
            logger.error("Unable to open %s for writing" % diagnostics_file_out)

        # Only write header to file if not appending
        fout.write("# This file contains a list of all diagnostics MARBL can compute for a given configuration,\n")
        fout.write("# as well as the recommended frequency and operator for outputting each diagnostic.\n")
        fout.write("# The format of this file is:\n")
        fout.write("#\n")
        fout.write("# DIAGNOSTIC_NAME : frequency_operator\n")
        fout.write("#\n")
        fout.write("# And fields that should be output at multiple different frequencies will be comma-separated:\n")
        fout.write("#\n")
        fout.write("# DIAGNOSTIC_NAME : frequency1_operator1, frequency2_operator2, ..., frequencyN_operatorN\n")
        fout.write("#\n")
        fout.write("# Frequencies are never, low, medium, and high.\n")
        fout.write("# Operators are instantaneous, average, minimum, and maximum.\n")

    else:
        try:
            fout = open(diagnostics_file_out,"a")
        except:
            logger.error("Unable to append to %s" % diagnostics_file_out)

    # Keys in diagnostics_dict are diagnostic short name, and diagnostic_dict[diag_name]
    # is also a dictionary containing frequency and operator information. Note that
    # string values of frequency and operator are converted to lists of len 1 when the
    # JSON file that generates this list is processed
    for diag_name in MARBL_diagnostics.diagnostics_dict.keys():
        frequencies = MARBL_diagnostics.diagnostics_dict[diag_name]['frequency']
        operators = MARBL_diagnostics.diagnostics_dict[diag_name]['operator']
        freq_op = []
        for freq, op in zip(frequencies, operators):
            freq_op.append(freq + '_' + op)
        fout.write("%s : %s\n" % (diag_name, ", ".join(freq_op)))
    fout.close()

#######################################

def _parse_args(marbl_root):
    """ Parse command line arguments
    """

    import argparse

    parser = argparse.ArgumentParser(description="Generate a MARBL settings file from a JSON file",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    # Command line argument to point to JSON settings file (default is $MARBLROOT/defaults/json/settings_latest.json)
    parser.add_argument('-f', '--default_settings_file', action='store', dest='default_settings_file',
                        default=os.path.join(marbl_root, 'defaults', 'json', 'settings_latest.json'),
                        help='Location of JSON-formatted MARBL settings configuration file')

    # Command line argument to point to JSON diagnostics file (default is $MARBLROOT/defaults/json/diagnostics_latest.json)
    parser.add_argument('-j', '--default_diagnostics_file', action='store', dest='default_diagnostics_file',
                        default=os.path.join(marbl_root, 'defaults', 'json', 'diagnostics_latest.json'),
                        help='Location of JSON-formatted MARBL diagnostics configuration file')

    # Is the GCM providing initial bury coefficients via saved state?
    parser.add_argument('-s', '--saved_state_vars_source', action='store', dest='saved_state_vars_source',
                        default='settings_file', choices = set(('settings_file', 'GCM')),
                        help="Source of initial value for saved state vars that can come from GCM or settings file")

    # Command line argument to specify resolution (default is None)
    parser.add_argument('-g', '--grid', action='store', dest='grid',
                        help='Some default values are grid-dependent')

    # Command line argument to specify an input settings file which would override the JSON
    parser.add_argument('-i', '--settings_file_in', action='store', dest='settings_file_in', default=None,
                        help='A file that overrides values in settings JSON file')

    # Command line argument to where to write the input file being generated
    parser.add_argument('-o', '--diagnostics_file_out', action='store', dest='diagnostics_file_out', default='marbl.diags',
                        help='Name of file to be written')

    # Append to existing diagnostics file?
    parser.add_argument('-a', '--append', action='store_true', dest='append',
                        help='Append to existing diagnostics file')
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
    from MARBL_tools import MARBL_diagnostics_class
    DefaultSettings = MARBL_settings_class(args.default_settings_file, args.saved_state_vars_source, args.grid, args.settings_file_in)
    MARBL_diagnostics = MARBL_diagnostics_class(args.default_diagnostics_file, DefaultSettings)

    # Write the input file
    generate_diagnostics_file(MARBL_diagnostics, args.diagnostics_file_out, args.append)
