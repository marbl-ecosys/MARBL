#!/usr/bin/env python

"""
Generate a MARBL settings file containing values for all MARBL settings, as defined
in a YAML file (and optional additional settings file).

This file can be run as a command line script or imported as part of the MARBL_tools module.
To use from a module, all arguments are required in the call

generate_settings_file(default_settings_file, settings_file_in, grid, settings_file_out)

To run from a command line, the following arguments are all optional otherwise default values are used:

  -f DEFAULT_SETTINGS_FILE, --default_settings_file DEFAULT_SETTINGS_FILE
                        Location of YAML-formatted MARBL configuration file
                        (default:
                        /home/mlevy/codes/marbl/src/default_settings.yaml)
  -j, --is_JSON         Is DEFAULT_SETTINGS_FILE in JSON format (False =>
                        YAML) (default: False)
  -s {GCM,settings_file}, --saved_state_vars_source {GCM,settings_file}
                        Source of initial value for saved state vars that can
                        come from GCM or settings file (default:
                        settings_file)
  -g GRID, --grid GRID  Some default values are grid-dependent (default: None)
  -i SETTINGS_FILE_IN, --settings_file_in SETTINGS_FILE_IN
                        A file that overrides values in YAML (default: None)
  -o SETTINGS_FILE_OUT, --settings_file_out SETTINGS_FILE_OUT
                        Name of file to be written (default: marbl.input)

"""

#######################################

def generate_settings_file(MARBL_settings, settings_file_out):
    """ Produce a valid MARBL input file from a YAML parameter file
    """

    fout = open(settings_file_out,"w")
    # Sort variables by subcategory
    for subcat_name in MARBL_settings.get_subcategory_names():
        fout.write("! %s\n" % subcat_name.split('. ')[1])
        for varname in MARBL_settings.get_settings_dict_variable_names(subcat_name):
            fout.write("%s = %s\n" % (varname, MARBL_settings.settings_dict[varname]))
        if subcat_name != MARBL_settings.get_subcategory_names()[-1]:
            fout.write("\n")
    fout.close()

#######################################

def _parse_args(marbl_root):
    """ Parse command line arguments
    """

    import argparse

    parser = argparse.ArgumentParser(description="Generate a MARBL settings file from a YAML file",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    # Command line argument to point to YAML file (default is $MARBLROOT/src/default_settings.yaml)
    parser.add_argument('-f', '--default_settings_file', action='store', dest='default_settings_file',
                        default=os.path.join(marbl_root, 'src', 'default_settings.yaml'),
                        help='Location of YAML-formatted MARBL configuration file')

    # Is the settings file actually a JSON file rather than YAML?
    parser.add_argument('-j', '--is_JSON', action='store_true', dest='is_JSON',
                        help="Is DEFAULT_SETTINGS_FILE in JSON format (False => YAML)")

    # Is the GCM providing initial bury coefficients via saved state?
    parser.add_argument('-s', '--saved_state_vars_source', action='store', dest='saved_state_vars_source',
                        default='settings_file', choices = set(('settings_file', 'GCM')),
                        help="Source of initial value for saved state vars that can come from GCM or settings file")

    # Command line argument to specify resolution (default is None)
    parser.add_argument('-g', '--grid', action='store', dest='grid',
                        help='Some default values are grid-dependent')

    # Command line argument to specify an input file which would override the YAML
    parser.add_argument('-i', '--settings_file_in', action='store', dest='settings_file_in', default=None,
                        help='A file that overrides values in YAML')

    # Command line argument to specify an input file which would override the YAML
    parser.add_argument('-o', '--settings_file_out', action='store', dest='settings_file_out', default='marbl.input',
                        help='Name of file to be written')

    return parser.parse_args()

#######################################

if __name__ == "__main__":
    # We need marbl_root in python path so we can import MARBL_tools from generate_settings_file()
    import sys, os
    marbl_root = os.path.abspath(os.path.join(os.path.dirname(sys.argv[0]), '..'))
    sys.path.append(marbl_root)

    # Parse command-line arguments (marbl_root is used to set default for YAML file location)
    args = _parse_args(marbl_root)

    # Set up logging
    import logging
    logging.basicConfig(format='%(levelname)s (%(funcName)s): %(message)s', level=logging.DEBUG)

    from MARBL_tools import MARBL_settings_class
    DefaultSettings = MARBL_settings_class(args.default_settings_file, args.is_JSON, args.saved_state_vars_source, args.grid, args.settings_file_in)


    # Write the input file
    generate_settings_file(DefaultSettings, args.settings_file_out)
