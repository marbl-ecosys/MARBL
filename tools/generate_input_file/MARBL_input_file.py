#!/usr/bin/env python

#######################################

def write_input_file(yaml_file, input_file, lib_dir, grid, output_file):
    """ Produce a valid MARBL input file from a YAML parameter file
    """

    from sys import path
    path.insert(0, lib_dir)
    from MARBL_parameter_values import MARBL_parameters_class
    DefaultParms = MARBL_parameters_class(yaml_file, grid, input_file)

    fout = open(output_file,"w")
    # Sort variables by subcategory
    for subcat_name in DefaultParms.get_subcategory_names():
        fout.write("! %s\n" % subcat_name.split('. ')[1])
        for varname in DefaultParms.get_parm_dict_variable_names(subcat_name):
            fout.write("%s = %s\n" % (varname, DefaultParms.parm_dict[varname]))
        if subcat_name != DefaultParms.get_subcategory_names()[-1]:
            fout.write("\n")
    fout.close()

#######################################

def _parse_args():
    """ Parse command line arguments
    """

    import argparse

    parser = argparse.ArgumentParser(description="Print default MARBL parameter values from YAML file")

    # Command line argument to point to YAML file (default is parameters.yaml)
    parser.add_argument('-y', '--yaml_file', action='store', dest='yaml_file', default='default_values.yaml',
                      help='Location of YAML-formatted MARBL configuration file')

    # Command line argument to specify resolution (default is CESM_x1)
    parser.add_argument('-g', '--grid', action='store', dest='grid', default='CESM_x1',
                      help='Some default values are grid-dependent')

    # Command line argument to specify an input file which would override the YAML
    parser.add_argument('-i', '--input_file', action='store', dest='input_file', default=None,
                      help='A file that overrides values in YAML')

    # Command line argument to specify an input file which would override the YAML
    parser.add_argument('-o', '--output_file', action='store', dest='output_file', default='marbl.input',
                      help='Name of file to be written')

    # Path to directory containing MARBL_defaults.py
    parser.add_argument('-l', '--lib_dir', action='store', dest='lib_dir', default='./',
                      help='Directory that contains MARBL_parameter_values.py')

    return parser.parse_args()

#######################################

if __name__ == "__main__":
    # Parse command-line arguments
    args = _parse_args()

    # Set up logging #
    import logging
    logging.basicConfig(format='%(levelname)s (%(funcName)s): %(message)s', level=logging.DEBUG)

    # Write the input file
    write_input_file(args.yaml_file, args.input_file, args.lib_dir, args.grid, args.output_file)
