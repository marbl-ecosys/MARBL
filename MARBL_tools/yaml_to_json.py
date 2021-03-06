#!/usr/bin/env python

"""
YAML support is not part of the Python standard library, so it is possible that
users may find themselves running MARBL scripts in an environment that does not
have PyYAML. To support those users, MARBL relies soly on JSON versions of
settings_*.yaml and diagnostics_*.yaml which should be generated by this script
if either YAML file is changed.

Developers MUST update the YAML and then regenerate the JSON file(s); commits that
change the JSON file(s) alone will not be accepted.

usage: yaml_to_json.py [-h] [-y YAML_FILES [YAML_FILES ...]] [-o OUTPUT_DIR]

Convert all of MARBL's YAML files to JSON

optional arguments:
  -h, --help            show this help message and exit
  -y YAML_FILES [YAML_FILES ...], --yaml_files YAML_FILES [YAML_FILES ...]
                        List of files to convert (default:
                        ['$MARBLROOT/defaults/settings_cesm2.0.yaml',
                        '$MARBLROOT/defaults/diagnostics_latest.yaml',
                        '$MARBLROOT/defaults/settings_latest.yaml'])
  -o OUTPUT_DIR, --output_dir OUTPUT_DIR
                        Directory where JSON file(s) will be created (default:
                        $MARBLROOT/defaults/json)

"""

import sys, os, logging

#######################################

def _parse_args(marbl_root):
    """ Parse command line arguments
    """

    import argparse

    parser = argparse.ArgumentParser(description="Convert all of MARBL's YAML files to JSON",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('-y', '--yaml_files', nargs='+', action='store', dest='yaml_files',
                        default=[os.path.join(yaml_dir,file) for file in os.listdir(yaml_dir) if file.endswith("yaml")],
                        help="List of files to convert")

    parser.add_argument('-o', '--output_dir', action='store', dest='output_dir',
                        default=os.path.join(marbl_root, 'defaults', 'json'),
                        help="Directory where JSON file(s) will be created")

    return parser.parse_args()

#######################################

def yaml_to_json(yaml_files, output_dir):

    import json
    # This tool requires PyYAML, error if it is not available
    try:
        import yaml
    except:
        logger.error("Can not find PyYAML library")
        sys.exit(1)

    # Routines to verify that YAML files meet MARBL formatting requirements
    from MARBL_tools import settings_dictionary_is_consistent, diagnostics_dictionary_is_consistent

    # Read YAML files
    for rel_file in yaml_files:
        filename = os.path.abspath(rel_file)
        yaml_filename = filename.split(os.path.sep)[-1]
        if not os.path.isfile(filename):
            logger.error("File not found: "+filename)
            sys.exit(1)
        with open(filename) as file_in:
            yaml_in = yaml.safe_load(file_in)

        # YAML consistency checks (MARBL expects these files to be formatted
        # in a specific way)
        if yaml_filename.startswith("settings"):
            check_func=settings_dictionary_is_consistent
        elif yaml_filename.startswith("diagnostics"):
            check_func=diagnostics_dictionary_is_consistent
        else:
            logger.error("Can not find consistency check for %s" % filename)
            sys.exit(1)

        logger.info("Running consistency check for %s" % filename)
        if not check_func(yaml_in):
            logger.error("Formatting error in %s reported from %s" % (filename, check_func.__name__))
            sys.exit(1)

        # Write JSON file
        if yaml_filename[-5:].lower() == ".yaml":
            json_filename = yaml_filename[:-5] + ".json"
        elif yaml_filename[-4:].lower() == ".yml":
            json_filename = yaml_filename[:-4] + ".json"
        else:
            json_filename = yaml_filename + ".json"
        json_fullname = os.path.abspath(os.path.join(output_dir, json_filename))
        with open(json_fullname, "w") as file_out:
            logger.info('Writing %s' % json_fullname)
            json.dump(yaml_in, file_out, separators=(',', ': '), sort_keys=True, indent=3)

#######################################

if __name__ == "__main__":

    # marbl_root is the top-level MARBL directory, which is a level above the directory containing this script
    # * Do some string manipulation to make marbl_root as human-readable as possible because it appears in the
    #   output if you run this script with the "-h" option
    script_dir = os.path.dirname(sys.argv[0])
    if script_dir == '.':
        marbl_root = '..'
    elif script_dir.endswith('MARBL_tools'):
        marbl_root = script_dir[:-12]
    else:
        marbl_root = os.path.join(script_dir, '..')
    yaml_dir = os.path.join(marbl_root, "defaults")
    sys.path.append(marbl_root)

    # Parse command-line arguments (marbl_root is used to set default for YAML file location)
    args = _parse_args(marbl_root)

    # Set up logging
    logging.basicConfig(format='%(levelname)s: %(message)s', level=logging.DEBUG)
    logger = logging.getLogger("__name__")

    yaml_to_json(args.yaml_files, args.output_dir)
