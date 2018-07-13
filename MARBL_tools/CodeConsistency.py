#!/usr/bin/env python

"""
This script flags lines that do not conform to MARBL's coding practices
"""

import os, sys, logging

##############

def check_for_hardtabs(file_and_line_number, line, log):
    """
    Log any lines containing hard tabs
    """
    if "\t" in line:
        log.append("%s: %s" % (file_and_line_number, line.replace("\t", 2*u"\u2588")))

##############

def check_for_trailing_whitespace(file_and_line_number, line, log):
    """
    Log any lines containing trailing whitespace
    """
    full_line_len = len(line)
    no_trailing_space_len = len(line.rstrip(" "))
    if no_trailing_space_len < full_line_len:
        log.append("%s: %s" % (file_and_line_number, line.rstrip(" ")+(full_line_len-no_trailing_space_len)*u"\u2588"))

##############

def process_err_log(test, log):
    logger = logging.getLogger(__name__)
    err_cnt = len(log)
    logger.info("* %s: %d error(s) found" % (test, err_cnt))
    for line in log:
        logger.info("  %s" % line)
    #logger.info("----")
    return err_cnt

##############

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

    fortran_files = [] # list containing path to all Fortran files
    python_files = []  # list containing path to all python files
    for root, dirs, files in os.walk(marbl_root):
        for file in files:
            if file.endswith(".F90"):
                fortran_files.append(os.path.join(root, file))
            elif file.endswith(".py"):
                python_files.append(os.path.join(root, file))

    # Use logging to write messages to stdout
    logging.basicConfig(format='%(message)s', level=logging.DEBUG)
    logger = logging.getLogger(__name__)

    # Fortran error checks
    f90_err_cnt = 0
    # Store messages to write to stdout appending lines to a list
    # -- This lets us read files once, rather than once per test, but still group output in readable fashion
    hard_tab_log = []
    trailing_space_log = []
    logger.info("Check Fortran files for coding standard violations:")
    for file in fortran_files:
        with open(file, "r") as fortran_file:
            line_cnt = 0
            for line in fortran_file.readlines():
                line_loc = line.rstrip("\n")
                line_cnt = line_cnt + 1
                file_and_line_number = "%s:%d" % (file, line_cnt)
                check_for_hardtabs(file_and_line_number, line_loc, hard_tab_log)
                check_for_trailing_whitespace(file_and_line_number, line_loc, trailing_space_log)
    # Process each test log
    f90_err_cnt += process_err_log("Check for hard tabs", hard_tab_log)
    f90_err_cnt += process_err_log("Check for trailing white space", trailing_space_log)

    # Python error checks
    py_err_cnt = 0
    # Store messages to write to stdout appending lines to a list
    # -- This lets us read files once, rather than once per test, but still group output in readable fashion
    hard_tab_log = []
    trailing_space_log = []
    logger.info("\nCheck python files for coding standard violations:")
    for file in python_files:
        with open(file, "r") as python_file:
            line_cnt = 0
            for line in python_file.readlines():
                line_loc = line.rstrip("\n")
                line_cnt = line_cnt + 1
                file_and_line_number = "%s:%d" % (file, line_cnt)
                check_for_hardtabs(file_and_line_number, line_loc, hard_tab_log)
                check_for_trailing_whitespace(file_and_line_number, line_loc, trailing_space_log)
    # Process each test log
    py_err_cnt += process_err_log("Check for hard tabs", hard_tab_log)
    py_err_cnt += process_err_log("Check for trailing white space", trailing_space_log)

    logger.info("\nFortran errors found: %d" % f90_err_cnt)
    logger.info("Python errors found: %d" % py_err_cnt)
    sys.exit(f90_err_cnt+py_err_cnt)
