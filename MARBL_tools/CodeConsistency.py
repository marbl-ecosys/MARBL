#!/usr/bin/env python

"""
This script flags lines that do not conform to MARBL's coding practices
"""

import os, sys, logging
uniblock = u"\u2588"

##############

def check_for_hardtabs(file_and_line_number, line, log):
    """
    Log any lines containing hard tabs
    """
    if "\t" in line:
        log.append("%s: %s" % (file_and_line_number, line.replace("\t", 2*uniblock)))

##############

def check_for_trailing_whitespace(file_and_line_number, line, log):
    """
    Log any lines containing trailing whitespace
    """
    full_line_len = len(line)
    no_trailing_space_len = len(line.rstrip(" "))
    if no_trailing_space_len < full_line_len:
        log.append("%s: %s" % (file_and_line_number, line.rstrip(" ")+(full_line_len-no_trailing_space_len)*uniblock))

##############

def check_line_length(file_and_line_number, line, log, comment_char="!", max_len=132):
    """
    Log any lines exceeding max_len
    Currently ignores comment characters / anything following
    """
    line_len = len(line.split(comment_char)[0].rstrip(" "))
    if line_len > max_len:
        log.append("%s: %s" % (file_and_line_number, line[:max_len]+(line_len-max_len)*uniblock))

##############

def check_case_sensitive_module_statements(file_and_line_number, line, log, comment_char="!", max_len=132):
    """
    The following module statements should be all lowercase:
    * implicit none
    * public
    * private
    * save
    Note that at some point we may want to remove "save" altogether, since it is implicit in a module
    """
    statements = ["implicit none", "public", "private", "save"]
    # ignore comments, and strip all white space
    line_loc = line.split(comment_char)[0].strip(" ")
    if line_loc.lower() in statements:
        if line_loc not in statements:
            log.append("%s: %s" % (file_and_line_number, line))

##############

def check_for_spaces(file_and_line_number, line, log, comment_char="!", max_len=132):
    """
    The following statements should all include spaces:
    * else if
    * end if
    * else where
    * end where
    * end do
    """
    statements = ["elseif", "endif", "elsewhere", "endwhere", "enddo"]
    # ignore comments, and strip all white space
    line_loc = line.split(comment_char)[0].strip(" ")
    for bad_statement in statements:
        if line_loc.lower().startswith(bad_statement):
            log.append("%s: %s" % (file_and_line_number, line_loc))
            break

##############

def check_for_double_quotes(file_and_line_number, line, log):
    """
    All Fortran strings should appear as 'string', not "string"
    """
    if '"' in line:
        log.append("%s: %s" % (file_and_line_number, line))

##############

def check_logical_statements(file_and_line_number, line, log):
    """
    Use symbols, not words, for logical operators:
    * >=, not .ge.
    * >, not .gt.
    * <=, not .le.
    * <, not .lt.
    * ==, not .eq.
    * /=, not .ne.
    """
    operators = ['.ge.', '.gt.', '.le.', '.lt.', '.eq.', '.ne.']
    for op in operators:
        if op in line:
            log.append("%s: %s" % (file_and_line_number, line))
            break

##############

def process_err_log(test, log):
    logger = logging.getLogger(__name__)
    err_cnt = len(log)
    logger.info("* %s: %d error(s) found" % (test, err_cnt))
    while len(log) > 0:
        logger.info("  %s" % log.popleft())
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

    # Store messages to write to stdout appending lines to a list
    # -- This lets us read files once, rather than once per test, but still group output in readable fashion
    from collections import deque # Faster pop / append than standard lists
    hard_tab_log = deque([])
    trailing_space_log = deque([])
    line_len_log = deque([])
    case_sensitive_log = deque([])
    spaces_log = deque([])
    quotes_log = deque([])
    logical_log = deque([])

    # Fortran error checks
    f90_err_cnt = 0
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
                check_line_length(file_and_line_number, line_loc, line_len_log)
                check_case_sensitive_module_statements(file_and_line_number, line_loc, case_sensitive_log)
                check_for_spaces(file_and_line_number, line_loc, spaces_log)
                check_for_double_quotes(file_and_line_number, line_loc, quotes_log)
                check_logical_statements(file_and_line_number, line_loc, logical_log)

    # Process each test log
    f90_err_cnt += process_err_log("Check for hard tabs", hard_tab_log)
    f90_err_cnt += process_err_log("Check for trailing white space", trailing_space_log)
    f90_err_cnt += process_err_log("Check length of lines", line_len_log)
    f90_err_cnt += process_err_log("Check for case sensitive statements", case_sensitive_log)
    #f90_err_cnt += process_err_log("Check for spaces in statements", spaces_log)
    #f90_err_cnt += process_err_log("Check for double quotes in statements", quotes_log)
    #f90_err_cnt += process_err_log("Check for unwanted logical operators", logical_log)

    # Python error checks
    py_err_cnt = 0
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
