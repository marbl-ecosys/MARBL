#!/usr/bin/env python

"""
This script flags lines that do not conform to MARBL's coding practices
"""

import os, sys, logging
from collections import deque # Faster pop / append than standard lists
from collections import OrderedDict
uniblock = u"\u2588"

##############

class consistency_test_class(object):
    def __init__(self, quiet=False):
        self.logs = OrderedDict()
        self.quiet = quiet

    ##############

    def process(self):
        """
        Check results from all tests that have been run:
        1. For each test:
            i.  log (as info) test description and number of lines of code that fail the test
            ii. log (as info) all lines of code that do not conform to standards
        2. return total number of errors across all tests
        """
        logger = logging.getLogger(__name__)
        tot_err_cnt = 0
        while len(self.logs) > 0:
            desc, log = self.logs.popitem(last=False)
            err_cnt = len(log)
            logger.info("* %s: %d error(s) found" % (desc, err_cnt))
            while len(log) > 0:
                msg = log.popleft()
                if not self.quiet:
                    logger.info("  %s" % msg)
            tot_err_cnt += err_cnt
        return tot_err_cnt
    ##############

    def init_test(self, description):
        if description not in self.logs.keys():
            self.logs[description] = deque([])

    ##############

    def check_for_hardtabs(self, file_and_line_number, line):
        """
        Log any lines containing hard tabs
        """
        test_desc = 'Check for hard tabs'
        self.init_test(test_desc)
        if "\t" in line:
            self.logs[test_desc].append("%s: %s" % (file_and_line_number, line.replace("\t", 2*uniblock)))

    ##############

    def check_for_trailing_whitespace(self, file_and_line_number, line):
        """
        Log any lines containing trailing whitespace
        """
        test_desc = 'Check for trailing white space'
        self.init_test(test_desc)
        full_line_len = len(line)
        no_trailing_space_len = len(line.rstrip(" "))
        if no_trailing_space_len < full_line_len:
            self.logs[test_desc].append("%s: %s" % (file_and_line_number, line.rstrip(" ")+(full_line_len-no_trailing_space_len)*uniblock))

    ##############

    def check_line_length(self, file_and_line_number, line, comment_char="!", max_len=132):
        """
        Log any lines exceeding max_len
        Currently ignores comment characters / anything following
        """
        test_desc = 'Check length of lines'
        self.init_test(test_desc)
        line_len = len(line.split(comment_char)[0].rstrip(" "))
        if line_len > max_len:
            self.logs[test_desc].append("%s: %s" % (file_and_line_number, line[:max_len]+(line_len-max_len)*uniblock))

    ##############

    def check_case_sensitive_module_statements(self, file_and_line_number, line, comment_char="!", max_len=132):
        """
        The following module statements should be all lowercase:
        * implicit none
        * public
        * private
        * save
        Note that at some point we may want to remove "save" altogether, since it is implicit in a module
        """
        test_desc = 'Check for case sensitive statements'
        self.init_test(test_desc)
        statements = ["implicit none", "public", "private", "save"]
        # ignore comments, and strip all white space
        line_loc = line.split(comment_char)[0].strip(" ")
        if line_loc.lower() in statements:
            if line_loc not in statements:
                self.logs[test_desc].append("%s: %s" % (file_and_line_number, line))

    ##############

    def check_for_spaces(self, file_and_line_number, line, comment_char="!", max_len=132):
        """
        The following statements should all include spaces:
        * else if
        * end if
        * else where
        * end where
        * end do
        """
        test_desc = 'Check for spaces in statements'
        self.init_test(test_desc)
        statements = ["elseif", "endif", "elsewhere", "endwhere", "enddo"]
        # ignore comments, and strip all white space
        line_loc = line.split(comment_char)[0].strip(" ")
        for bad_statement in statements:
            if line_loc.lower().startswith(bad_statement):
                self.logs[test_desc].append("%s: %s" % (file_and_line_number, line_loc))
                break

    ##############

    def check_for_double_quotes(self, file_and_line_number, line):
        """
        All Fortran strings should appear as 'string', not "string"
        """
        test_desc = 'Check for double quotes in statements'
        self.init_test(test_desc)
        if '"' in line:
            self.logs[test_desc].append("%s: %s" % (file_and_line_number, line))

    ##############

    def check_logical_statements(self, file_and_line_number, line):
        """
        Use symbols, not words, for logical operators:
        * >=, not .ge.
        * >, not .gt.
        * <=, not .le.
        * <, not .lt.
        * ==, not .eq.
        * /=, not .ne.
        """
        test_desc = 'Check for unwanted logical operators'
        self.init_test(test_desc)
        operators = ['.ge.', '.gt.', '.le.', '.lt.', '.eq.', '.ne.']
        for op in operators:
            if op in line:
                self.logs[test_desc].append("%s: %s" % (file_and_line_number, line))
                break

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

    Tests = consistency_test_class()

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
                Tests.check_for_hardtabs(file_and_line_number, line_loc)
                Tests.check_for_trailing_whitespace(file_and_line_number, line_loc)
                Tests.check_line_length(file_and_line_number, line_loc)
                Tests.check_case_sensitive_module_statements(file_and_line_number, line_loc)
                #Tests.check_for_spaces(file_and_line_number, line_loc)
                #Tests.check_for_double_quotes(file_and_line_number, line_loc)
                #Tests.check_logical_statements(file_and_line_number, line_loc)
    f90_err_cnt = Tests.process()
    logger.info("Fortran errors found: %d" % f90_err_cnt)

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
                Tests.check_for_hardtabs(file_and_line_number, line_loc)
                Tests.check_for_trailing_whitespace(file_and_line_number, line_loc)
    py_err_cnt  = Tests.process()
    logger.info("Python errors found: %d" % py_err_cnt)

    if f90_err_cnt + py_err_cnt > 0:
        logger.info("\nTotal error count: %d" % (f90_err_cnt + py_err_cnt))
        sys.exit(1)

    logger.info("\nNo errors found!")
