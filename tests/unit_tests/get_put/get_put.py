#!/usr/bin/env python

from sys import path

path.insert(0,'../../python_for_tests')
from marbl_testing_class import MARBL_testcase

mt = MARBL_testcase()

mt.parse_args(desc='Set all configuration variables and parameters via put '
                   'statements and use get statements to ensure they are '
                   'correct. Also put different values to make sure nothing '
                   'else changes.', HaveInputFile=False)

mt.build_exe()

mt.run_exe()
