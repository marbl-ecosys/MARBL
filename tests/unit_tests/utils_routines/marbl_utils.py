#!/usr/bin/env python

from sys import path

path.insert(0,'../../python_for_tests')
from marbl_testing_class import MARBL_testcase

mt = MARBL_testcase()

mt.parse_args(desc='Unit tests for all the functions / routines in '
                   'marbl_utils_mod.F90', HaveInputFile=False)

mt.build_exe()

mt.run_exe()
