#!/usr/bin/env python

from sys import path

path.insert(0,'../../python_for_tests')
from marbl_testing_class import MARBL_testcase

mt = MARBL_testcase()

mt.parse_args(desc='Run MARBL init and return tracer counts for ecosys_base, ciso, and marbl')

mt.build_exe()

mt.run_exe()
