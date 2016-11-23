#!/usr/bin/env python

from sys import path

path.insert(0,'../../python_for_tests')
from marbl_testing_class import MARBL_testcase

mt = MARBL_testcase()

mt.parse_args(desc='Run full MARBL setup (config, init, and complete), without reading configuration variables and parameters from namelist', HaveNamelist=False)

mt.build_exe()

mt.run_exe()
