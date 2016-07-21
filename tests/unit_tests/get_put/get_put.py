#!/usr/bin/env python

from sys import path
path.insert(0,'../../python_for_tests')

import marbl_testing as mt

mt.parse_args(desc='Set all configuration variables and parameters via put statements, use get to ensure they are correct. Also put different values to make sure nothing else changes.', HaveNamelist=False)

mt.build_exe()

mt.run_exe()
