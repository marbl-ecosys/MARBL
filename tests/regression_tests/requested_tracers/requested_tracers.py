#!/usr/bin/env python

from sys import path
path.insert(0,'../../python_for_tests')

import marbl_testing as mt

mt.parse_args(desc='Run full MARBL setup (config, init, and complete), reading configuration variables and parameters from namelist')

mt.build_exe()

mt.run_exe()
