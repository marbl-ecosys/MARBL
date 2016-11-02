#!/usr/bin/env python

from sys import path
path.insert(0,'../../python_for_tests')

import marbl_testing as mt

mt.parse_args(desc='Run full MARBL setup (config, init, and complete) and print '
                   'list of all the tracers MARBL will restore')

mt.build_exe()

mt.run_exe()
