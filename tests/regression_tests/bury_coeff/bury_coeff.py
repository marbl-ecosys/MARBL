#!/usr/bin/env python

from sys import path
import os

path.insert(0, os.path.join('..', '..', 'python_for_tests'))
from marbl_testing_class import MARBL_testcase

mt = MARBL_testcase()

mt.parse_args(desc='Run full MARBL setup (config, init, and complete) and print '
                   'output relating to burial coefficients',
                   DefaultSettingsFile='../../input_files/settings/marbl_with_ladjust_bury_coeff.settings')

mt.build_exe()

mt.run_exe()
