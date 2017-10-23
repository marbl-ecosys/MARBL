#!/usr/bin/env python

from sys import path
import os

path.insert(0, os.path.join('..', '..', 'python_for_tests'))
from marbl_testing_class import MARBL_testcase

mt = MARBL_testcase()

mt.parse_args(desc='Generate an input file for MARBL')

mt.build_exe()

mt.run_exe()
