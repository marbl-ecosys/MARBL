#!/usr/bin/env python

from sys import path

path.insert(0,'../../python_for_tests/')
from marbl_testing_class import MARBL_testcase

mt = MARBL_testcase()

# FIXME: how will this test actually differ from init_without_namelist?
mt.parse_args(desc='Run full MARBL setup (config, init, and complete), with test timer turned on', HaveNamelist=False)

mt.build_exe()

mt.run_exe()
