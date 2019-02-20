#!/usr/bin/env python

from sys import path
import os
import logging
logger = logging.getLogger(__name__)

path.insert(0, os.path.join('..', 'python_for_tests'))
from marbl_testing_class import MARBL_testcase
from general import pause

mt = MARBL_testcase()
mt.parse_args(desc='Build marbl.exe with every supported compiler on specified machine', HaveCompiler=False,
              HaveInputFile=False, HasPause=True)

for i,compiler in enumerate(mt.supported_compilers):
  mt.build_exe(loc_compiler=compiler)
  logging.info("Done with %s build" % compiler)
  if (i != len(mt.supported_compilers)-1) and (mt.pause):
    pause()

