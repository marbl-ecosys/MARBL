#!/usr/bin/env python

from sys import path
path.insert(0, '../python_for_tests')

import marbl_testing as mt

mt.parse_args(desc='Build marbl.exe with every supported compiler on specified machine', HaveCompiler=False, HaveNamelist=False)
for i,compiler in enumerate(mt.supported_compilers):
  mt.build_exe(loc_compiler=compiler)
  print "Done with %s build" % compiler
  if i != len(mt.supported_compilers)-1:
    mt.pause()

