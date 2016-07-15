#!/usr/bin/env python

from sys import path
path.insert(0, '../python_for_tests')

import marbl_testing as mt

mt.parse_args(desc='Build lib-marbl.a with every supported compiler on specified machine', HaveCompiler=False, HaveNamelist=False, CleanLibOnly=True)
for i,compiler in enumerate(mt.supported_compilers):
  mt.build_lib(loc_compiler=compiler)
  print "Done with %s build" % compiler
  if i != len(mt.supported_compilers)-1:
    mt.pause()

