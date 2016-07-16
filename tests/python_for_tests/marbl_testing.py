# module-wide variables
import sys
from os import system as sh_command
from os import path
from machines import load_module
from machines import machine_specific

compiler = None # compiler name
mach = None # machine name
namelist = 'marbl_in' # namelist file to read

# Supported machines for running MARBL tests
supported_machines = []
supported_machines.append('local-gnu')
supported_machines.append('yellowstone')
supported_machines.append('hobart')
supported_machines.append('edison')

# List of supported compilers will be generated after machine is specified
supported_compilers = []

# Absolute path of the root of MARBL checkout
# Needed to find source code for building (and executable for running)
marbl_dir = path.abspath('%s/../..' % path.dirname(__file__))

# -----------------------------------------------

# Parse the arguments to the MARBL test script
# Some tests will let you specify a compiler and / or namelist
# Some tests will require you to specify a machine
def parse_args(desc, HaveCompiler=True, HaveNamelist=True, CleanLibOnly=False):

  import argparse

  global compiler
  global mach
  global namelist
  global supported_compilers
  global supported_machines

  parser = argparse.ArgumentParser(description=desc)
  if HaveCompiler:
    parser.add_argument('-c', '--compiler', action='store', dest='compiler', help='compiler to build with')

  if HaveNamelist:
    parser.add_argument('-n', '--namelist', action='store', dest='namelist', help='namelist file to read', default='marbl_in')

  if CleanLibOnly:
    parser.add_argument('--clean', action='store_true', help='remove object, module, and library files for MARBL lib')
  else:
    parser.add_argument('--clean', action='store_true', help='remove object, module, and library files for MARBL driver')

  parser.add_argument('-m', '--mach', action='store', dest='mach', help='machine to build on', default='local-gnu', choices=supported_machines)

  args = parser.parse_args()

  # Run make clean if option is specified
  if args.clean:
    if CleanLibOnly:
      clean_lib()
    else:
      clean_exe()
    sys.exit(0)

  mach = args.mach
  if HaveCompiler:
    compiler = args.compiler
  if HaveNamelist:
    namelist = args.namelist

  machine_specific(mach, supported_compilers, supported_machines)
  if compiler == None:
    compiler = supported_compilers[0]

  if not compiler in supported_compilers:
    print("%s is not supported on %s, please use one of following:" % (compiler, mach))
    print supported_compilers
    sys.exit(1)

  print("Testing with %s on %s" % (compiler, mach))
  sys.stdout.flush()

# -----------------------------------------------

# clean libmarbl.a
def clean_lib():

  global marbl_dir

  src_dir = '%s/src' % marbl_dir
  sh_command('cd %s; make clean' % src_dir)

# -----------------------------------------------

# Clean marbl.exe
def clean_exe():

  global marbl_dir

  drv_dir = '%s/tests/driver_src' % marbl_dir
  sh_command('cd %s; make clean' % drv_dir)

# -----------------------------------------------

# Build libmarbl.a
def build_lib(loc_compiler=None):

  global marbl_dir
  global compiler

  if loc_compiler == None:
    loc_compiler = compiler

  src_dir = '%s/src' % marbl_dir

  if mach != 'local-gnu':
    load_module(mach, loc_compiler)

  sh_command('cd %s; make %s' % (src_dir, loc_compiler))

# -----------------------------------------------

# Build marbl.exe
def build_exe(loc_compiler=None):

  global marbl_dir
  global compiler

  if loc_compiler == None:
    loc_compiler = compiler

  drv_dir = '%s/tests/driver_src' % marbl_dir

  if mach != 'local-gnu':
    load_module(mach, loc_compiler)

  sh_command('cd %s; make %s' % (drv_dir, loc_compiler))

# -----------------------------------------------

# Execute marbl.exe
def run_exe():

  global marbl_dir
  global namelist

  exe_dir = '%s/exe' % marbl_dir

  sh_command('%s/marbl.exe < %s' % (exe_dir, namelist))

# -----------------------------------------------
