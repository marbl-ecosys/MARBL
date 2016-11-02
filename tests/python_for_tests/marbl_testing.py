# module-wide variables
import sys
from os import system as sh_command
from os import path
import machines as machs
from machines import machine_specific

compiler = None # compiler name
mach = None # machine name
namelist = 'marbl_in' # namelist file to read

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

  parser = argparse.ArgumentParser(description=desc)
  if HaveCompiler:
    parser.add_argument('-c', '--compiler', action='store', dest='compiler', help='compiler to build with')

  if HaveNamelist:
    parser.add_argument('-n', '--namelist', action='store', dest='namelist', help='namelist file to read', default='marbl_in')

  if CleanLibOnly:
    parser.add_argument('--clean', action='store_true', help='remove object, module, and library files for MARBL lib')
  else:
    parser.add_argument('--clean', action='store_true', help='remove object, module, and library files for MARBL driver')

  parser.add_argument('-m', '--mach', action='store', dest='mach', help='machine to build on', choices=machs.supported_machines)

  args = parser.parse_args()

  # Run make clean if option is specified
  if args.clean:
    if CleanLibOnly:
      clean_lib()
    else:
      clean_exe()
    sys.exit(0)

  if args.mach == None:
    # If --mach is not specified, guess at machine name from hostname
    from socket import gethostname
    hostname = gethostname()
    if any(host in hostname for host in ['geyser', 'caldera', 'prong', 'yslogin']):
      mach = 'yellowstone'
    elif 'hobart' in hostname:
      mach = 'hobart'
    elif 'edison' in hostname:
      mach = 'edison'
    else:
      mach = 'local-gnu'

    if mach == 'local-gnu':
      print 'No machine specified and %s is not recognized' % hostname
      print 'This test will assume you are not running on a supported cluster'
    else:
      print 'No machine specified, but it looks like you are running on %s' % mach

    print 'Override with the --mach option if this is not correct'
  else:
    mach = args.mach
    print 'Running test on %s' % mach
  if HaveCompiler:
    compiler = args.compiler
  if HaveNamelist:
    namelist = args.namelist

  machs.machine_specific(mach, supported_compilers)
  if HaveCompiler:
    if compiler == None:
      compiler = supported_compilers[0]
      print 'No compiler specified, using %s by default' % compiler
    else:
      print 'Testing with %s' % compiler

    if not compiler in supported_compilers:
      print("%s is not supported on %s, please use one of following:" % (compiler, mach))
      print supported_compilers
      sys.exit(1)

  print '----'
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
    machs.load_module(mach, loc_compiler)

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
    machs.load_module(mach, loc_compiler)

  sh_command('cd %s; make %s' % (drv_dir, loc_compiler))

# -----------------------------------------------

# Execute marbl.exe
def run_exe():

  global marbl_dir
  global namelist

  exe_dir = '%s/tests/driver_exe' % marbl_dir

  sh_command('%s/marbl.exe < %s' % (exe_dir, namelist))

# -----------------------------------------------
