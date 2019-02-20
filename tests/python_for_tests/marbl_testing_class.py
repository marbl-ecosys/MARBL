import logging
import sys
import machines as machs
import os
from os import system as sh_command

class MARBL_testcase(object):

  def __init__(self):
    # supported_compilers is public, needed by the build tests
    logging.basicConfig(format='(%(funcName)s): %(message)s', level=logging.DEBUG)
    self.supported_compilers = []

    # all other variables are private
    self._module_names = {}
    self._compiler = None
    self._machine = None
    self._hostname = None
    self._namelist_file = None
    self._input_file = None
    self._mpitasks = 0
    self._marbl_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))

  # -----------------------------------------------

  # Parse the arguments to the MARBL test script
  # Some tests will let you specify a compiler and / or input file
  # Some tests will require you to specify a machine
  def parse_args(self, desc, HaveCompiler=True, HaveInputFile=True,
                 HasPause=False, CleanLibOnly=False):

    import argparse

    parser = argparse.ArgumentParser(description=desc)
    logger = logging.getLogger(__name__)

    if HaveCompiler:
      parser.add_argument('-c', '--compiler', action='store',
                          dest='compiler', help='compiler to build with')

    if HaveInputFile:
      parser.add_argument('-i', '--input_file', action='store', dest='input_file',
                          default=None, help='input file to read')

    if HasPause:
        parser.add_argument('--no_pause', action='store_true', dest='no_pause',
                            help='do not pause between compilers')

    if CleanLibOnly:
      parser.add_argument('--clean', action='store_true',
             help='remove object, module, and library files for MARBL lib')
    else:
      parser.add_argument('--clean', action='store_true',
             help='remove object, module, and library files for MARBL driver')

    parser.add_argument('-m', '--mach', '--machine', action='store', dest='mach',
           help='machine to build on', choices=machs.supported_machines)

    parser.add_argument('--mpitasks', action='store', dest='mpitasks',
                        default=0, help='Number of MPI tasks (default: 0 => no MPI)')

    parser.add_argument('-n', '--namelist_file', action='store', dest='namelist_file',
                        default='test.nml', help='namelist file for the marbl standalone driver')

    args = parser.parse_args()

    # Run make clean if option is specified
    if args.clean:
      if CleanLibOnly:
        self._clean_lib()
      else:
        self._clean_exe()
      sys.exit(0)

    if args.mach == None:
      # If --mach is not specified, guess at machine name from hostname
      from socket import gethostname
      self._hostname = gethostname()
      found = True
      if any(host in self._hostname for host in ['geyser', 'caldera', 'prong', 'yslogin']):
        self._machine = 'yellowstone'
      elif 'cheyenne' in self._hostname:
        self._machine = 'cheyenne'
      elif 'hobart' in self._hostname:
        self._machine = 'hobart'
      elif 'edison' in self._hostname:
        self._machine = 'edison'
      else:
        found = False
        logger.info('No machine specified and %s is not recognized' % self._hostname)
        logger.info('This test will assume you are not running on a supported cluster')
        self._machine = 'local'

      if found:
        logger.info('No machine specified, but it looks like you are running on %s' % self._machine)

      logger.info('Override with the --mach option if this is not correct')
    else:
      self._machine = args.mach
      logger.info('Running test on %s' % self._machine)

    machs.machine_specific(self._machine, self.supported_compilers, self._module_names)

    if HaveCompiler:
      self._compiler = args.compiler
      if self._compiler == None:
        self._compiler = self.supported_compilers[0]
        logger.info('No compiler specified, using %s by default' % self._compiler)
      else:
        logger.info('Testing with %s' % self._compiler)

    if HaveInputFile:
      self._input_file = args.input_file

    self._namelist_file = args.namelist_file
    self._mpitasks = int(args.mpitasks)
    sys.stdout.flush()

    if HasPause:
        self.pause = not args.no_pause

    # ERROR CHECKING
    if HaveCompiler:
      if not self._compiler in self.supported_compilers:
        logger.error("%s is not supported on %s, please use one of following:" % (self._compiler, self._machine))
        logger.info(self.supported_compilers)
        sys.exit(1)

  # -----------------------------------------------

  # Build libmarbl.a
  def build_lib(self, loc_compiler=None):

    if loc_compiler == None:
      loc_compiler = self._compiler

    src_dir = os.path.join(self._marbl_dir, 'src')

    if self._machine != 'local':
      machs.load_module(self._machine, loc_compiler, self._module_names[loc_compiler])

    makecmd = 'make %s' % loc_compiler
    if self._mpitasks > 0:
      makecmd += ' USEMPI=TRUE'
    status_code = sh_command('cd %s; %s' % (src_dir, makecmd))
    if status_code != 0:
        logging.error("ERROR building MARBL library")
        sys.exit(1)

  # -----------------------------------------------

  # Build marbl executable
  def build_exe(self, loc_compiler=None):

    if loc_compiler == None:
      loc_compiler = self._compiler

    drv_dir = os.path.join(self._marbl_dir, 'tests', 'driver_src')

    if self._machine != 'local':
      machs.load_module(self._machine, loc_compiler, self._module_names[loc_compiler])

    makecmd = 'make %s' % loc_compiler
    if self._mpitasks > 0:
      makecmd += ' USEMPI=TRUE'
    status_code = sh_command('cd %s; %s' % (drv_dir, makecmd))
    if status_code != 0:
        logging.error("ERROR building MARBL stand-alone driver")
        sys.exit(1)

  # -----------------------------------------------

  # Execute marbl.exe
  def run_exe(self):

    logger = logging.getLogger(__name__)

    # build the executable command string
    execmd = os.path.join(self._marbl_dir, 'tests', 'driver_exe') + os.sep

    # if running in parallel, executable is marbl-mpi.exe
    if self._mpitasks > 0:
      execmd += "marbl-mpi.exe"

      # need to launch with mpirun
      # Note that yellowstone actually uses mpirun.lsf
      # (and we want to avoid running on a login node)
      if self._machine == 'yellowstone':
        execmd = 'mpirun.lsf %s' % execmd
        if 'yslogin' in self._hostname:
          # on login node => request caldera node!
          execmd = 'execca %s' % execmd
      else:
        execmd = 'mpirun -n %d %s' % (self._mpitasks, execmd)

    # if running in serial, executable is marbl.exe
    else:
      execmd += "marbl.exe"

    # First argument is the file containing &marbl_driver_nml namelist
    execmd += " -n %s" % self._namelist_file

    # If an input file was specified, it should be the second argument
    if self._input_file != None:
      execmd += " -i %s" % self._input_file

    # Log executable command
    logging.info("Running following command:")
    logging.info(execmd)
    sys.stdout.flush()
    status_code = sh_command(execmd)
    if status_code != 0:
        logging.error("ERROR in executable")
        sys.exit(1)

  # -----------------------------------------------
  # PRIVATE ROUTINES
  # -----------------------------------------------

  # clean libmarbl.a
  def _clean_lib(self):

    src_dir = os.path.join(self._marbl_dir, 'src')
    sh_command('cd %s; make clean' % src_dir)

  # -----------------------------------------------

  # Clean marbl.exe
  def _clean_exe(self):

    drv_dir = os.path.join(self._marbl_dir, 'tests', 'driver_src')
    sh_command('cd %s; make clean' % drv_dir)

