import logging
import sys
import os
from os import system as sh_command

# Supported machines for running MARBL tests
supported_machines = ['local',
                      'derecho',
                      'cheyenne',
                      'hobart',
                      'edison',
                      'greenplanet']

# -----------------------------------------------

def _compiler_is_present(compiler):
  for path in os.environ["PATH"].split(os.pathsep):
    if os.access(os.path.join(path, compiler), os.X_OK): return True
  return False

def load_module(mach, compiler, module_name):

  logger = logging.getLogger(__name__)
  logger.info("Building with %s on %s" % (compiler, mach))
  logger.info("Loading module %s..." % module_name)

  if mach == 'derecho':
    sys.path.insert(0, os.path.join(os.sep, 'glade', 'u', 'apps', 'derecho', '23.06', 'spack', 'opt', 'spack', 'lmod', '8.7.20', 'gcc', '7.5.0', 'pdxb', 'lmod', 'lmod', 'init'))
    from env_modules_python import module
    module('--force', 'purge')
    module('load', 'cesmdev/1.0')
    if compiler == 'cray':
        module('load', 'ncarenv/23.06')
        module('load', 'craype')
    elif compiler == 'nvhpc':
        module('load', 'ncarenv/23.06')
    else:
        module('load', 'ncarenv/23.09')
    module('load', module_name)
    module('load', 'ncarcompilers/1.0.0')
    if compiler == 'cray':
        module('load', 'cray-mpich/8.1.25')
    else:
        module('load', 'cray-mpich/8.1.27')
    module('load', 'netcdf/4.9.2')
    if compiler in ['gnu', 'cray']:
        module('load', 'cray-libsci/23.02.1.1')

  if mach == 'cheyenne':
    sys.path.insert(0, os.path.join(os.sep, 'glade', 'u', 'apps', 'ch', 'opt', 'lmod', '7.2.1', 'lmod', 'lmod', 'init'))
    from env_modules_python import module
    module('purge')
    module('load', module_name)
    module('load', 'netcdf/4.6.1')
    module('load', 'ncarcompilers')
    if compiler == 'intel':
      module('load', 'mpt/2.19')

  if mach == 'hobart':
    sys.path.insert(0, os.path.join(os.sep, 'usr', 'share', 'Modules', 'init'))
    from python import module
    module('purge')
    module(['load', module_name])

  if mach == 'edison':
    sys.path.insert(0, os.path.join(os.sep, 'opt', 'modules', 'default', 'init'))
    from python import module
    module('purge')
    module(['load', 'craype/2.5.12'])
    module(['load', module_name])

  if mach == 'greenplanet':
    sys.path.insert(0, os.path.join(os.sep, 'usr', 'share', 'lmod', 'lmod', 'init'))
    from env_modules_python import module
    module('purge')
    module('load', module_name)
    module('load', 'netcdf/4.7.0')
    module('load', 'openmpi/3.1.6')

# -----------------------------------------------

# Set up supported compilers based on what machine you are running on
# so code can abort if an unsupported compiler is requested.
# If no compiler is specified, the supported_compilers[0] will be used.
def machine_specific(mach, supported_compilers, module_names):

  global supported_machines
  logger = logging.getLogger(__name__)

  if mach not in supported_machines:
    logger.info("%s is not a supported machine! Try one of the following:" % mach)
    logger.info(supported_machines)
    sys.exit(1)

  if mach == 'derecho':
    # NCAR machine
    supported_compilers.append('intel')
    supported_compilers.append('gnu')
    supported_compilers.append('cray')
    supported_compilers.append('nvhpc')
    module_names['intel'] = 'intel/2023.2.1'
    module_names['gnu'] = 'gcc/12.2.0'
    module_names['cray'] = 'cce/15.0.1'
    module_names['nvhpc'] = 'nvhpc/23.1'
    return

  if mach == 'cheyenne':
    # NCAR machine
    supported_compilers.append('intel')
    supported_compilers.append('gnu')
    module_names['intel'] = 'intel/19.0.2'
    module_names['gnu'] = 'gnu/7.3.0'
    return

  if mach == 'hobart':
    # NCAR machine (run by CGD)
    supported_compilers.append('nag')
    supported_compilers.append('intel')
    supported_compilers.append('gnu')
    supported_compilers.append('pgi')
    module_names['nag'] = 'compiler/nag/6.2'
    module_names['intel'] = 'compiler/intel/18.0.3'
    module_names['gnu'] = 'compiler/gnu/8.1.0'
    module_names['pgi'] = 'compiler/pgi/18.1'
    return

  if mach == 'edison':
    # NERSC machine
    supported_compilers.append('cray')
    module_names['cray'] = 'PrgEnv-cray'
    return

  if mach == 'greenplanet':
    # UCI machine
    supported_compilers.append('intel')
    module_names['intel'] = 'intel/2018.3'

  if mach == 'local':
    # Not a specific machine, but a flag to specify
    # "look for all compilers in path, use what is available"
    if _compiler_is_present('gfortran'):
      supported_compilers.append('gnu')
    if _compiler_is_present('pgf90'):
      supported_compilers.append('pgi')
    if _compiler_is_present('ifort'):
      supported_compilers.append('intel')
    if _compiler_is_present('nagfor'):
      supported_compilers.append('nag')
    if supported_compilers == []:
      logger.error('ERROR: can not find any compilers on this machine')
      sys.exit(1)
    else:
      logger.info('Found the following compilers in $PATH: %s' % supported_compilers)

    return

# -----------------------------------------------
