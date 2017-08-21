import sys
from os import system as sh_command

# Supported machines for running MARBL tests
supported_machines = ['local-gnu',
                      'local-pgi',
                      'yellowstone',
                      'cheyenne',
                      'hobart',
                      'edison']

# -----------------------------------------------

def load_module(mach, compiler, module_name):

  print "Building with %s on %s" % (compiler, mach)
  print "Loading module %s..." % module_name

  if mach == 'yellowstone':
    sys.path.insert(0,'/glade/apps/opt/lmod/lmod/init')
    from env_modules_python import module
    module('purge')
    module('load', module_name)
    module('load', 'ncarcompilers')
    module('load', 'ncarbinlibs')

  if mach == 'cheyenne':
    sys.path.insert(0,'/glade/u/apps/ch/opt/lmod/7.2.1/lmod/lmod/init')
    from env_modules_python import module
    module('purge')
    module('load', module_name)
    module('load', 'ncarcompilers')
    module('load', 'mpt/2.15')

  if mach == 'hobart':
    sys.path.insert(0,'/usr/share/Modules/init')
    from python import module
    module('purge')
    module(['load', module_name])

  if mach == 'edison':
    sys.path.insert(0,'/opt/modules/default/init')
    from python import module
    module('purge')
    module(['load', module_name])
    if compiler == 'cray':
      module(['swap', 'cce', 'cce/8.5.0.4664'])

# -----------------------------------------------

# Set up supported compilers based on what machine you are running on
# so code can abort if an unsupported compiler is requested.
# If no compiler is specified, the supported_compilers[0] will be used.
def machine_specific(mach, supported_compilers, module_names):

  global supported_machines

  if mach not in supported_machines:
    print "%s is not a supported machine! Try one of the following:" % mach
    print supported_machines
    sys.exit(1)

  if mach == 'yellowstone':
    # NCAR machine
    supported_compilers.append('intel')
    supported_compilers.append('gnu')
    supported_compilers.append('pgi')
    module_names['intel'] = 'intel/17.0.1'
    module_names['gnu'] = 'gnu/7.1.0'
    module_names['pgi'] = 'pgi/17.5'
    return

  if mach == 'cheyenne':
    # NCAR machine
    supported_compilers.append('intel')
    supported_compilers.append('gnu')
    module_names['intel'] = 'intel/17.0.1'
    module_names['gnu'] = 'gnu/7.1.0'
    return

  if mach == 'hobart':
    # NCAR machine (run by CGD)
    supported_compilers.append('nag')
    supported_compilers.append('intel')
    supported_compilers.append('gnu')
    supported_compilers.append('pgi')
    module_names['nag'] = 'compiler/nag/6.1'
    module_names['intel'] = 'compiler/intel/17.0.4.196'
    module_names['gnu'] = 'compiler/gnu/5.4.0'
    module_names['pgi'] = 'compiler/pgi/17.04'
    return

  if mach == 'edison':
    # NERSC machine
    supported_compilers.append('cray')
    module_names['cray'] = 'PrgEnv-cray'
    return

  if mach == 'local-gnu':
    # Not a specific machine, but a flag to specify
    # "run with gnu without loading any modules"
    supported_compilers.append('gnu')
    return

  if mach == 'local-pgi':
    # Not a specific machine, but a flag to specify
    # "run with pgi without loading any modules"
    supported_compilers.append('pgi')
    return

# -----------------------------------------------
