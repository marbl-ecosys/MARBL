import sys
from os import system as sh_command

# Supported machines for running MARBL tests
supported_machines = ['local-gnu', 
                      'yellowstone',
                      'hobart',
                      'edison']

# -----------------------------------------------

def load_module(mach, compiler):

  print "Trying to load %s on %s" % (compiler, mach)
  
  if mach == 'yellowstone':
    sys.path.insert(0,'/glade/apps/opt/lmod/lmod/init')
    from env_modules_python import module
    module('purge')
    module('load', compiler)
    module('load', 'ncarcompilers')
    module('load', 'ncarbinlibs')

  if mach == 'hobart':
    sys.path.insert(0,'/usr/share/Modules/init')
    from python import module
    module('purge')
    if compiler == 'pgi':
      module(['load', 'compiler/%s/17.01' % compiler])
    else:
      module(['load', 'compiler/%s' % compiler])

  if mach == 'edison':
    sys.path.insert(0,'/opt/modules/default/init')
    from python import module
    module('purge')
    module(['load', 'PrgEnv-%s' % compiler])
    if compiler == 'cray':
      module(['swap', 'cce', 'cce/8.5.0.4664'])

# -----------------------------------------------

# Set up supported compilers based on what machine you are running on
# so code can abort if an unsupported compiler is requested.
# If no compiler is specified, the supported_compilers[0] will be used.
def machine_specific(mach, supported_compilers):

  global supported_machines

  if mach not in supported_machines:
    print "%s is not a supported machine! Try one of the following:" % mach
    print supported_machines
    sys.exit(1)

  if mach == 'yellowstone':
    # NCAR machine
    supported_compilers.append('intel')
    supported_compilers.append('gnu')
    return

  if mach == 'hobart':
    # NCAR machine (run by CGD)
    supported_compilers.append('nag')
    supported_compilers.append('intel')
    supported_compilers.append('gnu')
    supported_compilers.append('pgi')
    return

  if mach == 'edison':
    # NERSC machine
    supported_compilers.append('cray')
    return

  if mach == 'local-gnu':
    # Not a specific machine, but a flag to specify
    # "run with gnu without loading any modules"
    supported_compilers.append('gnu')
    return

# -----------------------------------------------
