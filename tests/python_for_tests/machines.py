import sys
from os import system as sh_command

# -----------------------------------------------

def load_module(mach, compiler):

  print "Trying to load %s on %s" % (compiler, mach)
  if mach == 'hobart':
    sh_command('module purge')
    sh_command('module load compiler/%s' % compiler)

  if mach == 'yellowstone':
    sys.path.insert(0,'/glade/apps/opt/lmod/lmod/init')
    import env_modules_python as lmod
    lmod.module('purge')
    lmod.module(' load %s' % compiler)

# -----------------------------------------------

# Set up supported compilers based on what machine you are running on
# so code can abort if an unsupported compiler is requested.
# If no compiler is specified, the supported_compilers[0] will be used.
def machine_specific(mach, supported_compilers, supported_machines):

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
