---------
| ABOUT |
---------


The MARine Biogeochemistry Library (MARBL) is organized as follows:
    
    * include/  -- default location of mod files
    * lib/ -- default location of libmarbl.a (or libmarbl-[compiler].a)
    * src/ -- source code for building libmarbl.a
    * test/ -- all source and scripts required for stand-alone executable, as
               well as the executable itself. Also moved the obj/ dir here so
               that users don't use it instead of the include/ (especially once
               we restrict include to just contain marbl_interface.mod)

Specifically, test/ contains

* driver_src/ -- Code to build the stand-alone driver used by the tests
* driver_exe/ -- Location of marbl.exe after it is built
* obj/ -- object files (with subdirectories for each supported compiler). Each
          compiler subdirectory also has a separate directory for object files
          created from the driver build. E.g.
          obj/gnu/ contains objects from the library build with gfortran
          obj/gnu/driver/ contains objects from building marbl.exe with gfortran
* python_for_tests/ -- Python modules for code that is used by multiple test
                       scripts. E.g. routines to parse command line arguments,
                       build the library, or load modules on supported machines
* bld_tests/ -- scripts to build either just the library or both the library and
                marbl.exe with every supported compiler on a supported machine.
* regression_tests/ -- Tests that produce output with various MARBL
                       configurations so developers can quickly check the effect
                       of new code
* unit_tests/ -- Tests that check the functionality of small pieces of the code
* README -- more details about the stand-alone test system

--------------------------
| INSTALLING THE LIBRARY |
--------------------------

The Makefile in src/ provides all that is needed to build the library. Run

$ make OBJ_DIR=[location of object files] depends
$ make [desired_location_of_lib]/libmarbl.a \
       USE_DEPS=TRUE \
       FC=[compiler] \
       FCFLAGS="[compiler flags]" \
       OBJ_DIR=[location of object files] \
       INC_DIR=[location of .mod files]

The first call will produce the dependency file (shared_deps.d), the second call
will build libmarbl.a and the *.mod files. When building the GCM, you will need
to add

-I$(INC_DIR)

And when linking

-L$(LIB_DIR) -lmarbl

