.. _pop_tracers:

==================================
POP Interacting with MARBL Tracers
==================================

POP will read the initial state for each tracers and store the data in a global array (dimensions ``nx`` by ``ny`` by ``nz`` by ``n_tracers``).
To know what tracer to read into each ``n_tracers`` index, POP copies the information from ``marbl_instace%tracer_metadata`` to a local type during initialization.
The data type is defined as

.. block comes from prognostic
.. code-block:: fortran

   type :: tracer_field
      character(char_len) :: short_name
      character(char_len) :: long_name
      character(char_len) :: units
      character(char_len) :: tend_units
      character(char_len) :: flux_units
      real(r8)            :: scale_factor
      logical :: lfull_depth_tavg
   end type

An array of the above type is then populated; note that MARBL expects the GCM to apply any necessary scale factor so the POP datatype is set to 1.

.. block comes from ecosys_driver
.. code-block:: fortran

    ! Initialize tracer_d_module input argument (needed before reading
    ! tracers from restart file)
    do n = 1, ecosys_tracer_cnt
       tracer_d_module(n)%short_name       = marbl_instances(1)%tracer_metadata(n)%short_name
       tracer_d_module(n)%long_name        = marbl_instances(1)%tracer_metadata(n)%long_name
       tracer_d_module(n)%units            = marbl_instances(1)%tracer_metadata(n)%units
       tracer_d_module(n)%tend_units       = marbl_instances(1)%tracer_metadata(n)%tend_units
       tracer_d_module(n)%flux_units       = marbl_instances(1)%tracer_metadata(n)%flux_units
       tracer_d_module(n)%lfull_depth_tavg = marbl_instances(1)%tracer_metadata(n)%lfull_depth_tavg
       tracer_d_module(n)%scale_factor     = c1
    end do

POP combines the ``tracer_d_module`` object with information regarding what files contain tracer initial conditions (and what the netCDF variable name corresponds to each tracer) to properly initialize each tracer.

---------------------------------
Reading Tracer Initial Conditions
---------------------------------

All tracer initial conditions are read from a file.
If the run is a continuation run, the initial tracer values are found in a restart file.
Otherwise they are read from an initial condition.

POP has a specific data type to manage the metadata of a tracer it is reading from a file.

.. block comes from passive_tracer_tools
.. code-block:: fortran

   !-----------------------------------------------------------------------
   !  derived type for reading tracers from a file
   !-----------------------------------------------------------------------

      type, public :: tracer_read
         character(char_len) :: mod_varname, filename, file_varname, file_fmt
         real(r8) :: scale_factor, default_val
      end type

Metadata such as the tracer name and the name of the tracer as it appears in the file is copied from ``tracer_d_module`` into ``tracer_inputs`` (an array of type ``tracer_read``).
The rest of ``tracer_inputs`` (file name, file format, etc) is also set and then each tracer state is read into the correct index of the tracer array  by looping over ``tracer_inputs``.
