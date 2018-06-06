.. _add-diagnostic:

===================
Adding a Diagnostic
===================

This is a four step process.
There are three changes to make in the Fortran code, all of which are made in ``marbl_diagnostics_mod.F90``.
There are also two steps to make sure the diagnostic is known the GCM so it is included in the output.

------------------
MARBL Code Changes
------------------

For this example, we follow the DIC Surface Gas Flux, which uses the ``DIC_GAS_FLUX`` index.

.. _ref-add-diag:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Add to MARBL diagnostic indexing type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To reduce the number of string comparisons inside routines called every time-step, MARBL uses integer indices to track many different variables.
These indices are packed into datatypes to group common indices together.
So the indices for diagnostics variables are split into ``marbl_surface_forcing_diagnostics_indexing_type`` and ``marbl_interior_forcing_diagnostics_indexing_type``.
``DIC_GAS_FLUX`` is a surface forcing diagnostics.

.. code-block:: fortran

   type marbl_surface_forcing_diagnostics_indexing_type
     integer(int_kind) :: ECOSYS_IFRAC
     integer(int_kind) :: ECOSYS_XKW
     integer(int_kind) :: ECOSYS_ATM_PRESS
     .
     .
     .
     integer(int_kind) :: DIC_GAS_FLUX
     .
     .
     .
   end type marbl_surface_forcing_diagnostics_indexing_type

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Add to diagnostic structure
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another common feature among MARBL datatypes is the idea of adding an element to a derived type to contain all the data.
Most derived types, including as ``marbl_diagnostics_type``,  are "reallocating":
when a field is added, a new array of size ``N+1`` is created, the existing array is copied into the first ``N`` elements and then deallocated, and the new entry becomes element ``N+1``.
In these situations, pointers are used instead of allocatable arrays so that ``marbl_instance%{surface,interior}_forcing_diags`` can point to the new array.

.. code-block:: fortran

  lname    = 'DIC Surface Gas Flux'
  sname    = 'FG_CO2'
  units    = 'mmol/m^3 cm/s'
  vgrid    = 'none'
  truncate = .false.
  call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
       ind%DIC_GAS_FLUX, marbl_status_log)
  if (marbl_status_log%labort_marbl) then
    call log_add_diagnostics_error(marbl_status_log, sname, subname)
    return
  end if

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 3. Populate diagnostic type with data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The purpose of the ``marbl_diagnostics_type`` structure is to allow an easy way to pass diagnostics through the interface.
This step copies data only available in MARBL into the datatype that is available to the GCM.

.. code-block:: fortran

  if (lflux_gas_co2) then
    .
    .
    .
    diags(ind_diag%DIC_GAS_FLUX)%field_2d(:)         = flux_co2(:)
    .
    .
    .
    diags(ind_diag%pCO2SURF)%field_2d(:)             = pco2surf(:)
    .
    .
    .
  end if  !  lflux_gas_co2

.. note::
  There are many different ``store_diagnostics_*`` subroutines for diagnostics coming out of ``set_interior_forcing()``, surface forcing fields (like ``DIC_GAS_FLUX``) are stored in ``marbl_diagnostics_set_surface_forcing().
  In a future release the ``store_diagnostics`` routines will be condensed into a smaller subset of routines and there will be a clearer naming convention.
  Regardless, find the routine that makes the most sense for your diagnostic variable.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 4. Update the Diagnostics YAML files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We use a YAML file to provide an easy-to-edit and human-readable text file containing a list of all diagnostics and the recommended frequency of output.

.. code-block:: yaml

  FG_CO2 : &FG_CO2 # rename ind%DIC_GAS_FLUX -> ind%FG_CO2
     longname : DIC Surface Gas Flux
     units : mmol/m^3 cm/s
     vertical_grid : none
     frequency :
        - medium
        - high
     operator :
        - average
        - average

Note that ``FG_CO2`` matches what we used for the shortname in `Step 2. Add to diagnostic structure`_.
The frequencies of ``medium`` and ``high`` mean "we recommend outputting this variable both daily and monthly", and the operators mean "average over both of those time periods."

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 5. Convert the YAML file to JSON
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We prefer editing YAML files to editing JSON files because they are much easier to maintain (and allow user comments).
Unfortunately, python does not include a YAML parser in the default distributions.
Rather than require all users to install ``pyYAML``, we require that of MARBL developers and then ask them to convert the YAML files to JSON.
The ``MARBL_tools/yaml_to_json.py`` script is provided to do just that:

.. code-block:: none

  $ cd MARBL_tools
  $ ./yaml_to_json.py

The rest of the python scripts provided in the ``MARBL_tools/`` subdirectory rely on the JSON file rather than the YAML.
``MARBL_tools/MARBL_generate_diagnostics_file.py`` will turn the JSON file into a list for the GCM to parse:

.. code-block:: none

  # This file contains a list of all diagnostics MARBL can compute for a given configuration,
  # as well as the recommended frequency and operator for outputting each diagnostic.
  # The format of this file is:
  #
  # DIAGNOSTIC_NAME : frequency_operator
  #
  # And fields that should be output at multiple different frequencies will be comma-separated:
  #
  # DIAGNOSTIC_NAME : frequency1_operator1, frequency2_operator2, ..., frequencyN_operatorN
  #
  # Frequencies are never, low, medium, and high.
  # Operators are instantaneous, average, minimum, and maximum.
  ...
  FG_CO2 : medium_average, high_average

It is then up to the GCM to convert this text file into a format it recognizes for output (e.g. POP will add to the ``tavg_contents`` file).