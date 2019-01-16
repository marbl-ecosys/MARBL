.. _add-diagnostic:

===================
Adding a Diagnostic
===================

This is a five step process.
There are three changes to make in the Fortran code.
The indexing type is in ``marbl_interface_private_types.F90``, and the rest of the code is in ``marbl_diagnostics_mod.F90``.
(If your diagnostic is part of the carbon isotope tracer module, that code belongs in ``marbl_ciso_diagnostics_mod.F90``.)
There are also two steps to make sure the diagnostic is known the GCM so it is included in the output.

For this example, we follow the in situ temperature, which uses the ``insitu_temp`` index.

.. _ref-add-diag:

---------------------------------------------
Step 1. Add to MARBL diagnostic indexing type
---------------------------------------------

To reduce the number of string comparisons inside routines called every time-step, MARBL uses integer indices to track many different variables.
These indices are packed into datatypes to group common indices together.
So the indices for diagnostics variables are split into ``marbl_surface_flux_diagnostics_indexing_type`` and ``marbl_interior_tendency_diagnostics_indexing_type``.
``insitu_temp`` is an interior forcing diagnostic.

.. block comes from marbl_interface_private_types
.. code-block:: fortran

   type, public :: marbl_interior_tendency_diagnostics_indexing_type
     ! General 2D diags
     integer(int_kind) :: zsatcalc
     integer(int_kind) :: zsatarag
     .
     .
     .
     ! General 3D diags
     integer(int_kind) :: insitu_temp
     .
     .
     .
   end type marbl_interior_tendency_diagnostics_indexing_type

-----------------------------------
Step 2. Add to diagnostic structure
-----------------------------------

Another common feature among MARBL datatypes is the idea of adding an element to a derived type to contain all the data.
Most derived types, including as ``marbl_diagnostics_type``,  are "reallocating":
when a field is added, a new array of size ``N+1`` is created, the existing array is copied into the first ``N`` elements and then deallocated, and the new entry becomes element ``N+1``.
In these situations, pointers are used instead of allocatable arrays so that ``marbl_instance%{surface,interior}_forcing_diags%diags`` can point to the new array.

.. block comes from marbl_diagnostics_mod
.. code-block:: fortran

  lname = 'in situ temperature'
  sname = 'insitu_temp'
  units = 'degC'
  vgrid = 'layer_avg'
  truncate = .false.
  call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
       ind%insitu_temp, marbl_status_log)
  if (marbl_status_log%labort_marbl) then
    call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
    return
  end if

------------------------------------------
Step 3. Populate diagnostic type with data
------------------------------------------

The purpose of the ``marbl_diagnostics_type`` structure is to allow an easy way to pass diagnostics through the interface.
This step copies data only available in MARBL into the datatype that is available to the GCM.

.. block comes from marbl_diagnostics_mod
.. code-block:: fortran

  associate( &
       kmt   => domain%kmt, &
       diags => marbl_interior_tendency_diags%diags, &
       ind   => marbl_interior_tendency_diag_ind &
       )
  diags(ind%insitu_temp)%field_3d(1:kmt, 1) = temperature(1:kmt)
  end associate

.. note::
  In situ temperature is copied to the diagnostic type in ``marbl_diagnostics_interior_tendency_compute()``.
  This subroutine also calls many different ``store_diagnostics_*`` subroutines, but in a future release the ``store_diagnostics`` routines will be condensed into a smaller subset of routines.
  Regardless, find the routine that makes the most sense for your diagnostic variable.
  (Surface forcing fields are copied to the diagnostic type in ``marbl_diagnostics_surface_flux_compute()``.)

-----------------------------------------
Step 4. Update the Diagnostics YAML files
-----------------------------------------

We use a YAML file to provide an easy-to-edit and human-readable text file containing a list of all diagnostics and the recommended frequency of output.
Developers adding or removing diagnostics should make changes to ``defaults/diagnostics_latest.yaml``.

.. block comes from diagnostics_latest.yaml
.. code-block:: yaml

  insitu_temp :
     longname : in situ temperature
     units : degC
     vertical_grid : layer_avg
     frequency : medium
     operator : average

Note that ``insitu_temp`` matches what we used for the short name in `Step 2. Add to diagnostic structure`_.
The frequency ``medium`` means "we recommend outputting this variable monthly".
Other acceptable frequencies are ``never``, ``low`` (annual), and ``high`` (daily).

The operator means "average over this time period."
Other acceptable operators are ``instantaneous``, ``minimum``, and ``maximum``.
You can recommend multiple frequencies by adding a list to the YAML, as long as the operator key is a list of the same size:

.. block comes from diagnostics_latest.yaml
.. code-block:: yaml

  CaCO3_form_zint :
     longname : Total CaCO3 Formation Vertical Integral
     units : mmol/m^3 cm/s
     vertical_grid : none
     frequency :
        - medium
        - high
     operator :
        - average
        - average

-------------------------------------
Step 5. Convert the YAML file to JSON
-------------------------------------

We prefer editing YAML files to editing JSON files because they are much easier to maintain (and allow user comments).
Unfortunately, python does not include a YAML parser in the default distributions.
Rather than require all users to install ``pyYAML``, we require that of MARBL developers and then ask them to convert the YAML files to JSON.
The ``MARBL_tools/yaml_to_json.py`` script is provided to do just that:

.. code-block:: none

  $ cd MARBL_tools
  $ ./yaml_to_json.py

The rest of the python scripts provided in the ``MARBL_tools/`` subdirectory rely on the JSON file rather than the YAML.
``MARBL_tools/MARBL_generate_diagnostics_file.py`` will turn the JSON file into a list for the GCM to parse:

.. block comes from marbl.diags
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
  .
  .
  .
  CaCO3_form_zint : medium_average, high_average
  .
  .
  .
  insitu_temp : medium_average

It is then up to the GCM to convert this text file into a format it recognizes for output (e.g. POP will add to the ``tavg_contents`` file).
