.. _add-diagnostic:

===================
Adding a Diagnostic
===================

This is a 4 step process.
There are 3 changes to make in the Fortran code, all of which are made in ``marbl_diagnostics_mod.F90``.
There is also the added step of adding the new diagnostic to model output.
Currently this is handled by the GCM (we will use POP as an example) but soon MARBL will be able to generate output lists for a variety of GCMs.

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
There is less consistency in how this is done, for example some data types (such as ``marbl_diagnostics_type``) require an exact count of the number of diagnostics before allocating memory.
Other datatypes are "reallocating" -- when a field is added, the existing structure is copied, deallocated, allocated to the new size, and then updated.
We are investigating the performance hit to being consistent with the reallocation strategy.

.. code-block:: fortran

  if (count_only) then
   num_forcing_diags = num_forcing_diags + 1
  else
   lname    = 'DIC Surface Gas Flux'
   sname    = 'FG_CO2'
   units    = 'mmol/m-3 cm/s'
   vgrid    = 'none'
   truncate = .false.
   call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
		ind%DIC_GAS_FLUX, marbl_status_log)
   if (marbl_status_log%labort_marbl) then
		call log_add_diagnostics_error(marbl_status_log, sname, subname)
		return
   end if
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

----------------
POP Code Changes
----------------

To add DIC surface gas flux to a tavg file, simply and the following line to your ``tavg_contents`` file::

  1  FG_CO2

Note that ``FG_CO2`` matches what we used for the shortname in `Step 2. Add to diagnostic structure`_.
Also, the ``1`` refers to the POP stream file to include ``FG_CO2`` in, so you may choose to add it to a different output file instead.
As mentioned previously, MARBL will soon produce the ``tavg_contents`` file for POP (with a tool that can be configured to produce output lists for other GCMs as well).
