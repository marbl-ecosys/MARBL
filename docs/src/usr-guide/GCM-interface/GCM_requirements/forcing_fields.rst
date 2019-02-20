.. _forcing_fields:

========================================
What Forcing Fields has MARBL Requested?
========================================

This question is similar to ":ref:`tracer_state`", and so it should not be a surprise that the answer is also very similar.
MARBL provides a stand-alone test in ``$MARBL/tests/regression_tests/requested_forcing`` as an example of how the MARBL library passes information to the GCM.

::

  $ ./requested_forcings.py

Provides a list of the forcing fields requested with the default MARBL configuration.

.. block comes from output of requested_forcings.py
.. code-block:: none

  --------------------------------
  Requested surface forcing fields
  --------------------------------

  1. u10_sqr (units: cm^2/s^2)
  2. sss (units: psu)
  3. sst (units: degC)
  4. Ice Fraction (units: unitless)
  5. Dust Flux (units: g/cm^2/s)
  6. Iron Flux (units: nmol/cm^2/s)
  7. NOx Flux (units: nmol/cm^2/s)
  8. NHy Flux (units: nmol/cm^2/s)
  9. Atmospheric Pressure (units: atmospheres)
  10. xco2 (units: ppmv)
  11. xco2_alt_co2 (units: ppmv)

  ---------------------------------
  Requested interior forcing fields
  ---------------------------------

  1. Dust Flux (units: g/cm^2/s)
  2. Surface Shortwave (units: W/m^2)
  3. Potential Temperature (units: degC)
  4. Salinity (units: psu)
  5. Pressure (units: bars)
  6. Iron Sediment Flux (units: nmol/cm^2/s)

---------------------------------------------------------
Forcing Field Metadata Available from the MARBL Interface
---------------------------------------------------------

The details are found in ``$MARBL/tests/driver_src/marbl.F90``:

.. block comes from tests/driver_src/marbl.F90
.. code-block:: fortran

  ! Log requested surface forcing fields
  call driver_status_log%log_header('Requested surface forcing fields', subname)
  do n=1,size(marbl_instance%surface_flux_forcings)
    write(log_message, "(I0, 5A)") n, '. ', &
          trim(marbl_instance%surface_flux_forcings(n)%metadata%varname), &
          ' (units: ', trim(marbl_instance%surface_flux_forcings(n)%metadata%field_units),')'
    call driver_status_log%log_noerror(log_message, subname)
  end do
  ! Log requested interior forcing fields
  call driver_status_log%log_header('Requested interior forcing fields', subname)
  do n=1,size(marbl_instance%interior_tendency_forcings)
    write(log_message, "(I0, 5A)") n, '. ', &
         trim(marbl_instance%interior_tendency_forcings(n)%metadata%varname), &
         ' (units: ', trim(marbl_instance%interior_tendency_forcings(n)%metadata%field_units),')'
    call driver_status_log%log_noerror(log_message, subname)
  end do

The ``marbl_interface_class`` contains two objects (``surface_flux_forcings`` and ``interior_tendency_forcings``) that are arrays with dimension equal to the number of surface and interior forcing fields, respectively.
Both are of type ``marbl_forcing_fields_type``, which contains a ``metadata`` object.
The ``marbl_forcing_fields_metadata_type`` contains metadata for each tracer:

.. block comes from marbl_interface_public_types
.. code-block:: fortran

  type :: marbl_forcing_fields_metadata_type
     ! Contains variable names and units for required forcing fields as well as
     ! dimensional information; actual forcing data is in array of
     ! marbl_forcing_fields_type
     character(len=char_len) :: varname
     character(len=char_len) :: field_units
     integer                 :: rank            ! 0d or 1d
     integer,  allocatable   :: extent(:)       ! length = rank
  end type marbl_forcing_fields_metadata_type

The ``varnames`` member of this data type is the only unique identifier provided.

------------------------------------------------
Example: Accessing Forcing Field Metadata in POP
------------------------------------------------

Details are on the :ref:`implementation <pop_forcing>` page

