.. _forcing_fields:

========================================
What Forcing Fields has MARBL Requested?
========================================

This similar to the question :ref:`tracer_state`, and so it should not be a surprise that the answer is also very similar.
MARBL provides a stand-alone test in ``$MARBL/tests/regression_tests/requested_forcing`` as an example of how the MARBL library passes information to the GCM.

::

  $ ./requested_forcings.py

Provides a list of the forcing fields requested with the default MARBL configuration.

::

  --------------------------------
  Requested surface forcing fields
  --------------------------------

  1. u10_sqr
  2. sss
  3. sst
  4. Ice Fraction
  5. Dust Flux
  6. Iron Flux
  7. NOx Flux
  8. NHy Flux
  9. Atmospheric Pressure
  10. xco2
  11. xco2_alt_co2

  ---------------------------------
  Requested interior forcing fields
  ---------------------------------

  1. Dust Flux
  2. Surface Shortwave
  3. Potential Temperature
  4. Salinity
  5. Pressure
  6. Iron Sediment Flux


---------------------------------------------------------
Forcing Field Metadata Available from the MARBL Interface
---------------------------------------------------------

The details are found in ``$MARBL/tests/driver_src/marbl.F90``:

.. code-block:: fortran

  call driver_status_log%log_header('Requested surface forcing fields', subname)
  do n=1,size(marbl_instance%surface_input_forcings)
    write(log_message, "(I0, 2A)") n, '. ', &
          trim(marbl_instance%surface_input_forcings(n)%metadata%varname)
    call driver_status_log%log_noerror(log_message, subname)
  end do
  ! Log requested interior forcing fields
  call driver_status_log%log_header('Requested interior forcing fields', subname)
  do n=1,size(marbl_instance%interior_input_forcings)
    write(log_message, "(I0, 2A)") n, '. ',                               &
         trim(marbl_instance%interior_input_forcings(n)%metadata%varname)
    call driver_status_log%log_noerror(log_message, subname)
  end do

The ``marbl_interface_class`` contains two objects (``surface_input_forcings`` and ``interior_input_forcings``) that are arrays with dimension equal to the number of surface and interior forcing fields, respectively.
Both are of type ``marbl_forcing_fields_type``, and contain the ``metadata`` object.
The ``marbl_forcing_fields_metadata_type`` contains metadata for each tracer:

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

The `varnames` member of this data type is the only unique identifier provided.

------------------------------------------------
Example: Accessing Forcing Field Metadata in POP
------------------------------------------------

Details are on the :ref:`implementation <pop_forcing>` page

