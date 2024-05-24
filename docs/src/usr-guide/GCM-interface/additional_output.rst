.. _additional_output:

=================
Additional Output
=================

MARBL can provide more output than just surface fluxes and interior tendencies.
For example, MARBL can return the total chlorophyll concentration to the GCM,
which can then be used to compute shortwave absorption.
(MARBL can provide both surface chlorophyll and the full depth chlorophyll field,
since difference shortwave absorption algorithms have different inputs.)
MARBL can also provide the O2, CO2, and NHx fluxes at the surface if the GCM wants those to feedback to the atmosphere.

----------------
Available Fields
----------------

The full list of outputs is available from the ``available_output`` regression test.
In the default configuration, the complete list is

.. block comes from available_output test
.. code-block:: none

  ------------------------
  Available output for GCM
  ------------------------

  1. Total Chlorophyll Concentration
    short name: total_Chl
    units: mg/m^3
    field_source: interior_tendency
  2. Total Surface Chlorophyll Concentration
    short name: total_surfChl
    units: mg/m^3
    field_source: surface_flux
  3. NHx Surface Emissions
    short name: flux_nhx
    units: nmol/cm^2/s
    field_source: surface_flux
  4. Carbon Dioxide Flux
    short name: flux_co2
    units: nmol/cm^2/s
    field_source: surface_flux
  5. Oxygen Flux
    short name: flux_o2
    units: nmol/cm^2/s
    field_source: surface_flux

-----------------
Requesting Output
-----------------

By default, MARBL will not return any of the additional output.
To request one or more of these fields,
use the ``add_output_for_GCM()`` function on the MARBL interface:

.. block comes from marbl_interface
.. code-block:: fortran

  subroutine add_output_for_GCM(this, num_elements, field_name, output_id, field_source, num_levels)
    ! Check the registry to see if field_name is provided from surface_flux_compute() or interior_tendency_compute()
    ! add it to the proper output_for_GCM type, or log a useful error message

    use marbl_interface_public_types, only : marbl_output_for_GCM_linked_list_type

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),     intent(in)    :: field_name
    integer(int_kind),    intent(in)    :: num_elements
    integer(int_kind),    intent(out)   :: output_id
    character(len=*),     intent(out)   :: field_source
    integer(int_kind),    optional, intent(in) :: num_levels

The output field ``field_source`` will be  ``interior_tendency_output`` or ``surface_flux_output``
component of the MARBL interface type.
The following block of code from the stand-alone driver shows how to request both surface and total chlorophyll.
Note that the driver tracks how many outputs are coming from ``field_source = "surface_flux"``
and how many come from ``field_source = "interior_tendency"``.

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

    if (base_bio_on) then
      do n=1, size(marbl_instances)
        call marbl_instances(n)%add_output_for_GCM(num_elements=col_cnt(n), &
                                                  field_name="total_surfChl", &
                                                  output_id=total_surfChl_id, &
                                                  field_source=field_source)
        if (marbl_instances(n)%StatusLog%labort_marbl) then
          call marbl_instances(n)%StatusLog%log_error_trace('marbl%add_output_for_GCM(total_surfChl)', subname)
          return
        end if
      end do
      if (trim(field_source) == "surface_flux") then
        sfo_cnt = sfo_cnt+1
      else if (trim(field_source) == "interior_tendency") then
        ito_cnt = ito_cnt+1
      else
        write(log_message, "(3A)") "'", trim(field_source), "' is not a recognized field source (total_surfChl)"
        call driver_status_log%log_error(log_message, subname)
        return
      end if
    end if

    if (base_bio_on) then
      do n=1, size(marbl_instances)
        call marbl_instances(n)%add_output_for_GCM(num_elements=col_cnt(n), &
                                                  field_name="total_Chl", &
                                                  output_id=total_Chl_id, &
                                                  num_levels=num_levels, &
                                                  field_source=field_source)
        if (marbl_instances(n)%StatusLog%labort_marbl) then
          call marbl_instances(n)%StatusLog%log_error_trace('marbl%add_output_for_GCM(total_Chl)', subname)
          return
        end if
      end do
      if (trim(field_source) == "surface_flux") then
        sfo_cnt = sfo_cnt+1
      else if (trim(field_source) == "interior_tendency") then
        ito_cnt = ito_cnt+1
      else
        write(log_message, "(3A)") "'", trim(field_source), "' is not a recognized field source (total_Chl)"
        call driver_status_log%log_error(log_message, subname)
        return
      end if
    end if
