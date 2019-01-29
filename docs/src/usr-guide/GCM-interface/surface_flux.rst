.. _surface_flux:

.. _ref-compute-surface-fluxes:

======================
Compute Surface Fluxes
======================

``surface_flux_compute()`` computes surface fluxes (and related diagnostics) over a 1D array of grid cells.
The array is assumed to be length ``num_elements_surface_flux``, per :ref:`ref-init-interface`.
The stand-alone test suite does not yet call this routine, so examples come from the POP driver.
The call to the routine is straightforward:

.. block comes from ecosys_driver in POP
.. code-block:: fortran

       call marbl_instances(iblock)%surface_flux_compute()

The details are in the surrounding calls.

------------------------------------------------------
What MARBL needs prior to calling surface_flux_compute
------------------------------------------------------

The GCM needs to make sure the MARBL instance has all the data it needs to compute surface fluxes correctly.
Specifically, it needs to do the following.

~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Set global scalars
~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, there are no global scalars that need to be set for this stage.
To prepare for future updates where that is no longer the case, it is recommended that the GCM calls

.. block comes from made-up example
.. code-block:: fortran

  call marbl_instance%set_global_scalars('surface_flux')

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Copy data into MARBL
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In POP, 2D ``(nx_block, ny_block)`` data is reshaped into an array of length ``num_elements_surface_flux = nx_block*ny_block``.
Data for ``surface_input_forcings``, ``surface_vals`` (tracer values at the surface), and saved state (MARBL data from previous time step) are all copied in.

.. block comes from ecosys_driver in POP
.. code-block:: fortran

  !-----------------------------------------------------------------------
  ! Copy data from slab data structure to column input for marbl
  !-----------------------------------------------------------------------

  do index_marbl = 1, marbl_col_cnt(iblock)
     i = marbl_col_to_pop_i(index_marbl,iblock)
     j = marbl_col_to_pop_j(index_marbl,iblock)

     do n = 1,size(surface_flux_forcings)
        marbl_instances(iblock)%surface_flux_forcings(n)%field_0d(index_marbl) = &
             surface_flux_forcings(n)%field_0d(i,j,iblock)
     end do

     do n = 1,ecosys_tracer_cnt
        marbl_instances(iblock)%tracers_at_surface(index_marbl,n) = &
             p5*(tracers_at_surface_old(i,j,n) + tracers_at_surface_cur(i,j,n))
     end do

     do n=1,size(surface_flux_saved_state)
       marbl_instances(iblock)%surface_flux_saved_state%state(n)%field_2d(index_marbl) = &
         surface_flux_saved_state(n)%field_2d(i,j,iblock)
     end do

  end do

--------------------------------------
What the GCM needs after MARBL returns
--------------------------------------

MARBL returns surface fluxes (and any requested surface forcing fields) for all the columns, and they need to be stored in the GCM.
Additionally, saved state needs to be saved so it is available in the next time step and any fields that are globally averaged also need to be stored.
Lastly, MARBL will return values for fields that need to be globally averaged.

.. block comes from ecosys_driver in POP
.. code-block:: fortran

  !-----------------------------------------------------------------------
  ! Copy data from marbl output column to pop slab data structure
  !-----------------------------------------------------------------------

  do index_marbl = 1, marbl_col_cnt(iblock)
     i = marbl_col_to_pop_i(index_marbl,iblock)
     j = marbl_col_to_pop_j(index_marbl,iblock)

     do n=1,size(surface_flux_saved_state)
       surface_flux_saved_state(n)%field_2d(i,j,iblock) = &
         marbl_instances(iblock)%surface_flux_saved_state%state(n)%field_2d(index_marbl)
     end do

     do n=1,sfo_cnt
       surface_flux_outputs(i,j,iblock,n) = &
          marbl_instances(iblock)%surface_flux_output%sfo(n)%forcing_field(index_marbl)
     end do

     !-----------------------------------------------------------
     ! before copying surface fluxes, check to see if any are NaNs
     !-----------------------------------------------------------

     if (any(shr_infnan_isnan(marbl_instances(iblock)%surface_fluxes(index_marbl,:)))) then
        write(stdout, *) subname, ': NaN in stf_module, (i,j)=(', &
           this_block%i_glob(i), ',', this_block%j_glob(j), ')'
        write(stdout, *) '(lon,lat)=(', TLOND(i,j,iblock), ',', TLATD(i,j,iblock), ')'
        do n = 1, ecosys_tracer_cnt
           write(stdout, *) trim(marbl_instances(1)%tracer_metadata(n)%short_name), ' ', &
              marbl_instances(iblock)%tracers_at_surface(index_marbl,n), ' ', &
              marbl_instances(iblock)%surface_fluxes(index_marbl,n)
        end do
        do n = 1, size(surface_flux_forcings)
           associate (forcing_field => surface_flux_forcings(n))
              write(stdout, *) trim(forcing_field%metadata%marbl_varname)
              if (forcing_field%rank == 2) then
                 write(stdout, *) forcing_field%field_0d(i,j,iblock)
              else
                 write(stdout, *) forcing_field%field_1d(i,j,:,iblock)
              end if
           end associate
        end do
        call exit_POP(sigAbort, 'Stopping in ' // subname)
     end if

     do n = 1,ecosys_tracer_cnt
        stf_module(i,j,n) = &
             marbl_instances(iblock)%surface_fluxes(index_marbl,n)
     end do

     do n=1,size(marbl_instances(1)%surface_flux_diags%diags)
        surface_flux_diags(i,j,n,iblock) = &
             marbl_instances(iblock)%surface_flux_diags%diags(n)%field_2d(index_marbl)
     end do

     ! copy values to be used in computing requested global averages
     ! arrays have zero extent if none are requested
     glo_avg_fields_surface(i,j,iblock,:) = marbl_instances(iblock)%glo_avg_fields_surface_flux(index_marbl,:)
  end do
