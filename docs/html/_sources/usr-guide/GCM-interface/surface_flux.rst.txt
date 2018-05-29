.. _surface_flux:

.. _ref-compute-surface-fluxes:

======================
Compute Surface Fluxes
======================

``set_surface_forcing()`` computes surface fluxes (and related diagnostics) over a 1D array of grid cells.
The array is assumed to be length ``num_surface_forcing_elements``, per :ref:`ref-init-interface`.
The stand-alone test suite does not yet call this routine, so examples come from the POP driver.
The call to the routine is straightforward:

.. code-block:: fortran

       call marbl_instances(iblock)%set_surface_forcing()

The details are in the surrounding calls.

-----------------------------------------------------
What MARBL needs prior to calling set_surface_forcing
-----------------------------------------------------

The GCM needs to make sure the MARBL instance has all the data it needs to compute surface fluxes correctly.
Specifically, it needs to do the following.

~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Set global scalars
~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, there are no global scalars that need to be set for this stage.
To prepare for future updates where that is no longer the case, it is recommended that the GCM calls

.. code-block:: fortran

  call marbl_instances(iblock)%set_global_scalars('surface')

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Copy data into MARBL
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In POP, 2D ``(nx_block, ny_block)`` data is reshaped into an array of length ``num_surface_forcing_elements = nx_block*ny_block``.
Data for ``surface_input_forcings``, ``surface_vals`` (tracer values at the surface), and saved state (MARBL data from previous time step) are all copied in.

.. code-block:: fortran

       !-----------------------------------------------------------------------
       ! Copy data from slab data structure to column input for marbl
       !-----------------------------------------------------------------------

       do j = 1, ny_block
          do i = 1,nx_block
             index_marbl = i + (j-1)*nx_block

             do n = 1,size(surface_forcing_fields)
                marbl_instances(iblock)%surface_input_forcings(n)%field_0d(index_marbl) = &
                     surface_forcing_fields(n)%field_0d(i,j,iblock)
             end do

             do n = 1,ecosys_tracer_cnt
                marbl_instances(iblock)%surface_vals(index_marbl,n) = &
                     p5*(surface_vals_old(i,j,n,iblock) + surface_vals_cur(i,j,n,iblock))

             end do

             do n=1,size(saved_state_surf)
               marbl_instances(iblock)%surface_saved_state%state(n)%field_2d(index_marbl) = &
                 saved_state_surf(n)%field_2d(i,j,iblock)
             end do

          end do
       end do

--------------------------------------
What the GCM needs after MARBL returns
--------------------------------------

MARBL returns surface fluxes (and any requested surface forcing fields) for all the columns, and they need to be stored in the GCM.
Additionally, saved state needs to be saved so it is available in the next time step and any fields that are globally averaged also need to be stored.
Lastly, MARBL will return values for fields that need to be globally averaged.

.. code-block:: fortran

  !-----------------------------------------------------------------------
  ! Copy data from marbl output column to pop slab data structure
  !-----------------------------------------------------------------------

  do index_marbl = 1, marbl_col_cnt(iblock)
     i = marbl_col_to_pop_i(index_marbl,iblock)
     j = marbl_col_to_pop_j(index_marbl,iblock)

     do n=1,size(saved_state_surf)
       saved_state_surf(n)%field_2d(i,j,iblock) = &
         marbl_instances(iblock)%surface_saved_state%state(n)%field_2d(index_marbl)
     end do

     do n=1,sfo_cnt
       surface_forcing_outputs(i,j,n,iblock) = &
          marbl_instances(iblock)%surface_forcing_output%sfo(n)%forcing_field(index_marbl)
     end do

    !-----------------------------------------------------------
    ! before copying surface fluxes, check to see if any are NaNs
    !-----------------------------------------------------------

    if (any(shr_infnan_isnan(marbl_instances(iblock)%surface_tracer_fluxes(index_marbl,:)))) then
      write(stdout, *) subname, ': NaN in stf_module, (i,j)=(', &
         this_block%i_glob(i), ',', this_block%j_glob(j), ')'
      write(stdout, *) '(lon,lat)=(', TLOND(i,j,iblock), ',', TLATD(i,j,iblock), ')'
      do n = 1, ecosys_tracer_cnt
         write(stdout, *) trim(marbl_instances(1)%tracer_metadata(n)%short_name), ' ', &
            marbl_instances(iblock)%surface_vals(index_marbl,n), ' ', &
            marbl_instances(iblock)%surface_tracer_fluxes(index_marbl,n)
      end do
      do n = 1, size(surface_forcing_fields)
         associate (forcing_field => surface_forcing_fields(n))
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
           marbl_instances(iblock)%surface_tracer_fluxes(index_marbl,n)
    end do

    do n=1,size(marbl_instances(1)%surface_forcing_diags%diags)
      surface_forcing_diags(i,j,n,iblock) = &
           marbl_instances(iblock)%surface_forcing_diags%diags(n)%field_2d(index_marbl)
    end do

    ! copy values to be used in computing requested global averages
    ! arrays have zero extent if none are requested
    glo_avg_fields_surface(i,j,iblock,:) = marbl_instances(iblock)%glo_avg_fields_surface(index_marbl,:)
  end do
