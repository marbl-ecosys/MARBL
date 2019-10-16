.. _processing_MARBL_output:

=======================
Processing MARBL Output
=======================

.. _ref-surface_flux_in_POP:

-------------------
Surface Flux Output
-------------------

After calling ``surface_flux_compute()``, the GCM needs to :ref:`manage all the MARBL output <ref-after-surface-flux-call>`.
The block of code shows how POP accesses ``surface_flux_saved_state``, ``surface_flux_output``, ``surface_fluxes``, ``surface_flux_diags``, and ``glo_avg_fields_surface_flux``.
Note that POP aborts if any values in ``surface_fluxes`` are ``NaN``.

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

.. _ref-interior_tend_in_POP:

------------------------
Interior Tendency Output
------------------------

.. block comes from ecosys_driver in POP
.. code-block:: fortran

  -- remove --
