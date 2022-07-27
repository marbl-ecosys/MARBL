.. _processing_MARBL_output:

=======================
Processing MARBL Output
=======================

.. _ref-surface_flux_in_POP:

-------------------
Surface Flux Output
-------------------

After calling ``surface_flux_compute()``, the GCM needs to :ref:`manage all the MARBL output <ref-after-surface-flux-call>`.
The block of code below shows how POP accesses ``surface_flux_saved_state``, ``surface_flux_output``, ``surface_fluxes``, ``surface_flux_diags``, and ``glo_avg_fields_surface_flux``.
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

After calling ``interior_tendency_compute()``, the GCM needs to :ref:`manage all the MARBL output <ref-after-interior-tend-call>`.
The block of code below shows how POP accesses ``interior_tendency_saved_state``, ``interior_tendencies`` and ``glo_avg_fields_interior_tendency``.
Diagnostics are accumulated in ``ecosys_tavg_accumulate_interior()``, which is a wrapper to handle column-wise accumulation (POP typically accumulates level by level).
Note that POP aborts if any values in ``interior_tendencies`` are ``NaN``.

.. block comes from ecosys_driver in POP
.. code-block:: fortran

  do c = this_block%jb,this_block%je
     do i = this_block%ib,this_block%ie

        if (land_mask(i,c,bid)) then

           ! [snipped prestage calls]

           !-----------------------------------------------------------
           ! copy marbl column data back to slab
           !-----------------------------------------------------------

           call timer_start(ecosys_interior_marbl_to_pop, block_id=bid)

           do n=1,size(interior_tendency_saved_state)
             interior_tendency_saved_state(n)%field_3d(:,i,c,bid) =               &
               marbl_instances(bid)%interior_tendency_saved_state%state(n)%field_3d(:,1)
           end do

           !-----------------------------------------------------------
           ! before copying tendencies, check to see if any are NaNs
           !-----------------------------------------------------------

           do k = 1, KMT(i, c, bid)
              if (any(shr_infnan_isnan(marbl_instances(bid)%interior_tendencies(:, k)))) then
                 write(stdout, *) subname, ': NaN in dtracer_module, (i,j,k)=(', &
                    this_block%i_glob(i), ',', this_block%j_glob(c), ',', k, ')'
                 write(stdout, *) '(lon,lat)=(', TLOND(i,c,bid), ',', TLATD(i,c,bid), ')'
                 do n = 1, ecosys_tracer_cnt
                    write(stdout, *) trim(marbl_instances(1)%tracer_metadata(n)%short_name), ' ', &
                       marbl_instances(bid)%tracers(n, k), ' ', &
                       marbl_instances(bid)%interior_tendencies(n, k)
                 end do
                 do n = 1, size(interior_tendency_forcings)
                    associate (forcing_field => interior_tendency_forcings(n))
                       write(stdout, *) trim(forcing_field%metadata%marbl_varname)
                       if (forcing_field%rank == 2) then
                          write(stdout, *) forcing_field%field_0d(i,c,bid)
                       else
                          if (forcing_field%ldim3_is_depth) then
                             write(stdout, *) forcing_field%field_1d(i,c,k,bid)
                          else
                             write(stdout, *) forcing_field%field_1d(i,c,:,bid)
                          end if
                       end if
                    end associate
                 end do
                 call exit_POP(sigAbort, 'Stopping in ' // subname)
              end if
           end do

           do n = 1, ecosys_tracer_cnt
              dtracer_module(i, c, 1:KMT(i, c, bid), n) = marbl_instances(bid)%interior_tendencies(n, 1:KMT(i, c, bid))
           end do

           ! copy values to be used in computing requested global averages
           ! arrays have zero extent if none are requested
           glo_avg_fields_interior(i, c, bid, :) = marbl_instances(bid)%glo_avg_fields_interior_tendency(:)
           call timer_stop(ecosys_interior_marbl_to_pop, block_id=bid)

           !-----------------------------------------------------------
           ! Update pop tavg diags
           !-----------------------------------------------------------

           call timer_start(ecosys_interior_marbl_tavg, block_id=bid)

           call ecosys_tavg_accumulate_interior(i, c, marbl_instances(bid), bid)

           call timer_stop(ecosys_interior_marbl_tavg, block_id=bid)

        end if ! end if land_mask > 0

     end do ! do i
  end do ! do c
