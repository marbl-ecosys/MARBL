.. _pop_forcing:

=============================================
POP Interacting with MARBL Requested Forcings
=============================================

POP mirrors the MARBL datatypes for forcing fields and the associated metadata, but expands the metadata class to also manage the source of the data (read from a file, provided by POP, provided by the flux coupler, etc).
In the code below, ``surface_forcings(:)`` is the MARBL data provided through the interface, and ``surface_forcing_fields(:)`` is the copy into the POP datatype.

.. code-block:: fortran

    allocate(surface_forcing_fields(size(surface_forcings)))
    do n=1,size(surface_forcing_fields)
      marbl_varname = surface_forcings(n)%metadata%varname
      units         = surface_forcings(n)%metadata%field_units
      select case (trim(surface_forcings(n)%metadata%varname))
        case ('surface_mask')
          mask_ind = n
          call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                               marbl_varname=marbl_varname, field_units=units,      &
                               driver_varname='SURFACE_MASK', id=n)

        case ('d13c')
          d13c_ind = n
          call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                               marbl_varname=marbl_varname, field_units=units,      &
                               driver_varname='D13C', id=n)

        case ('d14c')
          d14c_ind = n
          call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                               marbl_varname=marbl_varname, field_units=units,      &
                               driver_varname='D14C', id=n)

        case ('d14c_gloavg')
          d14c_glo_ind = n
          call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                               marbl_varname=marbl_varname, field_units=units,      &
                               driver_varname='D14C_GLOAVG', id=n)

        case ('u10_sqr')
          u10sqr_ind = n
          call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                               marbl_varname=marbl_varname, field_units=units,      &
                               driver_varname='U10_SQR', id=n)

        case ('sst')
          sst_ind = n
          call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                               marbl_varname=marbl_varname, field_units=units,      &
                               driver_varname='SST', id=n)

        case ('sss')
          sss_ind = n
          call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                               marbl_varname=marbl_varname, field_units=units,      &
                               driver_varname='SSS', id=n)

        case ('xco2')
          xco2_ind = n
          if (trim(atm_co2_opt).eq.'const') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='const', &
                                 marbl_varname=marbl_varname, field_units=units,   &
                                 field_constant=atm_co2_const, id=n)
          else if (trim(atm_co2_opt).eq.'drv_prog') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='named_field', &
                                 marbl_varname=marbl_varname, field_units=units,         &
                                 named_field='ATM_CO2_PROG', id=n)
          else if (trim(atm_co2_opt).eq.'drv_diag') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='named_field', &
                                 marbl_varname=marbl_varname, field_units=units,         &
                                 named_field='ATM_CO2_DIAG', id=n)
          else
            write(err_msg, "(A,1X,A)") trim(atm_co2_opt),                     &
                 'is not a valid option for atm_co2_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('xco2_alt_co2')
          if (trim(atm_alt_co2_opt).eq.'const') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='const', &
                                 marbl_varname=marbl_varname, field_units=units,   &
                                 field_constant=atm_alt_co2_const, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(atm_alt_co2_opt),                 &
                 'is not a valid option for atm_alt_co2_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('Ice Fraction')
          ifrac_ind = n
          if (trim(gas_flux_forcing_opt).eq.'drv') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                                 marbl_varname=marbl_varname, field_units=units,      &
                                 driver_varname='ICE Fraction', id=n)
          else if (trim(gas_flux_forcing_opt).eq.'file') then
            file_details => fice_file_loc
            call init_monthly_surface_forcing_metadata(file_details)
            call surface_forcing_fields(n)%add_forcing_field(                    &
                                 field_source='POP monthly calendar',            &
                                 marbl_varname=marbl_varname, field_units=units, &
                                 forcing_calendar_name=file_details, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(gas_flux_forcing_opt),            &
                 'is not a valid option for gas_flux_forcing_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('Atmospheric Pressure')
          ap_ind = n
          if (trim(gas_flux_forcing_opt).eq.'drv') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                                 marbl_varname=marbl_varname, field_units=units,      &
                                 driver_varname='AP_FILE_INPUT', id=n)
          else if (trim(gas_flux_forcing_opt).eq.'file') then
            file_details => ap_file_loc
            call init_monthly_surface_forcing_metadata(file_details)
            call surface_forcing_fields(n)%add_forcing_field(                    &
                                 field_source='POP monthly calendar',            &
                                 marbl_varname=marbl_varname, field_units=units, &
                                 forcing_calendar_name=file_details, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(gas_flux_forcing_opt),            &
                 'is not a valid option for gas_flux_forcing_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('Dust Flux')
          dust_ind = n
          if (trim(dust_flux_source).eq.'driver') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                                 marbl_varname=marbl_varname, field_units=units,      &
                                 driver_varname='DUST_FLUX', id=n)
          else if (trim(dust_flux_source).eq.'monthly-calendar') then
            file_details => dust_flux_file_loc
            call init_monthly_surface_forcing_metadata(file_details)
            call surface_forcing_fields(n)%add_forcing_field(                    &
                                 field_source='POP monthly calendar',            &
                                 marbl_varname=marbl_varname, field_units=units, &
                                 forcing_calendar_name=file_details, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(dust_flux_source),                &
                 'is not a valid option for dust_flux_source'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('Iron Flux')
          if (trim(iron_flux_source).eq.'driver-derived') then
            bc_ind = n
            call surface_forcing_fields(n)%add_forcing_field(field_source='internal', &
                                 marbl_varname=marbl_varname, field_units=units,      &
                                 driver_varname='BLACK_CARBON_FLUX', id=n)
          else if (trim(iron_flux_source).eq.'monthly-calendar') then
            Fe_ind = n
            file_details => iron_flux_file_loc
            call init_monthly_surface_forcing_metadata(file_details)
            call surface_forcing_fields(n)%add_forcing_field(                    &
                                 field_source='POP monthly calendar',            &
                                 marbl_varname=marbl_varname, field_units=units, &
                                 forcing_calendar_name=file_details, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(iron_flux_source),                &
                 'is not a valid option for iron_flux_source'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('NOx Flux')
          nox_ind = n
          if (trim(ndep_data_type).eq.'shr_stream') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='shr_stream', &
                                 marbl_varname=marbl_varname, field_units=units,  &
                                 unit_conv_factor=ndep_shr_stream_scale_factor,   &
                                 file_varname='NOy_deposition',                   &
                                 year_first = ndep_shr_stream_year_first,         &
                                 year_last = ndep_shr_stream_year_last,           &
                                 year_align = ndep_shr_stream_year_align,         &
                                 filename = ndep_shr_stream_file, id=n)
          else if (trim(ndep_data_type).eq.'monthly-calendar') then
            file_details => nox_flux_monthly_file_loc
            call init_monthly_surface_forcing_metadata(file_details)
            call surface_forcing_fields(n)%add_forcing_field(                    &
                                 field_source='POP monthly calendar',            &
                                 marbl_varname=marbl_varname, field_units=units, &
                                 forcing_calendar_name=file_details, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(ndep_data_type),                  &
                 'is not a valid option for ndep_data_type'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('NHy Flux')
          nhy_ind = n
          if (trim(ndep_data_type).eq.'shr_stream') then
            call surface_forcing_fields(n)%add_forcing_field(field_source='shr_stream', &
                                 marbl_varname=marbl_varname, field_units=units,  &
                                 unit_conv_factor=ndep_shr_stream_scale_factor,   &
                                 file_varname='NHx_deposition',                   &
                                 year_first = ndep_shr_stream_year_first,         &
                                 year_last = ndep_shr_stream_year_last,           &
                                 year_align = ndep_shr_stream_year_align,         &
                                 filename = ndep_shr_stream_file, id=n)
          else if (trim(ndep_data_type).eq.'monthly-calendar') then
            file_details => nhy_flux_monthly_file_loc
            call init_monthly_surface_forcing_metadata(file_details)
            call surface_forcing_fields(n)%add_forcing_field(                    &
                                 field_source='POP monthly calendar',            &
                                 marbl_varname=marbl_varname, field_units=units, &
                                 forcing_calendar_name=file_details, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(ndep_data_type),                  &
                 'is not a valid option for ndep_data_type'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('DIN River Flux')
          file_details => din_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case ('DIP River Flux')
          file_details => dip_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case ('DON River Flux')
          file_details => don_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case ('DOP River Flux')
          file_details => dop_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case ('DSi River Flux')
          file_details => dsi_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case ('DFe River Flux')
          file_details => dfe_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case ('DIC River Flux')
          file_details => dic_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case ('ALK River Flux')
          file_details => alk_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case ('DOC River Flux')
          file_details => doc_riv_flux_file_loc
          call init_monthly_surface_forcing_metadata(file_details)
          call surface_forcing_fields(n)%add_forcing_field(                    &
                               field_source='POP monthly calendar',            &
                               marbl_varname=marbl_varname, field_units=units, &
                               forcing_calendar_name=file_details, id=n)

        case DEFAULT
          write(err_msg, "(A,1X,A)") trim(surface_forcings(n)%metadata%varname), &
                         'is not a valid surface forcing field name.'
          call document(subname, err_msg)
          call exit_POP(sigAbort, 'Stopping in ' // subname)
      end select

      ! All surface forcing fields are 0d; if a 1d field is introduced later,
      ! move this allocate into the select case
      allocate(surface_forcing_fields(n)%field_0d(nx_block, ny_block, nblocks_clinic))

      ! Zero out forcing field. If a 1d field is introduced later, check to see
      ! which of field_0d and field_1d is allocated.
      surface_forcing_fields(n)%field_0d = c0
    end do

Note that POP uses ``field_source`` to denote where it will be getting the forcing field.
Not shown in this example is where POP actually populates the data.
The code for interior forcing fields looks similar, although there are far fewer fields to handle and that results in a shorter code snippet.
Again, ``interior_forcings`` is provided through the MARBL interface and ``interior_forcing_fields`` is a POP construct.

.. code-block:: fortran

    allocate(interior_forcing_fields(size(interior_forcings)))

    do n=1,size(interior_forcing_fields)
      marbl_varname = interior_forcings(n)%metadata%varname
      units = interior_forcings(n)%metadata%field_units

      var_processed = .false.
      ! Check to see if this forcing field is tracer restoring
      if (index(marbl_varname,'Restoring Field').gt.0) then
        tracer_name = trim(marbl_varname(1:scan(marbl_varname,' ')))
        do m=1,marbl_tracer_cnt
          if (trim(tracer_name).eq.trim(restoreable_tracer_names(m))) then
            ! Check to make sure restore_data_filenames and
            ! restore_data_file_varnames have both been provided by namelist
            if (len_trim(restore_data_filenames(m)).eq.0) then
              write(err_msg, "(3A)") "No file provided to read restoring ",   &
                                     "field for ", trim(tracer_name)
              call document(subname, err_msg)
              call exit_POP(sigAbort, 'Stopping in ' // subname)
            end if
            if (len_trim(restore_data_file_varnames(m)).eq.0) then
              write(err_msg, "(3A)") "No variable name provided to read ",    &
                                     "restoring field for ", trim(tracer_name)
              call document(subname, err_msg)
              call exit_POP(sigAbort, 'Stopping in ' // subname)
            end if
            if (my_task.eq.master_task) then
              write(stdout, "(6A)") "Will restore ", trim(tracer_name),       &
                            " with ", trim(restore_data_file_varnames(m)),    &
                            " from ", trim(restore_data_filenames(m))
            end if
            call interior_forcing_fields(n)%add_forcing_field(                &
                       field_source='file_time_invariant',                    &
                       marbl_varname=marbl_varname, field_units=units,        &
                       filename=restore_data_filenames(m),                    &
                       file_varname=restore_data_file_varnames(m),            &
                       id=n)
            allocate(interior_forcing_fields(n)%field_1d(nx_block, ny_block, km, nblocks_clinic))
            var_processed = .true.
            exit
          end if
        end do
      end if

      ! Check to see if this forcing field is a restoring time scale
      if (index(marbl_varname,'Restoring Inverse Timescale').gt.0) then
        tracer_name = trim(marbl_varname(1:scan(marbl_varname,' ')))
        select case (trim(restore_inv_tau_opt))
          case('const')
            call interior_forcing_fields(n)%add_forcing_field(                &
                       field_source='const',                                  &
                       marbl_varname=marbl_varname, field_units=units,        &
                       field_constant = restore_inv_tau_const,                &
                       id=n)
          ! case('shr_stream')
          ! NOT SUPPORTED YET
          ! will require additional namelist variables, and we can consider
          ! reading in one file per tracer instead of using the same mask
          ! for all restoring fields
          case DEFAULT
            write(err_msg, "(A,1X,A)") trim(restore_inv_tau_opt),             &
                 'is not a valid option for restore_inv_tau_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
        end select
        allocate(interior_forcing_fields(n)%field_1d(nx_block, ny_block, km, nblocks_clinic))
        var_processed = .true.
      end if

      if (.not.var_processed) then
        select case (trim(interior_forcings(n)%metadata%varname))
          case ('Dust Flux')
            dustflux_ind = n
            call interior_forcing_fields(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='dust_flux', id=n)
            allocate(interior_forcing_fields(n)%field_0d(nx_block, ny_block, nblocks_clinic))
          case ('PAR Column Fraction')
            PAR_col_frac_ind = n
            call interior_forcing_fields(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='PAR_col_frac', id=n)
            allocate(interior_forcing_fields(n)%field_1d(nx_block, ny_block, mcog_nbins, nblocks_clinic))
          case ('Surface Shortwave')
            surf_shortwave_ind = n
            call interior_forcing_fields(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='surf_shortwave', id=n)
            allocate(interior_forcing_fields(n)%field_1d(nx_block, ny_block, mcog_nbins, nblocks_clinic))
          case ('Temperature')
            temperature_ind = n
            call interior_forcing_fields(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='temperature', id=n)
            allocate(interior_forcing_fields(n)%field_1d(nx_block, ny_block, km, nblocks_clinic))
          case ('Salinity')
            salinity_ind = n
            call interior_forcing_fields(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='salinity', id=n)
            allocate(interior_forcing_fields(n)%field_1d(nx_block, ny_block, km, nblocks_clinic))
          case ('Pressure')
            pressure_ind = n
            call interior_forcing_fields(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='pressure', id=n)
            allocate(interior_forcing_fields(n)%field_1d(nx_block, ny_block, km, nblocks_clinic))
          case ('Iron Sediment Flux')
            fesedflux_ind = n
            call interior_forcing_fields(n)%add_forcing_field(                &
                          field_source='file_time_invariant',                 &
                          marbl_varname=marbl_varname, field_units=units,     &
                          filename=fesedflux_input%filename,                  &
                          file_varname=fesedflux_input%file_varname,          &
                          id=n)
            allocate(interior_forcing_fields(n)%field_1d(nx_block, ny_block, km, nblocks_clinic))
          case DEFAULT
            write(err_msg, "(A,1X,A)") trim(interior_forcings(n)%metadata%varname), &
                           'is not a valid interior forcing field name.'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
        end select
      end if

      ! Zero out field
      if (allocated(interior_forcing_fields(n)%field_0d)) then
        interior_forcing_fields(n)%field_0d = c0
      else
        interior_forcing_fields(n)%field_1d = c0
      end if
    end do
