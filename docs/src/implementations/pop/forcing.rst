.. _pop_forcing:

=============================================
POP Interacting with MARBL Requested Forcings
=============================================

POP mirrors the MARBL datatypes for forcing fields and the associated metadata, but expands the metadata class to also manage the source of the data (read from a file, provided by POP, provided by the flux coupler, etc).
In the code below, ``marbl_req_surface_flux_forcings(:)`` is the MARBL data provided through the interface, and ``surface_flux_forcings(:)`` is the copy into the POP datatype.

.. block comes from ecosys_forcing_mod
.. code-block:: fortran

    do n=1,size(surface_flux_forcings)
      marbl_varname = marbl_req_surface_flux_forcings(n)%metadata%varname
      units         = marbl_req_surface_flux_forcings(n)%metadata%field_units
      select case (trim(marbl_req_surface_flux_forcings(n)%metadata%varname))
        case ('d13c')
          d13c_ind = n
          call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
               marbl_varname=marbl_varname, field_units=units,                           &
               driver_varname='d13C', rank=2, id=n)

        case ('d14c')
          d14c_ind = n
          call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
               marbl_varname=marbl_varname, field_units=units,                           &
               driver_varname='D14C', rank=2, id=n)

        case ('u10_sqr')
          u10sqr_ind = n
          call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
               marbl_varname=marbl_varname, field_units=units,                           &
               driver_varname='U10_SQR', rank=2, id=n)

        case ('sst')
          sst_ind = n
          call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
               marbl_varname=marbl_varname, field_units=units,                           &
               driver_varname='SST', rank=2, id=n)

        case ('sss')
          sss_ind = n
          call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
               marbl_varname=marbl_varname, field_units=units,                           &
               driver_varname='SSS', rank=2, id=n)

        case ('xco2')
          if (trim(atm_co2_opt).eq.'const') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='const', &
                 marbl_varname=marbl_varname, field_units=units,                        &
                 field_constant=atm_co2_const, rank=2, id=n)
          else if (trim(atm_co2_opt).eq.'drv_prog') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='named_field', &
                 marbl_varname=marbl_varname, field_units=units,                              &
                 named_field='ATM_CO2_PROG', rank=2, id=n)
          else if (trim(atm_co2_opt).eq.'drv_diag') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='named_field', &
                 marbl_varname=marbl_varname, field_units=units,                              &
                 named_field='ATM_CO2_DIAG', rank=2, id=n)
          else if (trim(atm_co2_opt).eq.'box_atm_co2') then
            box_atm_co2_ind = n
            call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
                 marbl_varname=marbl_varname, field_units=units,                           &
                 driver_varname='box_atm_co2', rank=2, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(atm_co2_opt),                     &
                 'is not a valid option for atm_co2_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('xco2_alt_co2')
          if (trim(atm_alt_co2_opt).eq.'const') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='const', &
                 marbl_varname=marbl_varname, field_units=units,                        &
                 field_constant=atm_alt_co2_const, rank=2, id=n)
          else if (trim(atm_alt_co2_opt).eq.'box_atm_co2') then
            if (trim(atm_co2_opt).eq.'box_atm_co2') then
              box_atm_co2_dup_ind = n
            else
              box_atm_co2_ind = n
            end if
            call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
                 marbl_varname=marbl_varname, field_units=units,                           &
                 driver_varname='box_atm_co2', rank=2, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(atm_alt_co2_opt),                 &
                 'is not a valid option for atm_alt_co2_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('Ice Fraction')
          ifrac_ind = n
          if (trim(gas_flux_forcing_opt).eq.'drv') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
                 marbl_varname=marbl_varname, field_units=units,                           &
                 driver_varname='ICE Fraction', rank=2, id=n)
          else if (trim(gas_flux_forcing_opt).eq.'file') then
            file_details => fice_file_loc
            call init_monthly_surface_flux_forcing_metadata(file_details)
            call surface_flux_forcings(n)%add_forcing_field(                    &
                 field_source='POP monthly calendar',                                 &
                 marbl_varname=marbl_varname, field_units=units,                      &
                 forcing_calendar_name=file_details, rank=2, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(gas_flux_forcing_opt),            &
                 'is not a valid option for gas_flux_forcing_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('Atmospheric Pressure')
          ap_ind = n
          if (trim(gas_flux_forcing_opt).eq.'drv') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
                 marbl_varname=marbl_varname, field_units=units,                           &
                 driver_varname='AP_FILE_INPUT', rank=2, id=n)
          else if (trim(gas_flux_forcing_opt).eq.'file') then
            file_details => ap_file_loc
            call init_monthly_surface_flux_forcing_metadata(file_details)
            call surface_flux_forcings(n)%add_forcing_field(                    &
                 field_source='POP monthly calendar',                                 &
                 marbl_varname=marbl_varname, field_units=units,                      &
                                 forcing_calendar_name=file_details, rank=2, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(gas_flux_forcing_opt),            &
                 'is not a valid option for gas_flux_forcing_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('Dust Flux')
          dust_dep_ind = n
          if (trim(dust_flux_source).eq.'driver') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
                 marbl_varname=marbl_varname, field_units=units,                           &
                 driver_varname='DUST_FLUX', rank=2, id=n)
          else if (trim(dust_flux_source).eq.'monthly-calendar') then
            file_details => dust_flux_file_loc
            call init_monthly_surface_flux_forcing_metadata(file_details)
            call surface_flux_forcings(n)%add_forcing_field(                    &
                 field_source='POP monthly calendar',                                 &
                 marbl_varname=marbl_varname, field_units=units,                      &
                 forcing_calendar_name=file_details, rank=2, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(dust_flux_source),                &
                 'is not a valid option for dust_flux_source'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('Iron Flux')
          if (trim(iron_flux_source).eq.'driver-derived') then
            bc_dep_ind = n
            call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
                 marbl_varname=marbl_varname, field_units=units,                           &
                 driver_varname='BLACK_CARBON_FLUX', rank=2, id=n)
          else if (trim(iron_flux_source).eq.'monthly-calendar') then
            Fe_dep_ind = n
            file_details => iron_flux_file_loc
            call init_monthly_surface_flux_forcing_metadata(file_details)
            call surface_flux_forcings(n)%add_forcing_field(                    &
                 field_source='POP monthly calendar',                                 &
                 marbl_varname=marbl_varname, field_units=units,                      &
                 forcing_calendar_name=file_details, rank=2, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(iron_flux_source),                &
                 'is not a valid option for iron_flux_source'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('NOx Flux')
          if (trim(ndep_data_type).eq.'shr_stream') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='shr_stream', &
                 strdata_inputlist_ptr=surface_strdata_inputlist_ptr,                        &
                 marbl_varname=marbl_varname, field_units=units,                             &
                 unit_conv_factor=ndep_shr_stream_scale_factor,                              &
                 file_varname='NOy_deposition',                                              &
                 year_first = ndep_shr_stream_year_first,                                    &
                 year_last = ndep_shr_stream_year_last,                                      &
                 year_align = ndep_shr_stream_year_align,                                    &
                 filename = ndep_shr_stream_file,                                            &
                 rank = 2, id = n)
          else if (trim(ndep_data_type).eq.'monthly-calendar') then
            file_details => nox_flux_monthly_file_loc
            call init_monthly_surface_flux_forcing_metadata(file_details)
            call surface_flux_forcings(n)%add_forcing_field(                    &
                 field_source='POP monthly calendar',                                 &
                 marbl_varname=marbl_varname, field_units=units,                      &
                 forcing_calendar_name=file_details, rank=2, id=n)
          else if (trim(ndep_data_type).eq.'driver') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='named_field', &
                 marbl_varname=marbl_varname, field_units=units,                              &
                 named_field='ATM_NOy', rank=2, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(ndep_data_type),                  &
                 'is not a valid option for ndep_data_type'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('NHy Flux')
          if (trim(ndep_data_type).eq.'shr_stream') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='shr_stream', &
                 strdata_inputlist_ptr=surface_strdata_inputlist_ptr,                        &
                 marbl_varname=marbl_varname, field_units=units,                             &
                 unit_conv_factor=ndep_shr_stream_scale_factor,                              &
                 file_varname='NHx_deposition',                                              &
                 year_first = ndep_shr_stream_year_first,                                    &
                 year_last = ndep_shr_stream_year_last,                                      &
                 year_align = ndep_shr_stream_year_align,                                    &
                 filename = ndep_shr_stream_file,                                            &
                 rank = 2, id = n)
          else if (trim(ndep_data_type).eq.'monthly-calendar') then
            file_details => nhy_flux_monthly_file_loc
            call init_monthly_surface_flux_forcing_metadata(file_details)
            call surface_flux_forcings(n)%add_forcing_field(                    &
                 field_source='POP monthly calendar',                                 &
                 marbl_varname=marbl_varname, field_units=units,                      &
                 forcing_calendar_name=file_details, rank=2, id=n)
          else if (trim(ndep_data_type).eq.'driver') then
            call surface_flux_forcings(n)%add_forcing_field(field_source='named_field', &
                 marbl_varname=marbl_varname, field_units=units,                              &
                 named_field='ATM_NHx', rank=2, id=n)
          else
            write(err_msg, "(A,1X,A)") trim(ndep_data_type),                  &
                 'is not a valid option for ndep_data_type'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
          end if

        case ('external C Flux')
          ext_C_flux_ind = n
          call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
               marbl_varname=marbl_varname, field_units=units,                           &
               driver_varname='ext_C_flux', rank=2, id=n)

        case ('external P Flux')
          ext_P_flux_ind = n
          call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
               marbl_varname=marbl_varname, field_units=units,                           &
               driver_varname='ext_P_flux', rank=2, id=n)

        case ('external Si Flux')
          ext_Si_flux_ind = n
          call surface_flux_forcings(n)%add_forcing_field(field_source='internal', &
               marbl_varname=marbl_varname, field_units=units,                           &
               driver_varname='ext_Si_flux', rank=2, id=n)

        case DEFAULT
          write(err_msg, "(A,1X,A)") trim(marbl_req_surface_flux_forcings(n)%metadata%varname), &
                         'is not a valid surface flux forcing field name.'
          call document(subname, err_msg)
          call exit_POP(sigAbort, 'Stopping in ' // subname)
      end select
    end do

Note that POP uses ``field_source`` to denote where it will be getting the forcing field.
Not shown in this example is where POP actually populates the data.
The code for interior forcing fields looks similar, although there are far fewer fields to handle and that results in a shorter code snippet.
Again, ``marbl_req_interior_tendency_forcings`` is provided through the MARBL interface and ``interior_tendency_forcings`` is a POP construct.

.. block comes from ecosys_forcing_mod
.. code-block:: fortran

    do n=1,size(interior_tendency_forcings)
      marbl_varname = marbl_req_interior_tendency_forcings(n)%metadata%varname
      units = marbl_req_interior_tendency_forcings(n)%metadata%field_units

      var_processed = .false.
      ! Check to see if this forcing field is tracer restoring
      if (index(marbl_varname,'Restoring Field').gt.0) then
        tracer_name = trim(marbl_varname(1:scan(marbl_varname,' ')))
        do m=1,marbl_tracer_cnt
          if (trim(tracer_name).eq.trim(restorable_tracer_names(m))) then
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
            call interior_tendency_forcings(n)%add_forcing_field(                &
                       field_source='shr_stream',                             &
                       strdata_inputlist_ptr=interior_strdata_inputlist_ptr,  &
                       marbl_varname=marbl_varname, field_units=units,        &
                       filename=restore_data_filenames(m),                    &
                       file_varname=restore_data_file_varnames(m),            &
                       year_first=restore_year_first(m),                      &
                       year_last=restore_year_last(m),                        &
                       year_align=restore_year_align(m),                      &
                       unit_conv_factor=restore_scale_factor(m),              &
                       rank=3, dim3_len=km, id=n)
            var_processed = .true.
            exit
          end if
        end do
      end if

      ! Check to see if this forcing field is a restoring time scale
      if (index(marbl_varname,'Restoring Inverse Timescale').gt.0) then
        select case (trim(restore_inv_tau_opt))
          case('const')
            call interior_tendency_forcings(n)%add_forcing_field(                &
                       field_source='const',                                  &
                       marbl_varname=marbl_varname, field_units=units,        &
                       field_constant=restore_inv_tau_const,                  &
                       rank=3, dim3_len=km, id=n)
          case('file_time_invariant')
            call interior_tendency_forcings(n)%add_forcing_field(                &
                       field_source='file_time_invariant',                    &
                       marbl_varname=marbl_varname, field_units=units,        &
                       filename=restore_inv_tau_input%filename,               &
                       file_varname=restore_inv_tau_input%file_varname,       &
                       unit_conv_factor=restore_inv_tau_input%scale_factor,   &
                       rank=3, dim3_len=km, id=n)
          case DEFAULT
            write(err_msg, "(A,1X,A)") trim(restore_inv_tau_opt),             &
                 'is not a valid option for restore_inv_tau_opt'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
        end select
        var_processed = .true.
      end if

      if (.not.var_processed) then
        select case (trim(marbl_req_interior_tendency_forcings(n)%metadata%varname))
          case ('Dust Flux')
            dustflux_ind = n
            call interior_tendency_forcings(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='dust_flux', rank=2, id=n)
          case ('PAR Column Fraction')
            PAR_col_frac_ind = n
            call interior_tendency_forcings(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='PAR_col_frac', rank=3, dim3_len=mcog_nbins,  &
                          ldim3_is_depth=.false., id=n)
          case ('Surface Shortwave')
            surf_shortwave_ind = n
            call interior_tendency_forcings(n)%add_forcing_field(field_source='internal',  &
                          marbl_varname=marbl_varname, field_units=units,               &
                          driver_varname='surf_shortwave', rank=3, dim3_len=mcog_nbins, &
                          ldim3_is_depth=.false., id=n)
          case ('Potential Temperature')
            potemp_ind = n
            call interior_tendency_forcings(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='temperature', rank=3, dim3_len=km, id=n)
          case ('Salinity')
            salinity_ind = n
            call interior_tendency_forcings(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='salinity', rank=3, dim3_len=km, id=n)
          case ('Pressure')
            pressure_ind = n
            call interior_tendency_forcings(n)%add_forcing_field(field_source='internal', &
                          marbl_varname=marbl_varname, field_units=units,              &
                          driver_varname='pressure', rank=3, dim3_len=km, id=n)
          case ('Iron Sediment Flux')
            fesedflux_ind = n
            call interior_tendency_forcings(n)%add_forcing_field(                &
                          field_source='file_time_invariant',                 &
                          marbl_varname=marbl_varname, field_units=units,     &
                          filename=fesedflux_input%filename,                  &
                          file_varname=fesedflux_input%file_varname,          &
                          unit_conv_factor=fesedflux_input%scale_factor,      &
                          rank=3, dim3_len=km, id=n)
          case ('O2 Consumption Scale Factor')
            select case (trim(o2_consumption_scalef_opt))
            case ('const')
              call interior_tendency_forcings(n)%add_forcing_field(field_source='const', &
                   marbl_varname=marbl_varname, field_units=units, &
                   field_constant=o2_consumption_scalef_const, rank=3, dim3_len=km, id=n)
            case ('file_time_invariant')
              call interior_tendency_forcings(n)%add_forcing_field( &
                   field_source='file_time_invariant', &
                   marbl_varname=marbl_varname, field_units=units, &
                   filename=o2_consumption_scalef_input%filename, &
                   file_varname=o2_consumption_scalef_input%file_varname, &
                   unit_conv_factor=o2_consumption_scalef_input%scale_factor, &
                   rank=3, dim3_len=km, id=n)
            case default
              call document(subname, 'unknown o2_consumption_scalef_opt', o2_consumption_scalef_opt)
              call exit_POP(sigAbort, 'Stopping in ' // subname)
            end select
          case ('Particulate Remin Scale Factor')
            select case (trim(p_remin_scalef_opt))
            case ('const')
              call interior_tendency_forcings(n)%add_forcing_field(field_source='const', &
                   marbl_varname=marbl_varname, field_units=units, &
                   field_constant=p_remin_scalef_const, rank=3, dim3_len=km, id=n)
            case ('file_time_invariant')
              call interior_tendency_forcings(n)%add_forcing_field( &
                   field_source='file_time_invariant', &
                   marbl_varname=marbl_varname, field_units=units, &
                   filename=p_remin_scalef_input%filename, &
                   file_varname=p_remin_scalef_input%file_varname, &
                   unit_conv_factor=p_remin_scalef_input%scale_factor, &
                   rank=3, dim3_len=km, id=n)
            case default
              call document(subname, 'unknown p_remin_scalef_opt', p_remin_scalef_opt)
              call exit_POP(sigAbort, 'Stopping in ' // subname)
            end select
          case DEFAULT
            write(err_msg, "(A,1X,A)") trim(marbl_req_interior_tendency_forcings(n)%metadata%varname), &
                           'is not a valid interior tendency forcing field name.'
            call document(subname, err_msg)
            call exit_POP(sigAbort, 'Stopping in ' // subname)
        end select
      end if

    end do ! do n=1,size(interior_tendency_forcings)
