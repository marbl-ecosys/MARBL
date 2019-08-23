module marbl_io_mod
! This module provides an interface to the netCDF library if MARBL is built with -D_NETCDF
! It is meant to be used with the set_forcing regression test, which must be built with
! netCDF support. Machines without netCDF can still built / run the other tests (without
! -D_NETCDF this module does not include netCDF functions but will not provide any I/O)

  use marbl_kinds_mod,  only : r8, char_len
  use marbl_logging,    only : marbl_log_type
  use marbl_netcdf_mod, only : marbl_netcdf_read_field

  type, public :: grid_data_type
    real(kind=r8), allocatable, dimension(:) :: delta_z, zt, zw
  end type grid_data_type

  public :: marbl_io_read_domain
  public :: marbl_io_read_forcing_field
  public :: marbl_io_read_tracers_at_surface
  public :: marbl_io_read_tracers
contains

  !*****************************************************************************

  subroutine marbl_io_read_domain(infile, num_levels, num_PAR_subcols, grid_data, driver_status_log)

    use marbl_netcdf_mod, only : marbl_netcdf_read_dim

    character(len=*),     intent(in)    :: infile
    integer,              intent(inout) :: num_levels
    integer,              intent(inout) :: num_PAR_subcols
    type(grid_data_type), intent(inout) :: grid_data
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_domain'

    call marbl_netcdf_read_dim(infile, 'zt', num_levels, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_dim(zt)', subname)
      return
    end if

    call marbl_netcdf_read_dim(infile, 'nbin', num_PAR_subcols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_dim(nbin)', subname)
      return
    end if

    allocate(grid_data%delta_z(num_levels), grid_data%zt(num_levels), grid_data%zw(num_levels))

    call marbl_netcdf_read_field(infile, 'delta_z', grid_data%delta_z, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_field(delta_z)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%delta_z = grid_data%delta_z * 100._r8

    call marbl_netcdf_read_field(infile, 'zt', grid_data%zt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_field(zt)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%zt = grid_data%zt * 100._r8

    call marbl_netcdf_read_field(infile, 'zw', grid_data%zw, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_field(zw)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%zw = grid_data%zw * 100._r8

  end subroutine marbl_io_read_domain

  !*****************************************************************************

  subroutine marbl_io_read_forcing_field(infile, col_id, col_start, forcing_fields, driver_status_log, num_active_levels)

    use marbl_interface_public_types, only : marbl_forcing_fields_type

    character(len=*),                              intent(in)    :: infile
    integer,                                       intent(in)    :: col_id
    integer,                                       intent(in)    :: col_start
    type(marbl_forcing_fields_type), dimension(:), intent(inout) :: forcing_fields
    type(marbl_log_type),                          intent(inout) :: driver_status_log
    integer, optional,                             intent(in)    :: num_active_levels

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_forcing_field'
    character(len=char_len)     :: log_message
    integer :: n

    do n=1, size(forcing_fields)
      select case(trim(forcing_fields(n)%metadata%varname))
        case('u10_sqr')
          call marbl_netcdf_read_field(infile, 'u10_sqr', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from m^2 / s^2 -> cm^2 / s^2
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 10000._r8
        case('sss')
          call marbl_netcdf_read_field(infile, 'SSS', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('sst')
          call marbl_netcdf_read_field(infile, 'SST', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('Ice Fraction')
          call marbl_netcdf_read_field(infile, 'ice_frac', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('Dust Flux')
          if (size(forcing_fields(n)%field_0d) .eq. 1) then ! interior forcing
            call marbl_netcdf_read_field(infile, 'dust_flux', forcing_fields(n)%field_0d(1), &
                                         driver_status_log, col_id=col_id+col_start)
            ! convert from kg/m^2/s -> g/cm^2/s
            forcing_fields(n)%field_0d(1) = forcing_fields(n)%field_0d(1) * 0.1_r8
          else ! surface forcing
            call marbl_netcdf_read_field(infile, 'dust_flux', forcing_fields(n)%field_0d(col_id), &
                                         driver_status_log, col_id=col_id+col_start)
            ! convert from kg/m^2/s -> g/cm^2/s
            forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 0.1_r8
          end if
        case('Iron Flux')
          call marbl_netcdf_read_field(infile, 'iron_flux', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('NOx Flux')
          call marbl_netcdf_read_field(infile, 'nox_flux', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('NHy Flux')
          call marbl_netcdf_read_field(infile, 'nhy_flux', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('Atmospheric Pressure')
          call marbl_netcdf_read_field(infile, 'atm_pressure', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('xco2')
          call marbl_netcdf_read_field(infile, 'atm_co2', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('xco2_alt_co2')
          call marbl_netcdf_read_field(infile, 'atm_alt_co2', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('PAR Column Fraction')
          call marbl_netcdf_read_field(infile, 'FRACR_BIN', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Surface Shortwave')
          call marbl_netcdf_read_field(infile, 'QSW_BIN', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Potential Temperature')
          call marbl_netcdf_read_field(infile, 'temperature', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Salinity')
          call marbl_netcdf_read_field(infile, 'salinity', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Pressure')
          call marbl_netcdf_read_field(infile, 'pressure', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Iron Sediment Flux')
          call marbl_netcdf_read_field(infile, 'iron_sed_flux', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('O2 Consumption Scale Factor')
         call marbl_netcdf_read_field(infile, 'o2_consumption_scalef', forcing_fields(n)%field_1d(1,:), &
                                      driver_status_log, col_start=col_id+col_start)
        case DEFAULT
          write(log_message, "(3A)") "Unrecognized forcing field '", trim(forcing_fields(n)%metadata%varname), "'"
          call driver_status_log%log_error(log_message, subname)
          return
      end select
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_read_field(", trim(forcing_fields(n)%metadata%varname), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      if (present(num_active_levels)) then
        if (associated(forcing_fields(n)%field_1d)) then
          forcing_fields(n)%field_1d(1,num_active_levels+1:) = real(0, r8)
        end if
      end if
    end do

  end subroutine marbl_io_read_forcing_field

  !****************************************************************************

  subroutine marbl_io_read_tracers_at_surface(infile, col_start, col_cnt, tracer_metadata, &
                                              tracers_at_surface, driver_status_log)

    use marbl_interface_public_types, only : marbl_tracer_metadata_type

    character(len=*),                                 intent(in)    :: infile
    integer,                                          intent(in)    :: col_start
    integer,                                          intent(in)    :: col_cnt
    type(marbl_tracer_metadata_type), dimension(:),   intent(in)    :: tracer_metadata
    real(kind=r8),                    dimension(:,:), intent(inout) :: tracers_at_surface ! (num_surface_elem, tracer_cnt)
    type(marbl_log_type),                             intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_tracers_at_surface'
    character(len=char_len) :: log_message
    integer :: n

    do n = 1, size(tracer_metadata)
      call marbl_netcdf_read_field(infile, trim(tracer_metadata(n)%short_name), tracers_at_surface(:,n), &
                                   driver_status_log, surf_only=.true., col_start=col_start,             &
                                   col_cnt=col_cnt)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_read_field(", trim(tracer_metadata(n)%short_name), " [surface])"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine marbl_io_read_tracers_at_surface

  !****************************************************************************

  subroutine marbl_io_read_tracers(infile, col_start, tracer_metadata, tracers, driver_status_log)

    use marbl_interface_public_types, only : marbl_tracer_metadata_type

    character(len=*),                                 intent(in)    :: infile
    integer,                                          intent(in)    :: col_start
    type(marbl_tracer_metadata_type), dimension(:),   intent(in)    :: tracer_metadata
    real(kind=r8),                    dimension(:,:), intent(inout) :: tracers            ! (tracer_cnt, num_levels)
    type(marbl_log_type),                             intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_tracers'
    character(len=char_len) :: log_message
    integer :: n

    do n = 1, size(tracer_metadata)
      call marbl_netcdf_read_field(infile, trim(tracer_metadata(n)%short_name), tracers(n,:), &
                                   driver_status_log, col_start=col_start)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_read_field(", trim(tracer_metadata(n)%short_name), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine marbl_io_read_tracers

  !*****************************************************************************

end module marbl_io_mod
