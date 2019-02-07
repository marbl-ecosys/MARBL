module marbl_set_forcing_drv

  use marbl_interface, only : marbl_interface_class
  use marbl_kinds_mod, only : r8, char_len
  use marbl_logging,   only : marbl_log_type

  implicit none
  private
  save

  Public :: test

Contains

  !****************************************************************************

  subroutine test(marbl_instances, driver_status_log)

    use marbl_io_mod,  only : marbl_io_open
    use marbl_io_mod,  only : marbl_io_define_diags
    use marbl_io_mod,  only : marbl_io_write_diags
    use marbl_io_mod,  only : marbl_io_close_all

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_set_forcing_drv:test'
    character(len=*), parameter :: infile = 'marbl.nc'
    character(len=*), parameter :: outfile = 'diagnostics.nc'
    character(len=char_len) :: log_message
    real(kind=r8), allocatable, dimension(:) :: delta_z, zt, zw
    integer :: num_levels, m, n

    ! 1. Open necessary netCDF files
    !    a) Input (grid info, forcing fields, initial conditions)
    call marbl_io_open(infile, .true., n, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if
    write(log_message, "(A, 1X, A)") "* Reading input from", infile
    call driver_status_log%log_noerror(log_message, subname)
    call read_domain(infile, num_levels, delta_z, zt, zw, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('read_domain', subname)
      return
    end if

    !    b) Output (diagnostics)
    call marbl_io_open(outfile, .false., n, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if
    write(log_message, "(A, 1X, A)") "* Diagnostics will be written to", outfile
    call driver_status_log%log_noerror(log_message, subname)

    ! 2. Get domain info, initial conditions, and forcing fields from netCDF file

    ! PER-INSTANCE CALLS
    do n=1,size(marbl_instances)
      ! 3. Call init()
      call marbl_instances(n)%init(gcm_num_levels = num_levels,       &
                                   gcm_num_PAR_subcols = 1,           &
                                   gcm_num_elements_surface_flux = 1, &
                                   gcm_delta_z = delta_z,             &
                                   gcm_zw = zw,                       &
                                   gcm_zt = zt)

      ! 4. Call surface_flux_compute()
      !    i. populate surface_flux_forcings
      call read_forcing_field(infile, marbl_instances(n)%surface_flux_forcings, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_forcing_field', subname)
        return
      end if
      !    ii. populate tracers_at_surface
      !    iii. populate saved_state
      do m=1, size(marbl_instances(n)%surface_flux_saved_state%state)
        marbl_instances(n)%surface_flux_saved_state%state(m)%field_2d(1) = 0._r8
      end do
      !    iv. call surface_flux_compute()
      call marbl_instances(n)%surface_flux_compute()

      ! 5. Call set_interior_forcing()
    end do
    ! 6. Define diagnostic fields in output netCDF file
    call marbl_io_define_diags(marbl_instances, outfile, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_define_diags', subname)
      return
    end if

    ! 7. Output netCDF
    call marbl_io_write_diags(marbl_instances, outfile, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_write_diags', subname)
      return
    end if

    ! 8. Close all netCDF files
    call marbl_io_close_all(driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_close_all', subname)
      return
    end if

  end subroutine test

  !*****************************************************************************

  subroutine read_domain(infile, num_levels, delta_z, zt, zw, driver_status_log)

    use marbl_io_mod, only : marbl_io_read_field

    character(len=*),                         intent(in)    :: infile
    integer,                                  intent(inout) :: num_levels
    real(kind=r8), allocatable, dimension(:), intent(inout) :: delta_z, zt, zw
    type(marbl_log_type),                     intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_set_forcing_drv:read_domain'

    num_levels = 60
    allocate(delta_z(num_levels), zt(num_levels), zw(num_levels))

    call marbl_io_read_field(infile, 'delta_z', delta_z, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_field(delta_z)', subname)
      return
    end if

    call marbl_io_read_field(infile, 'zt', zt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_field(zt)', subname)
      return
    end if

    call marbl_io_read_field(infile, 'zw', zw, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_field(zw)', subname)
      return
    end if

  end subroutine read_domain

  !*****************************************************************************

  subroutine read_forcing_field(infile, forcing_fields, driver_status_log)

    use marbl_interface_public_types, only : marbl_forcing_fields_type
    use marbl_io_mod, only : marbl_io_read_field

    character(len=*),                              intent(in)    :: infile
    type(marbl_forcing_fields_type), dimension(:), intent(inout) :: forcing_fields
    type(marbl_log_type),                          intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_set_forcing_drv:read_forcing_field'
    character(len=char_len)     :: log_message
    integer :: n

    do n=1, size(forcing_fields)
      select case(trim(forcing_fields(n)%metadata%varname))
        case('u10_sqr')
          call marbl_io_read_field(infile, 'u10_sqr', forcing_fields(n)%field_0d(1), driver_status_log)
        case('sss')
          call marbl_io_read_field(infile, 'SSS', forcing_fields(n)%field_0d(1), driver_status_log)
        case('sst')
          call marbl_io_read_field(infile, 'SST', forcing_fields(n)%field_0d(1), driver_status_log)
        case('Ice Fraction')
          call marbl_io_read_field(infile, 'ice_frac', forcing_fields(n)%field_0d(1), driver_status_log)
        case('Dust Flux')
          call marbl_io_read_field(infile, 'dust_flux', forcing_fields(n)%field_0d(1), driver_status_log)
        case('Iron Flux')
          call marbl_io_read_field(infile, 'iron_flux', forcing_fields(n)%field_0d(1), driver_status_log)
        case('NOx Flux')
          call marbl_io_read_field(infile, 'nox_flux', forcing_fields(n)%field_0d(1), driver_status_log)
        case('NHy Flux')
          call marbl_io_read_field(infile, 'nhy_flux', forcing_fields(n)%field_0d(1), driver_status_log)
        case('Atmospheric Pressure')
          call marbl_io_read_field(infile, 'atm_pressure', forcing_fields(n)%field_0d(1), driver_status_log)
        case('xco2')
          call marbl_io_read_field(infile, 'atm_co2', forcing_fields(n)%field_0d(1), driver_status_log)
        case('xco2_alt_co2')
          call marbl_io_read_field(infile, 'atm_alt_co2', forcing_fields(n)%field_0d(1), driver_status_log)
        case DEFAULT
          write(log_message, "(3A)") "Unrecognized forcing field '", trim(forcing_fields(n)%metadata%varname), "'"
          call driver_status_log%log_error(log_message, subname)
          return
      end select
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_io_read_field(", trim(forcing_fields(n)%metadata%varname), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine read_forcing_field

  !****************************************************************************

end module marbl_set_forcing_drv