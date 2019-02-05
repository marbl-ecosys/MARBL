module marbl_set_forcing_drv

  use marbl_interface,     only : marbl_interface_class
  use marbl_kinds_mod,     only : r8, char_len

  implicit none
  private
  save

  Public :: test

Contains

  !****************************************************************************

  subroutine test(marbl_instances, driver_status_log)

    use marbl_logging, only : marbl_log_type
    use marbl_io_mod,  only : marbl_io_open
    use marbl_io_mod,  only : marbl_io_read_domain
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
    integer :: num_levels, n

    ! 1. Open necessary netCDF files
    !    a) Input (grid info, forcing fields, initial conditions)
    call marbl_io_open(infile, .true., n, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if
    write(log_message, "(A, 1X, A)") "* Reading input from", infile
    call driver_status_log%log_noerror(log_message, subname)
    call marbl_io_read_domain(infile, num_levels, delta_z, zt, zw, driver_status_log)

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
      ! 4. Call set_surface_forcing()
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

  !****************************************************************************

end module marbl_set_forcing_drv