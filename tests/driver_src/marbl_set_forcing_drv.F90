module marbl_set_forcing_drv

  use marbl_interface,     only : marbl_interface_class
  use marbl_kinds_mod,     only : r8

  Implicit None
  Private
  Save

  Public :: test

Contains

  !****************************************************************************

  subroutine test(marbl_instances, driver_status_log)

    use marbl_logging, only : marbl_log_type
    use marbl_io_mod,  only : marbl_io_open
    use marbl_io_mod,  only : marbl_io_define_diags
    use marbl_io_mod,  only : marbl_io_write_diags
    use marbl_io_mod,  only : marbl_io_close_all

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_set_forcing_drv:test'
    character(len=*), parameter :: outfile = 'file1.nc'
    integer :: n
    integer, parameter :: km = 5
    real(kind=r8), dimension(km) :: delta_z, zw, zt
    integer                      :: k

    ! 1. Open necessary netCDF files
    !    a) Input (forcing fields, initial conditions, etc)
    delta_z = 1.0_r8
    zw(1) = delta_z(1)
    zt(1) = 0.5_r8*delta_z(1)
    do k=2,km
      zw(k) = zw(k-1) + delta_z(k)
      zt(k) = 0.5_r8*(zw(k-1)+zw(k))
    end do
    !    b) Output (diagnostics)
    call marbl_io_open(outfile, .false., n, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if

    ! 2. Get domain info, initial conditions, and forcing fields from netCDF file

    ! PER-INSTANCE CALLS
    do n=1,size(marbl_instances)
      ! 3. Call init()
      call marbl_instances(n)%init(gcm_num_levels = km,                  &
                                   gcm_num_PAR_subcols = 1,              &
                                   gcm_num_elements_surface_forcing = 1, &
                                   gcm_delta_z = delta_z,                &
                                   gcm_zw = zw,                          &
                                   gcm_zt = zt)
      ! 4. Call set_surface_forcing()
      ! 5. Call set_interior_forcing()
    end do
    ! 6. Define diagnostic fields in output netCDF file
    call marbl_io_define_diags(marbl_instances, outfile, driver_status_log)
    ! 7. Output netCDF
    call marbl_io_write_diags(marbl_instances, outfile, driver_status_log)

    ! 8. Close all netCDF files
    call marbl_io_close_all(driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_close_all', subname)
      return
    end if

  end subroutine test

  !****************************************************************************

end module marbl_set_forcing_drv