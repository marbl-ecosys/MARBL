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
    use marbl_io_mod,  only : marbl_io_close_all

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_set_forcing_drv:test'
    integer :: n

    ! TESTING
    call marbl_io_open('file1', .false., n, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if

    call marbl_io_close_all(driver_status_log)
    !call marbl_io_close('file1', driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_close_all', subname)
      return
    end if

    ! 1. Read netCDF file -- get domain info, initial conditions, and forcings
    ! 2. Call init()
    ! 3. Call set_surface_forcing()
    ! 4. Call set_interior_forcing()
    ! 5. Output netCDF

  end subroutine test

  !****************************************************************************

end module marbl_set_forcing_drv