module marbl_set_forcing_drv

  use marbl_interface,     only : marbl_interface_class
  use marbl_kinds_mod,     only : r8

  Implicit None
  Private
  Save

  Public :: test

Contains

  !****************************************************************************

  subroutine test(marbl_instances)

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances

    integer :: n

    ! 1. Read netCDF file -- get domain info, initial conditions, and forcings
    ! 2. Call init()
    ! 3. Call set_surface_forcing()
    ! 4. Call set_interior_forcing()
    ! 5. Output netCDF

  end subroutine test

  !****************************************************************************

end module marbl_set_forcing_drv