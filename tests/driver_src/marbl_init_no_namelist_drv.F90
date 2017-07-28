module marbl_init_no_namelist_drv

  use marbl_interface,     only : marbl_interface_class
  use marbl_kinds_mod,     only : r8
  use marbl_constants_mod, only : c1, p5

  Implicit None
  Private
  Save

  integer, parameter :: km = 5

  Public :: test

Contains

  subroutine test(marbl_instance)

    use marbl_mpi_mod, only : marbl_mpi_abort

    type(marbl_interface_class), intent(inout) :: marbl_instance

    character(*), parameter      :: subname = 'marbl_init_no_namelist_drv:test'
    real(kind=r8), dimension(km) :: delta_z, zw, zt
    integer                      :: k

    ! Initialize levels
    delta_z = c1
    zw(1) = delta_z(1)
    zt(1) = p5*delta_z(1)
    do k=2,km
      zw(k) = zw(k-1) + delta_z(k)
      zt(k) = p5*(zw(k-1)+zw(k))
    end do

    ! Optional: call marbl_instance%configuration%put()
    call marbl_instance%configuration%put('ciso_on', .true.)

    ! Call marbl%config
    call marbl_instance%init_phase2()
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%init_phase2', subname)
      return
    end if

    ! Call marbl%init
    call marbl_instance%init_phase3(gcm_num_levels = km,           &
                             gcm_num_PAR_subcols = 1,              &
                             gcm_num_elements_surface_forcing = 1, &
                             gcm_delta_z = delta_z,                &
                             gcm_zw = zw,                          &
                             gcm_zt = zt)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%init_phase3', subname)
      return
    end if

    ! Optional: call marbl_instance%parameters%put()

    call marbl_instance%init_phase4()
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%init_phase4', subname)
      return
    end if

    ! Shutdown
    call marbl_instance%shutdown()
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%shutdown', subname)
      return
    end if

  end subroutine test

end module marbl_init_no_namelist_drv
