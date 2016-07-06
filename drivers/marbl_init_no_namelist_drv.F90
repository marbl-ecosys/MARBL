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

    type(marbl_interface_class), intent(inout) :: marbl_instance

    character(*), parameter      :: subname = 'marbl_init_no_namelist_drv:test'
    real(kind=r8), dimension(km) :: dz, zw, zt
    integer                      :: k

    ! Initialize levels
    dz = c1
    zw(1) = dz(1)
    zt(1) = p5*dz(1)
    do k=2,km
      zw(k) = zw(k-1) + dz(k)
      zt(k) = p5*(zw(k-1)+zw(k))
    end do

    ! Call marbl%config
    call marbl_instance%config()
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%config', subname)
      return
    end if

    ! Optional: call marbl_instance%configuration%put()
    call marbl_instance%configuration%put('ciso_on', .true., marbl_instance%StatusLog)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%config%put', subname)
      return
    end if

    ! Call marbl%init
    call marbl_instance%init(gcm_num_levels = km,                             &
                             gcm_num_PAR_subcols = 1,                         &
                             gcm_num_elements_interior_forcing = 1,           &
                             gcm_num_elements_surface_forcing = 1,            &
                             gcm_dz = dz,                                     &
                             gcm_zw = zw,                                     &
                             gcm_zt = zt)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%init', subname)
      return
    end if

    ! Optional: call marbl_instance%parameters%put()
    call marbl_instance%parameters%put('fe_max_scale2', 2050,                 &
                                       marbl_instance%StatusLog)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%parms%put', subname)
      return
    end if

    call marbl_instance%complete_config_and_init
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace(                          &
           'marbl%complete_config_and_init', subname)
      return
    end if

  end subroutine test

end module marbl_init_no_namelist_drv
