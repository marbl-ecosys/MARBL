module marbl_init_output_drv

  use marbl_interface,     only : marbl_interface_class
  use marbl_kinds_mod,     only : r8
  use marbl_constants_mod, only : c1, p5

  Implicit None
  Private
  Save

  integer, parameter :: km = 5

  Public :: marbl_init_output_test

Contains

  subroutine marbl_init_output_test(marbl_instance, ciso_on, gcm_namelist)

    type(marbl_interface_class), intent(inout) :: marbl_instance
    logical,                        intent(in) :: ciso_on
    character(len=*), dimension(:), intent(in) :: gcm_namelist

    character(*), parameter      :: subname = 'marbl_init_output_drv:test'
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
    call marbl_instance%config(gcm_nl_buffer = gcm_namelist,                  &
                               gcm_ciso_on = ciso_on)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%config', subname)
      return
    end if

    ! Log configuration setup
    call marbl_instance%configuration%list_vars(ciso_on, marbl_instance%StatusLog)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl_configuration%list', &
           subname)
      return
    end if

    ! Call marbl%init
    call marbl_instance%init(gcm_nl_buffer = gcm_namelist,                    &
                             gcm_num_levels = km,                             &
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

    ! Log parameter setup
    call marbl_instance%parameters%list_vars(ciso_on, marbl_instance%StatusLog)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl_parmeters%list',   &
           subname)
      return
    end if

  end subroutine marbl_init_output_test

end module marbl_init_output_drv
