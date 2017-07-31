module marbl_get_put_drv

  use marbl_kinds_mod,     only : r8, char_len
  use marbl_constants_mod, only : c1, p5
  use marbl_logging,       only : marbl_log_type
  use marbl_interface,     only : marbl_interface_class

  Implicit None
  Private
  Save

  ! List of keywords (from variable names) to ignore in put / get statements
  ! (necessary to prevent internal MARBL errors do to inconsistent settings)
  character(len=*), dimension(3), parameter :: ignore_in_varnames = (/"_ind                ", &
                                                                      "caco3_bury_thres_opt", &
                                                                      "tracer_restore_vars "/)
  integer, parameter :: km = 5

  Public :: test

Contains

  !*****************************************************************************

  subroutine test(marbl_instance, driver_status_log)

    use marbl_parms,      only : marbl_config_set_defaults
    use marbl_parms,      only : marbl_parms_set_defaults

    type(marbl_interface_class), intent(inout) :: marbl_instance
    type(marbl_log_type),        intent(inout) :: driver_status_log

    character(*), parameter      :: subname = 'marbl_get_put_drv:test'
    type(marbl_interface_class)  :: marbl_instance_loc
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

    ! Set ciso_on = .true. for local instance
    call marbl_instance_loc%put('ciso_on', 'phase2', .true.)
    if (marbl_instance_loc%StatusLog%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_loc%put', subname)
      return
    end if

    ! Call marbl_loc%init
    call marbl_instance_loc%init(gcm_num_levels = km,                         &
                                 gcm_num_PAR_subcols = 1,                     &
                                 gcm_num_elements_surface_forcing = 1,        &
                                 gcm_delta_z = delta_z,                       &
                                 gcm_zw = zw,                                 &
                                 gcm_zt = zt)
    if (marbl_instance_loc%StatusLog%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_loc%init', subname)
      return
    end if

    ! Set all marbl_instance config vars / parms to -1 or .true.
    call set_all_vals(marbl_instance_loc, marbl_instance, driver_status_log)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('set_all_vals', subname)
      return
    end if

    ! Shutdown marbl_loc
    call marbl_instance_loc%shutdown()
    if (marbl_instance_loc%StatusLog%labort_marbl) then
      call driver_status_log%log_error_trace('marbl%shutdown', subname)
      return
    end if

    ! Call marbl_loc%init
    call marbl_instance%init(gcm_num_levels = km,                             &
                             gcm_num_PAR_subcols = 1,                         &
                             gcm_num_elements_surface_forcing = 1,            &
                             gcm_delta_z = delta_z,                           &
                             gcm_zw = zw,                                     &
                             gcm_zt = zt,                                     &
                             lgcm_has_global_ops = .true.)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%init', subname)
      return
    end if

    ! One by one, make sure vars are -1 or .true.
    call check_all_vals(marbl_instance, driver_status_log)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('check_all_vals', subname)
      return
    end if

    ! Shutdown
    call marbl_instance%shutdown()
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace('marbl%shutdown', subname)
      return
    end if

  end subroutine test

  !*****************************************************************************

  subroutine set_all_vals(local_instance, marbl_instance, driver_status_log)

    use marbl_mpi_mod, only : marbl_mpi_abort

    type(marbl_interface_class),       intent(in)    :: local_instance
    type(marbl_interface_class),       intent(inout) :: marbl_instance
    type(marbl_log_type),              intent(inout) :: driver_status_log

    character(*), parameter :: subname = 'marbl_get_put_drv:set_all_vals'
    character(len=char_len) :: log_message, sname, datatype
    integer :: n, n2
    logical :: var_match

    ! Put values into marbl_instance%configuration%vars(n)
    ! logicals = .true.
    ! integers = -1
    ! reals    = real(-1,r8)
    ! strings  = '-1'
    log_message = "Setting variables to .true. or -1 ..."
    call driver_status_log%log_noerror(log_message, subname)

    ! configuration variables
    do n=1,local_instance%configuration%get_cnt()
      call local_instance%configuration%inquire_metadata(n, marbl_instance%StatusLog, &
                                                         sname=sname,         &
                                                         datatype=datatype)
      if (marbl_instance%StatusLog%labort_marbl) then
        call marbl_instance%StatusLog%log_error_trace('inquire_metadata', subname)
        return
      end if
      ! Do not change variables listed in ignore_in_varnames
      var_match = .false.
      do n2=1,size(ignore_in_varnames)
        if (index(sname, trim(ignore_in_varnames(n2))).ne.0) var_match = .true.
      end do
      if (var_match) cycle
      select case (trim(datatype))
        case ('real')
          call marbl_instance%put(sname, 'phase2', -1)
        case ('integer')
          call marbl_instance%put(sname, 'phase2', -1)
        case ('string')
          call marbl_instance%put(sname, 'phase2', '-1')
        case ('logical')
          call marbl_instance%put(sname, 'phase2', .true.)
      end select
      if (marbl_instance%StatusLog%labort_marbl) then
        call marbl_instance%StatusLog%log_error_trace('marbl_instance%put', subname)
        return
      end if
    end do

    ! parameters variables
    do n=1,local_instance%parameters%get_cnt()
      call local_instance%parameters%inquire_metadata(n, marbl_instance%StatusLog,    &
                                                      sname=sname,            &
                                                      datatype=datatype)
      if (marbl_instance%StatusLog%labort_marbl) then
        call marbl_instance%StatusLog%log_error_trace('inquire_metadata', subname)
        return
      end if
      ! Do not change variables listed in ignore_in_varnames
      var_match = .false.
      do n2=1,size(ignore_in_varnames)
        if (index(sname, trim(ignore_in_varnames(n2))).ne.0) var_match = .true.
      end do
      if (var_match) cycle
      select case (trim(datatype))
        case ('real')
          call marbl_instance%put(sname, 'phase3', -1)
        case ('integer')
          call marbl_instance%put(sname, 'phase3', -1)
        case ('string')
          call marbl_instance%put(sname, 'phase3', '-1')
        case ('logical')
          call marbl_instance%put(sname, 'phase3', .true.)
      end select
      if (marbl_instance%StatusLog%labort_marbl) then
        call marbl_instance%StatusLog%log_error_trace('marbl_instance%put', subname)
        return
      end if
    end do

    log_message = "... Done!"
    call driver_status_log%log_noerror(log_message, subname)

  end subroutine set_all_vals

  !*****************************************************************************

  subroutine check_all_vals(marbl_instance, driver_status_log)

    type(marbl_interface_class), intent(inout) :: marbl_instance
    type(marbl_log_type),        intent(inout) :: driver_status_log

    character(*), parameter :: subname = 'marbl_get_put_drv:check_all_vals'
    character(len=char_len) :: log_message, sname, datatype, sval
    logical  :: lval
    real(r8) :: rval
    integer  :: ival
    integer  :: n, n2
    logical  :: var_match

    call driver_status_log%log_noerror("Making sure variables are .true. or -1", subname)

    associate(marbl_status_log => marbl_instance%StatusLog)

      ! Configuration Variables
      do n = 1,marbl_instance%configuration%get_cnt()
        call marbl_instance%configuration%inquire_metadata(n, marbl_status_log, &
                                                           sname = sname,       &
                                                           datatype=datatype)
        if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace('inquire_metadata', subname)
          return
        end if

        ! Do not check variables listed in ignore_in_varnames
        var_match = .false.
        do n2=1,size(ignore_in_varnames)
          if (index(sname, trim(ignore_in_varnames(n2))).ne.0) var_match = .true.
        end do
        if (var_match) cycle
        select case (trim(datatype))
          case ('real')
            ! Check to see that variable was set to -1 correctly
            call marbl_instance%configuration%get(sname, rval, marbl_status_log)
            if (rval.ne.real(-1,r8)) then
              write(log_message, "(2A,E24.16,A)") trim(sname), ' = ', rval, ' not -1'
              call marbl_status_log%log_error(log_message, subname)
              return
            end if
          case ('integer')
            ! Check to see that variable was set to -1 correctly
            call marbl_instance%configuration%get(sname, ival, marbl_status_log)
            if (ival.ne.-1) then
              write(log_message, "(2A,I0,A)") trim(sname), ' = ', ival, ' not -1'
              call marbl_status_log%log_error(log_message, subname)
              return
            end if
          case ('string')
            ! Check to see that variable was set to .true. correctly
            call marbl_instance%configuration%get(sname, sval, marbl_status_log)
            if (trim(sval).ne.'-1') then
              write(log_message, "(4A)") trim(sname), ' = ', trim(sval), ', not -1'
              call marbl_status_log%log_error(log_message, subname)
              return
            end if
          case ('logical')
            ! Check to see that variable was set to .true. correctly
            call marbl_instance%configuration%get(sname, lval, marbl_status_log)
            if (.not.lval) then
              write(log_message, "(2A)") trim(sname), ' is .false., not .true.'
              call marbl_status_log%log_error(log_message, subname)
              return
            end if
        end select
        if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace('marbl_instance%configuration%put', subname)
          return
        end if
      end do
    end associate

    log_message = "... Done!"
    call driver_status_log%log_noerror(log_message, subname)

  end subroutine check_all_vals

  !*****************************************************************************

end module marbl_get_put_drv
