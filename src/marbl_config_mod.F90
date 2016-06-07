module marbl_config_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt

  use marbl_internal_types, only : zooplankton_type
  use marbl_internal_types, only : autotroph_type

  use marbl_logging, only : marbl_log_type

  use marbl_constants_mod, only : dps

  implicit none
  public
  save

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_config_nml
  !---------------------------------------------------------------------

  logical(log_kind), target ::  lsource_sink                  ! control which portion of code is executed, useful for debugging
  logical(log_kind), target :: ciso_lsource_sink              ! control which portion of carbon isotope code is executed, useful for debugging
  logical(log_kind), target :: lecovars_full_depth_tavg       ! should base ecosystem vars be written full depth
  logical(log_kind), target :: ciso_lecovars_full_depth_tavg  ! should carbon isotope vars be written full depth
  logical(log_kind), target :: lflux_gas_o2                   ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: lflux_gas_co2                  ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: locmip_k1_k2_bug_fix

  ! Zooplankton & autotrophs are in config because tracer count depends on them
  character(len=char_len), dimension(autotroph_cnt),   target :: auto_names
  character(len=char_len), dimension(zooplankton_cnt), target :: zoo_names
  type(zooplankton_type) :: zooplankton(zooplankton_cnt)
  type(autotroph_type)   :: autotrophs(autotroph_cnt)

  !*****************************************************************************

  private :: r8, int_kind, log_kind, char_len
  private :: zooplankton_type, autotroph_type
  private :: autotroph_cnt, zooplankton_cnt
  private :: marbl_log_type

contains

  !*****************************************************************************

  subroutine marbl_config_set_defaults

    !-----------------------------------------------------------------------
    !  &marbl_config_nml
    !-----------------------------------------------------------------------

    lsource_sink                  = .true.
    ciso_lsource_sink             = .true.
    lecovars_full_depth_tavg      = .false.
    ciso_lecovars_full_depth_tavg = .false.
    lflux_gas_o2                  = .true.
    lflux_gas_co2                 = .true.
    locmip_k1_k2_bug_fix          = .true.
    zoo_names = (/'zoo'/)
    auto_names = (/'sp  ', 'diat', 'diaz'/)

  end subroutine marbl_config_set_defaults

  !*****************************************************************************

  subroutine marbl_config_read_namelist(nl_buffer, marbl_status_log)

    use marbl_namelist_mod, only : marbl_nl_cnt
    use marbl_namelist_mod, only : marbl_nl_buffer_size
    use marbl_namelist_mod, only : marbl_namelist

    character(marbl_nl_buffer_size), dimension(marbl_nl_cnt), intent(in) :: nl_buffer
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_config:marbl_config_read_namelist'
    character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
    integer (int_kind)           :: nml_error                   ! namelist i/o error flag

    namelist /marbl_config_nml/                                               &
         lsource_sink, ciso_lsource_sink, lecovars_full_depth_tavg,           &
         ciso_lecovars_full_depth_tavg, lflux_gas_o2, lflux_gas_co2,          &
         locmip_k1_k2_bug_fix

    !-----------------------------------------------------------------------
    ! read the &marbl_config_nml namelist
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_config_nml')
    read(tmp_nl_buffer, nml=marbl_config_nml, iostat=nml_error)
    if (nml_error /= 0) then
      call marbl_status_log%log_error("error reading &marbl_config_nml", subname)
      return
    end if

  end subroutine marbl_config_read_namelist

  !*****************************************************************************

  subroutine marbl_config_zooplankton(marbl_status_log)

  type(marbl_log_type),                  intent(inout) :: marbl_status_log

  character(*), parameter :: subname = 'marbl_config_mod:marbl_config_zooplankton'
  character(len=char_len) :: log_message
  integer                 :: zoo_ind

  if (size(zoo_names).ne.size(zooplankton)) then
    log_message = 'zoo names does not match size of zooplankton derived type'
    call marbl_status_log%log_error(log_message, subname)
    return
  end if

  do zoo_ind=1,size(zoo_names)
    select case (trim(zoo_names(zoo_ind)))
      case ('zoo')
        zooplankton(zoo_ind)%sname          ='zoo'
        zooplankton(zoo_ind)%lname          = 'Zooplankton'
        zooplankton(zoo_ind)%z_mort_0       = 0.1_r8 * dps
        zooplankton(zoo_ind)%z_mort2_0      = 0.4_r8 * dps
        zooplankton(zoo_ind)%loss_thres     = 0.075_r8     !zoo conc. where losses go to zero
      case DEFAULT
        print*, 'zoo_name = ', trim(zoo_names(zoo_ind))
        print*, 'size = ', len(trim(zoo_names(zoo_ind)))
        stop 0
        write(log_message, '(2A)') trim(zoo_names(zoo_ind)),                  &
                               ' is not a valid name for a zooplankton class'
        call marbl_status_log%log_error(log_message, subname)
        return
    end select
  end do

  end subroutine marbl_config_zooplankton

  !*****************************************************************************

  subroutine marbl_config_autotrophs(marbl_status_log)

  type(marbl_log_type),                  intent(inout) :: marbl_status_log

  character(*), parameter :: subname = 'marbl_config_mod:marbl_config_autotrophs'
  character(len=char_len) :: log_message
  integer                 :: auto_ind

  if (size(auto_names).ne.size(autotrophs)) then
    log_message = 'auto_names does not match size of autotroph derived type'
    call marbl_status_log%log_error(log_message, subname)
    return
  end if

  do auto_ind=1,size(auto_names)
    select case (trim(auto_names(auto_ind)))
      case ('sp')
        autotrophs(auto_ind)%sname         = 'sp'
        autotrophs(auto_ind)%lname         = 'Small Phyto'
        autotrophs(auto_ind)%Nfixer        = .false.
        autotrophs(auto_ind)%imp_calcifier = .true.
        autotrophs(auto_ind)%exp_calcifier = .false.
        autotrophs(auto_ind)%kFe           = 0.03e-3_r8
        autotrophs(auto_ind)%kPO4          = 0.01_r8
        autotrophs(auto_ind)%kDOP          = 0.2_r8
        autotrophs(auto_ind)%kNO3          = 0.25_r8
        autotrophs(auto_ind)%kNH4          = 0.0125_r8
        autotrophs(auto_ind)%kSiO3         = 0.0_r8
        autotrophs(auto_ind)%Qp            = 0.00855_r8
        autotrophs(auto_ind)%gQfe_0        = 30.0e-6_r8
        autotrophs(auto_ind)%gQfe_min      = 3.0e-6_r8
        autotrophs(auto_ind)%alphaPI       = 0.39_r8 * dps
        autotrophs(auto_ind)%PCref         = 5.5_r8 * dps
        autotrophs(auto_ind)%thetaN_max    = 2.5_r8
        autotrophs(auto_ind)%loss_thres    = 0.02_r8
        autotrophs(auto_ind)%loss_thres2   = 0.0_r8
        autotrophs(auto_ind)%temp_thres    = -10.0_r8
        autotrophs(auto_ind)%mort          = 0.1_r8 * dps
        autotrophs(auto_ind)%mort2         = 0.01_r8 * dps
        autotrophs(auto_ind)%agg_rate_max  = 0.5_r8
        autotrophs(auto_ind)%agg_rate_min  = 0.01_r8
        autotrophs(auto_ind)%loss_poc      = 0.0_r8
      case ('diat')
        autotrophs(auto_ind)%sname         = 'diat'
        autotrophs(auto_ind)%lname         = 'Diatom'
        autotrophs(auto_ind)%Nfixer        = .false.
        autotrophs(auto_ind)%imp_calcifier = .false.
        autotrophs(auto_ind)%exp_calcifier = .false.
        autotrophs(auto_ind)%kFe           = 0.07e-3_r8
        autotrophs(auto_ind)%kPO4          = 0.05_r8
        autotrophs(auto_ind)%kDOP          = 0.5_r8
        autotrophs(auto_ind)%kNO3          = 1.0_r8
        autotrophs(auto_ind)%kNH4          = 0.05_r8
        autotrophs(auto_ind)%kSiO3         = 0.8_r8
        autotrophs(auto_ind)%Qp            = 0.00855_r8
        autotrophs(auto_ind)%gQfe_0        = 30.0e-6_r8
        autotrophs(auto_ind)%gQfe_min      = 3.0e-6_r8
        autotrophs(auto_ind)%alphaPI       = 0.29_r8 * dps
        autotrophs(auto_ind)%PCref         = 5.5_r8 * dps
        autotrophs(auto_ind)%thetaN_max    = 4.0_r8
        autotrophs(auto_ind)%loss_thres    = 0.02_r8
        autotrophs(auto_ind)%loss_thres2   = 0.0_r8
        autotrophs(auto_ind)%temp_thres    = -10.0_r8
        autotrophs(auto_ind)%mort          = 0.1_r8 * dps
        autotrophs(auto_ind)%mort2         = 0.01_r8 * dps
        autotrophs(auto_ind)%agg_rate_max  = 0.5_r8
        autotrophs(auto_ind)%agg_rate_min  = 0.02_r8
        autotrophs(auto_ind)%loss_poc      = 0.0_r8
      case ('diaz')
        autotrophs(auto_ind)%sname         = 'diaz'
        autotrophs(auto_ind)%lname         = 'Diazotroph'
        autotrophs(auto_ind)%Nfixer        = .true.
        autotrophs(auto_ind)%imp_calcifier = .false.
        autotrophs(auto_ind)%exp_calcifier = .false.
        autotrophs(auto_ind)%kFe           = 0.03e-3_r8
        autotrophs(auto_ind)%kPO4          = 0.0125_r8
        autotrophs(auto_ind)%kDOP          = 0.05_r8
        autotrophs(auto_ind)%kNO3          = 4.0_r8
        autotrophs(auto_ind)%kNH4          = 0.4_r8
        autotrophs(auto_ind)%kSiO3         = 0.0_r8
        autotrophs(auto_ind)%Qp            = 0.002735_r8
        autotrophs(auto_ind)%gQfe_0        = 60.0e-6_r8
        autotrophs(auto_ind)%gQfe_min      = 6.0e-6_r8
        autotrophs(auto_ind)%alphaPI       = 0.39_r8 * dps
        autotrophs(auto_ind)%PCref         = 1.55_r8 * dps
        autotrophs(auto_ind)%thetaN_max    = 2.5_r8
        autotrophs(auto_ind)%loss_thres    = 0.02_r8
        autotrophs(auto_ind)%loss_thres2   = 0.001_r8
        autotrophs(auto_ind)%temp_thres    = 15.0_r8
        autotrophs(auto_ind)%mort          = 0.1_r8 * dps
        autotrophs(auto_ind)%mort2         = 0.01_r8 * dps
        autotrophs(auto_ind)%agg_rate_max  = 0.5_r8
        autotrophs(auto_ind)%agg_rate_min  = 0.01_r8
        autotrophs(auto_ind)%loss_poc      = 0.0_r8
      case DEFAULT
        write(log_message, '(2A)') trim(auto_names(auto_ind)),                &
                                 ' is not a valid name for a autotroph class'
        call marbl_status_log%log_error(log_message, subname)
        return
    end select
  end do

  end subroutine marbl_config_autotrophs

  !*****************************************************************************

end module marbl_config_mod
