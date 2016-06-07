module marbl_config_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt

  use marbl_internal_types, only : zooplankton_type
  use marbl_internal_types, only : autotroph_type

  use marbl_logging, only: marbl_log_type

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

  private :: int_kind, log_kind, char_len
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

end module marbl_config_mod
