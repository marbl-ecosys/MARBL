module marbl_living_parms_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt

  use marbl_config_mod, only : marbl_config_vars_type

  use marbl_internal_types, only : zooplankton_type
  use marbl_internal_types, only : autotroph_type

  use marbl_logging, only : marbl_log_type

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : dps

  implicit none
  public
  save

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_living_parms_nml
  !---------------------------------------------------------------------

  ! Zooplankton & autotrophs are needed prior to rest of initialization because
  ! tracer count depends on them
  type(zooplankton_type), target :: zooplankton(zooplankton_cnt)
  type(autotroph_type),   target :: autotrophs(autotroph_cnt)

  !---------------------------------------------------------------------
  !  Datatype for marbl_instance%living_parameters
  !---------------------------------------------------------------------

  type, extends(marbl_config_vars_type), public :: marbl_living_parms_type
  contains
    procedure :: construct        => marbl_living_parms_construct
  end type marbl_living_parms_type

  private :: r8, int_kind, log_kind, char_len
  private :: marbl_config_vars_type
  private :: zooplankton_type, autotroph_type
  private :: marbl_log_type
  private :: c0, dps
  private :: autotroph_cnt, zooplankton_cnt

contains

  subroutine marbl_living_parms_set_defaults(marbl_status_log)

    use marbl_config_mod, only : auto_names
    use marbl_config_mod, only : zoo_names

    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(*), parameter :: subname =                                      &
                     'marbl_living_parms_mod%marbl_living_parms_set_defaults'
    integer :: n

    !-----------------------------------------------------------------------
    !  &marbl_living_parms_nml
    !-----------------------------------------------------------------------

    do n=1,size(auto_names)
      call autotrophs(n)%put_sname(auto_names(n), marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace('autotroph%put_sname', subname)
        return
      end if
      select case (trim(auto_names(n)))
        case ('sp')
          autotrophs(n)%lname         = 'Small Phyto'
          autotrophs(n)%Nfixer        = .false.
          autotrophs(n)%imp_calcifier = .true.
          autotrophs(n)%exp_calcifier = .false.
          autotrophs(n)%kFe           = 0.03e-3_r8
          autotrophs(n)%kPO4          = 0.01_r8
          autotrophs(n)%kDOP          = 0.2_r8
          autotrophs(n)%kNO3          = 0.25_r8
          autotrophs(n)%kNH4          = 0.0125_r8
          autotrophs(n)%kSiO3         = 0.0_r8
          autotrophs(n)%Qp            = 0.00855_r8
          autotrophs(n)%gQfe_0        = 30.0e-6_r8
          autotrophs(n)%gQfe_min      = 3.0e-6_r8
          autotrophs(n)%alphaPI       = 0.39_r8 * dps
          autotrophs(n)%PCref         = 5.5_r8 * dps
          autotrophs(n)%thetaN_max    = 2.5_r8
          autotrophs(n)%loss_thres    = 0.02_r8
          autotrophs(n)%loss_thres2   = 0.0_r8
          autotrophs(n)%temp_thres    = -10.0_r8
          autotrophs(n)%mort          = 0.1_r8 * dps
          autotrophs(n)%mort2         = 0.01_r8 * dps
          autotrophs(n)%agg_rate_max  = 0.5_r8
          autotrophs(n)%agg_rate_min  = 0.01_r8
          autotrophs(n)%loss_poc      = 0.0_r8
        case ('diat')
          autotrophs(n)%lname         = 'Diatom'
          autotrophs(n)%Nfixer        = .false.
          autotrophs(n)%imp_calcifier = .false.
          autotrophs(n)%exp_calcifier = .false.
          autotrophs(n)%kFe           = 0.07e-3_r8
          autotrophs(n)%kPO4          = 0.05_r8
          autotrophs(n)%kDOP          = 0.5_r8
          autotrophs(n)%kNO3          = 1.0_r8
          autotrophs(n)%kNH4          = 0.05_r8
          autotrophs(n)%kSiO3         = 0.8_r8
          autotrophs(n)%Qp            = 0.00855_r8
          autotrophs(n)%gQfe_0        = 30.0e-6_r8
          autotrophs(n)%gQfe_min      = 3.0e-6_r8
          autotrophs(n)%alphaPI       = 0.29_r8 * dps
          autotrophs(n)%PCref         = 5.5_r8 * dps
          autotrophs(n)%thetaN_max    = 4.0_r8
          autotrophs(n)%loss_thres    = 0.02_r8
          autotrophs(n)%loss_thres2   = 0.0_r8
          autotrophs(n)%temp_thres    = -10.0_r8
          autotrophs(n)%mort          = 0.1_r8 * dps
          autotrophs(n)%mort2         = 0.01_r8 * dps
          autotrophs(n)%agg_rate_max  = 0.5_r8
          autotrophs(n)%agg_rate_min  = 0.02_r8
          autotrophs(n)%loss_poc      = 0.0_r8
        case ('diaz')
          autotrophs(n)%lname         = 'Diazotroph'
          autotrophs(n)%Nfixer        = .true.
          autotrophs(n)%imp_calcifier = .false.
          autotrophs(n)%exp_calcifier = .false.
          autotrophs(n)%kFe           = 0.03e-3_r8
          autotrophs(n)%kPO4          = 0.0125_r8
          autotrophs(n)%kDOP          = 0.05_r8
          autotrophs(n)%kNO3          = 4.0_r8
          autotrophs(n)%kNH4          = 0.4_r8
          autotrophs(n)%kSiO3         = 0.0_r8
          autotrophs(n)%Qp            = 0.002735_r8
          autotrophs(n)%gQfe_0        = 60.0e-6_r8
          autotrophs(n)%gQfe_min      = 6.0e-6_r8
          autotrophs(n)%alphaPI       = 0.39_r8 * dps
          autotrophs(n)%PCref         = 1.55_r8 * dps
          autotrophs(n)%thetaN_max    = 2.5_r8
          autotrophs(n)%loss_thres    = 0.02_r8
          autotrophs(n)%loss_thres2   = 0.001_r8
          autotrophs(n)%temp_thres    = 15.0_r8
          autotrophs(n)%mort          = 0.1_r8 * dps
          autotrophs(n)%mort2         = 0.01_r8 * dps
          autotrophs(n)%agg_rate_max  = 0.5_r8
          autotrophs(n)%agg_rate_min  = 0.01_r8
          autotrophs(n)%loss_poc      = 0.0_r8
        case DEFAULT
          autotrophs(n)%lname         = 'unknown'
          autotrophs(n)%Nfixer        = .false.
          autotrophs(n)%imp_calcifier = .false.
          autotrophs(n)%exp_calcifier = .false.
          autotrophs(n)%kFe           = c0
          autotrophs(n)%kPO4          = c0
          autotrophs(n)%kDOP          = c0
          autotrophs(n)%kNO3          = c0
          autotrophs(n)%kNH4          = c0
          autotrophs(n)%kSiO3         = c0
          autotrophs(n)%Qp            = c0
          autotrophs(n)%gQfe_0        = c0
          autotrophs(n)%gQfe_min      = c0
          autotrophs(n)%alphaPI       = c0
          autotrophs(n)%PCref         = c0
          autotrophs(n)%thetaN_max    = c0
          autotrophs(n)%loss_thres    = c0
          autotrophs(n)%loss_thres2   = c0
          autotrophs(n)%temp_thres    = c0
          autotrophs(n)%mort          = c0
          autotrophs(n)%mort2         = c0
          autotrophs(n)%agg_rate_max  = c0
          autotrophs(n)%agg_rate_min  = c0
          autotrophs(n)%loss_poc      = c0
      end select
    end do

    do n=1,size(zoo_names)
      call zooplankton(n)%put_sname(zoo_names(n), marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace('zooplankton%put_sname', subname)
        return
      end if
      select case (trim(zoo_names(n)))
        case ('zoo')
          zooplankton(n)%lname      = 'Zooplankton'
          zooplankton(n)%z_mort_0   = 0.1_r8 * dps
          zooplankton(n)%z_mort2_0  = 0.4_r8 * dps
          zooplankton(n)%loss_thres = 0.075_r8
        case DEFAULT
          zooplankton(n)%lname      = 'unknown'
          zooplankton(n)%z_mort_0   = c0
          zooplankton(n)%z_mort2_0  = c0
          zooplankton(n)%loss_thres = c0
      end select
    end do

  end subroutine marbl_living_parms_set_defaults

  !*****************************************************************************

  subroutine marbl_living_parms_read_namelist(nl_buffer, marbl_status_log)

    use marbl_namelist_mod, only : marbl_nl_cnt
    use marbl_namelist_mod, only : marbl_nl_buffer_size
    use marbl_namelist_mod, only : marbl_namelist

    character(marbl_nl_buffer_size), dimension(marbl_nl_cnt), intent(in) :: nl_buffer
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname =                                  &
                    'marbl_living_parms_mod:marbl_living_parms_read_namelist'
    character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
    integer (int_kind)           :: nml_error                   ! namelist i/o error flag

    namelist /marbl_living_parms_nml/                                         &
         autotrophs, zooplankton

    !-----------------------------------------------------------------------
    ! read the &marbl_living_parms_nml namelist
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_living_parms_nml')
    read(tmp_nl_buffer, nml=marbl_living_parms_nml, iostat=nml_error)
    if (nml_error /= 0) then
      call marbl_status_log%log_error("error reading &marbl_living_parms_nml", subname)
      return
    end if

  end subroutine marbl_living_parms_read_namelist

  !*****************************************************************************

  subroutine marbl_living_parms_construct(this, marbl_status_log)

    use marbl_config_mod, only : log_add_var_error

    class(marbl_living_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname =                                      &
                        'marbl_living_parms_mod:marbl_living_parms_construct'
    character(len=char_len) :: log_message
    character(len=char_len) :: sname, lname, units, datatype, group
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    character(len=char_len) :: prefix
    integer :: n

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%living_parameters has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    this%cnt = 0
    allocate(this%vars(this%cnt))

    !-----------------------------!
    ! marbl_living_parameters_nml !
    !-----------------------------!

    do n=1,size(autotrophs)
      write(prefix, "(3A)") 'autotrophs(', trim(autotrophs(n)%sname), ')%'

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_living_parms_nml'
      sptr     => autotrophs(n)%lname
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'Nfixer'
      lname    = 'Flag is true if this autotroph fixes N2'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_living_parms_nml'
      lptr     => autotrophs(n)%Nfixer
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'imp_calcifier'
      lname    = 'Flag is true if this autotroph implicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_living_parms_nml'
      lptr     => autotrophs(n)%imp_calcifier
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'exp_calcifier'
      lname    = 'Flag is true if this autotroph explicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_living_parms_nml'
      lptr     => autotrophs(n)%exp_calcifier
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kFe'
      lname    = 'nutrient uptake half-sat constants'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%kFe
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kPO4'
      lname    = 'nutrient uptake half-sat constants'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%kPO4
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kDOP'
      lname    = 'nutrient uptake half-sat constants'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%kDOP
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kNO3'
      lname    = 'nutrient uptake half-sat constants'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%kNO3
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kNH4'
      lname    = 'nutrient uptake half-sat constants'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%kNH4
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kSiO3'
      lname    = 'nutrient uptake half-sat constants'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%kSiO3
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'Qp'
      lname    = 'P/C ratio'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%Qp
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'gQfe_0'
      lname    = 'initial Fe/C ratio'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%gQFe_0
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'gQfe_min'
      lname    = 'minimum Fe/C ratio'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%gQFe_min
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'alphaPi'
      lname    = 'Initial slope of P_I curve (GD98)'
      units    = 'mmol C m^2 / (mg Chl W s)'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%alphaPi
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'PCref'
      lname    = 'max C-spec growth rate at Tref'
      units    = '1/s'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%PCref
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'thetaN_max'
      lname    = 'max thetaN (Chl/N)'
      units    = 'mg Chl / mmol N'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%thetaN_max
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres'
      lname    = 'concentration where losses go to zero'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%loss_thres
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres2'
      lname    = 'concentration where losses go to zero'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%loss_thres2
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'temp_thres'
      lname    = 'Temperature where concentration threshold and photosynthesis rate drop'
      units    = 'deg C'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%temp_thres
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'mort'
      lname    = 'linear mortality rate'
      units    = '1/s'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%mort
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'mort2'
      lname    = 'quadratic mortality rate'
      units    = '1/s/(mmol C/m^3)'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%mort2
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'agg_rate_max'
      lname    = 'Maximum agg rate'
      units    = '1/d'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%agg_rate_max
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'agg_rate_min'
      lname    = 'Minimum agg rate'
      units    = '1/d'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%agg_rate_min
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_poc'
      lname    = 'routing of loss term'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => autotrophs(n)%loss_poc
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr, add_space=.true.)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

    do n=1,size(zooplankton)
      write(prefix, "(3A)") 'zooplankton(', trim(zooplankton(n)%sname), ')%'

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_living_parms_nml'
      sptr     => zooplankton(n)%lname
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'z_mort_0'
      lname    = 'Linear mortality rate'
      units    = '1/s'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => zooplankton(n)%z_mort_0
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres'
      lname    = 'Concentration where losses go to zero'
      units    = ''
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => zooplankton(n)%loss_thres
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'z_mort2_0'
      lname    = 'Quadratic mortality rate'
      units    = '1/s/(mmol C / m^3)'
      datatype = 'real'
      group    = 'marbl_living_parms_nml'
      rptr     => zooplankton(n)%z_mort2_0
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

  end subroutine marbl_living_parms_construct

  !*****************************************************************************

end module marbl_living_parms_mod

