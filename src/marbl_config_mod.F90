module marbl_config_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt
  use marbl_sizes, only : grazer_prey_cnt

  use marbl_logging, only : marbl_log_type

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : dps

  use marbl_internal_types, only : autotroph_config_type
  use marbl_internal_types, only : zooplankton_config_type
  use marbl_internal_types, only : grazing_config_type

  implicit none
  public
  save

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_config_nml
  !---------------------------------------------------------------------

  logical(log_kind), target ::  ciso_on                       ! control whether ciso tracer module is active
  logical(log_kind), target ::  lsource_sink                  ! control which portion of code is executed, useful for debugging
  logical(log_kind), target :: ciso_lsource_sink              ! control which portion of carbon isotope code is executed, useful for debugging
  logical(log_kind), target :: lecovars_full_depth_tavg       ! should base ecosystem vars be written full depth
  logical(log_kind), target :: ciso_lecovars_full_depth_tavg  ! should carbon isotope vars be written full depth
  logical(log_kind), target :: lflux_gas_o2                   ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: lflux_gas_co2                  ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: lapply_nhx_surface_emis        ! control if NHx emissions are applied to marbl NH4 tracer
  logical(log_kind), target :: locmip_k1_k2_bug_fix
  type(autotroph_config_type), dimension(autotroph_cnt), target :: autotrophs_config
  type(zooplankton_config_type), dimension(zooplankton_cnt), target :: zooplankton_config
  type(grazing_config_type), dimension(grazer_prey_cnt, zooplankton_cnt), target :: grazing_config

  !-----------------------------------------------------------------------
  !  bury to sediment options
  !  bury coefficients (POC_bury_coeff, POP_bury_coeff, bSi_bury_coeff) reside in marbl_particulate_share_type
  !  when ladjust_bury_coeff is .true., bury coefficients are adjusted
  !  to preserve C, P, Si inventories on timescales exceeding bury_coeff_rmean_timescale_years
  !  this is done primarily in spinup runs
  !-----------------------------------------------------------------------
  character(char_len), target :: init_bury_coeff_opt
  logical (log_kind),  target :: ladjust_bury_coeff


  !---------------------------------------------------------------------
  !  Variables that don't match namelist read
  !---------------------------------------------------------------------

  !---------------------------------------------------------------------------
  !  Datatypes for marbl_instance%configuration and marbl_instance%parameters
  !---------------------------------------------------------------------------

  type, public :: marbl_single_config_or_parm_type
    ! Metadata
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: group
    character(len=char_len) :: datatype
    integer                 :: category_id ! used for sorting output list
    character(len=char_len) :: comment   ! used to add comment in log
    ! Actual parameter data
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()
  end type marbl_single_config_or_parm_type

  type, public :: marbl_config_and_parms_type
    logical :: locked = .false.
    integer :: cnt = 0
    character(len=char_len),                dimension(:), pointer :: categories
    type(marbl_single_config_or_parm_type), dimension(:), pointer :: vars => NULL()
  contains
    procedure          :: add_var          => marbl_var_add
    procedure          :: add_var_1d_r8    => marbl_var_add_1d_r8
    procedure          :: add_var_1d_int   => marbl_var_add_1d_int
    procedure          :: add_var_1d_str   => marbl_var_add_1d_str
    procedure          :: finalize_vars    => marbl_vars_finalize
    procedure          :: inquire_id       => marbl_var_inquire_id
    procedure          :: inquire_metadata => marbl_var_inquire_metadata
    generic            :: put              => put_real,                       &
                                              put_integer,                    &
                                              put_logical,                    &
                                              put_string
    generic            :: get              => get_real,                       &
                                              get_integer,                    &
                                              get_logical,                    &
                                              get_string
    procedure, private :: put_real         => marbl_var_put_real
    procedure, private :: put_integer      => marbl_var_put_integer
    procedure, private :: put_logical      => marbl_var_put_logical
    procedure, private :: put_string       => marbl_var_put_string
    procedure, private :: get_real         => marbl_var_get_real
    procedure, private :: get_integer      => marbl_var_get_integer
    procedure, private :: get_logical      => marbl_var_get_logical
    procedure, private :: get_string       => marbl_var_get_string
    procedure, private :: put_general      => marbl_var_put_all_types
    procedure, private :: get_general      => marbl_var_get_all_types
  end type marbl_config_and_parms_type

  !*****************************************************************************

  private :: r8, int_kind, log_kind, char_len
  private :: autotroph_cnt, zooplankton_cnt
  private :: marbl_log_type
  private :: marbl_var_put_real, marbl_var_put_integer, marbl_var_put_logical
  private :: marbl_var_put_string, marbl_var_put_all_types

contains

  !*****************************************************************************

  subroutine marbl_config_set_defaults()

    integer :: m, n

    !-----------------------------------------------------------------------
    !  &marbl_config_nml
    !-----------------------------------------------------------------------

    ciso_on                       = .false.
    lsource_sink                  = .true.
    ciso_lsource_sink             = .true.
    lecovars_full_depth_tavg      = .false.
    ciso_lecovars_full_depth_tavg = .false.
    lflux_gas_o2                  = .true.
    lflux_gas_co2                 = .true.
    lapply_nhx_surface_emis       = .false.
    locmip_k1_k2_bug_fix          = .true.
    init_bury_coeff_opt           = 'nml'
    ladjust_bury_coeff            = .false.

    do n=1,autotroph_cnt
      select case (n)
        case (1)
          autotrophs_config(n)%sname         = 'sp'
          autotrophs_config(n)%lname         = 'Small Phyto'
          autotrophs_config(n)%Nfixer        = .false.
          autotrophs_config(n)%imp_calcifier = .true.
          autotrophs_config(n)%exp_calcifier = .false.
          autotrophs_config(n)%silicifier    = .false.
        case (2)
          autotrophs_config(n)%sname         = 'diat'
          autotrophs_config(n)%lname         = 'Diatom'
          autotrophs_config(n)%Nfixer        = .false.
          autotrophs_config(n)%imp_calcifier = .false.
          autotrophs_config(n)%exp_calcifier = .false.
          autotrophs_config(n)%silicifier    = .true.
        case (3)
          autotrophs_config(n)%sname         = 'diaz'
          autotrophs_config(n)%lname         = 'Diazotroph'
          autotrophs_config(n)%Nfixer        = .true.
          autotrophs_config(n)%imp_calcifier = .false.
          autotrophs_config(n)%exp_calcifier = .false.
          autotrophs_config(n)%silicifier    = .false.
        case DEFAULT
          write(autotrophs_config(n)%sname,"(A,I0)") 'auto', n
          write(autotrophs_config(n)%lname,"(A,I0)") 'Autotroph number ', n
          autotrophs_config(n)%Nfixer        = .false.
          autotrophs_config(n)%imp_calcifier = .false.
          autotrophs_config(n)%exp_calcifier = .false.
          autotrophs_config(n)%silicifier    = .false.
      end select
    end do

    do n=1,zooplankton_cnt
      select case (n)
        case (1)
          zooplankton_config(n)%sname = 'zoo'
          zooplankton_config(n)%lname = 'Zooplankton'
        case DEFAULT
          write(zooplankton_config(n)%sname, "(A,I0)") 'zoo', n
          write(zooplankton_config(n)%lname, "(A,I0)") 'Zooplankton number ', n
      end select
    end do

    ! predator-prey relationships
    do n=1,zooplankton_cnt
      do m=1,grazer_prey_cnt

        write(grazing_config(m,n)%sname, "(4A)") 'grz_',                      &
                                   trim(autotrophs_config(m)%sname),          &
                                   '_', trim(zooplankton_config(n)%sname)
        write(grazing_config(m,n)%lname, "(4A)") 'Grazing of ',               &
                                   trim(autotrophs_config(m)%sname),          &
                                   ' by ', trim(zooplankton_config(n)%sname)
        grazing_config(m,n)%auto_ind_cnt = 1
        grazing_config(m,n)%zoo_ind_cnt  = 0
      end do
    end do

  end subroutine marbl_config_set_defaults

  !*****************************************************************************

  subroutine marbl_config_read_namelist(nl_buffer, marbl_status_log)

    use marbl_namelist_mod, only : marbl_nl_cnt
    use marbl_namelist_mod, only : marbl_nl_buffer_size
    use marbl_namelist_mod, only : marbl_namelist

    character(marbl_nl_buffer_size), intent(in)    :: nl_buffer(:)
    type(marbl_log_type),            intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_config:marbl_config_read_namelist'
    character(len=char_len) :: log_message
    character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
    integer (int_kind)           :: nml_error                   ! namelist i/o error flag

    namelist /marbl_config_nml/                                               &
         ciso_on, lsource_sink, ciso_lsource_sink, lecovars_full_depth_tavg,  &
         ciso_lecovars_full_depth_tavg,                                       &
         lflux_gas_o2, lflux_gas_co2, lapply_nhx_surface_emis,                &
         locmip_k1_k2_bug_fix, init_bury_coeff_opt, ladjust_bury_coeff,       &
         autotrophs_config, zooplankton_config, grazing_config

    !-----------------------------------------------------------------------
    ! read the &marbl_config_nml namelist
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_config_nml', marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('marbl_namelist', subname)
      return
    end if

    read(tmp_nl_buffer, nml=marbl_config_nml, iostat=nml_error)
    if (nml_error /= 0) then
      write(log_message, "(A)") 'error reading &marbl_config_nml'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

  end subroutine marbl_config_read_namelist

  !*****************************************************************************

  subroutine marbl_define_config_vars(this, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_define_config_vars'
    character(len=char_len) :: log_message
    character(len=char_len) :: sname, lname, units, datatype, group, category
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    integer :: m, n
    character(len=char_len) :: prefix

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%configuration has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    this%cnt = 0
    allocate(this%vars(this%cnt))
    allocate(this%categories(0))

    !------------------!
    ! marbl_config_nml !
    !------------------!

    category  = 'config flags'

    sname     = 'ciso_on'
    lname     = 'Control whether CISO tracer module is active'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => ciso_on
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lsource_sink'
    lname     = 'Control which portions of code are executed (useful for debugging)'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lsource_sink
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lecovars_full_depth_tavg'
    lname     = 'Are base ecosystem tracers full depth?'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lecovars_full_depth_tavg
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ciso_lsource_sink'
    lname     = 'Control which portions of carbon isotope code are executed (useful for debugging)'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => ciso_lsource_sink
    call this%add_var(sname, lname, units, datatype, group, category,       &
                      marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ciso_lecovars_full_depth_tavg'
    lname     = 'Are carbon isotope tracers full depth?'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => ciso_lecovars_full_depth_tavg
    call this%add_var(sname, lname, units, datatype, group, category,       &
                      marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lflux_gas_o2'
    lname     = 'Run O2 gas flux portion of the code'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lflux_gas_o2
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lflux_gas_co2'
    lname     = 'Run CO2 gas flux portion of the code'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lflux_gas_co2
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lapply_nhx_surface_emis'
    lname     = 'control if NHx emissions are applied to marbl NH4 tracer'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lapply_nhx_surface_emis
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'locmip_k1_k2_bug_fix'
    lname     = 'Fix bug that was in code in OCMIP runs?'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => locmip_k1_k2_bug_fix
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ladjust_bury_coeff'
    lname     = 'Adjust the bury coefficient to maintain equilibrium'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => ladjust_bury_coeff
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'init_bury_coeff_opt'
    lname     = 'How to set initial bury coefficients'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_config_nml'
    sptr      => init_bury_coeff_opt
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    do n=1,autotroph_cnt
      write(prefix, "(A,I0,A)") 'autotrophs_config(', n, ')%'
      write(category, "(A,1X,I0)") 'autotroph_conf', n

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => autotrophs_config(n)%sname
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => autotrophs_config(n)%lname
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'Nfixer'
      lname    = 'Flag is true if this autotroph fixes N2'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%Nfixer
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'imp_calcifier'
      lname    = 'Flag is true if this autotroph implicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%imp_calcifier
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'exp_calcifier'
      lname    = 'Flag is true if this autotroph explicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%exp_calcifier
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'silicifier'
      lname    = 'Flag is true if this autotroph is a silicifier'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%silicifier
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

    do n=1, zooplankton_cnt
      write(prefix, "(A,I0,A)") 'zooplankton_config(', n, ')%'
      write(category, "(A,1X,I0)") 'zooplankton_conf', n

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => zooplankton_config(n)%sname
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => zooplankton_config(n)%lname
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if
    end do

    do n=1,zooplankton_cnt
      do m=1,grazer_prey_cnt
        write(prefix, "(A,I0,A,I0,A)") 'grazing_config(', m, ',', n, ')%'
        write(category, "(A,1X,I0,1X,I0)") 'grazing_conf', m, n

        write(sname, "(2A)") trim(prefix), 'sname'
        lname    = 'Short name of grazer'
        units    = 'unitless'
        datatype = 'string'
        group    = 'marbl_config_nml'
        sptr     => grazing_config(m,n)%sname
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, sptr=sptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'lname'
        lname    = 'Long name of grazer'
        units    = 'unitless'
        datatype = 'string'
        group    = 'marbl_config_nml'
        sptr     => grazing_config(m,n)%lname
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, sptr=sptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'auto_ind_cnt'
        lname    = 'number of autotrophs in prey-clase auto_ind'
        units    = 'unitless'
        datatype = 'integer'
        group    = 'marbl_config_nml'
        iptr     => grazing_config(m,n)%auto_ind_cnt
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, iptr=iptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'zoo_ind_cnt'
        lname    = 'number of zooplankton in prey-clase auto_ind'
        units    = 'unitless'
        datatype = 'integer'
        group    = 'marbl_config_nml'
        iptr     => grazing_config(m,n)%zoo_ind_cnt
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, iptr=iptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if
      end do
    end do

  end subroutine marbl_define_config_vars

  !*****************************************************************************

  subroutine marbl_var_add(this, sname, lname, units, datatype, group,        &
                           category, marbl_status_log,                        &
                           rptr, iptr, lptr, sptr, comment)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),        intent(in)    :: sname
    character(len=*),        intent(in)    :: lname
    character(len=*),        intent(in)    :: units
    character(len=*),        intent(in)    :: datatype
    character(len=*),        intent(in)    :: group
    character(len=*),        intent(in)    :: category
    type(marbl_log_type),    intent(inout) :: marbl_status_log
    real(r8),                optional, pointer, intent(in) :: rptr
    integer,                 optional, pointer, intent(in) :: iptr
    logical,                 optional, pointer, intent(in) :: lptr
    character(len=char_len), optional, pointer, intent(in) :: sptr
    character(len=char_len), optional,          intent(in) :: comment

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add'
    type(marbl_single_config_or_parm_type), dimension(:), pointer :: new_parms
    character(len=char_len),                dimension(:), pointer :: new_categories
    integer :: old_size, id, cat_id, n
    character(len=char_len) :: log_message

    if (.not.associated(this%vars)) then
      write(log_message, "(A)") 'Constructor must be run before adding vars'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    old_size = size(this%vars)
    id = old_size+1

    ! 1) Allocate new_parms to be size N (one element larger than this%vars)
    allocate(new_parms(id))

    ! 2) Determine category ID
    do cat_id = 1, size(this%categories)
      if (trim(category) .eq. trim(this%categories(cat_id))) then
        exit
      end if
    end do
    if (cat_id .gt. size(this%categories)) then
      allocate(new_categories(cat_id))
      new_categories(1:size(this%categories)) = this%categories
      new_categories(cat_id) = category
      deallocate(this%categories)
      this%categories => new_categories
    end if

    ! 3) copy this%vars into first N-1 elements of new_parms
    do n=1, old_size
      new_parms(n)%long_name    = this%vars(n)%long_name
      new_parms(n)%short_name   = this%vars(n)%short_name
      new_parms(n)%units        = this%vars(n)%units
      new_parms(n)%datatype     = this%vars(n)%datatype
      new_parms(n)%group        = this%vars(n)%group
      new_parms(n)%category_id  = this%vars(n)%category_id
      new_parms(n)%comment      = this%vars(n)%comment
      if (associated(this%vars(n)%lptr)) &
        new_parms(n)%lptr => this%vars(n)%lptr
      if (associated(this%vars(n)%iptr)) &
        new_parms(n)%iptr => this%vars(n)%iptr
      if (associated(this%vars(n)%rptr)) &
        new_parms(n)%rptr => this%vars(n)%rptr
      if (associated(this%vars(n)%sptr)) &
        new_parms(n)%sptr => this%vars(n)%sptr
    end do

    ! 3) add newest parm variable
    select case (trim(datatype))
      case ('real')
        if (present(rptr)) then
          new_parms(id)%rptr => rptr
        else
          write(log_message, "(A)")                                           &
               "Defining real parameter but rptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('integer')
        if (present(iptr)) then
          new_parms(id)%iptr => iptr
        else
          write(log_message, "(A)")                                           &
               "Defining integer parameter but iptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('logical')
        if (present(lptr)) then
          new_parms(id)%lptr => lptr
        else
          write(log_message, "(A)")                                           &
               "Defining logical parameter but lptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('string')
        if (present(sptr)) then
          new_parms(id)%sptr => sptr
        else
          write(log_message, "(A)")                                           &
               "Defining string parameter but aptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case DEFAULT
        write(log_message, "(2A)") "Unknown datatype: ", trim(datatype)
        call marbl_status_log%log_error(log_message, subname)
        return
    end select
    new_parms(id)%short_name  = trim(sname)
    new_parms(id)%long_name   = trim(lname)
    new_parms(id)%units       = trim(units)
    new_parms(id)%datatype    = trim(datatype)
    new_parms(id)%group       = trim(group)
    new_parms(id)%category_id = cat_id
    if (present(comment)) then
      new_parms(id)%comment = comment
    else
      new_parms(id)%comment = ''
    end if

    ! 4) deallocate / nullify this%vars
    deallocate(this%vars)
    nullify(this%vars)

    ! 5) point this%vars => new_parms and update parms_cnt
    this%vars => new_parms
    this%cnt = id

  end subroutine marbl_var_add

  !*****************************************************************************

  subroutine marbl_var_add_1d_r8(this, sname, lname, units, group, category,  &
                                 r8array, marbl_status_log)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    character(len=char_len),             intent(in)    :: category
    real(kind=r8), dimension(:), target, intent(in)    :: r8array
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_r8'
    character(len=char_len) :: sname_loc
    real(r8), pointer :: rptr => NULL()
    integer :: n

    do n=1,size(r8array)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      rptr => r8array(n)
      call this%add_var(sname_loc, lname, units, 'real', group, category,     &
                          marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_r8

  !*****************************************************************************

  subroutine marbl_var_add_1d_int(this, sname, lname, units, group, category, &
                                  intarray, marbl_status_log)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    character(len=char_len),             intent(in)    :: category
    integer, dimension(:), target,       intent(in)    :: intarray
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_int'
    character(len=char_len) :: sname_loc
    integer, pointer :: iptr => NULL()
    integer :: n

    do n=1,size(intarray)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      iptr => intarray(n)
      call this%add_var(sname_loc, lname, units, 'integer', group, category,  &
                          marbl_status_log, iptr=iptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_int

  !*****************************************************************************

  subroutine marbl_var_add_1d_str(this, sname, lname, units, group, category, &
                                  strarray, marbl_status_log)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    character(len=char_len),             intent(in)    :: category
    character(len=char_len),     target, intent(in)    :: strarray(:)
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_str'
    character(len=char_len) :: sname_loc
    character(len=char_len), pointer :: sptr => NULL()
    integer :: n

    do n=1,size(strarray)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      sptr => strarray(n)
      call this%add_var(sname_loc, lname, units, 'string', group, category,   &
                          marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_str

  !*****************************************************************************

  subroutine marbl_vars_finalize(this, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_vars_finalize'
    character(len=char_len) :: log_message
    character(len=char_len) :: group
    character(len=7)        :: logic
    integer :: i,n, cat_id

    ! (1) Lock data type (put calls will now cause MARBL to abort)
    this%locked = .true.
    group = ''

    do cat_id = 1,size(this%categories)
      do n=1,this%cnt
        if (this%vars(n)%category_id .eq. cat_id) then
          ! (2) Log the group name if different than previous parameter
          if (this%vars(n)%group.ne.group) then
            group = trim(this%vars(n)%group)
            log_message = ''
            do i=1,len(trim(group))
              log_message(i:i) = '-'
            end do
            call marbl_status_log%log_noerror(log_message, subname)
            call marbl_status_log%log_noerror(trim(group), subname)
            call marbl_status_log%log_noerror(log_message, subname)
            call marbl_status_log%log_noerror('', subname)
          end if

        ! (3) write parameter to log_message (format depends on datatype)
          select case(trim(this%vars(n)%datatype))
            case ('string')
              write(log_message, "(4A)") trim(this%vars(n)%short_name), " = '",  &
                                         trim(this%vars(n)%sptr), "'"
            case ('real')
              write(log_message, "(2A,E24.16)") trim(this%vars(n)%short_name),   &
                                                " = ", this%vars(n)%rptr
            case ('integer')
              write(log_message, "(2A,I0)") trim(this%vars(n)%short_name), " = ", &
                                            this%vars(n)%iptr
            case ('logical')
              if (this%vars(n)%lptr) then
                logic = '.true.'
              else
                logic = '.false.'
              end if
              write(log_message, "(3A)") trim(this%vars(n)%short_name), " = ",   &
                                         trim(logic)
            case DEFAULT
              write(log_message, "(2A)") trim(this%vars(n)%datatype),            &
                                         ' is not a valid datatype for parameter'
              call marbl_status_log%log_error(log_message, subname)
              return
          end select

          ! (4) Write log_message to the log
          if (this%vars(n)%comment.ne.'') &
            write(log_message, "(3A)") trim(log_message), ' ! ',                  &
                                       trim(this%vars(n)%comment)
          call marbl_status_log%log_noerror(log_message, subname)
        end if
      end do
      if (cat_id .ne. size(this%categories)) then
        call marbl_status_log%log_noerror('', subname)
      end if
    end do

  end subroutine marbl_vars_finalize

  !***********************************************************************

  subroutine log_add_var_error(marbl_status_log, sname, subname)

    type(marbl_log_type), intent(inout) :: marbl_status_log
    character(len=*),     intent(in)    :: sname
    character(len=*),     intent(in)    :: subname
    character(len=char_len) :: routine_name

    write(routine_name,"(3A)") "this%add_var(", trim(sname), ")"
    call marbl_status_log%log_error_trace(routine_name, subname)

  end subroutine log_add_var_error

  !*****************************************************************************

  subroutine set_derived_config(marbl_status_log)

    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod:set_derived_config'
    character(len=char_len) :: log_message

  end subroutine set_derived_config

  !*****************************************************************************

  subroutine marbl_var_put_all_types(this, var, marbl_status_log, rval, ival, &
                                     lval, sval)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),         optional,         intent(in)    :: rval
    integer,          optional,         intent(in)    :: ival
    logical,          optional,         intent(in)    :: lval
    character(len=*), optional,         intent(in)    :: sval
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod%marbl_var_put_all_types'
    character(len=char_len) :: log_message
    integer :: varid

    if (this%locked) then
      write(log_message, "(3A)") 'Can not change value of ', trim(var),       &
                                 ', parameters are locked!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    varid = this%inquire_id(var, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      write(log_message, "(3A)") 'config_parms%put(', trim(var), ')'
      call marbl_status_log%log_error_trace(log_message, subname)
      return
    end if

    select case(trim(this%vars(varid)%datatype))
      case ('real')
        if (present(rval)) then
          this%vars(varid)%rptr = rval
        else
          write(log_message, "(2A)") trim(var), ' requires real value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('integer')
        if (present(ival)) then
          this%vars(varid)%iptr = ival
        else
          write(log_message, "(2A)") trim(var), ' requires integer value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('logical')
        if (present(lval)) then
          this%vars(varid)%lptr = lval
        else
          write(log_message, "(2A)") trim(var), ' requires logical value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('string')
        if (present(sval)) then
          this%vars(varid)%sptr = sval
        else
          write(log_message, "(2A)") trim(var), ' requires string value'
          call marbl_status_log%log_error(log_message, subname)
        end if
    end select

  end subroutine marbl_var_put_all_types

  !*****************************************************************************

  subroutine marbl_var_get_all_types(this, var, marbl_status_log, rval, ival, &
                                     lval, sval)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),         optional,         intent(out)   :: rval
    integer,          optional,         intent(out)   :: ival
    logical,          optional,         intent(out)   :: lval
    character(len=*), optional,         intent(out)   :: sval
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod%marbl_var_get_all_types'
    character(len=char_len) :: log_message
    integer :: varid, cnt

    cnt = 0
    if (present(rval)) cnt = cnt + 1
    if (present(ival)) cnt = cnt + 1
    if (present(lval)) cnt = cnt + 1
    if (present(sval)) cnt = cnt + 1

    if (cnt .eq. 0) then
      write(log_message, "(A)") 'Must provide rval, ival, lval, or sval to var_get()'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    if (cnt .gt. 1) then
      write(log_message, "(A)") 'Must provide just one of rval, ival, lval, or sval to var_get()'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    varid = this%inquire_id(var, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      write(log_message, "(3A)") 'config_parms%get(', trim(var), ')'
      call marbl_status_log%log_error_trace(log_message, subname)
      return
    end if

    select case(trim(this%vars(varid)%datatype))
      case ('real')
        if (present(rval)) then
          rval = this%vars(varid)%rptr
        else
          write(log_message, "(2A)") trim(var), ' requires real value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('integer')
        if (present(ival)) then
          ival = this%vars(varid)%iptr
        else
          write(log_message, "(2A)") trim(var), ' requires integer value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('logical')
        if (present(lval)) then
          lval = this%vars(varid)%lptr
        else
          write(log_message, "(2A)") trim(var), ' requires logical value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('string')
        if (present(sval)) then
          if (len(sval).lt.len(trim(this%vars(varid)%sptr))) then
            write(log_message, "(2A,I0,A,I0,A)") trim(var), ' requires ',     &
              len(trim(this%vars(varid)%sptr)), ' bytes to store, but only ', &
              len(sval), ' are provided.'
            call marbl_status_log%log_error(log_message, subname)
            return
          end if
          sval = trim(this%vars(varid)%sptr)
        else
          write(log_message, "(2A)") trim(var), ' requires string value'
          call marbl_status_log%log_error(log_message, subname)
        end if
    end select

  end subroutine marbl_var_get_all_types

  !*****************************************************************************

  subroutine marbl_var_put_real(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),                           intent(in)    :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%put_general(var, marbl_status_log, rval = val)

  end subroutine marbl_var_put_real

  !*****************************************************************************

  subroutine marbl_var_put_integer(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    integer,                            intent(in)    :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%put_general(var, marbl_status_log, ival=val, rval=real(val,r8))

  end subroutine marbl_var_put_integer

  !*****************************************************************************

  subroutine marbl_var_put_string(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    character(len=*),                   intent(in)    :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%put_general(var, marbl_status_log, sval=val)

  end subroutine marbl_var_put_string

  !*****************************************************************************

  subroutine marbl_var_put_logical(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    logical,                            intent(in)    :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%put_general(var, marbl_status_log, lval=val)

  end subroutine marbl_var_put_logical

  !*****************************************************************************

  subroutine marbl_var_get_real(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),                           intent(out)   :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%get_general(var, marbl_status_log, rval = val)

  end subroutine marbl_var_get_real

  !*****************************************************************************

  subroutine marbl_var_get_integer(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    integer,                            intent(out)   :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%get_general(var, marbl_status_log, ival=val)

  end subroutine marbl_var_get_integer

  !*****************************************************************************

  subroutine marbl_var_get_string(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    character(len=*),                   intent(out)   :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%get_general(var, marbl_status_log, sval=val)

  end subroutine marbl_var_get_string

  !*****************************************************************************

  subroutine marbl_var_get_logical(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    logical,                            intent(out)   :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%get_general(var, marbl_status_log, lval=val)

  end subroutine marbl_var_get_logical

  !*****************************************************************************

  function marbl_var_inquire_id(this, var, marbl_status_log) result(id)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    type(marbl_log_type),               intent(inout) :: marbl_status_log
    integer                                           :: id

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_inquire_id'
    character(len=char_len) :: log_message
    integer :: n

    id = 0
    do n=1,this%cnt
      if (trim(var).eq.trim(this%vars(n)%short_name)) then
        id = n
        return
      end if
    end do
    write(log_message, "(2A)") trim(var), ' is not a known variable name for put'
    call marbl_status_log%log_error(log_message, subname)

  end function marbl_var_inquire_id

  !*****************************************************************************

  subroutine marbl_var_inquire_metadata(this, ind, marbl_status_log, lname,   &
             sname, units, group, datatype)

    class(marbl_config_and_parms_type), intent(inout) :: this
    integer,                            intent(in)    :: ind
    type(marbl_log_type),               intent(inout) :: marbl_status_log
    character(len=*), optional,         intent(out)   :: lname, sname, units
    character(len=*), optional,         intent(out)   :: group, datatype

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_inquire_metadata'
    character(len=char_len) :: log_message

    if ((ind .lt. 1).or.(ind .gt. this%cnt)) then
      write(log_message,'(I0,2A,I0)') ind, ' is not a valid index: must be ', &
                                      'between 1 and ', this%cnt
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    if (present(lname)) then
      lname = this%vars(ind)%long_name
    end if

    if (present(sname)) then
      sname = this%vars(ind)%short_name
    end if

    if (present(units)) then
      units = this%vars(ind)%units
    end if

    if (present(group)) then
      group = this%vars(ind)%group
    end if

    if (present(datatype)) then
      datatype = this%vars(ind)%datatype
    end if

  end subroutine marbl_var_inquire_metadata

  !*****************************************************************************

end module marbl_config_mod
