module marbl_config_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt

  use marbl_logging, only : marbl_log_type

  use marbl_constants_mod, only : dps

  use marbl_internal_types, only : autotroph_config_type
  use marbl_internal_types, only : zooplankton_config_type

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
  type(autotroph_config_type), dimension(autotroph_cnt), target :: autotrophs_config
  type(zooplankton_config_type), dimension(zooplankton_cnt), target :: zooplankton_config

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
    logical                 :: add_space ! used for formatting log output
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
    type(marbl_single_config_or_parm_type), dimension(:), pointer :: vars => NULL()
  contains
    procedure :: add_var        => marbl_var_add
    procedure :: add_var_tcr_rd => marbl_var_add_tcr_rd
    procedure :: add_var_1d_r8  => marbl_var_add_1d_r8
    procedure :: add_var_1d_int => marbl_var_add_1d_int
    procedure :: add_var_1d_str => marbl_var_add_1d_str
    procedure :: lock_and_log   => marbl_vars_lock_and_log
  end type marbl_config_and_parms_type

  !*****************************************************************************

  private :: r8, int_kind, log_kind, char_len
  private :: autotroph_cnt, zooplankton_cnt
  private :: marbl_log_type

contains

  !*****************************************************************************

  subroutine marbl_config_set_defaults()

    integer :: n

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

  subroutine marbl_config_construct(this, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_config_construct'
    character(len=char_len) :: log_message
    character(len=char_len) :: sname, lname, units, datatype, group
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    integer :: n
    character(len=char_len) :: prefix

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%configuration has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    this%cnt = 0
    allocate(this%vars(this%cnt))

    !------------------!
    ! marbl_config_nml !
    !------------------!

    sname     = 'lsource_sink'
    lname     = 'Control which portions of code are executed (useful for debugging)'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lsource_sink
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr, add_space=.true.)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    do n=1,autotroph_cnt
      write(prefix, "(A,I0,A)") 'autotrophs_config(', n, ')%'

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => autotrophs_config(n)%sname
      call this%add_var(sname, lname, units, datatype, group,               &
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
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%Nfixer
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
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%imp_calcifier
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
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%exp_calcifier
      call this%add_var(sname, lname, units, datatype, group,               &
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
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, lptr=lptr, add_space=.true.)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

    do n=1, zooplankton_cnt
      write(prefix, "(A,I0,A)") 'zooplankton_config(', n, ')%'

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => zooplankton_config(n)%sname
      call this%add_var(sname, lname, units, datatype, group,               &
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
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

  end subroutine marbl_config_construct

  !*****************************************************************************

  subroutine marbl_var_add(this, sname, lname, units, datatype, group,        &
             marbl_status_log, rptr, iptr, lptr, sptr, comment, add_space)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),        intent(in)    :: sname
    character(len=*),        intent(in)    :: lname
    character(len=*),        intent(in)    :: units
    character(len=*),        intent(in)    :: datatype
    character(len=*),        intent(in)    :: group
    type(marbl_log_type),    intent(inout) :: marbl_status_log
    real(r8),                optional, pointer, intent(in) :: rptr
    integer,                 optional, pointer, intent(in) :: iptr
    logical,                 optional, pointer, intent(in) :: lptr
    character(len=char_len), optional, pointer, intent(in) :: sptr
    character(len=char_len), optional,          intent(in) :: comment
    logical,                 optional,          intent(in) :: add_space

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add'
    type(marbl_single_config_or_parm_type), dimension(:), pointer :: new_parms
    integer :: old_size, id, n
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

    ! 2) copy this%vars into first N-1 elements of new_parms
    do n=1, old_size
      new_parms(n)%long_name  = this%vars(n)%long_name
      new_parms(n)%short_name = this%vars(n)%short_name
      new_parms(n)%units      = this%vars(n)%units
      new_parms(n)%datatype   = this%vars(n)%datatype
      new_parms(n)%group      = this%vars(n)%group
      new_parms(n)%add_space  = this%vars(n)%add_space
      new_parms(n)%comment    = this%vars(n)%comment
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
    new_parms(id)%short_name = trim(sname)
    new_parms(id)%long_name  = trim(lname)
    new_parms(id)%units      = trim(units)
    new_parms(id)%datatype   = trim(datatype)
    new_parms(id)%group      = trim(group)
    if (present(add_space)) then
      new_parms(id)%add_space = add_space
    else
      new_parms(id)%add_space = .false.
    end if
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

  subroutine marbl_var_add_tcr_rd(this, sname, group, tracer_read_var,        &
                                 marbl_status_log)

    use marbl_interface_types, only : marbl_tracer_read_type

    class(marbl_config_and_parms_type),        intent(inout) :: this
    character(len=char_len),              intent(in)    :: sname
    character(len=char_len),              intent(in)    :: group
    type(marbl_tracer_read_type), target, intent(in)    :: tracer_read_var
    type(marbl_log_type),                 intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_tcr_rd'

    character(len=char_len)          :: sname_loc
    character(len=char_len)          :: lname
    character(len=char_len)          :: units
    character(len=char_len)          :: datatype
    character(len=char_len)          :: prefix
    character(len=char_len), pointer :: sptr => NULL()
    real(r8),                pointer :: rptr => NULL()

    write(prefix,"(2A)") trim(sname), '%'

    write(sname_loc, "(2A)") trim(prefix), 'mod_varname'
    lname     = 'Variable name in module'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => tracer_read_var%mod_varname
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'filename'
    lname     = 'File containing initial tracer values'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => tracer_read_var%filename
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'file_varname'
    lname     = 'Tracer variable name in filename'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => tracer_read_var%file_varname
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'file_fmt'
    lname     = 'Format of filename'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => tracer_read_var%file_fmt
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'scale_factor'
    lname     = 'Scale factor to use when reading tracer from file'
    units     = 'unitless (or conversion)'
    datatype  = 'real'
    rptr      => tracer_read_var%scale_factor
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'default_val'
    lname     = 'Initial value of tracer if no file provided'
    units     = 'tracer units'
    datatype  = 'real'
    rptr      => tracer_read_var%default_val
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, rptr=rptr, add_space=.true.)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

  end subroutine marbl_var_add_tcr_rd

  !*****************************************************************************

  subroutine marbl_var_add_1d_r8(this, sname, lname, units, group, r8array,  &
                                 marbl_status_log, add_space)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    real(kind=r8), dimension(:), target, intent(in)    :: r8array
    type(marbl_log_type),                intent(inout) :: marbl_status_log
    logical, optional,                   intent(in)    :: add_space

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_r8'
    character(len=char_len) :: sname_loc
    real(r8), pointer :: rptr => NULL()
    logical :: space
    integer :: n

    do n=1,size(r8array)
      if (present(add_space)) then
        space = add_space.and.(n.eq.size(r8array))
      else
        space = .false.
      end if
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      rptr => r8array(n)
      call this%add_var(sname_loc, lname, units, 'real', group,             &
                          marbl_status_log, rptr=rptr, add_space=space)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_r8

  !*****************************************************************************

  subroutine marbl_var_add_1d_int(this, sname, lname, units, group, intarray, &
                                 marbl_status_log, add_space)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    integer, dimension(:), target,       intent(in)    :: intarray
    type(marbl_log_type),                intent(inout) :: marbl_status_log
    logical, optional,                   intent(in)    :: add_space

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_int'
    character(len=char_len) :: sname_loc
    integer, pointer :: iptr => NULL()
    integer :: n
    logical :: space

    do n=1,size(intarray)
      if (present(add_space)) then
        space = add_space.and.(n.eq.size(intarray))
      else
        space = .false.
      end if
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      iptr => intarray(n)
      call this%add_var(sname_loc, lname, units, 'integer', group,            &
                          marbl_status_log, iptr=iptr, add_space=space)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_int

  !*****************************************************************************

  subroutine marbl_var_add_1d_str(this, sname, lname, units, group,          &
                                  strarray, marbl_status_log)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    character(len=char_len),     target, intent(in)    :: strarray(:)
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_str'
    character(len=char_len) :: sname_loc
    character(len=char_len), pointer :: sptr => NULL()
    integer :: n
    logical :: islast

    do n=1,size(strarray)
      islast = (n.eq.size(strarray))
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      sptr => strarray(n)
      call this%add_var(sname_loc, lname, units, 'string', group,           &
                          marbl_status_log, sptr=sptr, add_space=islast)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_str

  !*****************************************************************************

  subroutine marbl_vars_lock_and_log(this, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_vars_lock_and_log'
    character(len=char_len) :: log_message
    character(len=char_len) :: group
    character(len=7)        :: logic
    integer :: i,n

    ! (1) Lock data type (put calls will now cause MARBL to abort)
    this%locked = .true.
    group = ''

    do n=1,this%cnt
      ! (2) Log the group name if different than previous parameter
      if (this%vars(n)%group.ne.group) then
        group = trim(this%vars(n)%group)
        call marbl_status_log%log_noerror('', subname)
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
!          print*, trim(this%vars(n)%short_name), len(trim(this%vars(n)%sptr))
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
      if (this%vars(n)%add_space) &
        call marbl_status_log%log_noerror('', subname)
    end do

  end subroutine marbl_vars_lock_and_log

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

end module marbl_config_mod
