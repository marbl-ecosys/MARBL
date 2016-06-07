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

  type, public :: marbl_single_config_var_type
    ! Metadata
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: group
    character(len=char_len) :: datatype
    ! Actual parameter data
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()
  end type marbl_single_config_var_type

  type, public :: marbl_config_vars_type
    integer :: cnt
    type(marbl_single_config_var_type), dimension(:), pointer :: vars => NULL()
  contains
    procedure :: add_var        => marbl_var_add
    procedure :: add_var_1d_r8  => marbl_var_add_1d_r8
    procedure :: add_var_1d_str => marbl_var_add_1d_str
    procedure :: list_vars      => marbl_vars_list
  end type marbl_config_vars_type

  type, extends(marbl_config_vars_type), public :: marbl_config_type
  contains
    procedure :: construct        => marbl_config_construct
  end type marbl_config_type

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

  subroutine marbl_var_add(this, sname, lname, units, datatype, group,        &
             marbl_status_log, rptr, iptr, lptr, sptr)

    class(marbl_config_vars_type), intent(inout) :: this
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

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add'
    type(marbl_single_config_var_type), dimension(:), pointer :: new_parms
    integer :: old_size, id, n
    character(len=char_len) :: log_message

    if (.not.associated(this%vars)) then
      write(log_message, "(A)") 'Parms constructor must be run'
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

    ! 4) deallocate / nullify this%vars
    deallocate(this%vars)
    nullify(this%vars)

    ! 5) point this%vars => new_parms and update parms_cnt
    this%vars => new_parms
    this%cnt = id

  end subroutine marbl_var_add

  !*****************************************************************************

  subroutine marbl_var_add_1d_r8(this, sname, lname, units, group, r8array,  &
                                 marbl_status_log)

    class(marbl_config_vars_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    real(kind=r8), dimension(:), target, intent(in)    :: r8array
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_r8'
    character(len=char_len) :: sname_loc
    real(r8), pointer :: rptr => NULL()
    integer :: n

    do n=1,size(r8array)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      rptr => r8array(n)
      call this%add_var(sname_loc, lname, units, 'real', group,             &
                          marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_r8

  !*****************************************************************************

  subroutine marbl_var_add_1d_str(this, sname, lname, units, group,          &
                                  strarray, marbl_status_log)

    class(marbl_config_vars_type),       intent(inout) :: this
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

    do n=1,size(strarray)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      sptr => strarray(n)
      call this%add_var(sname_loc, lname, units, 'string', group,           &
                          marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_str

  !*****************************************************************************

  subroutine marbl_vars_list(this, ciso_on, marbl_status_log)

    class(marbl_config_vars_type), intent(inout) :: this
    logical,                 intent(in)    :: ciso_on
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_vars_list'
    character(len=char_len) :: log_message
    character(len=char_len) :: group
    character(len=7)        :: logic
    integer :: n

    group = ''
    do n=1,this%cnt
      if (this%vars(n)%group.ne.group) then
        group = trim(this%vars(n)%group)
        if (ciso_on.or.(trim(group).ne.'marbl_ciso_nml')) then
          call marbl_status_log%log_noerror('', subname)
          write(log_message, "(2A)") trim(group), ' parameter values: '
          call marbl_status_log%log_noerror(log_message, subname)
          call marbl_status_log%log_noerror('---', subname)
        end if
      end if
      if (ciso_on.or.(trim(group).ne.'marbl_ciso_nml')) then
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
        call marbl_status_log%log_noerror(log_message, subname)
      end if
    end do

  end subroutine marbl_vars_list

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

  subroutine marbl_config_construct(this, marbl_status_log)

    class(marbl_config_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_config_construct'
    character(len=char_len) :: log_message
    character(len=char_len) :: sname, lname, units, datatype, group
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%parameters has been constructed already"
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
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'auto_names'
    lname     = 'Short names of active autotrophs'
    units     = 'unitless'
    group     = 'marbl_config_nml'
    call this%add_var_1d_str(sname, lname, units, group, auto_names,        &
                               marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_str', subname)
      return
    end if

    sname     = 'zoo_names'
    lname     = 'Short names of active zooplankton'
    units     = 'unitless'
    group     = 'marbl_config_nml'
    call this%add_var_1d_str(sname, lname, units, group, zoo_names,         &
                               marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_str', subname)
      return
    end if

  end subroutine marbl_config_construct

  !*****************************************************************************

end module marbl_config_mod
