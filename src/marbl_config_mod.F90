module marbl_config_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_logging, only : marbl_log_type

  implicit none
  private
  save

  !---------------------------------------------------------------------------
  !  Datatypes for marbl_instance%configuration and marbl_instance%parameters
  !---------------------------------------------------------------------------

  ! Needs to be public so that GCMs can create linked list to request metadata
  type, public :: marbl_single_config_or_parm_ll_type
    ! Metadata
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: group
    character(len=char_len) :: datatype
    integer                 :: category_ind ! used for sorting output list
    character(len=char_len) :: comment      ! used to add comment in log
    type(marbl_single_config_or_parm_ll_type), pointer :: next => NULL()
    ! Actual parameter data
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()
  end type marbl_single_config_or_parm_ll_type

  type, public :: marbl_config_and_parms_type
    logical :: locked = .false.
!    integer :: cnt = 0
    character(len=char_len), dimension(:),     pointer :: categories
    type(marbl_single_config_or_parm_ll_type), pointer :: vars => NULL()
    type(marbl_single_config_or_parm_ll_type), pointer :: LastEntry => NULL()
  contains
    procedure          :: add_var          => marbl_var_add
    procedure          :: add_var_1d_r8    => marbl_var_add_1d_r8
    procedure          :: add_var_1d_int   => marbl_var_add_1d_int
    procedure          :: add_var_1d_str   => marbl_var_add_1d_str
    procedure          :: finalize_vars    => marbl_vars_finalize
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

  public :: log_add_var_error

contains

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

    character(len=*), parameter :: subname = 'marbl_config_mod:marbl_var_add'

    type(marbl_single_config_or_parm_ll_type), pointer :: ll_index => NULL()
    character(len=char_len), dimension(:), pointer :: new_categories
    integer :: cat_ind, n
    character(len=char_len) :: log_message

    ! 1) Determine category ID
    do cat_ind = 1, size(this%categories)
      if (trim(category) .eq. trim(this%categories(cat_ind))) then
        exit
      end if
    end do
    if (cat_ind .gt. size(this%categories)) then
      allocate(new_categories(cat_ind))
      new_categories(1:size(this%categories)) = this%categories
      new_categories(cat_ind) = category
      deallocate(this%categories)
      this%categories => new_categories
    end if

    ! 2) Error checking
    ll_index => this%vars
    do while (associated(ll_index))
      if (trim(sname) .eq. trim(ll_index%short_name)) then
        write(log_message, "(A,1X,A)") trim(sname), "has been added twice"
        call marbl_status_log%log_error(log_message, subname)
      end if

      ! (b) Ensure pointers do not point to same target as other variables
      if (present(rptr)) then
        if (associated(rptr, ll_index%rptr)) then
          write(log_message, "(4A)") trim(sname), " and ", trim(ll_index%short_name), &
                                     " both point to same variable in memory."
          call marbl_status_log%log_error(log_message, subname)
        end if
      end if
      if (present(iptr)) then
        if (associated(iptr, ll_index%iptr)) then
          write(log_message, "(4A)") trim(sname), " and ", trim(ll_index%short_name), &
                                     " both point to same variable in memory."
          call marbl_status_log%log_error(log_message, subname)
        end if
      end if
      if (present(lptr)) then
        if (associated(lptr, ll_index%lptr)) then
          write(log_message, "(4A)") trim(sname), " and ", trim(ll_index%short_name), &
                                     " both point to same variable in memory."
          call marbl_status_log%log_error(log_message, subname)
        end if
      end if
      if (present(sptr)) then
        if (associated(sptr, ll_index%sptr)) then
          write(log_message, "(4A)") trim(sname), " and ", trim(ll_index%short_name), &
                                     " both point to same variable in memory."
          call marbl_status_log%log_error(log_message, subname)
        end if
      end if

      if (marbl_status_log%labort_marbl) return
      ll_index => ll_index%next
    end do

    ! 3) Create new entry
    !    All pointer components of ll_index are nullified in the type definition
    !    via => NULL() statements
    allocate(ll_index)
    select case (trim(datatype))
      case ('real')
        if (present(rptr)) then
          ll_index%rptr => rptr
        else
          write(log_message, "(A)")                                           &
               "Defining real parameter but rptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('integer')
        if (present(iptr)) then
          ll_index%iptr => iptr
        else
          write(log_message, "(A)")                                           &
               "Defining integer parameter but iptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('logical')
        if (present(lptr)) then
          ll_index%lptr => lptr
        else
          write(log_message, "(A)")                                           &
               "Defining logical parameter but lptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('string')
        if (present(sptr)) then
          ll_index%sptr => sptr
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
    ll_index%short_name   = trim(sname)
    ll_index%long_name    = trim(lname)
    ll_index%units        = trim(units)
    ll_index%datatype     = trim(datatype)
    ll_index%group        = trim(group)
    ll_index%category_ind = cat_ind
    if (present(comment)) then
      ll_index%comment = comment
    else
      ll_index%comment = ''
    end if

    ! 4) Append new entry to list
    if (.not.associated(this%vars)) then
      this%vars => ll_index
    else
      this%LastEntry%next => ll_index
    end if
    this%LastEntry => ll_index

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

    character(len=*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_r8'

    character(len=char_len) :: sname_loc
    real(r8), pointer       :: rptr => NULL()
    integer                 :: n

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

    character(len=*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_int'

    character(len=char_len) :: sname_loc
    integer, pointer        :: iptr => NULL()
    integer                 :: n

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

    character(len=*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_str'

    character(len=char_len)          :: sname_loc
    character(len=char_len), pointer :: sptr => NULL()
    integer                          :: n

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
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod:marbl_vars_finalize'
    character(len=char_len)     :: log_message

    character(len=char_len) :: group
    character(len=7)        :: logic
    integer                 :: i, cat_ind
    type(marbl_single_config_or_parm_ll_type), pointer :: ll_index => NULL()

    ! (1) Lock data type (put calls will now cause MARBL to abort)
    this%locked = .true.
    group = ''

    do cat_ind = 1,size(this%categories)
      ll_index => this%vars
      do while (associated(ll_index))
        if (ll_index%category_ind .eq. cat_ind) then
          ! (2) Log the group name if different than previous parameter
          if (ll_index%group.ne.group) then
            group = trim(ll_index%group)
            call marbl_status_log%log_header(trim(group), subname)
          end if

        ! (3) write parameter to log_message (format depends on datatype)
          select case(trim(ll_index%datatype))
            case ('string')
              write(log_message, "(4A)") trim(ll_index%short_name), " = '",  &
                                         trim(ll_index%sptr), "'"
            case ('real')
              write(log_message, "(2A,E24.16)") trim(ll_index%short_name),   &
                                                " = ", ll_index%rptr
            case ('integer')
              write(log_message, "(2A,I0)") trim(ll_index%short_name), " = ", &
                                            ll_index%iptr
            case ('logical')
              if (ll_index%lptr) then
                logic = '.true.'
              else
                logic = '.false.'
              end if
              write(log_message, "(3A)") trim(ll_index%short_name), " = ",   &
                                         trim(logic)
            case DEFAULT
              write(log_message, "(2A)") trim(ll_index%datatype),            &
                                         ' is not a valid datatype for parameter'
              call marbl_status_log%log_error(log_message, subname)
              return
          end select

          ! (4) Write log_message to the log
          if (ll_index%comment.ne.'') then
            if (len_trim(log_message) + 3 + len_trim(ll_index%comment) .le. len(log_message)) then
              write(log_message, "(3A)") trim(log_message), ' ! ',                  &
                                         trim(ll_index%comment)
            else
              call marbl_status_log%log_noerror(&
                   '! WARNING: omitting comment on line below because including it exceeds max length for log message', &
                   subname)
            end if
          endif
          call marbl_status_log%log_noerror(log_message, subname)
        end if
        ll_index => ll_index%next
      end do  ! ll_index
      if (cat_ind .ne. size(this%categories)) then
        call marbl_status_log%log_noerror('', subname)
      end if
    end do  ! cat_ind

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

  subroutine marbl_var_put_all_types(this, var, marbl_status_log, rval, ival, &
                                     lval, sval)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),         optional,         intent(in)    :: rval
    integer,          optional,         intent(in)    :: ival
    logical,          optional,         intent(in)    :: lval
    character(len=*), optional,         intent(in)    :: sval
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod%marbl_var_put_all_types'
    character(len=char_len)     :: log_message

    type(marbl_single_config_or_parm_ll_type), pointer :: ll_index => NULL()

    if (this%locked) then
      write(log_message, "(3A)") 'Can not change value of ', trim(var),       &
                                 ', parameters are locked!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    ll_index => this%vars
    do while (associated(ll_index))
      if (trim(ll_index%short_name) .eq. trim(var)) exit
      ll_index => ll_index%next
    end do
    if (.not.associated(ll_index)) then
      write(log_message, "(2A)") trim(var), 'not found!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    select case(trim(ll_index%datatype))
      case ('real')
        if (present(rval)) then
          ll_index%rptr = rval
        else
          write(log_message, "(2A)") trim(var), ' requires real value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('integer')
        if (present(ival)) then
          ll_index%iptr = ival
        else
          write(log_message, "(2A)") trim(var), ' requires integer value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('logical')
        if (present(lval)) then
          ll_index%lptr = lval
        else
          write(log_message, "(2A)") trim(var), ' requires logical value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('string')
        if (present(sval)) then
          ll_index%sptr = sval
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

    character(len=*), parameter :: subname = 'marbl_config_mod%marbl_var_get_all_types'
    character(len=char_len)     :: log_message

    type(marbl_single_config_or_parm_ll_type), pointer :: ll_index => NULL()
    integer :: cnt

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

    ll_index => this%vars
    do while (associated(ll_index))
      if (trim(ll_index%short_name) .eq. trim(var)) exit
      ll_index => ll_index%next
    end do
    if (.not.associated(ll_index)) then
      write(log_message, "(2A)") trim(var), 'not found!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    select case(trim(ll_index%datatype))
      case ('real')
        if (present(rval)) then
          rval = ll_index%rptr
        else
          write(log_message, "(2A)") trim(var), ' requires real value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('integer')
        if (present(ival)) then
          ival = ll_index%iptr
        else
          write(log_message, "(2A)") trim(var), ' requires integer value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('logical')
        if (present(lval)) then
          lval = ll_index%lptr
        else
          write(log_message, "(2A)") trim(var), ' requires logical value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('string')
        if (present(sval)) then
          if (len(sval).lt.len(trim(ll_index%sptr))) then
            write(log_message, "(2A,I0,A,I0,A)") trim(var), ' requires ',     &
              len(trim(ll_index%sptr)), ' bytes to store, but only ', &
              len(sval), ' are provided.'
            call marbl_status_log%log_error(log_message, subname)
            return
          end if
          sval = trim(ll_index%sptr)
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

  subroutine marbl_var_inquire_metadata(this, var, marbl_status_log, lname,   &
             units, group, datatype)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    type(marbl_log_type),               intent(inout) :: marbl_status_log
    character(len=*), optional,         intent(out)   :: lname, units
    character(len=*), optional,         intent(out)   :: group, datatype

    character(len=*), parameter :: subname = 'marbl_config_mod:marbl_var_inquire_metadata'
    character(len=char_len)     :: log_message

    type(marbl_single_config_or_parm_ll_type), pointer :: ll_index => NULL()

    ll_index => this%vars
    do while (associated(ll_index))
      if (trim(ll_index%short_name) .eq. trim(var)) exit
      ll_index => ll_index%next
    end do
    if (.not.associated(ll_index)) then
      write(log_message, "(2A)") trim(var), 'not found!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    if (present(lname)) then
      lname = ll_index%long_name
    end if

    if (present(units)) then
      units = ll_index%units
    end if

    if (present(group)) then
      group = ll_index%group
    end if

    if (present(datatype)) then
      datatype = ll_index%datatype
    end if

  end subroutine marbl_var_inquire_metadata

  !*****************************************************************************

end module marbl_config_mod
