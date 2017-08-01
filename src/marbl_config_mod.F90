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

  type, private :: marbl_single_setting_ll_type
    ! Metadata
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: group
    character(len=char_len) :: datatype
    integer                 :: category_ind ! used for sorting output list
    character(len=char_len) :: comment      ! used to add comment in log
    type(marbl_single_setting_ll_type), pointer :: next => NULL()
    ! Actual parameter data
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()
  end type marbl_single_setting_ll_type

  type, private :: marbl_setting_ptr
    type(marbl_single_setting_ll_type), pointer :: ptr => NULL()
  end type marbl_setting_ptr

  type, public :: marbl_settings_type
    logical, private :: init_called = .false.
    integer, private :: cnt = 0
    character(len=char_len), dimension(:),              pointer :: categories
    type(marbl_single_setting_ll_type),          pointer :: vars => NULL()
    type(marbl_single_setting_ll_type), private, pointer :: LastEntry => NULL()
    type(marbl_single_setting_ll_type), private, pointer :: user_supplied => NULL()
    type(marbl_single_setting_ll_type), private, pointer :: LastUserSupplied => NULL()
    type(marbl_setting_ptr), dimension(:),     private, allocatable :: varArray
  contains
    procedure :: add_var
    procedure :: add_var_1d_r8
    procedure :: add_var_1d_int
    procedure :: add_var_1d_str
    procedure :: finalize_vars
    procedure :: inquire_id
    procedure :: inquire_metadata
    procedure :: get_cnt
    procedure :: put
    procedure :: get
  end type marbl_settings_type

  public :: log_add_var_error

contains

  !*****************************************************************************

  subroutine add_var(this, sname, lname, units, datatype, group, category,    &
                     marbl_status_log, rptr, iptr, lptr, sptr, comment)

    class(marbl_settings_type),                 intent(inout) :: this
    character(len=*),                           intent(in)    :: sname
    character(len=*),                           intent(in)    :: lname
    character(len=*),                           intent(in)    :: units
    character(len=*),                           intent(in)    :: datatype
    character(len=*),                           intent(in)    :: group
    character(len=*),                           intent(in)    :: category
    type(marbl_log_type),                       intent(inout) :: marbl_status_log
    real(r8),                optional, pointer, intent(in) :: rptr
    integer,                 optional, pointer, intent(in) :: iptr
    logical,                 optional, pointer, intent(in) :: lptr
    character(len=char_len), optional, pointer, intent(in) :: sptr
    character(len=char_len), optional,          intent(in) :: comment

    character(len=*), parameter :: subname = 'marbl_config_mod:add_var'

    type(marbl_single_setting_ll_type), pointer :: new_entry, ll_index
    character(len=char_len), dimension(:), pointer :: new_categories
    integer :: cat_ind, n
    character(len=char_len) :: log_message
    logical :: put_success

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
    !    All pointer components of new_entry are nullified in the type definition
    !    via => NULL() statements
    allocate(new_entry)
    select case (trim(datatype))
      case ('real')
        if (present(rptr)) then
          new_entry%rptr => rptr
        else
          write(log_message, "(A)")                                           &
               "Defining real parameter but rptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('integer')
        if (present(iptr)) then
          new_entry%iptr => iptr
        else
          write(log_message, "(A)")                                           &
               "Defining integer parameter but iptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('logical')
        if (present(lptr)) then
          new_entry%lptr => lptr
        else
          write(log_message, "(A)")                                           &
               "Defining logical parameter but lptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('string')
        if (present(sptr)) then
          new_entry%sptr => sptr
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
    new_entry%short_name   = trim(sname)
    new_entry%long_name    = trim(lname)
    new_entry%units        = trim(units)
    new_entry%datatype     = trim(datatype)
    new_entry%group        = trim(group)
    new_entry%category_ind = cat_ind
    if (present(comment)) then
      new_entry%comment = comment
    else
      new_entry%comment = ''
    end if

    ! 4) Append new entry to list
    if (.not.associated(this%vars)) then
      this%vars => new_entry
    else
      this%LastEntry%next => new_entry
    end if
    this%LastEntry => new_entry

    ! 5) Was there a put() call to change this variable?
    !    note that "new_entry" is a dummy pointer used to track ll_index%prev
    ll_index => this%user_supplied
    nullify(new_entry)
    do while (associated(ll_index))
      if (ll_index%short_name .eq. this%LastEntry%short_name) then
        ! 5a) Update variable value
        if (associated(ll_index%iptr).and.associated(this%LastEntry%rptr)) then
          this%LastEntry%rptr = real(ll_index%iptr,r8)
        else
          select case (this%LastEntry%datatype)
          case ("real")
            put_success = associated(ll_index%rptr)
            if (put_success) this%LastEntry%rptr = ll_index%rptr
          case ("integer")
            put_success = associated(ll_index%iptr)
            if (put_success) this%LastEntry%iptr = ll_index%iptr
          case ("string")
            put_success = associated(ll_index%sptr)
            if (put_success) this%LastEntry%sptr = ll_index%sptr
          case ("logical")
            put_success = associated(ll_index%lptr)
            if (put_success) this%LastEntry%lptr = ll_index%lptr
          end select
          if (.not.put_success) then
            write(log_message, "(2A)") "Datatype does not match for put ", &
                               trim(ll_index%short_name)
            call marbl_status_log%log_error(log_message, subname)
            return
          end if
        end if

        ! 5b) Remove entry from user_supplied list
        !     Different procedure if ll_index is first entry in list
        if (associated(ll_index,this%user_supplied)) then
          this%user_supplied => ll_index%next
          deallocate(ll_index)
          ll_index => this%user_supplied
        else
          new_entry%next => ll_index%next
          deallocate(ll_index)
          ll_index => new_entry%next
        end if
      else
        ! Once we are past first entry, new entry%next = ll_index
        new_entry => ll_index
        ll_index => ll_index%next
      end if
    end do

    ! 6) Increment count
    this%cnt = this%cnt + 1

  end subroutine add_var

  !*****************************************************************************

  subroutine add_var_1d_r8(this, sname, lname, units, group, category,        &
                           r8array, marbl_status_log)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    character(len=char_len),             intent(in)    :: category
    real(kind=r8), dimension(:), target, intent(in)    :: r8array
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod:add_var_1d_r8'

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

  end subroutine add_var_1d_r8

  !*****************************************************************************

  subroutine add_var_1d_int(this, sname, lname, units, group, category,       &
                            intarray, marbl_status_log)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    character(len=char_len),             intent(in)    :: category
    integer, dimension(:), target,       intent(in)    :: intarray
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod:add_var_1d_int'

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

  end subroutine add_var_1d_int

  !*****************************************************************************

  subroutine add_var_1d_str(this, sname, lname, units, group, category,       &
                            strarray, marbl_status_log)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    character(len=char_len),             intent(in)    :: category
    character(len=char_len),     target, intent(in)    :: strarray(:)
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod:add_var_1d_str'

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

  end subroutine add_var_1d_str

  !*****************************************************************************

  subroutine finalize_vars(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod:finalize_vars'
    character(len=char_len)     :: log_message

    character(len=7)        :: logic
    integer                 :: i, cat_ind
    type(marbl_single_setting_ll_type), pointer :: ll_index

    ! (1) Lock data type (put calls will now cause MARBL to abort)
    this%init_called = .true.

    ! (2) Abort if anything is left in this%user_supplied
    if (associated(this%user_supplied)) then
      ll_index => this%user_supplied
      do while (associated(ll_index))
        write(log_message, "(3A)") trim(ll_index%short_name), ' was put() but not set!'
        call marbl_status_log%log_error(log_message, subname)
        ll_index => ll_index%next
      end do
      return
    end if

    call marbl_status_log%log_header("Tunable Parameters", subname)

    do cat_ind = 1,size(this%categories)
      ll_index => this%vars
      do while (associated(ll_index))
        if (ll_index%category_ind .eq. cat_ind) then
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

    ! (5) Set up array of pointers
    if (allocated(this%varArray)) then
      write(log_message, "(A)") "Already allocated memory for varArray!"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if
    allocate(this%varArray(this%cnt))
    ll_index => this%vars
    do i = 1,this%cnt
      this%varArray(i)%ptr => ll_index
      ll_index => ll_index%next
    end do

  end subroutine finalize_vars

  !*****************************************************************************

  subroutine put(this, var, marbl_status_log, rval, ival, lval, sval)

    class(marbl_settings_type), intent(inout) :: this
    character(len=*),           intent(in)    :: var
    type(marbl_log_type),       intent(inout) :: marbl_status_log
    real(r8),         optional, intent(in)    :: rval
    integer,          optional, intent(in)    :: ival
    logical,          optional, intent(in)    :: lval
    character(len=*), optional, intent(in)    :: sval

    type(marbl_single_setting_ll_type), pointer :: new_entry, ll_index
    character(len=*), parameter :: subname = 'marbl_config_mod:put'
    character(len=char_len) :: log_message

    if (this%init_called) then
      write(log_message, "(3A)") "Can not put ", trim(var), ", init has already been called"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    allocate(new_entry)
    new_entry%short_name = var

    if (present(rval)) then
      allocate(new_entry%rptr)
      new_entry%rptr = rval
      new_entry%datatype = 'real'
    end if
    if (present(ival)) then
      allocate(new_entry%iptr)
      new_entry%iptr = ival
      new_entry%datatype = 'integer'
    end if
    if (present(lval)) then
      allocate(new_entry%lptr)
      new_entry%lptr = lval
      new_entry%datatype = 'logical'
    end if
    if (present(sval)) then
      allocate(new_entry%sptr)
      new_entry%sptr = sval
      new_entry%datatype = 'string'
    end if

    if (.not.associated(this%user_supplied)) then
      this%user_supplied => new_entry
    else
      this%LastUserSupplied%next => new_entry
    end if
    this%LastUserSupplied => new_entry

  end subroutine put

  !*****************************************************************************

  subroutine get(this, var, marbl_status_log, rval, ival, lval, sval)

    class(marbl_settings_type), intent(in)    :: this
    character(len=*),           intent(in)    :: var
    real(r8),         optional, intent(out)   :: rval
    integer,          optional, intent(out)   :: ival
    logical,          optional, intent(out)   :: lval
    character(len=*), optional, intent(out)   :: sval
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod%get'
    character(len=char_len)     :: log_message

    type(marbl_single_setting_ll_type), pointer :: ll_index
    integer :: cnt

    cnt = 0
    if (present(rval)) cnt = cnt + 1
    if (present(ival)) cnt = cnt + 1
    if (present(lval)) cnt = cnt + 1
    if (present(sval)) cnt = cnt + 1

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

  end subroutine get

  !*****************************************************************************

  function get_cnt(this) result(cnt)
    class(marbl_settings_type), intent(in) :: this
    integer(int_kind) :: cnt

    cnt = this%cnt

  end function get_cnt

  !*****************************************************************************

  function inquire_id(this, var, marbl_status_log) result(id)

    class(marbl_settings_type), intent(in)    :: this
    character(len=*),           intent(in)    :: var
    type(marbl_log_type),       intent(inout) :: marbl_status_log
    integer(int_kind) :: id

    character(len=*), parameter :: subname = 'marbl_config_mod:inquire_id'
    character(len=char_len)     :: log_message
    integer(int_kind) :: n

    id = -1
    do n=1,this%cnt
      if (trim(var).eq.trim(this%varArray(n)%ptr%short_name)) then
        id = n
        return
      end if
    end do

    write(log_message, "(2A)") "No match for variable named ", trim(var)
    call marbl_status_log%log_error(log_message, subname)

  end function inquire_id

  !*****************************************************************************

  subroutine inquire_metadata(this, id, marbl_status_log, sname, lname, units, &
                              group, datatype)

    class(marbl_settings_type), intent(in)    :: this
    integer(int_kind),          intent(in)    :: id
    type(marbl_log_type),       intent(inout) :: marbl_status_log
    character(len=*), optional, intent(out)   :: sname, lname, units
    character(len=*), optional, intent(out)   :: group, datatype

    character(len=*), parameter :: subname = 'marbl_config_mod:inquire_metadata'
    character(len=char_len)     :: log_message

    if (present(sname)) then
      sname = this%varArray(id)%ptr%short_name
    end if

    if (present(lname)) then
      lname = this%varArray(id)%ptr%long_name
    end if

    if (present(units)) then
      units = this%varArray(id)%ptr%units
    end if

    if (present(group)) then
      group = this%varArray(id)%ptr%group
    end if

    if (present(datatype)) then
      datatype = this%varArray(id)%ptr%datatype
    end if

  end subroutine inquire_metadata

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
