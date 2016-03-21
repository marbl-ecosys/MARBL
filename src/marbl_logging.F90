module marbl_logging

  use marbl_kinds_mod, only : char_len
  use marbl_namelist_mod, only : marbl_nl_buffer_size
  implicit none
  private
  save

  integer, parameter, private :: marbl_log_len = marbl_nl_buffer_size

  ! MNL TO-DO: do we want a wrapper class that contains a linked list of
  !            marbl_log_type? Then labort_marbl can be in the wrapped class
  !            (and so can the status level definitions); would make this more
  !            in line with the diagnostic and forcing field types, as well
  type, public :: marbl_status_log_entry_type
    integer :: ElementInd
    logical :: lall_tasks  ! True => message should be written to stdout by
                           !         all tasks; False => master task only
    character(len=marbl_log_len) :: LogMessage   ! Message text
    character(len=char_len)      :: CodeLocation ! Information on where log was written
                               
    type(marbl_status_log_entry_type), pointer :: next
  end type marbl_status_log_entry_type

  type, private :: marbl_log_output_options_type
    logical :: labort_on_warning ! True => elevate Warnings to Errors
    logical :: lLogVerbose       ! Debugging output should be given Verbose label
    logical :: lLogNamelist      ! Write namelists to log?
    logical :: lLogGeneral       ! General diagnostic output
    logical :: lLogWarning       ! Warnings (can be elevated to errors)
    logical :: lLogError         ! Errors (will toggle labort_marbl whether log
                                 ! is written or not)
  contains
    procedure :: construct => marbl_output_options_constructor
  end type marbl_log_output_options_type

  type, public :: marbl_log_type
    logical, public :: labort_marbl ! True => driver should abort GCM
    type(marbl_log_output_options_type) :: OutputOptions
    type(marbl_status_log_entry_type), pointer :: FullLog
    type(marbl_status_log_entry_type), pointer :: LastEntry
  contains
    procedure, public :: construct => marbl_log_constructor
    procedure, public :: log_namelist => marbl_log_namelist
    procedure, public :: log_error    => marbl_log_error
    procedure, public :: log_noerror  => marbl_log_noerror
    procedure, public :: erase => marbl_log_erase
  end type marbl_log_type

  character(len=char_len), public :: error_msg
  character(len=char_len), public :: status_msg

contains

  subroutine marbl_output_options_constructor(this, labort_on_warning, LogVerbose, LogNamelist, &
                                              LogGeneral, LogWarning, LogError)

    class(marbl_log_output_options_type), intent(inout) :: this
    logical, intent(in), optional :: labort_on_warning, LogVerbose, LogNamelist
    logical, intent(in), optional :: LogGeneral, LogWarning, LogError

    if (present(labort_on_warning)) then
      this%labort_on_warning = labort_on_warning
    else
      this%labort_on_warning = .false.
    end if

    if (present(LogVerbose)) then
      this%lLogVerbose = LogVerbose
    else
      this%lLogVerbose = .false.
    end if

    if (present(LogNamelist)) then
      this%lLogNamelist = LogNamelist
    else
      this%lLogNamelist = .true.
    end if

    if (present(LogGeneral)) then
      this%lLogGeneral = LogGeneral
    else
      this%lLogGeneral = .true.
    end if

    if (present(LogWarning)) then
      this%lLogWarning = LogWarning
    else
      this%lLogWarning = .true.
    end if

    if (present(LogError)) then
      this%lLogError = LogError
    else
      this%lLogError = .true.
    end if

  end subroutine marbl_output_options_constructor

  subroutine marbl_log_constructor(this)

    class(marbl_log_type), intent(inout) :: this

    this%labort_marbl = .false.
    nullify(this%FullLog)
    nullify(this%LastEntry)
    call this%OutputOptions%construct()

  end subroutine marbl_log_constructor

  subroutine marbl_log_namelist(this, NamelistName, NamelistContents, CodeLoc, ElemInd)

    class(marbl_log_type), intent(inout) :: this
    character(len=*),      intent(in)    :: NamelistName, NamelistContents, CodeLoc
    integer, optional,     intent(in)    :: ElemInd
    type(marbl_status_log_entry_type), pointer :: new_entry

    ! Only allocate memory and add entry if we want to log full namelist!
    if (.not.this%OutputOptions%lLogNamelist) then
      return
    end if

    allocate(new_entry)
    nullify(new_entry%next)
    if (present(ElemInd)) then
      new_entry%ElementInd = ElemInd
    else
      new_entry%ElementInd = 1
    end if
    write(new_entry%LogMessage, "(4A)") "Contents of ", trim(NamelistName), ": ", &
                                        trim(NamelistContents)
    new_entry%CodeLocation = trim(CodeLoc)
    new_entry%lall_tasks = .false.

    if (associated(this%FullLog)) then
      ! Append new entry to last entry in the log
      this%LastEntry%next => new_entry
    else
      this%FullLog => new_entry
    end if
    ! Update LastEntry attribute of linked list
    this%LastEntry => new_entry

  end subroutine marbl_log_namelist

  subroutine marbl_log_error(this, ErrorMsg, CodeLoc, ElemInd)

    class(marbl_log_type), intent(inout) :: this
    character(len=*),      intent(in)    :: ErrorMsg, CodeLoc
    integer, optional,     intent(in)    :: ElemInd
    type(marbl_status_log_entry_type), pointer :: new_entry

    ! Only allocate memory and add entry if we want to log full namelist!
    this%labort_marbl = .true.
    if (.not.this%OutputOptions%lLogError) then
      return
    end if

    allocate(new_entry)
    nullify(new_entry%next)
    if (present(ElemInd)) then
      new_entry%ElementInd = ElemInd
    else
      new_entry%ElementInd = 1
    end if
    write(new_entry%LogMessage, "(4A)") "MARBL ERROR (", trim(CodeLoc), "): ", &
                                        trim(ErrorMsg)
    new_entry%CodeLocation = trim(CodeLoc)
    new_entry%lall_tasks = .true.

    if (associated(this%FullLog)) then
      ! Append new entry to last entry in the log
      this%LastEntry%next => new_entry
    else
      this%FullLog => new_entry
    end if
    ! Update LastEntry attribute of linked list
    this%LastEntry => new_entry

  end subroutine marbl_log_error

  subroutine marbl_log_noerror(this, StatusMsg, CodeLoc, ElemInd, lall_tasks)

    class(marbl_log_type), intent(inout) :: this
    character(len=*),      intent(in)    :: StatusMsg, CodeLoc
    integer, optional,     intent(in)    :: ElemInd
    logical, optional,     intent(in)    :: lall_tasks
    type(marbl_status_log_entry_type), pointer :: new_entry

    ! Only allocate memory and add entry if we want to log full namelist!
    if (.not.this%OutputOptions%lLogGeneral) then
      return
    end if

    allocate(new_entry)
    nullify(new_entry%next)
    if (present(ElemInd)) then
      new_entry%ElementInd = ElemInd
    else
      new_entry%ElementInd = 1
    end if
    new_entry%LogMessage   = trim(StatusMsg)
    new_entry%CodeLocation = trim(CodeLoc)
    if (present(lall_tasks)) then
      new_entry%lall_tasks = lall_tasks
    else
      new_entry%lall_tasks = .false.
    end if

    if (associated(this%FullLog)) then
      ! Append new entry to last entry in the log
      this%LastEntry%next => new_entry
    else
      this%FullLog => new_entry
    end if
    ! Update LastEntry attribute of linked list
    this%LastEntry => new_entry

  end subroutine marbl_log_noerror

  subroutine marbl_log_erase(this)

    class(marbl_log_type), intent(inout) :: this
    type(marbl_status_log_entry_type), pointer :: tmp

    do while (associated(this%FullLog))
      tmp => this%FullLog%next
      deallocate(this%FullLog)
      this%FullLog => tmp
    end do
    nullify(this%FullLog)
    nullify(this%LastEntry)

  end subroutine marbl_log_erase

end module marbl_logging
