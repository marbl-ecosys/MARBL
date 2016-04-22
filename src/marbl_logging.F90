module marbl_logging

! ------------
! Module Usage
! ------------
!
! Assume a variable named StatusLog (as appears in the marbl_interface_class)
!
! Use the following routines to write log entries
! -----------------------------------------------
!
! (1) StatusLog%log_namelist -- this stores a log message that contains a
!     namelist that has been read in by MARBL. The log message will include
!     "Contents of &[namelist_name]:" as a prefix to the actual namelist.
! (2) StatusLog%log_noerror -- this stores a log message in StatusLog that does
!     not contain a fatal error
! (3) StatusLog%log_error -- this stores a log message in StatusLog that DOES
!     contain a fatal error. It does this by setting StatusLog%labort_marbl =
!     .true.; when a call from the GCM to MARBL returns, it is important for the
!     GCM to check the value of StatusLog%labort_marbl and abort the run if an
!     error has been reported.
! (4) StatusLog%log_error_trace -- this routine helps trace the path in the code
!     that resulted in an error being reported. If a MARBL routine logs an error
!     message, then the routine that called the errant routine also logs an
!     error message giving more information about where the call was made from.
!
! Pseudo-code for writing StatusLog in the driver
! -----------------------------------------------
!
! Using '#if 0' so code below is not compiled
#if 0
  type(marbl_status_log_entry_type), pointer :: LogEntry

  ! Set pointer to first entry of the log
  LogEntry => StatusLog%FullLog

  do while (associated(LogEntry))
    ! If running in parallel, you may want to check if you are the master
    ! task or if LogEntry%lalltasks = .true.
    write(stdout,*) trim(LogEntry%LogMessage)
    LogEntry => LogEntry%next
  end do

  ! Erase contents of log now that they have been written out
  call StatusLog%erase()

  if (StatusLog%labort_marbl) then
    [GCM abort call: "error found in MARBL"]
  end if

#endif

  use marbl_kinds_mod, only : char_len
  use marbl_namelist_mod, only : marbl_nl_buffer_size

  implicit none
  private
  save

  integer, parameter, private :: marbl_log_len = marbl_nl_buffer_size

  !****************************************************************************

  type, public :: marbl_status_log_entry_type
    integer :: ElementInd
    logical :: lall_tasks  ! True => message should be written to stdout by
                           !         all tasks; False => master task only
    character(len=marbl_log_len) :: LogMessage   ! Message text
    character(len=char_len)      :: CodeLocation ! Information on where log was written
                               
    type(marbl_status_log_entry_type), pointer :: next
  end type marbl_status_log_entry_type

  !****************************************************************************

  ! Note: this data type is not in use at the moment, but it is included as an
  !       initial step towards allowing the user some control over what types
  !       of messages are added to the log. For example, if you do not want
  !       the contents of namelists written to the log, you would simply set
  !
  !       lLogNamelist = .false.
  !
  !       In the future we hope to be able to set these options via namelist,
  !       but for now lLogNamelist, lLogGeneral, lLogWarning, and lLogError are
  !       all set to .true. and can not be changed without modifying the source
  !       code in this file.
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

  !****************************************************************************

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
    procedure, public :: log_error_trace => marbl_log_error_trace
    procedure, public :: erase => marbl_log_erase
  end type marbl_log_type

  !****************************************************************************

contains

  !****************************************************************************

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

  !****************************************************************************

  subroutine marbl_log_constructor(this)

    class(marbl_log_type), intent(inout) :: this

    this%labort_marbl = .false.
    nullify(this%FullLog)
    nullify(this%LastEntry)
    call this%OutputOptions%construct()

  end subroutine marbl_log_constructor

  !****************************************************************************

  subroutine marbl_log_namelist(this, NamelistName, NamelistContents, CodeLoc, ElemInd)

    class(marbl_log_type), intent(inout) :: this
    ! NamelistName is the name of the namelist
    ! NamelistContents is a string containing the contents of the namelist
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_namelist
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

  !****************************************************************************

  subroutine marbl_log_error(this, ErrorMsg, CodeLoc, ElemInd)

    class(marbl_log_type), intent(inout) :: this
    ! ErrorMsg is the error message to be printed in the log; it does not need
    !     to contain the name of the module or subroutine triggering the error
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_error
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

  !****************************************************************************

  subroutine marbl_log_noerror(this, StatusMsg, CodeLoc, ElemInd, lall_tasks)

    class(marbl_log_type), intent(inout) :: this
    ! StatusMsg is the message to be printed in the log; it does not need to
    !    contain the name of the module or subroutine producing the log message
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_noerror
    character(len=*),      intent(in)    :: StatusMsg, CodeLoc
    integer, optional,     intent(in)    :: ElemInd
    ! If lall_tasks is .true., then this is a message that should be printed out
    ! regardless of which task produced it. By default, MARBL assumes that only
    ! the master task needs to print a message
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

  !****************************************************************************

  subroutine marbl_log_error_trace(this, RoutineName, CodeLoc, ElemInd)

  ! This routine should only be called if another subroutine has returned and
  ! StatusLog%labort_marbl = .true.

    class(marbl_log_type), intent(inout) :: this
    ! RoutineName is the name of the subroutine that returned with
    !             labort_marbl = .true.
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_error_trace
    !
    ! Log will contain a message along the lines of
    !
    ! "(CodeLoc) Error reported from RoutineName"
    !
    ! When the log is printed, this will provide a traceback through the sequence
    ! of calls that led to the original error message.
    character(len=*),      intent(in)    :: RoutineName, CodeLoc
    integer, optional,     intent(in)    :: ElemInd
    character(len=char_len) :: log_message

    write(log_message, "(2A)") "Error reported from ", trim(RoutineName)
    call this%log_error(log_message, CodeLoc, ElemInd)

  end subroutine marbl_log_error_trace

  !****************************************************************************

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
