module marbl_timing_mod

  use marbl_kinds_mod, only : char_len, r8
  use marbl_constants_mod, only : c0
  use marbl_logging, only : marbl_log_type

  implicit none
  private

#if USE_GPTL
#include "gptl.inc"
#elif HAVE_MPI
#include "mpif.h"
#endif

  !*****************************************************************************

  type :: marbl_single_timer_type
    character(char_len) :: name
    logical             :: initialized = .false.
    logical             :: is_running
    real(r8)            :: cur_start
    real(r8)            :: cummulative_runtime
  contains
  end type marbl_single_timer_type

  type :: marbl_timer_type
    logical :: is_locked = .false.
    integer :: num_timers
    type(marbl_single_timer_type), allocatable :: individual_timers(:)
  contains
    procedure :: construct => marbl_timer_type_constructor
    procedure :: add_timer
    procedure :: lock => marbl_timer_type_lock
  end type marbl_timer_type

  !*****************************************************************************

  ! Private module variable to track all the timers being use
  type(marbl_timer_type) :: all_timers

  ! IDs for the different timers, needs to be accessible elsewhere in MARBL
  integer, public :: init_timer_id

  public :: marbl_timing_init
  public :: marbl_timing_start
  public :: marbl_timing_stop
  public :: marbl_timing_shutdown

  !*****************************************************************************

Contains

  !*****************************************************************************

  subroutine marbl_timing_init(marbl_status_log)

    type(marbl_log_type), intent(inout) :: marbl_status_log

    call all_timers%construct(marbl_status_log)

    call all_timers%add_timer('Initialization', init_timer_id, marbl_status_log)

    call all_timers%lock(marbl_status_log)

  end subroutine marbl_timing_init

  !*****************************************************************************

  subroutine marbl_timing_shutdown(interface_timers, marbl_status_log)

    use marbl_interface_types, only : marbl_timers_type

    type(marbl_timers_type), intent(inout) :: interface_timers
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_shutdown'
    character(len=char_len) :: log_message
    integer :: n

    associate(num_timers => all_timers%num_timers)
      interface_timers%num_timers = num_timers
      allocate(interface_timers%names(num_timers))
      allocate(interface_timers%cummulative_runtimes(num_timers))
      interface_timers%names(:) = ''
    end associate

    do n=1,all_timers%num_timers
      associate(ind_timer => all_timers%individual_timers(n))
        if (ind_timer%is_running) then
          write(log_message, "(A,I0,A)") "Timer ", n, " is still running."
          call marbl_status_log%log_error(log_message,subname)
          return
        end if
        interface_timers%names(n) = ind_timer%name
        interface_timers%cummulative_runtimes(n) = ind_timer%cummulative_runtime
      end associate
    end do

  end subroutine marbl_timing_shutdown

  !*****************************************************************************

  subroutine marbl_timing_start(id, marbl_status_log)

    integer,              intent(in)    :: id
    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_start'
    character(len=char_len) :: log_message

    ! Error checking
    if (id.gt.all_timers%num_timers) then
      write(log_message, "(A,I0)") id, ' is not a valid timer ID'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    associate(timer => all_timers%individual_timers(id))
      ! Error checking
      if (.not.timer%initialized) then
        write(log_message, "(A,I0,A)") 'Timer ', id, ' has not been initialized!'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      if (timer%is_running) then
        log_message = 'Timer has already been started!'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      timer%is_running = .true.
#if USE_GPTL
#elif HAVE_MPI
      timer%cur_start = MPI_Wtime()
#else
       call cpu_time(timer%cur_start)
#endif
     end associate

  end subroutine marbl_timing_start

  !*****************************************************************************

  subroutine marbl_timing_stop(id, marbl_status_log)

    integer,              intent(in)    :: id
    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_stop'
    character(len=char_len) :: log_message
    real(r8) :: runtime

    ! Error checking
    if (id.gt.all_timers%num_timers) then
      write(log_message, "(A,I0)") id, ' is not a valid timer ID'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    associate(timer => all_timers%individual_timers(id))
      ! Error checking
      if (.not.timer%initialized) then
        log_message = 'Timer has not been initialized!'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      if (.not.timer%is_running) then
        log_message = 'Timer has not been started!'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      timer%is_running = .false.
#if USE_GPTL
      ! FIXME: add GPTL support
      runtime = c0
#elif HAVE_MPI
      runtime = MPI_Wtime() - timer%cur_start
#else
      call cpu_time(runtime)
      runtime = runtime - timer%cur_start
#endif
      timer%cur_start = c0
      timer%cummulative_runtime = timer%cummulative_runtime + runtime
    end associate

  end subroutine marbl_timing_stop

  !*****************************************************************************

  ! FIXME #124: using self instead of this!
  subroutine marbl_timer_type_constructor(self, marbl_status_log)

    class(marbl_timer_type), intent(inout) :: self
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:timer_type_constructor'

    ! Check to see if constructed
    if (allocated(self%individual_timers)) then
      call marbl_status_log%log_error('Timers already constructed', subname)
      return
    end if

    self%num_timers = 0
    allocate(self%individual_timers(self%num_timers))

  end subroutine marbl_timer_type_constructor

  !*****************************************************************************

  subroutine marbl_timer_type_lock(self, marbl_status_log)

    class(marbl_timer_type), intent(inout) :: self
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:timer_type_lock'
    integer :: n

    ! Error check: are timers already locked?
    if (self%is_locked) then
      call marbl_status_log%log_error('Timers are already locked', subname)
      return
    end if

    do n=1,self%num_timers
      self%individual_timers(n)%is_running = .false.
      self%individual_timers(n)%cur_start  = c0
      self%individual_timers(n)%cummulative_runtime = c0
      self%individual_timers(n)%initialized = .true.
    end do
    self%is_locked = .true.

  end subroutine marbl_timer_type_lock

  !*****************************************************************************

  subroutine add_timer(self, name, id, marbl_status_log)

    class(marbl_timer_type), intent(inout) :: self
    character(len=*),        intent(in)    :: name
    integer,                 intent(out)   :: id
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:add_timer'
    type(marbl_single_timer_type), allocatable :: tmp(:)
    integer :: n

    ! Error check: are timers locked?
    if (self%is_locked) then
      call marbl_status_log%log_error('Can not add new timer, timers are locked', &
                                      subname)
      return
    end if

    allocate(tmp(self%num_timers))
    do n = 1,self%num_timers
      tmp(n)%name = self%individual_timers(n)%name
    end do
    deallocate(self%individual_timers)

    self%num_timers = self%num_timers + 1
    allocate(self%individual_timers(self%num_timers))
    do n = 1,self%num_timers-1
      self%individual_timers(n)%name = tmp(n)%name
    end do
    id = self%num_timers
    self%individual_timers(id)%name = trim(name)

  end subroutine add_timer

  !*****************************************************************************

end module marbl_timing_mod
