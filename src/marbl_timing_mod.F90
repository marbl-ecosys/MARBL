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
    logical             :: is_running
    real(r8)            :: cur_start
    real(r8)            :: cummulative_runtime
  contains
    procedure :: init => init_single_timer
  end type marbl_single_timer_type

  type :: marbl_timer_type
    integer :: num_timers
    type(marbl_single_timer_type), allocatable :: individual_timers(:)
  end type marbl_timer_type

  !*****************************************************************************

  ! Private module variable to track all the timers being use
  type(marbl_timer_type) :: all_timers

  public :: marbl_timing_add
  public :: marbl_timing_start
  public :: marbl_timing_stop
  public :: marbl_timing_shutdown

  !*****************************************************************************

Contains

  !*****************************************************************************

  subroutine marbl_timing_add(name, id, marbl_status_log)

    character(len=*),     intent(in)    :: name
    integer,              intent(out)   :: id
    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_add'
    character(len=char_len) :: log_message
    type(marbl_single_timer_type), allocatable :: tmp(:)
    integer :: n

    if (.not.allocated(all_timers%individual_timers)) then
      allocate(all_timers%individual_timers(0))
    end if

    ! Error check: is this name already in use?
    do n=1,all_timers%num_timers
      if (trim(name).eq.trim(all_timers%individual_timers(n)%name)) then
        write(log_message, "(2A,I0)") trim(name), ' is already in use for timer ', &
                                      n
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    end do

    allocate(tmp(all_timers%num_timers))
    call copy_timers(all_timers%individual_timers, tmp, all_timers%num_timers)
    deallocate(all_timers%individual_timers)

    all_timers%num_timers = all_timers%num_timers + 1
    allocate(all_timers%individual_timers(all_timers%num_timers))
    call copy_timers(tmp, all_timers%individual_timers, all_timers%num_timers-1)
    id = all_timers%num_timers
    call all_timers%individual_timers(id)%init(name)

  end subroutine marbl_timing_add

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

  subroutine copy_timers(src, dest, num_timers)

    type(marbl_single_timer_type), intent(in)    :: src(:)
    type(marbl_single_timer_type), intent(inout) :: dest(:)
    integer,                       intent(in)    :: num_timers

    integer :: n

    do n=1, num_timers
      dest(n)%name                = src(n)%name
      dest(n)%is_running          = src(n)%is_running
      dest(n)%cur_start           = src(n)%cur_start
      dest(n)%cummulative_runtime = src(n)%cummulative_runtime
    end do

  end subroutine copy_timers

  !*****************************************************************************

  ! FIXME #124: using self instead of this!
  subroutine init_single_timer(self, name)

    class(marbl_single_timer_type), intent(inout) :: self
    character(len=*),               intent(in)    :: name

    self%name = name
    self%is_running = .false.
    self%cur_start = c0
    self%cummulative_runtime = c0

  end subroutine init_single_timer

  !*****************************************************************************

end module marbl_timing_mod
