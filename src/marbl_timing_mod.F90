module marbl_timing_mod

  use marbl_kinds_mod, only : char_len, r8
  use marbl_constants_mod, only : c0
  use marbl_logging, only : marbl_log_type
#ifdef CCSMCOUPLED
  ! It looks like MARBL can't tie directly in to GPTL, need to use POP wrappers?
  use timers, only : get_timer, timer_start, timer_stop
  use domain, only : nblocks_clinic, distrb_clinic
#endif

  implicit none
  private

#ifdef MARBL_WITH_GPTL
#ifndef CCSMCOUPLED
#include "gptl.inc"
#endif
#elif  defined(MARBL_WITH_MPI)
#include "mpif.h"
#endif

  !*****************************************************************************
  ! Internal timer types

  type :: marbl_single_timer_type
    character(char_len) :: name
    logical             :: is_running
    real(r8)            :: cur_start
    real(r8)            :: cummulative_runtime
#ifdef CCSMCOUPLED
    integer             :: CESM_timer_id
    integer             :: lCESM_threaded_timer
#endif
  contains
    procedure :: init => init_single_timer
  end type marbl_single_timer_type

  type, public :: marbl_internal_timers_type
    integer :: num_timers
    type(marbl_single_timer_type), allocatable :: individual_timers(:)
  contains
    procedure :: add => add_new_timer
    procedure :: start => start_timer
    procedure :: stop => stop_timer
  end type marbl_internal_timers_type

  !*****************************************************************************

  public :: marbl_timing_shutdown

  !*****************************************************************************

Contains

  !*****************************************************************************

  subroutine marbl_timing_shutdown(interface_timers, internal_timers, marbl_status_log)

    use marbl_interface_types, only : marbl_timers_type

    type(marbl_timers_type),          intent(inout) :: interface_timers
    type(marbl_internal_timers_type), intent(in)    :: internal_timers
    type(marbl_log_type),             intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_shutdown'
    character(len=char_len) :: log_message
    integer :: n

    if (allocated(internal_timers%individual_timers)) then
      associate(num_timers => internal_timers%num_timers)
        interface_timers%num_timers = num_timers
        allocate(interface_timers%names(num_timers))
        allocate(interface_timers%cummulative_runtimes(num_timers))
        interface_timers%names(:) = ''
      end associate

      do n=1,internal_timers%num_timers
        associate(ind_timer => internal_timers%individual_timers(n))
          if (ind_timer%is_running) then
            write(log_message, "(A,I0,A)") "Timer ", n, " is still running."
            call marbl_status_log%log_error(log_message,subname)
            return
          end if
          interface_timers%names(n) = ind_timer%name
          interface_timers%cummulative_runtimes(n) = ind_timer%cummulative_runtime
        end associate
      end do
    else
      allocate(interface_timers%names(0))
      allocate(interface_timers%cummulative_runtimes(0))
    end if

  end subroutine marbl_timing_shutdown

  !*****************************************************************************

  ! FIXME #124: using self instead of this!
  subroutine add_new_timer(self, name, id, marbl_status_log, threaded)

    class(marbl_internal_timers_type), intent(inout) :: self
    character(len=*),                  intent(in)    :: name
    integer,                           intent(out)   :: id
    type(marbl_log_type),              intent(inout) :: marbl_status_log
    ! threaded is only used by CESM timers
    logical, optional,                 intent(in)    :: threaded

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_add'
    character(len=char_len) :: log_message
    type(marbl_single_timer_type), allocatable :: tmp(:)
    integer :: n

    if (.not.allocated(self%individual_timers)) then
      allocate(self%individual_timers(0))
    end if

    ! Error check: is this name already in use?
    do n=1,self%num_timers
      if (trim(name).eq.trim(self%individual_timers(n)%name)) then
        write(log_message, "(2A,I0)") trim(name), ' is already in use for timer ', &
                                      n
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    end do

    allocate(tmp(self%num_timers))
    call copy_timers(self%individual_timers, tmp, self%num_timers)
    deallocate(self%individual_timers)

    self%num_timers = self%num_timers + 1
    allocate(self%individual_timers(self%num_timers))
    call copy_timers(tmp, self%individual_timers, self%num_timers-1)
    id = self%num_timers
    associate(new_timer => self%individual_timers(id))
      call new_timer%init(name)
#ifdef CCSMCOUPLED
      if (present(threaded)) then
        new_timer%lCESM_threaded_timer = threaded
      else
        new_timer%lCESM_threaded_timer = .false.
      endif
      if (new_timer%lCESM_threaded_timer) then
        n = nblocks_clinic
      else
        n = 1
      end if
      call get_timer(new_timer%CESM_timer_id, trim(new_timer%name), n,        &
                     distrb_clinic%nprocs)
#endif
    end associate

  end subroutine add_new_timer

  !*****************************************************************************

  subroutine start_timer(self, id, marbl_status_log)

    class(marbl_internal_timers_type), intent(inout) :: self
    integer,                           intent(in)    :: id
    type(marbl_log_type),              intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_start'
    character(len=char_len) :: log_message

#ifdef MARBL_WITH_GPTL
    integer gptl_retval
#endif

    ! Error checking
    if (id.gt.self%num_timers) then
      write(log_message, "(A,I0)") id, ' is not a valid timer ID'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    associate(timer => self%individual_timers(id))
      if (timer%is_running) then
        log_message = 'Timer has already been started!'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      timer%is_running = .true.
#ifdef MARBL_WITH_GPTL
      gptl_retval = gptlstart(trim(self%individual_timers(id)%name))
#elif defined(CCSMCOUPLED)
      call timer_start(timer%CESM_timer_id)
#else
      timer%cur_start = get_time()
#endif
     end associate

  end subroutine start_timer

  !*****************************************************************************

  subroutine stop_timer(self, id, marbl_status_log)

    class(marbl_internal_timers_type), intent(inout) :: self
    integer,                           intent(in)    :: id
    type(marbl_log_type),              intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_stop'
    character(len=char_len) :: log_message
    real(r8) :: runtime

#ifdef MARBL_WITH_GPTL
    integer gptl_retval
#endif

    ! Error checking
    if (id.gt.self%num_timers) then
      write(log_message, "(A,I0)") id, ' is not a valid timer ID'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    associate(timer => self%individual_timers(id))
      ! Error checking
      if (.not.timer%is_running) then
        log_message = 'Timer has not been started!'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      timer%is_running = .false.
#ifdef MARBL_WITH_GPTL
      gptl_retval = gptlstop(trim(self%individual_timers(id)%name))
#elif defined(CCSMCOUPLED)
      call timer_stop(timer%CESM_timer_id)
#else
      runtime = get_time() - timer%cur_start
      timer%cur_start = c0
      timer%cummulative_runtime = timer%cummulative_runtime + runtime
#endif
    end associate

  end subroutine stop_timer

  !*****************************************************************************

  subroutine init_single_timer(self, name)

    class(marbl_single_timer_type), intent(inout) :: self
    character(len=*),               intent(in)    :: name

    self%name = name
    self%is_running = .false.
    self%cur_start = c0
    self%cummulative_runtime = c0

  end subroutine init_single_timer

  !*****************************************************************************

  function get_time() result(cur_time)

    real(r8) :: cur_time

#if defined(MARBL_WITH_GPTL) || defined(CCSMCOUPLED)
    cur_time = 0._r8
#elif defined(MARBL_WITH_MPI)
    cur_time = MPI_Wtime()
#else
    call cpu_time(cur_time)
#endif

  end function get_time

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

end module marbl_timing_mod
