module marbl_timing_mod

  ! For mutually-exclusive options for timers:
  ! -DMARBL_TIMING_OPT=CIME    => use perf_mod
#define CIME 3
  ! -DMARBL_TIMING_OPT=GPTL    => use GPTL library (FIXME: remove this option!)
#define GPTL 2
  ! -DMARBL_TIMING_OPT=MPI     => use MPI_Wtime()
#define MPI 1
  ! MARBL_TIMING_OPT undefined => use cpu_time()

  use marbl_kinds_mod, only : char_len, r8
  use marbl_constants_mod, only : c0
  use marbl_logging, only : marbl_log_type
#if MARBL_TIMING_OPT == CIME
  ! cime/share/timing/perf_mod.F90 wrappers to GPTL
  use perf_mod, only : t_startf, t_stopf, t_stampf
#endif

  implicit none
  private

#if MARBL_TIMING_OPT == GPTL
#include "gptl.inc"
#endif

#if MARBL_TIMING_OPT == MPI
#include "mpif.h"
#endif

  !*****************************************************************************
  ! Internal timer types

  type :: marbl_single_timer_type
    character(char_len) :: name
    logical             :: is_running
    logical             :: is_threaded
    real(r8)            :: cur_start
    real(r8)            :: cumulative_runtime
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
    integer :: n, num_timers

    if (allocated(internal_timers%individual_timers)) then
      num_timers = internal_timers%num_timers
    else
      num_timers = 0
    end if
    interface_timers%num_timers = num_timers
    allocate(interface_timers%names(num_timers))
    allocate(interface_timers%is_threaded(num_timers))
    allocate(interface_timers%cumulative_runtimes(num_timers))
    if (num_timers .gt. 0) &
      interface_timers%names(:) = ''

    do n=1,num_timers
      associate(ind_timer => internal_timers%individual_timers(n))
        if (ind_timer%is_running) then
          write(log_message, "(A,I0,A)") "Timer ", n, " is still running."
          call marbl_status_log%log_error(log_message,subname)
          return
        end if
        interface_timers%names(n) = ind_timer%name
        interface_timers%cumulative_runtimes(n) = ind_timer%cumulative_runtime
      end associate
    end do

  end subroutine marbl_timing_shutdown

  !*****************************************************************************

  ! FIXME #124: using self instead of this!
  subroutine add_new_timer(self, name, id, marbl_status_log)

    class(marbl_internal_timers_type), intent(inout) :: self
    character(len=*),                  intent(in)    :: name
    integer,                           intent(out)   :: id
    type(marbl_log_type),              intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_add'
    character(len=char_len) :: log_message
    type(marbl_single_timer_type), allocatable :: tmp(:)
    integer :: n

    if (.not.allocated(self%individual_timers)) then
      allocate(self%individual_timers(0))
      self%num_timers = 0
    end if

    ! Error check: is this name already in use?
    do n=1,self%num_timers
      if (trim(name).eq.trim(self%individual_timers(n)%name)) then
        write(log_message, "(2A,I0)") trim(name), ' is already in use for timer ', n
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
    end associate

  end subroutine add_new_timer

  !*****************************************************************************

  subroutine start_timer(self, id, marbl_status_log)

    class(marbl_internal_timers_type), intent(inout) :: self
    integer,                           intent(in)    :: id
    type(marbl_log_type),              intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_start'
    character(len=char_len) :: log_message

#if MARBL_TIMING_OPT == GPTL
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
#ifdef _OPENMP
      timer%is_threaded = omp_get_num_threads() .gt. 1
#endif
#if MARBL_TIMING_OPT == CIME
      call t_startf(trim(self%individual_timers(id)%name))
#elif MARBL_TIMING_OPT == GPTL
      gptl_retval = gptlstart(trim(self%individual_timers(id)%name))
#endif
      timer%cur_start = get_time()
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

#if MARBL_TIMING_OPT == GPTL
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

      timer%is_running  = .false.
#if MARBL_TIMING_OPT == CIME
      call t_stopf(trim(self%individual_timers(id)%name))
#elif  MARBL_TIMING_OPT == GPTL
      gptl_retval = gptlstop(trim(self%individual_timers(id)%name))
#endif
      runtime = get_time() - timer%cur_start
      timer%cur_start = c0
      timer%cumulative_runtime = timer%cumulative_runtime + runtime
    end associate

  end subroutine stop_timer

  !*****************************************************************************

  subroutine init_single_timer(self, name)

    class(marbl_single_timer_type), intent(inout) :: self
    character(len=*),               intent(in)    :: name

    self%name = name
    self%is_running  = .false.
    self%is_threaded = .false.
    self%cur_start = c0
    self%cumulative_runtime = c0

  end subroutine init_single_timer

  !*****************************************************************************

  function get_time() result(cur_time)

#if (MARBL_TIMING_OPT == CIME) || (MARBL_TIMING_OPT == GPTL)
    real(r8) :: wall, usr, sys
#endif
#if MARBL_TIMING_OPT == GPTL
    integer :: gptl_retval
#endif
    real(r8) :: cur_time

#if MARBL_TIMING_OPT == CIME
    call t_stampf(wall, usr, sys)
    cur_time = wall
#elif MARBL_TIMING_OPT == GPTL
    gptl_retval = gptlstamp(wall, usr, sys)
    cur_time = wall
#elif MARBL_TIMING_OPT == MPI
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
      dest(n)%is_threaded         = src(n)%is_threaded
      dest(n)%cur_start           = src(n)%cur_start
      dest(n)%cumulative_runtime = src(n)%cumulative_runtime
    end do

  end subroutine copy_timers

  !*****************************************************************************

end module marbl_timing_mod
