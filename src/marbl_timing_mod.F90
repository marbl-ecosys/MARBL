module marbl_timing_mod

! This module adds support for internal timers to MARBL. There are three options
! for the source of timing data, controlled by the MARBL_TIMING_OPT macro:
! (1) By default, MARBL will use the FORTRAN intrinsic cpu_time() to determine
!     wall time
! (2) If you build MARBL with -DMARBL_TIMING_OPT=MPI, MARBL will use MPI_Wtime()
!     to determine walltime
#define MPI 1
! (3) If you build MARBL with -DMARBL_TIMING_OPT=CIME, MARBL will use the GPTL
!     timing wrappers found in $CIME/share/timing/perf_mod.F90
#define CIME 2

! Module Usage
! ------------
!
! To add a new timer, you must add an index for it to marbl_timing_indexing_type
! [which is in this module] and update marbl_timing_setup_timers() [also in this
! module] to define the timer. The timers themselves are part of the MARBL
! interface, so pass this%timers and this%timer_ids from marbl_interface.F90 to
! the routine you want to time, and wrap the desired code in
! call timers%start(timer_ids%your_new_index, marbl_status_log)
! ... [code to time]
! call timers%stop(timer_ids%your_new_index, marbl_status_log)
!
! The GCM accesses timing data by calling marbl_interface%update_timers() and then
! pulling data from marbl_interface%timer_summary:
! timer_summary%num_timers is the number of timers provided from MARBL
! timer_summary%names(:) is an array of the names associated with each timer
!                        [set in marbl_timing_setup_timers()]
! timer_summary%cumulative_runtimes(:) is an array of the amount of time MARBL
!                                      spent in each block of code
! timer_summary%is_threaded(:) is an array of logicals - .true. => code was executed
!                              in an OpenMP threaded region

  use marbl_kinds_mod, only : char_len, r8
  use marbl_constants_mod, only : c0
  use marbl_logging, only : marbl_log_type
#if MARBL_TIMING_OPT == CIME
  ! cime/share/timing/perf_mod.F90 wrappers to GPTL
  use perf_mod, only : t_startf, t_stopf, t_stampf
#endif

  implicit none
  private

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

  type, public :: marbl_timer_indices_type
     integer :: init_timer_id
     integer :: surface_forcing_id
     integer :: interior_forcing_id
     integer :: carbonate_chem_id
  end type marbl_timer_indices_type

  !*****************************************************************************

  public :: marbl_timing_copy_timing_data
  public :: marbl_timing_setup_timers
  public :: marbl_timing_shutdown

  !*****************************************************************************

Contains

  !*****************************************************************************

  subroutine marbl_timing_copy_timing_data(interface_timers, internal_timers, &
                                           marbl_status_log)

    use marbl_interface_types, only : marbl_timers_type

    type(marbl_timers_type),          intent(inout) :: interface_timers
    type(marbl_internal_timers_type), intent(in)    :: internal_timers
    type(marbl_log_type),             intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_copy_timing_data'
    character(len=char_len) :: log_message
    integer :: n, num_timers

    if (allocated(internal_timers%individual_timers)) then
      num_timers = internal_timers%num_timers
    else
      num_timers = 0
    end if

    call interface_timers%deconstruct()
    call interface_timers%construct(num_timers)

    do n=1,num_timers
      associate(ind_timer => internal_timers%individual_timers(n))
        if (ind_timer%is_running) then
          write(log_message, "(A,I0,A)") "Timer ", n, " is still running."
          call marbl_status_log%log_error(log_message,subname)
          return
        end if
        interface_timers%names(n) = ind_timer%name
        interface_timers%is_threaded(n) = ind_timer%is_threaded
        interface_timers%cumulative_runtimes(n) = ind_timer%cumulative_runtime
      end associate
    end do

  end subroutine marbl_timing_copy_timing_data

  !*****************************************************************************

  subroutine marbl_timing_setup_timers(internal_timers, timer_ids, marbl_status_log)

    type(marbl_internal_timers_type), intent(inout) :: internal_timers
    type(marbl_timer_indices_type),   intent(inout) :: timer_ids
    type(marbl_log_type),             intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_setup_timers'

    !-----------------------------------------------------------------------
    !  Set up timers for inside time loops
    !-----------------------------------------------------------------------

    call internal_timers%add('MARBL set_sflux', timer_ids%surface_forcing_id, &
                         marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("timers%add()", subname)
      return
    end if

    call internal_timers%add('MARBL set_interior', timer_ids%interior_forcing_id, &
                         marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("timers%add()", subname)
      return
    end if

    call internal_timers%add('MARBL carbonate chemistry', timer_ids%carbonate_chem_id, &
                         marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("timers%add()", subname)
      return
    end if

  end subroutine marbl_timing_setup_timers

  !*****************************************************************************

  subroutine marbl_timing_shutdown(interface_timers, internal_timers, marbl_status_log)

    use marbl_interface_types, only : marbl_timers_type

    type(marbl_timers_type),          intent(inout) :: interface_timers
    type(marbl_internal_timers_type), intent(inout) :: internal_timers
    type(marbl_log_type),             intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_timing_mod:marbl_timing_shutdown'

    call marbl_timing_copy_timing_data(interface_timers, internal_timers,     &
                                       marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('marbl_timing_copy_timing_data',  &
                                            subname)
      return
    end if

    internal_timers%num_timers = 0
    if (allocated(internal_timers%individual_timers)) &
      deallocate(internal_timers%individual_timers)

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

#ifdef _OPENMP
    integer, external :: omp_get_num_threads
#endif

    ! Error checking
    if (id.gt.self%num_timers) then
      write(log_message, "(I0,A)") id, ' is not a valid timer ID'
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
      call t_startf(trim(timer%name))
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

#if MARBL_TIMING_OPT == CIME
    real(r8) :: wall, usr, sys
#endif
    real(r8) :: cur_time

#if MARBL_TIMING_OPT == CIME
    call t_stampf(wall, usr, sys)
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
