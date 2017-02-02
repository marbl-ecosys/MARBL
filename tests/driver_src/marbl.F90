Program marbl

! *****************************************************************************
! The driver for MARBL's stand-alone testing. When built, the executable will
! run one of several tests based on the value of testname in &marbl_driver_nml
!
! In $MARBL/tests/ the actual tests are divided into the following categories:
! 1) Build tests ($MARBL/tests/bld_tests/)
!    * Make sure the code provided compiles, but nothing is executed. Tests
!      every compiler available on the specified machine
!    i)  bld_lib.py -- builds just libmarbl.a
!    ii) bld_exe.py -- build libmarbl.a and then build marbl.exe (linking to
!                      libmarbl.a)
! 2) Unit tests ($MARBL/tets/unit_tests/)
!    * Tests for specific routines in MARBL
!    i) get_put/-- Ensure the get() and put() functionality works for configuration
!                  variables and MARBL parameters
! 3) Regression tests ($MARBL/tests/regression_tests)
!    * Tests that updates to the code don't unexpectedly change existing behavior
!    i)   init_without_namelist/ -- Initialize MARBL without any namelist input
!    ii)  init_from_namelist/ -- Initialize MARBL by reading &marbl_config_nml
!                               and &marbl_parms_nml
!    iii) requested_tracers/ -- Print out the list of tracers MARBL is computing
!                               tendencies for (so GCM can initialize correctly)
!    iv)  requested_forcings/ -- Print out the list of surface forcing fields
!                                that MARBL expects the GCM to provide
!    v)   requested_restoring/ -- Print out the list of tracers MARBL will be
!                                 restoring to some given data field
! *****************************************************************************

  ! Use from libmarbl.a
  use marbl_interface,    only : marbl_interface_class
  use marbl_logging,      only : marbl_log_type
  use marbl_namelist_mod, only : marbl_nl_in_size
  use marbl_namelist_mod, only : marbl_nl_cnt
  use marbl_namelist_mod, only : marbl_nl_buffer_size
  use marbl_namelist_mod, only : marbl_nl_split_string
  use marbl_namelist_mod, only : marbl_namelist

  ! Driver modules for individual tests
  use marbl_init_namelist_drv,    only : marbl_init_namelist_test    => test
  use marbl_init_no_namelist_drv, only : marbl_init_no_namelist_test => test
  use marbl_get_put_drv,          only : marbl_get_put_test          => test

  ! MPI wrappers (will build without MPI as well)
  use marbl_mpi_mod, only : marbl_mpi_init
  use marbl_mpi_mod, only : marbl_mpi_finalize
  use marbl_mpi_mod, only : marbl_mpi_abort
  use marbl_mpi_mod, only : marbl_mpi_bcast
  use marbl_mpi_mod, only : marbl_mpi_send
  use marbl_mpi_mod, only : marbl_mpi_recv
  ! MPI-related variables (if .not.mpi_on, my_task = 0 and num_tasks = 1)
  use marbl_mpi_mod, only : mpi_on, my_task, num_tasks

  Implicit None

  character(len=256), parameter :: subname = 'Program Marbl'
  type(marbl_interface_class) :: marbl_instance
  type(marbl_log_type)        :: driver_status_log
  character(len=marbl_nl_buffer_size) :: nl_buffer(marbl_nl_cnt)
  character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
  character(len=marbl_nl_in_size)     :: nl_str, tmp_str
  integer                             :: ioerr=0
  integer                             :: m, n, nt, cnt
  character(len=256)                  :: testname, varname, log_message
  logical                             :: lprint_marbl_log
  logical                             :: lprint_driver_log
  logical                             :: lwrite_gptl_file

  namelist /marbl_driver_nml/testname, lwrite_gptl_file

  !****************************************************************************

  ! (0) Initialization
  !     MPI?
  call marbl_mpi_init()

  !     Set up local variables
  !       * Empty strings used to pass namelist file contents to MARBL
  nl_buffer(:) = ''
  nl_str       = ''
  !       * Some tests use a different status log than marbl_instance%StatusLog
  !         (default is to use marbl_instance%StatusLog)
  lprint_marbl_log = .true.
  lprint_driver_log = .false.

  ! (1) Set marbl_driver_nml defaults
  testname     = ''
  lwrite_gptl_file = .false.

  ! Read namelist
  if (my_task.eq.0) then
    n = 0
    m = 0
    do while(ioerr.eq.0)
      n = n+m+1
      read(*, fmt="(A)", iostat=ioerr) tmp_str
      m = len(trim(tmp_str))
      nl_str(n:n+m-1) = trim(tmp_str)
      if (ioerr.eq.0) nl_str(n+m:n+m) = achar(10)
    end do
    if (.not.is_iostat_end(ioerr)) then
      write(*,"(A,I0)") "ioerr = ", ioerr
      write(*,"(A)") "ERROR encountered when reading MARBL namelist from stdin"
      stop 1
    end if
    write(*,"(A,I0,A)") "MARBL namelist file contained ", len_trim(nl_str),     &
                        " characters"
  end if
  call marbl_mpi_bcast(nl_str, 0)
  call marbl_nl_split_string(nl_str, nl_buffer)

  ! (2) Read driver namelist to know what test to run
  call driver_status_log%construct()
  tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_driver_nml',               &
                                 driver_status_log)
  if (driver_status_log%labort_marbl) then
    call print_marbl_log(marbl_instance%StatusLog)
  end if

  read(tmp_nl_buffer, nml=marbl_driver_nml, iostat=ioerr)
  if (ioerr.ne.0) then
    write(*,*) "ERROR reading &marbl_driver_nml"
    stop 1
  end if

  ! (3) Run proper test
  if (my_task.eq.0) write(*,"(3A)") "Beginning ", trim(testname), " test..."
  select case (trim(testname))
    case ('init_from_namelist')
      call marbl_init_namelist_test(marbl_instance, nl_buffer)
    case ('init_without_namelist')
      call marbl_init_no_namelist_test(marbl_instance)
    case ('init_timers')
      lprint_marbl_log = .false.
      lprint_driver_log = .true.
      call marbl_init_no_namelist_test(marbl_instance)
      call summarize_timers()
    case ('get_put')
      lprint_marbl_log = .false.
      lprint_driver_log = .true.
      call marbl_get_put_test(marbl_instance, driver_status_log)
    case ('request_tracers')
      lprint_marbl_log = .false.
      lprint_driver_log = .true.
      call marbl_init_namelist_test(marbl_instance, nl_buffer, nt)
      ! Log tracers requested for initialization
      call driver_status_log%log_noerror('', subname)
      call driver_status_log%log_noerror('Requested tracers', subname)
      call driver_status_log%log_noerror('---', subname)
      do n=1,nt
        write(log_message, "(I0, 2A)") n, '. ',                               &
          trim(marbl_instance%tracer_metadata(n)%short_name)
        call driver_status_log%log_noerror(log_message, subname)
      end do
    case ('request_forcings')
      lprint_marbl_log = .false.
      lprint_driver_log = .true.
      call marbl_init_namelist_test(marbl_instance, nl_buffer)

      ! Log requested surface forcing fields
      call driver_status_log%log_noerror('', subname)
      call driver_status_log%log_noerror('Requested surface forcing fields', subname)
      call driver_status_log%log_noerror('---', subname)
      do n=1,size(marbl_instance%surface_input_forcings)
        write(log_message, "(I0, 2A)") n, '. ', &
              trim(marbl_instance%surface_input_forcings(n)%metadata%varname)
        call driver_status_log%log_noerror(log_message, subname)
      end do

      ! Log requested interior forcing fields
      call driver_status_log%log_noerror('', subname)
      call driver_status_log%log_noerror('Requested interior forcing fields', subname)
      call driver_status_log%log_noerror('---', subname)
      do n=1,size(marbl_instance%interior_input_forcings)
        write(log_message, "(I0, 2A)") n, '. ',                               &
             trim(marbl_instance%interior_input_forcings(n)%metadata%varname)
        call driver_status_log%log_noerror(log_message, subname)
      end do
    case ('request_restoring')
      lprint_marbl_log = .false.
      lprint_driver_log = .true.
      call marbl_init_namelist_test(marbl_instance, nl_buffer, nt)

      ! Log tracers requested for restoring
      call driver_status_log%log_noerror('', subname)
      call driver_status_log%log_noerror('Requested tracers to restore', subname)
      call driver_status_log%log_noerror('---', subname)
      cnt = 0
      do n=1,size(marbl_instance%interior_input_forcings)
        varname = marbl_instance%interior_input_forcings(n)%metadata%varname
        if (index(varname, 'Restoring Field').gt.0) then
          cnt = cnt + 1
          varname = varname(1:scan(varname,' ')-1)
          write(log_message, "(I0, 2A)") cnt, '. ', trim(varname)
      call driver_status_log%log_noerror(log_message, subname)
        end if
      end do
      if (cnt.eq.0) then
        call driver_status_log%log_noerror('No tracers to restore!', subname)
      end if
    case DEFAULT
      write(*,*) "ERROR: testname = ", trim(testname), " is not a valid option"
      stop 1
  end select

  ! (4) Print log(s)
  ! (4a) If MARBL returns an error, print MARBL log
  if (marbl_instance%StatusLog%labort_marbl) &
      call print_marbl_log(marbl_instance%StatusLog)

  ! (4b) If requested, print MARBL log
  if (lprint_marbl_log) then
    call print_marbl_log(marbl_instance%StatusLog)
  end if

  ! (4c) If requested, print driver log
  if (lprint_driver_log) then
    call print_marbl_log(driver_status_log)
  end if

  call marbl_mpi_finalize()

  !****************************************************************************

Contains

  !****************************************************************************

  subroutine print_marbl_log(log_to_print)

    use marbl_logging, only : marbl_status_log_entry_type

    class(marbl_log_type), intent(inout) :: log_to_print
    type(marbl_status_log_entry_type), pointer :: tmp
    logical :: marbl_err

    marbl_err = log_to_print%labort_marbl

100 format(A)
101 format(I0, ': ', A)

    tmp => log_to_print%FullLog
    do while (associated(tmp))
      if (mpi_on.and.(marbl_err.or.tmp%lall_tasks)) then
        ! Output log from task(s) reporting errors, prefix task #
        write(*,101) my_task, trim(tmp%LogMessage)
      elseif (my_task.eq.0) then
        write(*,100) trim(tmp%LogMessage)
      end if
      tmp => tmp%next
    end do

    call log_to_print%erase()

    if (marbl_err) call marbl_mpi_abort()

  end subroutine print_marbl_log

  !****************************************************************************

  subroutine summarize_timers()

    use marbl_kinds_mod, only : r8

    real(r8) :: min_runtime, ind_runtime, max_runtime, tot_runtime
    character(len=15) :: int_to_str

100 format(A, ': ', F11.3, ' seconds',A)

    associate(timers =>marbl_instance%timer_summary)
      write(log_message, "(A, I0, A)") 'There are ', timers%num_timers,       &
                                       ' timers being returned'
      call driver_status_log%log_noerror(log_message, subname)
      call driver_status_log%log_noerror('----', subname)
      do n=1, timers%num_timers
        ind_runtime = timers%cumulative_runtimes(n)
        if (mpi_on) then
          if (my_task.eq.0) then
            min_runtime = ind_runtime
            max_runtime = ind_runtime
            tot_runtime = ind_runtime
            write(log_message, 100) trim(timers%names(n)), ind_runtime,       &
                                    ' (Task 0)'
            call driver_status_log%log_noerror(log_message, subname)
            do m=1, num_tasks-1
              call marbl_mpi_recv(ind_runtime, m)
              min_runtime = min(min_runtime, ind_runtime)
              max_runtime = max(max_runtime, ind_runtime)
              tot_runtime = tot_runtime + ind_runtime
              write(int_to_str, "(' (Task ',I0,')')") m
              write(log_message, 100) trim(timers%names(n)), ind_runtime,     &
                                      trim(int_to_str)
              call driver_status_log%log_noerror(log_message, subname)
            end do
          else ! not task 0
            call marbl_mpi_send(ind_runtime, 0)
          end if

          if (my_task.eq.0) then
            write(log_message, 100) trim(timers%names(n)), tot_runtime/real(num_tasks,r8), ' (avg)'
            call driver_status_log%log_noerror(log_message, subname)
            write(log_message, 100) trim(timers%names(n)), min_runtime, ' (min)'
            call driver_status_log%log_noerror(log_message, subname)
            write(log_message, 100) trim(timers%names(n)), max_runtime, ' (max)'
            call driver_status_log%log_noerror(log_message, subname)
          end if
        else ! no MPI
          write(log_message, 100) trim(timers%names(n)), ind_runtime, ''
          call driver_status_log%log_noerror(log_message, subname)
        end if
      end do
    end associate

  end subroutine summarize_timers

  !****************************************************************************

End Program marbl
