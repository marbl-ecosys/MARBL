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
  use marbl_interface, only : marbl_interface_class
  use marbl_logging,   only : marbl_log_type
  use marbl_kinds_mod, only : r8

  ! Driver modules for individual tests
  use marbl_init_drv,    only : marbl_init_test    => test
  use marbl_get_put_drv, only : marbl_get_put_test => test

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
  integer,            parameter :: max_settings_cnt = 1024

  type(marbl_interface_class)   :: marbl_instance
  type(marbl_log_type)          :: driver_status_log
  integer                       :: m, n, nt, cnt
  character(len=256)            :: input_line, testname, varname, log_message
  logical                       :: lprint_marbl_log

  ! Processing input file for put calls
  character(len=256), dimension(max_settings_cnt) :: vars, types, vals
  integer  :: ioerr=0
  integer  :: ival
  real(r8) :: rval

  namelist /marbl_driver_nml/testname

  !****************************************************************************

  ! (0) Initialization
  !     MPI?
  call marbl_mpi_init()

  !     Set up local variables
  !       * Empty strings that will be filled for put() calls in marbl init
  vars(:) = ''
  types(:) = ''
  vals(:) = ''
  !       * Some tests use a different status log than marbl_instance%StatusLog
  !         (default is to use marbl_instance%StatusLog)
  lprint_marbl_log = .true.
  call driver_status_log%construct()

  ! (1) Set marbl_driver_nml defaults
  testname     = ''

  ! Read inputfile
  if (my_task.eq.0) then
    n = 1
    do while(ioerr.eq.0)
      input_line = ''
      read(*,"(A)", iostat=ioerr) input_line
      call marbl_instance%parse_inputfile_line(input_line, vars(n), types(n), vals(n))
      ! Increment array index if line is not empty / commented out
      if (len_trim(vars(n)) .gt. 0) then
        call marbl_mpi_bcast(vars(n), 0)
        call marbl_mpi_bcast(types(n), 0)
        call marbl_mpi_bcast(vals(n), 0)
        n = n+1
      end if
    end do
    if (.not.is_iostat_end(ioerr)) then
      write(*,"(A,I0)") "ioerr = ", ioerr
      write(*,"(A)") "ERROR encountered when reading MARBL input file from stdin"
      call marbl_mpi_abort()
    end if
    n = n-1
  end if

  call marbl_instance%put_setting(vars, types, vals)
  if (marbl_instance%StatusLog%labort_marbl) then
    call marbl_instance%StatusLog%log_error("Error reading input file!", subname)
    call print_marbl_log(marbl_instance%StatusLog)
  end if

  ! (2) Read driver namelist to know what test to run
  open(8, file="test.nml", status="old")
  read(8, nml=marbl_driver_nml, iostat=ioerr)
  if (ioerr.ne.0) then
    write(*,*) "ERROR reading &marbl_driver_nml"
    call marbl_mpi_abort()
  end if
  close(8)

  ! (3) Run proper test
  if (my_task.eq.0) write(*,"(3A)") "Beginning ", trim(testname), " test..."
  select case (trim(testname))
    case ('init')
      call marbl_init_test(marbl_instance)
    case ('get_put')
      lprint_marbl_log = .false.
      call marbl_get_put_test(marbl_instance, driver_status_log)
    case ('request_tracers')
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instance, nt)

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
      call marbl_init_test(marbl_instance)

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
      call marbl_init_test(marbl_instance, nt)

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
      call marbl_mpi_abort()
  end select

  ! (4) Print log(s)
  ! (4a) If MARBL returns an error (or MARBL log was requested), print MARBL log
  if (marbl_instance%StatusLog%labort_marbl.or.lprint_marbl_log) &
      call print_marbl_log(marbl_instance%StatusLog)

  ! (4b) Print timer results (and any other driver-logged output)
  call summarize_timers()
  call print_marbl_log(driver_status_log)

  call marbl_mpi_finalize()

  !****************************************************************************

Contains

  !****************************************************************************

  subroutine print_marbl_log(log_to_print)

    use marbl_logging, only : marbl_status_log_entry_type

    class(marbl_log_type), intent(inout) :: log_to_print
    type(marbl_status_log_entry_type), pointer :: tmp

    tmp => log_to_print%FullLog
    do while (associated(tmp))
      if (mpi_on .and. (.not. tmp%lonly_master_writes)) then
        ! If running in parallel and all tasks are writing to the log, prefix
        ! the task # to log message
        write(*,"(I0,': ',A)") my_task, trim(tmp%LogMessage)
      elseif (my_task.eq.0) then
        ! Otherwise only task 0 writes to the log and no prefix is necessary
        write(*,"(A)") trim(tmp%LogMessage)
      end if
      tmp => tmp%next
    end do

    call log_to_print%erase()

    if (log_to_print%labort_marbl) call marbl_mpi_abort()

  end subroutine print_marbl_log

  !****************************************************************************

  subroutine summarize_timers()

    use marbl_kinds_mod, only : r8

    real(r8) :: min_runtime, ind_runtime, max_runtime, tot_runtime
    character(len=15) :: int_to_str

100 format(A, ': ', F11.3, ' seconds',A)

    associate(timers =>marbl_instance%timer_summary)
      call driver_status_log%log_header('Timer summary', subname)
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
