Program marbl

! *****************************************************************************
! The driver for MARBL's stand-alone testing. When built, the executable will
! run one of several tests based on the value of testname in &marbl_driver_nml
!
! Usage:
! $ marbl.exe NAMELIST [INPUTFILE]
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
  use marbl_utils_drv,   only : marbl_utils_test => test

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

  ! Variables for processing commandline arguments
  character(256) :: progname, argstr
  character(256) :: namelist_file, inputfile
  integer        :: argcnt

  type(marbl_interface_class)   :: marbl_instance
  type(marbl_log_type)          :: driver_status_log
  integer                       :: m, n, nt, cnt
  character(len=256)            :: input_line, testname, varname, log_message, log_out_file
  logical                       :: lprint_marbl_log, lhas_inputfile
  logical                       :: ldriver_log_to_file, lsummarize_timers

  ! Processing input file for put calls
  integer  :: ioerr
  integer  :: ival
  real(r8) :: rval

  namelist /marbl_driver_nml/testname, log_out_file

  !****************************************************************************

  ! (0) Initialization
  !     MPI?
  call marbl_mpi_init()

  !     Command line arguments?
  call get_command_argument(0, progname)
  argcnt = command_argument_count()
  ! Program requires one command line argument (namelist)
  ! and accepts and optional second argument (input file)
  if (.not. any(argcnt .eq. (/1,2/))) then
    if (my_task .eq. 0) then
      write(*,"(3A)") "USAGE: ", trim(progname), " NAMELIST [INPUTFILE]"
      write(*,"(A)") ''
      write(*,"(7X,A)") 'NAMELIST    required argument, namelist tells driver what test to run'
      write(*,"(7X,A)") 'INPUTFILE   optional inputfile to read and send to put_setting()'
      write(*,"(A)") ''
    end if
    call marbl_mpi_abort()
  end if
  call get_command_argument(1, namelist_file)
  if (argcnt .eq. 2) then
    lhas_inputfile = .true.
    call get_command_argument(2, inputfile)
  else
    lhas_inputfile = .false.
    inputfile = ''
  end if

  !     Set up local variables
  !       * Some tests use a different status log than marbl_instance%StatusLog
  !         (default is to use marbl_instance%StatusLog)
  lprint_marbl_log = .true.
  ldriver_log_to_file = .false.
  lsummarize_timers = .true.
  call driver_status_log%construct()

  ! (1) Set marbl_driver_nml defaults
  testname       = ''
  log_out_file   = 'marbl.out' ! only written if ldriver_log_to_file = .true.

  ! (2a) Read driver namelist to know what test to run
  if (my_task .eq. 0) open(98, file=trim(namelist_file), status="old", iostat=ioerr)
  call marbl_mpi_bcast(ioerr, 0)
  if (ioerr.ne.0) then
    if (my_task.eq.0) &
      write(*,*) "ERROR reading &marbl_driver_nml"
    call marbl_mpi_abort()
  end if

  if (my_task .eq. 0) read(98, nml=marbl_driver_nml, iostat=ioerr)
  call marbl_mpi_bcast(ioerr, 0)
  if (ioerr.ne.0) then
    if (my_task.eq.0) &
      write(*,*) "ERROR reading &marbl_driver_nml"
    call marbl_mpi_abort()
  else
    ! Only read the file on master => broadcast contents
    call marbl_mpi_bcast(testname, 0)
    call marbl_mpi_bcast(log_out_file, 0)
  end if

  if (my_task .eq. 0) close(98)

  ! (2b) Read inputfile
  if (lhas_inputfile) then
    call read_inputfile(inputfile, marbl_instance)
  end if

  ! (3) Run proper test
  if (my_task.eq.0) write(*,"(3A)") "Beginning ", trim(testname), " test..."
  select case (trim(testname))
    !    REGRESSION TESTS
    ! -- init regression test -- !
    case ('init')
      call marbl_init_test(marbl_instance)

    ! -- init-twice test -- !
    case ('init-twice')
      call marbl_instance%put_setting('ciso_on = .false.')
      call marbl_init_test(marbl_instance)
      call summarize_timers(driver_status_log, header_text = 'Without the CISO Tracers')
      call marbl_instance%put_setting('ciso_on = .true.')
      call marbl_init_test(marbl_instance)
      call summarize_timers(driver_status_log, header_text = 'With the CISO Tracers')
      lsummarize_timers = .false.

    ! -- gen_inputfile test -- !
    case ('gen_inputfile')
      lprint_marbl_log = .false.
      ldriver_log_to_file = .true.
      call marbl_init_test(marbl_instance, lshutdown=.false.)
      if (.not. marbl_instance%StatusLog%labort_marbl) then
        do n=1,marbl_instance%get_settings_var_cnt()
          call marbl_instance%inquire_settings_metadata(n, sname=varname)
          if (marbl_instance%StatusLog%labort_marbl) exit
          call marbl_instance%get_setting(varname, input_line, linputfile_format=.true.)
          if (marbl_instance%StatusLog%labort_marbl) exit
          call driver_status_log%log_noerror(input_line, subname)
        end do
        call marbl_instance%shutdown()
      end if

    ! -- request_diags test -- !
    case ('request_diags')
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instance, lshutdown = .false.)
      ! Log surface forcing diagnostics passed back to driver
      associate(diags => marbl_instance%surface_forcing_diags%diags)
        call driver_status_log%log_header('Surface forcing diagnostics', subname)
        do n=1, size(diags)
          write(log_message, "(I0,4A)") n, '. ', trim(diags(n)%short_name), ': ', trim(diags(n)%long_name)
          call driver_status_log%log_noerror(log_message, subname)
        end do
      end associate
      ! Log interior forcing diagnostics passed back to driver
      associate(diags => marbl_instance%interior_forcing_diags%diags)
        call driver_status_log%log_header('Interior forcing diagnostics', subname)
        do n=1, size(diags)
          write(log_message, "(I0,4A)") n, '. ', trim(diags(n)%short_name), ': ', trim(diags(n)%long_name)
          call driver_status_log%log_noerror(log_message, subname)
        end do
      end associate
      call marbl_instance%shutdown()

    ! -- request_tracers test -- !
    case ('request_tracers')
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instance, nt = nt, lshutdown = .false.)
      ! Log tracers requested for initialization
      call driver_status_log%log_header('Requested tracers', subname)
      do n=1,nt
        write(log_message, "(I0, 2A)") n, '. ',                               &
          trim(marbl_instance%tracer_metadata(n)%short_name)
        call driver_status_log%log_noerror(log_message, subname)
      end do
      call marbl_instance%shutdown()

    ! -- request_forcings test -- !
    case ('request_forcings')
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instance, lshutdown=.false.)
      ! Log requested surface forcing fields
      call driver_status_log%log_header('Requested surface forcing fields', subname)
      do n=1,size(marbl_instance%surface_input_forcings)
        write(log_message, "(I0, 2A)") n, '. ', &
              trim(marbl_instance%surface_input_forcings(n)%metadata%varname)
        call driver_status_log%log_noerror(log_message, subname)
      end do
      ! Log requested interior forcing fields
      call driver_status_log%log_header('Requested interior forcing fields', subname)
      do n=1,size(marbl_instance%interior_input_forcings)
        write(log_message, "(I0, 2A)") n, '. ',                               &
             trim(marbl_instance%interior_input_forcings(n)%metadata%varname)
        call driver_status_log%log_noerror(log_message, subname)
      end do
      call marbl_instance%shutdown()

    ! -- request_restoring test -- !
    case ('request_restoring')
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instance, nt = nt, lshutdown = .false.)

      ! Log tracers requested for restoring
      call driver_status_log%log_header('Requested tracers to restore', subname)
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
      call marbl_instance%shutdown()

    !    UNIT TESTS
    ! -- get_put test -- !
    case ('get_put')
      lprint_marbl_log = .false.
      call marbl_get_put_test(marbl_instance, driver_status_log)

    ! -- marbl_utils test -- !
    case ('marbl_utils')
      lprint_marbl_log = .false.
      lsummarize_timers = .false.
      call marbl_utils_test(driver_status_log)

    !   UNRECOGNIZED TEST
    case DEFAULT
      write(*,*) "ERROR: testname = ", trim(testname), " is not a valid option"
      call marbl_mpi_abort()
  end select

  ! (4) Print log(s)
  ! (4a) If MARBL returns an error (or MARBL log was requested), print MARBL log
  if (marbl_instance%StatusLog%labort_marbl.or.lprint_marbl_log) &
      call print_marbl_log(marbl_instance%StatusLog)

  ! (4b) If driver log should be written to file, do so
  if (ldriver_log_to_file) then
    call print_marbl_log(driver_status_log, outfile=log_out_file)
  end if

  ! (4c) Add timer information to driver log, then print driver log
  !      note that if driver log was previously written to a file,
  !      timers are all that will be written to screen
  if (lsummarize_timers) &
    call summarize_timers(driver_status_log)
  call print_marbl_log(driver_status_log)


  call marbl_mpi_finalize()

  !****************************************************************************

Contains

  !****************************************************************************

  subroutine read_inputfile(inputfile, marbl_instance)

    character(len=*),            intent(in)    :: inputfile
    type(marbl_interface_class), intent(inout) :: marbl_instance

    character(len=256), parameter :: subname = 'marbl::read_inputfile'
    character(len=256) :: input_line
    integer :: ioerr

    if (my_task .eq. 0) open(97, file=trim(inputfile), status="old", iostat=ioerr)
    call marbl_mpi_bcast(ioerr, 0)
    if (ioerr .ne. 0) then
      if (my_task .eq. 0) then
        write(*,"(A,I0)") "ioerr = ", ioerr
        write(*,"(2A)") "ERROR encountered when opening MARBL input file ", trim(inputfile)
      end if
      call marbl_mpi_abort()
    end if

    input_line = ''
    do while(ioerr .eq. 0)
      ! (i) call put_setting(); abort if error
      !     calling with empty input_line on first entry to loop is okay, and
      !     this ensures we don't call put_setting with a garbage line if
      !     ioerr is non-zero
      call marbl_instance%put_setting(input_line)
      if (marbl_instance%StatusLog%labort_marbl) then
        call marbl_instance%StatusLog%log_error_trace("put_setting(input_line)", subname)
        call print_marbl_log(marbl_instance%StatusLog)
      end if
      ! (ii) master task reads next line in inputfile
      if (my_task .eq. 0) read(97,"(A)", iostat=ioerr) input_line
      ! (iii) broadcast inputfile line to all tasks (along with iostat)
      call marbl_mpi_bcast(input_line, 0)
      call marbl_mpi_bcast(ioerr, 0)
    end do

    if (.not.is_iostat_end(ioerr)) then
      if (my_task .eq. 0) then
        write(*,"(A,I0)") "ioerr = ", ioerr
        write(*,"(2A)") "ERROR encountered when reading MARBL input file ", trim(inputfile)
      end if
      call marbl_mpi_abort()
    end if

    if (my_task .eq. 0) close(97)

  end subroutine read_inputfile

  !****************************************************************************

  subroutine print_marbl_log(log_to_print, outfile)

    use marbl_logging, only : marbl_status_log_entry_type

    class(marbl_log_type),      intent(inout) :: log_to_print
    character(len=*), optional, intent(in)    :: outfile
    type(marbl_status_log_entry_type), pointer :: tmp
    integer :: out_unit

    ! write to stdout unless outfile is provided (only task 0 writes to file)
    out_unit = 6
    if ((my_task .eq. 0) .and. (present(outfile))) then
      out_unit = 99
      open(out_unit, file=outfile, action="write", status="replace")
      write(6, "(3A)") "  Writing log to ", trim(outfile), "..."
    end if

    tmp => log_to_print%FullLog
    do while (associated(tmp))
      if (mpi_on .and. (.not. tmp%lonly_master_writes)) then
        ! If running in parallel and all tasks are writing to the log, prefix
        ! the task # to log message
        write(out_unit, "(I0,': ',A)") my_task, trim(tmp%LogMessage)
      elseif (my_task.eq.0) then
        ! Otherwise only task 0 writes to the log and no prefix is necessary
        write(out_unit, "(A)") trim(tmp%LogMessage)
      end if
      tmp => tmp%next
    end do

    if ((my_task .eq. 0) .and. (present(outfile))) then
      close(out_unit)
      if (my_task .eq. 0) write(6, "(A)") "  ... Done writing to file!"
    end if

    call log_to_print%erase()

    if (log_to_print%labort_marbl) call marbl_mpi_abort()

  end subroutine print_marbl_log

  !****************************************************************************

  subroutine summarize_timers(driver_status_log, header_text)

    use marbl_kinds_mod, only : r8

    type(marbl_log_type),       intent(inout) :: driver_status_log
    character(len=*), optional, intent(in)    :: header_text

    real(r8) :: min_runtime, ind_runtime, max_runtime, tot_runtime
    character(len=15) :: int_to_str

100 format(A, ': ', F11.3, ' seconds',A)

    associate(timers =>marbl_instance%timer_summary)
      if (present(header_text)) then
        call driver_status_log%log_header(header_text, subname)
      else
        call driver_status_log%log_header('Timer summary', subname)
      end if
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
