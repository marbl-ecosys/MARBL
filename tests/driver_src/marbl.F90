Program marbl

! *****************************************************************************
! The driver for MARBL's stand-alone testing. When built, the executable will
! run one of several tests based on the value of testname in &marbl_driver_nml
!
! usage: marbl.exe -n NAMELIST_FILE [-s SETTINGS_FILE]
!   required argument:
!      -n NAMELIST_FILE, --namelist_file NAMELIST_FILE
!                     File containing &marbl_driver_nml
!   optional argument:
!      -s SETTINGS_FILE, --settings_file SETTINGS_FILE
!                     File containing MARBL settings file
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
!    i)    init/                     -- Initialize MARBL and then call shutdown()
!    ii)   init-twice/               -- Initialize MARBL, call shutdown(), and then repeat
!    iii)  call_compute_subroutines/ -- Initialize MARBL, call surface_flux_compute() and
!                                       interior_tendency_compute(), and then call shutdown()
!    iv)   requested_diags/          -- Print out the list of diagnostics MARBL is returning
!    v)    requested_tracers/        -- Print out the list of tracers MARBL is computing
!                                       tendencies for (so GCM can initialize correctly)
!    vi)   requested_forcings/       -- Print out the list of forcing fields for computing both
!                                       surface flux and interior tendencies that MARBL expects
!                                       the GCM to provide
!    vii)  requested_restoring/      -- restoring to some given data field
!    viii) gen_settings_file/        -- Generate a MARBL settings file (Fortran-based equivalent
!                                       of MARBL_tools/MARBL_generate_settings_file.py)
! *****************************************************************************

  ! Use from libmarbl.a
  use marbl_interface, only : marbl_interface_class
  use marbl_logging,   only : marbl_log_type
  use marbl_kinds_mod, only : char_len

  ! Driver modules for individual tests
  use marbl_init_drv,                     only : marbl_init_test                     => test
  use marbl_call_compute_subroutines_drv, only : marbl_call_compute_subroutines_test => test
  use marbl_get_put_drv,                  only : marbl_get_put_test                  => test
  use marbl_utils_drv,                    only : marbl_utils_test                    => test

  ! MPI wrappers (will build without MPI as well)
  use marbl_mpi_mod, only : marbl_mpi_init
  use marbl_mpi_mod, only : marbl_mpi_finalize
  use marbl_mpi_mod, only : marbl_mpi_abort
  use marbl_mpi_mod, only : marbl_mpi_bcast
  ! MPI-related variables (my_task = 0 for non-MPI runs)
  use marbl_mpi_mod, only : my_task
  use marbl_mpi_mod, only : marbl_mpi_return_error

  use marbl_io_mod, only : marbl_io_read_settings_file
  use marbl_io_mod, only : marbl_io_print_marbl_log

  use marbl_tools_mod, only : marbl_tools_summarize_timers

  implicit none

  character(len=char_len), parameter :: subname = 'Program Marbl'

  ! Variables for processing commandline arguments
  character(len=char_len) :: progname, argstr
  character(len=char_len) :: namelist_file, settings_file
  integer        :: argcnt
  logical        :: labort_after_argparse, lshow_usage, lfound_file

  type(marbl_interface_class), dimension(:), allocatable :: marbl_instances
  type(marbl_log_type)          :: driver_status_log
  integer                       :: n, cnt
  character(len=char_len)       :: settings_file_line, varname, log_message
  logical                       :: lprint_marbl_log, lhas_namelist_file, lhas_settings_file
  logical                       :: ldriver_log_to_file, lsummarize_timers

  ! Namelist vars
  character(len=char_len) :: testname, hist_file, log_out_file
  integer                 :: num_inst, num_PAR_subcols

  ! Processing input file for put calls
  integer  :: ioerr

  namelist /marbl_driver_nml/testname, hist_file, log_out_file, num_inst, num_PAR_subcols

  !****************************************************************************

  ! (0) Initialization
  !     MPI?
  call marbl_mpi_init()

  !     Command line arguments?
  !     (a) set default values
  num_inst = 1
  num_PAR_subcols = 1 ! Only used for request_forcings test
  labort_after_argparse = .false.
  lhas_namelist_file = .false.
  lhas_settings_file = .false.
  settings_file = ''
  namelist_file = ''

  !     (b) get program name and argument count
  call get_command_argument(0, progname)
  argcnt = command_argument_count()
  lshow_usage = (argcnt .eq. 0)

  !     (c) process command line arguments
  n = 1
  do while (n .le. argcnt)
    call get_command_argument(n, argstr)
    select case (argstr)
      case ('-n', '--namelist_file')
        lhas_namelist_file = .true.

        ! Error checking: is this the second occurance of the '--namelist_file' argument?
        if (len_trim(namelist_file) .ne. 0) then
          labort_after_argparse = .true.
          if (my_task .eq. 0) &
            write(*, "(A)") "ERROR: Namelist file specified multiple times on command line"
          exit
        end if

        ! Error checking: is this the last commandline argument (i.e. no file was specified?)
        if (n .eq. argcnt) then
          labort_after_argparse = .true.
          lshow_usage = .true.
          if (my_task .eq. 0) &
            write(*, "(A)") "ERROR: -n argument requires namelist file as well"
          exit
        end if

        n = n+1
        call get_command_argument(n, namelist_file)

        ! Error checking: does the file exist?
        inquire( file=trim(namelist_file), exist=lfound_file)
        if (.not. lfound_file) then
          labort_after_argparse = .true.
          if (my_task .eq. 0) &
            write(*, "(2A)") "ERROR: Namelist file not found: ", trim(namelist_file)
          exit
        end if

      case ('-s', '--settings_file')
        lhas_settings_file = .true.

        ! Error checking: is this the second occurance of the '--settings_file' argument?
        if (len_trim(settings_file) .ne. 0) then
          labort_after_argparse = .true.
          if (my_task .eq. 0) &
            write(*, "(A)") "ERROR: Input file specified multiple times on command line"
          exit
        end if

        ! Error checking: is this the last commandline argument (i.e. no file was specified?)
        if (n .eq. argcnt) then
          labort_after_argparse = .true.
          lshow_usage = .true.
          if (my_task .eq. 0) &
            write(*, "(A)") "ERROR: -i argument requires input file as well"
          exit
        end if

        n = n+1
        call get_command_argument(n, settings_file)

        ! Error checking: does the file exist?
        inquire( file=trim(settings_file), exist=lfound_file)
        if (.not. lfound_file) then
          labort_after_argparse = .true.
          if (my_task .eq. 0) &
            write(*, "(2A)") "ERROR: Input file not found: ", trim(settings_file)
          exit
        end if

      case ('-h', '--help')
        lshow_usage = .true.
        ! suppress "no namelist" error message
        lhas_namelist_file = .true.
        ! call mpi_abort after parsing arguments rather than running driver
        labort_after_argparse = .true.
        ! Do not return error status code
        marbl_mpi_return_error = .false.
        ! no need to parse additonal arguments
        exit
      case DEFAULT
        labort_after_argparse = .true.
        if(my_task .eq. 0) &
          write(*, "(2A)") "ERROR: Unknown command line argument: ", trim(argstr)
        exit
    end select
    n = n+1
  end do

  ! Namelist file is required:
  if (.not. lhas_namelist_file) then
    labort_after_argparse = .true.
    if (my_task .eq. 0) &
      write(*,"(A)") "ERROR: namelist file is required!"
  end if

  ! Program requires one command line argument (namelist)
  ! and accepts and optional second argument (input file)
  if (lshow_usage) then
    labort_after_argparse = .true.
    if (my_task .eq. 0) then
      write(*,"(3A)") "usage: ", trim(progname), " -n NAMELIST_FILE [-s settings_file]"
      write(*,"(A)") "required argument:"
      write(*,"(A)") "  -n NAMELIST_FILE, --namelist_file NAMELIST_FILE"
      write(*,"(A)") "                        File containing &marbl_driver_nml"
      write(*,"(A)") "optional argument:"
      write(*,"(A)") "  -s settings_file, --settings_file SETTINGS_FILE"
      write(*,"(A)") "                        File containing MARBL input file"
    end if
  end if

  ! Abort if error processing command line arguments
  if (labort_after_argparse) call marbl_mpi_abort()

  !     Set up local variables
  !       * Some tests use a different status log than marbl_instances%StatusLog
  !         (default is to use marbl_instances%StatusLog)
  lprint_marbl_log = .true.
  ldriver_log_to_file = .false.
  lsummarize_timers = .true.
  call driver_status_log%construct()

  ! (1) Set marbl_driver_nml defaults
  testname       = ''
  log_out_file   = 'marbl.out' ! only written if ldriver_log_to_file = .true.
  hist_file      = 'history.nc'

  ! (2a) Read driver namelist to know what test to run
  if (my_task .eq. 0) open(98, file=trim(namelist_file), status="old", iostat=ioerr)
  call marbl_mpi_bcast(ioerr, 0)
  if (ioerr.ne.0) then
    if (my_task.eq.0) &
      write(*,'(2A)') "ERROR opening namelist file: ", trim(namelist_file)
    call marbl_mpi_abort()
  end if

  if (my_task .eq. 0) read(98, nml=marbl_driver_nml, iostat=ioerr)
  call marbl_mpi_bcast(ioerr, 0)
  if (ioerr.ne.0) then
    if (my_task.eq.0) &
      write(*,'(2A)') "ERROR reading &marbl_driver_nml from ", trim(namelist_file)
    call marbl_mpi_abort()
  else
    ! Only read the file on master => broadcast contents
    call marbl_mpi_bcast(testname, 0)
    call marbl_mpi_bcast(log_out_file, 0)
  end if

  if (my_task .eq. 0) close(98)

  ! (2b) allocate memory for marbl_instances
  allocate(marbl_instances(num_inst))

  ! (2c) Read input file
  if (lhas_settings_file) then
    do n=1,num_inst
      call marbl_io_read_settings_file(settings_file, marbl_instances(n))
    end do
  end if

  ! (3) Run proper test
  if (my_task.eq.0) write(*,"(3A)") "Beginning ", trim(testname), " test..."
  select case (trim(testname))
    !    REGRESSION TESTS
    ! -- init regression test -- !
    case ('init')
      call verify_single_instance(num_inst, trim(testname))
      call marbl_init_test(marbl_instances(1))

    ! -- init-twice test -- !
    case ('init-twice')
      call verify_single_instance(num_inst, trim(testname))
      call marbl_instances(1)%put_setting('ciso_on = .false.')
      call marbl_init_test(marbl_instances(1))
      if (.not. marbl_instances(1)%StatusLog%labort_marbl) then
        call marbl_tools_summarize_timers(marbl_instances, driver_status_log, header_text = 'Without the CISO Tracers')
        call marbl_instances(1)%put_setting('ciso_on = .true.')
        call marbl_init_test(marbl_instances(1))
        if (.not. marbl_instances(1)%StatusLog%labort_marbl) then
          call marbl_tools_summarize_timers(marbl_instances, driver_status_log, header_text = 'With the CISO Tracers')
          lsummarize_timers = .false.
        end if
      end if

    ! -- gen_settings_file test -- !
    case ('gen_settings_file')
      call verify_single_instance(num_inst, trim(testname))
      lprint_marbl_log = .false.
      ldriver_log_to_file = .true.
      call marbl_init_test(marbl_instances(1), lshutdown=.false.)
      if (.not. marbl_instances(1)%StatusLog%labort_marbl) then
        do n=1,marbl_instances(1)%get_settings_var_cnt()
          call marbl_instances(1)%inquire_settings_metadata(n, sname=varname)
          if (marbl_instances(1)%StatusLog%labort_marbl) exit
          call marbl_instances(1)%get_setting(varname, settings_file_line, lsettings_file_format=.true.)
          if (marbl_instances(1)%StatusLog%labort_marbl) exit
          call driver_status_log%log_noerror(settings_file_line, subname)
        end do
        call marbl_instances(1)%shutdown()
      end if

    ! -- request_diags test -- !
    case ('request_diags')
      call verify_single_instance(num_inst, trim(testname))
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instances(1), lshutdown = .false.)
      if (.not. marbl_instances(1)%StatusLog%labort_marbl) then
        ! Log surface flux diagnostics passed back to driver
        associate(diags => marbl_instances(1)%surface_flux_diags%diags)
          call driver_status_log%log_header('Surface flux diagnostics', subname)
          do n=1, size(diags)
            write(log_message, "(I0,7A)") n, '. ', trim(diags(n)%short_name), ': ', trim(diags(n)%long_name), &
                                          ' (units: ', trim(diags(n)%units),')'
            call driver_status_log%log_noerror(log_message, subname)
          end do
        end associate
        ! Log interior tendency diagnostics passed back to driver
        associate(diags => marbl_instances(1)%interior_tendency_diags%diags)
          call driver_status_log%log_header('Interior tendency diagnostics', subname)
          do n=1, size(diags)
            write(log_message, "(I0,7A)") n, '. ', trim(diags(n)%short_name), ': ', trim(diags(n)%long_name), &
                                          ' (units: ', trim(diags(n)%units),')'
            call driver_status_log%log_noerror(log_message, subname)
          end do
        end associate
        call marbl_instances(1)%shutdown()
      end if

    ! -- request_tracers test -- !
    case ('request_tracers')
      call verify_single_instance(num_inst, trim(testname))
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instances(1), lshutdown = .false.)
      if (.not. marbl_instances(1)%StatusLog%labort_marbl) then
        ! Log tracers requested for initialization
        call driver_status_log%log_header('Requested tracers', subname)
        do n=1, size(marbl_instances(1)%tracer_metadata)
          write(log_message, "(I0, 2A)") n, '. ',                               &
            trim(marbl_instances(1)%tracer_metadata(n)%short_name)
          call driver_status_log%log_noerror(log_message, subname)
        end do
        call marbl_instances(1)%shutdown()
      end if

    ! -- request_forcings test -- !
    case ('request_forcings')
      call verify_single_instance(num_inst, trim(testname))
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instances(1), lshutdown=.false., num_PAR_subcols=num_PAR_subcols)
      if (.not. marbl_instances(1)%StatusLog%labort_marbl) then
        ! Log requested surface forcing fields
        call driver_status_log%log_header('Requested surface forcing fields', subname)
        do n=1,size(marbl_instances(1)%surface_flux_forcings)
          write(log_message, "(I0, 5A)") n, '. ', &
                trim(marbl_instances(1)%surface_flux_forcings(n)%metadata%varname), &
                ' (units: ', trim(marbl_instances(1)%surface_flux_forcings(n)%metadata%field_units),')'
          call driver_status_log%log_noerror(log_message, subname)
        end do
        ! Log requested interior forcing fields
        call driver_status_log%log_header('Requested interior forcing fields', subname)
        do n=1,size(marbl_instances(1)%interior_tendency_forcings)
          write(log_message, "(I0, 5A)") n, '. ', &
               trim(marbl_instances(1)%interior_tendency_forcings(n)%metadata%varname), &
               ' (units: ', trim(marbl_instances(1)%interior_tendency_forcings(n)%metadata%field_units),')'
          call driver_status_log%log_noerror(log_message, subname)
        end do
        call marbl_instances(1)%shutdown()
      end if

    ! -- request_restoring test -- !
    case ('request_restoring')
      call verify_single_instance(num_inst, trim(testname))
      lprint_marbl_log = .false.
      call marbl_init_test(marbl_instances(1), lshutdown = .false.)
      if (.not. marbl_instances(1)%StatusLog%labort_marbl) then
        ! Log tracers requested for restoring
        call driver_status_log%log_header('Requested tracers to restore', subname)
        cnt = 0
        do n=1,size(marbl_instances(1)%interior_tendency_forcings)
          varname = marbl_instances(1)%interior_tendency_forcings(n)%metadata%varname
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
        call marbl_instances(1)%shutdown()
      end if

    case ('call_compute_subroutines')
      lprint_marbl_log = .false.
      call marbl_call_compute_subroutines_test(marbl_instances, hist_file, driver_status_log)

    !    UNIT TESTS
    ! -- get_put test -- !
    case ('get_put')
      call verify_single_instance(num_inst, trim(testname))
      lprint_marbl_log = .false.
      call marbl_get_put_test(marbl_instances(1), driver_status_log)

    ! -- marbl_utils test -- !
    case ('marbl_utils')
      call verify_single_instance(num_inst, trim(testname))
      lprint_marbl_log = .false.
      lsummarize_timers = .false.
      call marbl_utils_test(driver_status_log)

    !   UNRECOGNIZED TEST
    case DEFAULT
      write(*,*) "ERROR: testname = ", trim(testname), " is not a valid option"
      call marbl_mpi_abort()
  end select

  ! (4) Print log(s)
  ! (4a) if test reports an error, let user know
  if (driver_status_log%labort_MARBL) &
    call marbl_io_print_marbl_log(driver_status_log)

  ! (4b) If MARBL returns an error (or MARBL log was requested), print MARBL log
  do n=1,num_inst
    if (marbl_instances(n)%StatusLog%labort_marbl .or. lprint_marbl_log) &
        call marbl_io_print_marbl_log(marbl_instances(n)%StatusLog)
  end do

  ! (4c) If driver log should be written to file, do so
  if (ldriver_log_to_file) then
    call marbl_io_print_marbl_log(driver_status_log, outfile=log_out_file)
  end if

  ! (4d) Add timer information to driver log, then print driver log
  !      note that if driver log was previously written to a file,
  !      timers are all that will be written to screen
  if (lsummarize_timers) &
    call marbl_tools_summarize_timers(marbl_instances, driver_status_log)
  call marbl_io_print_marbl_log(driver_status_log)


  call marbl_mpi_finalize()

  !****************************************************************************

Contains

  !****************************************************************************

  subroutine verify_single_instance(num_inst, testname)

    integer, intent(in) :: num_inst
    character(len=*), intent(in) :: testname

    if (num_inst .ne. 1) then
      write(*,"(3A,I0)") "ERROR: The '", testname, "' test requires a single instance, but num_inst = ", num_inst
      call marbl_mpi_abort()
    end if

  end subroutine verify_single_instance

  !****************************************************************************

End Program marbl
