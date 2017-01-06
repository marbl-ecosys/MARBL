Program marbl

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

  ! Use from libmarbl.a
  use marbl_interface,    only : marbl_interface_class
  use marbl_logging,      only : marbl_log_type
  use marbl_namelist_mod, only : marbl_nl_in_size
  use marbl_namelist_mod, only : marbl_nl_cnt
  use marbl_namelist_mod, only : marbl_nl_buffer_size
  use marbl_namelist_mod, only : marbl_nl_split_string
  use marbl_namelist_mod, only : marbl_namelist

  ! Use from drivers/
  use marbl_init_namelist_drv,    only : marbl_init_namelist_test    => test
  use marbl_init_no_namelist_drv, only : marbl_init_no_namelist_test => test
  use marbl_get_put_drv,          only : marbl_get_put_test          => test

  Implicit None

#if HAVE_MPI
  include 'mpif.h'
#endif

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

  ! rank when building with MPI (my_task = 0 for serial build)
  integer :: my_task
  integer :: err_code

  namelist /marbl_driver_nml/testname

  ! (0) Initialization
  !     MPI?
#ifdef HAVE_MPI
  call MPI_Init(err_code)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_task, err_code)
#else
  my_task = 0
#endif

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
#if HAVE_MPI
  call MPI_Bcast(nl_str,marbl_nl_in_size,MPI_CHARACTER,0, MPI_COMM_WORLD, err_code)
#endif
  call marbl_nl_split_string(nl_str, nl_buffer)

  ! (2) Read driver namelist to know what test to run
  call driver_status_log%construct()
  tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_driver_nml',               &
                                 driver_status_log)
  if (driver_status_log%labort_marbl) then
    call print_marbl_log(marbl_instance%StatusLog, my_task, err_code)
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
      write(log_message, "(A)") 'At this point there are no timers to inspect'
      call driver_status_log%log_noerror(log_message, subname)
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
      call print_marbl_log(marbl_instance%StatusLog, my_task, err_code)

  ! (4b) If requested, print MARBL log
  if (lprint_marbl_log.and.(err_code.eq.0)) then
    call print_marbl_log(marbl_instance%StatusLog, my_task, err_code)
  end if

  ! (4c) If requested, print driver log
  if (lprint_driver_log.and.(err_code.eq.0)) then
    call print_marbl_log(driver_status_log, my_task, err_code)
  end if

  if (err_code.ne.0) then
#if HAVE_MPI
    call MPI_Abort(MPI_COMM_WORLD, err_code)
#else
    stop 1
#endif
  end if

#if HAVE_MPI
  call MPI_Finalize(err_code)
#endif

Contains

  subroutine print_marbl_log(log_to_print, my_task, err_code)

    use marbl_logging, only : marbl_status_log_entry_type

    class(marbl_log_type), intent(inout) :: log_to_print
    integer,               intent(in)    :: my_task
    integer,               intent(out)   :: err_code
    type(marbl_status_log_entry_type), pointer :: tmp

    err_code = 0

    ! Output log from master task
    if (my_task.eq.0) then
      tmp => log_to_print%FullLog
      do while (associated(tmp))
        write(*,"(A)") trim(tmp%LogMessage)
        tmp => tmp%next
      end do
    end if

    if (log_to_print%labort_marbl) then
      if (my_task.ne.0) then
        ! Output log from non-master task in case of error
        tmp => log_to_print%FullLog
        do while (associated(tmp))
          write(*,"(I0,2A)") my_task, ': ', trim(tmp%LogMessage)
          tmp => tmp%next
        end do
      end if
      err_code = 1
    end if

    call log_to_print%erase()

  end subroutine print_marbl_log

End Program marbl
