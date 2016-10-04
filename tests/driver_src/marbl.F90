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

  character(len=256), parameter :: subname = 'Program Marbl'
  type(marbl_interface_class) :: marbl_instance
  type(marbl_log_type)        :: driver_status_log
  character(len=marbl_nl_buffer_size) :: nl_buffer(marbl_nl_cnt)
  character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
  character(len=marbl_nl_in_size)     :: nl_str, tmp_str
  integer                             :: ioerr=0
  integer                             :: m, n, nt, restore_nt
  character(len=256)                  :: testname, log_message
  logical                             :: lprint_marbl_log
  logical                             :: lprint_driver_log

  namelist /marbl_driver_nml/testname

  ! (0) Set up local variables
  !     * Empty strings used to pass namelist file contents to MARBL
  nl_buffer(:) = ''
  nl_str       = ''
  !     * Some tests use a different status log than marbl_instance%StatusLog
  !       (default is to use marbl_instance%StatusLog)
  lprint_marbl_log = .true.
  lprint_driver_log = .false.

  ! (1) Set marbl_driver_nml defaults
  testname     = ''

  ! Read namelist
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
    print*, ioerr
    write(*,"(A)") "ERROR encountered when reading MARBL namelist from stdin"
    stop 1
  end if
  write(*,"(A,I0,A)") "MARBL namelist file contained ", len_trim(nl_str),     &
                      " characters"
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
  write(*,"(3A)") "Beginning ", trim(testname), " test..."
  select case (trim(testname))
    case ('init_from_namelist')
      call marbl_init_namelist_test(marbl_instance, nl_buffer)
    case ('init_without_namelist')
      call marbl_init_no_namelist_test(marbl_instance)
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
      ! Log requested forcing fields
      call driver_status_log%log_noerror('', subname)
      call driver_status_log%log_noerror('Requested forcing fields', subname)
      call driver_status_log%log_noerror('---', subname)
      do n=1,size(marbl_instance%surface_forcing_metadata)
        write(log_message, "(I0, 2A)") n, '. ',                               &
          trim(marbl_instance%surface_forcing_metadata(n)%varname)
        call driver_status_log%log_noerror(log_message, subname)
      end do
    case ('request_restoring')
      lprint_marbl_log = .false.
      lprint_driver_log = .true.
      call marbl_init_namelist_test(marbl_instance, nl_buffer, nt)
      restore_nt = size(marbl_instance%interior_forcing_input%tracer_names)
      ! Log tracers requested for restoring
      call driver_status_log%log_noerror('', subname)
      call driver_status_log%log_noerror('Requested tracers to restore', subname)
      call driver_status_log%log_noerror('---', subname)
      do n=1,restore_nt
        write(log_message, "(I0, 2A)") n, '. ',                               &
          trim(marbl_instance%interior_forcing_input%tracer_names(n))
        call driver_status_log%log_noerror(log_message, subname)
      end do
      if (restore_nt.eq.0) then
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

Contains

  subroutine print_marbl_log(log_to_print)

    use marbl_logging,      only : marbl_status_log_entry_type

    class(marbl_log_type), intent(inout) :: log_to_print
    type(marbl_status_log_entry_type), pointer :: tmp

    tmp => log_to_print%FullLog
    do while (associated(tmp))
      write(*,"(A)") trim(tmp%LogMessage)
      tmp => tmp%next
    end do

    call log_to_print%erase()

    if (log_to_print%labort_marbl) then
      stop 1
    end if

  end subroutine print_marbl_log

End Program marbl
