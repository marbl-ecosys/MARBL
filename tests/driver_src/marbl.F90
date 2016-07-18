Program marbl

  ! Use from libmarbl.a
  use marbl_interface,    only : marbl_interface_class
  use marbl_logging,      only : marbl_log_type
  use marbl_logging,      only : marbl_status_log_entry_type
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

  type(marbl_interface_class) :: marbl_instance
  type(marbl_log_type)        :: marbl_status_log
  character(len=marbl_nl_buffer_size) :: nl_buffer(marbl_nl_cnt)
  character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
  character(len=marbl_nl_in_size)     :: nl_str, tmp_str
  integer                             :: ioerr=0
  integer                             :: m, n
  character(len=256)                  :: testname
  logical                             :: ldiff_log

  namelist /marbl_driver_nml/testname

  ! (0) Set up local variables
  !     * Empty strings used to pass namelist file contents to MARBL
  nl_buffer(:) = ''
  nl_str       = ''
  !     * Some tests use a different status log than marbl_instance%StatusLog
  !       (default is to use marbl_instance%StatusLog)
  ldiff_log = .false.
  call marbl_status_log%construct()

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
  tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_driver_nml', marbl_status_log)
  if (marbl_status_log%labort_marbl) then
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
      ldiff_log = .true.
      call marbl_get_put_test(marbl_instance, marbl_status_log)
    case DEFAULT
      write(*,*) "ERROR: testname = ", trim(testname), " is not a valid option"
      stop 1
  end select

  ! (4) Print marbl_log
  if (ldiff_log) then
    call print_marbl_log(marbl_status_log)
    if (marbl_instance%StatusLog%labort_marbl) &
      call print_marbl_log(marbl_instance%StatusLog)
  else
    call print_marbl_log(marbl_instance%StatusLog)
  end if

Contains

  subroutine print_marbl_log(log_to_print)

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
