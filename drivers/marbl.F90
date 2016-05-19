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
  use marbl_init_output_drv

  Implicit None

  type(marbl_interface_class) :: marbl_instance
  character(len=marbl_nl_buffer_size) :: nl_buffer(marbl_nl_cnt)
  character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
  character(len=marbl_nl_in_size)     :: nl_str
  integer                             :: nl_unit
  integer                             :: ioerr
  character(len=256)                  :: testname
  namelist /marbl_driver_nml/testname

  ! (1) Read namelist file to array of strings
  nl_unit      = 10
  nl_buffer(:) = ''
  nl_str       = ''
  testname     = ''

  ! Open file pop_in to read
  open(nl_unit, file='marbl_in', action='read', access='stream', iostat=ioerr)
  if (ioerr.ne.0) then
    write(*,"(A,I0,A)") "ERROR ", ioerr, ": encountered when opening marbl_in"
    stop 1
  end if
  read(unit=nl_unit, iostat=ioerr) nl_str
  if (.not.is_iostat_end(ioerr)) then
    write(*,"(A)") "ERROR encountered when reading marbl_in to buffer"
    stop 1
  end if
  close(nl_unit)
  write(*,"(A,I0,A)") "marbl_in contained ", len_trim(nl_str), " characters"
  call marbl_nl_split_string(nl_str, nl_buffer)


  ! (2) Read driver namelist to know what test to run
  tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_driver_nml')
  read(tmp_nl_buffer, nml=marbl_driver_nml, iostat=ioerr)
  if (ioerr.ne.0) then
    write(*,*) "ERROR reading marbl_driver_nml"
    stop 1
  end if

  ! (3) Run proper test
  write(*,"(3A)") "Beginning ", trim(testname), " test..."
  select case (trim(testname))
    case ("namelist_write")
      call marbl_init_output_test(marbl_instance, nl_buffer)
    case DEFAULT
      write(*,*) "ERROR: testname = ", trim(testname), " is not a valid option"
      stop 1
  end select

  ! (4) Print marbl_log
  call print_marbl_log(marbl_instance%StatusLog)

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
