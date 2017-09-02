module marbl_utils_drv

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : char_len
  use marbl_logging,   only : marbl_log_type

  Implicit None
  Private
  Save

  public :: test

contains

  !*****************************************************************************

  subroutine test(driver_status_log)

    type(marbl_log_type),        intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_utils_drv:test'
    character(len=char_len)     :: log_message
    real(kind=r8)               :: linear_root, expected_root
    real(kind=r8), dimension(2) :: x, y
    character(len=char_len)     :: str
    character(len=char_len), allocatable :: array(:), expected_array(:)
    integer                     :: test_cnt, pass_cnt, fail_cnt

    ! Linear root tests
    test_cnt = 0
    pass_cnt = 0
    fail_cnt = 0

    ! (1) root between (1,-1) and (2,1) is at x=2
    test_cnt = test_cnt + 1
    x = (/1.0_r8,2.0_r8/)
    y = (/-1.0_r8, 1.0_r8/)
    expected_root = 1.5_r8
    if (linear_root_test(x, y, expected_root, test_cnt, driver_status_log)) then
      pass_cnt = pass_cnt + 1
    else
      fail_cnt = fail_cnt + 1
    end if

    ! (2) root between (5,0) and (7,3) is at x=5
    test_cnt = test_cnt + 1
    x = (/5.0_r8,7.0_r8/)
    y = (/0.0_r8, 3.0_r8/)
    expected_root = 5.0_r8
    if (linear_root_test(x, y, expected_root, test_cnt, driver_status_log)) then
      pass_cnt = pass_cnt + 1
    else
      fail_cnt = fail_cnt + 1
    end if

    ! (3) root between (5,3) and (7,0) is at x=7
    test_cnt = test_cnt + 1
    x = (/5.0_r8,7.0_r8/)
    y = (/3.0_r8, 0.0_r8/)
    expected_root = 7.0_r8
    if (linear_root_test(x, y, expected_root, test_cnt, driver_status_log)) then
      pass_cnt = pass_cnt + 1
    else
      fail_cnt = fail_cnt + 1
    end if

    ! (4) root between (5,0) and (7,0) is at x=7
    test_cnt = test_cnt + 1
    x = (/5.0_r8,7.0_r8/)
    y = (/0.0_r8, 0.0_r8/)
    expected_root = 7.0_r8
    if (linear_root_test(x, y, expected_root, test_cnt, driver_status_log)) then
      pass_cnt = pass_cnt + 1
    else
      fail_cnt = fail_cnt + 1
    end if

    ! Any failures above?
    if (fail_cnt .gt. 0) then
      write(log_message, "(A,I0,A)") "Failed ", fail_cnt, " linear root test(s)"
      call driver_status_log%log_error(log_message, subname)
      driver_status_log%labort_marbl = .false.
    end if

    ! str_to_array tests
    test_cnt = 0
    pass_cnt = 0
    fail_cnt = 0

    ! (1) ".true." -> ".true."
    test_cnt = test_cnt + 1
    str = ".true."
    allocate(expected_array(1))
    expected_array(1) = ".true."
    if (str_to_array_test(str, expected_array, test_cnt, driver_status_log)) then
      pass_cnt = pass_cnt + 1
    else
      fail_cnt = fail_cnt + 1
    end if
    deallocate(expected_array)

    ! (2) "123, 456" -> "123", " 456"
    ! (3) "'ABC, DEF', GHI" -> "'ABC, DEF', " GHI"

    if (fail_cnt .gt. 0) then
      write(log_message, "(A,I0,A)") "Failed ", fail_cnt, " str_to_array test(s)"
      call driver_status_log%log_error(log_message, subname)
    end if

  end subroutine test

  !*****************************************************************************

  function linear_root_test(x, y, expected_root, test_cnt, driver_status_log) result(ltest_pass)

    use marbl_utils_mod, only : marbl_utils_linear_root

    real(r8), dimension(2), intent(in)    :: x
    real(r8), dimension(2), intent(in)    :: y
    real(r8),               intent(in)    :: expected_root
    integer,                intent(in)    :: test_cnt
    type(marbl_log_type),   intent(inout) :: driver_status_log
    logical :: ltest_pass

    character(len=*), parameter :: subname = 'marbl_utils_drv:linear_root_test'
    character(len=char_len)     :: log_message

    real(r8) :: linear_root
    real(r8) :: tol

    tol = 1e-13_r8
    ltest_pass = .false.

    linear_root = marbl_utils_linear_root(x, y, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_utils_linear_root', subname)
      return
    end if

    if (abs(linear_root - expected_root) .le. tol) then
      ltest_pass = .true.
      write(log_message, "(A,I0,A)") "PASS: Test ", test_cnt, " (linear root)"
      call driver_status_log%log_noerror(log_message, subname)
    else
      write(log_message, "(A,I0,A)") "FAIL: Test ", test_cnt, " (linear root)"
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(A, E24.16)") "       Expected ", expected_root
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(A, E24.16)") "       Result   ", linear_root
      call driver_status_log%log_noerror(log_message, subname)
    end if

  end function linear_root_test

  !*****************************************************************************

  function str_to_array_test(str, expected_array, test_cnt, driver_status_log) result(ltest_pass)

    use marbl_utils_mod, only : marbl_utils_str_to_array

    character(len=*),               intent(in)    :: str
    character(len=*), dimension(:), intent(in)    :: expected_array
    integer,                        intent(in)    :: test_cnt
    type(marbl_log_type),           intent(inout) :: driver_status_log
    logical :: ltest_pass

    character(len=*), parameter :: subname = 'marbl_utils_drv:str_to_array_test'
    character(len=char_len)     :: log_message

    character(len=char_len), allocatable :: array(:)
    integer :: i

    call marbl_utils_str_to_array(str, ",", array)
    ltest_pass = (size(array) .eq. size(expected_array))
    if (.not. ltest_pass) then
      write(log_message, "(A,I0,A)") "FAIL: Test ", test_cnt, " (str_to_array)"
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(A,I0,A)") "      expected ", &
                                     size(expected_array), &
                                     " element(s) in array"
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(A,I0,A)") "      result had ", &
                                     size(array),         &
                                     " element(s)"
      call driver_status_log%log_noerror(log_message, subname)
      deallocate(array)
      return
    end if

    do i=1,size(array)
      if (array(i) .ne. expected_array(i)) then
        if (ltest_pass) then
          ltest_pass = .false.
          write(log_message, "(A,I0,A)") "FAIL: Test ", test_cnt, " (str_to_array)"
          call driver_status_log%log_noerror(log_message, subname)
        end if
        write(log_message, "(2A)") "      Expected ", trim(expected_array(i))
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(2A)") "      Result   ", trim(array(i))
        call driver_status_log%log_noerror(log_message, subname)
      end if
    end do
    deallocate(array)
    if (ltest_pass) then
      write(log_message, "(A,I0,A)") "PASS: Test ", test_cnt, " (str_to_array)"
      call driver_status_log%log_noerror(log_message, subname)
    end if

  end function str_to_array_test

  !*****************************************************************************

end module marbl_utils_drv
