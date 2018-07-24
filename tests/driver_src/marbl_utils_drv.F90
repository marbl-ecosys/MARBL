module marbl_utils_drv

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : char_len
  use marbl_logging,   only : marbl_log_type

  implicit none
  private
  save

  public :: test

contains

  !*****************************************************************************

  subroutine test(driver_status_log)

    type(marbl_log_type),        intent(inout) :: driver_status_log

    real(kind=r8)               :: expected_root
    real(kind=r8), dimension(2) :: x, y
    character(len=char_len)     :: str, expected_str
    character(len=char_len), allocatable :: expected_substrs(:)
    integer                     :: test_cnt, fail_cnt, tot_fail_cnt
    character(len=*), parameter :: subname = 'marbl_utils_drv:test'
    character(len=char_len)     :: log_message

    ! Linear root tests
    call driver_status_log%log_header("Linear Root Tests", subname)
    test_cnt = 0
    fail_cnt = 0
    tot_fail_cnt = 0

    ! (1) root between (1,-1) and (2,1) is at x=2
    test_cnt = test_cnt + 1
    x = (/ 1.0_r8, 2.0_r8/)
    y = (/-1.0_r8, 1.0_r8/)
    expected_root = 1.5_r8
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log, expected_root)) fail_cnt = fail_cnt + 1

    ! (2) root between (2,1) and (1,-1) is at x=2
    test_cnt = test_cnt + 1
    x = (/2.0_r8,  1.0_r8/)
    y = (/1.0_r8, -1.0_r8/)
    expected_root = 1.5_r8
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log, expected_root)) fail_cnt = fail_cnt + 1

    ! (3) root between (1,1) and (2,-1) is at x=2
    test_cnt = test_cnt + 1
    x = (/1.0_r8,  2.0_r8/)
    y = (/1.0_r8, -1.0_r8/)
    expected_root = 1.5_r8
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log, expected_root)) fail_cnt = fail_cnt + 1

    ! (4) root between (2,-1) and (1,1) is at x=2
    test_cnt = test_cnt + 1
    x = (/ 2.0_r8, 1.0_r8/)
    y = (/-1.0_r8, 1.0_r8/)
    expected_root = 1.5_r8
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log, expected_root)) fail_cnt = fail_cnt + 1

    ! (5) root between (5,0) and (7,3) is at x=5
    test_cnt = test_cnt + 1
    x = (/5.0_r8, 7.0_r8/)
    y = (/0.0_r8, 3.0_r8/)
    expected_root = 5.0_r8
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log, expected_root)) fail_cnt = fail_cnt + 1

    ! (6) root between (5,3) and (7,0) is at x=7
    test_cnt = test_cnt + 1
    x = (/5.0_r8, 7.0_r8/)
    y = (/3.0_r8, 0.0_r8/)
    expected_root = 7.0_r8
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log, expected_root)) fail_cnt = fail_cnt + 1

    ! (7) root between (5,0) and (7,0) is at x=7
    test_cnt = test_cnt + 1
    x = (/5.0_r8, 7.0_r8/)
    y = (/0.0_r8, 0.0_r8/)
    expected_root = 7.0_r8
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log, expected_root)) fail_cnt = fail_cnt + 1

    ! (8) no root between (1,1) and (2,3)
    test_cnt = test_cnt + 1
    x = (/1.0_r8, 2.0_r8/)
    y = (/1.0_r8, 3.0_r8/)
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log)) fail_cnt = fail_cnt + 1

    ! (9) no root between (1,-1) and (2,-3)
    test_cnt = test_cnt + 1
    x = (/ 1.0_r8,  2.0_r8/)
    y = (/-1.0_r8, -3.0_r8/)
    if (.not. linear_root_test(x, y, test_cnt, driver_status_log)) fail_cnt = fail_cnt + 1

    ! Any failures above?
    call analyze_results('linear root', fail_cnt, tot_fail_cnt, driver_status_log)

    ! str_to_substrs tests
    call driver_status_log%log_header("String -> Substrings Tests", subname)
    test_cnt = 0
    fail_cnt = 0

    ! (1) ".true." -> ".true."
    test_cnt = test_cnt + 1
    str = ".true."
    allocate(expected_substrs(1))
    expected_substrs(1) = ".true."
    if (.not. str_to_substrs_test(str, expected_substrs, test_cnt, driver_status_log)) fail_cnt = fail_cnt + 1
    deallocate(expected_substrs)

    ! (2) "123, 456" -> "123", " 456"
    test_cnt = test_cnt + 1
    str = "123, 456"
    allocate(expected_substrs(2))
    expected_substrs(1) = "123"
    expected_substrs(2) = " 456"
    if (.not. str_to_substrs_test(str, expected_substrs, test_cnt, driver_status_log)) fail_cnt = fail_cnt + 1
    deallocate(expected_substrs)

    ! (3) "'ABC, DEF', 'GHI'" -> "'ABC, DEF', " 'GHI'"
    test_cnt = test_cnt + 1
    str = "'ABC, DEF', 'GHI'"
    allocate(expected_substrs(2))
    expected_substrs(1) = "'ABC, DEF'"
    expected_substrs(2) = " 'GHI'"
    if (.not. str_to_substrs_test(str, expected_substrs, test_cnt, driver_status_log)) fail_cnt = fail_cnt + 1
    deallocate(expected_substrs)

    ! Any failures above?
    call analyze_results('string to substrings', fail_cnt, tot_fail_cnt, driver_status_log)

    ! strip_comment tests
    call driver_status_log%log_header("Comment-Stripping Tests", subname)
    test_cnt = 0
    fail_cnt = 0

    ! (1) "ciso_on = .true.  ! Turn on ciso" -> "ciso_on = .true.  "
    test_cnt = test_cnt + 1
    str = "ciso_on = .true.  ! Turn on ciso"
    expected_str = "ciso_on = .true."
    if (.not. strip_comments_test(str, expected_str, test_cnt, driver_status_log)) fail_cnt = fail_cnt + 1

    ! (2) "autotrophs(1)%lname='Small Phytoplankton!'" -> "autotrophs(1)%lname='Small Phytoplankton!'"
    test_cnt = test_cnt + 1
    str = "autotrophs(1)%lname='Small Phytoplankton!'"
    expected_str = str
    if (.not. strip_comments_test(str, expected_str, test_cnt, driver_status_log)) fail_cnt = fail_cnt + 1

    ! Any failures above?
    call analyze_results('comment stripping', fail_cnt, tot_fail_cnt, driver_status_log)

    call driver_status_log%log_noerror('', subname)
    if (tot_fail_cnt .eq. 0) then
      call driver_status_log%log_noerror('All unit tests passed!', subname)
    else
      write(log_message, "(A,I0,A)") "Failed ", tot_fail_cnt, " unit test(s)."
      call driver_status_log%log_error(log_message, subname)
    end if

  end subroutine test

  !*****************************************************************************

  function linear_root_test(x, y, test_cnt, driver_status_log, expected_root) result(ltest_pass)

    use marbl_utils_mod, only : marbl_utils_linear_root

    real(r8), dimension(2), intent(in)    :: x
    real(r8), dimension(2), intent(in)    :: y
    integer,                intent(in)    :: test_cnt
    type(marbl_log_type),   intent(inout) :: driver_status_log
    real(r8), optional,     intent(in)    :: expected_root
    logical :: ltest_pass

    character(len=*), parameter :: subname = 'marbl_utils_drv:linear_root_test'
    character(len=char_len)     :: log_message
    character(len=char_len)     :: test_desc

    real(r8) :: linear_root
    real(r8) :: tol

    tol = 1e-13_r8
    ltest_pass = .false.
    write(test_desc, "(A,F0.1,A,F0.1,A,F0.1,A,F0.1,A)") "root between (", x(1), ", ", y(1), ") and (", x(2), ", ", y(2), ')'

    if (.not. present(expected_root)) driver_status_log%OutputOptions%lLogError=.false.
    linear_root = marbl_utils_linear_root(x, y, driver_status_log)
    if (present(expected_root)) then ! expecting real result
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('marbl_utils_linear_root', subname)
        return
      end if

      if (abs(linear_root - expected_root) .le. tol) then
        ltest_pass = .true.
        write(log_message, "(A,I0,3A)") "PASS: Test ", test_cnt, " [linear root: ", trim(test_desc), "]"
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(6X,A,F0.1)") "Root at x = ", linear_root
        call driver_status_log%log_noerror(log_message, subname)
      else
        write(log_message, "(A,I0,3A)") "FAIL: Test ", test_cnt, " [linear root: ", trim(test_desc), "]"
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(6X,A,F0.1)") "Expected root: ", expected_root
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(6X,A,E24.16)") "Found root:   ", linear_root
        call driver_status_log%log_noerror(log_message, subname)
      end if
    else ! no expected real root => expect error
      driver_status_log%OutputOptions%lLogError=.true.
      ltest_pass = driver_status_log%labort_marbl
      driver_status_log%labort_marbl = .false.
      if (ltest_pass) then
        write(log_message, "(A,I0,3A)") "PASS: Test ", test_cnt, " (linear root: ", trim(test_desc), "]"
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(6X,A)")   "No root found"
        call driver_status_log%log_noerror(log_message, subname)
      else
        write(log_message, "(A,I0,3A)") "FAIL: Test ", test_cnt, " (linear root: ", trim(test_desc), "]"
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(6X,A)") "Expected no root"
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(6X,A,E24.16)") "fount root: ", linear_root
        call driver_status_log%log_noerror(log_message, subname)
      end if
    end if

  end function linear_root_test

  !*****************************************************************************

  function str_to_substrs_test(str, expected_substrs, test_cnt, driver_status_log) result(ltest_pass)

    use marbl_utils_mod, only : marbl_utils_str_to_substrs

    character(len=*),               intent(in)    :: str
    character(len=*), dimension(:), intent(in)    :: expected_substrs
    integer,                        intent(in)    :: test_cnt
    type(marbl_log_type),           intent(inout) :: driver_status_log
    logical :: ltest_pass

    character(len=*), parameter :: subname = 'marbl_utils_drv:str_to_substrs_test'
    character(len=char_len)     :: log_message

    character(len=char_len), allocatable :: substrs(:)
    integer :: i

    call marbl_utils_str_to_substrs(str, ",", substrs)
    ltest_pass = (size(substrs) .eq. size(expected_substrs))
    if (.not. ltest_pass) then
      write(log_message, "(A,I0,3A)") "FAIL: Test ", test_cnt, " [str_to_substrs: ", trim(str), "]"
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(6X,A,I0,A)") "expected ", size(expected_substrs), " element(s) in substrs"
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(6X,A,I0,A)") "result had ", size(substrs), " element(s)"
      call driver_status_log%log_noerror(log_message, subname)
      deallocate(substrs)
      return
    end if

    do i=1,size(substrs)
      if (substrs(i) .ne. expected_substrs(i)) then
        ltest_pass = .false.
        write(log_message, "(A,I0,3A)") "FAIL: Test ", test_cnt, " (str_to_substrs: ", trim(str), ")"
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(6X,A,I0,2A)") "Expected substr(", i, "): ", trim(expected_substrs(i))
        call driver_status_log%log_noerror(log_message, subname)
        write(log_message, "(6X,A,I0,2A)") "Result substr(", i, "): ", trim(substrs(i))
        call driver_status_log%log_noerror(log_message, subname)
      end if
    end do

    if (ltest_pass) then
      write(log_message, "(A,I0,3A)") "PASS: Test ", test_cnt, " (str_to_substrs: ", trim(str), ")"
      call driver_status_log%log_noerror(log_message, subname)
      do i=1,size(substrs)
        write(log_message, "(6X,A,I0,2A)") "substr(", i, "): ", trim(substrs(i))
        call driver_status_log%log_noerror(log_message, subname)
      end do
    end if

    deallocate(substrs)

  end function str_to_substrs_test

  !*****************************************************************************

  function strip_comments_test(str, expected_str, test_cnt, driver_status_log) result(ltest_pass)

    use marbl_utils_mod, only : marbl_utils_str_to_substrs

    character(len=*),     intent(in)    :: str
    character(len=*),     intent(in)    :: expected_str
    integer,              intent(in)    :: test_cnt
    type(marbl_log_type), intent(inout) :: driver_status_log
    logical :: ltest_pass

    character(len=*), parameter :: subname = 'marbl_utils_drv:str_to_substrs_test'
    character(len=char_len)     :: log_message

    character(len=char_len), allocatable :: substrs(:)

    call marbl_utils_str_to_substrs(str, "!", substrs)
    ltest_pass = trim(substrs(1)) .eq. trim(expected_str)
    if (ltest_pass) then
      write(log_message, "(A,I0,3A)") "PASS: Test ", test_cnt, " (strip_comments: ", trim(str), ")"
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(6X,2A)") "string: ", trim(substrs(1))
      call driver_status_log%log_noerror(log_message, subname)
    else
      write(log_message, "(A,I0,3A)") "FAIL: Test ", test_cnt, " (strip_comments: ", trim(str), ")"
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(6X,2A)") "Expected: ", trim(expected_str)
      call driver_status_log%log_noerror(log_message, subname)
      write(log_message, "(6X,2A)") "Result:   ", trim(substrs(1))
      call driver_status_log%log_noerror(log_message, subname)
    end if

  end function strip_comments_test

  !*****************************************************************************

  subroutine analyze_results(testname, fail_cnt, tot_fail_cnt, driver_status_log)

    character(len=*),     intent(in)    :: testname
    integer,              intent(in)    :: fail_cnt
    integer,              intent(inout) :: tot_fail_cnt
    type(marbl_log_type), intent(inout) :: driver_status_log
    character(len=char_len)     :: log_message

    character(len=*), parameter :: subname = 'marbl_utils_drv:analyze_results'
    call driver_status_log%log_noerror('', subname)
    if (fail_cnt .gt. 0) then
      write(log_message, "(A,I0,1X,A,1X,A)") "Failed ", fail_cnt, trim(testname), "test(s)"
      call driver_status_log%log_error(log_message, subname)
      driver_status_log%labort_marbl = .false.
    else
      write(log_message, "(3A)") "** passed all ", trim(testname), " tests **"
      call driver_status_log%log_noerror(log_message, subname)
    end if
    tot_fail_cnt = tot_fail_cnt + fail_cnt

  end subroutine analyze_results

  !*****************************************************************************

end module marbl_utils_drv
