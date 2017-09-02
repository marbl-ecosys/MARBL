module marbl_utils_drv

  use marbl_kinds_mod, only : r8

  Implicit None
  Private
  Save

  public :: test

contains

  subroutine test(driver_status_log)

    use marbl_logging,   only : marbl_log_type
    use marbl_utils_mod, only : marbl_utils_linear_root
    use marbl_utils_mod, only : marbl_utils_str_to_array
    use marbl_kinds_mod, only : char_len

    type(marbl_log_type),        intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_utils_drv:test'
    character(len=char_len)     :: log_message
    real(kind=r8), dimension(2) :: x, y
    real(kind=r8) :: linear_root
    real(kind=r8) :: expected_val
    integer       :: test_cnt, pass_cnt, fail_cnt


    ! Linear root tests
    test_cnt = 0
    pass_cnt = 0
    fail_cnt = 0

    ! (1) root between (1,-1) and (2,1) is at x=2
    test_cnt = test_cnt + 1
    x = (/1.0_r8,2.0_r8/)
    y = (/-1.0_r8, 1.0_r8/)
    expected_val = 1.5_r8
    linear_root = marbl_utils_linear_root(x, y, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_utils_linear_root', subname)
      return
    end if
    if (close_enough(linear_root, expected_val, tol=1e-13_r8)) then
      pass_cnt = pass_cnt + 1
      write(log_message, "(A,I0,A)") "PASS: Test ", test_cnt, " (linear root)"
    else
      fail_cnt = fail_cnt + 1
      write(log_message, "(A,I0,A)") "FAIL: Test ", test_cnt, " (linear root)"
    end if
    call driver_status_log%log_noerror(log_message, subname)

    ! (2) root between (5,0) and (7,3) is at x=5
    ! (3) root between (5,3) and (7,0) is at x=7

    ! str_to_array tests

  end subroutine test

  function close_enough(x, y, tol)

    real(kind=r8), intent(in) :: x, y, tol
    logical :: close_enough

    close_enough = (abs(x-y).le.tol)


  end function close_enough

end module marbl_utils_drv
