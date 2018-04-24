! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_debug_mod

  !-----------------------------------------------------------------------------
  ! utilities to write info from within MARBL
  !
  ! These utilities are intended to be used only for debugging within MARBL, not
  ! for general purpose output. General purpose output should use marbl_logging.
  !-----------------------------------------------------------------------------

  use marbl_kinds_mod, only : int_kind, r8, log_kind

  implicit none
  private

  public :: marbl_debug_print_var

  interface marbl_debug_print_var
     module procedure marbl_debug_print_var_int_0d, &
                      marbl_debug_print_var_r8_0d,  &
                      marbl_debug_print_var_int_1d, &
                      marbl_debug_print_var_r8_1d
  end interface

  !-----------------------------------------------------------------------------
  ! module parameters
  !-----------------------------------------------------------------------------

  integer(int_kind), parameter :: stdout = 6

  !*****************************************************************************

contains

  !*****************************************************************************

  subroutine marbl_debug_print_var_int_0d(msg, val)

    character(len=*),  intent(in) :: msg
    integer(int_kind), intent(in) :: val

    write(stdout, '(A,1X,I0)') 'marbl_debug:' // trim(msg), val

  end subroutine marbl_debug_print_var_int_0d

  !*****************************************************************************

  subroutine marbl_debug_print_var_r8_0d(msg, val)

    character(len=*), intent(in) :: msg
    real(r8),         intent(in) :: val

    write(stdout, '(A,1X,E25.17)') 'marbl_debug:' // trim(msg), val

  end subroutine marbl_debug_print_var_r8_0d

  !*****************************************************************************

  subroutine marbl_debug_print_var_int_1d(msg, num_elements, val, mask)

    character(len=*),            intent(in) :: msg
    integer(int_kind),           intent(in) :: num_elements
    integer(int_kind),           intent(in) :: val(num_elements)
    logical(log_kind), optional, intent(in) :: mask(num_elements)

    logical(log_kind) :: mask_loc(num_elements)
    integer(int_kind) :: n

    if (present(mask)) then
       mask_loc(:) = mask
    else
       mask_loc(:) = .true.
    end if

    write(stdout, '(A,1X,I0)') 'marbl_debug:' // trim(msg) // ', num_elements=', num_elements
    do n = 1, num_elements
       if (mask_loc(n)) then
          write(stdout, '(A,1X,I0,1X,I0)') 'marbl_debug:' // trim(msg), n, val(n)
       end if
    end do

  end subroutine marbl_debug_print_var_int_1d

  !*****************************************************************************

  subroutine marbl_debug_print_var_r8_1d(msg, num_elements, val, mask)

    character(len=*),            intent(in) :: msg
    integer(int_kind),           intent(in) :: num_elements
    real(r8),                    intent(in) :: val(num_elements)
    logical(log_kind), optional, intent(in) :: mask(num_elements)

    logical(log_kind) :: mask_loc(num_elements)
    integer(int_kind) :: n

    if (present(mask)) then
       mask_loc(:) = mask
    else
       mask_loc(:) = .true.
    end if

    write(stdout, '(A,1X,I0)') 'marbl_debug:' // trim(msg) // ', num_elements=', num_elements
    do n = 1, num_elements
       if (mask_loc(n)) then
          write(stdout, '(A,1X,I0,1X,E25.17)') 'marbl_debug:' // trim(msg), n, val(n)
       end if
    end do

  end subroutine marbl_debug_print_var_r8_1d

end module marbl_debug_mod
