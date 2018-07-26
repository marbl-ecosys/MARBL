module marbl_utils_mod

  use marbl_kinds_mod, only : r8

  use marbl_logging, only : marbl_log_type

  implicit none
  public

contains

  !***********************************************************************

  function marbl_utils_linear_root(x,y, marbl_status_log) result(linear_root)
    ! Given two points (x(1), y(1)) and (x(2), y(2)), find the root
    ! between x(1) and x(2), or return an error if root doesn't exist
    !
    ! TO-DOs:
    ! (1) this can be generalized to a better root-finding routine
    ! (2) provide a flag to allow a root that is not between x(1) and x(2)
    !     (would only return error if y(1) = y(2) != 0)

    use marbl_constants_mod, only : c0

    real(kind=r8), dimension(2), intent(in)    :: x,y
    type(marbl_log_type),        intent(inout) :: marbl_status_log
    real(kind=r8) :: linear_root

    character(len=*), parameter :: subname = 'marbl_utils_mod:marbl_utils_linear_root'

    real(kind=r8) :: m_inv

    if ((y(1).gt.c0).and.(y(2).gt.c0)) then
       ! If no linear root is found, function will return 0
       linear_root = c0 ! avoid 'linear_root' may be used uninitialized warning from gfortran
       call marbl_status_log%log_error("can not find root, both y-values are positive!", subname)
       return
    else if ((y(1).lt.c0).and.(y(2).lt.c0)) then
       ! If no linear root is found, function will return 0
       linear_root = c0 ! avoid 'linear_root' may be used uninitialized warning from gfortran
       call marbl_status_log%log_error("can not find root, both y-values are negative!", subname)
       return
    end if

    if (y(2).eq.c0) then
       linear_root = x(2)
    else
       m_inv = (x(2)-x(1))/(y(2)-y(1))
       linear_root = x(1)-m_inv*y(1)
    end if

  end function marbl_utils_linear_root

  !***********************************************************************

  subroutine marbl_utils_str_to_substrs(str, separator, substrs)
    ! Given a string and a separator character, break the string into substrings
    ! everywhere the separator appears (unless separator occurs between delimiters
    ! defined in ignore_substr_delims)

    character(len=*),                            intent(in)    :: str
    character,                                   intent(in)    :: separator
    character(len=*), allocatable, dimension(:), intent(out)   :: substrs

    character, dimension(2), parameter :: ignore_substr_delims = (/'"', "'"/)

    character(len=len(substrs)), allocatable, dimension(:) :: substrs_loc
    character(len=len_trim(str)) :: substr
    character :: delim_char
    integer :: char_ind, cur_pos, cur_size
    logical :: linit_new_substr, linside_delim, lseparator_outside_delims

    ! Initialize some local variables and intent(out)
    linit_new_substr = .true.
    linside_delim = .false.
    cur_pos = 1
    if (len_trim(str) .eq. 0) then
      allocate(substrs(1))
      substrs(1) = ''
      return
    end if
    allocate(substrs(0))

    ! Look for separator character, but not between any matched pair of ignore_substr_delims characters
    do char_ind = 1, len_trim(str)
      ! (1) Is str(char_ind:char_ind) the first character in a new substring?
      !     If so, empty substr and set cur_pos = 1
      if (linit_new_substr) then
        substr = ''
        cur_pos  = 1
        linit_new_substr = .false.
      end if

      ! (2) Does current character change whether or not we are between ignore_substr_delims characters?
      if (.not. linside_delim) then
        ! Set linside_delim = true (and set delim_char) if we encounter ANY acceptable delimiters
        if (any(str(char_ind:char_ind) .eq. ignore_substr_delims)) then
          linside_delim = .true.
          delim_char = str(char_ind:char_ind)
        end if
      else ! linside_delim = .true.
        ! If we encounter the delimiter that started substring a second time,
        ! then we have exited the substring
        if (str(char_ind:char_ind) .eq. delim_char) linside_delim = .false.
      end if

      ! Is the current character a separator outside delimiters?
      lseparator_outside_delims = ((.not. linside_delim) .and. (str(char_ind:char_ind) .eq. separator))

      ! (3) Append str(char_ind:char_ind) to substr unless character is a separator
      !     outside of delimiters
      if (.not. lseparator_outside_delims) then
        ! append current character of str to substr
        substr(cur_pos:cur_pos) = str(char_ind:char_ind)
        cur_pos = cur_pos + 1
      end if

      ! (4) Grow substrs(:) by one element and set new element to substr if
      !     str(char_ind:char_ind) is the last character of a substr;
      !     either a separator outside delimiters or the last character of str
      if ( lseparator_outside_delims .or. (char_ind .eq. len_trim(str))) then
        ! (4a) extend substrs by one element
        cur_size = size(substrs)
        if (cur_size .eq. 0) then
          ! end of first substr => allocate substrs(1)
          deallocate(substrs)
          allocate(substrs(1))
        else
          ! substrs already contains some substrings, so copy to substrs_loc
          ! before deallocating and allocating more memory
          allocate(substrs_loc(cur_size))
          substrs_loc = substrs
          deallocate(substrs)
          allocate(substrs(cur_size+1))
          substrs(1:cur_size) = substrs_loc
          deallocate(substrs_loc)
        end if
        ! (4b) store substr as newest element of substrs
        substrs(cur_size+1) = trim(substr)
        ! (4c) next character will be first in the next element
        linit_new_substr = .true.
      end if
    end do

  end subroutine marbl_utils_str_to_substrs

  !***********************************************************************

end module marbl_utils_mod
