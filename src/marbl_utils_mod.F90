module marbl_utils_mod

  implicit none
  public

contains

  subroutine marbl_utils_str_to_array(str, separator, array)

    character(len=*),                            intent(in)    :: str
    character,                                   intent(in)    :: separator
    character(len=*), allocatable, dimension(:), intent(out)   :: array

    character, dimension(2), parameter :: ignore_substr_delims = (/'"', "'"/)

    character(len=len(array)), allocatable, dimension(:) :: array_loc
    character(len=len_trim(str)) :: str_loc
    character :: delim_char
    integer :: char_ind, cur_pos, cur_size
    logical :: lfirst_char_in_elem
    logical :: linside_delim

    ! Initialize some local variables and intent(out)
    lfirst_char_in_elem = .true.
    linside_delim = .false.
    if (len_trim(str) .eq. 0) then
      allocate(array(1))
      array(1) = ''
      return
    end if
    allocate(array(0))

    ! Look for separator character, but not between any matched pair of ignore_substr_delims characters
    do char_ind = 1, len_trim(str)
      ! (1) If this is first character in an element of array, reset some local vars
      if (lfirst_char_in_elem) then
        str_loc = ''
        cur_pos  = 1
        lfirst_char_in_elem = .false.
      end if

      ! (2) Does current character change whether or not we are between ignore_substr_delims characters?
      if (.not. linside_delim) then
        ! Set to true (and set delim_char) if we encounter ANY acceptable delimiters
        if (any(str(char_ind:char_ind) .eq. ignore_substr_delims)) then
          linside_delim = .true.
          delim_char = str(char_ind:char_ind)
        end if
      else ! linside_delim = .true.
        ! If we encounter the delimiter that started substring a second time,
        ! then we have exited the substring
        if (str(char_ind:char_ind) .eq. delim_char) linside_delim = .false.
      end if

      ! Inside delimiter or processing character that is not separator?
      if (linside_delim .or. (str(char_ind:char_ind) .ne. separator)) then
        ! append current character of str to str_loc
        str_loc(cur_pos:cur_pos) = str(char_ind:char_ind)
        cur_pos = cur_pos + 1
      end if

      ! Processing either the last character or a separator outside of delimiters?
      if (((.not. linside_delim) .and. (str(char_ind:char_ind) .eq. separator)) .or. &
          (char_ind .eq. len_trim(str))) then
        ! extend array to be dimension elem_ind
        cur_size = size(array)
        if (cur_size .gt. 0) then
          allocate(array_loc(cur_size))
          array_loc = array
          deallocate(array)
          allocate(array(cur_size+1))
          array(1:cur_size) = array_loc
          deallocate(array_loc)
        else
          deallocate(array)
          allocate(array(1))
        end if
        ! store str_loc as newest element of array
        array(cur_size+1) = trim(str_loc)
        ! next character will be first in the next element
        lfirst_char_in_elem = .true.
      end if
    end do

  end subroutine marbl_utils_str_to_array

end module marbl_utils_mod