module marbl_namelist_mod

  ! This module contains a few parameters to control the size of the buffer used
  ! to pass the namelist from the GCM to MARBL as well as functions to convert
  ! from one long string to an array of strings each containing a single
  ! namelist. There is also a function to return the requested namelist, which
  ! is used for the read() calls. This is needed due to what I believe is a bug
  ! in gfortran, where reading a string that does not contain the requested
  ! namelist returns a successful status code so we can't just loop over all
  ! elements of nl_buffer until the namelist has been read.

  implicit none
  private

  ! FIXME #33: nl_buffer_size shouldn't be a hard coded constant
  !            but runtime configurable?! Just not sure what the
  !            best approach is at the moment....

  ! NOTES:
  !        nl_cnt is the number of distinct namelists in the file
  !        nl_buffer_size is the number of characters in the largest _nml

  integer, public, parameter :: marbl_nl_cnt = 256
  integer, public, parameter :: marbl_nl_buffer_size = 32768

  ! Need to know what carriage return is on the system; use #define if we
  ! come across a machine that doesn't use achar(10)

  character,       parameter :: cr = achar(10)

  public :: marbl_nl_split_string
  public :: marbl_namelist

  !***********************************************************************

contains

  !***********************************************************************

  subroutine marbl_nl_split_string(str_in, array_out)
  ! This routine takes a string (str_in) containing the entire contents of a
  ! a namelist file and returns an array of strings (array_out) where each
  ! element contains a single namelist. It also removes all carriage returns
  ! from the elements of array_out

    ! FIXME #34: This routine depends on the namelist file conforming
    !            to very specific formatting - a more general / robust
    !            solution would be preferred

    ! FIXME #74: Strip comments out of str_in (without accidentally removing
    !            strings that happen to contain exclamation points)

    character(len=*), intent(in) :: str_in
    ! array_out is intent(inout) because we initialized to '' previously
    ! (and also to save memory)
    character(len=marbl_nl_buffer_size), dimension(marbl_nl_cnt), intent(inout) :: &
             array_out

    character(len=marbl_nl_buffer_size) :: str_tmp
    integer :: old_pos, nl_cnt, i, j

    ! each namelist needs to be stored in different element of array_out
    old_pos = 1
    nl_cnt = 1
    do i=1,len_trim(str_in)-1
      if (str_in(i:i+1) .eq. '/' // cr) then
        ! FIXME #32: add error checking in case 
        !            (i+1-old_pos) > marbl_nl_buffer_size
        array_out(nl_cnt) = str_in(old_pos:i)
        nl_cnt = nl_cnt+1
        old_pos = i+2
      end if
    end do

    ! We need to strip carriage returns from the namelist, replacing them with
    ! empty space
    do j= 1,nl_cnt
      str_tmp = array_out(j)
      do i=1,len_trim(str_tmp)
        if (str_tmp(i:i).eq.cr) then
          str_tmp(i:i) = ' '
        end if
      end do
      ! Remove whitespace from beginning of string (if any)
      array_out(j) = trim(adjustl(str_tmp))
      ! FIXME #32: add error checking in case first character is not '&'
    end do
  end subroutine marbl_nl_split_string

  !*****************************************************************************

  function marbl_namelist(nl_buffer, nl_name, marbl_status_log)

    use marbl_logging,   only : marbl_log_type
    use marbl_kinds_mod, only : char_len

    character(len=marbl_nl_buffer_size), intent(in) :: nl_buffer(:)
    character(len=*), intent(in) :: nl_name
    type(marbl_log_type), intent(inout) :: marbl_status_log
    character(len=marbl_nl_buffer_size) :: marbl_namelist

    character(*), parameter :: subname = 'marbl_namelist_mod:marbl_namelist'
    character(len=char_len) :: log_message
    character(len=marbl_nl_buffer_size) :: single_namelist
    integer :: j, n

    ! Will return empty string if namelist not found
    marbl_namelist = ''

    ! Look for correct namelist in array
    do j = 1, size(nl_buffer)
      single_namelist = nl_buffer(j)
      n = len_trim(nl_name)
      if (single_namelist(2:n+1).eq.trim(nl_name)) then
         marbl_namelist = single_namelist
         exit
      end if
    end do

    if (trim(marbl_namelist).eq.'') then
      write(log_message, "(2A)") trim(nl_name), ' is not included in nl_buffer'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

  end function marbl_namelist

  !*****************************************************************************

end module marbl_namelist_mod
