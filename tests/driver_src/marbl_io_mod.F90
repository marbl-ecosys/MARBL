module marbl_io_mod
! This module provides an interface to the netCDF library if MARBL is built with -D_NETCDF
! It is meant to be used with the set_forcing regression test, which must be built with
! netCDF support. Machines without netCDF can still built / run the other tests (without
! -D_NETCDF this module does not include netCDF functions but will not provide any I/O)

  use marbl_kinds_mod, only : char_len
  use marbl_logging,   only : marbl_log_type
#ifdef _NETCDF
   use netcdf
#endif

  implicit none
  private
  save

  ! MARBL uses a linked list to keep track of what files are open at any given time
  type marbl_file_entry
    integer :: file_id
    character(len=char_len) :: file_name
    type(marbl_file_entry), pointer :: next => NULL()
  end type marbl_file_entry
  type(marbl_file_entry), pointer :: file_database => NULL()

  public :: marbl_io_open
  public :: marbl_io_close
  public :: marbl_io_close_all

  interface marbl_io_close
    module procedure marbl_io_close_by_id
    module procedure marbl_io_close_by_name
  end interface marbl_io_close

contains

  !*****************************************************************************

  subroutine marbl_io_open(file_name, read_only, file_id, driver_status_log)
    ! Open a netCDF file
    ! if read_only is .true. then file is assumed to exist, but if
    ! read_only is not .true. then new file will be created or the old
    ! file will be clobbered.
    !
    ! Also prepends file info to module variable file_database (used to track
    ! what files are open)

    character(len=*),     intent(in)    :: file_name
    logical,              intent(in)    :: read_only
    integer,              intent(out)   :: file_id
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_open'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: new_entry, dummy_entry
    logical :: id_match

    nullify(new_entry)
    nullify(dummy_entry)
#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    call driver_status_log%log_error('Can not call marbl_io_open without netCDF support', subname)
    file_id = -1
    return
#else
    ! Only make netCDF calls if built with -D_NETCDF

    ! Error check: if file_name is already in database, abort!
    dummy_entry => file_database
    do while (associated(dummy_entry))
      if (file_name .eq. trim(dummy_entry%file_name)) then
        write(log_message, "(2A)") file_name, " is already open!"
        call driver_status_log%log_error(log_message, subname)
        return
      end if
      dummy_entry => dummy_entry%next
    end do

    ! Create new entry for database
    allocate(new_entry)
    new_entry%file_name = file_name

    ! Open the file
    if (read_only) then
      call netcdf_check(nf90_open(file_name, NF90_NOWRITE, new_entry%file_id), driver_status_log)
    else
      call netcdf_check(nf90_create(file_name, NF90_CLOBBER, new_entry%file_id), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_open or nf90_create', subname)
      return
    end if

    ! Prepend new_entry to the linked list
    new_entry%next => file_database
    file_database => new_entry
#endif

  end subroutine marbl_io_open

  !*****************************************************************************

  subroutine marbl_io_close_by_database(file_database_entry, driver_status_log)
    ! Given an entry in file_database, close the netCDF file and then
    ! remove the entry from the linked list (private routine called from the
    ! public marbl_io_close interface)

    type(marbl_log_type),            intent(inout) :: driver_status_log
    type(marbl_file_entry), pointer, intent(inout) :: file_database_entry

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_close_by_database'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: prev_entry

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    call driver_status_log%log_error('Can not call marbl_io_close without netCDF support', subname)
    return
#else
    ! Close the netCDF file
    call netcdf_check(nf90_close(file_database_entry%file_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_close', subname)
      return
    end if

    ! File has already been closed, now just need to pop file_database_entry out of linked list
    ! Two options:
    file_database_entry%file_id = -1
    if (file_database%file_id .eq. -1) then
      ! (1) Closing first file
      file_database => file_database_entry%next
    else
      ! (2) Closing file that is not first in the stack
      prev_entry => file_database
      do while (prev_entry%next%file_id .ne. -1)
        prev_entry => prev_entry%next
      end do
      prev_entry%next => file_database_entry%next
    end if
    deallocate(file_database_entry)
#endif

  end subroutine marbl_io_close_by_database

  !*****************************************************************************

  subroutine marbl_io_close_by_id(file_id, driver_status_log)
    ! Given netCDF identifier, close the file and remove it from file_database

    integer,              intent(in)    :: file_id
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_close_by_id'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: entry_to_remove, prev_entry

    entry_to_remove => file_database
    do while (associated(entry_to_remove))
      if (file_id .eq. entry_to_remove%file_id) then
        call marbl_io_close_by_database(entry_to_remove, driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('marbl_io_close_by_database', subname)
        end if
        ! Return after closing file
        return
      end if
      entry_to_remove => entry_to_remove%next
    end do
    ! If loop finishes then file name never matched
    write(log_message, "(A,I0,A)") "File with identifier ", file_id, " is not open!"
    call driver_status_log%log_error(log_message, subname)

  end subroutine marbl_io_close_by_id

  !*****************************************************************************

  subroutine marbl_io_close_by_name(file_name, driver_status_log)
    ! Given netCDF file name, close the file and remove it from file_database

    character(len=*),     intent(in)    :: file_name
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_close_by_name'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: entry_to_remove

    entry_to_remove => file_database
    do while (associated(entry_to_remove))
      if (trim(file_name) .eq. trim(entry_to_remove%file_name)) then
        call marbl_io_close_by_database(entry_to_remove, driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('marbl_io_close_by_database', subname)
        end if
        ! Return after closing file
        return
      end if
      entry_to_remove => entry_to_remove%next
    end do
    ! If loop finishes then file name never matched
    write(log_message, "(2A)") trim(file_name), " is not open!"
    call driver_status_log%log_error(log_message, subname)

  end subroutine marbl_io_close_by_name

  !*****************************************************************************

  subroutine marbl_io_close_all(driver_status_log)
    ! Close all the files in file_database

    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_close_by_name'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: entry_to_remove

    do while (associated(file_database))
        ! Can not just pass file_database to marbl_io_close_by_database()
        ! because argument is intent(inout) and gets deallocated; we don't
        ! want to deallocate the module variable file_database, we want to
        ! update it to point at next object in link list and then be able
        ! to deallocate the first object in the list
        entry_to_remove => file_database
        call marbl_io_close_by_database(entry_to_remove, driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('marbl_io_close_by_id', subname)
        end if
    end do

  end subroutine marbl_io_close_all

  !*****************************************************************************

#ifdef _NETCDF
  subroutine netcdf_check(status, driver_status_log)
    ! Private routine to handle errors returned from netcdf
    ! (can only be called if _NETCDF is defined)

    integer, intent(in)                 :: status
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:netcdf_check'
    character(len=char_len) :: log_message
    if (status.ne.nf90_noerr) then
      call marbl_io_close_all(driver_status_log)
      write(log_message, "(2A)") "netCDF error: ", trim(nf90_strerror(status))
      call driver_status_log%log_error(log_message, subname)
    end if

  end subroutine netcdf_check
#endif

  !*****************************************************************************

end module marbl_io_mod
