module marbl_io_mod
! This module provides an interface to the netCDF library if MARBL is built with -D_NETCDF
! It is meant to be used with the set_forcing regression test, which must be built with
! netCDF support. Machines without netCDF can still built / run the other tests (without
! -D_NETCDF this module does not include netCDF functions but will not provide any I/O)

  use marbl_kinds_mod, only : r8, char_len
  use marbl_interface, only : marbl_interface_class
  use marbl_logging,   only : marbl_log_type
#ifdef _NETCDF
   use netcdf
#endif

  implicit none
  private
  save

  ! MARBL uses a linked list to keep track of what files are open at any given time
  type, private :: marbl_file_entry
    integer :: file_id
    character(len=char_len) :: file_name
    type(marbl_file_entry), pointer :: next => NULL()
  end type marbl_file_entry
  type(marbl_file_entry), pointer :: file_database => NULL()

  ! MARBL needs to track dimension ids to use when defining netCDF variables
  type, private :: netcdf_dimids
    integer :: num_inst_id
    integer :: num_levels_id
  end type netcdf_dimids
  type(netcdf_dimids) :: dimids_in, dimids_out

  ! MARBL needs to track domain variable ids to use when writing netCDF variables
  type, private :: netcdf_domain_ids
    integer :: delta_z_id
    integer :: zt_id
    integer :: zw_id
  end type netcdf_domain_ids
  type(netcdf_domain_ids) :: domain_ids_in, domain_ids_out

  ! netCDF ids usd for writing diagnostic output
  integer, allocatable, dimension(:) :: surface_diag_ids, interior_diag_ids

  public :: marbl_io_open
  public :: marbl_io_read_domain
  public :: marbl_io_read_forcing_field
  public :: marbl_io_define_diags
  public :: marbl_io_write_diags
  public :: marbl_io_close
  public :: marbl_io_close_all

  interface marbl_io_read_domain
    module procedure marbl_io_read_domain_by_id
    module procedure marbl_io_read_domain_by_name
  end interface marbl_io_read_domain

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

    nullify(new_entry)
    nullify(dummy_entry)
#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    call driver_status_log%log_error('Can not call marbl_io_open without netCDF support', subname)
    file_id = -1
    return
#else
    file_id = 0
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

  subroutine marbl_io_read_domain_by_id(file_id, num_levels, delta_z, zt, zw, driver_status_log)
    ! Given netCDF identifier, populate domain variables

    integer,                                  intent(in)    :: file_id
    integer,                                  intent(inout)  :: num_levels
    real(kind=r8), allocatable, dimension(:), intent(inout) :: delta_z, zt, zw
    type(marbl_log_type),                     intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_domain_by_id'

    ! Get file_id given file_name
    if ((file_id.eq.0).and.(driver_status_log%labort_marbl)) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

    ! Read num_levels dimension
    call netcdf_check(nf90_inq_dimid(file_id, 'zt', dimids_in%num_levels_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_inq_dimid(zt)', subname)
      return
    end if
    call netcdf_check(nf90_inquire_dimension(file_id, dimids_in%num_levels_id, len=num_levels), driver_status_log)

    ! Allocate memory for delta_z, zt, and zw then read from file
    allocate(delta_z(num_levels), zt(num_levels), zw(num_levels))
    call netcdf_check(nf90_inq_varid(file_id, 'delta_z', domain_ids_in%delta_z_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_inq_varid(delta_z)', subname)
      return
    end if
    call netcdf_check(nf90_inq_varid(file_id, 'zt', domain_ids_in%zt_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_inq_varid(zt)', subname)
      return
    end if
    call netcdf_check(nf90_inq_varid(file_id, 'zw', domain_ids_in%zw_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_inq_varid(zw)', subname)
      return
    end if
    call netcdf_check(nf90_get_var(file_id, domain_ids_in%delta_z_id, delta_z), driver_status_log)
    call netcdf_check(nf90_get_var(file_id, domain_ids_in%zt_id, zt), driver_status_log)
    call netcdf_check(nf90_get_var(file_id, domain_ids_in%zw_id, zw), driver_status_log)

  end subroutine marbl_io_read_domain_by_id

  !*****************************************************************************

  subroutine marbl_io_read_domain_by_name(file_name, num_levels, delta_z, zt, zw, driver_status_log)
    ! Given netCDF file name, populate domain variables

    character(len=*),                         intent(in)     :: file_name
    integer,                                  intent(inout)  :: num_levels
    real(kind=r8), allocatable, dimension(:), intent(inout) :: delta_z, zt, zw
    type(marbl_log_type),                     intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_domain_by_name'
    integer :: file_id

    ! Get file_id given file_name
    file_id = get_nc_file_id(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if
    call marbl_io_read_domain_by_id(file_id, num_levels, delta_z, zt, zw, driver_status_log)

  end subroutine marbl_io_read_domain_by_name

  !*****************************************************************************

  subroutine marbl_io_read_forcing_field(file_name, forcing_fields, driver_status_log)

    use marbl_interface_public_types, only : marbl_forcing_fields_type

    character(len=*),                              intent(in)    :: file_name
    type(marbl_forcing_fields_type), dimension(:), intent(inout) :: forcing_fields
    type(marbl_log_type),                          intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_forcing_field'
    character(len=char_len)     :: log_message
    integer :: n, varid, file_id

    ! Get file_id given file_name
    file_id = get_nc_file_id(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

    do n=1, size(forcing_fields)
      select case(trim(forcing_fields(n)%metadata%varname))
        case('u10_sqr')
          call netcdf_check(nf90_inq_varid(file_id, 'u10_sqr', varid), driver_status_log)
        case('sss')
          call netcdf_check(nf90_inq_varid(file_id, 'SSS', varid), driver_status_log)
        case('sst')
          call netcdf_check(nf90_inq_varid(file_id, 'SST', varid), driver_status_log)
        case('Ice Fraction')
          call netcdf_check(nf90_inq_varid(file_id, 'ice_frac', varid), driver_status_log)
        case('Dust Flux')
          call netcdf_check(nf90_inq_varid(file_id, 'dust_flux', varid), driver_status_log)
        case('Iron Flux')
          call netcdf_check(nf90_inq_varid(file_id, 'iron_flux', varid), driver_status_log)
        case('NOx Flux')
          call netcdf_check(nf90_inq_varid(file_id, 'nox_flux', varid), driver_status_log)
        case('NHy Flux')
          call netcdf_check(nf90_inq_varid(file_id, 'nhy_flux', varid), driver_status_log)
        case('Atmospheric Pressure')
          call netcdf_check(nf90_inq_varid(file_id, 'atm_pressure', varid), driver_status_log)
        case('xco2')
          call netcdf_check(nf90_inq_varid(file_id, 'atm_co2', varid), driver_status_log)
        case('xco2_alt_co2')
          call netcdf_check(nf90_inq_varid(file_id, 'atm_alt_co2', varid), driver_status_log)
        case DEFAULT
          write(log_message, "(3A)") "Unrecognized forcing field '", trim(forcing_fields(n)%metadata%varname), "'"
          call driver_status_log%log_error(log_message, subname)
          return
      end select
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "nf90_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      call netcdf_check(nf90_get_var(file_id, varid, forcing_fields(n)%field_0d(1)), driver_status_log)
    end do

  end subroutine marbl_io_read_forcing_field

  !*****************************************************************************

  subroutine marbl_io_define_diags(marbl_instances, outfile, driver_status_log)

    type(marbl_interface_class), dimension(:), intent(in)    :: marbl_instances
    character(len=*),                          intent(in)    :: outfile
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_define_diags'
    character(len=char_len) :: log_message
    integer :: n, diag_size
    integer :: file_id
    integer :: num_inst
    integer :: num_levels

    ! Get file_id given file_name
    file_id = get_nc_file_id(outfile, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

#ifdef _NETCDF
    ! netCDF dimensions
    ! 1) num_inst = number of netCDF instances
    num_inst = size(marbl_instances)
    call netcdf_check(nf90_def_dim(file_id, 'num_inst', num_inst, dimids_out%num_inst_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_def_dim(num_inst)', subname)
      return
    end if

    ! 2) num_levels = number of levels (domain should be the same for all columns)
    num_levels = marbl_instances(1)%domain%km
    call netcdf_check(nf90_def_dim(file_id, 'num_levels', num_levels, dimids_out%num_levels_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_def_dim(num_levels)', subname)
      return
    end if

    ! netCDF variables
    ! 1) Domain variables
    call netcdf_check(nf90_def_var(file_id, 'zt', NF90_DOUBLE, (/dimids_out%num_levels_id/), domain_ids_out%zt_id), &
                      driver_status_log)
    call netcdf_check(nf90_put_att(file_id, domain_ids_out%zt_id, "long_name", "cell center depth"), driver_status_log)
    call netcdf_check(nf90_put_att(file_id, domain_ids_out%zt_id, "units", "m"), driver_status_log)
    call netcdf_check(nf90_def_var(file_id, 'zw', NF90_DOUBLE, (/dimids_out%num_levels_id/), domain_ids_out%zw_id), &
                      driver_status_log)
    call netcdf_check(nf90_put_att(file_id, domain_ids_out%zw_id, "long_name", "cell interface depth"), &
                      driver_status_log)
    call netcdf_check(nf90_put_att(file_id, domain_ids_out%zw_id, "units", "m"), driver_status_log)

    ! 2) Surface diagnostics
    diag_size = size(marbl_instances(1)%surface_flux_diags%diags)
    allocate(surface_diag_ids(diag_size))
    do n=1, diag_size
      call define_diag(file_id, marbl_instances(1)%surface_flux_diags, n, surface_diag_ids(n), &
           driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'define_diag(', trim(marbl_instances(1)%surface_flux_diags%diags(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

    ! 3) Interior diagnostics
    diag_size = size(marbl_instances(1)%interior_tendency_diags%diags)
    allocate(interior_diag_ids(diag_size))
    do n=1, diag_size
      call define_diag(file_id, marbl_instances(1)%interior_tendency_diags, n, interior_diag_ids(n), &
           driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'define_diag(', trim(marbl_instances(1)%interior_tendency_diags%diags(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

    ! Exit define mode
    call netcdf_check(nf90_enddef(file_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_enddef()', subname)
      return
    end if
#endif

  end subroutine marbl_io_define_diags

  !*****************************************************************************

  subroutine marbl_io_write_diags(marbl_instances, outfile, driver_status_log)

    type(marbl_interface_class), dimension(:), intent(in)    :: marbl_instances
    character(len=*),                          intent(in)    :: outfile
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_write_diags'
    character(len=char_len) :: log_message
    integer :: n, diag_size
    integer :: file_id
    integer :: num_inst

    ! Get file_id given file_name
    file_id = get_nc_file_id(outfile, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

#ifdef _NETCDF
    ! 1) Domain variables
    call netcdf_check(nf90_put_var(file_id, domain_ids_out%zt_id, marbl_instances(1)%domain%zt), driver_status_log)
    call netcdf_check(nf90_put_var(file_id, domain_ids_out%zw_id, marbl_instances(1)%domain%zw), driver_status_log)

    ! 2) Surface diagnostics
    diag_size = size(marbl_instances(1)%surface_flux_diags%diags)
    do num_inst=1, size(marbl_instances)
      do n=1, diag_size
        call write_diag(file_id, marbl_instances(num_inst)%surface_flux_diags, n, num_inst, surface_diag_ids(n), &
             driver_status_log)
        if (driver_status_log%labort_marbl) then
          write(log_message, "(3A)") 'write_diag(', trim(marbl_instances(1)%surface_flux_diags%diags(n)%short_name), ')'
          call driver_status_log%log_error_trace(log_message, subname)
          return
        end if
      end do
    end do

    ! 3) Interior diagnostics
    diag_size = size(marbl_instances(1)%interior_tendency_diags%diags)
    do num_inst=1, size(marbl_instances)
      do n=1, diag_size
        call write_diag(file_id, marbl_instances(num_inst)%interior_tendency_diags, n, num_inst, interior_diag_ids(n), &
             driver_status_log)
        if (driver_status_log%labort_marbl) then
          write(log_message, "(3A)") 'write_diag(', trim(marbl_instances(1)%interior_tendency_diags%diags(n)%short_name), ')'
          call driver_status_log%log_error_trace(log_message, subname)
          return
        end if
      end do
    end do
#endif

  end subroutine marbl_io_write_diags

  !*****************************************************************************

  subroutine marbl_io_close_by_database(file_database_entry, driver_status_log)
    ! Given an entry in file_database, close the netCDF file and then
    ! remove the entry from the linked list (private routine called from the
    ! public marbl_io_close interface)

    type(marbl_log_type),            intent(inout) :: driver_status_log
    type(marbl_file_entry), pointer, intent(inout) :: file_database_entry

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_close_by_database'
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
    type(marbl_file_entry), pointer :: entry_to_remove

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
    integer :: file_id

    ! Get file_id given file_name
    file_id = get_nc_file_id(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

    ! close the file
    call marbl_io_close_by_id(file_id, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_close_by_id', subname)
      return
    end if

  end subroutine marbl_io_close_by_name

  !*****************************************************************************

  subroutine marbl_io_close_all(driver_status_log)
    ! Close all the files in file_database

    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_close_by_name'
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

  function get_nc_file_id(file_name, driver_status_log)
    ! Given netCDF file name, return ID from file_database

    character(len=*),     intent(in)    :: file_name
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer                             :: get_nc_file_id

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_close_by_name'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: single_entry

    get_nc_file_id = -1
    if (len_trim(file_name) .eq. 0) then
      log_message = "file_name can not be blank!"
      call driver_status_log%log_error(log_message, subname)
      return
    end if

    single_entry => file_database
    do while (associated(single_entry))
      if (trim(file_name) .eq. trim(single_entry%file_name)) then
        get_nc_file_id = single_entry%file_id
        return
      end if
      single_entry => single_entry%next
    end do
    ! If loop finishes then file name never matched
    write(log_message, "(2A)") trim(file_name), " is not open!"
    call driver_status_log%log_error(log_message, subname)

  end function get_nc_file_id

  !*****************************************************************************

  subroutine define_diag(file_id, diag, diag_ind, ncid, driver_status_log)

    use marbl_interface_public_types , only : marbl_diagnostics_type

    integer,                      intent(in)    :: file_id
    type(marbl_diagnostics_type), intent(in)    :: diag
    integer,                      intent(in)    :: diag_ind
    integer,                      intent(out)   :: ncid
    type(marbl_log_type),         intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:define_diag'
    character(len=char_len) :: log_message, varname

    ! Define netCDF variable with appropriate dimensions
    varname = trim(diag%diags(diag_ind)%short_name)
    select case (trim(diag%diags(diag_ind)%vertical_grid))
      case ('none')
        call netcdf_check(nf90_def_var(file_id, varname, NF90_DOUBLE, &
             (/dimids_out%num_inst_id/), ncid), driver_status_log)
      case ('layer_avg')
        call netcdf_check(nf90_def_var(file_id, varname, NF90_DOUBLE, &
             (/dimids_out%num_levels_id, dimids_out%num_inst_id/), ncid), driver_status_log)
      case DEFAULT
        write(log_message, '(3A)') "'", trim(diag%diags(diag_ind)%vertical_grid), &
             "' is not a valid vertical grid"
        call driver_status_log%log_error(log_message, subname)
        return
    end select
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_def_var(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Add attributes to netCDF variable
    ! 1) Long name
    call netcdf_check(nf90_put_att(file_id, ncid, "long_name", &
         trim(diag%diags(diag_ind)%long_name)), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_put_att(long_name)', subname)
      return
    end if

    ! 2) Units
    call netcdf_check(nf90_put_att(file_id, ncid, "units", &
         trim(diag%diags(diag_ind)%units)), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_put_att(units)', subname)
      return
    end if

  end subroutine define_diag

  !*****************************************************************************

  subroutine write_diag(file_id, diag, diag_ind, num_inst, ncid, driver_status_log)

    use marbl_interface_public_types , only : marbl_diagnostics_type

    integer,                      intent(in)    :: file_id
    type(marbl_diagnostics_type), intent(in)    :: diag
    integer,                      intent(in)    :: diag_ind
    integer,                      intent(in)    :: num_inst
    integer,                      intent(in)    :: ncid
    type(marbl_log_type),         intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:write_diag'
    character(len=char_len) :: log_message

    select case (trim(diag%diags(diag_ind)%vertical_grid))
      case ('none')
        call netcdf_check(nf90_put_var(file_id, ncid, diag%diags(diag_ind)%field_2d(1), (/num_inst/)), driver_status_log)
      case ('layer_avg')
        call netcdf_check(nf90_put_var(file_id, ncid, diag%diags(diag_ind)%field_3d(:,1), (/1,num_inst/)), driver_status_log)
      case DEFAULT
        write(log_message, '(3A)') "'", trim(diag%diags(diag_ind)%vertical_grid), &
             "' is not a valid vertical grid"
        call driver_status_log%log_error(log_message, subname)
        return
    end select
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_def_var', subname)
      return
    end if

  end subroutine write_diag

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
