module marbl_netcdf_mod
! This module provides an interface to the netCDF library if MARBL is built with -D_NETCDF
! It is meant to be used with the set_forcing regression test, which must be built with
! netCDF support. Machines without netCDF can still built / run the other tests (without
! -D_NETCDF this module does not include netCDF functions but will not provide any I/O)

  use marbl_kinds_mod,              only : r8, char_len
  use marbl_interface,              only : marbl_interface_class
  use marbl_interface_public_types, only : marbl_diagnostics_type
  use marbl_logging,                only : marbl_log_type

#ifdef _NETCDF
   use netcdf
#endif

  implicit none
  private
  save

  ! MARBL uses a linked list to keep track of what files are open at any given time
  type, private :: marbl_file_entry
    integer :: ncid
    character(len=char_len) :: file_name
    type(marbl_file_entry), pointer :: next => NULL()
  end type marbl_file_entry
  type(marbl_file_entry), pointer :: file_database => NULL()

#ifdef _NETCDF
  ! MARBL needs to track dimension ids to use when defining netCDF variables
  type, private :: netcdf_dimids
    integer :: num_levels_id
    integer :: num_cols_id
  end type netcdf_dimids
  type(netcdf_dimids) :: dimids_out

  ! MARBL needs to track domain variable ids to use when writing netCDF variables
  type, private :: netcdf_domain_ids
    integer :: delta_z_id
    integer :: zt_id
    integer :: zw_id
  end type netcdf_domain_ids
  type(netcdf_domain_ids) :: domain_ids_out
#endif

  public :: marbl_netcdf_open
  public :: marbl_netcdf_enddef
  public :: marbl_netcdf_def_dim
  public :: marbl_netcdf_read_dim
  public :: marbl_netcdf_def_var
  public :: marbl_netcdf_read_var
  public :: marbl_netcdf_put_var
  public :: marbl_netcdf_close
  public :: marbl_netcdf_close_all

  interface marbl_netcdf_read_var
    module procedure marbl_netcdf_read_var_int_0d
    module procedure marbl_netcdf_read_var_r8_0d
    module procedure marbl_netcdf_read_var_r8_1d
  end interface marbl_netcdf_read_var

  interface marbl_netcdf_put_var
    module procedure marbl_netcdf_put_var_r8_1d_by_varname
    module procedure marbl_netcdf_put_var_r8_1d_by_varid
    module procedure marbl_netcdf_put_var_r8_2d_by_varid
  end interface marbl_netcdf_put_var

contains

  !*****************************************************************************

  subroutine marbl_netcdf_open(file_name, read_only, driver_status_log)
    ! Open a netCDF file
    ! if read_only is .true. then file is assumed to exist, but if
    ! read_only is not .true. then new file will be created or the old
    ! file will be clobbered.
    !
    ! Also prepends file info to module variable file_database (used to track
    ! what files are open)

    character(len=*),     intent(in)    :: file_name
    logical,              intent(in)    :: read_only
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_open'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: new_entry, dummy_entry

    nullify(new_entry)
    nullify(dummy_entry)
#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call open(', trim(file_name), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    if (read_only .or. .true.) then ! use read_only to avoid warning when building without netcdf
    end if
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
      call netcdf_check(nf90_open(file_name, NF90_NOWRITE, new_entry%ncid), driver_status_log)
      if (driver_status_log%labort_marbl) &
        write(log_message, "(3A)") 'nf90_open(', trim(file_name), ')'
    else
      call netcdf_check(nf90_create(file_name, NF90_CLOBBER, new_entry%ncid), driver_status_log)
      if (driver_status_log%labort_marbl) &
        write(log_message, "(3A)") 'nf90_create(', trim(file_name), ')'
    end if
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Prepend new_entry to the linked list
    new_entry%next => file_database
    file_database => new_entry
#endif

  end subroutine marbl_netcdf_open

  !*****************************************************************************

  subroutine marbl_netcdf_read_dim(file_name, dim_name, dim, driver_status_log)

    character(len=*),     intent(in)    :: file_name
    character(len=*),     intent(in)    :: dim_name
    integer,              intent(inout) :: dim
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_read_dim'
    character(len=char_len) :: log_message
    integer :: dimid
    integer :: ncid

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call marbl_netcdf_read_dim without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ncid = -1 ! use ncid to avoid warning when building without netcdf
    dimid = -1 ! use ncid to avoid warning when building without netcdf
    dim = len_trim(file_name) + len_trim(dim_name) ! use file_name to avoid warning when building without netcdf
    return
#else
    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    call netcdf_check(nf90_inq_dimid(ncid, trim(dim_name), dimid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_dimid(', trim(dim_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    call netcdf_check(nf90_inquire_dimension(ncid, dimid, len=dim), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_dimension(', trim(dim_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_read_dim

  !*****************************************************************************

  subroutine marbl_netcdf_def_dim(file_name, dim_name, dim, driver_status_log)

    character(len=*),     intent(in)    :: file_name
    character(len=*),     intent(in)    :: dim_name
    integer,              intent(in)    :: dim
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_def_dim'
    character(len=char_len) :: log_message
    integer :: ncid, dimid

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call marbl_netcdf_read_dim without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ncid = dim + len_trim(file_name) + len_trim(dim_name) ! use ncid, dim, file_name, and dim_name to avoid warning when building without netcdf
    dimid = -1  ! use dimid to avoid warning when building without netcdf
    return
#else
    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    call netcdf_check(nf90_def_dim(ncid, dim_name, dim, dimid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_def_dim(num_levels)', subname)
      return
    end if

    ! Store dimid in proper element of dimids_out
    select case (dim_name)
      case ('num_levels')
        dimids_out%num_levels_id = dimid
      case ('num_cols')
        dimids_out%num_cols_id = dimid
    end select
#endif

end subroutine marbl_netcdf_def_dim

  !*****************************************************************************

  subroutine marbl_netcdf_read_var_int_0d(file_name, varname, var, driver_status_log, col_id)
    ! Given netCDF identifier, populate domain variables

    character(len=*),     intent(in)    :: file_name
    character(len=*),     intent(in)    :: varname
    integer,              intent(inout) :: var
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: col_id

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_read_var_int_0d'
    character(len=char_len) :: log_message
    integer :: varid
    integer :: ncid

    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call read_var(', trim(varname), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    if (present(col_id)) varid = col_id ! use varid and col_id to avoid warning when building without netcdf
    var = ncid ! use var and ncid to avoid warning when building without netcdf
    return
#else
    call netcdf_check(nf90_inq_varid(ncid, trim(varname), varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_varid(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    if (present(col_id)) then
      call netcdf_check(nf90_get_var(ncid, varid, var, start=(/col_id/)), driver_status_log)
    else
      call netcdf_check(nf90_get_var(ncid, varid, var), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_get_var(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_read_var_int_0d

  !*****************************************************************************

  subroutine marbl_netcdf_read_var_r8_0d(file_name, varname, var, driver_status_log, col_id)
    ! Given netCDF identifier, populate domain variables

    character(len=*),     intent(in)    :: file_name
    character(len=*),     intent(in)    :: varname
    real(kind=r8),        intent(inout) :: var
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: col_id

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_read_var_r8_0d'
    character(len=char_len) :: log_message
    integer :: varid, nc_status
    real(r8) :: scale_factor
    integer :: ncid

    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call read_var(', trim(varname), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    if (present(col_id)) varid = 0 ! use col_id and varid to avoid warning when building without netcdf
    nc_status = 0 ! use nc_status to avoid warning when building without netcdf
    scale_factor = 1.0_r8 ! use scale_factor to avoid warning when building without netcdf
    var = real(ncid, r8) ! use var and ncid to avoid warning when building without netcdf
    return
#else
    call netcdf_check(nf90_inq_varid(ncid, trim(varname), varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_varid(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    if (present(col_id)) then
      call netcdf_check(nf90_get_var(ncid, varid, var, start=(/col_id/)), driver_status_log)
    else
      call netcdf_check(nf90_get_var(ncid, varid, var), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_get_var(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Get scale_factor, apply it if attribute exists
    nc_status = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    call netcdf_check(nc_status, driver_status_log, ignore_err=NF90_ENOTATT)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_iquire_attribute(scale_factor)', subname)
      return
    end if
    if (nc_status .ne. NF90_ENOTATT) &
      var = scale_factor*var
#endif

  end subroutine marbl_netcdf_read_var_r8_0d

  !*****************************************************************************

  subroutine marbl_netcdf_read_var_r8_1d(file_name, varname, var, driver_status_log, surf_only, col_start, col_cnt)
    ! Given netCDF identifier, populate domain variables

    character(len=*),            intent(in)    :: file_name
    character(len=*),            intent(in)    :: varname
    real(kind=r8), dimension(:), intent(inout) :: var
    type(marbl_log_type),        intent(inout) :: driver_status_log
    logical, optional,           intent(in)    :: surf_only ! if true, only read surface values
    integer, optional,           intent(in)    :: col_start
    integer, optional,           intent(in)    :: col_cnt

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_read_var_r8_1d'
    character(len=char_len) :: log_message
    integer :: ncid
    integer :: varid, num_levels, nc_status
    real(r8) :: scale_factor
    logical :: surf_only_loc

    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    if (present(surf_only)) then
      if ((.not.present(col_start)) .or. (.not.present(col_cnt))) then
        call driver_status_log%log_error('Can not pass surf_only without col_start and col_cnt', subname)
        return
      end if
      surf_only_loc = surf_only
      call marbl_netcdf_read_dim(file_name, 'zt', num_levels, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('marbl_netcdf_read_var(zt)', subname)
        return
      end if
    else
      surf_only_loc = .false.
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call read_var(', trim(varname),') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    var = 0._r8
    varid = 0
    nc_status = 0 ! use nc_status to avoid warning when building without netcdf
    scale_factor = 1.0_r8 ! use scale_factor to avoid warning when building without netcdf
    return
#else
    call netcdf_check(nf90_inq_varid(ncid, trim(varname), varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_varid(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    if (surf_only_loc) then
      call netcdf_check(nf90_get_var(ncid, varid, var, start=(/1, col_start/), count=(/1, col_cnt/)), driver_status_log)
    else
      if (present(col_start)) then
        call netcdf_check(nf90_get_var(ncid, varid, var, (/1, col_start/)), driver_status_log)
      else
        call netcdf_check(nf90_get_var(ncid, varid, var), driver_status_log)
      end if
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_get_var(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Get scale_factor, apply it if attribute exists
    nc_status = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    call netcdf_check(nc_status, driver_status_log, ignore_err=NF90_ENOTATT)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_iquire_attribute(scale_factor)', subname)
      return
    end if
    if (nc_status .ne. NF90_ENOTATT) &
      var = scale_factor*var

#endif

  end subroutine marbl_netcdf_read_var_r8_1d

  !*****************************************************************************

  subroutine marbl_netcdf_def_var(file_name, varname, type, dimnames, long_name, units, driver_status_log, ldef_fillval, varid_out)

    character(len=*),           intent(in)    :: file_name
    character(len=*),           intent(in)    :: varname
    character(len=*),           intent(in)    :: type
    character(len=*),           intent(in)    :: dimnames(:)
    character(len=*),           intent(in)    :: long_name
    character(len=*),           intent(in)    :: units
    type(marbl_log_type),       intent(inout) :: driver_status_log
    logical,          optional, intent(in)    :: ldef_fillval
    integer,          optional, intent(out)   :: varid_out

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_def_var'
    character(len=char_len) :: log_message
    integer, allocatable :: dimids(:)
    integer :: ncid
    integer :: xtype
    integer :: varid
    integer :: i

    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call read_var(', trim(varname),') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ! avoid unused variable warnings when building without netCDF
    if (allocated(dimids) .and. (present(ldef_fillval)) .and. (present(varid_out))) then
      varid = size(dimnames) + len(long_name) + len(units) + len(type)
      xtype = 0
      i = 0
    end if
    return
#else
    allocate(dimids(size(dimnames)))
    do i=1, size(dimids)
      select case (trim(dimnames(i)))
        case ('num_levels')
          dimids(i) = dimids_out%num_levels_id
        case ('num_cols')
          dimids(i) = dimids_out%num_cols_id
      end select
    end do

    select case (type)
      case ('double')
        xtype = NF90_DOUBLE
    end select

    ! Define variable
    call netcdf_check(nf90_def_var(ncid, varname, xtype, dimids, varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_def_var(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Add long_name attribute
    call netcdf_check(nf90_put_att(ncid, varid, "long_name", long_name), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_put_att(long_name) (for ', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Add units attribute
    call netcdf_check(nf90_put_att(ncid, varid, "units", units), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_put_att(units) (for ', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! If requested, and _FillValue attribute (set to default double precision fill value)
    if (present(ldef_fillval)) then
      if (ldef_fillval) then
        call netcdf_check(nf90_put_att(ncid, varid, "_FillValue", NF90_FILL_DOUBLE), driver_status_log)
        if (driver_status_log%labort_marbl) then
          write(log_message, "(3A)") 'nf90_put_att(_FillValue) (for ', trim(varname), ')'
          call driver_status_log%log_error_trace(log_message, subname)
          return
        end if
      end if
    end if

    if (present(varid_out)) then
      varid_out = varid
    else
      select case (varname)
        case ('zt')
          domain_ids_out%zt_id = varid
        case ('zw')
          domain_ids_out%zw_id = varid
      end select
    end if
#endif

  end subroutine marbl_netcdf_def_var

  !*****************************************************************************

  subroutine marbl_netcdf_put_var_r8_1d_by_varname(file_name, varname, var, driver_status_log)

    character(len=*),           intent(in)    :: file_name
    character(len=*),           intent(in)    :: varname
    real(r8),                   intent(in)    :: var(:)
    type(marbl_log_type),       intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_put_var_r8_1d_by_varname'
    character(len=char_len) :: log_message
    integer :: ncid
    integer :: varid

    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call put_var(', trim(varname),') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ! avoid unused variable warnings when building without netCDF
    varid = size(var) + len(varname)
    return
#else
    select case (varname)
      case ('zt')
        varid = domain_ids_out%zt_id
      case ('zw')
        varid = domain_ids_out%zw_id
    end select

    call netcdf_check(nf90_put_var(ncid, varid, var), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_put_var(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_put_var_r8_1d_by_varname

  !*****************************************************************************

  subroutine marbl_netcdf_put_var_r8_1d_by_varid(file_name, varid, var, driver_status_log, mask_in)

    character(len=*),     intent(in)    :: file_name
    integer,              intent(in)    :: varid
    real(r8),             intent(in)    :: var(:)
    type(marbl_log_type), intent(inout) :: driver_status_log
    logical, optional,     intent(in)    :: mask_in(:)

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_put_var_r8_1d_by_varid'
    character(len=char_len) :: log_message
    integer :: ncid, col_id

    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call put_var() without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ! avoid unused variable warnings when building without netCDF
    if (present(mask_in)) &
      col_id = varid + size(var)
    return
#else

    if (present(mask_in)) then
      do col_id=1, size(mask_in)
        if (mask_in(col_id)) &
          call netcdf_check(nf90_put_var(ncid, varid, var(col_id), start=(/col_id/)), driver_status_log)
      end do
    else
      call netcdf_check(nf90_put_var(ncid, varid, var), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A,I0,A)") 'nf90_put_var(varid=', varid, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

end subroutine marbl_netcdf_put_var_r8_1d_by_varid

!*****************************************************************************

subroutine marbl_netcdf_put_var_r8_2d_by_varid(file_name, varid, var, num_active_levels, driver_status_log)

  character(len=*),     intent(in)    :: file_name
  integer,              intent(in)    :: varid
  real(r8),             intent(in)    :: var(:,:)
  integer,               intent(in)   :: num_active_levels(:)
  type(marbl_log_type), intent(inout) :: driver_status_log

  character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_put_var_r8_2d_by_varid'
  character(len=char_len) :: log_message
  integer :: ncid, col_id

  ! Get ncid given file_name
  ncid = get_ncid(file_name, driver_status_log)
  if (driver_status_log%labort_marbl) then
    write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
    call driver_status_log%log_error_trace(log_message, subname)
    return
  end if

#ifndef _NETCDF
  ! Abort if not built with -D_NETCDF
  log_message = 'Can not call put_var() without netCDF support'
  call driver_status_log%log_error(log_message, subname)
  ! avoid unused variable warnings when building without netCDF
  col_id = varid + size(var) + size(num_active_levels)
  return
#else

  do col_id = 1, size(var, dim=2)
    call netcdf_check(nf90_put_var(ncid, varid, var(1:num_active_levels(col_id), col_id), &
                                   start=(/1, col_id/)), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A,I0,A,I0,A)") 'nf90_put_var(varid=', varid, ', col_id=', col_id, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
  end do
#endif

end subroutine marbl_netcdf_put_var_r8_2d_by_varid

  !*****************************************************************************

  subroutine marbl_netcdf_close(file_name, driver_status_log)
    ! Given netCDF file name, close the file and remove it from file_database

    character(len=*),     intent(in)    :: file_name
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_close'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: entry_to_remove
    integer :: ncid

    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! close the file
    entry_to_remove => file_database
    do while (associated(entry_to_remove))
      if (ncid .eq. entry_to_remove%ncid) then
        call close_by_database(entry_to_remove, driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('close_by_database', subname)
        end if
        ! Return after closing file
        return
      end if
      entry_to_remove => entry_to_remove%next
    end do

    ! If loop finishes then file name never matched
    write(log_message, "(A,I0,A)") "File with identifier ", ncid, " is not open!"
    call driver_status_log%log_error(log_message, subname)

  end subroutine marbl_netcdf_close

  !*****************************************************************************

  subroutine marbl_netcdf_close_all(driver_status_log)
    ! Close all the files in file_database

    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_close_all'
    type(marbl_file_entry), pointer :: entry_to_remove

    do while (associated(file_database))
        ! Can not just pass file_database to close_by_database()
        ! because argument is intent(inout) and gets deallocated; we don't
        ! want to deallocate the module variable file_database, we want to
        ! update it to point at next object in link list and then be able
        ! to deallocate the first object in the list
        entry_to_remove => file_database
        call close_by_database(entry_to_remove, driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('marbl_netcdf_close_by_id', subname)
        end if
    end do

  end subroutine marbl_netcdf_close_all

  !*****************************************************************************

  subroutine marbl_netcdf_enddef(file_name, driver_status_log)
    character(len=*),     intent(in)    :: file_name
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_enddef'
    character(len=char_len) :: log_message
    integer :: ncid

    ! Get ncid given file_name
    ncid = get_ncid(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'get_ncid(', trim(file_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call enddef(', trim(file_name),') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    return
#else
    call netcdf_check(nf90_enddef(ncid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_enddef()', subname)
      return
    end if
#endif
  end subroutine marbl_netcdf_enddef

  !*****************************************************************************

  subroutine close_by_database(file_database_entry, driver_status_log)
    ! Given an entry in file_database, close the netCDF file and then
    ! remove the entry from the linked list (private routine called from the
    ! public marbl_netcdf_close interface)

    type(marbl_log_type),            intent(inout) :: driver_status_log
    type(marbl_file_entry), pointer, intent(inout) :: file_database_entry

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:close_by_database'
    type(marbl_file_entry), pointer :: prev_entry

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    prev_entry => file_database_entry ! avoid warning when building without netcdf
    call driver_status_log%log_error('Can not call marbl_netcdf_close without netCDF support', subname)
    return
#else
    ! Close the netCDF file
    call netcdf_check(nf90_close(file_database_entry%ncid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_close', subname)
      return
    end if

    ! File has already been closed, now just need to pop file_database_entry out of linked list
    ! Two options:
    file_database_entry%ncid = -1
    if (file_database%ncid .eq. -1) then
      ! (1) Closing first file
      file_database => file_database_entry%next
    else
      ! (2) Closing file that is not first in the stack
      prev_entry => file_database
      do while (prev_entry%next%ncid .ne. -1)
        prev_entry => prev_entry%next
      end do
      prev_entry%next => file_database_entry%next
    end if
    deallocate(file_database_entry)
#endif

  end subroutine close_by_database

  !*****************************************************************************

  function get_ncid(file_name, driver_status_log)
    ! Given netCDF file name, return ID from file_database

    character(len=*),     intent(in)    :: file_name
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer                             :: get_ncid

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:get_ncid'
    character(len=char_len) :: log_message
    type(marbl_file_entry), pointer :: single_entry

    get_ncid = -1
    if (len_trim(file_name) .eq. 0) then
      log_message = "file_name can not be blank!"
      call driver_status_log%log_error(log_message, subname)
      return
    end if

    single_entry => file_database
    do while (associated(single_entry))
      if (trim(file_name) .eq. trim(single_entry%file_name)) then
        get_ncid = single_entry%ncid
        return
      end if
      single_entry => single_entry%next
    end do
    ! If loop finishes then file name never matched
    write(log_message, "(2A)") trim(file_name), " is not open!"
    call driver_status_log%log_error(log_message, subname)

  end function get_ncid

  !*****************************************************************************

#ifdef _NETCDF
  subroutine netcdf_check(status, driver_status_log, ignore_err)
    ! Private routine to handle errors returned from netcdf
    ! (can only be called if _NETCDF is defined)

    integer,              intent(in)    :: status
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: ignore_err

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:netcdf_check'
    character(len=char_len) :: log_message

    ! User can specify an error to ignore
    if (present(ignore_err)) then
      if (status.eq.ignore_err) return
    end if

    if (status.ne.nf90_noerr) then
      call marbl_netcdf_close_all(driver_status_log)
      write(log_message, "(A,I0,2A)") "netCDF error (", status, "): ", trim(nf90_strerror(status))
      call driver_status_log%log_error(log_message, subname)
    end if

  end subroutine netcdf_check
#endif

  !*****************************************************************************

end module marbl_netcdf_mod
