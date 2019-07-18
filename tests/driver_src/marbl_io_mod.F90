module marbl_io_mod
! This module provides an interface to the netCDF library if MARBL is built with -D_NETCDF
! It is meant to be used with the set_forcing regression test, which must be built with
! netCDF support. Machines without netCDF can still built / run the other tests (without
! -D_NETCDF this module does not include netCDF functions but will not provide any I/O)

  use marbl_kinds_mod,               only : r8, char_len
  use marbl_interface,               only : marbl_interface_class
  use marbl_interface_public_types , only : marbl_diagnostics_type
  use marbl_logging,                 only : marbl_log_type
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

  ! Driver needs a type to store individual columns from interior
  ! (This is a buffer for the diagnostics so a marbl_instance can be used across columns)
  ! Note:
  !       * field_2d => horizontal field (spatial dimensions are lat and lon)
  !       * field_3d => also vertical (spatial dimensions are lat, lon, and depth)
  !       For this portion of the code, we collapse the horizontal dimensions into
  !       a single dimension with a column index
  type, private :: single_diag_type
    real(r8), allocatable, dimension(:)   :: field_2d     ! dimension: num_cols
    real(r8)                              :: ref_depth_2d ! Not all 2D fields are surface fields
    real(r8), allocatable, dimension(:,:) :: field_3d     ! dimension: num_levels x num_cols
  end type single_diag_type

  type, private :: many_diags_type
    integer                                           :: num_diags
    type(single_diag_type), allocatable, dimension(:) :: diags
  contains
    procedure :: construct => io_diag_construct
    procedure :: destruct  => io_diag_destruct
  end type many_diags_type

  type(many_diags_type), public :: surface_flux_diag_buffer
  type(many_diags_type), public :: interior_tendency_diag_buffer

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

  type, public :: netcdf_prognostic_ids
    integer, allocatable, dimension(:) :: sflux_ids
    integer, allocatable, dimension(:) :: tendency_ids
    integer, allocatable, dimension(:) :: init_ids
  end type netcdf_prognostic_ids
  type(netcdf_prognostic_ids) :: prog_ids_out

  ! netCDF ids usd for writing diagnostic output
  integer, allocatable, dimension(:) :: surface_diag_ids, interior_diag_ids
#endif

  public :: marbl_io_open
  public :: marbl_io_read_dim
  public :: marbl_io_read_field
  public :: marbl_io_define_history
  public :: marbl_io_write_history
  public :: marbl_io_close
  public :: marbl_io_close_all

  interface marbl_io_read_dim
    module procedure marbl_io_read_dim_by_id
    module procedure marbl_io_read_dim_by_name
  end interface marbl_io_read_dim

  interface marbl_io_read_field
    module procedure marbl_io_read_int_field_0d_by_id
    module procedure marbl_io_read_int_field_0d_by_name
    module procedure marbl_io_read_r8_field_0d_by_id
    module procedure marbl_io_read_r8_field_0d_by_name
    module procedure marbl_io_read_r8_field_1d_by_id
    module procedure marbl_io_read_r8_field_1d_by_name
  end interface marbl_io_read_field

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
    write(log_message, "(3A)") 'Can not call open(', trim(file_name), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    if (read_only .or. .true.) then ! use read_only to avoid warning when building without netcdf
      file_id = -1
    end if
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
      if (driver_status_log%labort_marbl) &
        write(log_message, "(3A)") 'nf90_open(', trim(file_name), ')'
    else
      call netcdf_check(nf90_create(file_name, NF90_CLOBBER, new_entry%file_id), driver_status_log)
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

  end subroutine marbl_io_open

  !*****************************************************************************

  subroutine marbl_io_read_dim_by_id(file_id, dim_name, dim, driver_status_log)

    integer,              intent(in)    :: file_id
    character(len=*),     intent(in)    :: dim_name
    integer,              intent(inout) :: dim
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_dim_by_id'
    character(len=char_len) :: log_message
    integer :: dimid

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call read_dim(', trim(dim_name), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    dim = file_id ! use file_id to avoid warning when building without netcdf
    dimid = -1
    return
#else
    call netcdf_check(nf90_inq_dimid(file_id, trim(dim_name), dimid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_dimid(', trim(dim_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    call netcdf_check(nf90_inquire_dimension(file_id, dimid, len=dim), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_dimension(', trim(dim_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_io_read_dim_by_id

  !*****************************************************************************

  subroutine marbl_io_read_dim_by_name(file_name, dim_name, dim, driver_status_log)

    character(len=*),     intent(in)    :: file_name
    character(len=*),     intent(in)    :: dim_name
    integer,              intent(inout) :: dim
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_dim_by_name'
    integer :: file_id

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    call driver_status_log%log_error('Can not call marbl_io_read_dim without netCDF support', subname)
    file_id = -1 ! use file_id to avoid warning when building without netcdf
    dim = len_trim(file_name) + len_trim(dim_name) ! use file_name to avoid warning when building without netcdf
    return
#else
    ! Get file_id given file_name
    file_id = get_nc_file_id(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

    call marbl_io_read_dim_by_id(file_id, dim_name, dim, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_dim_by_id', subname)
      return
    end if
#endif

  end subroutine marbl_io_read_dim_by_name

  !*****************************************************************************

  subroutine marbl_io_read_int_field_0d_by_id(file_id, field_name, field, driver_status_log, col_id)

    integer,              intent(in)    :: file_id
    character(len=*),     intent(in)    :: field_name
    integer,              intent(inout) :: field
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: col_id

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_int_field_0d_by_id'
    character(len=char_len) :: log_message
    integer :: varid

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call read_field(', trim(field_name), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    if (present(col_id)) varid = col_id ! use varid and col_id to avoid warning when building without netcdf
    field = file_id ! use file_id to avoid warning when building without netcdf
    return
#else
    call netcdf_check(nf90_inq_varid(file_id, trim(field_name), varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_varid(', trim(field_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    if (present(col_id)) then
      call netcdf_check(nf90_get_var(file_id, varid, field, start=(/col_id/)), driver_status_log)
    else
      call netcdf_check(nf90_get_var(file_id, varid, field), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_get_var(', trim(field_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_io_read_int_field_0d_by_id

  !*****************************************************************************

  subroutine marbl_io_read_int_field_0d_by_name(file_name, field_name, field, driver_status_log, col_id)
    ! Given netCDF identifier, populate domain variables

    character(len=*),     intent(in)    :: file_name
    character(len=*),     intent(in)    :: field_name
    integer,              intent(inout) :: field
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: col_id

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_int_field_0d_by_id'
    integer :: file_id

    ! Get file_id given file_name
    file_id = get_nc_file_id(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

    call marbl_io_read_int_field_0d_by_id(file_id, field_name, field, driver_status_log, col_id)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_int_field_0d_by_id', subname)
      return
    end if

  end subroutine marbl_io_read_int_field_0d_by_name

  !*****************************************************************************

  subroutine marbl_io_read_r8_field_0d_by_id(file_id, field_name, field, driver_status_log, col_id)

    integer,              intent(in)    :: file_id
    character(len=*),     intent(in)    :: field_name
    real(kind=r8),        intent(inout) :: field
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: col_id

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_r8_field_0d_by_id'
    character(len=char_len) :: log_message
    integer :: varid, nc_status
    real(r8) :: scale_factor

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call read_field(', trim(field_name), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    if (present(col_id)) varid = 0 ! use col_id and var_id to avoid warning when building without netcdf
    nc_status = 0 ! use nc_status to avoid warning when building without netcdf
    scale_factor = 1.0_r8 ! use scale_factor to avoid warning when building without netcdf
    field = real(file_id, r8) ! use file_id to avoid warning when building without netcdf
    return
#else
    call netcdf_check(nf90_inq_varid(file_id, trim(field_name), varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_varid(', trim(field_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    if (present(col_id)) then
      call netcdf_check(nf90_get_var(file_id, varid, field, start=(/col_id/)), driver_status_log)
    else
      call netcdf_check(nf90_get_var(file_id, varid, field), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_get_var(', trim(field_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Get scale_factor, apply it if attribute exists
    nc_status = nf90_get_att(file_id, varid, 'scale_factor', scale_factor)
    call netcdf_check(nc_status, driver_status_log, ignore_err=NF90_ENOTATT)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_iquire_attribute(scale_factor)', subname)
      return
    end if
    if (nc_status .ne. NF90_ENOTATT) &
      field = scale_factor*field

#endif

  end subroutine marbl_io_read_r8_field_0d_by_id

  !*****************************************************************************

  subroutine marbl_io_read_r8_field_0d_by_name(file_name, field_name, field, driver_status_log, col_id)
    ! Given netCDF identifier, populate domain variables

    character(len=*),     intent(in)    :: file_name
    character(len=*),     intent(in)    :: field_name
    real(kind=r8),        intent(inout) :: field
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: col_id

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_r8_field_0d_by_id'
    integer :: file_id

    ! Get file_id given file_name
    file_id = get_nc_file_id(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

    call marbl_io_read_r8_field_0d_by_id(file_id, field_name, field, driver_status_log, col_id)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_r8_field_0d_by_id', subname)
      return
    end if

  end subroutine marbl_io_read_r8_field_0d_by_name

  !*****************************************************************************

  subroutine marbl_io_read_r8_field_1d_by_id(file_id, field_name, field, driver_status_log, surf_only, col_start, col_cnt)
    ! Given netCDF identifier, populate domain variables

    integer,                     intent(in)    :: file_id
    character(len=*),            intent(in)    :: field_name
    real(kind=r8), dimension(:), intent(inout) :: field
    type(marbl_log_type),        intent(inout) :: driver_status_log
    logical, optional,           intent(in)    :: surf_only ! if true, only read surface values
    integer, optional,           intent(in)    :: col_start
    integer, optional,           intent(in)    :: col_cnt

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_r8_field_1d_by_id'
    character(len=char_len) :: log_message
    integer :: varid, num_levels, nc_status
    real(r8) :: scale_factor
    logical :: surf_only_loc

    if (present(surf_only)) then
      if ((.not.present(col_start)) .or. (.not.present(col_cnt))) then
        call driver_status_log%log_error('Can not pass surf_only without col_start and col_cnt', subname)
        return
      end if
      surf_only_loc = surf_only
      call marbl_io_read_dim(file_id, 'zt', num_levels, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('marbl_io_read_field(zt)', subname)
        return
      end if
    else
      surf_only_loc = .false.
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call read_field(', trim(field_name),') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    field = 0._r8
    varid = 0
    nc_status = 0 ! use nc_status to avoid warning when building without netcdf
    scale_factor = 1.0_r8 ! use scale_factor to avoid warning when building without netcdf
    return
#else
    call netcdf_check(nf90_inq_varid(file_id, trim(field_name), varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_varid(', trim(field_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    if (surf_only_loc) then
      call netcdf_check(nf90_get_var(file_id, varid, field, start=(/1, col_start/), count=(/1, col_cnt/)), driver_status_log)
    else
      if (present(col_start)) then
        call netcdf_check(nf90_get_var(file_id, varid, field, (/1, col_start/)), driver_status_log)
      else
        call netcdf_check(nf90_get_var(file_id, varid, field), driver_status_log)
      end if
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_get_var(', trim(field_name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Get scale_factor, apply it if attribute exists
    nc_status = nf90_get_att(file_id, varid, 'scale_factor', scale_factor)
    call netcdf_check(nc_status, driver_status_log, ignore_err=NF90_ENOTATT)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_iquire_attribute(scale_factor)', subname)
      return
    end if
    if (nc_status .ne. NF90_ENOTATT) &
      field = scale_factor*field

#endif

  end subroutine marbl_io_read_r8_field_1d_by_id

  !*****************************************************************************

  subroutine marbl_io_read_r8_field_1d_by_name(file_name, field_name, field, driver_status_log, surf_only, col_start, col_cnt)
    ! Given netCDF identifier, populate domain variables

    character(len=*),            intent(in)    :: file_name
    character(len=*),            intent(in)    :: field_name
    real(kind=r8), dimension(:), intent(inout) :: field
    type(marbl_log_type),        intent(inout) :: driver_status_log
    logical, optional,           intent(in)    :: surf_only ! if true, only read surface values
    integer, optional,           intent(in)    :: col_start
    integer, optional,           intent(in)    :: col_cnt

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_r8_field_1d_by_name'
    integer :: file_id

    ! Get file_id given file_name
    file_id = get_nc_file_id(file_name, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

    call marbl_io_read_r8_field_1d_by_id(file_id, field_name, field, driver_status_log, surf_only, col_start, col_cnt)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_r8_field_1d_by_id', subname)
      return
    end if

  end subroutine marbl_io_read_r8_field_1d_by_name

  !*****************************************************************************

  subroutine marbl_io_define_history(marbl_instances, col_cnt, hist_file, driver_status_log)

    type(marbl_interface_class), dimension(:), intent(in)    :: marbl_instances
    integer,                     dimension(:), intent(in)    :: col_cnt
    character(len=*),                          intent(in)    :: hist_file
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_define_history'
    character(len=char_len) :: log_message, var_name
    integer :: n, diag_size
    integer :: file_id
    integer :: num_levels, num_cols, num_tracers

    ! Get file_id given file_name
    file_id = get_nc_file_id(hist_file, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if
    num_tracers = size(marbl_instances(1)%tracer_metadata)

#ifndef _NETCDF
    write(log_message, "(3A)") 'Can not call define_history(', trim(hist_file), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    write(var_name, "(6I0)") n, diag_size, num_levels, num_cols, num_tracers, col_cnt
    return
#else
    ! netCDF dimensions
    ! 1) num_levels = number of levels (domain should be the same for all columns)
    num_levels = marbl_instances(1)%domain%km
    call netcdf_check(nf90_def_dim(file_id, 'num_levels', num_levels, dimids_out%num_levels_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_def_dim(num_levels)', subname)
      return
    end if

    ! 2) num_cols = number of total columns across all instances
    num_cols = 0
    do n=1, size(marbl_instances)
      ! FIXME: should be num_surface_elem from MARBL, not hard-coded to 1 per instance
      num_cols = num_cols + col_cnt(n)
    end do
    call netcdf_check(nf90_def_dim(file_id, 'num_cols', num_cols, dimids_out%num_cols_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_def_dim(num_cols)', subname)
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

    ! 4) Tracer surface fluxes, tendencies, and initial conditions
    allocate(prog_ids_out%sflux_ids(num_tracers))
    prog_ids_out%sflux_ids(:) = 0
    allocate(prog_ids_out%tendency_ids(num_tracers))
    prog_ids_out%tendency_ids(:) = 0
    allocate(prog_ids_out%init_ids(num_tracers))
    prog_ids_out%init_ids(:) = 0
    do n=1, num_tracers
      ! NOTE: we use log_message as a temporary buffer for strings written as netCDF metadata

      ! Surface fluxes
      write(var_name, "(2A)") "STF_", trim(marbl_instances(1)%tracer_metadata(n)%short_name)
      call netcdf_check(nf90_def_var(file_id, var_name, NF90_DOUBLE, (/dimids_out%num_cols_id/), &
                        prog_ids_out%sflux_ids(n)), driver_status_log)
      write(log_message, "(2A)") "Surface flux of ", trim(marbl_instances(1)%tracer_metadata(n)%long_name)
      call netcdf_check(nf90_put_att(file_id, prog_ids_out%sflux_ids(n), "long_name", log_message), driver_status_log)
      write(log_message, "(2A)") trim(marbl_instances(1)%tracer_metadata(n)%units), ' cm/s'
      call netcdf_check(nf90_put_att(file_id, prog_ids_out%sflux_ids(n), "units", log_message), driver_status_log)

      ! Interior tendencies
      write(var_name, "(2A)") "J_", trim(marbl_instances(1)%tracer_metadata(n)%short_name)
      call netcdf_check(nf90_def_var(file_id, var_name, NF90_DOUBLE, (/dimids_out%num_levels_id, dimids_out%num_cols_id/), &
                        prog_ids_out%tendency_ids(n)), driver_status_log)
      write(log_message, "(2A)") trim(marbl_instances(1)%tracer_metadata(n)%long_name), " Tendency"
      call netcdf_check(nf90_put_att(file_id, prog_ids_out%tendency_ids(n), "long_name", log_message), driver_status_log)
      write(log_message, "(2A)") trim(marbl_instances(1)%tracer_metadata(n)%units), '/s'
      call netcdf_check(nf90_put_att(file_id, prog_ids_out%tendency_ids(n), "units", log_message), driver_status_log)

      ! Initial values
      var_name = marbl_instances(1)%tracer_metadata(n)%short_name
      call netcdf_check(nf90_def_var(file_id, var_name, NF90_DOUBLE, (/dimids_out%num_levels_id, dimids_out%num_cols_id/), &
                        prog_ids_out%init_ids(n)), driver_status_log)
      write(log_message, "(2A)") "Initial value of ", trim(marbl_instances(1)%tracer_metadata(n)%long_name)
      call netcdf_check(nf90_put_att(file_id, prog_ids_out%init_ids(n), "long_name", log_message), driver_status_log)
      log_message = marbl_instances(1)%tracer_metadata(n)%units
      call netcdf_check(nf90_put_att(file_id, prog_ids_out%init_ids(n), "units", log_message), driver_status_log)

    end do

    ! Exit define mode
    call netcdf_check(nf90_enddef(file_id), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_enddef()', subname)
      return
    end if
#endif

  end subroutine marbl_io_define_history

  !*****************************************************************************

  subroutine marbl_io_write_history(hist_file, marbl_instance, surface_fluxes, interior_tendencies, &
                                    tracer_initial_vals, num_active_levels, driver_status_log)

    character(len=*),                              intent(in)    :: hist_file
    type(marbl_interface_class),                   intent(in)    :: marbl_instance
    real(r8),                    dimension(:,:),   intent(in)    :: surface_fluxes       ! num_cols x num_tracers
    real(r8),                    dimension(:,:,:), intent(in)    :: interior_tendencies  ! num_tracers x num_levels x num_cols
    real(r8),                    dimension(:,:,:), intent(in)    :: tracer_initial_vals  ! num_tracers x num_levels x num_cols
    integer,                     dimension(:),     intent(in)    :: num_active_levels
    type(marbl_log_type),                          intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_write_history'
    character(len=char_len) :: log_message
    logical, allocatable, dimension(:) :: mask_2d
    integer :: n, file_id, col_id

    ! Get file_id given file_name
    file_id = get_nc_file_id(hist_file, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('get_nc_file_id', subname)
      return
    end if

    allocate(mask_2d(size(num_active_levels)))

#ifndef _NETCDF
    write(log_message, "(3A)") 'Can not call write_history(', trim(hist_file), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    write(log_message, "(6I0,E10.4)") size(marbl_instance%tracers), size(interior_tendencies), size(surface_fluxes), &
                                      num_active_levels(1), n, col_id, tracer_initial_vals(1,1,1)
    return
#else
    ! 1) Domain variables
    call netcdf_check(nf90_put_var(file_id, domain_ids_out%zt_id, marbl_instance%domain%zt), driver_status_log)
    call netcdf_check(nf90_put_var(file_id, domain_ids_out%zw_id, marbl_instance%domain%zw), driver_status_log)

    ! 2) Surface diagnostics
    mask_2d = .true. ! all surface flux diagnostics are at the surface
    do n=1, surface_flux_diag_buffer%num_diags
      call write_diag(file_id, surface_flux_diag_buffer%diags(n), num_active_levels, surface_diag_ids(n), &
                      mask_2d, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'write_diag(', trim(marbl_instance%surface_flux_diags%diags(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

    ! ! 3) Interior diagnostics
    ! ! FIXME #176: changing num_active_levels to num_levels (km instead of kmt) will populate levels below
    ! !             num_active_levels with nonsensical values
    do n=1, interior_tendency_diag_buffer%num_diags
      ! Some 2D diagnostics come from non-surface levels
      do col_id = 1, size(num_active_levels)
        mask_2d(col_id) = (marbl_instance%domain%zw(num_active_levels(col_id)) .ge. &
                           interior_tendency_diag_buffer%diags(n)%ref_depth_2d)
      end do
      call write_diag(file_id, interior_tendency_diag_buffer%diags(n), num_active_levels, interior_diag_ids(n), &
                      mask_2d, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'write_diag(', trim(marbl_instance%interior_tendency_diags%diags(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

    ! 4) Tracer surface fluxes, tendencies, and initial conditions
    do col_id = 1, size(surface_fluxes, dim=1)
      ! Surface fluxes
      do n=1, size(marbl_instance%tracer_metadata)
        call netcdf_check(nf90_put_var(file_id, prog_ids_out%sflux_ids(n), surface_fluxes(:,n)), driver_status_log)
        if (driver_status_log%labort_marbl) then
          write(log_message, "(3A)") 'nf90_put_var(STF_', trim(marbl_instance%tracer_metadata(n)%short_name), ')'
          call driver_status_log%log_error_trace(log_message, subname)
          return
        end if

        ! Interior tendencies
        call netcdf_check(nf90_put_var(file_id, prog_ids_out%tendency_ids(n), &
                          interior_tendencies(n, 1:num_active_levels(col_id), col_id), &
                          (/1, col_id/)), driver_status_log)
        if (driver_status_log%labort_marbl) then
          write(log_message, "(3A)") 'nf90_put_var(J_', trim(marbl_instance%tracer_metadata(n)%short_name), ')'
          call driver_status_log%log_error_trace(log_message, subname)
          return
        end if

        ! Initial values
        call netcdf_check(nf90_put_var(file_id, prog_ids_out%init_ids(n), &
                          tracer_initial_vals(n, 1:num_active_levels(col_id), col_id), &
                          (/1, col_id/)), driver_status_log)
        if (driver_status_log%labort_marbl) then
          write(log_message, "(3A)") 'nf90_put_var(', trim(marbl_instance%tracer_metadata(n)%short_name), ')'
          call driver_status_log%log_error_trace(log_message, subname)
          return
        end if

      end do
    end do
#endif

    deallocate(mask_2d)

  end subroutine marbl_io_write_history

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
    prev_entry => file_database_entry ! avoid warning when building without netcdf
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

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_close_all'
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

    character(len=*), parameter :: subname = 'marbl_io_mod:get_nc_file_id'
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

#ifdef _NETCDF
  subroutine define_diag(file_id, diag, diag_ind, varid, driver_status_log)

    integer,                      intent(in)    :: file_id
    type(marbl_diagnostics_type), intent(in)    :: diag
    integer,                      intent(in)    :: diag_ind
    integer,                      intent(out)   :: varid
    type(marbl_log_type),         intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:define_diag'
    character(len=char_len) :: log_message, varname

    ! Define netCDF variable with appropriate dimensions
    varname = trim(diag%diags(diag_ind)%short_name)
    select case (trim(diag%diags(diag_ind)%vertical_grid))
      case ('none')
        call netcdf_check(nf90_def_var(file_id, varname, NF90_DOUBLE, &
             (/dimids_out%num_cols_id/), varid), driver_status_log)
      case ('layer_avg')
        call netcdf_check(nf90_def_var(file_id, varname, NF90_DOUBLE, &
             (/dimids_out%num_levels_id, dimids_out%num_cols_id/), varid), driver_status_log)
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
    call netcdf_check(nf90_put_att(file_id, varid, "long_name", &
         trim(diag%diags(diag_ind)%long_name)), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_put_att(long_name)', subname)
      return
    end if

    ! 2) Units
    call netcdf_check(nf90_put_att(file_id, varid, "units", &
         trim(diag%diags(diag_ind)%units)), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_put_att(units)', subname)
      return
    end if

    ! 3) Default fill value
    call netcdf_check(nf90_def_var_fill(file_id, varid, 0, NF90_FILL_DOUBLE), driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_def_var_fill)', subname)
      return
    end if

  end subroutine define_diag
#endif

  !*****************************************************************************

  subroutine io_diag_construct(self, num_levels, num_cols, diags_in)
    class(many_diags_type),    intent(inout) :: self
    integer,                      intent(in)    :: num_levels
    integer,                      intent(in)    :: num_cols
    type(marbl_diagnostics_type), intent(in)    :: diags_in

    integer :: n
    self%num_diags = size(diags_in%diags)
    allocate(self%diags(self%num_diags))
    do n=1, self%num_diags
      if (allocated(diags_in%diags(n)%field_2d)) then
        allocate(self%diags(n)%field_2d(num_cols))
      else if (allocated(diags_in%diags(n)%field_3d)) then
        allocate(self%diags(n)%field_3d(num_levels, num_cols))
      else
        write(*, "(A)") "ERROR: neither field_2d nor field_3d has been allocated"
      end if
    end do

  end subroutine io_diag_construct

  !*****************************************************************************

  subroutine io_diag_destruct(self)
    class(many_diags_type), intent(inout) :: self

    integer :: n
    do n=1, self%num_diags
      if (allocated(self%diags(n)%field_2d)) &
        deallocate(self%diags(n)%field_2d)
      if (allocated(self%diags(n)%field_3d)) &
        deallocate(self%diags(n)%field_3d)
    end do
    self%num_diags = 0

  end subroutine io_diag_destruct

  !*****************************************************************************

#ifdef _NETCDF
  subroutine write_diag(file_id, diag, num_active_levels, varid, mask_2d, driver_status_log)

    use marbl_interface_public_types , only : marbl_diagnostics_type

    integer,                intent(in)    :: file_id
    type(single_diag_type), intent(in)    :: diag
    integer,                intent(in)    :: num_active_levels(:)
    integer,                intent(in)    :: varid
    logical,                intent(in)    :: mask_2d(:)
    type(marbl_log_type),   intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:write_diag'
    integer :: col_id

    if (allocated(diag%field_2d)) then
      do col_id=1, size(diag%field_2d)
        if (mask_2d(col_id)) then
          call netcdf_check(nf90_put_var(file_id, varid, diag%field_2d(col_id), start=(/col_id/)), &
               driver_status_log)
        end if
      end do
    else
      do col_id=1,size(diag%field_3d, dim=2)
        call netcdf_check(nf90_put_var(file_id, varid, diag%field_3d(1:num_active_levels(col_id),col_id), &
                          start=(/1, col_id/)), driver_status_log)
      end do
    end if
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_def_var', subname)
      return
    end if

  end subroutine write_diag
#endif

  !*****************************************************************************

#ifdef _NETCDF
  subroutine netcdf_check(status, driver_status_log, ignore_err)
    ! Private routine to handle errors returned from netcdf
    ! (can only be called if _NETCDF is defined)

    integer,              intent(in)    :: status
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: ignore_err

    character(len=*), parameter :: subname = 'marbl_io_mod:netcdf_check'
    character(len=char_len) :: log_message

    ! User can specify an error to ignore
    if (present(ignore_err)) then
      if (status.eq.ignore_err) return
    end if

    if (status.ne.nf90_noerr) then
      call marbl_io_close_all(driver_status_log)
      write(log_message, "(A,I0,2A)") "netCDF error (", status, "): ", trim(nf90_strerror(status))
      call driver_status_log%log_error(log_message, subname)
    end if

  end subroutine netcdf_check
#endif

  !*****************************************************************************

end module marbl_io_mod
