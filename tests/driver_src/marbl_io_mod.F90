module marbl_io_mod
! This module provides MARBL-specific routines that call marbl_netcdf_mod to perform
! I/O. It is designed to build regardless of whether the model is built with netCDF support,
! but will report errors from marbl_netcdf_mod if netCDF is not linked when required.

  use marbl_kinds_mod, only : r8, char_len

  use marbl_logging, only : marbl_log_type

  use marbl_interface, only : marbl_interface_class

  use marbl_interface_public_types, only : marbl_tracer_metadata_type

  use marbl_netcdf_mod, only : marbl_netcdf_def_var
  use marbl_netcdf_mod, only : marbl_netcdf_inq_dimid
  use marbl_netcdf_mod, only : marbl_netcdf_inq_varid
  use marbl_netcdf_mod, only : marbl_netcdf_get_var
  use marbl_netcdf_mod, only : marbl_netcdf_put_var

  implicit none
  private
  save

  type, public :: grid_data_type
    real(kind=r8), allocatable, dimension(:) :: delta_z, zt, zw
  end type grid_data_type

  ! There may be two netCDF files open simultaneously:
  ! 1. File containing initial conditions to be read
  ! 2. File where we write diagnostics and other output
  integer :: ncid_in
  integer :: ncid_out

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
    character(len=char_len)               :: short_name
  end type single_diag_type

  type, private :: many_diags_type
    integer                                           :: num_diags
    type(single_diag_type), allocatable, dimension(:) :: diags
  contains
    procedure :: construct => io_diag_construct
    procedure :: destruct  => io_diag_destruct
  end type many_diags_type
  type(many_diags_type) :: surface_flux_diag_buffer
  type(many_diags_type) :: interior_tendency_diag_buffer

  public :: marbl_io_open_files
  public :: marbl_io_distribute_cols
  public :: marbl_io_construct_diag_buffers
  public :: marbl_io_read_domain
  public :: marbl_io_read_forcing_field
  public :: marbl_io_read_tracers_at_surface
  public :: marbl_io_read_tracers
  public :: marbl_io_get_init_file_var_by_name
  public :: marbl_io_define_history
  public :: marbl_io_copy_into_diag_buffer
  public :: marbl_io_write_history
  public :: marbl_io_close_files
  public :: marbl_io_destruct_diag_buffers

  interface marbl_io_copy_into_diag_buffer
    module procedure marbl_io_copy_into_surface_diag_buffer
    module procedure marbl_io_copy_into_interior_diag_buffer
  end interface marbl_io_copy_into_diag_buffer

  interface marbl_io_get_init_file_var_by_name
    module procedure marbl_io_get_init_file_var_by_name_int_1d
    module procedure marbl_io_get_init_file_var_by_name_r8_1d
  end interface marbl_io_get_init_file_var_by_name

contains

  !*****************************************************************************

  subroutine marbl_io_open_files(init_file, hist_file, driver_status_log)

    use marbl_netcdf_mod, only : marbl_netcdf_open
    use marbl_netcdf_mod, only : marbl_netcdf_create

    character(len=*),     intent(in)    :: init_file
    character(len=*),     intent(in)    :: hist_file
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_open_files'
    character(len=char_len) :: log_message

    ! Open initial condition file (read-only)
    call marbl_netcdf_open(init_file, ncid_in, driver_status_log)
    if (driver_status_log%labort_MARBL) then
      write(log_message, "(3A)") "marbl_netcdf_open(", trim(init_file), ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Open history file (writeable / clobber existing)
    call marbl_netcdf_create(hist_file, ncid_out, driver_status_log)
    if (driver_status_log%labort_MARBL) then
      write(log_message, "(3A)") "marbl_netcdf_create(", trim(hist_file), ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

  end subroutine marbl_io_open_files

  !*****************************************************************************

  subroutine marbl_io_get_init_file_dim_by_name(name, len, driver_status_log)

    use marbl_netcdf_mod, only : marbl_netcdf_inquire_dimension

    character(len=*),     intent(in)    :: name
    integer,              intent(out)   :: len
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_get_init_file_dim_by_name'
    character(len=char_len) :: log_message
    integer :: dimid

    ! 1. Get dimid for dimension
    call marbl_netcdf_inq_dimid(ncid_in, trim(name), dimid, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") "marbl_netcdf_inq_dimid(", trim(name), ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! 2. Get value of dimension
    call marbl_netcdf_inquire_dimension(ncid_in, dimid, len, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") "marbl_netcdf_inquire_dimension(", trim(name), ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

  end subroutine marbl_io_get_init_file_dim_by_name

  !*****************************************************************************

  subroutine marbl_io_get_init_file_var_by_name_int_1d(name, var, driver_status_log)

    character(len=*),     intent(in)    :: name
    integer,              intent(out)   :: var(:)
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_get_init_file_var_by_name_int_1d'
    character(len=char_len) :: log_message
    integer :: varid

    ! 1. Get varid for variable
    call marbl_netcdf_inq_varid(ncid_in, trim(name), varid, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(name), ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! 2. Get value of variable
    call marbl_netcdf_get_var(ncid_in, varid, var, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") "marbl_netcdf_get_var(", trim(name), ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

  end subroutine marbl_io_get_init_file_var_by_name_int_1d

  !*****************************************************************************

  subroutine marbl_io_get_init_file_var_by_name_r8_1d(name, var, driver_status_log)

    character(len=*),     intent(in)    :: name
    real(r8),             intent(out)   :: var(:)
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_get_init_file_var_by_name_r8_1d'
    character(len=char_len) :: log_message
    integer :: varid

    ! 1. Get varid for variable
    call marbl_netcdf_inq_varid(ncid_in, trim(name), varid, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(name), ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! 2. Get value of variable
    call marbl_netcdf_get_var(ncid_in, varid, var, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") "marbl_netcdf_get_var(", trim(name), ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

  end subroutine marbl_io_get_init_file_var_by_name_r8_1d

  !*****************************************************************************

  subroutine marbl_io_distribute_cols(num_cols, col_start, col_cnt, driver_status_log)

    integer,              intent(out)   :: num_cols
    integer,              intent(out)   :: col_start(:)
    integer,              intent(out)   :: col_cnt(:)
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_distribute_cols'
    character(len=char_len) :: log_message
    integer :: n, cols_remaining, num_insts

    ! 1. Get column count from netCDF
    call marbl_io_get_init_file_dim_by_name('column', num_cols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_get_init_file_dim_by_name(column)', subname)
      return
    end if

    ! 2. Determine which columns each instance owns
    !    Note that we use 0-base indexing for
    !    col_start and 1-base indexing for col_id_loc
    !    (col_id_loc in [1, col_cnt(n)]), so
    !        col_id = col_start(n) + col_id_loc
    cols_remaining = num_cols
    num_insts = size(col_cnt)
    do n=1, num_insts
      if (n.eq.1) then
        col_start(n) = 0
      else
        col_start(n) = col_start(n-1) + col_cnt(n-1)
      end if
      col_cnt(n) = cols_remaining / (num_insts-n+1) ! remaining cols / remaining insts
      cols_remaining = cols_remaining - col_cnt(n)
    end do

    !  3. Log decomposition
    do n=1, num_insts
      write(log_message, "(A, I0, A, I0, A, I0)") "Instance ", n, " has ", col_cnt(n),        &
                                                  " columns, beginning with ", col_start(n)+1
      call driver_status_log%log_noerror(log_message, subname)
    end do

  end subroutine marbl_io_distribute_cols

  !*****************************************************************************

  subroutine marbl_io_construct_diag_buffers(num_levels, num_cols, marbl_instance)

    integer,                     intent(in) :: num_levels
    integer,                     intent(in) :: num_cols
    type(marbl_interface_class), intent(in) :: marbl_instance

    call surface_flux_diag_buffer%construct(num_levels, num_cols, &
         marbl_instance%surface_flux_diags)
    call interior_tendency_diag_buffer%construct(num_levels, num_cols, &
         marbl_instance%interior_tendency_diags)

  end subroutine marbl_io_construct_diag_buffers

  !*****************************************************************************

  subroutine marbl_io_read_domain(num_levels, num_PAR_subcols, grid_data, driver_status_log)

    integer,              intent(inout) :: num_levels
    integer,              intent(inout) :: num_PAR_subcols
    type(grid_data_type), intent(inout) :: grid_data
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_domain'

    ! 1. Get dimensions (num_levels and num_PAR_subcols)
    call marbl_io_get_init_file_dim_by_name('zt', num_levels, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_get_init_file_dim_by_name(zt)', subname)
      return
    end if

    call marbl_io_get_init_file_dim_by_name('nbin', num_PAR_subcols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_get_init_file_dim_by_name(nbin)', subname)
      return
    end if

    allocate(grid_data%delta_z(num_levels), grid_data%zt(num_levels), grid_data%zw(num_levels))

    call marbl_io_get_init_file_var_by_name('delta_z', grid_data%delta_z, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_get_init_file_var_by_name(delta_z)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%delta_z = grid_data%delta_z * 100._r8

    call marbl_io_get_init_file_var_by_name('zt', grid_data%zt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_get_init_file_var_by_name(zt)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%zt = grid_data%zt * 100._r8

    call marbl_io_get_init_file_var_by_name('zw', grid_data%zw, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_get_init_file_var_by_name(zw)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%zw = grid_data%zw * 100._r8

  end subroutine marbl_io_read_domain

  !*****************************************************************************

  subroutine marbl_io_read_forcing_field(col_id, col_start, forcing_fields, driver_status_log, active_level_cnt)

    use marbl_interface_public_types, only : marbl_forcing_fields_type

    integer,                                       intent(in)    :: col_id
    integer,                                       intent(in)    :: col_start
    type(marbl_forcing_fields_type), dimension(:), intent(inout) :: forcing_fields
    type(marbl_log_type),                          intent(inout) :: driver_status_log
    integer, optional,                             intent(in)    :: active_level_cnt

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_forcing_field'
    character(len=char_len)     :: log_message
    integer :: n, varid

    do n=1, size(forcing_fields)
      select case(trim(forcing_fields(n)%metadata%varname))
        case('u10_sqr')
          call marbl_netcdf_inq_varid(ncid_in, 'u10_sqr', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from m^2 / s^2 -> cm^2 / s^2
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 10000._r8
        case('sss')
          call marbl_netcdf_inq_varid(ncid_in, 'SSS', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('sst')
          call marbl_netcdf_inq_varid(ncid_in, 'SST', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('Ice Fraction')
          call marbl_netcdf_inq_varid(ncid_in, 'ice_frac', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('Dust Flux')
          call marbl_netcdf_inq_varid(ncid_in, 'dust_flux', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          if (size(forcing_fields(n)%field_0d) .eq. 1) then ! interior forcing
            call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(1), &
                                         driver_status_log, col_id=col_id+col_start)
            ! convert from kg/m^2/s -> g/cm^2/s
            forcing_fields(n)%field_0d(1) = forcing_fields(n)%field_0d(1) * 0.1_r8
          else ! surface forcing
            call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                         driver_status_log, col_id=col_id+col_start)
            ! convert from kg/m^2/s -> g/cm^2/s
            forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 0.1_r8
          end if
        case('Iron Flux')
          call marbl_netcdf_inq_varid(ncid_in, 'iron_flux', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('NOx Flux')
          call marbl_netcdf_inq_varid(ncid_in, 'nox_flux', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('NHy Flux')
          call marbl_netcdf_inq_varid(ncid_in, 'nhy_flux', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('Atmospheric Pressure')
          call marbl_netcdf_inq_varid(ncid_in, 'atm_pressure', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('xco2')
          call marbl_netcdf_inq_varid(ncid_in, 'atm_co2', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('xco2_alt_co2')
          call marbl_netcdf_inq_varid(ncid_in, 'atm_alt_co2', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('PAR Column Fraction')
          call marbl_netcdf_inq_varid(ncid_in, 'FRACR_BIN', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Surface Shortwave')
          call marbl_netcdf_inq_varid(ncid_in, 'QSW_BIN', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Potential Temperature')
          call marbl_netcdf_inq_varid(ncid_in, 'temperature', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Salinity')
          call marbl_netcdf_inq_varid(ncid_in, 'salinity', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Pressure')
          call marbl_netcdf_inq_varid(ncid_in, 'pressure', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Iron Sediment Flux')
          call marbl_netcdf_inq_varid(ncid_in, 'iron_sed_flux', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
          call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('O2 Consumption Scale Factor')
          call marbl_netcdf_inq_varid(ncid_in, 'o2_consumption_scalef', varid, driver_status_log)
          if (driver_status_log%labort_marbl) then
            write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(forcing_fields(n)%metadata%varname), ")"
            call driver_status_log%log_error_trace(log_message, subname)
            return
          end if
         call marbl_netcdf_get_var(ncid_in, varid, forcing_fields(n)%field_1d(1,:), &
                                      driver_status_log, col_start=col_id+col_start)
        case DEFAULT
          write(log_message, "(3A)") "Unrecognized forcing field '", trim(forcing_fields(n)%metadata%varname), "'"
          call driver_status_log%log_error(log_message, subname)
          return
      end select
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_get_var(", trim(forcing_fields(n)%metadata%varname), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      if (present(active_level_cnt)) then
        if (associated(forcing_fields(n)%field_1d)) then
          forcing_fields(n)%field_1d(1,active_level_cnt+1:) = real(0, r8)
        end if
      end if
    end do

  end subroutine marbl_io_read_forcing_field

  !****************************************************************************

  subroutine marbl_io_read_tracers_at_surface(col_start, col_cnt, tracer_metadata, &
                                              tracers_at_surface, driver_status_log)

    integer,                                          intent(in)    :: col_start
    integer,                                          intent(in)    :: col_cnt
    type(marbl_tracer_metadata_type), dimension(:),   intent(in)    :: tracer_metadata
    real(kind=r8),                    dimension(:,:), intent(inout) :: tracers_at_surface ! (num_surface_elem, tracer_cnt)
    type(marbl_log_type),                             intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_tracers_at_surface'
    character(len=char_len) :: log_message
    integer :: n, varid

    do n = 1, size(tracer_metadata)
      call marbl_netcdf_inq_varid(ncid_in, trim(tracer_metadata(n)%short_name), varid, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(tracer_metadata(n)%short_name), " [surface])"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      call marbl_netcdf_get_var(ncid_in, varid, tracers_at_surface(:,n), &
                                   driver_status_log, surf_only=.true., col_start=col_start,             &
                                   col_cnt=col_cnt)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_get_var(", trim(tracer_metadata(n)%short_name), " [surface])"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine marbl_io_read_tracers_at_surface

  !****************************************************************************

  subroutine marbl_io_read_tracers(col_start, tracer_metadata, tracers, driver_status_log)

    integer,                                          intent(in)    :: col_start
    type(marbl_tracer_metadata_type), dimension(:),   intent(in)    :: tracer_metadata
    real(kind=r8),                    dimension(:,:), intent(inout) :: tracers            ! (tracer_cnt, num_levels)
    type(marbl_log_type),                             intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_tracers'
    character(len=char_len) :: log_message
    integer :: n, varid

    do n = 1, size(tracer_metadata)
      call marbl_netcdf_inq_varid(ncid_in, trim(tracer_metadata(n)%short_name), varid, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(tracer_metadata(n)%short_name), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      call marbl_netcdf_get_var(ncid_in, varid, tracers(n,:), driver_status_log, col_start=col_start)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_get_var(", trim(tracer_metadata(n)%short_name), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine marbl_io_read_tracers

  !*****************************************************************************

  subroutine marbl_io_define_history(marbl_instances, col_cnt, driver_status_log)

    use marbl_netcdf_mod, only : marbl_netcdf_def_dim
    use marbl_netcdf_mod, only : marbl_netcdf_enddef

    type(marbl_interface_class), dimension(:), intent(in)    :: marbl_instances
    integer,                     dimension(:), intent(in)    :: col_cnt
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_io_define_history'
    character(len=char_len) :: log_message, varname, long_name, units
    integer :: n
    integer :: num_levels, num_cols, num_tracers
    integer :: dimid_num_levels, dimid_num_cols

    num_tracers = size(marbl_instances(1)%tracer_metadata)

    ! netCDF dimensions
    ! 1) num_levels = number of levels (domain should be the same for all columns)
    num_levels = marbl_instances(1)%domain%km
    call marbl_netcdf_def_dim(ncid_out, 'num_levels', num_levels, driver_status_log, dimid_out=dimid_num_levels)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_def_dim(num_levels)', subname)
      return
    end if

    ! 2) num_cols = number of total columns across all instances
    num_cols = 0
    do n=1, size(marbl_instances)
      ! FIXME: should be num_surface_elem from MARBL, not hard-coded to 1 per instance
      num_cols = num_cols + col_cnt(n)
    end do
    call marbl_netcdf_def_dim(ncid_out, 'num_cols', num_cols, driver_status_log, dimid_out=dimid_num_cols)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_def_dim(num_cols)', subname)
      return
    end if

    ! netCDF variables
    ! 1) Domain variables
    call marbl_netcdf_def_var(ncid_out, 'zt', 'double', (/dimid_num_levels/), &
                              "cell center depth", "m", driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_def_var(zt)', subname)
      return
    end if

    call marbl_netcdf_def_var(ncid_out, 'zw', 'double', (/dimid_num_levels/), &
                              "cell interface depth", "m", driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_def_var(zw)', subname)
      return
    end if

    ! ! 2) Surface diagnostics
    call def_marbl_diag_in_ncid_out(marbl_instances(1)%surface_flux_diags, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('def_marbl_diag_in_ncid_out(surface_flux)', subname)
      return
    end if

    ! 3) Interior diagnostics
    call def_marbl_diag_in_ncid_out(marbl_instances(1)%interior_tendency_diags, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('def_marbl_diag_in_ncid_out(interior_tendency)', subname)
      return
    end if

    do n=1, num_tracers
      ! NOTE: we use log_message as a temporary buffer for strings written as netCDF metadata

      ! Surface fluxes
      call get_surface_flux_desc_from_metadata(marbl_instances(1)%tracer_metadata(n), varname, long_name, units)
      call marbl_netcdf_def_var(ncid_out, varname, 'double', (/dimid_num_cols/), long_name, units, &
                                driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_def_var(', trim(varname), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      ! Interior tendencies
      call get_interior_tendency_desc_from_metadata(marbl_instances(1)%tracer_metadata(n), varname, long_name, units)
      call marbl_netcdf_def_var(ncid_out, varname, 'double', (/dimid_num_levels, dimid_num_cols/), &
                                long_name, units, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_def_var(', varname, ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      ! Initial values
      varname = marbl_instances(1)%tracer_metadata(n)%short_name
      write(long_name, "(2A)") "Initial value of ", trim(marbl_instances(1)%tracer_metadata(n)%long_name)
      units = trim(marbl_instances(1)%tracer_metadata(n)%units)
      call marbl_netcdf_def_var(ncid_out, varname, 'double', (/dimid_num_levels, dimid_num_cols/), &
                                long_name, units, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_def_var(', varname, ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

    ! Exit define mode
    call marbl_netcdf_enddef(ncid_out, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A)") 'marbl_netcdf_enddef(hist_file)'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

  end subroutine marbl_io_define_history

  !*****************************************************************************

  subroutine marbl_io_copy_into_surface_diag_buffer(col_start, col_cnt, marbl_instance)
    integer,                     intent(in) :: col_start
    integer,                     intent(in) :: col_cnt
    type(marbl_interface_class), intent(in) :: marbl_instance

    integer :: m

    do m=1, surface_flux_diag_buffer%num_diags
      if (allocated(surface_flux_diag_buffer%diags(m)%field_2d)) then
        surface_flux_diag_buffer%diags(m)%field_2d((col_start+1):(col_start+col_cnt)) = &
            marbl_instance%surface_flux_diags%diags(m)%field_2d(:)
        surface_flux_diag_buffer%diags(m)%ref_depth_2d = &
            marbl_instance%surface_flux_diags%diags(m)%ref_depth * 100._r8 ! m -> cm
      else
        surface_flux_diag_buffer%diags(m)%field_3d(:,(col_start+1):(col_start+col_cnt)) = &
            marbl_instance%surface_flux_diags%diags(m)%field_3d(:,:)
      end if
    end do

  end subroutine marbl_io_copy_into_surface_diag_buffer

  !*****************************************************************************

  subroutine marbl_io_copy_into_interior_diag_buffer(col_id, marbl_instance)
    integer,                     intent(in) :: col_id
    type(marbl_interface_class), intent(in) :: marbl_instance

    integer :: m

    do m=1, interior_tendency_diag_buffer%num_diags
      if (allocated(interior_tendency_diag_buffer%diags(m)%field_2d)) then
        interior_tendency_diag_buffer%diags(m)%field_2d(col_id) = &
            marbl_instance%interior_tendency_diags%diags(m)%field_2d(1)
        interior_tendency_diag_buffer%diags(m)%ref_depth_2d = &
            marbl_instance%interior_tendency_diags%diags(m)%ref_depth * 100._r8 ! m -> cm
      else
        interior_tendency_diag_buffer%diags(m)%field_3d(:,col_id) = &
            marbl_instance%interior_tendency_diags%diags(m)%field_3d(:,1)
      end if
    end do

  end subroutine marbl_io_copy_into_interior_diag_buffer

  !*****************************************************************************

  subroutine marbl_io_write_history(marbl_instance, surface_fluxes, interior_tendencies, &
                                    tracer_initial_vals, active_level_cnt, driver_status_log)

    type(marbl_interface_class),                   intent(in)    :: marbl_instance
    real(r8),                    dimension(:,:),   intent(in)    :: surface_fluxes       ! num_cols x num_tracers
    real(r8),                    dimension(:,:,:), intent(in)    :: interior_tendencies  ! num_tracers x num_levels x num_cols
    real(r8),                    dimension(:,:,:), intent(in)    :: tracer_initial_vals  ! num_tracers x num_levels x num_cols
    integer,                     dimension(:),     intent(in)    :: active_level_cnt
    type(marbl_log_type),                          intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_io_write_history'
    character(len=char_len) :: log_message
    character(len=char_len) :: varname
    integer :: col_id, varid, n
    real(r8), dimension(size(active_level_cnt)) :: bot_depth

    ! 1) Domain variables
    call marbl_netcdf_inq_varid(ncid_out, 'zt', varid, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_inq_varid(zt)', subname)
      return
    end if
    call marbl_netcdf_put_var(ncid_out, varid, marbl_instance%domain%zt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_put_var(zt)', subname)
      return
    end if

    call marbl_netcdf_inq_varid(ncid_out, 'zw', varid, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_inq_varid(zw)', subname)
      return
    end if
    call marbl_netcdf_put_var(ncid_out, varid, marbl_instance%domain%zw, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_put_var(zw)', subname)
      return
    end if

    do col_id=1, size(bot_depth)
      bot_depth(col_id) = marbl_instance%domain%zw(active_level_cnt(col_id))
    end do
    ! 2) Surface and Interior diagnostics
    call write_diag_buffer_to_nc(surface_flux_diag_buffer, active_level_cnt, driver_status_log)
    ! FIXME #176: changing active_level_cnt to num_levels (km instead of kmt) will populate levels below
    !             active_level_cnt with nonsensical values
    call write_diag_buffer_to_nc(interior_tendency_diag_buffer, active_level_cnt, &
                                 driver_status_log, bot_depth=bot_depth)

    ! 4) Tracer surface fluxes, tendencies, and initial conditions
    do n=1, size(marbl_instance%tracer_metadata)
      ! Surface fluxes
      call get_surface_flux_desc_from_metadata(marbl_instance%tracer_metadata(n), varname)
      call marbl_netcdf_inq_varid(ncid_out, varname, varid, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_inq_varid(', trim(varname), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      call marbl_netcdf_put_var(ncid_out, varid, surface_fluxes(:,n), driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_put_var(', trim(marbl_instance%tracer_metadata(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      ! Interior tendencies
      call get_interior_tendency_desc_from_metadata(marbl_instance%tracer_metadata(n), varname)
      call marbl_netcdf_inq_varid(ncid_out, varname, varid, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_inq_varid(', trim(varname), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      call marbl_netcdf_put_var(ncid_out, varid, interior_tendencies(n, :, :), &
                                active_level_cnt, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_put_var(J_', trim(marbl_instance%tracer_metadata(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      ! Initial values
      varname = marbl_instance%tracer_metadata(n)%short_name
      call marbl_netcdf_inq_varid(ncid_out, varname, varid, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_inq_varid(', trim(varname), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      call marbl_netcdf_put_var(ncid_out, varid, tracer_initial_vals(n, :, :), active_level_cnt, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_put_var(', trim(varname), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

    end do

  end subroutine marbl_io_write_history

  !*****************************************************************************

  subroutine marbl_io_close_files(driver_status_log)

    use marbl_netcdf_mod, only : marbl_netcdf_close

    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname='marbl_io_mod:marbl_io_close_files'

    call marbl_netcdf_close(ncid_in, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_close(init_file)', subname)
      return
    end if

    call marbl_netcdf_close(ncid_out, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_close(hist_file)', subname)
      return
    end if

  end subroutine marbl_io_close_files

  !*****************************************************************************

  subroutine marbl_io_destruct_diag_buffers()

    call surface_flux_diag_buffer%destruct()
    call interior_tendency_diag_buffer%destruct()

  end subroutine marbl_io_destruct_diag_buffers

  !*****************************************************************************

  subroutine def_marbl_diag_in_ncid_out(diag, driver_status_log)

    use marbl_interface_public_types , only : marbl_diagnostics_type

    type(marbl_diagnostics_type), intent(in)    :: diag
    type(marbl_log_type),         intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:def_marbl_diag_in_ncid_out'
    character(len=char_len) :: log_message, varname, long_name, units
    integer :: n
    integer :: dimids_all(2)
    integer, allocatable :: dimids(:)

    ! Set up dimids_all
    call marbl_netcdf_inq_dimid(ncid_out, 'num_levels', dimids_all(1), driver_status_log)
    if (driver_status_log%labort_marbl) then
      log_message = 'marbl_netcdf_inq_dimid(num_levels)'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
    call marbl_netcdf_inq_dimid(ncid_out, 'num_cols', dimids_all(2), driver_status_log)
    if (driver_status_log%labort_marbl) then
      log_message = 'marbl_netcdf_inq_dimid(num_cols)'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    do n=1, size(diag%diags)
      ! Define netCDF variable with appropriate units and dimensions
      varname = trim(diag%diags(n)%short_name)
      long_name = trim(diag%diags(n)%long_name)
      units = trim(diag%diags(n)%units)
      select case (trim(diag%diags(n)%vertical_grid))
        case ('none')
          allocate(dimids(1))
          dimids(1) = dimids_all(2)
        case ('layer_avg')
          allocate(dimids(2))
          dimids = dimids_all
        case DEFAULT
          write(log_message, '(3A)') "'", trim(diag%diags(n)%vertical_grid), &
                "' is not a valid vertical grid"
          call driver_status_log%log_error(log_message, subname)
          return
      end select
      call marbl_netcdf_def_var(ncid_out, varname, 'double', dimids, long_name, units, &
                                driver_status_log, ldef_fillval=.true.)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_def_var(', trim(varname), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      deallocate(dimids)
    end do

  end subroutine def_marbl_diag_in_ncid_out

  !*****************************************************************************

  subroutine get_surface_flux_desc_from_metadata(metadata, varname, long_name, units)

    type(marbl_tracer_metadata_type), intent(in)  :: metadata
    character(len=*),                 intent(out) :: varname
    character(len=*), optional,       intent(out) :: long_name
    character(len=*), optional,       intent(out) :: units

    write(varname, "(2A)") "STF_", trim(metadata%short_name)
    if (present(long_name)) &
      write(long_name, "(2A)") "Surface flux of ", trim(metadata%long_name)
    if (present(units)) &
      write(units, "(2A)") trim(metadata%units), ' cm/s'

  end subroutine get_surface_flux_desc_from_metadata

  !*****************************************************************************

  subroutine get_interior_tendency_desc_from_metadata(metadata, varname, long_name, units)

    type(marbl_tracer_metadata_type), intent(in)  :: metadata
    character(len=*),                 intent(out) :: varname
    character(len=*), optional,       intent(out) :: long_name
    character(len=*), optional,       intent(out) :: units

    write(varname, "(2A)") "J_", trim(metadata%short_name)
    if (present(long_name)) &
      write(long_name, "(2A)") trim(metadata%long_name), " Tendency"
    if (present(units)) &
      write(units, "(2A)") trim(metadata%units), '/s'

  end subroutine get_interior_tendency_desc_from_metadata

  !*****************************************************************************

  subroutine write_diag_buffer_to_nc(diag_buffer, active_level_cnt, driver_status_log, bot_depth)

    type(many_diags_type), intent(in)    :: diag_buffer
    integer,               intent(in)    :: active_level_cnt(:)
    type(marbl_log_type),  intent(inout) :: driver_status_log
    real(r8), optional,    intent(in)    :: bot_depth(:)

    character(len=*), parameter :: subname = 'marbl_io_mod:write_diag_buffer_to_nc'
    character(len=char_len) :: log_message
    integer :: n, varid

    do n=1, diag_buffer%num_diags
      call marbl_netcdf_inq_varid(ncid_out, trim(diag_buffer%diags(n)%short_name), varid, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_inq_varid(", trim(diag_buffer%diags(n)%short_name), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      if (allocated(diag_buffer%diags(n)%field_2d)) then
        if (present(bot_depth)) then
          call marbl_netcdf_put_var(ncid_out, varid, diag_buffer%diags(n)%field_2d(:), driver_status_log, &
                                    mask_in=(bot_depth .ge. diag_buffer%diags(n)%ref_depth_2d))
        else
          call marbl_netcdf_put_var(ncid_out, varid, diag_buffer%diags(n)%field_2d(:), driver_status_log)
        end if
      else if (allocated(diag_buffer%diags(n)%field_3d)) then
        call marbl_netcdf_put_var(ncid_out, varid, diag_buffer%diags(n)%field_3d(:,:), active_level_cnt, driver_status_log)
      end if
      if (driver_status_log%labort_MARBL) then
        write(log_message, "(A,I0,A)") "marbl_netcdf_put_var(buffer index", n, ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine write_diag_buffer_to_nc

  !*****************************************************************************

  subroutine io_diag_construct(self, num_levels, num_cols, diags_in)

    use marbl_interface_public_types , only : marbl_diagnostics_type

    class(many_diags_type),       intent(inout) :: self
    integer,                      intent(in)    :: num_levels
    integer,                      intent(in)    :: num_cols
    type(marbl_diagnostics_type), intent(in)    :: diags_in

    integer :: n

    self%num_diags = size(diags_in%diags)
    allocate(self%diags(self%num_diags))
    do n=1, self%num_diags
      self%diags(n)%short_name = diags_in%diags(n)%short_name
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
    deallocate(self%diags)

  end subroutine io_diag_destruct

  !*****************************************************************************

end module marbl_io_mod
