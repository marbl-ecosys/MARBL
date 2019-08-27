module marbl_io_mod
! This module provides MARBL-specific routines that call marbl_netcdf_mod to perform
! I/O. It is designed to build regardless of whether the model is built with netCDF support,
! but will report errors from marbl_netcdf_mod if netCDF is not linked when required.

  use marbl_kinds_mod,  only : r8, char_len
  use marbl_logging,    only : marbl_log_type
  use marbl_netcdf_mod, only : marbl_netcdf_read_var

  implicit none
  private
  save

  type, public :: grid_data_type
    real(kind=r8), allocatable, dimension(:) :: delta_z, zt, zw
  end type grid_data_type

  type, public :: netcdf_prognostic_ids
    integer, allocatable, dimension(:) :: sflux_ids
    integer, allocatable, dimension(:) :: tendency_ids
    integer, allocatable, dimension(:) :: init_ids
  end type netcdf_prognostic_ids
  type(netcdf_prognostic_ids) :: prog_ids_out

  ! varids used for writing variables in MARBL diagnostic type to netCDF
  integer, allocatable, dimension(:) :: surface_diag_varids, interior_diag_varids

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

  public :: marbl_io_read_domain
  public :: marbl_io_read_forcing_field
  public :: marbl_io_read_tracers_at_surface
  public :: marbl_io_read_tracers
  public :: marbl_io_define_history
  public :: marbl_io_write_history

contains

  !*****************************************************************************

  subroutine marbl_io_read_domain(infile, num_levels, num_PAR_subcols, grid_data, driver_status_log)

    use marbl_netcdf_mod, only : marbl_netcdf_read_dim

    character(len=*),     intent(in)    :: infile
    integer,              intent(inout) :: num_levels
    integer,              intent(inout) :: num_PAR_subcols
    type(grid_data_type), intent(inout) :: grid_data
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_domain'

    call marbl_netcdf_read_dim(infile, 'zt', num_levels, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_dim(zt)', subname)
      return
    end if

    call marbl_netcdf_read_dim(infile, 'nbin', num_PAR_subcols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_dim(nbin)', subname)
      return
    end if

    allocate(grid_data%delta_z(num_levels), grid_data%zt(num_levels), grid_data%zw(num_levels))

    call marbl_netcdf_read_var(infile, 'delta_z', grid_data%delta_z, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_var(delta_z)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%delta_z = grid_data%delta_z * 100._r8

    call marbl_netcdf_read_var(infile, 'zt', grid_data%zt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_var(zt)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%zt = grid_data%zt * 100._r8

    call marbl_netcdf_read_var(infile, 'zw', grid_data%zw, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_var(zw)', subname)
      return
    end if
    ! convert from m -> cm
    grid_data%zw = grid_data%zw * 100._r8

  end subroutine marbl_io_read_domain

  !*****************************************************************************

  subroutine marbl_io_read_forcing_field(infile, col_id, col_start, forcing_fields, driver_status_log, num_active_levels)

    use marbl_interface_public_types, only : marbl_forcing_fields_type

    character(len=*),                              intent(in)    :: infile
    integer,                                       intent(in)    :: col_id
    integer,                                       intent(in)    :: col_start
    type(marbl_forcing_fields_type), dimension(:), intent(inout) :: forcing_fields
    type(marbl_log_type),                          intent(inout) :: driver_status_log
    integer, optional,                             intent(in)    :: num_active_levels

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_forcing_field'
    character(len=char_len)     :: log_message
    integer :: n

    do n=1, size(forcing_fields)
      select case(trim(forcing_fields(n)%metadata%varname))
        case('u10_sqr')
          call marbl_netcdf_read_var(infile, 'u10_sqr', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from m^2 / s^2 -> cm^2 / s^2
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 10000._r8
        case('sss')
          call marbl_netcdf_read_var(infile, 'SSS', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('sst')
          call marbl_netcdf_read_var(infile, 'SST', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('Ice Fraction')
          call marbl_netcdf_read_var(infile, 'ice_frac', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('Dust Flux')
          if (size(forcing_fields(n)%field_0d) .eq. 1) then ! interior forcing
            call marbl_netcdf_read_var(infile, 'dust_flux', forcing_fields(n)%field_0d(1), &
                                         driver_status_log, col_id=col_id+col_start)
            ! convert from kg/m^2/s -> g/cm^2/s
            forcing_fields(n)%field_0d(1) = forcing_fields(n)%field_0d(1) * 0.1_r8
          else ! surface forcing
            call marbl_netcdf_read_var(infile, 'dust_flux', forcing_fields(n)%field_0d(col_id), &
                                         driver_status_log, col_id=col_id+col_start)
            ! convert from kg/m^2/s -> g/cm^2/s
            forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 0.1_r8
          end if
        case('Iron Flux')
          call marbl_netcdf_read_var(infile, 'iron_flux', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('NOx Flux')
          call marbl_netcdf_read_var(infile, 'nox_flux', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('NHy Flux')
          call marbl_netcdf_read_var(infile, 'nhy_flux', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
          ! convert from mmol/m^2/s -> nmol/cm^2/s
          forcing_fields(n)%field_0d(col_id) = forcing_fields(n)%field_0d(col_id) * 100._r8
        case('Atmospheric Pressure')
          call marbl_netcdf_read_var(infile, 'atm_pressure', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('xco2')
          call marbl_netcdf_read_var(infile, 'atm_co2', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('xco2_alt_co2')
          call marbl_netcdf_read_var(infile, 'atm_alt_co2', forcing_fields(n)%field_0d(col_id), &
                                       driver_status_log, col_id=col_id+col_start)
        case('PAR Column Fraction')
          call marbl_netcdf_read_var(infile, 'FRACR_BIN', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Surface Shortwave')
          call marbl_netcdf_read_var(infile, 'QSW_BIN', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Potential Temperature')
          call marbl_netcdf_read_var(infile, 'temperature', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Salinity')
          call marbl_netcdf_read_var(infile, 'salinity', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Pressure')
          call marbl_netcdf_read_var(infile, 'pressure', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('Iron Sediment Flux')
          call marbl_netcdf_read_var(infile, 'iron_sed_flux', forcing_fields(n)%field_1d(1,:), &
                                       driver_status_log, col_start=col_id+col_start)
        case('O2 Consumption Scale Factor')
         call marbl_netcdf_read_var(infile, 'o2_consumption_scalef', forcing_fields(n)%field_1d(1,:), &
                                      driver_status_log, col_start=col_id+col_start)
        case DEFAULT
          write(log_message, "(3A)") "Unrecognized forcing field '", trim(forcing_fields(n)%metadata%varname), "'"
          call driver_status_log%log_error(log_message, subname)
          return
      end select
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_read_var(", trim(forcing_fields(n)%metadata%varname), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      if (present(num_active_levels)) then
        if (associated(forcing_fields(n)%field_1d)) then
          forcing_fields(n)%field_1d(1,num_active_levels+1:) = real(0, r8)
        end if
      end if
    end do

  end subroutine marbl_io_read_forcing_field

  !****************************************************************************

  subroutine marbl_io_read_tracers_at_surface(infile, col_start, col_cnt, tracer_metadata, &
                                              tracers_at_surface, driver_status_log)

    use marbl_interface_public_types, only : marbl_tracer_metadata_type

    character(len=*),                                 intent(in)    :: infile
    integer,                                          intent(in)    :: col_start
    integer,                                          intent(in)    :: col_cnt
    type(marbl_tracer_metadata_type), dimension(:),   intent(in)    :: tracer_metadata
    real(kind=r8),                    dimension(:,:), intent(inout) :: tracers_at_surface ! (num_surface_elem, tracer_cnt)
    type(marbl_log_type),                             intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_tracers_at_surface'
    character(len=char_len) :: log_message
    integer :: n

    do n = 1, size(tracer_metadata)
      call marbl_netcdf_read_var(infile, trim(tracer_metadata(n)%short_name), tracers_at_surface(:,n), &
                                   driver_status_log, surf_only=.true., col_start=col_start,             &
                                   col_cnt=col_cnt)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_read_var(", trim(tracer_metadata(n)%short_name), " [surface])"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine marbl_io_read_tracers_at_surface

  !****************************************************************************

  subroutine marbl_io_read_tracers(infile, col_start, tracer_metadata, tracers, driver_status_log)

    use marbl_interface_public_types, only : marbl_tracer_metadata_type

    character(len=*),                                 intent(in)    :: infile
    integer,                                          intent(in)    :: col_start
    type(marbl_tracer_metadata_type), dimension(:),   intent(in)    :: tracer_metadata
    real(kind=r8),                    dimension(:,:), intent(inout) :: tracers            ! (tracer_cnt, num_levels)
    type(marbl_log_type),                             intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:marbl_io_read_tracers'
    character(len=char_len) :: log_message
    integer :: n

    do n = 1, size(tracer_metadata)
      call marbl_netcdf_read_var(infile, trim(tracer_metadata(n)%short_name), tracers(n,:), &
                                   driver_status_log, col_start=col_start)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_netcdf_read_var(", trim(tracer_metadata(n)%short_name), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine marbl_io_read_tracers

  !*****************************************************************************

  subroutine marbl_io_define_history(marbl_instances, col_cnt, hist_file, driver_status_log)

    use marbl_interface, only : marbl_interface_class

    use marbl_netcdf_mod, only : marbl_netcdf_def_dim
    use marbl_netcdf_mod, only : marbl_netcdf_def_var
    use marbl_netcdf_mod, only : marbl_netcdf_enddef

    type(marbl_interface_class), dimension(:), intent(in)    :: marbl_instances
    integer,                     dimension(:), intent(in)    :: col_cnt
    character(len=*),                          intent(in)    :: hist_file
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_io_define_history'
    character(len=char_len) :: log_message, varname, long_name, units
    integer :: n
    integer :: num_levels, num_cols, num_tracers

    num_tracers = size(marbl_instances(1)%tracer_metadata)

    ! netCDF dimensions
    ! 1) num_levels = number of levels (domain should be the same for all columns)
    num_levels = marbl_instances(1)%domain%km
    call marbl_netcdf_def_dim(hist_file, 'num_levels', num_levels, driver_status_log)
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
    call marbl_netcdf_def_dim(hist_file, 'num_cols', num_cols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_def_dim(num_cols)', subname)
      return
    end if

    ! netCDF variables
    ! 1) Domain variables
    call marbl_netcdf_def_var(hist_file, 'zt', 'double', (/'num_levels'/), "cell center depth", "m", driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_def_var(zt)', subname)
      return
    end if

    call marbl_netcdf_def_var(hist_file, 'zw', 'double', (/'num_levels'/), "cell interface depth", "m", driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_def_var(zw)', subname)
      return
    end if

    ! 2) Surface diagnostics
    call def_marbl_diag_in_nc(hist_file, marbl_instances(1)%surface_flux_diags, surface_diag_varids, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('def_marbl_diag_in_nc(surface_flux)', subname)
      return
    end if

    ! 3) Interior diagnostics
    call def_marbl_diag_in_nc(hist_file, marbl_instances(1)%interior_tendency_diags, interior_diag_varids, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('def_marbl_diag_in_nc(interior_tendency)', subname)
      return
    end if

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
      write(varname, "(2A)") "STF_", trim(marbl_instances(1)%tracer_metadata(n)%short_name)
      write(long_name, "(2A)") "Surface flux of ", trim(marbl_instances(1)%tracer_metadata(n)%long_name)
      write(units, "(2A)") trim(marbl_instances(1)%tracer_metadata(n)%units), ' cm/s'
      call marbl_netcdf_def_var(hist_file, varname, 'double', (/'num_cols'/), long_name, units, &
                                driver_status_log, varid_out=prog_ids_out%sflux_ids(n))
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_def_var(', varname, ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      ! Interior tendencies
      write(varname, "(2A)") "J_", trim(marbl_instances(1)%tracer_metadata(n)%short_name)
      write(long_name, "(2A)") trim(marbl_instances(1)%tracer_metadata(n)%long_name), " Tendency"
      write(units, "(2A)") trim(marbl_instances(1)%tracer_metadata(n)%units), '/s'
      call marbl_netcdf_def_var(hist_file, varname, 'double', (/'num_levels', 'num_cols  '/), &
                                long_name, units, driver_status_log, varid_out=prog_ids_out%tendency_ids(n))
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_def_var(', varname, ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      ! Initial values
      varname = marbl_instances(1)%tracer_metadata(n)%short_name
      write(long_name, "(2A)") "Initial value of ", trim(marbl_instances(1)%tracer_metadata(n)%long_name)
      units = trim(marbl_instances(1)%tracer_metadata(n)%units)
      call marbl_netcdf_def_var(hist_file, varname, 'double', (/'num_levels', 'num_cols  '/), &
                                long_name, units, driver_status_log, varid_out=prog_ids_out%init_ids(n))
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_def_var(', varname, ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

    ! Exit define mode
    call marbl_netcdf_enddef(hist_file, driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'marbl_netcdf_enddef(', hist_file, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

  end subroutine marbl_io_define_history

  !*****************************************************************************

  subroutine marbl_io_write_history(hist_file, marbl_instance, surface_fluxes, interior_tendencies, &
                                    tracer_initial_vals, num_active_levels, driver_status_log)

    use marbl_interface, only : marbl_interface_class

    use marbl_netcdf_mod, only : marbl_netcdf_put_var

    character(len=*),                              intent(in)    :: hist_file
    type(marbl_interface_class),                   intent(in)    :: marbl_instance
    real(r8),                    dimension(:,:),   intent(in)    :: surface_fluxes       ! num_cols x num_tracers
    real(r8),                    dimension(:,:,:), intent(in)    :: interior_tendencies  ! num_tracers x num_levels x num_cols
    real(r8),                    dimension(:,:,:), intent(in)    :: tracer_initial_vals  ! num_tracers x num_levels x num_cols
    integer,                     dimension(:),     intent(in)    :: num_active_levels
    type(marbl_log_type),                          intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_io_write_history'
    character(len=char_len) :: log_message
    integer :: n, col_id
    real(r8), dimension(size(num_active_levels)) :: bot_depth

    ! 1) Domain variables
    call marbl_netcdf_put_var(hist_file, 'zt', marbl_instance%domain%zt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_put_var(zt)', subname)
      return
    end if

    call marbl_netcdf_put_var(hist_file, 'zw', marbl_instance%domain%zw, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_put_var(zw)', subname)
      return
    end if

    do col_id=1, size(bot_depth)
      bot_depth(col_id) = marbl_instance%domain%zw(num_active_levels(col_id))
    end do
    ! 2) Surface and Interior diagnostics
    call write_diag_buffer_to_nc(hist_file, surface_diag_varids, surface_flux_diag_buffer, &
                                 num_active_levels, driver_status_log)
    ! FIXME #176: changing num_active_levels to num_levels (km instead of kmt) will populate levels below
    !             num_active_levels with nonsensical values
    call write_diag_buffer_to_nc(hist_file, interior_diag_varids, interior_tendency_diag_buffer, &
                                 num_active_levels, driver_status_log, bot_depth=bot_depth)

    ! 4) Tracer surface fluxes, tendencies, and initial conditions
    ! Surface fluxes
    do n=1, size(marbl_instance%tracer_metadata)
      call marbl_netcdf_put_var(hist_file, prog_ids_out%sflux_ids(n), surface_fluxes(:,n), driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_put_var(', trim(marbl_instance%tracer_metadata(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      ! Interior tendencies
      call marbl_netcdf_put_var(hist_file, prog_ids_out%tendency_ids(n), interior_tendencies(n, :, :), &
                                num_active_levels, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_put_var(J_', trim(marbl_instance%tracer_metadata(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

      ! Initial values
      call marbl_netcdf_put_var(hist_file, prog_ids_out%init_ids(n), tracer_initial_vals(n, :, :), &
                                num_active_levels, driver_status_log)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_put_var(', trim(marbl_instance%tracer_metadata(n)%short_name), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if

    end do

  end subroutine marbl_io_write_history

  !*****************************************************************************

  subroutine def_marbl_diag_in_nc(file_name, diag, varids, driver_status_log)

    use marbl_interface_public_types , only : marbl_diagnostics_type
    use marbl_netcdf_mod, only : marbl_netcdf_def_var

    character(len=*),             intent(in)    :: file_name
    type(marbl_diagnostics_type), intent(in)    :: diag
    integer, allocatable,         intent(inout) :: varids(:)
    type(marbl_log_type),         intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_io_mod:def_marbl_diag_in_nc'
    character(len=char_len) :: log_message, varname, long_name, units
    integer :: n
    character(len=char_len), allocatable :: dimnames(:)

    ! Set up array of varids
    allocate(varids(size(diag%diags)))

    do n=1, size(diag%diags)
      ! Define netCDF variable with appropriate units and dimensions
      varname = trim(diag%diags(n)%short_name)
      long_name = trim(diag%diags(n)%long_name)
      units = trim(diag%diags(n)%units)
      select case (trim(diag%diags(n)%vertical_grid))
        case ('none')
          allocate(dimnames(1))
          dimnames(1) = 'num_cols'
        case ('layer_avg')
          allocate(dimnames(2))
          dimnames(1) = 'num_levels'
          dimnames(2) = 'num_cols'
        case DEFAULT
          write(log_message, '(3A)') "'", trim(diag%diags(n)%vertical_grid), &
                "' is not a valid vertical grid"
          call driver_status_log%log_error(log_message, subname)
          return
      end select
      call marbl_netcdf_def_var(file_name, varname, 'double', dimnames, long_name, units, &
                                driver_status_log, ldef_fillval=.true., varid_out=varids(n))
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") 'marbl_netcdf_def_var(', trim(varname), ')'
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
      deallocate(dimnames)
    end do

  end subroutine def_marbl_diag_in_nc

  !*****************************************************************************

  subroutine write_diag_buffer_to_nc(file_name, varids, diag_buffer, num_active_levels, driver_status_log, bot_depth)

    use marbl_netcdf_mod, only : marbl_netcdf_put_var

    character(len=*),      intent(in)    :: file_name
    integer,               intent(in)    :: varids(:)
    type(many_diags_type), intent(in)    :: diag_buffer
    integer,               intent(in)    :: num_active_levels(:)
    type(marbl_log_type),  intent(inout) :: driver_status_log
    real(r8), optional,    intent(in)    :: bot_depth(:)

    character(len=*), parameter :: subname = 'marbl_io_mod:write_diag_buffer_to_nc'
    character(len=char_len) :: log_message
    integer :: n

    do n=1, diag_buffer%num_diags
      if (allocated(diag_buffer%diags(n)%field_2d)) then
        if (present(bot_depth)) then
          call marbl_netcdf_put_var(file_name, varids(n), diag_buffer%diags(n)%field_2d(:), driver_status_log, &
                                    mask_in=(bot_depth .ge. diag_buffer%diags(n)%ref_depth_2d))
        else
          call marbl_netcdf_put_var(file_name, varids(n), diag_buffer%diags(n)%field_2d(:), driver_status_log)
        end if
      else if (allocated(diag_buffer%diags(n)%field_3d)) then
        call marbl_netcdf_put_var(file_name, varids(n), diag_buffer%diags(n)%field_3d(:,:), num_active_levels, driver_status_log)
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

end module marbl_io_mod
