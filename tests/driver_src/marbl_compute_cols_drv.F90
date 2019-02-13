module marbl_compute_cols_drv

  use marbl_interface, only : marbl_interface_class
  use marbl_kinds_mod, only : r8, char_len
  use marbl_logging,   only : marbl_log_type

  implicit none
  private
  save

  type grid_data_type
    real(kind=r8), allocatable, dimension(:) :: delta_z, zt, zw
  end type grid_data_type

  Public :: test

Contains

  !****************************************************************************

  subroutine test(marbl_instances, num_PAR_subcols, driver_status_log)

    use marbl_io_mod,  only : marbl_io_read_field
    use marbl_io_mod,  only : marbl_io_define_history
    use marbl_io_mod,  only : marbl_io_write_history
    use marbl_io_mod,  only : marbl_io_close_all

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances
    integer,                                   intent(in)    :: num_PAR_subcols
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_compute_cols_drv:test'
    character(len=*), parameter :: infile = 'marbl.nc'
    character(len=*), parameter :: outfile = 'history.nc'
    integer :: num_levels, m, n, col_id_loc, col_id
    type(grid_data_type) :: grid_data
    integer, allocatable, dimension(:) :: num_active_levels, col_start, col_cnt


    ! 1. Initialize the test (reads grid info, sets up history files, etc)
    call initialize(infile, outfile, size(marbl_instances), num_levels, num_active_levels, col_start, col_cnt, &
                    grid_data, driver_status_log)
    if (driver_status_log%labort_MARBL) then
      call driver_status_log%log_error_trace('initialize', subname)
      return
    end if


    ! 2. Initialize each instance of MARBL
    do n=1, size(marbl_instances)
      call marbl_instances(n)%init(gcm_num_levels = num_levels,           &
                                   gcm_num_PAR_subcols = num_PAR_subcols, &
                                   gcm_num_elements_surface_flux = 1,     &
                                   gcm_delta_z = grid_data%delta_z,       &
                                   gcm_zw = grid_data%zw,                 &
                                   gcm_zt = grid_data%zt)
    end do


    ! 3. Define diagnostic fields in output netCDF file
    call marbl_io_define_history(marbl_instances, col_cnt, outfile, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_define_history', subname)
      return
    end if


    ! Brunt of MARBL computations
    do n=1, size(marbl_instances)
      ! 4. Call surface_flux_compute() (all columns simultaneously)
      !    4a. populate surface tracer values
      call read_tracers_at_surface(infile, col_start(n)+1, marbl_instances(n)%tracer_metadata, &
                                   marbl_instances(n)%tracers_at_surface, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_tracers_at_surface', subname)
        return
      end if
      !    4b. populate surface_flux_forcings
      call read_forcing_field(infile, col_start(n)+1, marbl_instances(n)%surface_flux_forcings, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_forcing_field(surface)', subname)
        return
      end if
      !    4c. populate saved_state
      do m=1, size(marbl_instances(n)%surface_flux_saved_state%state)
        marbl_instances(n)%surface_flux_saved_state%state(m)%field_2d(:) = 0._r8
      end do
      !    4d. call surface_flux_compute()
      call marbl_instances(n)%surface_flux_compute()
      if (marbl_instances(n)%StatusLog%labort_MARBL) then
        call marbl_instances(n)%StatusLog%log_error_trace('surface_flux_compute', subname)
        return
      end if
      !    4e. write to history file?


      ! 5. Call interior_tendency_compute() (one column at a time)
      do col_id_loc = 1, col_cnt(n)
        col_id = col_start(n)+col_id_loc
        call marbl_io_read_field(infile, 'active_level_cnt', marbl_instances(n)%domain%kmt, driver_status_log, col_id=col_id)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('marbl_io_read_field(active_level_cnt)', subname)
          return
        end if
        num_active_levels(col_id) = marbl_instances(n)%domain%kmt

        !  5a. populate tracer values
        call read_tracers(infile, col_id, marbl_instances(n)%tracer_metadata, marbl_instances(n)%tracers, &
                          driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('read_tracers', subname)
          return
        end if

        !  5b. populate interior_tendency_forcings
        call read_forcing_field(infile, col_id, marbl_instances(n)%interior_tendency_forcings, driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('read_forcing_field(interior)', subname)
          return
        end if

        !  5c. populate saved_state
        do m=1, size(marbl_instances(n)%interior_tendency_saved_state%state)
          if (allocated(marbl_instances(n)%interior_tendency_saved_state%state(m)%field_2d)) then
            marbl_instances(n)%interior_tendency_saved_state%state(m)%field_2d(:) = 0._r8
          else
            marbl_instances(n)%interior_tendency_saved_state%state(m)%field_3d(:,1) = 0._r8
          end if
        end do

        !  5d. call interior_tendency_compute()
        call marbl_instances(n)%interior_tendency_compute()
        if (marbl_instances(n)%StatusLog%labort_MARBL) then
          call marbl_instances(n)%StatusLog%log_error_trace('interior_tendency_compute', subname)
          return
        end if

        !  5e. write to history file?
      end do ! column
    end do ! instance


    ! 6. Output netCDF
    call marbl_io_write_history(outfile, col_start, col_cnt, marbl_instances, num_active_levels, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_write_history', subname)
      return
    end if


    ! 7. Close all netCDF files
    call marbl_io_close_all(driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_close_all', subname)
      return
    end if

  end subroutine test

  !*****************************************************************************

  subroutine initialize(infile, outfile, num_insts, num_levels, num_active_levels, col_start, col_cnt, grid_data, driver_status_log)

    use marbl_io_mod,  only : marbl_io_open
    use marbl_io_mod,  only : marbl_io_read_dim

    character(len=*),                   intent(in)    :: infile
    character(len=*),                   intent(in)    :: outfile
    integer,                            intent(in)    :: num_insts
    integer,                            intent(out)   :: num_levels
    integer, dimension(:), allocatable, intent(out)   :: num_active_levels
    integer, dimension(:), allocatable, intent(out)   :: col_start
    integer, dimension(:), allocatable, intent(out)   :: col_cnt
    type(grid_data_type),               intent(inout) :: grid_data
    type(marbl_log_type),               intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_compute_cols_drv:initalize'
    character(len=char_len) :: log_message
    integer :: n, tmp_fid ! netCDF will return a file id
    integer :: num_cols

    ! 1. Open necessary netCDF files
    !    1a. Input (grid info, forcing fields, initial conditions)
    call marbl_io_open(infile, .true., tmp_fid, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if

    !    1b. Output (diagnostics)
    call marbl_io_open(outfile, .false., tmp_fid, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if
    write(log_message, "(A, 1X, A)") "* MARBL output will be written to", outfile
    call driver_status_log%log_noerror(log_message, subname)


    ! 2. Domain decomposition (distribute columns among instances)
    !    2a. Get column count from netCDF
    call marbl_io_read_dim(infile, 'column', num_cols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_dim(num_cols)', subname)
      return
    end if

    !    2b. Determine which columns each instance owns
    !        Note that we use 0-base indexing for
    !        col_start and 1-base indexing for col_id_loc
    !        (col_id_loc in [1, col_cnt(n)]), so
    !            col_id = col_start(n) + col_id_loc
    allocate(col_start(num_insts), col_cnt(num_insts))
    col_start = 0
    col_cnt = ceiling(real(num_cols, r8) / real(num_insts, r8))
    do n=2, num_insts
      col_start(n) = col_start(n-1) + col_cnt(n-1)
      if (col_start(n) .ge. num_insts) then
        col_start(n) = num_insts
        col_cnt(n) = 0
      else
        col_cnt(n) = min(num_insts - col_start(n-1), col_cnt(n))
      end if
    end do

    !   2c. Log decomposition
    do n=1, num_insts
      write(log_message, "(A,I0,A,I0,A,I0)") "Instance ", n-1, " has ", col_cnt(n), &
                                             " columns, beginning with ", col_start(n)
      call driver_status_log%log_noerror(log_message, subname)
    end do


    ! 3. Read domain info, initial conditions, and forcing fields from netCDF file
    write(log_message, "(A, 1X, A)") "* Reading input from", infile
    call driver_status_log%log_noerror(log_message, subname)
    call read_domain(infile, num_levels, grid_data, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('read_domain', subname)
      return
    end if

    ! 4. Temporary: need to store number of active levels for each column
    !    so it can be written later
    allocate(num_active_levels(num_cols))

  end subroutine initialize

  !*****************************************************************************

  subroutine read_domain(infile, num_levels, grid_data, driver_status_log)

    use marbl_io_mod, only : marbl_io_read_dim
    use marbl_io_mod, only : marbl_io_read_field

    character(len=*),     intent(in)    :: infile
    integer,              intent(inout) :: num_levels
    type(grid_data_type), intent(inout) :: grid_data
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_compute_cols_drv:read_domain'

    call marbl_io_read_dim(infile, 'zt', num_levels, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_field(zt)', subname)
      return
    end if

    allocate(grid_data%delta_z(num_levels), grid_data%zt(num_levels), grid_data%zw(num_levels))

    call marbl_io_read_field(infile, 'delta_z', grid_data%delta_z, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_field(delta_z)', subname)
      return
    end if

    call marbl_io_read_field(infile, 'zt', grid_data%zt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_field(zt)', subname)
      return
    end if

    call marbl_io_read_field(infile, 'zw', grid_data%zw, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_read_field(zw)', subname)
      return
    end if

  end subroutine read_domain

  !*****************************************************************************

  subroutine read_forcing_field(infile, col_id, forcing_fields, driver_status_log)

    use marbl_interface_public_types, only : marbl_forcing_fields_type
    use marbl_io_mod, only : marbl_io_read_field

    character(len=*),                              intent(in)    :: infile
    integer,                                       intent(in)    :: col_id
    type(marbl_forcing_fields_type), dimension(:), intent(inout) :: forcing_fields
    type(marbl_log_type),                          intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_compute_cols_drv:read_forcing_field'
    character(len=char_len)     :: log_message
    integer :: n

    do n=1, size(forcing_fields)
      select case(trim(forcing_fields(n)%metadata%varname))
        case('u10_sqr')
          call marbl_io_read_field(infile, 'u10_sqr', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('sss')
          call marbl_io_read_field(infile, 'SSS', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('sst')
          call marbl_io_read_field(infile, 'SST', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('Ice Fraction')
          call marbl_io_read_field(infile, 'ice_frac', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('Dust Flux')
          call marbl_io_read_field(infile, 'dust_flux', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('Iron Flux')
          call marbl_io_read_field(infile, 'iron_flux', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('NOx Flux')
          call marbl_io_read_field(infile, 'nox_flux', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('NHy Flux')
          call marbl_io_read_field(infile, 'nhy_flux', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('Atmospheric Pressure')
          call marbl_io_read_field(infile, 'atm_pressure', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('xco2')
          call marbl_io_read_field(infile, 'atm_co2', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('xco2_alt_co2')
          call marbl_io_read_field(infile, 'atm_alt_co2', forcing_fields(n)%field_0d(1), driver_status_log, col_id=col_id)
        case('PAR Column Fraction')
          call marbl_io_read_field(infile, 'FRACR_BIN', forcing_fields(n)%field_1d(1,:), driver_status_log, col_id=col_id)
        case('Surface Shortwave')
          call marbl_io_read_field(infile, 'QSW_BIN', forcing_fields(n)%field_1d(1,:), driver_status_log, col_id=col_id)
        case('Potential Temperature')
          call marbl_io_read_field(infile, 'temperature', forcing_fields(n)%field_1d(1,:), driver_status_log, col_id=col_id)
        case('Salinity')
          call marbl_io_read_field(infile, 'salinity', forcing_fields(n)%field_1d(1,:), driver_status_log, col_id=col_id)
        case('Pressure')
          call marbl_io_read_field(infile, 'pressure', forcing_fields(n)%field_1d(1,:), driver_status_log, col_id=col_id)
        case('Iron Sediment Flux')
         call marbl_io_read_field(infile, 'iron_sed_flux', forcing_fields(n)%field_1d(1,:), driver_status_log, col_id=col_id)
        case DEFAULT
          write(log_message, "(3A)") "Unrecognized forcing field '", trim(forcing_fields(n)%metadata%varname), "'"
          call driver_status_log%log_error(log_message, subname)
          return
      end select
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_io_read_field(", trim(forcing_fields(n)%metadata%varname), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine read_forcing_field

  !****************************************************************************

  subroutine read_tracers_at_surface(infile, col_id, tracer_metadata, tracers_at_surface, driver_status_log)

    use marbl_interface_public_types, only : marbl_tracer_metadata_type
    use marbl_io_mod, only : marbl_io_read_field

    character(len=*),                                 intent(in)    :: infile
    integer,                                          intent(in)    :: col_id
    type(marbl_tracer_metadata_type), dimension(:),   intent(in)    :: tracer_metadata
    real(kind=r8),                    dimension(:,:), intent(inout) :: tracers_at_surface ! (num_surface_elem, tracer_cnt)
    type(marbl_log_type),                             intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_compute_cols_drv:read_tracers_at_surface'
    character(len=char_len) :: log_message
    integer :: n

    do n = 1, size(tracer_metadata)
      call marbl_io_read_field(infile, trim(tracer_metadata(n)%short_name), tracers_at_surface(n,:), driver_status_log, &
                               surf_only=.true., col_id=col_id)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_io_read_field(", trim(tracer_metadata(n)%short_name), " [surface])"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine read_tracers_at_surface

  !****************************************************************************

  subroutine read_tracers(infile, col_id, tracer_metadata, tracers, driver_status_log)

    use marbl_interface_public_types, only : marbl_tracer_metadata_type
    use marbl_io_mod, only : marbl_io_read_field

    character(len=*),                                 intent(in)    :: infile
    integer,                                          intent(in)    :: col_id
    type(marbl_tracer_metadata_type), dimension(:),   intent(in)    :: tracer_metadata
    real(kind=r8),                    dimension(:,:), intent(inout) :: tracers            ! (tracer_cnt, num_levels)
    type(marbl_log_type),                             intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_compute_cols_drv:read_tracers'
    character(len=char_len) :: log_message
    integer :: n

    do n = 1, size(tracer_metadata)
      call marbl_io_read_field(infile, trim(tracer_metadata(n)%short_name), tracers(n,:), driver_status_log, col_id=col_id)
      if (driver_status_log%labort_marbl) then
        write(log_message, "(3A)") "marbl_io_read_field(", trim(tracer_metadata(n)%short_name), ")"
        call driver_status_log%log_error_trace(log_message, subname)
        return
      end if
    end do

  end subroutine read_tracers

  !****************************************************************************

end module marbl_compute_cols_drv