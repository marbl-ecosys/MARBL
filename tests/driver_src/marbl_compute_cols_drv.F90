module marbl_compute_cols_drv

  use marbl_interface, only : marbl_interface_class
  use marbl_kinds_mod, only : r8, char_len
  use marbl_logging,   only : marbl_log_type
  use marbl_io_mod,    only : grid_data_type

  implicit none
  private
  save

  Public :: test

Contains

  !****************************************************************************

  subroutine test(marbl_instances, hist_file, driver_status_log)

    use marbl_netcdf_mod, only : marbl_netcdf_read_field
    use marbl_netcdf_mod, only : marbl_netcdf_define_history
    use marbl_netcdf_mod, only : marbl_netcdf_write_history
    use marbl_netcdf_mod, only : marbl_netcdf_close_all
    use marbl_netcdf_mod, only : surface_flux_diag_buffer
    use marbl_netcdf_mod, only : interior_tendency_diag_buffer

    use marbl_io_mod, only : marbl_io_read_forcing_field
    use marbl_io_mod, only : marbl_io_read_tracers_at_surface
    use marbl_io_mod, only : marbl_io_read_tracers

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances
    character(len=*),                          intent(in)    :: hist_file
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_compute_cols_drv:test'
    character(len=*), parameter :: infile = '../../input_files/initial_conditions/compute_cols.20190718.nc'
    real(r8), allocatable, dimension(:,:) :: surface_fluxes        ! num_cols x num_tracers
    real(r8), allocatable, dimension(:,:,:) :: interior_tendencies ! num_tracers x num_levels x num_cols
    real(r8), allocatable, dimension(:,:,:) :: tracer_initial_vals ! num_tracers x num_levels x num_cols
    integer :: num_levels, num_cols, num_tracers, m, n, col_id_loc, col_id, num_PAR_subcols
    type(grid_data_type) :: grid_data
    integer, allocatable, dimension(:) :: num_active_levels, col_start, col_cnt


    ! 1. Initialize the test (reads grid info, sets up history files, etc)
    call initialize(infile, hist_file, size(marbl_instances), num_levels, num_active_levels, &
                    num_PAR_subcols, col_start, col_cnt, grid_data, driver_status_log)
    if (driver_status_log%labort_MARBL) then
      call driver_status_log%log_error_trace('initialize', subname)
      return
    end if


    ! 2. Initialize each instance of MARBL
    do n=1, size(marbl_instances)
      call marbl_instances(n)%init(gcm_num_levels = num_levels,                &
                                   gcm_num_PAR_subcols = num_PAR_subcols,      &
                                   gcm_num_elements_surface_flux = col_cnt(n), &
                                   gcm_delta_z = grid_data%delta_z,            &
                                   gcm_zw = grid_data%zw,                      &
                                   gcm_zt = grid_data%zt)
    end do


    ! 3. Initialize diagnostic buffers and define diagnostic fields in output netCDF file
    !    (a) Set constants
    num_cols = sum(col_cnt)
    num_tracers = size(marbl_instances(1)%tracer_metadata)

    !    (b) Initialize diagnostic buffers
    call surface_flux_diag_buffer%construct(num_levels, num_cols, &
         marbl_instances(1)%surface_flux_diags)
    call interior_tendency_diag_buffer%construct(num_levels, num_cols, &
         marbl_instances(1)%interior_tendency_diags)

    !    (c) Initialize memory for fields that driver writes to history (not coming via MARBL diagnostic type)
    allocate(surface_fluxes(num_cols, num_tracers))
    allocate(interior_tendencies(num_tracers, num_levels, num_cols))
    allocate(tracer_initial_vals(num_tracers, num_levels, num_cols))

    !    (d) netCDF calls to create history file (dimensions are defined but data is not written)
    call marbl_netcdf_define_history(marbl_instances, col_cnt, hist_file, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_define_history', subname)
      return
    end if


    ! Brunt of MARBL computations
    do n=1, size(marbl_instances)
      ! 4. Call surface_flux_compute() (all columns simultaneously)
      !    4a. populate surface tracer values
      call marbl_io_read_tracers_at_surface(infile, col_start(n)+1, col_cnt(n), marbl_instances(n)%tracer_metadata, &
                                   marbl_instances(n)%tracers_at_surface, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_tracers_at_surface', subname)
        return
      end if

      !    4b. populate surface_flux_forcings (per column)
      do col_id_loc = 1, col_cnt(n)
        call marbl_io_read_forcing_field(infile, col_id_loc, col_start(n), marbl_instances(n)%surface_flux_forcings, &
                                         driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('read_forcing_field(surface)', subname)
          return
        end if
      end do

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

      !    4e. write to diagnostic buffer
      do m=1, surface_flux_diag_buffer%num_diags
        if (allocated(surface_flux_diag_buffer%diags(m)%field_2d)) then
          surface_flux_diag_buffer%diags(m)%field_2d((col_start(n)+1):(col_start(n)+col_cnt(n))) = &
              marbl_instances(n)%surface_flux_diags%diags(m)%field_2d(:)
          surface_flux_diag_buffer%diags(m)%ref_depth_2d = &
              marbl_instances(n)%surface_flux_diags%diags(m)%ref_depth * 100._r8 ! m -> cm
        else
          surface_flux_diag_buffer%diags(m)%field_3d(:,(col_start(n)+1):(col_start(n)+col_cnt(n))) = &
              marbl_instances(n)%surface_flux_diags%diags(m)%field_3d(:,:)
        end if
      end do
      surface_fluxes((col_start(n)+1):(col_start(n)+col_cnt(n)),:) = marbl_instances(n)%surface_fluxes(:,:)

      ! 5. Call interior_tendency_compute() (one column at a time)
      do col_id_loc = 1, col_cnt(n)
        col_id = col_start(n)+col_id_loc
        call marbl_netcdf_read_field(infile, 'active_level_cnt', marbl_instances(n)%domain%kmt, driver_status_log, col_id=col_id)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('marbl_netcdf_read_field(active_level_cnt)', subname)
          return
        end if
        num_active_levels(col_id) = marbl_instances(n)%domain%kmt

        !  5a. populate tracer values
        call marbl_io_read_tracers(infile, col_id, marbl_instances(n)%tracer_metadata, marbl_instances(n)%tracers, &
                          driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('read_tracers', subname)
          return
        end if
        tracer_initial_vals(:,:,col_id) = marbl_instances(n)%tracers

        !  5b. populate interior_tendency_forcings
        call marbl_io_read_forcing_field(infile, col_id_loc, col_start(n), marbl_instances(n)%interior_tendency_forcings, &
                                driver_status_log, num_active_levels(col_id))
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

        !  5e. write to diagnostic buffer
        do m=1, interior_tendency_diag_buffer%num_diags
          if (allocated(interior_tendency_diag_buffer%diags(m)%field_2d)) then
            interior_tendency_diag_buffer%diags(m)%field_2d(col_id) = &
                marbl_instances(n)%interior_tendency_diags%diags(m)%field_2d(1)
            interior_tendency_diag_buffer%diags(m)%ref_depth_2d = &
                marbl_instances(n)%interior_tendency_diags%diags(m)%ref_depth * 100._r8 ! m -> cm
          else
            interior_tendency_diag_buffer%diags(m)%field_3d(:,col_id) = &
                marbl_instances(n)%interior_tendency_diags%diags(m)%field_3d(:,1)
          end if
        end do
        interior_tendencies(:,:,col_id) = marbl_instances(n)%interior_tendencies(:,:)
      end do ! column
    end do ! instance


    ! 6. Output netCDF
    call marbl_netcdf_write_history(hist_file, marbl_instances(1), surface_fluxes, interior_tendencies, &
                                tracer_initial_vals, num_active_levels, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_write_history', subname)
      return
    end if


    ! 7. Shutdown MARBL
    do n=1, size(marbl_instances)
      call marbl_instances(n)%shutdown()
    end do

    ! 7. Close all netCDF files
    call marbl_netcdf_close_all(driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_close_all', subname)
      return
    end if

  end subroutine test

  !*****************************************************************************

  subroutine initialize(infile, hist_file, num_insts, num_levels, num_active_levels, num_PAR_subcols, &
                        col_start, col_cnt, grid_data, driver_status_log)

    use marbl_netcdf_mod, only : marbl_netcdf_open
    use marbl_netcdf_mod, only : marbl_netcdf_read_dim

    use marbl_io_mod, only : marbl_io_read_domain

    character(len=*),                   intent(in)    :: infile
    character(len=*),                   intent(in)    :: hist_file
    integer,                            intent(in)    :: num_insts
    integer,                            intent(out)   :: num_levels
    integer, dimension(:), allocatable, intent(out)   :: num_active_levels
    integer,                            intent(out)   :: num_PAR_subcols
    integer, dimension(:), allocatable, intent(out)   :: col_start
    integer, dimension(:), allocatable, intent(out)   :: col_cnt
    type(grid_data_type),               intent(inout) :: grid_data
    type(marbl_log_type),               intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_compute_cols_drv:initalize'
    character(len=char_len) :: log_message
    integer :: n
    integer :: num_cols, cols_remaining

    ! 1. Open necessary netCDF files
    !    1a. Input (grid info, forcing fields, initial conditions)
    call marbl_netcdf_open(infile, .true., driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_open', subname)
      return
    end if

    !    1b. Output (diagnostics)
    call marbl_netcdf_open(hist_file, .false., driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_open', subname)
      return
    end if
    write(log_message, "(A, 1X, A)") "* MARBL output will be written to", trim(hist_file)
    call driver_status_log%log_noerror(log_message, subname)

    ! 2. Domain decomposition (distribute columns among instances)
    !    2a. Get column count from netCDF
    call marbl_netcdf_read_dim(infile, 'column', num_cols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_netcdf_read_dim(num_cols)', subname)
      return
    end if

    !    2b. Determine which columns each instance owns
    !        Note that we use 0-base indexing for
    !        col_start and 1-base indexing for col_id_loc
    !        (col_id_loc in [1, col_cnt(n)]), so
    !            col_id = col_start(n) + col_id_loc
    allocate(col_start(num_insts), col_cnt(num_insts))
    cols_remaining = num_cols
    do n=1, num_insts
      if (n.eq.1) then
        col_start(n) = 0
      else
        col_start(n) = col_start(n-1) + col_cnt(n-1)
      end if
      col_cnt(n) = cols_remaining / (num_insts-n+1) ! remaining cols / remaining insts
      cols_remaining = cols_remaining - col_cnt(n)
    end do

    !   2c. Log decomposition
    do n=1, num_insts
      write(log_message, "(A, I0, A, I0, A, I0)") "Instance ", n, " has ", col_cnt(n),        &
                                                  " columns, beginning with ", col_start(n)+1
      call driver_status_log%log_noerror(log_message, subname)
    end do


    ! 3. Read domain info, initial conditions, and forcing fields from netCDF file
    write(log_message, "(A, 1X, A)") "* Reading input from", infile
    call driver_status_log%log_noerror(log_message, subname)
    call marbl_io_read_domain(infile, num_levels, num_PAR_subcols, grid_data, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('read_domain', subname)
      return
    end if

    ! 4. Temporary: need to store number of active levels for each column
    !    so it can be written later
    allocate(num_active_levels(num_cols))

  end subroutine initialize

  !****************************************************************************

end module marbl_compute_cols_drv
