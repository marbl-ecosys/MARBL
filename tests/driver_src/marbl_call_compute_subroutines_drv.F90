module marbl_call_compute_subroutines_drv

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

    use marbl_io_mod, only : marbl_io_define_history
    use marbl_io_mod, only : marbl_io_write_history
    use marbl_io_mod, only : marbl_io_read_forcing_field
    use marbl_io_mod, only : marbl_io_read_tracers_at_surface
    use marbl_io_mod, only : marbl_io_read_tracers
    use marbl_io_mod, only : surface_flux_diag_buffer
    use marbl_io_mod, only : interior_tendency_diag_buffer
    use marbl_io_mod, only : marbl_io_get_init_file_var_by_name
    use marbl_io_mod, only : marbl_io_close_files

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances
    character(len=*),                          intent(in)    :: hist_file
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_call_compute_subroutines_drv:test'
    character(len=*), parameter :: infile = '../../input_files/initial_conditions/call_compute_subroutines.20190718.nc'
    real(r8), allocatable, dimension(:,:) :: surface_fluxes        ! num_cols x num_tracers
    real(r8), allocatable, dimension(:,:,:) :: interior_tendencies ! num_tracers x num_levels x num_cols
    real(r8), allocatable, dimension(:,:,:) :: tracer_initial_vals ! num_tracers x num_levels x num_cols
    integer :: num_levels, num_cols, num_tracers, m, n, col_id_loc, col_id, num_PAR_subcols
    type(grid_data_type) :: grid_data
    integer, allocatable, dimension(:) :: active_level_cnt, col_start, col_cnt


    ! 1. Initialize the test (reads grid info, sets up history files, etc)
    call initialize(infile, hist_file, size(marbl_instances), num_levels, active_level_cnt, &
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
    call marbl_io_define_history(marbl_instances, col_cnt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_define_history', subname)
      return
    end if


    ! Brunt of MARBL computations
    call marbl_io_get_init_file_var_by_name('active_level_cnt', active_level_cnt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_get_init_file_var_by_name(active_level_cnt)', subname)
      return
    end if

    do n=1, size(marbl_instances)
      ! 4. Call surface_flux_compute() (all columns simultaneously)
      !    4a. populate surface tracer values
      call marbl_io_read_tracers_at_surface(col_start(n)+1, col_cnt(n), marbl_instances(n)%tracer_metadata, &
                                   marbl_instances(n)%tracers_at_surface, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_tracers_at_surface', subname)
        return
      end if

      !    4b. populate surface_flux_forcings (per column)
      do col_id_loc = 1, col_cnt(n)
        call marbl_io_read_forcing_field(col_id_loc, col_start(n), marbl_instances(n)%surface_flux_forcings, &
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
        marbl_instances(n)%domain%kmt = active_level_cnt(col_id)

        !  5a. populate tracer values
        call marbl_io_read_tracers(col_id, marbl_instances(n)%tracer_metadata, marbl_instances(n)%tracers, &
                          driver_status_log)
        if (driver_status_log%labort_marbl) then
          call driver_status_log%log_error_trace('read_tracers', subname)
          return
        end if
        tracer_initial_vals(:,:,col_id) = marbl_instances(n)%tracers

        !  5b. populate interior_tendency_forcings
        call marbl_io_read_forcing_field(col_id_loc, col_start(n), marbl_instances(n)%interior_tendency_forcings, &
                                driver_status_log, active_level_cnt(col_id))
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
    call marbl_io_write_history(marbl_instances(1), surface_fluxes, interior_tendencies, &
                               tracer_initial_vals, active_level_cnt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_write_history', subname)
      return
    end if


    ! 7. Shutdown MARBL
    do n=1, size(marbl_instances)
      call marbl_instances(n)%shutdown()
    end do

    ! 7. Close all netCDF files
    call marbl_io_close_files(driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_close_files', subname)
      return
    end if

  end subroutine test

  !*****************************************************************************

  subroutine initialize(infile, hist_file, num_insts, num_levels, active_level_cnt, num_PAR_subcols, &
                        col_start, col_cnt, grid_data, driver_status_log)

    use marbl_io_mod, only : marbl_io_open_files
    use marbl_io_mod, only : marbl_io_distribute_cols
    use marbl_io_mod, only : marbl_io_read_domain

    character(len=*),                   intent(in)    :: infile
    character(len=*),                   intent(in)    :: hist_file
    integer,                            intent(in)    :: num_insts
    integer,                            intent(out)   :: num_levels
    integer, dimension(:), allocatable, intent(out)   :: active_level_cnt
    integer,                            intent(out)   :: num_PAR_subcols
    integer, dimension(:), allocatable, intent(out)   :: col_start
    integer, dimension(:), allocatable, intent(out)   :: col_cnt
    type(grid_data_type),               intent(inout) :: grid_data
    type(marbl_log_type),               intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_call_compute_subroutines_drv:initialize'
    character(len=char_len) :: log_message
    integer :: num_cols

    ! 1. Open necessary netCDF files
    !    1a. Input (grid info, forcing fields, initial conditions)
    !    1b. Output (diagnostics)
    call marbl_io_open_files(infile, hist_file, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if
    write(log_message, "(A, 1X, A)") "* MARBL output will be written to", trim(hist_file)
    call driver_status_log%log_noerror(log_message, subname)

    ! 2. Domain decomposition (distribute columns among instances)
    allocate(col_start(num_insts), col_cnt(num_insts))
    call marbl_io_distribute_cols(num_cols, col_start, col_cnt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_distribute_cols', subname)
      return
    end if

    ! 3. Read domain info, initial conditions, and forcing fields from netCDF file
    write(log_message, "(A, 1X, A)") "* Reading input from", infile
    call driver_status_log%log_noerror(log_message, subname)
    call marbl_io_read_domain(num_levels, num_PAR_subcols, grid_data, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('read_domain', subname)
      return
    end if

    ! 4. Need to store number of active levels for each column to properly apply
    !    mask when writing full-depth data to history file
    allocate(active_level_cnt(num_cols))

  end subroutine initialize

  !****************************************************************************

end module marbl_call_compute_subroutines_drv
