module marbl_call_compute_subroutines_drv

  use marbl_interface, only : marbl_interface_class
  use marbl_kinds_mod, only : r8, char_len
  use marbl_logging,   only : marbl_log_type
  use marbl_io_mod,    only : grid_data_type
  use marbl_io_mod,    only : forcing_fields_type

  implicit none
  private
  save

  Public :: test

Contains

  !****************************************************************************

  subroutine test(marbl_instances, hist_file, driver_status_log)

    use marbl_io_mod, only : marbl_io_open_files
    use marbl_io_mod, only : marbl_io_construct_diag_buffers
    use marbl_io_mod, only : marbl_io_define_history
    use marbl_io_mod, only : marbl_io_copy_into_diag_buffer
    use marbl_io_mod, only : marbl_io_write_history
    use marbl_io_mod, only : marbl_io_read_forcing_field
    use marbl_io_mod, only : marbl_io_read_tracers
    use marbl_io_mod, only : marbl_io_close_files
    use marbl_io_mod, only : marbl_io_destruct_diag_buffers

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances
    character(len=*),                          intent(in)    :: hist_file
    type(marbl_log_type),                      intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_call_compute_subroutines_drv:test'
    character(len=*), parameter :: infile = '../../input_files/initial_conditions/call_compute_subroutines.20190718.nc'
    character(len=char_len) :: log_message
    real(r8),                  allocatable, dimension(:,:)   :: surface_fluxes              ! num_cols x num_tracers
    real(r8),                  allocatable, dimension(:,:,:) :: interior_tendencies         ! num_tracers x num_levels x num_cols
    real(r8),                  allocatable, dimension(:,:,:) :: tracer_initial_vals         ! num_tracers x num_levels x num_cols
    type(forcing_fields_type), allocatable, dimension(:)     :: surface_flux_forcings       ! num_forcings
    type(forcing_fields_type), allocatable, dimension(:)     :: interior_tendency_forcings  ! num_forcings
    integer,                   allocatable, dimension(:)     :: active_level_cnt, col_start, col_cnt

    integer :: num_levels, num_cols, num_tracers, m, n, col_id_loc, col_id, num_PAR_subcols
    type(grid_data_type) :: grid_data

    ! 1. Open necessary netCDF files
    !    (a) Input (grid info, forcing fields, initial conditions)
    !    (b) Output (diagnostics)
    call marbl_io_open_files(infile, hist_file, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      return
    end if
    write(log_message, "(A, 1X, A)") "* MARBL output will be written to", trim(hist_file)
    call driver_status_log%log_noerror(log_message, subname)

    ! --------------------------------------------------------------------------

    ! 2. Initialize the test (reads grid info, distributes columns, etc)
    call set_domain(size(marbl_instances), num_levels, active_level_cnt, num_PAR_subcols, &
                    col_start, col_cnt, grid_data, driver_status_log)
    if (driver_status_log%labort_MARBL) then
      call driver_status_log%log_error_trace('set_domain', subname)
      return
    end if

    ! --------------------------------------------------------------------------

    ! 3. Initialize each instance of MARBL
    do n=1, size(marbl_instances)
      call marbl_instances(n)%init(gcm_num_levels = num_levels,                &
                                   gcm_num_PAR_subcols = num_PAR_subcols,      &
                                   gcm_num_elements_surface_flux = col_cnt(n), &
                                   gcm_delta_z = grid_data%delta_z,            &
                                   gcm_zw = grid_data%zw,                      &
                                   gcm_zt = grid_data%zt)
    end do

    ! --------------------------------------------------------------------------

    ! 4. Initialize diagnostic buffers, define diagnostic fields in output netCDF file, and
    !    read initial conditions / forcing data
    !    (a) Set constants
    num_cols = sum(col_cnt)
    num_tracers = size(marbl_instances(1)%tracer_metadata)

    !    (b) Initialize diagnostic buffers
    call marbl_io_construct_diag_buffers(num_levels, num_cols, marbl_instances(1))

    !    (c) Initialize memory for fields that driver writes to history (not coming via MARBL diagnostic type)
    !        as well as fields that driver reads (forcing fields)
    allocate(surface_fluxes(num_cols, num_tracers))
    allocate(interior_tendencies(num_tracers, num_levels, num_cols))
    allocate(tracer_initial_vals(num_tracers, num_levels, num_cols))
    allocate(surface_flux_forcings(size(marbl_instances(1)%surface_flux_forcings)))
    allocate(interior_tendency_forcings(size(marbl_instances(1)%interior_tendency_forcings)))
    ! Allocate memory inside surface_flux_forcings
    do m=1, size(marbl_instances(1)%surface_flux_forcings)
      if (associated(marbl_instances(1)%surface_flux_forcings(m)%field_0d)) then
        allocate(surface_flux_forcings(m)%field_0d(num_cols))
      else
        allocate(surface_flux_forcings(m)%field_1d(num_cols, &
            size(marbl_instances(1)%surface_flux_forcings(m)%field_1d, dim=2)))
      end if
    end do
    ! Allocate memory inside interior_tendency_forcings
    do m=1, size(marbl_instances(1)%interior_tendency_forcings)
      if (associated(marbl_instances(1)%interior_tendency_forcings(m)%field_0d)) then
        allocate(interior_tendency_forcings(m)%field_0d(num_cols))
      else
        allocate(interior_tendency_forcings(m)%field_1d(num_cols, &
            size(marbl_instances(1)%interior_tendency_forcings(m)%field_1d, dim=2)))
      end if
    end do

    !    (d) netCDF calls to create history file (dimensions are defined but data is not written)
    call marbl_io_define_history(marbl_instances, col_cnt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_define_history', subname)
      return
    end if

    !    (e) Read initial conditions and forcing data
    do n=1, num_cols
      !      (i) Read tracer values over full column
      call marbl_io_read_tracers(n, marbl_instances(1)%tracer_metadata, tracer_initial_vals(:,:,n), &
                        driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_tracers', subname)
        return
      end if

      !      (ii) Read surface flux forcing fields
      call marbl_io_read_forcing_field(n, marbl_instances(1)%surface_flux_forcings, &
                                       surface_flux_forcings, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_forcing_field(surface)', subname)
        return
      end if

      !      (iii) Read interior tendency forcing fields
      call marbl_io_read_forcing_field(n, marbl_instances(1)%interior_tendency_forcings, &
                                       interior_tendency_forcings, driver_status_log, active_level_cnt(n))
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_forcing_field(interior)', subname)
        return
      end if
    end do

    ! --------------------------------------------------------------------------
    ! Brunt of MARBL computations
    ! --------------------------------------------------------------------------

    do n=1, size(marbl_instances)
      ! 5. Call surface_flux_compute() (all columns simultaneously)
      !    (a) call set_global_scalars() for consistent setting of time-varying scalars
      !        [surface_flux computation doesn't currently have any time-varying scalars]
      call marbl_instances(n)%set_global_scalars('surface_flux')

      do col_id_loc = 1, col_cnt(n)
        col_id = col_start(n)+col_id_loc

        !  (b) copy surface tracer values into marbl_instances(n)%tracers_at_surface
        marbl_instances(n)%tracers_at_surface(col_id_loc, :) = tracer_initial_vals(:, 1, col_id)

        !  (c) copy surface flux forcings into marbl_instances(n)%surface_flux_forcings
        do m=1, size(marbl_instances(n)%surface_flux_forcings)
          if (associated(marbl_instances(n)%surface_flux_forcings(m)%field_0d)) then
            marbl_instances(n)%surface_flux_forcings(m)%field_0d(col_id_loc) = surface_flux_forcings(m)%field_0d(col_id)
          else
            marbl_instances(n)%surface_flux_forcings(m)%field_1d(col_id_loc,:) = surface_flux_forcings(m)%field_1d(col_id,:)
          end if
        end do
      end do

      !    (d) populate marbl_instances(n)%surface_flux_saved_state (with 0s)
      do m=1, size(marbl_instances(n)%surface_flux_saved_state%state)
        marbl_instances(n)%surface_flux_saved_state%state(m)%field_2d(:) = 0._r8
      end do

      !    (e) call surface_flux_compute()
      call marbl_instances(n)%surface_flux_compute()
      if (marbl_instances(n)%StatusLog%labort_MARBL) then
        call marbl_instances(n)%StatusLog%log_error_trace('surface_flux_compute', subname)
        return
      end if

      !    (f) write to diagnostic buffers
      !        Note: passing col_start and col_cnt => surface flux diagnostic buffer
      call marbl_io_copy_into_diag_buffer(col_start(n), col_cnt(n), marbl_instances(n))
      surface_fluxes((col_start(n)+1):(col_start(n)+col_cnt(n)),:) = marbl_instances(n)%surface_fluxes(:,:)

      ! ------------------------------------------------------------------------

      ! 6. Call interior_tendency_compute() (one column at a time)
      do col_id_loc = 1, col_cnt(n)
        col_id = col_start(n)+col_id_loc

        !  (a) call set_global_scalars() for consistent setting of time-varying scalars
        !      [necessary when running ladjust_bury_coeff, since GCM is responsible
        !       for computing running means of values needed to compute burial coefficients]
        call marbl_instances(n)%set_global_scalars('interior_tendency')

        !  (b) set domain information
        !      In this example, the vertical grid is the same from column to column and
        !      therefore set during initialization. The columns vary in depth, so
        !      the index of the bottom layer must be updated for each column.
        marbl_instances(n)%domain%kmt = active_level_cnt(col_id)

        !  (c) copy tracer values into marbl_instances(n)%tracers
        marbl_instances(n)%tracers = tracer_initial_vals(:,:,col_id)

        !  (d) copy interior tendency forcings into marbl_instances(n)%interior_tendency_forcings
        do m=1, size(marbl_instances(n)%interior_tendency_forcings)
          if (associated(marbl_instances(n)%interior_tendency_forcings(m)%field_0d)) then
            marbl_instances(n)%interior_tendency_forcings(m)%field_0d(1) = &
                 interior_tendency_forcings(m)%field_0d(col_id)
          else
            marbl_instances(n)%interior_tendency_forcings(m)%field_1d(1,:) = &
                 interior_tendency_forcings(m)%field_1d(col_id,:)
          end if
        end do

        !  (e) populate marbl_instances(n)%interior_tendency_saved_state (with 0s)
        do m=1, size(marbl_instances(n)%interior_tendency_saved_state%state)
          if (allocated(marbl_instances(n)%interior_tendency_saved_state%state(m)%field_2d)) then
            marbl_instances(n)%interior_tendency_saved_state%state(m)%field_2d(:) = 0._r8
          else
            marbl_instances(n)%interior_tendency_saved_state%state(m)%field_3d(:,1) = 0._r8
          end if
        end do

        !  (f) call interior_tendency_compute()
        call marbl_instances(n)%interior_tendency_compute()
        if (marbl_instances(n)%StatusLog%labort_MARBL) then
          call marbl_instances(n)%StatusLog%log_error_trace('interior_tendency_compute', subname)
          return
        end if

        !  (g) write to diagnostic buffer
        !        Note: passing just col_id => interior tendency diagnostic buffer
        call marbl_io_copy_into_diag_buffer(col_id, marbl_instances(n))
        interior_tendencies(:,:,col_id) = marbl_instances(n)%interior_tendencies(:,:)
      end do ! column
    end do ! instance

    ! --------------------------------------------------------------------------

    ! 7. Output netCDF
    call marbl_io_write_history(marbl_instances(1), surface_fluxes, interior_tendencies, &
                               tracer_initial_vals, active_level_cnt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_write_history', subname)
      return
    end if

    ! --------------------------------------------------------------------------

    ! 8. Close all netCDF files
    call marbl_io_close_files(driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_close_files', subname)
      return
    end if

    ! --------------------------------------------------------------------------

    ! 9. Deallocate local variables as well as those in marbl_io_mod
    call marbl_io_destruct_diag_buffers()
    deallocate(surface_fluxes)
    deallocate(interior_tendencies)
    deallocate(tracer_initial_vals)
    ! Deallocate memory inside surface_flux_forcings
    do m=1, size(marbl_instances(1)%surface_flux_forcings)
      if (associated(marbl_instances(1)%surface_flux_forcings(m)%field_0d)) then
        deallocate(surface_flux_forcings(m)%field_0d)
      else
        deallocate(surface_flux_forcings(m)%field_1d)
      end if
    end do
    ! Deallocate memory inside interior_tendency_forcings
    do m=1, size(marbl_instances(1)%interior_tendency_forcings)
      if (associated(marbl_instances(1)%interior_tendency_forcings(m)%field_0d)) then
        deallocate(interior_tendency_forcings(m)%field_0d)
      else
        deallocate(interior_tendency_forcings(m)%field_1d)
      end if
    end do
    deallocate(surface_flux_forcings)
    deallocate(interior_tendency_forcings)
    deallocate(active_level_cnt)
    deallocate(col_start)
    deallocate(col_cnt)

    ! --------------------------------------------------------------------------

    ! 10. Shutdown MARBL
    do n=1, size(marbl_instances)
      call marbl_instances(n)%shutdown()
    end do

  end subroutine test

  !*****************************************************************************

  subroutine set_domain(num_insts, num_levels, active_level_cnt, num_PAR_subcols, &
                           col_start, col_cnt, grid_data, driver_status_log)

    use marbl_tools_mod, only : marbl_tools_distribute_cols
    use marbl_io_mod, only : marbl_io_read_domain

    integer,                            intent(in)    :: num_insts
    integer,                            intent(out)   :: num_levels
    integer, dimension(:), allocatable, intent(out)   :: active_level_cnt
    integer,                            intent(out)   :: num_PAR_subcols
    integer, dimension(:), allocatable, intent(out)   :: col_start
    integer, dimension(:), allocatable, intent(out)   :: col_cnt
    type(grid_data_type),               intent(inout) :: grid_data
    type(marbl_log_type),               intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_call_compute_subroutines_drv:set_domain'
    integer :: num_cols

    ! 1. Read domain info from netCDF file
    call marbl_io_read_domain(grid_data, active_level_cnt, num_cols, num_levels, num_PAR_subcols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('read_domain', subname)
      return
    end if

    ! 2. Domain decomposition (distribute columns among instances)
    call marbl_tools_distribute_cols(num_cols, num_insts, col_start, col_cnt, driver_status_log)
    ! NOTE: driver_status_log gets information but there are no abort conditions

  end subroutine set_domain

  !****************************************************************************

end module marbl_call_compute_subroutines_drv
