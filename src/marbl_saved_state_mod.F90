module marbl_saved_state_mod

  implicit none
  private
  save

  public :: marbl_saved_state_init

Contains

  subroutine marbl_saved_state_init(surface_state, interior_state, surf_ind,  &
             interior_ind, num_levels, num_elements_surface_flux,             &
             num_elements_interior_tendency, marbl_status_log)

    use marbl_interface_public_types, only : marbl_saved_state_type
    use marbl_interface_private_types, only : marbl_surface_flux_saved_state_indexing_type
    use marbl_interface_private_types, only : marbl_interior_tendency_saved_state_indexing_type
    use marbl_logging, only : marbl_log_type
    use marbl_kinds_mod, only : char_len


    type(marbl_saved_state_type), intent(inout) :: surface_state
    type(marbl_saved_state_type), intent(inout) :: interior_state
    type(marbl_surface_flux_saved_state_indexing_type),  intent(inout) :: surf_ind
    type(marbl_interior_tendency_saved_state_indexing_type), intent(inout) :: interior_ind
    integer,                      intent(in)    :: num_levels
    integer,                      intent(in)    :: num_elements_surface_flux
    integer,                      intent(in)    :: num_elements_interior_tendency
    type(marbl_log_type),         intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_saved_state_mod:marbl_saved_state_init'

    character(len=char_len) :: lname, sname, units, vgrid
    integer :: rank

    call surface_state%construct(num_elements_surface_flux, num_levels)

    lname = 'surface pH'
    sname = 'PH_SURF'
    units = 'pH'
    vgrid = 'none'
    rank  = 2
    call surface_state%add_state(lname, sname, units, vgrid, rank,            &
         surf_ind%ph_surf, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("add_state(PH_SURF)", subname)
      return
    end if

    lname = 'surface pH (alternate CO2)'
    sname = 'PH_SURF_ALT_CO2'
    units = 'pH'
    vgrid = 'none'
    rank  = 2
    call surface_state%add_state(lname, sname, units, vgrid, rank,            &
         surf_ind%ph_alt_co2_surf, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("add_state(PH_SURF_ALT_CO2)", subname)
      return
    end if

    call interior_state%construct(num_elements_interior_tendency, num_levels)

    lname = '3D pH'
    sname = 'PH_3D'
    units = 'pH'
    vgrid = 'layer_avg'
    rank  = 3
    call interior_state%add_state(lname, sname, units, vgrid, rank,           &
         interior_ind%ph_col, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("add_state(PH_3D)", subname)
      return
    end if

    lname = '3D pH (alternate CO2)'
    sname = 'PH_3D_ALT_CO2'
    units = 'pH'
    vgrid = 'layer_avg'
    rank  = 3
    call interior_state%add_state(lname, sname, units, vgrid, rank,           &
         interior_ind%ph_alt_co2_col, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("add_state(PH_3D_ALT_CO2)", subname)
      return
    end if

  end subroutine marbl_saved_state_init

end module marbl_saved_state_mod
