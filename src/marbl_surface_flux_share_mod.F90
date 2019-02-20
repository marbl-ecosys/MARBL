module marbl_surface_flux_share_mod

  use marbl_kinds_mod, only : r8
  use marbl_settings_mod, only : ciso_on

  implicit none
  private

  public :: marbl_surface_flux_share_export_variables

contains

  subroutine marbl_surface_flux_share_export_variables(&
       surface_flux_internal, &
       tracers_at_surface, &
       marbl_tracer_indices, &
       surface_flux_share)

    use marbl_interface_private_types, only : marbl_surface_flux_internal_type
    use marbl_interface_private_types, only : marbl_tracer_index_type
    use marbl_interface_private_types, only : marbl_surface_flux_share_type

    type(marbl_surface_flux_internal_type), intent(in)    :: surface_flux_internal
    real (r8)                          ,    intent(in)    :: tracers_at_surface(:,:)
    type(marbl_tracer_index_type)      ,    intent(in)    :: marbl_tracer_indices
    type(marbl_surface_flux_share_type),    intent(inout) :: surface_flux_share

    ! Populate fields used by carbon isotopes if running with ciso module
    if (ciso_on) then
       surface_flux_share%pv_surf_fields(:)       = surface_flux_internal%pv_co2(:)
       surface_flux_share%dic_surf_fields(:)      = tracers_at_surface(:, marbl_tracer_indices%dic_ind)
       surface_flux_share%co2star_surf_fields(:)  = surface_flux_internal%co2star(:)
       surface_flux_share%dco2star_surf_fields(:) = surface_flux_internal%dco2star(:)
       surface_flux_share%co3_surf_fields(:)      = surface_flux_internal%co3(:)
    endif

  end subroutine marbl_surface_flux_share_export_variables

end module marbl_surface_flux_share_mod