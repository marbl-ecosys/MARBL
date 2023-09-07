module marbl_abio_dic_surface_flux_mod

    use marbl_kinds_mod, only : r8

    use marbl_constants_mod, only : c0

    use marbl_interface_private_types, only : marbl_tracer_index_type

    use marbl_settings_mod, only : abio_dic_on

    implicit none
    private

    public :: marbl_abio_dic_surface_flux_compute

contains

  subroutine marbl_abio_dic_surface_flux_compute(surface_fluxes, marbl_tracer_indices)


    real(r8),                                       intent(inout) :: surface_fluxes(:, :)
    type(marbl_tracer_index_type),                  intent(in)    :: marbl_tracer_indices

    ! Return immediately if not running with abiotic dic tracer module
    if (.not. abio_dic_on) return

    associate(                                                 &
        abio_ind_beg => marbl_tracer_indices%abio_dic%ind_beg, &
        abio_ind_end => marbl_tracer_indices%abio_dic%ind_end  &
        )

    !-----------------------------------------------------------------------
    !  abio fluxes initially set to 0
    !-----------------------------------------------------------------------

    surface_fluxes(:,abio_ind_beg:abio_ind_end) = c0

    end associate


  end subroutine marbl_abio_dic_surface_flux_compute

end module marbl_abio_dic_surface_flux_mod
