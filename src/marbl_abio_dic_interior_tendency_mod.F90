module marbl_abio_dic_interior_tendency_mod

    use marbl_kinds_mod, only : r8

    use marbl_constants_mod, only : c0
    use marbl_constants_mod, only : c14_lambda

    use marbl_settings_mod, only : abio_dic_on

    use marbl_interface_private_types, only : marbl_tracer_index_type

    use marbl_interface_public_types, only : marbl_diagnostics_type

    use marbl_abio_dic_diagnostics_mod, only : marbl_abio_dic_diagnostics_interior_tendency_compute

    implicit none
    private

    public :: marbl_abio_dic_interior_tendency_compute

contains

    subroutine marbl_abio_dic_interior_tendency_compute(marbl_tracer_indices, &
                                                        tracer_local, &
                                                        interior_tendency_diags, &
                                                        interior_tendencies)

        type(marbl_tracer_index_type), intent(in)    :: marbl_tracer_indices
        real (r8),                     intent(in)    :: tracer_local(:,:)
        type(marbl_diagnostics_type),  intent(inout) :: interior_tendency_diags
        real(r8),                      intent(inout) :: interior_tendencies(:,:)          ! (tracer_cnt, km) computed source/sink terms

        ! Return immediately if not running with abiotic dic tracer module
        if (.not. abio_dic_on) return

        associate(&
            ! Tracer
            di14c => tracer_local(marbl_tracer_indices%abio_di14c_ind, :), &
            ! Tracer indices
            di14c_ind    => marbl_tracer_indices%abio_di14c_ind, &
            abio_ind_beg => marbl_tracer_indices%abio_dic%ind_beg, &
            abio_ind_end => marbl_tracer_indices%abio_dic%ind_end &
            )

            interior_tendencies(abio_ind_beg:abio_ind_end, :) = c0
            interior_tendencies(di14c_ind, :) = -c14_lambda * di14c(:)

            call marbl_abio_dic_diagnostics_interior_tendency_compute(marbl_tracer_indices, tracer_local, &
                                                                      interior_tendency_diags)

        end associate

    end subroutine marbl_abio_dic_interior_tendency_compute

end module marbl_abio_dic_interior_tendency_mod
