module marbl_abio_init_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_settings_mod, only : abio_dic_on
  use marbl_interface_public_types, only : marbl_tracer_metadata_type
  use marbl_interface_private_types, only : marbl_tracer_index_type

  implicit none
  private

  public  :: marbl_abio_init_tracer_metadata

contains

  !*****************************************************************************

  subroutine marbl_abio_init_tracer_metadata(unit_system, &
                                             marbl_tracer_indices, &
                                             marbl_tracer_metadata)

    !  Set tracer and forcing metadata
    use marbl_settings_mod, only : unit_system_type

    type(unit_system_type),           intent(in)    :: unit_system
    type(marbl_tracer_index_type),    intent(in)    :: marbl_tracer_indices
    type(marbl_tracer_metadata_type), intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n                             ! tracer index

    if (.not. abio_dic_on) return

    !-----------------------------------------------------------------------
    !  initialize non-autotroph metadata values
    !-----------------------------------------------------------------------

    associate(abio_dic_ind   => marbl_tracer_indices%abio_dic_ind,     &
              abio_di14c_ind => marbl_tracer_indices%abio_di14c_ind,   &
              abio_ind_beg   => marbl_tracer_indices%abio_dic%ind_beg, &
              abio_ind_end   => marbl_tracer_indices%abio_dic%ind_end  &
             )

    ! All abio tracers share units, tend_units, flux_units, and
    ! tracer_module_name
    do n=abio_ind_beg,abio_ind_end
      marbl_tracer_metadata(n)%units      = unit_system%conc_units
      marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
      marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units
      marbl_tracer_metadata(n)%tracer_module_name = 'abio'
    end do
    marbl_tracer_metadata(abio_dic_ind)%short_name='ABIO_DIC'
    marbl_tracer_metadata(abio_dic_ind)%long_name='Abiotic Dissolved Inorganic Carbon'

    marbl_tracer_metadata(abio_di14c_ind)%short_name='ABIO_DI14C'
    marbl_tracer_metadata(abio_di14c_ind)%long_name='Abiotic Dissolved Inorganic Carbon-14'


    end associate

  end subroutine marbl_abio_init_tracer_metadata

  !*****************************************************************************

end module marbl_abio_init_mod
