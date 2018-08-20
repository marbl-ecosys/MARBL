module marbl_interior_share_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8
  use marbl_interface_private_types, only : column_sinking_particle_type
  use marbl_interface_private_types, only : marbl_tracer_index_type
  use marbl_interface_private_types, only : carbonate_type
  use marbl_interface_private_types, only : dissolved_organic_matter_type
  use marbl_interface_private_types, only : marbl_interior_tendency_share_type
  use marbl_pft_mod, only : zooplankton_local_type
  use marbl_pft_mod, only : zooplankton_secondary_species_type
  use marbl_pft_mod, only : marbl_zooplankton_share_type
  use marbl_settings_mod, only : ciso_on

  implicit none
  private

  public :: marbl_interior_share_update_sinking_particle_from_level_above
  public :: marbl_interior_share_export_variables
  public :: marbl_interior_share_export_zooplankton

contains

  !***********************************************************************

  subroutine marbl_interior_share_update_sinking_particle_from_level_above(k, sinking_particle)

    integer (int_kind), intent(in) :: k
    type(column_sinking_particle_type), intent(inout) :: sinking_particle

    ! NOTE(bja, 201504) level k influx is equal to the level k-1 outflux.
    sinking_particle%sflux_in(k)  = sinking_particle%sflux_out(k-1)
    sinking_particle%hflux_in(k)  = sinking_particle%hflux_out(k-1)

  end subroutine marbl_interior_share_update_sinking_particle_from_level_above

  !***********************************************************************

  subroutine marbl_interior_share_export_variables(&
       tracer_local, &
       marbl_tracer_indices, &
       carbonate, &
       dissolved_organic_matter, &
       QA_dust_def, &
       marbl_interior_tendency_share)

    real(r8),                                 intent(in)    :: tracer_local(:)
    type(marbl_tracer_index_type),            intent(in)    :: marbl_tracer_indices
    type(carbonate_type),                     intent(in)    :: carbonate
    type(dissolved_organic_matter_type),      intent(in)    :: dissolved_organic_matter
    real(r8),                                 intent(in)    :: QA_dust_def
    type(marbl_interior_tendency_share_type), intent(inout) :: marbl_interior_tendency_share

    ! Only populate this datatype if running with carbon isotopes
    if (.not. ciso_on) return

    marbl_interior_tendency_share%QA_dust_def    = QA_dust_def

    marbl_interior_tendency_share%CO3_fields   = carbonate%CO3
    marbl_interior_tendency_share%HCO3_fields  = carbonate%HCO3
    marbl_interior_tendency_share%H2CO3_fields = carbonate%H2CO3
    marbl_interior_tendency_share%CO3_sat_calcite = carbonate%CO3_sat_calcite

    marbl_interior_tendency_share%DOCtot_loc_fields = &
         tracer_local(marbl_tracer_indices%DOC_ind) + tracer_local(marbl_tracer_indices%DOCr_ind)
    marbl_interior_tendency_share%DOCtot_remin_fields = &
         dissolved_organic_matter%DOC_remin + dissolved_organic_matter%DOCr_remin

  end subroutine marbl_interior_share_export_variables

  !***********************************************************************

  subroutine marbl_interior_share_export_zooplankton(&
       zooplankton_local, &
       zooplankton_secondary_species, &
       marbl_zooplankton_share)

    type(zooplankton_local_type)             , intent(in)    :: zooplankton_local(:)
    type(zooplankton_secondary_species_type) , intent(in)    :: zooplankton_secondary_species(:)
    type(marbl_zooplankton_share_type)       , intent(inout) :: marbl_zooplankton_share

    ! Only populate this datatype if running with carbon isotopes
    if (.not. ciso_on) return

    associate(share => marbl_zooplankton_share)
       share%zoototC_loc_fields      = sum(zooplankton_local(:)%C)
       share%zootot_loss_fields      = sum(zooplankton_secondary_species(:)%zoo_loss)
       share%zootot_loss_poc_fields  = sum(zooplankton_secondary_species(:)%zoo_loss_poc)
       share%zootot_loss_doc_fields  = sum(zooplankton_secondary_species(:)%zoo_loss_doc)
       share%zootot_loss_dic_fields  = sum(zooplankton_secondary_species(:)%zoo_loss_dic)
       share%zootot_graze_fields     = sum(zooplankton_secondary_species(:)%zoo_graze)
       share%zootot_graze_zoo_fields = sum(zooplankton_secondary_species(:)%zoo_graze_zoo)
       share%zootot_graze_poc_fields = sum(zooplankton_secondary_species(:)%zoo_graze_poc)
       share%zootot_graze_doc_fields = sum(zooplankton_secondary_species(:)%zoo_graze_doc)
       share%zootot_graze_dic_fields = sum(zooplankton_secondary_species(:)%zoo_graze_dic)
    end associate

  end subroutine marbl_interior_share_export_zooplankton

  !***********************************************************************

end module marbl_interior_share_mod