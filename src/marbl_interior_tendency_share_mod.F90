module marbl_interior_tendency_share_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8
  use marbl_settings_mod, only : ciso_on

  implicit none
  private

  public :: marbl_interior_tendency_share_update_particle_flux_from_above
  public :: marbl_interior_tendency_share_export_variables
  public :: marbl_interior_tendency_share_export_zooplankton
  public :: marbl_interior_tendency_share_export_particulate

contains

  !***********************************************************************

  subroutine marbl_interior_tendency_share_update_particle_flux_from_above(k, sinking_particle)

    use marbl_interface_private_types, only : column_sinking_particle_type

    integer (int_kind), intent(in) :: k
    type(column_sinking_particle_type), intent(inout) :: sinking_particle

    ! NOTE(bja, 201504) level k influx is equal to the level k-1 outflux.
    sinking_particle%sflux_in(k)  = sinking_particle%sflux_out(k-1)
    sinking_particle%hflux_in(k)  = sinking_particle%hflux_out(k-1)

  end subroutine marbl_interior_tendency_share_update_particle_flux_from_above

  !***********************************************************************

  subroutine marbl_interior_tendency_share_export_variables(&
       km, &
       marbl_tracer_indices, &
       tracer_local, &
       carbonate, &
       dissolved_organic_matter, &
       QA_dust_def, &
       interior_tendency_share)

    use marbl_interface_private_types, only : marbl_tracer_index_type
    use marbl_interface_private_types, only : carbonate_type
    use marbl_interface_private_types, only : dissolved_organic_matter_type
    use marbl_interface_private_types, only : marbl_interior_tendency_share_type

    integer,                                  intent(in)    :: km
    type(marbl_tracer_index_type),            intent(in)    :: marbl_tracer_indices
    real(r8),                                 intent(in)    :: tracer_local(marbl_tracer_indices%total_cnt,km)
    type(carbonate_type),                     intent(in)    :: carbonate
    type(dissolved_organic_matter_type),      intent(in)    :: dissolved_organic_matter
    real(r8),                                 intent(in)    :: QA_dust_def(km)
    type(marbl_interior_tendency_share_type), intent(inout) :: interior_tendency_share

    ! Populate fields used by carbon isotopes if running with ciso module
    if (ciso_on) then
      interior_tendency_share%QA_dust_def(:)    = QA_dust_def(:)
      interior_tendency_share%CO3_fields(:)   = carbonate%CO3(:)
      interior_tendency_share%HCO3_fields(:)  = carbonate%HCO3(:)
      interior_tendency_share%H2CO3_fields(:) = carbonate%H2CO3(:)
      interior_tendency_share%CO3_sat_calcite(:) = carbonate%CO3_sat_calcite(:)
      interior_tendency_share%DOCtot_remin_fields(:) = &
           dissolved_organic_matter%DOC_remin(:) + dissolved_organic_matter%DOCr_remin(:)
      interior_tendency_share%DOCtot_loc_fields(:) = &
           tracer_local(marbl_tracer_indices%DOC_ind,:) + tracer_local(marbl_tracer_indices%DOCr_ind,:)
    end if

  end subroutine marbl_interior_tendency_share_export_variables

  !***********************************************************************

  subroutine marbl_interior_tendency_share_export_zooplankton(zooplankton_local, &
       zooplankton_derived_terms, zooplankton_share)

    use marbl_interface_private_types, only : zooplankton_derived_terms_type
    use marbl_interface_private_types, only : zooplankton_local_type
    use marbl_interface_private_types, only : zooplankton_share_type

    type(zooplankton_local_type),         intent(in)    :: zooplankton_local
    type(zooplankton_derived_terms_type), intent(in)    :: zooplankton_derived_terms
    type(zooplankton_share_type),         intent(inout) :: zooplankton_share

    ! Populate fields used by carbon isotopes if running with ciso module
    if (ciso_on) then
      zooplankton_share%zoototC_loc_fields(:)      = sum(zooplankton_local%C(:,:), dim=1)
      zooplankton_share%zootot_loss_fields(:)      = sum(zooplankton_derived_terms%zoo_loss(:,:), dim=1)
      zooplankton_share%zootot_loss_poc_fields(:)  = sum(zooplankton_derived_terms%zoo_loss_poc(:,:), dim=1)
      zooplankton_share%zootot_loss_doc_fields(:)  = sum(zooplankton_derived_terms%zoo_loss_doc(:,:), dim=1)
      zooplankton_share%zootot_loss_dic_fields(:)  = sum(zooplankton_derived_terms%zoo_loss_dic(:,:), dim=1)
      zooplankton_share%zootot_graze_fields(:)     = sum(zooplankton_derived_terms%zoo_graze(:,:), dim=1)
      zooplankton_share%zootot_graze_zoo_fields(:) = sum(zooplankton_derived_terms%zoo_graze_zoo(:,:), dim=1)
      zooplankton_share%zootot_graze_poc_fields(:) = sum(zooplankton_derived_terms%zoo_graze_poc(:,:), dim=1)
      zooplankton_share%zootot_graze_doc_fields(:) = sum(zooplankton_derived_terms%zoo_graze_doc(:,:), dim=1)
      zooplankton_share%zootot_graze_dic_fields(:) = sum(zooplankton_derived_terms%zoo_graze_dic(:,:), dim=1)
    end if

  end subroutine marbl_interior_tendency_share_export_zooplankton

  !***********************************************************************

  subroutine marbl_interior_tendency_share_export_particulate(k, POC, DECAY_Hard, &
       POC_PROD_avail, decay_POC_E, decay_CaCO3, poc_diss, caco3_diss, &
       marbl_particulate_share)

    use marbl_interface_private_types, only : column_sinking_particle_type
    use marbl_interface_private_types, only : marbl_particulate_share_type

    integer,                            intent(in)    :: k
    type(column_sinking_particle_type), intent(in)    :: POC
    real(r8),                           intent(in)    :: DECAY_Hard
    real(r8),                           intent(in)    :: POC_PROD_avail
    real(r8),                           intent(in)    :: decay_POC_E
    real(r8),                           intent(in)    :: decay_CaCO3
    real(r8),                           intent(in)    :: poc_diss
    real(r8),                           intent(in)    :: caco3_diss
    type(marbl_particulate_share_type), intent(inout) :: marbl_particulate_share

    if (ciso_on) then
       marbl_particulate_share%POC_remin_fields(k)      = POC%remin(k)
       marbl_particulate_share%DECAY_Hard_fields(k)     = DECAY_Hard
       marbl_particulate_share%POC_PROD_avail_fields(k) = POC_PROD_avail
       marbl_particulate_share%decay_POC_E_fields(k)    = decay_POC_E
       marbl_particulate_share%decay_CaCO3_fields(k)    = decay_CaCO3
       marbl_particulate_share%poc_diss_fields(k)       = poc_diss
       marbl_particulate_share%caco3_diss_fields(k)     = caco3_diss
    endif
  end subroutine marbl_interior_tendency_share_export_particulate
  !***********************************************************************

end module marbl_interior_tendency_share_mod