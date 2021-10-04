module marbl_interior_tendency_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_constants_mod, only : T0_Kelvin
  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : c2
  use marbl_constants_mod, only : c10
  use marbl_constants_mod, only : mpercm
  use marbl_constants_mod, only : spd
  use marbl_constants_mod, only : dps
  use marbl_constants_mod, only : yps

  use marbl_interface_private_types, only : marbl_PAR_type
  use marbl_interface_private_types, only : autotroph_derived_terms_type
  use marbl_interface_private_types, only : autotroph_local_type
  use marbl_interface_private_types, only : zooplankton_derived_terms_type
  use marbl_interface_private_types, only : zooplankton_local_type
  use marbl_interface_private_types, only : zooplankton_share_type
  use marbl_interface_private_types, only : marbl_surface_flux_forcing_indexing_type
  use marbl_interface_private_types, only : marbl_interior_tendency_forcing_indexing_type
  use marbl_interface_private_types, only : marbl_tracer_index_type
  use marbl_interface_private_types, only : marbl_particulate_share_type
  use marbl_interface_private_types, only : marbl_interior_tendency_share_type
  use marbl_interface_private_types, only : dissolved_organic_matter_type
  use marbl_interface_private_types, only : carbonate_type
  use marbl_interface_private_types, only : column_sinking_particle_type

  use marbl_interface_public_types, only : marbl_domain_type
  use marbl_interface_public_types, only : marbl_forcing_fields_type
  use marbl_interface_public_types, only : marbl_saved_state_type
  use marbl_interface_public_types, only : marbl_running_mean_0d_type

  use marbl_logging, only : marbl_log_type

  use marbl_settings_mod, only : autotroph_cnt
  use marbl_settings_mod, only : zooplankton_cnt
  use marbl_settings_mod, only : max_grazer_prey_cnt
  use marbl_settings_mod, only : lsource_sink
  use marbl_settings_mod, only : ladjust_bury_coeff
  use marbl_settings_mod, only : autotroph_settings
  use marbl_settings_mod, only : zooplankton_settings
  use marbl_settings_mod, only : dust_to_Fe
  use marbl_settings_mod, only : denitrif_C_N
  use marbl_settings_mod, only : parm_Red_Fe_C
  use marbl_settings_mod, only : Q
  use marbl_settings_mod, only : parm_scalelen_z
  use marbl_settings_mod, only : parm_scalelen_vals
  use marbl_settings_mod, only : caco3_poc_min
  use marbl_settings_mod, only : DOCprod_refract
  use marbl_settings_mod, only : DONprod_refract
  use marbl_settings_mod, only : DOPprod_refract
  use marbl_settings_mod, only : POCremin_refract
  use marbl_settings_mod, only : PONremin_refract
  use marbl_settings_mod, only : POPremin_refract
  use marbl_settings_mod, only : f_toDON
  use marbl_settings_mod, only : f_graze_CaCO3_remin
  use marbl_settings_mod, only : f_graze_si_remin
  use marbl_settings_mod, only : f_graze_sp_poc_lim
  use marbl_settings_mod, only : parm_labile_ratio
  use marbl_settings_mod, only : parm_o2_min
  use marbl_settings_mod, only : parm_o2_min_delta
  use marbl_settings_mod, only : parm_red_d_c_o2
  use marbl_settings_mod, only : parm_red_d_c_o2_diaz
  use marbl_settings_mod, only : parm_Remin_D_C_O2
  use marbl_settings_mod, only : QCaCO3_max
  use marbl_settings_mod, only : Qfe_zoo
  use marbl_settings_mod, only : spc_poc_fac
  use marbl_settings_mod, only : grazing_relationship_settings
  use marbl_settings_mod, only : PON_bury_coeff
  use marbl_settings_mod, only : del_ph
  use marbl_settings_mod, only : phhi_3d_init
  use marbl_settings_mod, only : phlo_3d_init

  use marbl_pft_mod, only : Qp_zoo

  implicit none
  private

  public  :: marbl_interior_tendency_compute
  public  :: marbl_interior_tendency_adjust_bury_coeff

contains

  !***********************************************************************

  subroutine marbl_interior_tendency_compute( &
       domain,                                &
       interior_tendency_forcings,            &
       tracers,                               &
       surface_flux_forcing_indices,          &
       interior_tendency_forcing_indices,     &
       saved_state_ind,                       &
       marbl_tracer_indices,                  &
       marbl_timer_indices,                   &
       PAR,                                   &
       dissolved_organic_matter,              &
       carbonate,                             &
       autotroph_derived_terms,               &
       autotroph_local,                       &
       zooplankton_derived_terms,             &
       zooplankton_local,                     &
       zooplankton_share,                     &
       saved_state,                           &
       marbl_timers,                          &
       interior_tendency_share,               &
       marbl_particulate_share,               &
       interior_tendency_diags,               &
       interior_tendencies,                   &
       glo_avg_fields_interior_tendency,      &
       marbl_status_log)

    !  Compute time derivatives for ecosystem state variables

    use marbl_temperature, only : marbl_temperature_potemp
    use marbl_ciso_interior_tendency_mod, only : marbl_ciso_interior_tendency_compute
    use marbl_diagnostics_mod , only : marbl_diagnostics_interior_tendency_compute
    use marbl_interface_private_types, only : marbl_internal_timers_type
    use marbl_interface_private_types, only : marbl_timer_indexing_type
    use marbl_interface_private_types, only : marbl_interior_tendency_saved_state_indexing_type
    use marbl_interface_public_types, only : marbl_diagnostics_type
    use marbl_interior_tendency_share_mod, only : marbl_interior_tendency_share_export_variables
    use marbl_interior_tendency_share_mod, only : marbl_interior_tendency_share_export_zooplankton
    use marbl_restore_mod, only : marbl_restore_compute_interior_restore
    use marbl_settings_mod, only : lo2_consumption_scalef
    use marbl_settings_mod, only : lp_remin_scalef

    type(marbl_domain_type),                                 intent(in)    :: domain
    type(marbl_forcing_fields_type),                         intent(in)    :: interior_tendency_forcings(:)
    real(r8),                                                intent(in)    :: tracers(:,:)         ! (tracer_cnt, km) tracer values
    type(marbl_surface_flux_forcing_indexing_type),          intent(in)    :: surface_flux_forcing_indices
    type(marbl_interior_tendency_forcing_indexing_type),     intent(in)    :: interior_tendency_forcing_indices
    type(marbl_interior_tendency_saved_state_indexing_type), intent(in)    :: saved_state_ind
    type(marbl_tracer_index_type),                           intent(in)    :: marbl_tracer_indices
    type(marbl_timer_indexing_type),                         intent(in)    :: marbl_timer_indices
    type(marbl_PAR_type),                                    intent(inout) :: PAR
    type(dissolved_organic_matter_type),                     intent(inout) :: dissolved_organic_matter
    type(carbonate_type),                                    intent(inout) :: carbonate
    type(autotroph_derived_terms_type),                      intent(inout) :: autotroph_derived_terms
    type(autotroph_local_type),                              intent(inout) :: autotroph_local
    type(zooplankton_derived_terms_type),                    intent(inout) :: zooplankton_derived_terms
    type(zooplankton_local_type),                            intent(inout) :: zooplankton_local
    type(zooplankton_share_type),                            intent(inout) :: zooplankton_share
    type(marbl_saved_state_type),                            intent(inout) :: saved_state
    type(marbl_internal_timers_type),                        intent(inout) :: marbl_timers
    type(marbl_interior_tendency_share_type),                intent(inout) :: interior_tendency_share
    type(marbl_particulate_share_type),                      intent(inout) :: marbl_particulate_share
    type(marbl_diagnostics_type),                            intent(inout) :: interior_tendency_diags
    real(r8),                                                intent(out)   :: interior_tendencies(:,:)          ! (tracer_cnt, km) computed source/sink terms
    real(r8),                                                intent(out)   :: glo_avg_fields_interior_tendency(:)
    type(marbl_log_type),                                    intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_interior_tendency_mod:marbl_interior_tendency_compute'

    real(r8), dimension(size(tracers,1), domain%km) :: interior_restore
    real(r8), dimension(size(tracers,1), domain%km) :: tracer_local

    integer (int_kind) :: k         ! vertical level index

    real (r8) :: surf_press(domain%km)       ! pressure in surface layer
    real (r8) :: temperature(domain%km)      ! in situ temperature
    real (r8) :: o2_consumption_scalef(domain%km) ! O2 Consumption Scale Factor
    real (r8) :: p_remin_scalef(domain%km)   ! Particulate Remin Scale Factor
    real (r8) :: O2_production(domain%km)    ! O2 production
    real (r8) :: O2_consumption(domain%km)   ! O2 consumption
    real (r8) :: nitrif(domain%km)           ! nitrification (NH4 -> NO3) (mmol N/m^3/sec)
    real (r8) :: denitrif(domain%km)         ! WC nitrification (NO3 -> N2) (mmol N/m^3/sec)
    real (r8) :: sed_denitrif(domain%km)     ! sedimentary denitrification (nmol N/cm^3/sec)
    real (r8) :: other_remin(domain%km)      ! organic C remin not due oxic or denitrif (nmolC/cm^3/sec)
    real (r8) :: Tfunc(domain%km)
    real (r8) :: Fe_scavenge_rate(domain%km) ! annual scavenging rate of iron as % of ambient
    real (r8) :: Fe_scavenge(domain%km)      ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
    real (r8) :: Lig_scavenge(domain%km)     ! loss of Fe-binding Ligand from scavenging (mmol Fe/m^3/sec)
    real (r8) :: QA_dust_def(domain%km)
    real (r8) :: PON_remin(domain%km)        ! remin of PON
    real (r8) :: PON_sed_loss(domain%km)     ! loss of PON to sediments
    real (r8) :: Fefree(domain%km)           ! unbound Fe
    real (r8) :: Lig_prod(domain%km)         ! production of Fe-binding Ligand
    real (r8) :: Lig_photochem(domain%km)    ! loss of Fe-binding Ligand from UV radiation
    real (r8) :: Lig_deg(domain%km)          ! loss of Fe-binding Ligand from bacterial degradation
    real (r8) :: Lig_loss(domain%km)         ! loss of Fe-binding Ligand
    real (r8) :: totalChl_local(domain%km)   ! local value of totalChl

    ! NOTE(bja, 2015-07) vectorization: arrays that are (n, k, c, i)
    ! probably can not be vectorized reasonably over c without memory
    ! copies. If we break up the main k loop, some of the (k, c) loops
    ! can probably be vectorized over k and / or c!
    !-----------------------------------------------------------------------

    ! NOTE(bja, 2015-07) interior_tendencies=0 must come before the "not
    ! lsource_sink check to ensure correct answer when not doing
    ! computations.

    interior_tendencies(:, :) = c0

    if (.not. lsource_sink) then
       !-----------------------------------------------------------------------
       !  exit immediately if computations are not to be performed
       !-----------------------------------------------------------------------
       return
    endif

    associate(                                                      &
         km                  => domain%km,                          &
         kmt                 => domain%kmt,                         &
         num_PAR_subcols     => domain%num_PAR_subcols,             &
         delta_z1            => domain%delta_z(1),                  &

         POC                 => marbl_particulate_share%POC,        &
         POP                 => marbl_particulate_share%POP,        &
         P_CaCO3             => marbl_particulate_share%P_CaCO3,    &
         P_CaCO3_ALT_CO2     => marbl_particulate_share%P_CaCO3_ALT_CO2,&
         P_SiO2              => marbl_particulate_share%P_SiO2,     &
         dust                => marbl_particulate_share%dust,       &
         P_iron              => marbl_particulate_share%P_iron,     &

         ph_prev_col         => saved_state%state(saved_state_ind%ph_col)%field_3d(:,1), &
         ph_prev_alt_co2_col => saved_state%state(saved_state_ind%ph_alt_co2_col)%field_3d(:,1), &

         ! Hard-coding in that there is only 1 column passed in at a time!
         dust_flux_in        => interior_tendency_forcings(interior_tendency_forcing_indices%dustflux_id)%field_0d(1),   &
         potemp              => interior_tendency_forcings(interior_tendency_forcing_indices%potemp_id)%field_1d(1,:),   &
         pressure            => interior_tendency_forcings(interior_tendency_forcing_indices%pressure_id)%field_1d(1,:), &
         salinity            => interior_tendency_forcings(interior_tendency_forcing_indices%salinity_id)%field_1d(1,:), &
         fesedflux           => interior_tendency_forcings(interior_tendency_forcing_indices%fesedflux_id)%field_1d(1,:),&

         po4_ind           => marbl_tracer_indices%po4_ind,         &
         no3_ind           => marbl_tracer_indices%no3_ind,         &
         sio3_ind          => marbl_tracer_indices%sio3_ind,        &
         nh4_ind           => marbl_tracer_indices%nh4_ind,         &
         fe_ind            => marbl_tracer_indices%fe_ind,          &
         lig_ind           => marbl_tracer_indices%lig_ind,   &
         o2_ind            => marbl_tracer_indices%o2_ind,          &
         dic_ind           => marbl_tracer_indices%dic_ind,         &
         dic_alt_co2_ind   => marbl_tracer_indices%dic_alt_co2_ind, &
         doc_ind           => marbl_tracer_indices%doc_ind,         &
         don_ind           => marbl_tracer_indices%don_ind,         &
         dop_ind           => marbl_tracer_indices%dop_ind,         &
         dopr_ind          => marbl_tracer_indices%dopr_ind,        &
         donr_ind          => marbl_tracer_indices%donr_ind,        &
         docr_ind          => marbl_tracer_indices%docr_ind         &
         )

    !-----------------------------------------------------------------------
    !  Compute in situ temp
    !  displace surface parcel with temp potemp to pressure
    !-----------------------------------------------------------------------

    surf_press(1:kmt) = c0
    temperature(1:kmt) = marbl_temperature_potemp(kmt, salinity, potemp, surf_press, pressure)
    temperature(kmt+1:km) = c0

    !-----------------------------------------------------------------------
    !  local copies of optional forcings
    !  these are not handled via associate block because their forcing indices might be zero
    !-----------------------------------------------------------------------

    if (lo2_consumption_scalef) then
       o2_consumption_scalef(:) = &
            interior_tendency_forcings(interior_tendency_forcing_indices%o2_consumption_scalef_id)%field_1d(1,:)
    else
       o2_consumption_scalef(:) = c1
    endif

    if (lp_remin_scalef) then
       p_remin_scalef(:) = &
            interior_tendency_forcings(interior_tendency_forcing_indices%p_remin_scalef_id)%field_1d(1,:)
    else
       p_remin_scalef(:) = c1
    endif

    !-----------------------------------------------------------------------
    !  Compute adjustment to tendencies due to tracer restoring
    !-----------------------------------------------------------------------

    call marbl_restore_compute_interior_restore(            &
               tracers,                                     &
               interior_tendency_forcings,                  &
               interior_tendency_forcing_indices,           &
               interior_restore)

    !-----------------------------------------------------------------------
    !  create local copies of model tracers
    !-----------------------------------------------------------------------

    call setup_local_tracers(kmt, marbl_tracer_indices, tracers(:,:), autotroph_local, &
         tracer_local(:,:), zooplankton_local, totalChl_local)

    call set_surface_particulate_terms(surface_flux_forcing_indices, POC, POP, P_CaCO3, &
         P_CaCO3_ALT_CO2, P_SiO2, dust, P_iron, QA_dust_def(:), dust_flux_in)

    call marbl_timers%start(marbl_timer_indices%carbonate_chem_id,            &
                            marbl_status_log)
    call compute_carbonate_chemistry(domain, temperature, pressure,           &
         salinity, tracer_local(:, :), marbl_tracer_indices, carbonate,       &
         ph_prev_col(:), ph_prev_alt_co2_col(:), marbl_status_log)
    call marbl_timers%stop(marbl_timer_indices%carbonate_chem_id,             &
                            marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace(&
            'compute_carbonate_chemistry()', subname)
       return
    end if

    call compute_PAR(domain, interior_tendency_forcings, interior_tendency_forcing_indices, &
                     totalChl_local, PAR)

    call compute_autotroph_elemental_ratios(km, autotroph_local, marbl_tracer_indices, tracer_local, &
         autotroph_derived_terms)

    call compute_function_scaling(temperature, Tfunc)

    call compute_Pprime(km, domain%zt, autotroph_local, temperature, autotroph_derived_terms%Pprime)

    call compute_autotroph_uptake(km, marbl_tracer_indices, tracer_local(:, :), autotroph_derived_terms)

    call compute_autotroph_photosynthesis(km, num_PAR_subcols, autotroph_local, temperature(:), &
         Tfunc(:), PAR%col_frac(:), PAR%avg(:,:), autotroph_derived_terms)

    call compute_autotroph_nutrient_uptake(marbl_tracer_indices, autotroph_derived_terms)

    call compute_autotroph_calcification(km, autotroph_local, temperature, autotroph_derived_terms)

    call compute_autotroph_nfixation(km, autotroph_derived_terms)

    call compute_autotroph_loss(km, Tfunc, autotroph_derived_terms)

    call compute_Zprime(km, domain%zt, zooplankton_local%C, Tfunc, zooplankton_derived_terms)

    call compute_grazing(km, Tfunc, zooplankton_local, zooplankton_derived_terms, autotroph_derived_terms)

    call compute_routing(km, zooplankton_derived_terms, autotroph_derived_terms)

    call compute_dissolved_organic_matter (km, marbl_tracer_indices, num_PAR_subcols, &
         PAR, zooplankton_derived_terms, autotroph_derived_terms,   &
         delta_z1, tracer_local(:, :), dissolved_organic_matter)

    do k = 1, km

       call compute_scavenging(k, km, marbl_tracer_indices, tracer_local(:,:), &
            POC, P_CaCO3, P_SiO2, dust, Fefree(:), Fe_scavenge_rate(:), &
            Fe_scavenge(:), Lig_scavenge(:), marbl_status_log)

       if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace('compute_scavenging()', subname)
          return
       end if

       call compute_large_detritus_prod(k, domain, marbl_tracer_indices, zooplankton_derived_terms, &
            autotroph_derived_terms, Fe_scavenge(k),                    &
            POC, POP, P_CaCO3, P_CaCO3_ALT_CO2, P_SiO2, dust, P_iron,   &
            dissolved_organic_matter%DOP_loss_P_bal(k), marbl_status_log)

       ! FIXME #28: need to pull particulate share out
       !            of compute_particulate_terms!
       call compute_particulate_terms(k, domain,                   &
            marbl_particulate_share, p_remin_scalef(k),            &
            POC, POP, P_CaCO3, P_CaCO3_ALT_CO2,                    &
            P_SiO2, dust, P_iron, PON_remin(k), PON_sed_loss(k),   &
            QA_dust_def(k),                                        &
            tracer_local(:, k), carbonate, sed_denitrif(k),        &
            other_remin(k), fesedflux(k), marbl_tracer_indices,    &
            glo_avg_fields_interior_tendency, marbl_status_log)

       if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace('compute_particulate_terms()', subname)
          return
       end if

       if  (k < km) then
          call update_particulate_terms_from_prior_level(k+1, POC, POP, P_CaCO3, &
               P_CaCO3_ALT_CO2, P_SiO2, dust, P_iron, QA_dust_def(:))
       endif

    end do ! k

    call compute_Lig_terms(km, num_PAR_subcols, marbl_tracer_indices, &
         POC%remin(:), dissolved_organic_matter%DOC_prod(:), PAR, delta_z1, tracer_local, Lig_scavenge(:), &
         autotroph_derived_terms%photoFe(:,:), Lig_prod(:), Lig_photochem(:), Lig_deg(:), Lig_loss(:))

    call compute_nitrif(kmt, km, num_PAR_subcols, marbl_tracer_indices, PAR, tracer_local(:,:), nitrif(:))

    call compute_denitrif(km, marbl_tracer_indices, tracer_local(:, :), &
         dissolved_organic_matter%DOC_remin(:), &
         dissolved_organic_matter%DOCr_remin(:), &
         POC%remin(:), other_remin(:), sed_denitrif(:), denitrif(:))

    call compute_local_tendencies(km, marbl_tracer_indices, autotroph_derived_terms, &
         zooplankton_derived_terms, &
         dissolved_organic_matter, &
         nitrif(:), denitrif(:), sed_denitrif(:), &
         Fe_scavenge(:), Lig_prod(:), Lig_loss(:), &
         P_iron%remin(:), POC%remin(:), POP%remin(:), &
         P_SiO2%remin(:), P_CaCO3%remin(:), P_CaCO3_ALT_CO2%remin(:), &
         other_remin(:), PON_remin(:), &
         tracer_local(:,:), &
         o2_consumption_scalef(:), &
         o2_production(:), o2_consumption(:), &
         interior_tendencies(:,:))

    ! Compute interior diagnostics
    call marbl_diagnostics_interior_tendency_compute(       &
         domain,                                            &
         interior_tendency_forcing_indices,                 &
         interior_tendency_forcings,                        &
         temperature,                                       &
         interior_tendencies,                               &
         marbl_tracer_indices,                              &
         carbonate,                                         &
         autotroph_local,                                   &
         autotroph_derived_terms,                           &
         zooplankton_derived_terms,                         &
         dissolved_organic_matter,                          &
         marbl_particulate_share,                           &
         PAR,                                               &
         PON_remin, PON_sed_loss,                           &
         sed_denitrif, other_remin, nitrif, denitrif,       &
         tracers(o2_ind, :), o2_production, o2_consumption, &
         fe_scavenge, fe_scavenge_rate,                     &
         Lig_prod, Lig_loss, Lig_scavenge, Fefree,          &
         Lig_photochem, Lig_deg,                            &
         interior_restore,                                  &
         interior_tendency_diags,                           &
         marbl_status_log)
    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace(&
            'marbl_diagnostics_interior_tendency_compute()', subname)
       return
    end if

    !-----------------------------------------------------------------------
    !  Compute time derivatives for ecosystem carbon isotope state variables
    !-----------------------------------------------------------------------

    ! Store any variables needed in other tracer modules
    ! FIXME #28: need to pull particulate share out
    !            of compute_particulate_terms!
    call marbl_interior_tendency_share_export_variables(km, marbl_tracer_indices, &
        tracer_local(:,:), carbonate, dissolved_organic_matter,                   &
        QA_dust_def(:), interior_tendency_share)

    call marbl_interior_tendency_share_export_zooplankton(zooplankton_local, &
         zooplankton_derived_terms, zooplankton_share)

    ! call marbl_ciso_interior_tendency_compute()
    call marbl_ciso_interior_tendency_compute(                  &
         marbl_domain            = domain,                      &
         interior_tendency_share = interior_tendency_share,     &
         zooplankton_share = zooplankton_share,                 &
         marbl_particulate_share = marbl_particulate_share,     &
         autotroph_derived_terms = autotroph_derived_terms,     &
         tracer_local            = tracer_local,                &
         autotroph_local         = autotroph_local,             &
         temperature             = temperature,                 &
         marbl_tracer_indices    = marbl_tracer_indices,        &
         interior_tendencies     = interior_tendencies,         &
         marbl_interior_diags    = interior_tendency_diags,     &
         marbl_status_log        = marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace(&
            'marbl_ciso_interior_tendency_compute()', subname)
       return
    end if

    end associate

    ! ADD RESTORING
    interior_tendencies = interior_tendencies + interior_restore

  end subroutine marbl_interior_tendency_compute

  !***********************************************************************

  subroutine compute_PAR(domain, interior_tendency_forcings, interior_tendency_forcing_ind, &
                         totalChl_local, PAR)

    !-----------------------------------------------------------------------
    !  compute PAR related quantities
    !  Morel, Maritorena, JGR, Vol 106, No. C4, pp 7163--7180, 2001
    !  0.45   fraction of incoming SW -> PAR (non-dim)
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : f_qsw_par

    ! PAR is intent(inout) because it components, while entirely set here, are allocated elsewhere

    type(marbl_domain_type)                   , intent(in)    :: domain
    type(marbl_forcing_fields_type)           , intent(in)    :: interior_tendency_forcings(:)
    type(marbl_interior_tendency_forcing_indexing_type), intent(in) :: interior_tendency_forcing_ind
    real(r8)                                  , intent(in)    :: totalChl_local(:)
    type(marbl_PAR_type)                      , intent(inout) :: PAR

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    ! PAR below this threshold is changed to 0.0
    ! tendencies from PAR below this threshold are small enough to not affect tracer values
    real (r8), parameter :: PAR_threshold = 1.0e-19_r8

    real (r8) :: WORK1(domain%kmt)
    integer(int_kind) :: k, subcol_ind
    !-----------------------------------------------------------------------

    associate(                                        &
         dkm              => domain%km,               &
         column_kmt       => domain%kmt,              &
         delta_z          => domain%delta_z,          &
         PAR_nsubcols     => domain%num_PAR_subcols   &
         )

      !-----------------------------------------------------------------------
      ! set depth independent quantities, sub-column fractions and PAR at surface
      ! ignore provided shortwave where col_frac == 0
      !-----------------------------------------------------------------------

      if (interior_tendency_forcing_ind%PAR_col_frac_id .ne. 0) then
        PAR%col_frac(:) = interior_tendency_forcings(interior_tendency_forcing_ind%PAR_col_frac_id)%field_1d(1,:)
      else
        PAR%col_frac(:) = c1
      end if

      where (PAR%col_frac(:) > c0)
        PAR%interface(0,:) = f_qsw_par &
                           * interior_tendency_forcings(interior_tendency_forcing_ind%surf_shortwave_id)%field_1d(1,:)
      elsewhere
        PAR%interface(0,:) = c0
      end where

      where (PAR%interface(0,:) < PAR_threshold)
        PAR%interface(0,:) = c0
      end where

      !-----------------------------------------------------------------------
      ! avoid further computations, such as computing attenuation coefficient, if there is no light
      ! treat forcing as a single dark value, by setting col_frac(1) to 1
      !-----------------------------------------------------------------------

      if (all(PAR%interface(0,:) == c0)) then
        PAR%col_frac(:)    = c0
        PAR%col_frac(1)    = c1
        PAR%interface(:,:) = c0
        PAR%avg(:,:)       = c0
        PAR%KPARdz(:)      = c0
        return
      end if

      !-----------------------------------------------------------------------
      ! compute attenuation coefficient over column
      !-----------------------------------------------------------------------

      WORK1(:) = max(totalChl_local(1:column_kmt), 0.02_r8)
      do k = 1, column_kmt
        if (WORK1(k) < 0.13224_r8) then
          PAR%KPARdz(k) = 0.000919_r8*(WORK1(k)**0.3536_r8)
        else
          PAR%KPARdz(k) = 0.001131_r8*(WORK1(k)**0.4562_r8)
        end if
        PAR%KPARdz(k) = PAR%KPARdz(k) * delta_z(k)
      end do
      PAR%KPARdz(column_kmt+1:dkm) = c0

      !-----------------------------------------------------------------------
      ! propagate PAR values through column, only on subcolumns with PAR>0
      ! note that if col_frac is 0, then so is PAR
      !-----------------------------------------------------------------------

      WORK1(:) = exp(-PAR%KPARdz(1:column_kmt))

      do subcol_ind = 1, PAR_nsubcols
        if (PAR%interface(0,subcol_ind) > c0) then

          ! this loop will probably not vectorize
          do k = 1, column_kmt
            PAR%interface(k,subcol_ind) = PAR%interface(k-1,subcol_ind) * WORK1(k)
            if (PAR%interface(k,subcol_ind) < PAR_threshold) then
              PAR%interface(k:column_kmt,subcol_ind) = c0
              exit
            end if
          end do
          PAR%interface(column_kmt+1:dkm,subcol_ind) = c0

          do k = 1, column_kmt
            PAR%avg(k,subcol_ind) = PAR%interface(k-1,subcol_ind) * (c1 - WORK1(k)) / PAR%KPARdz(k)
          end do
          PAR%avg(column_kmt+1:dkm,subcol_ind) = c0
        else
          PAR%interface(1:dkm,subcol_ind) = c0
          PAR%avg(1:dkm,subcol_ind) = c0
        end if
      end do
    end associate

  end subroutine compute_PAR

  !***********************************************************************

  subroutine marbl_interior_tendency_adjust_bury_coeff(marbl_particulate_share, &
       glo_avg_rmean_interior_tendency, glo_avg_rmean_surface_flux, &
       glo_scalar_rmean_interior_tendency, glo_scalar_interior_tendency)

    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_CaCO3_bury
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_POC_bury
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_POP_bury
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_bSi_bury
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_C_input
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_P_input
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_Si_input
    use marbl_glo_avg_mod, only : glo_scalar_ind_interior_tendency_POC_bury_coeff
    use marbl_glo_avg_mod, only : glo_scalar_ind_interior_tendency_POP_bury_coeff
    use marbl_glo_avg_mod, only : glo_scalar_ind_interior_tendency_bSi_bury_coeff

    type (marbl_particulate_share_type), intent(inout) :: marbl_particulate_share
    type (marbl_running_mean_0d_type)  , intent(in)    :: glo_avg_rmean_interior_tendency(:)
    type (marbl_running_mean_0d_type)  , intent(in)    :: glo_avg_rmean_surface_flux(:)
    type (marbl_running_mean_0d_type)  , intent(in)    :: glo_scalar_rmean_interior_tendency(:)
    real (r8)                          , intent(inout) :: glo_scalar_interior_tendency(:)

    !-----------------------------------------------------------------------

    if (.not. ladjust_bury_coeff) return

    associate( &
         POC_bury_coeff => marbl_particulate_share%POC_bury_coeff, &
         POP_bury_coeff => marbl_particulate_share%POP_bury_coeff, &
         bSi_bury_coeff => marbl_particulate_share%bSi_bury_coeff, &

         rmean_CaCO3_bury_avg => glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_CaCO3_bury)%rmean, &
         rmean_POC_bury_avg   => glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_POC_bury)%rmean, &
         rmean_POP_bury_avg   => glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_POP_bury)%rmean, &
         rmean_bSi_bury_avg   => glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_bSi_bury)%rmean, &

         rmean_POC_bury_deriv_avg => &
              glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff)%rmean, &
         rmean_POP_bury_deriv_avg => &
              glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff)%rmean, &
         rmean_bSi_bury_deriv_avg => &
              glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff)%rmean, &

         rmean_C_input_avg  => glo_avg_rmean_surface_flux(glo_avg_field_ind_surface_flux_C_input)%rmean, &
         rmean_P_input_avg  => glo_avg_rmean_surface_flux(glo_avg_field_ind_surface_flux_P_input)%rmean, &
         rmean_Si_input_avg => glo_avg_rmean_surface_flux(glo_avg_field_ind_surface_flux_Si_input)%rmean, &

         rmean_POC_bury_coeff => &
              glo_scalar_rmean_interior_tendency(glo_scalar_ind_interior_tendency_POC_bury_coeff)%rmean, &
         rmean_POP_bury_coeff => &
              glo_scalar_rmean_interior_tendency(glo_scalar_ind_interior_tendency_POP_bury_coeff)%rmean, &
         rmean_bSi_bury_coeff => &
              glo_scalar_rmean_interior_tendency(glo_scalar_ind_interior_tendency_bSi_bury_coeff)%rmean &
    )

      ! Newton's method for POC_bury(coeff) + CaCO3_bury - C_input = 0
      POC_bury_coeff = rmean_POC_bury_coeff &
                     - (rmean_POC_bury_avg + rmean_CaCO3_bury_avg - rmean_C_input_avg) &
                     / rmean_POC_bury_deriv_avg

      ! Newton's method for POP_bury(coeff) - P_input = 0
      POP_bury_coeff = rmean_POP_bury_coeff &
                     - (rmean_POP_bury_avg - rmean_P_input_avg) / rmean_POP_bury_deriv_avg

      ! Newton's method for bSi_bury(coeff) - Si_input = 0
      bSi_bury_coeff = rmean_bSi_bury_coeff &
                     - (rmean_bSi_bury_avg - rmean_Si_input_avg) / rmean_bSi_bury_deriv_avg

      ! copy computed bury coefficients into output argument
      glo_scalar_interior_tendency(glo_scalar_ind_interior_tendency_POC_bury_coeff) = POC_bury_coeff
      glo_scalar_interior_tendency(glo_scalar_ind_interior_tendency_POP_bury_coeff) = POP_bury_coeff
      glo_scalar_interior_tendency(glo_scalar_ind_interior_tendency_bSi_bury_coeff) = bSi_bury_coeff

    end associate

  end subroutine marbl_interior_tendency_adjust_bury_coeff

  !***********************************************************************

  subroutine setup_local_tracers(column_kmt, marbl_tracer_indices, tracers, &
       autotroph_local, tracer_local, zooplankton_local, totalChl_local)

    !-----------------------------------------------------------------------
    !  create local copies of model tracers
    !  treat negative values as zero,  apply mask to local copies
    !-----------------------------------------------------------------------

    integer(int_kind)            , intent(in)    :: column_kmt
    type(marbl_tracer_index_type), intent(in)    :: marbl_tracer_indices
    real (r8)                    , intent(in)    :: tracers(:,:)
    type(autotroph_local_type)   , intent(inout) :: autotroph_local
    real (r8)                    , intent(out)   :: tracer_local(:,:)
    type(zooplankton_local_type) , intent(inout) :: zooplankton_local
    real (r8)                    , intent(out)   :: totalChl_local(:)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: k                    ! vertical loop index
    integer (int_kind) :: zoo_ind, auto_ind, n ! tracer indices
    !-----------------------------------------------------------------------

    do k = 1, column_kmt
      tracer_local(:,k) = max(c0, tracers(1:size(tracer_local,1),k))
    end do

    do k = column_kmt+1, size(tracer_local,2)
      tracer_local(:,k) = c0
    end do

    !-----------------------------------------------------------------------
    ! populate zooplankton specific arrays from tracer_local
    !-----------------------------------------------------------------------

    do zoo_ind = 1, zooplankton_cnt
      n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
      zooplankton_local%C(zoo_ind,:) = tracer_local(n,:)
    end do

    !-----------------------------------------------------------------------
    ! populate autotroph specific arrays from tracer_local
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
      n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
      autotroph_local%Chl(auto_ind,:) = tracer_local(n,:)

      n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
      autotroph_local%C(auto_ind,:) = tracer_local(n,:)

      n = marbl_tracer_indices%auto_inds(auto_ind)%P_ind
      if (n > 0) then
        autotroph_local%P(auto_ind,:) = tracer_local(n,:)
      else
        autotroph_local%P(auto_ind,:) = autotroph_settings(auto_ind)%Qp_fixed * autotroph_local%C(auto_ind,:)
      end if

      n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
      autotroph_local%Fe(auto_ind,:) = tracer_local(n,:)

      n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
      if (n > 0) then
        autotroph_local%Si(auto_ind,:) = tracer_local(n,:)
      end if

      n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
      if (n > 0) then
        autotroph_local%CaCO3(auto_ind,:) = tracer_local(n,:)
      end if

      ! Carbon isotope elements of autotroph_local
      n = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
      if (n > 0) then
        autotroph_local%C13(auto_ind,:) = tracer_local(n,:)
      end if
      n = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
      if (n > 0) then
        autotroph_local%C14(auto_ind,:) = tracer_local(n,:)
      end if

      n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
      if (n > 0) then
        autotroph_local%Ca13CO3(auto_ind,:) = tracer_local(n,:)
      end if

      n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
      if (n > 0) then
        autotroph_local%Ca14CO3(auto_ind,:) = tracer_local(n,:)
      end if
    end do

    ! autotroph consistency check
    call autotroph_zero_consistency_enforce(column_kmt, marbl_tracer_indices, autotroph_local)

    ! set totalChl_local
    totalChl_local = sum(autotroph_local%Chl(:,:), dim=1)

  end subroutine setup_local_tracers

  !***********************************************************************

  subroutine set_surface_particulate_terms(surface_flux_forcing_indices, POC, POP, P_CaCO3, &
       P_CaCO3_ALT_CO2, P_SiO2, dust, P_iron, QA_dust_def, NET_DUST_IN)

    !  Set incoming fluxes (put into outgoing flux for first level usage).
    !  Set dissolution length, production fraction and mass terms.
    !
    !  The first 6 arguments are intent(inout) in
    !  order to preserve contents on other blocks.

    ! !USES:

    use marbl_settings_mod, only : parm_POC_diss
    use marbl_settings_mod, only : parm_CaCO3_diss
    use marbl_settings_mod, only : parm_CaCO3_gamma
    use marbl_settings_mod, only : parm_hPOC_CaCO3_ratio
    use marbl_settings_mod, only : parm_SiO2_diss
    use marbl_settings_mod, only : parm_SiO2_gamma
    use marbl_settings_mod, only : parm_hPOC_SiO2_ratio
    use marbl_settings_mod, only : parm_hPOC_dust_ratio

    real (r8)                          , intent(in)    :: net_dust_in     ! dust flux
    type(marbl_surface_flux_forcing_indexing_type), intent(in)   :: surface_flux_forcing_indices
    type(column_sinking_particle_type) , intent(inout) :: POC             ! base units = nmol C
    type(column_sinking_particle_type) , intent(inout) :: POP             ! base units = nmol P
    type(column_sinking_particle_type) , intent(inout) :: P_CaCO3         ! base units = nmol CaCO3
    type(column_sinking_particle_type) , intent(inout) :: P_CaCO3_ALT_CO2 ! base units = nmol CaCO3
    type(column_sinking_particle_type) , intent(inout) :: P_SiO2          ! base units = nmol SiO2
    type(column_sinking_particle_type) , intent(inout) :: dust            ! base units = g
    type(column_sinking_particle_type) , intent(inout) :: P_iron          ! base units = nmol Fe
    real (r8)                          , intent(inout) :: QA_dust_def(:)  ! incoming deficit in the QA(dust) POC flux (km)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: ksurf

    !-----------------------------------------------------------------------
    !  parameters, from Armstrong et al. 2000
    !
    !  July 2002, length scale for excess POC and bSI modified by temperature
    !  Value given here is at Tref of 30 degC, JKM
    !-----------------------------------------------------------------------

    POC%diss      = parm_POC_diss   ! diss. length (cm), modified by TEMP
    POC%gamma     = c0              ! not used
    POC%mass      = 12.01_r8        ! molecular weight of C
    POC%rho       = c0              ! not used

    POP%diss      = parm_POC_diss   ! not used
    POP%gamma     = c0              ! not used
    POP%mass      = 30.974_r8       ! molecular weight of P
    POP%rho       = c0              ! not used

    P_CaCO3%diss  = parm_CaCO3_diss ! diss. length (cm)
    P_CaCO3%gamma = parm_CacO3_gamma! prod frac -> hard subclass
    P_CaCO3%mass  = 100.09_r8       ! molecular weight of CaCO
    P_CaCO3%rho   = parm_hPOC_CaCO3_ratio * P_CaCO3%mass / POC%mass ! QA mass ratio for CaCO3

    P_CaCO3_ALT_CO2%diss  = P_CaCO3%diss
    P_CaCO3_ALT_CO2%gamma = P_CaCO3%gamma
    P_CaCO3_ALT_CO2%mass  = P_CaCO3%mass
    P_CaCO3_ALT_CO2%rho   = P_CaCO3%rho

    P_SiO2%diss   = parm_SiO2_diss  ! diss. length (cm), modified by TEMP
    P_SiO2%gamma  = parm_SiO2_gamma ! prod frac -> hard subclass
    P_SiO2%mass   = 60.08_r8        ! molecular weight of SiO2
    P_SiO2%rho    = parm_hPOC_SiO2_ratio * P_SiO2%mass / POC%mass ! QA mass ratio for SiO2

    dust%diss     = 40000.0_r8      ! diss. length (cm)
    dust%gamma    = 0.98_r8         ! prod frac -> hard subclass
    dust%mass     = 1.0e9_r8        ! base units are already grams
    dust%rho      = parm_hPOC_dust_ratio * dust%mass / POC%mass ! QA mass ratio for dust

    P_iron%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron%mass   = c0              ! not used
    P_iron%rho    = c0              ! not used

    !-----------------------------------------------------------------------
    !  Set incoming fluxes
    !-----------------------------------------------------------------------

    ksurf = 1

    P_CaCO3%sflux_in(ksurf) = c0
    P_CaCO3%hflux_in(ksurf) = c0

    P_CaCO3_ALT_CO2%sflux_in(ksurf) = c0
    P_CaCO3_ALT_CO2%hflux_in(ksurf) = c0

    P_SiO2%sflux_in(ksurf) = c0
    P_SiO2%hflux_in(ksurf) = c0

    if (surface_flux_forcing_indices%dust_flux_id.ne.0) then
      dust%sflux_in(ksurf) = (c1 - dust%gamma) * net_dust_in
      dust%hflux_in(ksurf) = dust%gamma * net_dust_in
    else
      dust%sflux_in(ksurf) = c0
      dust%hflux_in(ksurf) = c0
    end if

    P_iron%sflux_in(ksurf) = c0
    P_iron%hflux_in(ksurf) = c0

    !-----------------------------------------------------------------------
    !  Hard POC is QA flux and soft POC is excess POC.
    !-----------------------------------------------------------------------

    POC%sflux_in(ksurf) = c0
    POC%hflux_in(ksurf) = c0

    POP%sflux_in(ksurf) = c0
    POP%hflux_in(ksurf) = c0

    !-----------------------------------------------------------------------
    !  Compute initial QA(dust) POC flux deficit.
    !-----------------------------------------------------------------------

    QA_dust_def(ksurf) = dust%rho * (dust%sflux_in(ksurf) + dust%hflux_in(ksurf))

  end subroutine set_surface_particulate_terms

  !***********************************************************************

  subroutine compute_carbonate_chemistry(domain, temperature, press_bar, &
       salinity, tracer_local, marbl_tracer_indices, carbonate, ph_prev_col,   &
       ph_prev_alt_co2_col, marbl_status_log)

    use marbl_co2calc_mod, only : marbl_co2calc_interior
    use marbl_co2calc_mod, only : marbl_co2calc_co3_sat_vals
    use marbl_co2calc_mod, only : co2calc_coeffs_type
    use marbl_co2calc_mod, only : co2calc_state_type

    type(marbl_domain_type),       intent(in)    :: domain
    real (r8),                     intent(in)    :: temperature(:)
    real (r8),                     intent(in)    :: press_bar(:)
    real (r8),                     intent(in)    :: salinity(:)
    real (r8),                     intent(in)    :: tracer_local(:,:)       ! local copies of model tracer concentrations
    type(marbl_tracer_index_type), intent(in)    :: marbl_tracer_indices
    type(carbonate_type),          intent(inout) :: carbonate
    real(r8),                      intent(inout) :: ph_prev_col(:)          ! km
    real(r8),                      intent(inout) :: ph_prev_alt_co2_col(:)  ! km
    type(marbl_log_type),          intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_interior_tendency_mod:compute_carbonate_chemistry'

    integer :: k
    type(co2calc_coeffs_type), dimension(domain%km) :: co2calc_coeffs
    type(co2calc_state_type),  dimension(domain%km) :: co2calc_state
    real(r8),                  dimension(domain%km) :: ph_lower_bound
    real(r8),                  dimension(domain%km) :: ph_upper_bound
    real(r8),                  dimension(domain%km) :: dic_loc
    real(r8),                  dimension(domain%km) :: dic_alt_co2_loc
    real(r8),                  dimension(domain%km) :: alk_loc
    real(r8),                  dimension(domain%km) :: alk_alt_co2_loc
    real(r8),                  dimension(domain%km) :: po4_loc
    real(r8),                  dimension(domain%km) :: sio3_loc

    ! make local copies instead of using associate construct because of gnu fortran bug
    ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=68546
    dic_loc         = tracer_local(marbl_tracer_indices%dic_ind,:)
    dic_alt_co2_loc = tracer_local(marbl_tracer_indices%dic_alt_co2_ind,:)
    alk_loc         = tracer_local(marbl_tracer_indices%alk_ind,:)
    alk_alt_co2_loc = tracer_local(marbl_tracer_indices%alk_alt_co2_ind,:)
    po4_loc         = tracer_local(marbl_tracer_indices%po4_ind,:)
    sio3_loc        = tracer_local(marbl_tracer_indices%sio3_ind,:)

    associate(                                                         &
              dkm               => domain%km,                          &
              column_kmt        => domain%kmt,                         &
              co3               => carbonate%CO3(:),                   &
              hco3              => carbonate%HCO3(:),                  &
              h2co3             => carbonate%H2CO3(:),                 &
              ph                => carbonate%pH(:),                    &
              co3_sat_calcite   => carbonate%CO3_sat_calcite(:),       &
              co3_sat_aragonite => carbonate%CO3_sat_aragonite(:),     &
              co3_alt_co2       => carbonate%CO3_ALT_CO2(:),           &
              hco3_alt_co2      => carbonate%HCO3_ALT_CO2(:),          &
              h2co3_alt_co2     => carbonate%H2CO3_ALT_CO2(:),         &
              ph_alt_co2        => carbonate%pH_ALT_CO2(:)             &
              )

      do k=1,dkm
        if (ph_prev_col(k)  /= c0) then
          ph_lower_bound(k) = ph_prev_col(k) - del_ph
          ph_upper_bound(k) = ph_prev_col(k) + del_ph
        else
          ph_lower_bound(k) = phlo_3d_init
          ph_upper_bound(k) = phhi_3d_init
        end if
      end do

      call marbl_co2calc_interior(&
           dkm, column_kmt, .true., co2calc_coeffs, co2calc_state, &
           temperature, salinity, press_bar, dic_loc, alk_loc, po4_loc, sio3_loc,    &
           ph_lower_bound, ph_upper_bound, ph, h2co3, hco3, co3, marbl_status_log)

      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace('marbl_co2calc_interior() with dic', subname)
        return
      end if

      if (marbl_status_log%lwarning) then
        call marbl_status_log%log_warning_trace('marbl_co2calc_interior() with dic', subname)
      end if

      do k=1,dkm
        ph_prev_col(k) = pH(k)
        if (ph_prev_alt_co2_col(k) /= c0) then
          ph_lower_bound(k) = ph_prev_alt_co2_col(k) - del_ph
          ph_upper_bound(k) = ph_prev_alt_co2_col(k) + del_ph
        else
          ph_lower_bound(k) = phlo_3d_init
          ph_upper_bound(k) = phhi_3d_init
        end if
      end do

      call marbl_co2calc_interior(&
           dkm, column_kmt, .false., co2calc_coeffs, co2calc_state,     &
           temperature, salinity, press_bar, dic_alt_co2_loc, alk_alt_co2_loc, po4_loc,   &
           sio3_loc, ph_lower_bound, ph_upper_bound, ph_alt_co2, h2co3_alt_co2,           &
           hco3_alt_co2, co3_alt_co2, marbl_status_log)

      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace('marbl_co2calc_interior() with dic_alt_co2', subname)
! NOTE ignore problems with ALT_CO2 for now
        marbl_status_log%labort_marbl = .false.
        return
      end if

      if (marbl_status_log%lwarning) then
        call marbl_status_log%log_warning_trace('marbl_co2calc_interior() with dic_alt_co2', subname)
      end if

      ph_prev_alt_co2_col = ph_alt_co2

      call marbl_co2calc_co3_sat_vals(&
           dkm, column_kmt, temperature, salinity, &
           press_bar, co3_sat_calcite, co3_sat_aragonite)

    end associate

  end subroutine compute_carbonate_chemistry

  !***********************************************************************

  subroutine autotroph_zero_consistency_enforce(column_kmt, marbl_tracer_indices, autotroph_local)

    use marbl_ciso_interior_tendency_mod, only : marbl_ciso_interior_tendency_autotroph_zero_consistency_enforce

    !-----------------------------------------------------------------------
    !  If any phyto box are zero, set others to zeros.
    !-----------------------------------------------------------------------

    integer(int_kind),             intent(in)    :: column_kmt ! number of active model layers
    type(marbl_tracer_index_type), intent(in)    :: marbl_tracer_indices
    type(autotroph_local_type),    intent(inout) :: autotroph_local

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: auto_ind, k
    logical (log_kind) :: zero_mask(column_kmt)
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
      do k = 1, column_kmt

        zero_mask(k) = (autotroph_local%Chl(auto_ind,k) == c0 &
                        .or. autotroph_local%C(auto_ind,k) == c0 &
                        .or. autotroph_local%P(auto_ind,k) == c0 &
                        .or. autotroph_local%Fe(auto_ind,k)  == c0)

        if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
          zero_mask(k) = zero_mask(k) .or. autotroph_local%Si(auto_ind,k) == c0
        end if
      end do
      where (zero_mask)
        autotroph_local%Chl(auto_ind,1:column_kmt) = c0
        autotroph_local%C(auto_ind,1:column_kmt) = c0
        autotroph_local%P(auto_ind,1:column_kmt) = c0
        autotroph_local%Fe(auto_ind,1:column_kmt) = c0
      end where
      if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
        where (zero_mask)
          autotroph_local%Si(auto_ind,1:column_kmt) = c0
        end where
      end if
      if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
        where (zero_mask)
          autotroph_local%CaCO3(auto_ind,1:column_kmt) = c0
        end where
      end if

      ! carbon isotope components of autotroph_local_type
      ! FIXME #278: this interface will change when logical checks are not index-based
      call marbl_ciso_interior_tendency_autotroph_zero_consistency_enforce(auto_ind, column_kmt, zero_mask, &
           marbl_tracer_indices%auto_inds(auto_ind), autotroph_local)
    end do

  end subroutine autotroph_zero_consistency_enforce

  !***********************************************************************

  subroutine compute_autotroph_elemental_ratios(km, autotroph_local, marbl_tracer_indices, tracer_local, &
       autotroph_derived_terms)

    use marbl_constants_mod, only : epsC
    use marbl_settings_mod , only : lvariable_PtoC
    use marbl_settings_mod , only : gQsi_0
    use marbl_settings_mod , only : gQsi_max
    use marbl_settings_mod , only : gQsi_min
    use marbl_settings_mod , only : gQ_Fe_kFe_thres
    use marbl_settings_mod , only : gQ_Si_kSi_thres
    use marbl_settings_mod , only : PquotaSlope, PquotaIntercept, PquotaMinNP

    integer,                            intent(in)    :: km
    type(autotroph_local_type),         intent(in)    :: autotroph_local
    type(marbl_tracer_index_type),      intent(in)    :: marbl_tracer_indices
    real (r8),                          intent(in)    :: tracer_local(marbl_tracer_indices%total_cnt,km)
    type(autotroph_derived_terms_type), intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                                        &
         PO4_loc    => tracer_local(marbl_tracer_indices%po4_ind,:),  &
         Fe_loc     => tracer_local(marbl_tracer_indices%fe_ind,:),   &
         SiO3_loc   => tracer_local(marbl_tracer_indices%sio3_ind,:), &

         auto_C     => autotroph_local%C(:,:),     &
         auto_P     => autotroph_local%P(:,:),     &
         auto_Chl   => autotroph_local%Chl(:,:),   &
         auto_Fe    => autotroph_local%Fe(:,:),    &
         auto_Si    => autotroph_local%Si(:,:),    &
         auto_CaCO3 => autotroph_local%CaCO3(:,:), &

         thetaC     => autotroph_derived_terms%thetaC(:,:), & ! current Chl/C ratio (mg Chl/mmol C)
         QCaCO3     => autotroph_derived_terms%QCaCO3(:,:), & ! currenc CaCO3/C ratio (mmol CaCO3/mmol C)
         Qp         => autotroph_derived_terms%Qp(:,:),     & ! current P/C ratio (mmol P/mmol C)
         gQp        => autotroph_derived_terms%gQp(:,:),    & ! P/C for growth
         Qfe        => autotroph_derived_terms%Qfe(:,:),    & ! current Fe/C ratio (mmol Fe/mmol C)
         gQfe       => autotroph_derived_terms%gQfe(:,:),   & ! Fe/C for growth
         Qsi        => autotroph_derived_terms%Qsi(:,:),    & ! current Si/C ratio (mmol Si/mmol C)
         gQsi       => autotroph_derived_terms%gQsi(:,:)    & ! diatom Si/C ratio for growth (new biomass)
         )

      !-----------------------------------------------------------------------
      !  set local variables, with incoming ratios
      !-----------------------------------------------------------------------

      do auto_ind = 1, autotroph_cnt
        thetaC(auto_ind,:) = auto_Chl(auto_ind,:) / (auto_C(auto_ind,:) + epsC)
        if (lvariable_PtoC) then
          Qp(auto_ind,:) = auto_P(auto_ind,:) / (auto_C(auto_ind,:) + epsC)
          !-----------------------------------------------------------------------
          !-- Calculate Qp for new growth based on Galbraith and Martiny (2015), with min. N/P
          ! - 14= 0.00976801, 14.5 = 0.00944239 15= 0.00911677 15.5=0.00882272 16= 0.00854701
          ! - std intercept 6.0 = 166.66maxCP, 5.26=190, 4.0 = 250, 3.0 = 333.33
          !-----------------------------------------------------------------------
          gQp(auto_ind,:) = min((((PquotaSlope * PO4_loc(:)) + PquotaIntercept) * 0.001_r8), PquotaMinNP)
        else
          Qp(auto_ind,:) = autotroph_settings(auto_ind)%Qp_fixed
          gQp(auto_ind,:) = autotroph_settings(auto_ind)%Qp_fixed
        endif
        !  Uncomment this line to use modified, fixed Redfield (C/N/P 117/16/1) stoichiomdetry
        !      gQp(auto_ind) = 0.00854701_r8      ! fixed Redfield C/N/P

        !-----------------------------------------------------------------------
        !  DETERMINE NEW ELEMENTAL RATIOS FOR GROWTH (NEW BIOMASS)
        !-----------------------------------------------------------------------

        Qfe(auto_ind,:) = auto_Fe(auto_ind,:) / (auto_C(auto_ind,:) + epsC)
        gQfe(auto_ind,:) = autotroph_settings(auto_ind)%gQfe_0
        where (Fe_loc(:) < gQ_Fe_kFe_thres * autotroph_settings(auto_ind)%kFe)
          gQfe(auto_ind,:) = max((gQfe(auto_ind,:) * Fe_loc(:) &
                                  / (gQ_Fe_kFe_thres * autotroph_settings(auto_ind)%kFe)), &
                                 autotroph_settings(auto_ind)%gQfe_min)
        end where

        if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
          Qsi(auto_ind,:) = min(auto_Si(auto_ind,:) / (auto_C(auto_ind,:) + epsC), gQsi_max)
          gQsi(auto_ind,:) = gQsi_0

          !  Modify these initial ratios under low ambient iron conditions
          where ((Fe_loc(:) < gQ_Fe_kFe_thres * autotroph_settings(auto_ind)%kFe) &
              .and. (Fe_loc(:) > c0) &
              .and. (SiO3_loc(:) > (gQ_Si_kSi_thres * autotroph_settings(auto_ind)%kSiO3)))
            gQsi(auto_ind,:) = min((gQsi(auto_ind,:) * gQ_Fe_kFe_thres &
                             * autotroph_settings(auto_ind)%kFe / Fe_loc(:)), gQsi_max)
          else where (Fe_loc(:) == c0)
            gQsi(auto_ind,:) = gQsi_max
          end where

          !  Modify the initial si/C ratio under low ambient Si conditions
          where (SiO3_loc(:) < (gQ_Si_kSi_thres * autotroph_settings(auto_ind)%kSiO3))
            gQsi(auto_ind,:) = max((gQsi(auto_ind,:) * SiO3_loc(:) &
                             / (gQ_Si_kSi_thres * autotroph_settings(auto_ind)%kSiO3)), gQsi_min)
          end where
        end if

        !-----------------------------------------------------------------------
        !  QCaCO3 is the percentage of sp organic matter which is associated
        !  with coccolithophores
        !-----------------------------------------------------------------------

        if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
          QCaCO3(auto_ind,:) = min(auto_CaCO3(auto_ind,:) / (auto_C(auto_ind,:) + epsC), QCaCO3_max)
        end if
      end do
    end associate

  end subroutine compute_autotroph_elemental_ratios

  !***********************************************************************

  subroutine compute_function_scaling(temperature, Tfunc)

    !-----------------------------------------------------------------------
    !  Tref = 30.0 reference temperature (degC)
    !  Using q10 formulation with Q10 value of 2.0 (Doney et al., 1996).
    !  growth, mort and grazing rates scaled by Tfunc where they are computed
    !-----------------------------------------------------------------------

    use marbl_settings_mod,  only : Q_10
    use marbl_constants_mod, only : Tref
    use marbl_constants_mod, only : c10

    real(r8), intent(in)  :: temperature(:)
    real(r8), intent(out) :: Tfunc(:)

    Tfunc = Q_10**(((temperature + T0_Kelvin) - (Tref + T0_Kelvin)) / c10)

  end subroutine compute_function_scaling

  !***********************************************************************

  subroutine compute_Pprime(km, zt, autotroph_local, temperature, Pprime)

    use marbl_settings_mod, only : thres_z1_auto
    use marbl_settings_mod, only : thres_z2_auto

    integer,                    intent(in)  :: km
    real(r8),                   intent(in)  :: zt(km)
    type(autotroph_local_type), intent(in)  :: autotroph_local
    real(r8),                   intent(in)  :: temperature(km)
    real(r8),                   intent(out) :: Pprime(autotroph_cnt, km)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    real(r8) :: f_loss_thres(km)
    real(r8) :: C_loss_thres(km)
    !-----------------------------------------------------------------------

    !  calculate the loss threshold interpolation factor
    f_loss_thres(:) = min(max((thres_z2_auto - zt(:))/(thres_z2_auto - thres_z1_auto), c0), c1)

    !  Compute Pprime for all autotrophs, used for loss terms
    do auto_ind = 1, autotroph_cnt
      where (temperature < autotroph_settings(auto_ind)%temp_thres)
        C_loss_thres = f_loss_thres * autotroph_settings(auto_ind)%loss_thres2
      else where
        C_loss_thres = f_loss_thres * autotroph_settings(auto_ind)%loss_thres
      end where
      Pprime(auto_ind,:) = max(autotroph_local%C(auto_ind,:) - C_loss_thres, c0)
    end do

  end subroutine compute_Pprime

  !***********************************************************************

  subroutine compute_autotroph_photosynthesis(km, PAR_nsubcols, &
       autotroph_local, temperature, Tfunc, PAR_col_frac, PAR_avg, &
       autotroph_derived_terms)

    !-----------------------------------------------------------------------
    !     get photosynth. rate, phyto C biomass change, photoadapt
    !-----------------------------------------------------------------------

    use marbl_constants_mod, only : epsTinv

    integer(int_kind),                  intent(in)    :: km
    integer(int_kind),                  intent(in)    :: PAR_nsubcols
    type(autotroph_local_type),         intent(in)    :: autotroph_local
    real(r8),                           intent(in)    :: temperature(km)
    real(r8),                           intent(in)    :: Tfunc(km)
    real(r8),                           intent(in)    :: PAR_col_frac(PAR_nsubcols)
    real(r8),                           intent(in)    :: PAR_avg(km,PAR_nsubcols)
    type(autotroph_derived_terms_type), intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: k, auto_ind, subcol_ind
    real(r8) :: PCmax            ! max value of PCphoto at temperature TEMP (1/sec)
    real(r8) :: light_lim_subcol ! light_lim for a sub-column
    real(r8) :: PCphoto_subcol   ! PCphoto for a sub-column
    real(r8) :: pChl_subcol      ! Chl synth. regulation term (mg Chl/mmol N)
    real(r8) :: photoacc_subcol  ! photoacc for a sub-column
    !-----------------------------------------------------------------------

    associate(                                                &
         ! current Chl/C ratio (mg Chl / mmol C)
         thetaC    => autotroph_derived_terms%thetaC(:,:),    &
         f_nut     => autotroph_derived_terms%f_nut(:,:),     &
         light_lim => autotroph_derived_terms%light_lim(:,:), &
         PCPhoto   => autotroph_derived_terms%PCPhoto(:,:),   &
         photoC    => autotroph_derived_terms%photoC(:,:),    &
         photoacc  => autotroph_derived_terms%photoacc(:,:),  &
         alphaPI   => autotroph_settings(:)%alphaPI           &
         )

      do auto_ind = 1, autotroph_cnt
        do k=1,km

          if (temperature(k) < autotroph_settings(auto_ind)%temp_thres) then
            PCmax = c0
          else
            PCmax = autotroph_settings(auto_ind)%PCref * f_nut(auto_ind,k) * Tfunc(k)
          end if

          if (thetaC(auto_ind,k) > c0) then
            light_lim(auto_ind,k) = c0
            PCphoto(auto_ind,k)   = c0
            photoacc(auto_ind,k)  = c0

            do subcol_ind = 1, PAR_nsubcols
              if (PAR_avg(k,subcol_ind) > c0) then
                light_lim_subcol = (c1 - exp((-c1 * alphaPI(auto_ind) * thetaC(auto_ind,k) * PAR_avg(k,subcol_ind)) &
                                       / (PCmax + epsTinv)))

                PCphoto_subcol = PCmax * light_lim_subcol

                ! GD 98 Chl. synth. term
                pChl_subcol = autotroph_settings(auto_ind)%thetaN_max * PCphoto_subcol &
                            / (alphaPI(auto_ind) * thetaC(auto_ind,k) * PAR_avg(k,subcol_ind))
                photoacc_subcol = (pChl_subcol * PCphoto_subcol * Q / thetaC(auto_ind,k)) &
                                * autotroph_local%Chl(auto_ind,k)

                light_lim(auto_ind,k) = light_lim(auto_ind,k) + PAR_col_frac(subcol_ind) * light_lim_subcol
                PCphoto(auto_ind,k)   = PCphoto(auto_ind,k)   + PAR_col_frac(subcol_ind) * PCphoto_subcol
                photoacc(auto_ind,k)  = photoacc(auto_ind,k)  + PAR_col_frac(subcol_ind) * photoacc_subcol
              end if
            end do

            photoC(auto_ind,k) = PCphoto(auto_ind,k) * autotroph_local%C(auto_ind,k)
          else
            light_lim(auto_ind,k) = c0
            PCphoto(auto_ind,k)   = c0
            photoacc(auto_ind,k)  = c0
            photoC(auto_ind,k)    = c0
          endif
        end do
      end do
    end associate

  end subroutine compute_autotroph_photosynthesis

  !***********************************************************************

  subroutine compute_autotroph_calcification(km, autotroph_local, temperature, autotroph_derived_terms)

    !-----------------------------------------------------------------------
    !  CaCO3 Production, parameterized as function of small phyto production
    !  decrease CaCO3 as function of nutrient limitation decrease CaCO3 prod
    !  at low temperatures increase CaCO3 prod under bloom conditions
    !  maximum calcification rate is 40% of primary production
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : parm_f_prod_sp_CaCO3
    use marbl_settings_mod, only : CaCO3_sp_thres
    use marbl_settings_mod, only : CaCO3_temp_thres1
    use marbl_settings_mod, only : CaCO3_temp_thres2
    use marbl_settings_mod, only : f_photosp_CaCO3

    integer,                            intent(in)    :: km
    type(autotroph_local_type),         intent(in)    :: autotroph_local
    real(r8),                           intent(in)    :: temperature(km)
    type(autotroph_derived_terms_type), intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                                 &
         f_nut      => autotroph_derived_terms%f_nut(:,:),     & ! input
         photoC     => autotroph_derived_terms%photoC(:,:),    & ! input
         CaCO3_form => autotroph_derived_terms%CaCO3_form(:,:) & ! output
         )

      do auto_ind = 1, autotroph_cnt
        if (autotroph_settings(auto_ind)%imp_calcifier) then
          CaCO3_form(auto_ind,:) = parm_f_prod_sp_CaCO3 * photoC(auto_ind,:)
          CaCO3_form(auto_ind,:) = CaCO3_form(auto_ind,:) * f_nut(auto_ind,:) * f_nut(auto_ind,:)

          where (temperature(:) < CaCO3_temp_thres1)
            CaCO3_form(auto_ind,:) = CaCO3_form(auto_ind,:) * max((temperature(:) - CaCO3_temp_thres2), c0) / &
                 (CaCO3_temp_thres1-CaCO3_temp_thres2)
          end where

          where (autotroph_local%C(auto_ind,:) > CaCO3_sp_thres)
            CaCO3_form(auto_ind,:) = min((CaCO3_form(auto_ind,:) * autotroph_local%C(auto_ind,:) / CaCO3_sp_thres), &
                 (f_photosp_CaCO3 * photoC(auto_ind,:)))
          end where
        end if

      end do

    end associate

  end subroutine compute_autotroph_calcification

  !***********************************************************************

  subroutine compute_autotroph_uptake(km, marbl_tracer_indices, tracer_local, autotroph_derived_terms)

    integer,                            intent(in)    :: km
    type(marbl_tracer_index_type),      intent(in)    :: marbl_tracer_indices
    real(r8),                           intent(in)    :: tracer_local(marbl_tracer_indices%total_cnt,km)
    type(autotroph_derived_terms_type), intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  Get relative nutrient uptake rates for autotrophs,
    !  min. relative uptake rate modifies C fixation in the manner
    !  that the min. cell quota does in GD98.
    !-----------------------------------------------------------------------

    associate(                                                           &
              DOP_loc => tracer_local(marbl_tracer_indices%dop_ind,:),   &
              NO3_loc => tracer_local(marbl_tracer_indices%no3_ind,:),   &
              NH4_loc => tracer_local(marbl_tracer_indices%nh4_ind,:),   &
              PO4_loc => tracer_local(marbl_tracer_indices%po4_ind,:),   &
              Fe_loc   => tracer_local(marbl_tracer_indices%fe_ind,:),   &
              SiO3_loc => tracer_local(marbl_tracer_indices%sio3_ind,:), &
              ! AUTOTROPHS
              Nfixer     => autotroph_settings(:)%Nfixer,     &
              silicifier => autotroph_settings(:)%silicifier, &
              kNO3   => autotroph_settings(:)%kNO3,           &
              kNH4   => autotroph_settings(:)%kNH4,           &
              kFe    => autotroph_settings(:)%kFe,            &
              kPO4   => autotroph_settings(:)%kPO4,           &
              kDOP   => autotroph_settings(:)%kDOP,           &
              kSiO3  => autotroph_settings(:)%kSiO3,          &
              ! OUTPUTS
              VNO3  => autotroph_derived_terms%VNO3(:,:),  &
              VNH4  => autotroph_derived_terms%VNH4(:,:),  &
              VNtot => autotroph_derived_terms%VNtot(:,:), &
              VFe   => autotroph_derived_terms%VFe(:,:),   &
              f_nut => autotroph_derived_terms%f_nut(:,:), &
              VDOP  => autotroph_derived_terms%VDOP(:,:),  &
              VPO4  => autotroph_derived_terms%VPO4(:,:),  &
              VPtot => autotroph_derived_terms%VPtot(:,:), &
              VSiO3 => autotroph_derived_terms%VSiO3(:,:)  &
              )

      do auto_ind = 1, autotroph_cnt

        VNO3(auto_ind,:) = (NO3_loc(:) / kNO3(auto_ind)) &
                         / (c1 + (NO3_loc(:) / kNO3(auto_ind)) + (NH4_loc(:) / kNH4(auto_ind)))
        VNH4(auto_ind,:) = (NH4_loc(:) / kNH4(auto_ind)) &
                         / (c1 + (NO3_loc(:) / kNO3(auto_ind)) + (NH4_loc(:) / kNH4(auto_ind)))
        if (Nfixer(auto_ind)) then
          VNtot(auto_ind,:) = c1
        else
          VNtot(auto_ind,:) = VNO3(auto_ind,:) + VNH4(auto_ind,:)
        end if

        VFe(auto_ind, :) = Fe_loc(:) / (Fe_loc(:) + kFe(auto_ind))
        f_nut(auto_ind, :) = min(VNtot(auto_ind, :), VFe(auto_ind, :))

        VPO4(auto_ind, :) = (PO4_loc(:) / kPO4(auto_ind)) &
                          / (c1 + (PO4_loc(:) / kPO4(auto_ind)) + (DOP_loc(:) / kDOP(auto_ind)))
        VDOP(auto_ind, :) = (DOP_loc(:) / kDOP(auto_ind)) &
                          / (c1 + (PO4_loc(:) / kPO4(auto_ind)) + (DOP_loc(:) / kDOP(auto_ind)))
        VPtot(auto_ind, :) = VPO4(auto_ind, :) + VDOP(auto_ind, :)
        f_nut(auto_ind, :) = min(f_nut(auto_ind, :), VPtot(auto_ind, :))

        if (silicifier(auto_ind)) then
          VSiO3(auto_ind, :) = SiO3_loc(:) / (SiO3_loc(:) + kSiO3(auto_ind))
          f_nut(auto_ind, :) = min(f_nut(auto_ind, :), VSiO3(auto_ind, :))
        endif

      end do

    end associate

  end subroutine compute_autotroph_uptake

  !***********************************************************************

  subroutine compute_autotroph_nfixation(km, autotroph_derived_terms)

    !-----------------------------------------------------------------------
    !  Get N fixation by diazotrophs based on C fixation,
    !  Diazotrophs fix more than they need then 20% is excreted
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : Q
    use marbl_settings_mod, only : r_Nfix_photo

    integer,                            intent(in)    :: km
    type(autotroph_derived_terms_type), intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    real(r8) :: work1(km)
    !-----------------------------------------------------------------------

    associate(                                              &
         photoC   => autotroph_derived_terms%photoC(:, :),  & ! input
         NO3_V    => autotroph_derived_terms%NO3_V(:, :),   & ! input
         NH4_V    => autotroph_derived_terms%NH4_V(:, :),   & ! input
         Nfix     => autotroph_derived_terms%Nfix(:, :),    & ! output total Nitrogen fixation (mmol N/m^3/sec)
         Nexcrete => autotroph_derived_terms%Nexcrete(:, :) & ! output fixed N excretion
         )

      do auto_ind = 1, autotroph_cnt
        if (autotroph_settings(auto_ind)%Nfixer) then
          work1(:) = photoC(auto_ind,:) * Q
          Nfix(auto_ind,:) = (work1(:) * r_Nfix_photo) - NO3_V(auto_ind,:) - NH4_V(auto_ind,:)
          Nexcrete(auto_ind,:) = Nfix(auto_ind,:) + NO3_V(auto_ind,:) + NH4_V(auto_ind,:) - work1(:)
        endif
      end do

    end associate

  end subroutine compute_autotroph_nfixation

  !***********************************************************************

  subroutine compute_autotroph_nutrient_uptake(marbl_tracer_indices, autotroph_derived_terms)

    !-----------------------------------------------------------------------
    !  Get nutrient uptakes by small phyto based on calculated C fixation
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : Q

    type(marbl_tracer_index_type),      intent(in)    :: marbl_tracer_indices
    type(autotroph_derived_terms_type), intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                              &
         gQp      => autotroph_derived_terms%gQp(:, :),     & ! p/C for growth
         gQfe     => autotroph_derived_terms%gQfe(:, :),    & ! fe/C for growth
         gQsi     => autotroph_derived_terms%gQsi(:, :),    & ! diatom Si/C ratio for growth (new biomass)
         VNO3     => autotroph_derived_terms%VNO3(:, :),    & ! input
         VNH4     => autotroph_derived_terms%VNH4(:, :),    & ! input
         VNtot    => autotroph_derived_terms%VNtot(:, :),   & ! input
         VPO4     => autotroph_derived_terms%VPO4(:, :),    & ! input
         VDOP     => autotroph_derived_terms%VDOP(:, :),    & ! input
         VPtot    => autotroph_derived_terms%VPtot(:, :),   & ! input
         photoC   => autotroph_derived_terms%photoC(:, :),  & ! input
         NO3_V    => autotroph_derived_terms%NO3_V(:, :),   & ! output
         NH4_V    => autotroph_derived_terms%NH4_V(:, :),   & ! output
         PO4_V    => autotroph_derived_terms%PO4_V(:, :),   & ! output
         DOP_V    => autotroph_derived_terms%DOP_V(:, :),   & ! output
         photoFe  => autotroph_derived_terms%photoFe(:, :), & ! output
         photoSi  => autotroph_derived_terms%photoSi(:, :)  & ! output
         )

      do auto_ind = 1, autotroph_cnt

        where (VNtot(auto_ind,:) > c0)
          NO3_V(auto_ind,:) = (VNO3(auto_ind,:) / VNtot(auto_ind,:)) * photoC(auto_ind,:) * Q
          NH4_V(auto_ind,:) = (VNH4(auto_ind,:) / VNtot(auto_ind,:)) * photoC(auto_ind,:) * Q
        else where
          NO3_V(auto_ind,:) = c0
          NH4_V(auto_ind,:) = c0
        end where

        where (VPtot(auto_ind,:) > c0)
          PO4_V(auto_ind,:) = (VPO4(auto_ind,:) / VPtot(auto_ind,:)) * photoC(auto_ind,:) * gQp(auto_ind,:)
          DOP_V(auto_ind,:) = (VDOP(auto_ind,:) / VPtot(auto_ind,:)) * photoC(auto_ind,:) * gQp(auto_ind,:)
        else where
          PO4_V(auto_ind,:) = c0
          DOP_V(auto_ind,:) = c0
        end where

        !-----------------------------------------------------------------------
        !  Get nutrient uptake by diatoms based on C fixation
        !-----------------------------------------------------------------------

        photoFe(auto_ind,:) = photoC(auto_ind,:) * gQfe(auto_ind,:)

        if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
          photoSi(auto_ind,:) = photoC(auto_ind,:) * gQsi(auto_ind,:)
        endif

      end do
    end associate

  end subroutine compute_autotroph_nutrient_uptake

  !***********************************************************************

  subroutine compute_autotroph_loss(km, Tfunc, autotroph_derived_terms)

    !-----------------------------------------------------------------------
    ! Compute autotroph-loss, autotroph aggregation loss and routine of
    ! loss terms
    !-----------------------------------------------------------------------

    integer,                            intent(in)    :: km
    real(r8),                           intent(in)    :: Tfunc(km)
    type(autotroph_derived_terms_type), intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                                         &
         QCaCO3        => autotroph_derived_terms%QCaCO3(:, :)       , & ! input
         Pprime        => autotroph_derived_terms%Pprime(:, :)       , & ! input
         auto_loss     => autotroph_derived_terms%auto_loss(:, :)    , & ! output
         auto_loss_poc => autotroph_derived_terms%auto_loss_poc(:, :), & ! output
         auto_loss_dic => autotroph_derived_terms%auto_loss_dic(:, :), & ! output
         auto_loss_doc => autotroph_derived_terms%auto_loss_doc(:, :), & ! output
         auto_agg      => autotroph_derived_terms%auto_agg(:, :)       & ! output
         )

      do auto_ind = 1, autotroph_cnt
        !-----------------------------------------------------------------------
        !  get autotroph loss (in C units)
        !  autotroph agg loss
        !-----------------------------------------------------------------------

        auto_loss(auto_ind,:) = autotroph_settings(auto_ind)%mort * Pprime(auto_ind,:) * Tfunc(:)

        auto_agg(auto_ind,:) = min((autotroph_settings(auto_ind)%agg_rate_max * dps) * Pprime(auto_ind,:), &
                                   autotroph_settings(auto_ind)%mort2 * Pprime(auto_ind,:)**1.75_r8)
        auto_agg(auto_ind,:) = max((autotroph_settings(auto_ind)%agg_rate_min * dps) * Pprime(auto_ind,:), &
                                   auto_agg(auto_ind,:))

        !-----------------------------------------------------------------------
        !  routing of loss terms
        !  all aggregation goes to POM
        !  min.%C routed from sp_loss = 0.59 * QCaCO3, or P_CaCO3%rho
        !-----------------------------------------------------------------------

        if (autotroph_settings(auto_ind)%imp_calcifier) then
          auto_loss_poc(auto_ind,:) = QCaCO3(auto_ind,:) * auto_loss(auto_ind,:)
        else
          auto_loss_poc(auto_ind,:) = autotroph_settings(auto_ind)%loss_poc * auto_loss(auto_ind,:)
        endif
        auto_loss_doc(auto_ind,:) = (c1 - parm_labile_ratio) * (auto_loss(auto_ind,:) - auto_loss_poc(auto_ind,:))
        auto_loss_dic(auto_ind,:) = parm_labile_ratio * (auto_loss(auto_ind,:) - auto_loss_poc(auto_ind,:))
      end do

    end associate

  end subroutine compute_autotroph_loss

  !***********************************************************************

  subroutine compute_Zprime(km, zt, zooC, Tfunc, zooplankton_derived_terms)

    use marbl_settings_mod, only : thres_z1_zoo
    use marbl_settings_mod, only : thres_z2_zoo

    integer(int_kind),                    intent(in)    :: km
    real(r8),                             intent(in)    :: zt(km)
    real(r8),                             intent(in)    :: zooC(zooplankton_cnt,km)
    real(r8),                             intent(in)    :: Tfunc(km)
    type(zooplankton_derived_terms_type), intent(inout) :: zooplankton_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: zoo_ind
    real(r8) :: f_loss_thres(km)
    real(r8) :: C_loss_thres(km)
    !-----------------------------------------------------------------------

    associate(                                               &
         Zprime   => zooplankton_derived_terms%Zprime(:,:),  & !(zooplankton_cnt)
         zoo_loss => zooplankton_derived_terms%zoo_loss(:,:) & !(zooplankton_cnt) output
         )

      !  calculate the loss threshold interpolation factor
      f_loss_thres(:) = min(max((thres_z2_zoo - zt(:))/(thres_z2_zoo - thres_z1_zoo), c0), c1)

      do zoo_ind = 1, zooplankton_cnt
        C_loss_thres(:) = f_loss_thres(:) * zooplankton_settings(zoo_ind)%loss_thres
        Zprime(zoo_ind,:) = max(zooC(zoo_ind,:) - C_loss_thres, c0)

        zoo_loss(zoo_ind,:) = (zooplankton_settings(zoo_ind)%z_mort2_0 * Zprime(zoo_ind,:)**1.5_r8 &
                               + zooplankton_settings(zoo_ind)%z_mort_0  * Zprime(zoo_ind,:)) * Tfunc(:)
      end do

    end associate

  end subroutine compute_Zprime

  !***********************************************************************

  subroutine compute_grazing(km, Tfunc, zooplankton_local, zooplankton_derived_terms, autotroph_derived_terms)

    !-----------------------------------------------------------------------
    !  CALCULATE GRAZING
    !
    !  Autotroph prey
    !  routing of grazing terms
    !  all aggregation goes to POC
    !  currently assumes that 33% of grazed caco3 is remineralized
    !  if autotroph_settings(sp_ind)%graze_zoo ever changes, coefficients on routing grazed sp must change!
    !  min.%C routed to POC from grazing for ballast requirements = 0.4 * Qcaco3
    !  NOTE: if autotroph_settings(diat_ind)%graze_zoo is changed, coeff.s for poc, doc and dic must change!
    !-----------------------------------------------------------------------

    use marbl_constants_mod, only : epsC
    use marbl_constants_mod, only : epsTinv
    use marbl_pft_mod, only : grz_fnc_michaelis_menten
    use marbl_pft_mod, only : grz_fnc_sigmoidal

    integer,                              intent(in)    :: km
    real(r8),                             intent(in)    :: Tfunc(km)
    type(zooplankton_local_type),         intent(in)    :: zooplankton_local
    type(zooplankton_derived_terms_type), intent(inout) :: zooplankton_derived_terms
    type(autotroph_derived_terms_type),   intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: k, auto_ind, auto_ind2
    integer  :: zoo_ind, zoo_ind2
    integer  :: pred_ind
    integer  :: prey_ind
    real(r8) :: work1, work2, work3, work4
    real(r8) :: graze_rate
    !-----------------------------------------------------------------------

    associate(                                                           &
         Pprime         => autotroph_derived_terms%Pprime(:,:),          & ! input
         QCaCO3         => autotroph_derived_terms%QCaCO3(:,:),          & ! input
         Zprime         => zooplankton_derived_terms%Zprime(:,:),        & ! input
         auto_graze     => autotroph_derived_terms%auto_graze(:,:),      & ! output
         auto_graze_poc => autotroph_derived_terms%auto_graze_poc(:,:),  & ! output
         auto_graze_dic => autotroph_derived_terms%auto_graze_dic(:,:),  & ! output
         auto_graze_doc => autotroph_derived_terms%auto_graze_doc(:,:),  & ! output
         auto_graze_zoo => autotroph_derived_terms%auto_graze_zoo(:,:),  & ! output
         zoo_graze      => zooplankton_derived_terms%zoo_graze(:,:),     & ! output
         zoo_graze_poc  => zooplankton_derived_terms%zoo_graze_poc(:,:), & ! output
         zoo_graze_dic  => zooplankton_derived_terms%zoo_graze_dic(:,:), & ! output
         zoo_graze_doc  => zooplankton_derived_terms%zoo_graze_doc(:,:), & ! output
         zoo_graze_zoo  => zooplankton_derived_terms%zoo_graze_zoo(:,:), & ! output
         x_graze_zoo    => zooplankton_derived_terms%x_graze_zoo(:,:),   & ! output
         f_zoo_detr     => zooplankton_derived_terms%f_zoo_detr(:,:)     & ! output
         )

      auto_graze(:,:)     = c0 ! total grazing losses from autotroph pool at auto_ind
      auto_graze_zoo(:,:) = c0 ! autotroph grazing losses routed to zooplankton at auto_ind
      auto_graze_poc(:,:) = c0 ! autotroph grazing losses routed to poc
      auto_graze_doc(:,:) = c0 ! autotroph grazing losses routed to doc
      auto_graze_dic(:,:) = c0 ! autotroph grazing losses routed to dic (computed by residual)

      zoo_graze(:,:)     = c0 ! total grazing losses from zooplankton pool at zoo_ind
      zoo_graze_zoo(:,:) = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
      zoo_graze_poc(:,:) = c0 ! zooplankton grazing losses routed to poc
      zoo_graze_doc(:,:) = c0 ! zooplankton grazing losses routed to doc
      zoo_graze_dic(:,:) = c0 ! zooplankton grazing losses routed to dic (computed by residual)

      x_graze_zoo(:,:)   = c0 ! grazing gains by zooplankton at zoo_ind

      do k=1, km
        do pred_ind = 1, zooplankton_cnt

          work3 = c0
          work4 = c0

          do prey_ind = 1, max_grazer_prey_cnt

            !-----------------------------------------------------------------------
            !  compute sum of carbon in the grazee class, both autotrophs and zoop
            !-----------------------------------------------------------------------
            work1 = c0 ! biomass in prey class prey_ind
            do auto_ind2 = 1, grazing_relationship_settings(prey_ind, pred_ind)%auto_ind_cnt
              auto_ind = grazing_relationship_settings(prey_ind, pred_ind)%auto_ind(auto_ind2)
              work1 = work1 + Pprime(auto_ind,k)
            end do

            do zoo_ind2 = 1, grazing_relationship_settings(prey_ind, pred_ind)%zoo_ind_cnt
              zoo_ind = grazing_relationship_settings(prey_ind, pred_ind)%zoo_ind(zoo_ind2)
              work1 = work1 + Zprime(zoo_ind,k)
            end do

            ! compute grazing rate
            graze_rate = c0
            select case (grazing_relationship_settings(prey_ind, pred_ind)%grazing_function)

              case (grz_fnc_michaelis_menten)

                if (work1 > c0) then
                  graze_rate = grazing_relationship_settings(prey_ind, pred_ind)%z_umax_0 * Tfunc(k) &
                             * zooplankton_local%C(pred_ind,k) &
                             * ( work1 / (work1 + grazing_relationship_settings(prey_ind, pred_ind)%z_grz) )
                end if

              case (grz_fnc_sigmoidal)

                if (work1 > c0) then
                  graze_rate = grazing_relationship_settings(prey_ind, pred_ind)%z_umax_0 * Tfunc(k) &
                             * zooplankton_local%C(pred_ind,k) &
                             * ( work1**2 / (work1**2 + grazing_relationship_settings(prey_ind, pred_ind)%z_grz**2) )
                end if

            end select

            !-----------------------------------------------------------------------
            !  autotroph prey
            !-----------------------------------------------------------------------

            do auto_ind2 = 1, grazing_relationship_settings(prey_ind, pred_ind)%auto_ind_cnt
              auto_ind = grazing_relationship_settings(prey_ind, pred_ind)%auto_ind(auto_ind2)

              ! scale by biomass from autotroph pool
              if (work1 > c0) then
                work2 = (Pprime(auto_ind,k) / work1) * graze_rate ! total grazing loss from auto_ind
              else
                work2 = c0
              end if
              auto_graze(auto_ind,k) = auto_graze(auto_ind,k) + work2

              ! routed to zooplankton
              auto_graze_zoo(auto_ind,k) = auto_graze_zoo(auto_ind,k) &
                                         + grazing_relationship_settings(prey_ind, pred_ind)%graze_zoo * work2
              x_graze_zoo(pred_ind,k)    = x_graze_zoo(pred_ind,k)    &
                                         + grazing_relationship_settings(prey_ind, pred_ind)%graze_zoo * work2

              ! routed to POC
              if (autotroph_settings(auto_ind)%imp_calcifier) then
                auto_graze_poc(auto_ind,k) = auto_graze_poc(auto_ind,k) &
                                           + work2 * max((caco3_poc_min * QCaCO3(auto_ind,k)),  &
                                                         min(spc_poc_fac * (Pprime(auto_ind,k)+0.6_r8)**1.6_r8, &
                                                             f_graze_sp_poc_lim))
              else
                auto_graze_poc(auto_ind,k) = auto_graze_poc(auto_ind,k) &
                                           + grazing_relationship_settings(prey_ind, pred_ind)%graze_poc * work2
              endif

              ! routed to DOC
              auto_graze_doc(auto_ind,k) = auto_graze_doc(auto_ind,k) &
                                         + grazing_relationship_settings(prey_ind, pred_ind)%graze_doc * work2

              !  get fractional factor for routing of zoo losses, based on food supply
              work3 = work3 + grazing_relationship_settings(prey_ind, pred_ind)%f_zoo_detr * (work2 + epsC * epsTinv)
              work4 = work4 + (work2 + epsC * epsTinv)

            end do

            !-----------------------------------------------------------------------
            !  Zooplankton prey
            !-----------------------------------------------------------------------
            do zoo_ind2 = 1, grazing_relationship_settings(prey_ind, pred_ind)%zoo_ind_cnt
              zoo_ind = grazing_relationship_settings(prey_ind, pred_ind)%zoo_ind(zoo_ind2)

              ! scale by biomass from zooplankton pool
              if (work1 > c0) then
                work2 = (Zprime(zoo_ind,k) / work1) * graze_rate
              else
                work2 = c0
              end if

              ! grazing loss from zooplankton prey pool
              zoo_graze(zoo_ind,k) = zoo_graze(zoo_ind,k) + work2

              ! routed to zooplankton
              zoo_graze_zoo(zoo_ind,k) = zoo_graze_zoo(zoo_ind,k) &
                                       + grazing_relationship_settings(prey_ind, pred_ind)%graze_zoo * work2
              x_graze_zoo(pred_ind,k)  = x_graze_zoo(pred_ind,k)  &
                                       + grazing_relationship_settings(prey_ind, pred_ind)%graze_zoo * work2

              ! routed to POC/DOC
              zoo_graze_poc(zoo_ind,k) = zoo_graze_poc(zoo_ind,k) &
                                       + grazing_relationship_settings(prey_ind, pred_ind)%graze_poc * work2
              zoo_graze_doc(zoo_ind,k) = zoo_graze_doc(zoo_ind,k) &
                                       + grazing_relationship_settings(prey_ind, pred_ind)%graze_doc * work2

              !  get fractional factor for routing of zoo losses, based on food supply
              work3 = work3 + grazing_relationship_settings(prey_ind, pred_ind)%f_zoo_detr * (work2 + epsC * epsTinv)
              work4 = work4 + (work2 + epsC * epsTinv)

            end do
          end do
          f_zoo_detr(pred_ind,k) = work3 / work4

        end do
      end do

    end associate

  end subroutine compute_grazing

  !***********************************************************************

  subroutine compute_routing(km, zooplankton_derived_terms, autotroph_derived_terms)

    use marbl_settings_mod, only : parm_labile_ratio
    use marbl_settings_mod, only : f_toDOP

    integer,                              intent(in)    :: km
    type(zooplankton_derived_terms_type), intent(inout) :: zooplankton_derived_terms
    type(autotroph_derived_terms_type),   intent(inout) :: autotroph_derived_terms

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: k, auto_ind, zoo_ind
    real(r8) :: remaining_P      ! used in routing P from autotrophs
    !-----------------------------------------------------------------------

    associate(                                                           &
         Qp              => autotroph_derived_terms%Qp(:,:),             & ! input
         auto_graze      => autotroph_derived_terms%auto_graze(:,:),     & ! input
         auto_graze_zoo  => autotroph_derived_terms%auto_graze_zoo(:,:), & ! input
         auto_graze_poc  => autotroph_derived_terms%auto_graze_poc(:,:), & ! input
         auto_graze_doc  => autotroph_derived_terms%auto_graze_doc(:,:), & ! input
         auto_loss       => autotroph_derived_terms%auto_loss(:,:),      & ! input
         auto_loss_poc   => autotroph_derived_terms%auto_loss_poc(:,:),  & ! input
         auto_agg        => autotroph_derived_terms%auto_agg(:,:),       & ! input

         zoo_graze       => zooplankton_derived_terms%zoo_graze(:,:),     & ! input
         zoo_graze_poc   => zooplankton_derived_terms%zoo_graze_poc(:,:), & ! input
         zoo_graze_doc   => zooplankton_derived_terms%zoo_graze_doc(:,:), & ! input
         zoo_graze_zoo   => zooplankton_derived_terms%zoo_graze_zoo(:,:), & ! input
         zoo_loss        => zooplankton_derived_terms%zoo_loss(:,:),      & ! input
         f_zoo_detr      => zooplankton_derived_terms%f_zoo_detr(:,:),    & ! input

         auto_graze_dic  => autotroph_derived_terms%auto_graze_dic(:,:),  & ! output
         remaining_P_dop => autotroph_derived_terms%remaining_P_dop(:,:), & ! output
         remaining_P_pop => autotroph_derived_terms%remaining_P_pop(:,:), & ! output
         remaining_P_dip => autotroph_derived_terms%remaining_P_dip(:,:), & ! output

         zoo_graze_dic   => zooplankton_derived_terms%zoo_graze_dic(:,:), & ! output
         zoo_loss_poc    => zooplankton_derived_terms%zoo_loss_poc(:,:),  & ! output
         zoo_loss_doc    => zooplankton_derived_terms%zoo_loss_doc(:,:),  & ! output
         zoo_loss_dic    => zooplankton_derived_terms%zoo_loss_dic(:,:)   & ! output
         )

      do k=1,km
        !-----------------------------------------------------------------------
        ! compute routing to dic of grazed material
        !-----------------------------------------------------------------------
        do auto_ind = 1, autotroph_cnt
          auto_graze_dic(auto_ind,k) = auto_graze(auto_ind,k) &
                                     - (auto_graze_zoo(auto_ind,k) + auto_graze_poc(auto_ind,k) &
                                        + auto_graze_doc(auto_ind,k))
        end do
        do zoo_ind = 1, zooplankton_cnt
          zoo_graze_dic(zoo_ind,k) = zoo_graze(zoo_ind,k)  &
                                   - (zoo_graze_zoo(zoo_ind,k) + zoo_graze_poc(zoo_ind,k) + zoo_graze_doc(zoo_ind,k))
        end do

        !-----------------------------------------------------------------------
        ! compute zooplankton loss routing
        !-----------------------------------------------------------------------
        do zoo_ind = 1, zooplankton_cnt
          zoo_loss_poc(zoo_ind,k) = f_zoo_detr(zoo_ind,k) * zoo_loss(zoo_ind,k)
          zoo_loss_doc(zoo_ind,k) = (c1 - parm_labile_ratio) * (c1 - f_zoo_detr(zoo_ind,k)) * zoo_loss(zoo_ind,k)
          zoo_loss_dic(zoo_ind,k) = parm_labile_ratio * (c1 - f_zoo_detr(zoo_ind,k)) * zoo_loss(zoo_ind,k)
        end do

        !-----------------------------------------------------------------------
        ! We ensure the zooplankton pool gets its Pquota, fixed P/C ratio,
        ! sinking POP is routed as POC * autoQp, but reduced where insuffient P is available
        ! The remaining P is split between DOP and PO4.
        !-----------------------------------------------------------------------

        do auto_ind = 1, autotroph_cnt
          remaining_P_pop(auto_ind,k) = (auto_graze_poc(auto_ind,k) + auto_loss_poc(auto_ind,k) + auto_agg(auto_ind,k)) &
                                    * Qp(auto_ind,k)

          remaining_P = (auto_graze(auto_ind,k) + auto_loss(auto_ind,k) + auto_agg(auto_ind,k)) * Qp(auto_ind,k) &
                      - auto_graze_zoo(auto_ind,k) * Qp_zoo - remaining_P_pop(auto_ind,k)

          !-----------------------------------------------------------------------
          ! reduce sinking pop if remaining_P is negative
          !-----------------------------------------------------------------------
          if (remaining_P < c0) then
            remaining_P_pop(auto_ind,k) = remaining_P_pop(auto_ind,k) + remaining_P
            remaining_P = c0
          endif

          !-----------------------------------------------------------------------
          ! increase fraction routed to dop, relative to doc 0.06, 0.94
          !    better matches DOP obs with var P quotas
          !-----------------------------------------------------------------------

          remaining_P_dop(auto_ind,k) = f_toDOP * remaining_P
          remaining_P_dip(auto_ind,k) = (c1 - f_toDOP) * remaining_P

        end do
      end do

    end associate

  end subroutine compute_routing

  !***********************************************************************

  subroutine compute_dissolved_organic_matter (km, marbl_tracer_indices, &
             PAR_nsubcols, PAR, &
             zooplankton_derived_terms, autotroph_derived_terms, &
             dz1, tracer_local, dissolved_organic_matter)

    use marbl_settings_mod, only : Q
    use marbl_settings_mod, only : DOC_reminR_light
    use marbl_settings_mod, only : DON_reminR_light
    use marbl_settings_mod, only : DOP_reminR_light
    use marbl_settings_mod, only : DOC_reminR_dark
    use marbl_settings_mod, only : DON_reminR_dark
    use marbl_settings_mod, only : DOP_reminR_dark
    use marbl_settings_mod, only : DOCr_reminR0
    use marbl_settings_mod, only : DONr_reminR0
    use marbl_settings_mod, only : DOPr_reminR0
    use marbl_settings_mod, only : DOMr_reminR_photo

    integer(int_kind),                    intent(in)    :: km
    type(marbl_tracer_index_type),        intent(in)    :: marbl_tracer_indices
    integer(int_kind),                    intent(in)    :: PAR_nsubcols
    type(marbl_PAR_type),                 intent(in)    :: PAR
    type(zooplankton_derived_terms_type), intent(in)    :: zooplankton_derived_terms
    type(autotroph_derived_terms_type),   intent(in)    :: autotroph_derived_terms
    real(r8),                             intent(in)    :: dz1
    real(r8),                             intent(in)    :: tracer_local(marbl_tracer_indices%total_cnt,km)
    type(dissolved_organic_matter_type),  intent(inout) :: dissolved_organic_matter

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: k, subcol_ind
    real(r8) :: work
    real(r8) :: DOC_reminR        ! remineralization rate (1/sec)
    real(r8) :: DON_reminR        ! remineralization rate (1/sec)
    real(r8) :: DOP_reminR        ! remineralization rate (1/sec)
    real(r8) :: DOCr_reminR       ! remineralization rate (1/sec)
    real(r8) :: DONr_reminR       ! remineralization rate (1/sec)
    real(r8) :: DOPr_reminR       ! remineralization rate (1/sec)
    !-----------------------------------------------------------------------

    associate(                                                           &
         DOC_loc         => tracer_local(marbl_tracer_indices%doc_ind,:),  &
         DON_loc         => tracer_local(marbl_tracer_indices%don_ind,:),  &
         DOP_loc         => tracer_local(marbl_tracer_indices%dop_ind,:),  &
         DONr_loc        => tracer_local(marbl_tracer_indices%donr_ind,:), &
         DOPr_loc        => tracer_local(marbl_tracer_indices%dopr_ind,:), &
         DOCr_loc        => tracer_local(marbl_tracer_indices%docr_ind,:), &

         PAR_col_frac    => PAR%col_frac(:),    &
         PAR_in          => PAR%interface(0,:), &
         PAR_avg         => PAR%avg(:,:),       &

         remaining_P_dop => autotroph_derived_terms%remaining_P_dop(:,:), & ! input
         auto_loss_doc   => autotroph_derived_terms%auto_loss_doc(:,:),   & ! input
         auto_graze_doc  => autotroph_derived_terms%auto_graze_doc(:,:),  & ! input

         zoo_loss_doc    => zooplankton_derived_terms%zoo_loss_doc(:,:),  & ! input
         zoo_graze_doc   => zooplankton_derived_terms%zoo_graze_doc(:,:), & ! input

         DOC_prod        => dissolved_organic_matter%DOC_prod(:),   & ! output production of DOC (mmol C/m^3/sec)
         DOC_remin       => dissolved_organic_matter%DOC_remin(:),  & ! output remineralization of DOC (mmol C/m^3/sec)
         DOCr_remin      => dissolved_organic_matter%DOCr_remin(:), & ! output remineralization of DOCr
         DON_prod        => dissolved_organic_matter%DON_prod(:),   & ! output production of DON
         DON_remin       => dissolved_organic_matter%DON_remin(:),  & ! output remineralization of DON
         DONr_remin      => dissolved_organic_matter%DONr_remin(:), & ! output remineralization of DONr
         DOP_prod        => dissolved_organic_matter%DOP_prod(:),   & ! output production of DOP
         DOP_remin       => dissolved_organic_matter%DOP_remin(:),  & ! output remineralization of DOP
         DOPr_remin      => dissolved_organic_matter%DOPr_remin(:)  & ! output remineralization of DOPr
         )

      !-----------------------------------------------------------------------
      !  compute terms for DOM
      !-----------------------------------------------------------------------

      DOC_prod(:) = sum(zoo_loss_doc(:,:), dim=1) + sum(auto_loss_doc(:,:), dim=1) &
                  + sum(auto_graze_doc(:,:), dim=1) + sum(zoo_graze_doc(:,:), dim=1)
      DON_prod(:) = Q * DOC_prod(:) * f_toDON
      DOP_prod(:) = Qp_zoo * (sum(zoo_loss_doc(:,:), dim=1) + sum(zoo_graze_doc(:,:), dim=1)) &
                  + sum(remaining_P_dop(:,:), dim=1)

      do k=1, km
        !-----------------------------------------------------------------------
        !  Different remin rates in light and dark for semi-labile pools
        !-----------------------------------------------------------------------

        DOC_reminR = c0
        DON_reminR = c0
        DOP_reminR = c0

        do subcol_ind = 1, PAR_nsubcols
          if (PAR_col_frac(subcol_ind) > c0) then
            if (PAR_avg(k,subcol_ind) > 1.0_r8) then
              DOC_reminR = DOC_reminR + PAR_col_frac(subcol_ind) * DOC_reminR_light
              DON_reminR = DON_reminR + PAR_col_frac(subcol_ind) * DON_reminR_light
              DOP_reminR = DOP_reminR + PAR_col_frac(subcol_ind) * DOP_reminR_light
            else
              DOC_reminR = DOC_reminR + PAR_col_frac(subcol_ind) * DOC_reminR_dark
              DON_reminR = DON_reminR + PAR_col_frac(subcol_ind) * DON_reminR_dark
              DOP_reminR = DOP_reminR + PAR_col_frac(subcol_ind) * DOP_reminR_dark
            endif
          endif
        end do

        !-----------------------------------------------------------------------
        !  Refractory remin increased in top layer from photodegradation due to UV
        !-----------------------------------------------------------------------

        DOCr_reminR = DOCr_reminR0
        DONr_reminR = DONr_reminR0
        DOPr_reminR = DOPr_reminR0

        if (k == 1) then
          do subcol_ind = 1, PAR_nsubcols
            if ((PAR_col_frac(subcol_ind) > c0) .and. (PAR_in(subcol_ind) > 1.0_r8)) then
              work = PAR_col_frac(subcol_ind) * (log(PAR_in(subcol_ind))*0.4373_r8) * (10.0e2_r8/dz1)
              DOCr_reminR = DOCr_reminR + work * DOMr_reminR_photo
              DONr_reminR = DONr_reminR + work * DOMr_reminR_photo
              DOPr_reminR = DOPr_reminR + work * DOMr_reminR_photo
            endif
          end do
        endif

        DOC_remin(k)  = DOC_loc(k)  * DOC_reminR
        DON_remin(k)  = DON_loc(k)  * DON_reminR
        DOP_remin(k)  = DOP_loc(k)  * DOP_reminR
        DOCr_remin(k) = DOCr_loc(k) * DOCr_reminR
        DONr_remin(k) = DONr_loc(k) * DONr_reminR
        DOPr_remin(k) = DOPr_loc(k) * DOPr_reminR

      end do

    end associate

  end subroutine compute_dissolved_organic_matter

  !***********************************************************************

  subroutine compute_scavenging(k, km, marbl_tracer_indices, &
       tracer_local, POC, P_CaCO3, P_SiO2, dust, &
       Fefree, Fe_scavenge_rate, Fe_scavenge, Lig_scavenge, &
       marbl_status_log)

    use marbl_constants_mod, only : c3, c4
    use marbl_settings_mod , only : Lig_cnt
    use marbl_settings_mod , only : parm_Fe_scavenge_rate0
    use marbl_settings_mod , only : parm_Lig_scavenge_rate0
    use marbl_settings_mod , only : parm_FeLig_scavenge_rate0
    use marbl_settings_mod , only : dust_Fe_scavenge_scale

    integer,                            intent(in)    :: k
    integer,                            intent(in)    :: km
    type(marbl_tracer_index_type),      intent(in)    :: marbl_tracer_indices
    real(r8),                           intent(in)    :: tracer_local(marbl_tracer_indices%total_cnt,km)
    type(column_sinking_particle_type), intent(in)    :: POC
    type(column_sinking_particle_type), intent(in)    :: P_CaCO3
    type(column_sinking_particle_type), intent(in)    :: P_SiO2
    type(column_sinking_particle_type), intent(in)    :: dust
    real(r8),                           intent(out)   :: Fefree(km)
    real(r8),                           intent(out)   :: Fe_scavenge_rate(km)
    real(r8),                           intent(out)   :: Fe_scavenge(km)
    real(r8),                           intent(out)   :: Lig_scavenge(km)
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    character(len=*), parameter :: subname = 'marbl_interior_tendency_mod:compute_scavenging'
    character(len=char_len)     :: log_message

    ! ligand binding strengths, original values are L/mol, model units are L/umol
    real(kind=r8), parameter :: KFeLig1 = 10.0e13_r8 * 1.0e-6_r8

    real(r8) :: FeLig1               ! iron bound to ligand 1
    real(r8) :: sinking_mass         ! sinking mass flux used in calculating scavenging
    real(r8) :: Lig_scavenge_rate    ! scavenging rate of bound ligand (1/yr)
    real(r8) :: FeLig_scavenge_rate  ! scavenging rate of bound iron (1/yr)

    ! local vars specific to 2 ligand model
    real(kind=r8), parameter :: KFeLig2 = 10.0e11_r8 * 1.0e-6_r8
    real(kind=r8), parameter :: Lig2 = 0.002_r8      ! Lig2 are present everywhere at 2nM concentration
    integer                  :: n                    ! Newton's method loop index
    logical (log_kind)       :: Newton_convergence   ! has Newton's method converged
    real(r8)                 :: FeLig2               ! iron bound to ligand 2
    real(r8)                 :: Fefree_inc           ! increment in free iron in Newton's method
    real(r8)                 :: p0,p1,p2,p3          ! polynomial coefficients of Fefree_fcn
    real(r8)                 :: Fefree_fcn           ! function being solved to determine Fefree
    real(r8)                 :: dFefree_fcn          ! derivative of Fefree_fcn wrt Fefree

    associate(&
         Fe_loc  => tracer_local(marbl_tracer_indices%Fe_ind, :), &
         Lig_loc => tracer_local(marbl_tracer_indices%Lig_ind, :) &
         )

      !-----------------------------------------------------------------------
      !  compute how much iron is bound to ligand
      !-----------------------------------------------------------------------

      if (Lig_cnt == 1) then
        !--------------------------------------------------------------------
        !  Lt  : total ligand; Lf  : unbound ligand; FeL : iron bound to ligand
        !  Fet : total Fe;     Fef : unbound iron
        !
        !  Lt  = Lf  + FeL
        !  Fet = Fef + FeL
        !  K = FeL / (Fef Lf) => FeL = K Fef Lf
        !  =>
        !  Lt  = Lf  + K Fef Lf => Lf = Lt / (1 + K Fef)
        !  Fet = Fef + K Fef Lf
        !  Fet = Fef (1 + K Lf) = Fef (1 + K Lt / (1 + K Fef))
        !  =>
        !  (1 + K Fef) Fet = Fef (1 + K Fef + K Lt)
        !  =>
        !  0 = K Fef^2 + (1 + K (Lt - Fet)) Fef - Fet
        !
        !  FeL = K Fef Lf = K Fef Lt / (1 + K Fef)
        !--------------------------------------------------------------------

        if (Fe_loc(k) > c0) then
          p2 = KFeLig1
          p1 = c1 + KFeLig1*(Lig_loc(k) - Fe_loc(k))
          p0 = -Fe_loc(k)

          ! formula for solution of quadratic equation for Fefree (Fef above)
          !
          ! the positive root arises from + in the quadratic formula,
          ! because p2 (= KFeLig1) is positive
          Fefree(k) = (-p1 + sqrt(p1**2 - c4*p2*p0))/(c2*p2)

          FeLig1 = KFeLig1 * Fefree(k) * Lig_loc(k) / (c1 + KFeLig1 * Fefree(k))
        else ! Fe_loc(k) == 0
          Fefree(k) = c0
          FeLig1 = c0
        end if
        FeLig2 = c0
      end if

      if (Lig_cnt == 2) then
        !--------------------------------------------------------------------
        !  L1t : ligand 1; L1f : unbound ligand 1; FeL1 : iron bound to ligand 1
        !  L2t : ligand 2; L2f : unbound ligand 2; FeL2 : iron bound to ligand 2
        !  Fet : total Fe; Fef : unbound iron
        !
        !  L1t = L1f + FeL1
        !  L2t = L2f + FeL2
        !  Fet = Fef + FeL1 + FeL2
        !  K1 = FeL1 / (Fef L1f) => FeL1 = K1 Fef L1f
        !  K2 = FeL2 / (Fef L2f) => FeL2 = K2 Fef L2f
        !  =>
        !  L1t = L1f + K1 Fef L1f => L1f = L1t / (1 + K1 Fef)
        !  L2t = L2f + K2 Fef L2f => L2f = L2t / (1 + K2 Fef)
        !  Fet = Fef + K1 Fef L1f + K2 Fef L2f
        !      = Fef + K1 Fef L1t / (1 + K1 Fef) + K2 Fef L2t / (1 + K2 Fef)
        !  =>
        !  Fet (1 + K1 Fef) (1 + K2 Fef) = Fef (1 + K1 Fef) (1 + K2 Fef) &
        !     + K1 Fef L1t (1 + K2 Fef) + K2 Fef L2t (1 + K1 Fef)
        !  =>
        !  0 = K1 K2 Fef^3 + ((K1 + K2) + K1 K2 (L1t + L2t - Fet)) Fef^2 &
        !     + (1 + K1 L1t + K2 L2t - (K1+K2) Fet) Fef - Fet
        !
        !  FeL1 = K1 Fef L1f = K1 Fef L1t / (1 + K1 Fef)
        !  FeL2 = K2 Fef L2f = K2 Fef L2t / (1 + K2 Fef)
        !--------------------------------------------------------------------

        if (Fe_loc(k) > c0) then
          p3 = KFeLig1*KFeLig2
          p2 = (KFeLig1+KFeLig2) + KFeLig1*KFeLig2*(Lig_loc(k)+Lig2-Fe_loc(k))
          p1 = c1 + KFeLig1*Lig_loc(k) + KFeLig2*Lig2 - (KFeLig1+KFeLig2)*Fe_loc(k)
          p0 = -Fe_loc(k)

          ! initial iterate for Newton's method
          Fefree(k) = Fe_loc(k)

          Newton_convergence = .false.

          do n = 1,30 ! maximum number of iterations
            ! Horner's method to evaluate p0 + p1*Fefree + p2*Fefree^2 + p3*Fefree^3
            Fefree_fcn  = p0 + Fefree(k)*(p1 + Fefree(k)*(p2 + p3*Fefree(k)))

            ! Horner's method to evaluate p1 + (c2*p2)*Fefree + (c3*p3)*Fefree^2
            dFefree_fcn = p1 + Fefree(k)*((c2*p2) + (c3*p3)*Fefree(k))

            ! Newton's method
            Fefree_inc  = Fefree_fcn / dFefree_fcn
            Fefree(k)      = Fefree(k) - Fefree_inc

            ! Newton's method converges quadratically, once you are close to the solution.
            ! So if relative change in Fefree is small, then we have converged.
            if (abs(Fefree_inc) .le. 1.0e-9_r8 * Fefree(k)) then
              Newton_convergence = .true.
              exit
            end if
          end do

          ! check that Newton's method converged
          if (.not. Newton_convergence) then
            write(log_message,"(A)") 'failure to converge in Newtons method for Fefree'
            call marbl_status_log%log_error(log_message, subname)
            write(log_message, "(A,1x,I0)") 'k =', k
            call marbl_status_log%log_error(log_message, subname)
            write(log_message, "(A,1x,E25.17)") 'Fe_loc =', Fe_loc(k)
            call marbl_status_log%log_error(log_message, subname)
            write(log_message, "(A,1x,E25.17)") 'Lig_loc =', Lig_loc(k)
            call marbl_status_log%log_error(log_message, subname)
            return
          end if

          FeLig1 = KFeLig1 * (Fefree(k) * Lig_loc(k)) / (c1 + KFeLig1*Fefree(k))
          FeLig2 = KFeLig2 * (Fefree(k) * Lig_loc(k)) / (c1 + KFeLig2*Fefree(k))
        else ! Fe_loc(k) == 0
          Fefree(k) = c0
          FeLig1 = c0
          FeLig2 = c0
        end if
      end if

      !-----------------------------------------------------------------------
      !  Compute iron and ligand scavenging :
      !  1) compute in terms of loss per year per unit iron (%/year/fe)
      !  2) scale by sinking mass flux (POM + Dust + bSi + CaCO3)
      !    POC x 12.01 x 3.0 = 36.03 > POM,
      !              remin, particle number, and TEP production all scale with POC
      !  3) Scavenging linear function of sinking mass,
      !     1.6 ng/cm2/s = 500g/m2/yr, 1.44 450g/m2/yr,
      !
      ! scavening of FeLig2 is not implemented
      !-----------------------------------------------------------------------

      sinking_mass = (POC%sflux_in(k)     + POC%hflux_in(k)    ) * (3.0_r8 * 12.01_r8) &
                   + (P_CaCO3%sflux_in(k) + P_CaCO3%hflux_in(k)) * P_CaCO3%mass &
                   + (P_SiO2%sflux_in(k)  + P_SiO2%hflux_in(k) ) * P_SiO2%mass &
                   + (dust%sflux_in(k)    + dust%hflux_in(k)   ) * dust_Fe_scavenge_scale

      Fe_scavenge_rate(k) = parm_Fe_scavenge_rate0 * sinking_mass
      Lig_scavenge_rate   = parm_Lig_scavenge_rate0 * sinking_mass
      FeLig_scavenge_rate = parm_FeLig_scavenge_rate0 * sinking_mass

      Lig_scavenge(k) = yps * FeLig1 * Lig_scavenge_rate
      Fe_scavenge(k)  = yps * (Fefree(k) * Fe_scavenge_rate(k) + FeLig1 * FeLig_scavenge_rate)

    end associate

  end subroutine compute_scavenging

  !***********************************************************************

   subroutine compute_large_detritus_prod(k, domain, marbl_tracer_indices, &
        zooplankton_derived_terms, autotroph_derived_terms, &
        Fe_scavenge, POC, POP, P_CaCO3, P_CaCO3_ALT_CO2, P_SiO2, dust, P_iron, &
        DOP_loss_P_bal, marbl_status_log)

     use marbl_settings_mod, only : Jint_Ptot_thres
     use marbl_settings_mod, only : f_graze_CaCO3_remin
     use marbl_settings_mod, only : f_graze_si_remin
     use marbl_settings_mod, only : Qfe_zoo

     ! Note (mvertens, 2016-02), all the column_sinking_partiles must be intent(inout)
     ! rather than intent(out), since if they were intent(out) they would be automatically
     ! deallocated on entry in this routine (this is not required behavior - but is
     ! standard)

     integer,                              intent(in)    :: k
     type(marbl_domain_type),              intent(in)    :: domain
     type(marbl_tracer_index_type),        intent(in)    :: marbl_tracer_indices
     type(zooplankton_derived_terms_type), intent(in)    :: zooplankton_derived_terms
     type(autotroph_derived_terms_type),   intent(in)    :: autotroph_derived_terms
     real(r8),                             intent(in)    :: Fe_scavenge
     type(column_sinking_particle_type),   intent(inout) :: POC
     type(column_sinking_particle_type),   intent(inout) :: POP
     type(column_sinking_particle_type),   intent(inout) :: P_CaCO3
     type(column_sinking_particle_type),   intent(inout) :: P_CaCO3_ALT_CO2
     type(column_sinking_particle_type),   intent(inout) :: P_SiO2
     type(column_sinking_particle_type),   intent(inout) :: dust
     type(column_sinking_particle_type),   intent(inout) :: P_iron
     real(r8),                             intent(out)   :: DOP_loss_P_bal
     type(marbl_log_type),                 intent(inout) :: marbl_status_log

     !-----------------------------------------------------------------------
     !  local variables
     !-----------------------------------------------------------------------
     character(len=*), parameter :: subname = 'marbl_interior_tendency_mod:compute_large_detritus_prod'
     character(len=char_len)     :: log_message

     integer :: auto_ind

     !-----------------------------------------------------------------------

     associate(                                                            &
          QCaCO3          => autotroph_derived_terms%QCaCO3(:,k),          & ! input
          Qsi             => autotroph_derived_terms%Qsi(:,k),             & ! input
          Qfe             => autotroph_derived_terms%Qfe(:,k),             & ! input
          auto_graze      => autotroph_derived_terms%auto_graze(:,k),      & ! input
          auto_graze_poc  => autotroph_derived_terms%auto_graze_poc(:,k),  & ! input
          auto_agg        => autotroph_derived_terms%auto_agg(:,k),        & ! input
          auto_loss       => autotroph_derived_terms%auto_loss(:,k),       & ! input
          auto_loss_poc   => autotroph_derived_terms%auto_loss_poc(:,k),   & ! input
          remaining_P_pop => autotroph_derived_terms%remaining_P_pop(:,k), & ! input
          zoo_loss_poc    => zooplankton_derived_terms%zoo_loss_poc(:,k),  & ! input
          zoo_graze_poc   => zooplankton_derived_terms%zoo_graze_poc(:,k)  & ! input
          )

     !-----------------------------------------------------------------------
     !  large detritus C
     !-----------------------------------------------------------------------

     POC%prod(k) = sum(zoo_loss_poc(:)) + sum(auto_graze_poc(:)) + sum(zoo_graze_poc(:)) &
          + sum(auto_agg(:)) + sum(auto_loss_poc(:))

     !-----------------------------------------------------------------------
     !  large detritus P
     !-----------------------------------------------------------------------

     POP%prod(k) = Qp_zoo * (sum(zoo_loss_poc(:)) + sum(zoo_graze_poc(:))) + sum(remaining_P_pop(:))

     if (POP%prod(k) < c0) then
        DOP_loss_P_bal = -POP%prod(k)
        POP%prod(k) = c0

        ! write warning to log if omitting DOP_loss_P_bal would have led to a Jint_Ptot error
        if (domain%delta_z(k) * DOP_loss_P_bal .gt. Jint_Ptot_thres) then
           write(log_message,"(A,E11.3e3,A,E11.3e3)") &
                'dz*DOP_loss_P_bal=', domain%delta_z(k) * DOP_loss_P_bal, &
                ' exceeds Jint_Ptot_thres=', Jint_Ptot_thres
           call marbl_status_log%log_warning(log_message, subname, ElemInd=k)
        end if
     else
        DOP_loss_P_bal = c0
     endif

     !-----------------------------------------------------------------------
     !  large detrital CaCO3
     !  33% of CaCO3 is remin when phyto are grazed
     !-----------------------------------------------------------------------

     P_CaCO3%prod(k) = c0
     do auto_ind = 1, autotroph_cnt
        if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
           P_CaCO3%prod(k) = P_CaCO3%prod(k) + ((c1 - f_graze_CaCO3_remin) * auto_graze(auto_ind) &
                + auto_loss(auto_ind) + auto_agg(auto_ind)) * QCaCO3(auto_ind)
        endif
     end do
     P_CaCO3_ALT_CO2%prod(k) = P_CaCO3%prod(k)

     !-----------------------------------------------------------------------
     !  large detritus SiO2
     !  grazed diatom SiO2, 60% is remineralized
     !-----------------------------------------------------------------------

     P_SiO2%prod(k) = c0
     do auto_ind = 1, autotroph_cnt
        if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
           P_SiO2%prod(k) = P_SiO2%prod(k) + Qsi(auto_ind) &
                * ((c1 - f_graze_si_remin) * auto_graze(auto_ind) + auto_agg(auto_ind) &
                + autotroph_settings(auto_ind)%loss_poc * auto_loss(auto_ind))
        endif
     end do

     !-----------------------------------------------------------------------
     ! Dust
     !-----------------------------------------------------------------------

     dust%prod(k) = c0

     !-----------------------------------------------------------------------
     ! Iron
     !-----------------------------------------------------------------------

     P_iron%prod(k) = (sum(zoo_loss_poc(:)) + sum(zoo_graze_poc(:))) * Qfe_zoo + Fe_scavenge

     do auto_ind = 1, autotroph_cnt
        P_iron%prod(k) = P_iron%prod(k) + Qfe(auto_ind) * &
             (auto_agg(auto_ind) + auto_graze_poc(auto_ind) + auto_loss_poc(auto_ind))
     end do

     end associate
   end subroutine compute_large_detritus_prod

   !***********************************************************************

   subroutine compute_particulate_terms(k, domain,                        &
              marbl_particulate_share, p_remin_scalef,                    &
              POC, POP, P_CaCO3, P_CaCO3_ALT_CO2,                         &
              P_SiO2, dust, P_iron, PON_remin, PON_sed_loss, QA_dust_def, &
              tracer_local, carbonate, sed_denitrif, other_remin,         &
              fesedflux, marbl_tracer_indices,                            &
              glo_avg_fields_interior_tendency, marbl_status_log)

     !  Compute outgoing fluxes and remineralization terms. Assumes that
     !  production terms have been set. Incoming fluxes are assumed to be the
     !  outgoing fluxes from the previous level.
     !
     !  It is assumed that there is no production of dust.
     !
     !  Instantaneous remineralization in the bottom cell is implemented by
     !  setting the outgoing flux to zero.
     !
     !  For POC, the hard subclass is the POC flux qualitatively associated
     !  with the ballast flux. The soft subclass is the excess POC flux.
     !
     !  Remineralization for the non-iron particulate pools is computing
     !  by first computing the outgoing flux and then computing the
     !  remineralization from conservation, i.e.
     !     flux_in - flux_out + prod * dz - remin * dz == 0.
     !
     !  For iron, remineralization is first computed from POC remineralization
     !  and then flux_out is computed from conservation. If the resulting
     !  flux_out is negative or should be zero because of the sea floor, the
     !  remineralization is adjusted.
     !  Note: all the sinking iron is in the P_iron%sflux pool, hflux Fe not
     !        explicitly tracked, it is assumed that total iron remin is
     !        proportional to total POC remin.
     !
     !  Based upon Armstrong et al. 2000
     !
     !  July 2002, added temperature effect on remin length scale of
     !  excess POC (all soft POM& Iron) and on SiO2.
     !  new variable passed into ballast, Tfunc, main Temperature function
     !  computed in ecosystem routine.  scaling factor for dissolution
     !  of excess POC, Fe, and Bsi now varies with location (f(temperature)).
     !
     !  Added diffusive iron flux from sediments at depths < 1100m,
     !  based on Johnson et al., 1999, value of 5 umolFe/m2/day,
     !      this value too high, using 2 umolFe/m2/day here
     !
     !  Allow hard fraction of ballast to remin with long length scale 40, 000m
     !     thus ~ 10% of hard ballast remins over 4000m water column.
     !
     !  Sinking dust flux is decreased by assumed instant solubility/dissolution
     !     at ocean surface from the parm_Fe_bioavail.
     !
     !  Modified to allow different Q10 factors for soft POM and bSI remin,
     !  water TEMP is now passed in instead of Tfunc (1/2005, JKM)

     ! !USES:

     use marbl_constants_mod, only : cmperm
     use marbl_settings_mod, only : parm_Fe_desorption_rate0
     use marbl_settings_mod, only : parm_sed_denitrif_coeff
     use marbl_settings_mod, only : particulate_flux_ref_depth
     use marbl_settings_mod, only : caco3_bury_thres_iopt
     use marbl_settings_mod, only : caco3_bury_thres_iopt_fixed_depth
     use marbl_settings_mod, only : caco3_bury_thres_depth
     use marbl_settings_mod, only : caco3_bury_thres_omega_calc
     use marbl_settings_mod, only : POM_bury_frac_max
     use marbl_settings_mod, only : bSi_bury_frac_max
     use marbl_settings_mod, only : o2_sf_o2_range_hi
     use marbl_settings_mod, only : o2_sf_o2_range_lo
     use marbl_settings_mod, only : o2_sf_val_lo_o2
     use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_CaCO3_bury
     use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_POC_bury
     use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_POP_bury
     use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_bSi_bury
     use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff
     use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff
     use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff
     use marbl_interior_tendency_share_mod, only : marbl_interior_tendency_share_export_particulate

     integer (int_kind)                , intent(in)    :: k                   ! vertical model level
     type(marbl_domain_type)           , intent(in)    :: domain
     real (r8)                         , intent(in)    :: p_remin_scalef      ! Particulate Remin Scale Factor
     real (r8), dimension(:)           , intent(in)    :: tracer_local        ! local copies of model tracer concentrations
     type(carbonate_type)              , intent(in)    :: carbonate
     real(r8)                          , intent(in)    :: fesedflux           ! sedimentary Fe input
     real(r8)                          , intent(out)   :: PON_remin           ! remin of PON
     real(r8)                          , intent(out)   :: PON_sed_loss        ! loss of PON to sediments
     type(column_sinking_particle_type), intent(inout) :: POC                 ! base units = nmol C
     type(column_sinking_particle_type), intent(inout) :: POP                 ! base units = nmol P
     type(column_sinking_particle_type), intent(inout) :: P_CaCO3             ! base units = nmol CaCO3
     type(column_sinking_particle_type), intent(inout) :: P_CaCO3_ALT_CO2     ! base units = nmol CaCO3
     type(column_sinking_particle_type), intent(inout) :: P_SiO2              ! base units = nmol SiO2
     type(column_sinking_particle_type), intent(inout) :: dust                ! base units = g
     type(column_sinking_particle_type), intent(inout) :: P_iron              ! base units = nmol Fe
     real (r8)                         , intent(inout) :: QA_dust_def         ! incoming deficit in the QA(dust) POC flux
     real (r8)                         , intent(out)   :: sed_denitrif        ! sedimentary denitrification (umolN/cm^2/s)
     real (r8)                         , intent(out)   :: other_remin         ! sedimentary remin not due to oxic or denitrification
     type(marbl_particulate_share_type), intent(inout) :: marbl_particulate_share
     type(marbl_tracer_index_type)     , intent(in)    :: marbl_tracer_indices
     real (r8)                         , intent(inout) :: glo_avg_fields_interior_tendency(:)
     type(marbl_log_type)              , intent(inout) :: marbl_status_log

     !-----------------------------------------------------------------------
     !  local variables
     !-----------------------------------------------------------------------
     real (r8) :: poc_diss, & ! diss. length used (cm)
          sio2_diss, & ! diss. length varies spatially with O2
          caco3_diss, &
          dust_diss

     character(len=*), parameter :: subname = 'marbl_interior_tendency_mod:compute_particulate_terms'
     character(len=char_len)     :: log_message

     real (r8) :: &
          DECAY_Hard,         & ! scaling factor for dissolution of Hard Ballast
          DECAY_HardDust        ! scaling factor for dissolution of Hard dust

     real (r8) :: &
          decay_POC_E,        & ! scaling factor for dissolution of excess POC
          decay_SiO2,         & ! scaling factor for dissolution of SiO2
          decay_CaCO3,        & ! scaling factor for dissolution of CaCO3
          decay_dust,         & ! scaling factor for dissolution of dust
          POC_PROD_avail,     & ! POC production available for excess POC flux
          new_QA_dust_def,    & ! outgoing deficit in the QA(dust) POC flux
          scalelength,        & ! used to scale dissolution length scales as function of depth
          o2_scalefactor,     & ! used to scale dissolution length scales as function of o2
          ztop,               & ! depth of top of layer
          flux_top, flux_bot, & ! total (sflux+hflux) particulate fluxes at top and bottom of layer
          wbot,               & ! weight for interpolating fluxes vertically
          flux_alt,           & ! flux to floor in alternative units, to match particular parameterizations
          bury_frac,          & ! fraction of flux hitting floor that gets buried
          dz_loc, dzr_loc       ! dz, dzr at a particular i, j location

     real (r8) :: particulate_flux_ref_depth_cm

     integer (int_kind) :: n     ! loop indices

     logical (log_kind) :: poc_error   ! POC error flag
     !-----------------------------------------------------------------------

     associate(                                                                         &
          column_kmt               => domain%kmt,                                       &
          delta_z                  => domain%delta_z,                                   &
          zw                       => domain%zw,                                        &
          O2_loc                   => tracer_local(marbl_tracer_indices%o2_ind),        &
          NO3_loc                  => tracer_local(marbl_tracer_indices%no3_ind),       &
          POC_bury_coeff           => marbl_particulate_share%POC_bury_coeff,           & ! IN/OUT
          POP_bury_coeff           => marbl_particulate_share%POP_bury_coeff,           & ! IN/OUT
          bSi_bury_coeff           => marbl_particulate_share%bSi_bury_coeff            & ! IN/OUT
          )

     !-----------------------------------------------------------------------
     !  initialize local copy of percent sed
     !-----------------------------------------------------------------------
     sed_denitrif = c0
     other_remin = c0

     !-----------------------------------------------------------------------
     !  compute scalelength and decay factors
     !-----------------------------------------------------------------------

     scalelength = c1 ! avoid 'scalelength' may be used uninitialized warning from gfortran
     if (zw(k) < parm_scalelen_z(1)) then
        scalelength = parm_scalelen_vals(1)
     else if (zw(k) >= parm_scalelen_z(size(parm_scalelen_z))) then
        scalelength = parm_scalelen_vals(size(parm_scalelen_z))
     else
        do n = 2, size(parm_scalelen_z)
           if (zw(k) < parm_scalelen_z(n)) then
              scalelength = parm_scalelen_vals(n-1) &
                   + (parm_scalelen_vals(n) - parm_scalelen_vals(n-1)) &
                   * (zw(k) - parm_scalelen_z(n-1))/(parm_scalelen_z(n) - parm_scalelen_z(n-1))
              exit
           endif
        end do
     endif

     if (p_remin_scalef /= c1) scalelength = scalelength / p_remin_scalef

     DECAY_Hard     = exp(-delta_z(k) * p_remin_scalef / 4.0e6_r8)
     DECAY_HardDust = exp(-delta_z(k) * p_remin_scalef / 1.2e8_r8)

     poc_error = .false.
     dz_loc = delta_z(k)

     if (k <= column_kmt) then

        dzr_loc    = c1 / dz_loc
        poc_diss   = POC%diss
        sio2_diss  = P_SiO2%diss
        caco3_diss = P_CaCO3%diss
        dust_diss  = dust%diss

        !-----------------------------------------------------------------------
        !  increase POC diss length scale where O2 concentrations are low
        !-----------------------------------------------------------------------

        if (O2_loc < o2_sf_o2_range_hi) then
           o2_scalefactor = c1 + (o2_sf_val_lo_o2 - c1) * &
                min(c1, (o2_sf_o2_range_hi - O2_loc)/(o2_sf_o2_range_hi - o2_sf_o2_range_lo))
           poc_diss   = poc_diss   * o2_scalefactor
           sio2_diss  = sio2_diss  * o2_scalefactor
           caco3_diss = caco3_diss * o2_scalefactor
           dust_diss  = dust_diss  * o2_scalefactor
        endif

        !-----------------------------------------------------------------------
        !  apply scalelength factor to length scales
        !-----------------------------------------------------------------------

        poc_diss = scalelength * poc_diss
        sio2_diss = scalelength * sio2_diss
        caco3_diss = scalelength * caco3_diss
        dust_diss = scalelength * dust_diss

        !-----------------------------------------------------------------------
        !  decay_POC_E and decay_SiO2 set locally, modified by O2
        !-----------------------------------------------------------------------

        decay_POC_E = exp(-dz_loc / poc_diss)
        decay_SiO2  = exp(-dz_loc / sio2_diss)
        decay_CaCO3 = exp(-dz_loc / caco3_diss)
        decay_dust  = exp(-dz_loc / dust_diss)

        !-----------------------------------------------------------------------
        !  Set outgoing fluxes for non-iron pools.
        !  The outoing fluxes for ballast materials are from the
        !  solution of the coresponding continuous ODE across the model
        !  level. The ODE has a constant source term and linear decay.
        !  It is assumed that there is no sub-surface dust production.
        !-----------------------------------------------------------------------

        P_CaCO3%sflux_out(k) = P_CaCO3%sflux_in(k) * decay_CaCO3 + &
             P_CaCO3%prod(k) * ((c1 - P_CaCO3%gamma) * (c1 - decay_CaCO3) &
             * caco3_diss)

        P_CaCO3%hflux_out(k) = P_CaCO3%hflux_in(k) * DECAY_Hard + &
             P_CaCO3%prod(k) * (P_CaCO3%gamma * dz_loc)

        P_CaCO3_ALT_CO2%sflux_out(k) = P_CaCO3_ALT_CO2%sflux_in(k) * decay_CaCO3 + &
             P_CaCO3_ALT_CO2%prod(k) * ((c1 - P_CaCO3_ALT_CO2%gamma) * (c1 - decay_CaCO3) &
             * caco3_diss)

        P_CaCO3_ALT_CO2%hflux_out(k) = P_CaCO3_ALT_CO2%hflux_in(k) * DECAY_Hard + &
             P_CaCO3_ALT_CO2%prod(k) * (P_CaCO3_ALT_CO2%gamma * dz_loc)

        P_SiO2%sflux_out(k) = P_SiO2%sflux_in(k) * decay_SiO2 + &
             P_SiO2%prod(k) * ((c1 - P_SiO2%gamma) * (c1 - decay_SiO2) &
             * sio2_diss)

        P_SiO2%hflux_out(k) = P_SiO2%hflux_in(k) * DECAY_Hard + &
             P_SiO2%prod(k) * (P_SiO2%gamma * dz_loc)

        dust%sflux_out(k) = dust%sflux_in(k) * decay_dust

        dust%hflux_out(k) = dust%hflux_in(k) * DECAY_HardDust

        !-----------------------------------------------------------------------
        !  Compute how much POC_PROD is available for deficit reduction
        !  and excess POC flux after subtracting off fraction of non-dust
        !  ballast production from net POC_PROD.
        !-----------------------------------------------------------------------

        POC_PROD_avail = POC%prod(k) - &
             P_CaCO3%rho * P_CaCO3%prod(k) - &
             P_SiO2%rho * P_SiO2%prod(k)

        !-----------------------------------------------------------------------
        !  Check for POC production bounds violations
        !-----------------------------------------------------------------------

        if (POC_PROD_avail < c0) then
           poc_error = .true.
        endif

        !-----------------------------------------------------------------------
        !  Compute 1st approximation to new QA_dust_def, the QA_dust
        !  deficit leaving the cell. Ignore POC_PROD_avail at this stage.
        !-----------------------------------------------------------------------

        if (QA_dust_def > 0) then
           new_QA_dust_def = QA_dust_def * &
                (dust%sflux_out(k) + dust%hflux_out(k)) / &
                (dust%sflux_in(k) + dust%hflux_in(k))
        else
           new_QA_dust_def = c0
        endif

        !-----------------------------------------------------------------------
        !  Use POC_PROD_avail to reduce new_QA_dust_def.
        !-----------------------------------------------------------------------

        if (new_QA_dust_def > c0 .and. POC%prod(k) > c0) then
           new_QA_dust_def = new_QA_dust_def - POC_PROD_avail * dz_loc
           if (new_QA_dust_def < c0) then
              POC_PROD_avail = -new_QA_dust_def * dzr_loc
              new_QA_dust_def = c0
           else
              POC_PROD_avail = c0
           endif
        endif

        QA_dust_def = new_QA_dust_def

        !-----------------------------------------------------------------------
        !  Compute outgoing POC fluxes. QA POC flux is computing using
        !  ballast fluxes and new_QA_dust_def. If no QA POC flux came in
        !  and no production occured, then no QA POC flux goes out. This
        !  shortcut is present to avoid roundoff cancellation errors from
        !  the dust%rho * dust_flux_out - QA_dust_def computation.
        !  Any POC_PROD_avail still remaining goes into excess POC flux.
        !-----------------------------------------------------------------------

        if (POC%hflux_in(k) == c0 .and. POC%prod(k) == c0) then
           POC%hflux_out(k) = c0
        else
           POC%hflux_out(k) = &
              P_CaCO3%rho * (P_CaCO3%sflux_out(k) + P_CaCO3%hflux_out(k)) + &
              P_SiO2%rho  * (P_SiO2%sflux_out(k) + P_SiO2%hflux_out(k)) + &
              dust%rho    * (dust%sflux_out(k) + dust%hflux_out(k)) - new_QA_dust_def
           POC%hflux_out(k) = max(POC%hflux_out(k), c0)
        endif

        POC%sflux_out(k) = POC%sflux_in(k) * decay_POC_E + &
             POC_PROD_avail * ((c1 - decay_POC_E) * poc_diss)

        !-----------------------------------------------------------------------
        !  Compute remineralization terms. It is assumed that there is no
        !  sub-surface dust production.
        !-----------------------------------------------------------------------

        P_CaCO3%remin(k) = P_CaCO3%prod(k) + &
             ((P_CaCO3%sflux_in(k) - P_CaCO3%sflux_out(k)) + &
             (P_CaCO3%hflux_in(k) - P_CaCO3%hflux_out(k))) * dzr_loc

        P_CaCO3_ALT_CO2%remin(k) = P_CaCO3_ALT_CO2%prod(k) + &
             ((P_CaCO3_ALT_CO2%sflux_in(k) - P_CaCO3_ALT_CO2%sflux_out(k)) + &
             (P_CaCO3_ALT_CO2%hflux_in(k) - P_CaCO3_ALT_CO2%hflux_out(k))) * dzr_loc

        P_SiO2%remin(k) = P_SiO2%prod(k) + &
             ((P_SiO2%sflux_in(k) - P_SiO2%sflux_out(k)) + &
             (P_SiO2%hflux_in(k) - P_SiO2%hflux_out(k))) * dzr_loc

        POC%remin(k) = POC%prod(k) + &
             ((POC%sflux_in(k) - POC%sflux_out(k)) + &
             (POC%hflux_in(k) - POC%hflux_out(k))) * dzr_loc

        PON_remin = Q * POC%remin(k)

        dust%remin(k) = &
             ((dust%sflux_in(k) - dust%sflux_out(k)) + &
             (dust%hflux_in(k) - dust%hflux_out(k))) * dzr_loc

        !-----------------------------------------------------------------------
        !  Compute iron remineralization and flux out.
        !-----------------------------------------------------------------------

        if (POC%sflux_in(k) + POC%hflux_in(k) == c0) then
           P_iron%remin(k) = (POC%remin(k) * parm_Red_Fe_C)
        else
           P_iron%remin(k) = (POC%remin(k) * &
                (P_iron%sflux_in(k) + P_iron%hflux_in(k)) / &
                (POC%sflux_in(k) + POC%hflux_in(k)))
        endif

        ! add term for desorption of iron from sinking particles
        P_iron%remin(k) = P_iron%remin(k) +                &
             (P_iron%sflux_in(k) * parm_Fe_desorption_rate0)

        P_iron%sflux_out(k) = P_iron%sflux_in(k) + dz_loc * &
             ((c1 - P_iron%gamma) * P_iron%prod(k) - P_iron%remin(k))

        if (P_iron%sflux_out(k) < c0) then
           P_iron%sflux_out(k) = c0
           P_iron%remin(k) = P_iron%sflux_in(k) * dzr_loc + &
                (c1 - P_iron%gamma) * P_iron%prod(k)
        endif

        !-----------------------------------------------------------------------
        !  Compute iron release from dust remin/dissolution and add in Fe source
        !  from sediments
        !-----------------------------------------------------------------------


        P_iron%remin(k) = P_iron%remin(k) &
             + dust%remin(k) * dust_to_Fe &
             + (fesedflux * dzr_loc)

        P_iron%hflux_out(k) = P_iron%hflux_in(k)

 !------------------------------------------------------------------------
 !  compute POP remin and flux out, following code for iron
 !------------------------------------------------------------------------

        POP%remin(k) = c0
        if (POC%sflux_in(k) + POC%hflux_in(k) > c0) then
           POP%remin(k) = POC%remin(k) * &
              ((POP%sflux_in(k) + POP%hflux_in(k)) / &
               (POC%sflux_in(k) + POC%hflux_in(k)))
        else if (POC%prod(k) > c0) then
           POP%remin(k) = POC%remin(k) * (POP%prod(k) / POC%prod(k))
        end if

        POP%sflux_out(k) = POP%sflux_in(k) + dz_loc * &
           ((c1 - POP%gamma) * POP%prod(k) - POP%remin(k))

        if (POP%sflux_out(k) < c0) then
           POP%sflux_out(k) = c0
           POP%remin(k) = POP%sflux_in(k) * dzr_loc + &
              (c1 - POP%gamma) * POP%prod(k)
        endif

        POP%hflux_out(k) = POP%hflux_in(k)

     else

        dzr_loc = c0 ! avoid 'dzr_loc' may be used uninitialized warning from gfortran
        POC%remin(k) = c0
        POC%sflux_out(k) = c0
        POC%hflux_out(k) = c0

     endif

     ! Save some fields for use by other modules
     call marbl_interior_tendency_share_export_particulate(k, POC, DECAY_Hard, &
          POC_PROD_avail, decay_POC_E, decay_CaCO3, poc_diss, caco3_diss, &
          marbl_particulate_share)

     ! extract particulate fluxes at particulate_flux_ref_depth, if this layer contains that depth
     if (k .gt. 1) then
       ztop = zw(k-1)
     else
       ztop = c0
     end if
     particulate_flux_ref_depth_cm = cmperm * particulate_flux_ref_depth
     if (ztop .le. particulate_flux_ref_depth_cm .and. particulate_flux_ref_depth_cm .lt. zw(k)) then
       if (k <= column_kmt) then
         if (particulate_flux_ref_depth_cm .eq. ztop) then
           ! expressions are simplified if particulate_flux_ref_depth is exactly the top of the layer
           POC%flux_at_ref_depth     = POC%sflux_in(k)     + POC%hflux_in(k)
           POP%flux_at_ref_depth     = POP%sflux_in(k)     + POP%hflux_in(k)
           P_CaCO3%flux_at_ref_depth = P_CaCO3%sflux_in(k) + P_CaCO3%hflux_in(k)
           P_SiO2%flux_at_ref_depth  = P_SiO2%sflux_in(k)  + P_SiO2%hflux_in(k)
           P_iron%flux_at_ref_depth  = P_iron%sflux_in(k)  + P_iron%hflux_in(k)
         else
           wbot = (particulate_flux_ref_depth_cm - ztop) / delta_z(k)

           flux_top = POC%sflux_in(k) + POC%hflux_in(k)
           flux_bot = POC%sflux_out(k) + POC%hflux_out(k)
           POC%flux_at_ref_depth = flux_top + wbot * (flux_bot - flux_top)

           flux_top = POP%sflux_in(k) + POP%hflux_in(k)
           flux_bot = POP%sflux_out(k) + POP%hflux_out(k)
           POP%flux_at_ref_depth = flux_top + wbot * (flux_bot - flux_top)

           flux_top = P_CaCO3%sflux_in(k) + P_CaCO3%hflux_in(k)
           flux_bot = P_CaCO3%sflux_out(k) + P_CaCO3%hflux_out(k)
           P_CaCO3%flux_at_ref_depth = flux_top + wbot * (flux_bot - flux_top)

           flux_top = P_SiO2%sflux_in(k) + P_SiO2%hflux_in(k)
           flux_bot = P_SiO2%sflux_out(k) + P_SiO2%hflux_out(k)
           P_SiO2%flux_at_ref_depth = flux_top + wbot * (flux_bot - flux_top)

           flux_top = P_iron%sflux_in(k) + P_iron%hflux_in(k)
           flux_bot = P_iron%sflux_out(k) + P_iron%hflux_out(k)
           P_iron%flux_at_ref_depth = flux_top + wbot * (flux_bot - flux_top)
         end if
       else
         POC%flux_at_ref_depth     = c0
         POP%flux_at_ref_depth     = c0
         P_CaCO3%flux_at_ref_depth = c0
         P_SiO2%flux_at_ref_depth  = c0
         P_iron%flux_at_ref_depth  = c0
       end if
     end if

     !-----------------------------------------------------------------------
     !  Bottom Sediments Cell?
     !  If so compute sedimentary burial and denitrification N losses.
     !  Using empirical relations from Bohlen et al., 2012 (doi:10.1029/2011GB004198) for Sed Denitrification
     !  other_remin estimates organic matter remineralized in the sediments
     !      by the processes other than oxic remin and denitrification (SO4 and CO2,
     !      etc..)
     !      based on Soetaert et al., 1996, varies between 10% and 50%
     !      0.4_r8 is a coefficient with units mmolC/cm2/yr sinking flux,
     !      other_remin is 50% above this high flux value,
     !      In special case where bottom O2 has been depleted to < 1.0 uM,
     !               all sedimentary remin is due to DENITRIFICATION + other_remin
     !  POC burial from Dunne et al. 2007 (doi:10.1029/2006GB002907), maximum of 80% burial efficiency imposed
     !  Bsi preservation in sediments based on
     !     Ragueneau et al. 2000 (doi:10.1016/S0921-8181(00)00052-7)
     !  Calcite is preserved in sediments above a threshold depth,
     !     which is based on caco3_bury_thres_opt.
     !-----------------------------------------------------------------------

     POC%sed_loss(k)             = c0
     POP%sed_loss(k)             = c0
     P_SiO2%sed_loss(k)          = c0
     P_CaCO3%sed_loss(k)         = c0
     P_CaCO3_ALT_CO2%sed_loss(k) = c0
     P_iron%sed_loss(k)          = c0
     dust%sed_loss(k)            = c0

     PON_sed_loss        = c0

     if (k == column_kmt) then

        POC%to_floor = POC%sflux_out(k) + POC%hflux_out(k)

        if (POC%to_floor > c0) then
           flux_alt = POC%to_floor*mpercm*spd ! convert to mmol/m^2/day

           ! first compute burial efficiency, then compute loss to sediments
           bury_frac = 0.013_r8 + 0.53_r8 * flux_alt*flux_alt / (7.0_r8 + flux_alt)**2
           if (p_remin_scalef /= c1) bury_frac = c1 - p_remin_scalef * (c1 - bury_frac)
           POC%sed_loss(k) = POC%to_floor * min(POM_bury_frac_max, POC_bury_coeff * bury_frac)

           PON_sed_loss = PON_bury_coeff * Q * POC%sed_loss(k)

           POP%to_floor = POP%sflux_out(k) + POP%hflux_out(k)
           POP%sed_loss(k) = POP%to_floor * min(POM_bury_frac_max, POP_bury_coeff * bury_frac)

           if (ladjust_bury_coeff) then
              glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_POC_bury) = POC%sed_loss(k)
              if (POC_bury_coeff * bury_frac < POM_bury_frac_max) then
                 glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff) = &
                      POC%to_floor * bury_frac
              else
                 glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff) = c0
              endif

              glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_POP_bury) = POP%sed_loss(k)
              if (POP_bury_coeff * bury_frac < POM_bury_frac_max) then
                 glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff) = &
                      POP%to_floor * bury_frac
              else
                 glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff) = c0
              endif
           endif

           sed_denitrif = dzr_loc * parm_sed_denitrif_coeff * POC%to_floor &
                * (0.06_r8 + 0.19_r8 * 0.99_r8**(O2_loc-NO3_loc))

           flux_alt = POC%to_floor*1.0e-6_r8*spd*365.0_r8 ! convert to mmol/cm^2/year
           other_remin = dzr_loc &
                * min(min(0.1_r8 + flux_alt, 0.5_r8) * (POC%to_floor - POC%sed_loss(k)), &
                (POC%to_floor - POC%sed_loss(k) - (sed_denitrif*dz_loc*denitrif_C_N)))

           !----------------------------------------------------------------------------------
           !              if bottom water O2 is depleted, assume all remin is denitrif + other
           !----------------------------------------------------------------------------------

           if (O2_loc < c1) then
              other_remin = dzr_loc * &
                   (POC%to_floor - POC%sed_loss(k) - (sed_denitrif*dz_loc*denitrif_C_N))
           endif

        else

           POP%to_floor = POP%sflux_out(k) + POP%hflux_out(k)

           if (ladjust_bury_coeff) then
              glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_POC_bury) = c0
              glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff) = c0

              glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_POP_bury) = c0
              glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff) = c0
           endif

        endif

        P_SiO2%to_floor = P_SiO2%sflux_out(k) + P_SiO2%hflux_out(k)
        flux_alt = P_SiO2%to_floor*mpercm*spd ! convert to mmol/m^2/day
        ! first compute burial efficiency, then compute loss to sediments
        if (flux_alt > c2) then
           bury_frac = 0.2_r8
        else
           bury_frac = 0.04_r8
        endif
        if (p_remin_scalef /= c1) bury_frac = c1 - p_remin_scalef * (c1 - bury_frac)
        if (bSi_bury_coeff * bury_frac < bSi_bury_frac_max) then
           P_SiO2%sed_loss(k) = P_SiO2%to_floor * bSi_bury_coeff * bury_frac
        else
           P_SiO2%sed_loss(k) = P_SiO2%to_floor * bSi_bury_frac_max
        endif

        if (ladjust_bury_coeff) then
           glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_bSi_bury) = P_SiO2%sed_loss(k)
           if (bSi_bury_coeff * bury_frac < bSi_bury_frac_max) then
              glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff) = &
                   P_SiO2%to_floor * bury_frac
           else
              glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff) = c0
           endif
        endif

        P_CaCO3%to_floor         = P_CaCO3%sflux_out(k)         + P_CaCO3%hflux_out(k)
        P_CaCO3_ALT_CO2%to_floor = P_CaCO3_ALT_CO2%sflux_out(k) + P_CaCO3_ALT_CO2%hflux_out(k)

        if (caco3_bury_thres_iopt == caco3_bury_thres_iopt_fixed_depth) then
           if (zw(k) < caco3_bury_thres_depth) then
              P_CaCO3%sed_loss(k)         = P_CaCO3%to_floor
              P_CaCO3_ALT_CO2%sed_loss(k) = P_CaCO3_ALT_CO2%to_floor
           endif
        else ! caco3_bury_thres_iopt = caco3_bury_thres_iopt_omega_calc
           if (carbonate%CO3(k) > caco3_bury_thres_omega_calc * carbonate%CO3_sat_calcite(k)) then
              P_CaCO3%sed_loss(k) = P_CaCO3%to_floor
           endif
           if (carbonate%CO3_ALT_CO2(k) > caco3_bury_thres_omega_calc * carbonate%CO3_sat_calcite(k)) then
              P_CaCO3_ALT_CO2%sed_loss(k) = P_CaCO3_ALT_CO2%to_floor
           endif
        endif

        if (ladjust_bury_coeff) then
           glo_avg_fields_interior_tendency(glo_avg_field_ind_interior_tendency_CaCO3_bury) = P_CaCO3%sed_loss(k)
        endif

        !----------------------------------------------------------------------------------
        !  Update sinking fluxes and remin fluxes, accounting for sediments.
        !  flux used to hold sinking fluxes before update.
        !----------------------------------------------------------------------------------

        if (P_CaCO3%to_floor > c0) then
           P_CaCO3%remin(k) = P_CaCO3%remin(k) &
                + ((P_CaCO3%to_floor - P_CaCO3%sed_loss(k)) * dzr_loc)
        endif

        if (P_CaCO3_ALT_CO2%to_floor > c0) then
           P_CaCO3_ALT_CO2%remin(k) = P_CaCO3_ALT_CO2%remin(k) &
                + ((P_CaCO3_ALT_CO2%to_floor - P_CaCO3_ALT_CO2%sed_loss(k)) * dzr_loc)
        endif

        if (P_SiO2%to_floor > c0) then
           P_SiO2%remin(k) = P_SiO2%remin(k) &
                + ((P_SiO2%to_floor - P_SiO2%sed_loss(k)) * dzr_loc)
        endif

        if (POC%to_floor > c0) then
           POC%remin(k) = POC%remin(k) &
                + ((POC%to_floor - POC%sed_loss(k)) * dzr_loc)

           PON_remin = PON_remin &
                + ((Q * POC%to_floor - PON_sed_loss) * dzr_loc)
        endif

        if (POP%to_floor > c0) then
           POP%remin(k) = POP%remin(k) &
                + ((POP%to_floor - POP%sed_loss(k)) * dzr_loc)
        endif

        !-----------------------------------------------------------------------
        !   Remove all Piron and dust that hits bottom, sedimentary Fe source
        !        accounted for by fesedflux elsewhere.
        !-----------------------------------------------------------------------

        P_iron%to_floor = P_iron%sflux_out(k) + P_iron%hflux_out(k)
        if (P_iron%to_floor > c0) then
           P_iron%sed_loss(k) = P_iron%to_floor
        endif

        dust%to_floor = dust%sflux_out(k) + dust%hflux_out(k)
        dust%sed_loss(k) = dust%to_floor

     endif

     if (poc_error) then
       write(log_message, "(A)") "mass ratio of ballast production exceeds POC production"
       call marbl_status_log%log_error(log_message, subname)
     endif

     end associate

   end subroutine compute_particulate_terms

  !***********************************************************************

  subroutine compute_Lig_terms(km, PAR_nsubcols, marbl_tracer_indices, &
       POC_remin, DOC_prod, PAR, dz1, tracer_local, Lig_scavenge, &
       photoFe, Lig_prod, Lig_photochem, Lig_deg, Lig_loss)

    use marbl_settings_mod, only : remin_to_Lig
    use marbl_settings_mod, only : parm_Lig_degrade_rate0

    integer(int_kind),             intent(in)  :: km
    integer(int_kind),             intent(in)  :: PAR_nsubcols
    type(marbl_tracer_index_type), intent(in)  :: marbl_tracer_indices
    real(r8),                      intent(in)  :: POC_remin(km)
    real(r8),                      intent(in)  :: DOC_prod(km)
    type(marbl_PAR_type),          intent(in)  :: PAR
    real(r8),                      intent(in)  :: dz1
    real(r8),                      intent(in)  :: tracer_local(marbl_tracer_indices%total_cnt, km)
    real(r8),                      intent(in)  :: Lig_scavenge(km)
    real(r8),                      intent(in)  :: photoFe(autotroph_cnt,km)
    real(r8),                      intent(out) :: Lig_prod(km)
    real(r8),                      intent(out) :: Lig_photochem(km)
    real(r8),                      intent(out) :: Lig_deg(km)
    real(r8),                      intent(out) :: Lig_loss(km)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: subcol_ind
    real(r8) :: rate_per_sec_subcol ! (1/sec)
    real(r8) :: rate_per_sec        ! (1/sec)

    associate(&
              PAR_col_frac => PAR%col_frac(:), &
              Lig_loc      => tracer_local(marbl_tracer_indices%lig_ind,:) &
              )

      !-----------------------------------------------------------------------
      !  Ligand production tied to remin of particulate organic matter
      !      and production of DOM
      !-----------------------------------------------------------------------

      Lig_prod(:) = (POC_remin(:) * remin_to_Lig) + (DOC_prod(:) * remin_to_Lig)

      !----------------------------------------------------------------------
      !  Ligand losses due to photochemistry in first ocean layer
      !  ligand photo-oxidation a function of PAR (2/5/2015)
      !----------------------------------------------------------------------

      Lig_photochem(:) = c0

      ! Initialize top level
      rate_per_sec = c0
      do subcol_ind = 1, PAR_nsubcols
        if ((PAR_col_frac(subcol_ind) > c0) .and. (PAR%interface(0,subcol_ind) > 1.0_r8)) then
          rate_per_sec_subcol = (log(PAR%interface(0,subcol_ind))*0.4373_r8)*(10.0e2_r8/dz1)*((12.0_r8/3.0_r8)*yps) ! 1/(3 months)
          rate_per_sec = rate_per_sec + PAR_col_frac(subcol_ind) * rate_per_sec_subcol
        endif
      end do
      Lig_photochem(1) = Lig_loc(1) * rate_per_sec

      !----------------------------------------------------------------------
      !  Ligand losses due to uptake/degradation (by hetrotrophic bacteria)
      !----------------------------------------------------------------------

      Lig_deg(:) = POC_remin(:) * parm_Lig_degrade_rate0

      !----------------------------------------------------------------------
      !  total ligand loss
      !----------------------------------------------------------------------

      Lig_loss(:) = Lig_scavenge(:) + 0.20_r8*sum(photoFe(:,:),dim=1) + Lig_photochem(:) + Lig_deg(:)

    end associate

  end subroutine compute_Lig_terms

  !***********************************************************************

  subroutine compute_nitrif(kmt, km, PAR_nsubcols, marbl_tracer_indices, PAR, tracer_local, nitrif)

    !-----------------------------------------------------------------------
    !  nitrate & ammonium
    !  nitrification in low light
    !  use exponential decay of PAR across model level to compute taper factor
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : parm_nitrif_par_lim
    use marbl_settings_mod, only : parm_kappa_nitrif

    integer(int_kind),             intent(in)  :: kmt
    integer(int_kind),             intent(in)  :: km
    integer(int_kind),             intent(in)  :: PAR_nsubcols
    type(marbl_tracer_index_type), intent(in)  :: marbl_tracer_indices
    type(marbl_PAR_type),          intent(in)  :: PAR
    real(r8),                      intent(in)  :: tracer_local(marbl_tracer_indices%total_cnt, kmt)
    real(r8),                      intent(out) :: nitrif(km)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: k, subcol_ind
    real(r8) :: nitrif_subcol
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    ! skip computations for non-active layers or NH4 is 0
    !-----------------------------------------------------------------------

    nitrif(:) = c0

    associate(                                                          &
         PAR_col_frac => PAR%col_frac(:),                               &
         kPARdz       => PAR%kPARdz(:),                                 &
         NH4_loc      => tracer_local(marbl_tracer_indices%NH4_ind, :)  &
         )
      do k=1,kmt
        if (NH4_loc(k) == c0) cycle
        do subcol_ind = 1, PAR_nsubcols
          if (PAR_col_frac(subcol_ind) > c0) then
            if (PAR%interface(k,subcol_ind) < parm_nitrif_par_lim) then
              nitrif_subcol = parm_kappa_nitrif * NH4_loc(k)
              if (PAR%interface(k-1,subcol_ind) > parm_nitrif_par_lim) then
                nitrif_subcol = nitrif_subcol * &
                log(PAR%interface(k,subcol_ind) / parm_nitrif_par_lim) / (-KPARdz(k))
              end if
              nitrif(k) = nitrif(k) + PAR_col_frac(subcol_ind) * nitrif_subcol
            end if
          end if
        end do
      end do
    end associate

  end subroutine compute_nitrif

  !***********************************************************************

  subroutine compute_denitrif(km, marbl_tracer_indices, tracer_local, DOC_remin, DOCr_remin,   &
       POC_remin, other_remin, sed_denitrif, denitrif)

    !-----------------------------------------------------------------------
    !  Compute denitrification under low O2 conditions
    !-----------------------------------------------------------------------

    integer,                       intent(in)    :: km
    type(marbl_tracer_index_type), intent(in)    :: marbl_tracer_indices
    real(r8),                      intent(in)    :: tracer_local(marbl_tracer_indices%total_cnt, km)
    real(r8),                      intent(in)    :: DOC_remin(km)
    real(r8),                      intent(in)    :: DOCr_remin(km)
    real(r8),                      intent(in)    :: POC_remin(km)
    real(r8),                      intent(in)    :: other_remin(km)
    real(r8),                      intent(inout) :: sed_denitrif(km)
    real(r8),                      intent(out)   :: denitrif(km)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: k
    real(r8) :: work
    !-----------------------------------------------------------------------

    associate(&
              O2_loc  => tracer_local(marbl_tracer_indices%o2_ind,:),  &
              NO3_loc => tracer_local(marbl_tracer_indices%no3_ind,:)  &
             )
      do k=1,km
        work = ((parm_o2_min + parm_o2_min_delta) - O2_loc(k)) / parm_o2_min_delta
        work = min(max(work, c0), c1)
        denitrif(k) = work * ((DOC_remin(k) + DOCr_remin(k) + POC_remin(k) * (c1 - POCremin_refract) &
                 - other_remin(k)) / denitrif_C_N  - sed_denitrif(k))

        ! scale down denitrif if computed rate would consume all NO3 in 10 days
        if (NO3_loc(k) < ((c10*spd)*(denitrif(k)+sed_denitrif(k)))) then
          work = NO3_loc(k) / ((c10*spd)*(denitrif(k)+sed_denitrif(k)))
          denitrif(k) = denitrif(k) * work
          sed_denitrif(k) = sed_denitrif(k) * work
        end if
      end do

    end associate

  end subroutine compute_denitrif

  !***********************************************************************

  subroutine compute_local_tendencies(km, marbl_tracer_indices, autotroph_derived_terms, &
       zooplankton_derived_terms, dissolved_organic_matter, nitrif, denitrif, sed_denitrif, &
       Fe_scavenge, Lig_prod, Lig_loss, P_iron_remin, POC_remin, POP_remin, P_SiO2_remin, &
       P_CaCO3_remin, P_CaCO3_ALT_CO2_remin, other_remin, PON_remin, tracer_local, &
       o2_consumption_scalef, o2_production, o2_consumption, interior_tendencies)

    integer,                              intent(in)    :: km
    type(marbl_tracer_index_type),        intent(in)    :: marbl_tracer_indices
    type(autotroph_derived_terms_type),   intent(in)    :: autotroph_derived_terms
    type(zooplankton_derived_terms_type), intent(in)    :: zooplankton_derived_terms
    type(dissolved_organic_matter_type),  intent(in)    :: dissolved_organic_matter
    real(r8),                             intent(in)    :: nitrif(km)
    real(r8),                             intent(in)    :: denitrif(km)
    real(r8),                             intent(in)    :: sed_denitrif(km)
    real(r8),                             intent(in)    :: Fe_scavenge(km)
    real(r8),                             intent(in)    :: Lig_prod(km)
    real(r8),                             intent(in)    :: Lig_loss(km)
    real(r8),                             intent(in)    :: P_iron_remin(km)
    real(r8),                             intent(in)    :: POC_remin(km)
    real(r8),                             intent(in)    :: POP_remin(km)
    real(r8),                             intent(in)    :: P_SiO2_remin(km)
    real(r8),                             intent(in)    :: P_CaCO3_remin(km)
    real(r8),                             intent(in)    :: P_CaCO3_ALT_CO2_remin(km)
    real(r8),                             intent(in)    :: other_remin(km)
    real(r8),                             intent(in)    :: PON_remin(km)
    real(r8),                             intent(in)    :: tracer_local(marbl_tracer_indices%total_cnt, km)
    real(r8),                             intent(in)    :: o2_consumption_scalef(km)
    real(r8),                             intent(out)   :: o2_production(km)
    real(r8),                             intent(out)   :: o2_consumption(km)
    real(r8),                             intent(inout) :: interior_tendencies(marbl_tracer_indices%total_cnt, km)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: k, auto_ind, zoo_ind, n
    real(r8) :: auto_sum
    !-----------------------------------------------------------------------

    associate(                                                            &
         thetaC          => autotroph_derived_terms%thetaC(:,:),          & ! current Chl/C ratio (mg Chl/mmol C)
         QCaCO3          => autotroph_derived_terms%QCaCO3(:,:),          & ! current CaCO3/C ratio (mmol CaCO3/mmol C)
         Qp              => autotroph_derived_terms%Qp(:,:),              & ! current P/C ratio (mmol P/mmol C)
         Qfe             => autotroph_derived_terms%Qfe(:,:),             & ! current Fe/C ratio (mmol Fe/mmol C)
         Qsi             => autotroph_derived_terms%Qsi(:,:),             & ! current Si/C ratio (mmol Si/mmol C)
         NO3_V           => autotroph_derived_terms%NO3_V(:,:),           & ! nitrate uptake (mmol NO3/m^3/sec)
         NH4_V           => autotroph_derived_terms%NH4_V(:,:),           & ! ammonium uptake (mmol NH4/m^3/sec)
         PO4_V           => autotroph_derived_terms%PO4_V(:,:),           & ! PO4 uptake (mmol PO4/m^3/sec)
         DOP_V           => autotroph_derived_terms%DOP_V(:,:),           & ! DOP uptake (mmol DOP/m^3/sec)
         photoC          => autotroph_derived_terms%photoC(:,:),          & ! C-fixation (mmol C/m^3/sec)
         photoFe         => autotroph_derived_terms%photoFe(:,:),         & ! iron uptake
         photoSi         => autotroph_derived_terms%photoSi(:,:),         & ! silicon uptake (mmol Si/m^3/sec)
         photoacc        => autotroph_derived_terms%photoacc(:,:),        & ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
         auto_loss       => autotroph_derived_terms%auto_loss(:,:),       & ! autotroph non-grazing mort (mmol C/m^3/sec)
         auto_loss_dic   => autotroph_derived_terms%auto_loss_dic(:,:),   & ! auto_loss routed to dic (mmol C/m^3/sec)
         auto_loss_doc   => autotroph_derived_terms%auto_loss_doc(:,:),   & ! auto_loss routed to doc (mmol C/m^3/sec)
         auto_agg        => autotroph_derived_terms%auto_agg(:,:),        & ! autotroph aggregation (mmol C/m^3/sec)
         auto_graze      => autotroph_derived_terms%auto_graze(:,:),      & ! autotroph grazing rate (mmol C/m^3/sec)
         auto_graze_zoo  => autotroph_derived_terms%auto_graze_zoo(:,:),  & ! auto_graze routed to zoo (mmol C/m^3/sec)
         auto_graze_dic  => autotroph_derived_terms%auto_graze_dic(:,:),  & ! auto_graze routed to dic (mmol C/m^3/sec)
         auto_graze_doc  => autotroph_derived_terms%auto_graze_doc(:,:),  & ! auto_graze routed to doc (mmol C/m^3/sec)
         CaCO3_form      => autotroph_derived_terms%CaCO3_form(:,:),      & ! prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
         Nfix            => autotroph_derived_terms%Nfix(:,:),            & ! total Nitrogen fixation (mmol N/m^3/sec)
         Nexcrete        => autotroph_derived_terms%Nexcrete(:,:),        & ! fixed N excretion
         remaining_P_dip => autotroph_derived_terms%remaining_P_dip(:,:), & ! remaining_P from mort routed to remin

         x_graze_zoo     => zooplankton_derived_terms%x_graze_zoo(:,:),   & ! {auto, zoo}_graze routed to zoo (mmol C/m^3/sec)
         zoo_graze       => zooplankton_derived_terms%zoo_graze(:,:),     & ! zooplankton losses due to grazing (mmol C/m^3/sec)
         zoo_graze_zoo   => zooplankton_derived_terms%zoo_graze_zoo(:,:), & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
         zoo_graze_dic   => zooplankton_derived_terms%zoo_graze_dic(:,:), & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
         zoo_graze_doc   => zooplankton_derived_terms%zoo_graze_doc(:,:), & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
         zoo_loss        => zooplankton_derived_terms%zoo_loss(:,:),      & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
         zoo_loss_dic    => zooplankton_derived_terms%zoo_loss_dic(:,:),  & ! zoo_loss routed to dic (mmol C/m^3/sec)
         zoo_loss_doc    => zooplankton_derived_terms%zoo_loss_doc(:,:),  & ! zoo_loss routed to doc (mmol C/m^3/sec)

         DOC_prod        => dissolved_organic_matter%DOC_prod(:),       & ! production of DOC (mmol C/m^3/sec)
         DOC_remin       => dissolved_organic_matter%DOC_remin(:),      & ! remineralization of DOC (mmol C/m^3/sec)
         DOCr_remin      => dissolved_organic_matter%DOCr_remin(:),     & ! remineralization of DOCr
         DON_prod        => dissolved_organic_matter%DON_prod(:),       & ! production of DON
         DON_remin       => dissolved_organic_matter%DON_remin(:),      & ! remineralization of DON
         DONr_remin      => dissolved_organic_matter%DONr_remin(:),     & ! remineralization of DONr
         DOP_prod        => dissolved_organic_matter%DOP_prod(:),       & ! production of DOP
         DOP_remin       => dissolved_organic_matter%DOP_remin(:),      & ! remineralization of DOP
         DOPr_remin      => dissolved_organic_matter%DOPr_remin(:),     & ! remineralization of DOPr
         DOP_loss_P_bal  => dissolved_organic_matter%DOP_loss_P_bal(:), & ! DOP loss, due to P budget balancing

         O2_loc            => tracer_local(marbl_tracer_indices%O2_ind, :), &
         po4_ind           => marbl_tracer_indices%po4_ind,                 &
         no3_ind           => marbl_tracer_indices%no3_ind,                 &
         sio3_ind          => marbl_tracer_indices%sio3_ind,                &
         nh4_ind           => marbl_tracer_indices%nh4_ind,                 &
         fe_ind            => marbl_tracer_indices%fe_ind,                  &
         lig_ind           => marbl_tracer_indices%lig_ind,                 &
         o2_ind            => marbl_tracer_indices%o2_ind,                  &
         dic_ind           => marbl_tracer_indices%dic_ind,                 &
         dic_alt_co2_ind   => marbl_tracer_indices%dic_alt_co2_ind,         &
         alk_ind           => marbl_tracer_indices%alk_ind,                 &
         alk_alt_co2_ind   => marbl_tracer_indices%alk_alt_co2_ind,         &
         doc_ind           => marbl_tracer_indices%doc_ind,                 &
         don_ind           => marbl_tracer_indices%don_ind,                 &
         dop_ind           => marbl_tracer_indices%dop_ind,                 &
         dopr_ind          => marbl_tracer_indices%dopr_ind,                &
         donr_ind          => marbl_tracer_indices%donr_ind,                &
         docr_ind          => marbl_tracer_indices%docr_ind                 &
         )

      do k=1, km
        !-----------------------------------------------------------------------
        !  nitrate & ammonium
        !-----------------------------------------------------------------------

        interior_tendencies(no3_ind,k) = nitrif(k) - denitrif(k) - sed_denitrif(k) - sum(NO3_V(:,k))

        interior_tendencies(nh4_ind,k) = -sum(NH4_V(:,k)) - nitrif(k) + DON_remin(k) + DONr_remin(k)  &
                                       + Q * (sum(zoo_loss_dic(:,k)) + sum(zoo_graze_dic(:,k)) + sum(auto_loss_dic(:,k)) &
                                              + sum(auto_graze_dic(:,k)) + DOC_prod(k)*(c1 - f_toDON)) &
                                       + PON_remin(k) * (c1 - PONremin_refract)

        do auto_ind = 1, autotroph_cnt
          if (autotroph_settings(auto_ind)%Nfixer) then
            interior_tendencies(nh4_ind,k) = interior_tendencies(nh4_ind,k) + Nexcrete(auto_ind,k)
          end if
        end do

        !-----------------------------------------------------------------------
        !  dissolved iron
        !-----------------------------------------------------------------------

        interior_tendencies(fe_ind,k) = P_iron_remin(k) - sum(photoFe(:,k)) - Fe_scavenge(k) &
                                      + Qfe_zoo * (sum(zoo_loss_dic(:,k)) + sum(zoo_loss_doc(:,k)) &
                                                   + sum(zoo_graze_dic(:,k)) + sum(zoo_graze_doc(:,k)))

        do auto_ind = 1, autotroph_cnt
          interior_tendencies(fe_ind,k) = interior_tendencies(fe_ind,k) &
                                        + (Qfe(auto_ind,k) * (auto_loss_dic(auto_ind,k) + auto_graze_dic(auto_ind,k))) &
                                        + auto_graze_zoo(auto_ind,k) * (Qfe(auto_ind,k) - Qfe_zoo) &
                                        + (Qfe(auto_ind,k) * (auto_loss_doc(auto_ind,k) + auto_graze_doc(auto_ind,k)))
        end do

        !-----------------------------------------------------------------------
        !  iron binding ligand
        !-----------------------------------------------------------------------

        interior_tendencies(lig_ind,k) = Lig_prod(k) - Lig_loss(k)

        !-----------------------------------------------------------------------
        !  dissolved SiO3
        !-----------------------------------------------------------------------

        interior_tendencies(sio3_ind,k) = P_SiO2_remin(k)

        do auto_ind = 1, autotroph_cnt
          if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
            interior_tendencies(sio3_ind,k) = interior_tendencies(sio3_ind,k) - photoSi(auto_ind,k) &
                                            + Qsi(auto_ind,k) * (f_graze_si_remin * auto_graze(auto_ind,k) &
                                                                 + (c1 - autotroph_settings(auto_ind)%loss_poc) &
                                                                   * auto_loss(auto_ind,k))
          end if
        end do

        !-----------------------------------------------------------------------
        !  phosphate
        !-----------------------------------------------------------------------

        interior_tendencies(po4_ind,k) = DOP_remin(k) + DOPr_remin(k) - sum(PO4_V(:,k)) &
                                       + (c1 - POPremin_refract) * POP_remin(k) + sum(remaining_P_dip(:,k)) &
                                       + Qp_zoo * (sum(zoo_loss_dic(:,k)) + sum(zoo_graze_dic(:,k)))

        !-----------------------------------------------------------------------
        !  zoo Carbon
        !-----------------------------------------------------------------------
        do zoo_ind = 1, zooplankton_cnt
          n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
          interior_tendencies(n,k) = x_graze_zoo(zoo_ind,k) - zoo_graze(zoo_ind,k) - zoo_loss(zoo_ind,k)
        end do

        !-----------------------------------------------------------------------
        !  autotroph Carbon
        !  autotroph Phosphorus
        !  autotroph Chlorophyll
        !  autotroph Fe
        !  autotroph Si
        !  autotroph CaCO3
        !-----------------------------------------------------------------------

        do auto_ind = 1, autotroph_cnt
          auto_sum = auto_graze(auto_ind,k) + auto_loss(auto_ind,k) + auto_agg(auto_ind,k)

          n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
          interior_tendencies(n,k) = photoC(auto_ind,k) - auto_sum

          n = marbl_tracer_indices%auto_inds(auto_ind)%P_ind
          if (n > 0) then
            interior_tendencies(n,k) = PO4_V(auto_ind,k) + DOP_V(auto_ind,k) - Qp(auto_ind,k) * auto_sum
          end if

          n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
          interior_tendencies(n,k) = photoacc(auto_ind,k) - thetaC(auto_ind,k) * auto_sum

          n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
          interior_tendencies(n,k) =  photoFe(auto_ind,k) - Qfe(auto_ind,k) * auto_sum

          n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
          if (n > 0) then
            interior_tendencies(n,k) =  photoSi(auto_ind,k) - Qsi(auto_ind,k) * auto_sum
          end if

          n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
          if (n > 0) then
            interior_tendencies(n,k) = CaCO3_form(auto_ind,k) - QCaCO3(auto_ind,k) * auto_sum
          end if
        end do

        !-----------------------------------------------------------------------
        !  dissolved organic Matter
        !  from sinking remin small fraction to refractory pool
        !-----------------------------------------------------------------------

        interior_tendencies(doc_ind,k) = DOC_prod(k) * (c1 - DOCprod_refract) - DOC_remin(k)

        interior_tendencies(docr_ind,k) = DOC_prod(k) * DOCprod_refract - DOCr_remin(k) + (POC_remin(k) * POCremin_refract)

        interior_tendencies(don_ind,k) = (DON_prod(k) * (c1 - DONprod_refract)) - DON_remin(k)

        interior_tendencies(donr_ind,k) = (DON_prod(k) * DONprod_refract) - DONr_remin(k) &
                                        + (PON_remin(k) * PONremin_refract)

        interior_tendencies(dop_ind,k) = (DOP_prod(k) * (c1 - DOPprod_refract)) - DOP_remin(k) - sum(DOP_V(:,k)) &
                                       - DOP_loss_P_bal(k)

        interior_tendencies(dopr_ind,k) = (DOP_prod(k) * DOPprod_refract) - DOPr_remin(k) &
                                        + (POP_remin(k) * POPremin_refract)

        !-----------------------------------------------------------------------
        !  dissolved inorganic Carbon
        !-----------------------------------------------------------------------

        interior_tendencies(dic_ind,k) = sum(auto_loss_dic(:,k)) + sum(auto_graze_dic(:,k)) - sum(photoC(:,k)) &
                                       + DOC_remin(k) + POC_remin(k) * (c1 - POCremin_refract) + sum(zoo_loss_dic(:,k)) &
                                       + sum(zoo_graze_dic(:,k)) + P_CaCO3_remin(k) + DOCr_remin(k)

        do auto_ind = 1, autotroph_cnt
          if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
             interior_tendencies(dic_ind,k) = interior_tendencies(dic_ind,k) + f_graze_CaCO3_remin &
                                            * auto_graze(auto_ind,k) * QCaCO3(auto_ind,k) - CaCO3_form(auto_ind,k)
          end if
        end do

        interior_tendencies(dic_alt_co2_ind,k) = interior_tendencies(dic_ind,k) &
                                               + (P_CaCO3_ALT_CO2_remin(k) - P_CaCO3_remin(k))

        !-----------------------------------------------------------------------
        !  alkalinity
        !-----------------------------------------------------------------------

        interior_tendencies(alk_ind,k) = -interior_tendencies(no3_ind,k) + interior_tendencies(nh4_ind,k) &
                                       + c2 * P_CaCO3_remin(k)

        do auto_ind = 1, autotroph_cnt
          if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
             interior_tendencies(alk_ind,k) = interior_tendencies(alk_ind,k) &
                                            + c2 * (f_graze_CaCO3_remin * auto_graze(auto_ind,k) * QCaCO3(auto_ind,k) &
                                                    - CaCO3_form(auto_ind,k))
          end if
        end do

        interior_tendencies(alk_alt_co2_ind,k) = interior_tendencies(alk_ind,k) &
                                               + c2 * (P_CaCO3_ALT_CO2_remin(k) - P_CaCO3_remin(k))

        !-----------------------------------------------------------------------
        !  oxygen
        !-----------------------------------------------------------------------

        o2_production(k) = c0
        do auto_ind = 1, autotroph_cnt
          if (.not. autotroph_settings(auto_ind)%Nfixer) then
            if (photoC(auto_ind,k) > c0) then
              o2_production(k) = o2_production(k) + photoC(auto_ind,k) &
                               * ((NO3_V(auto_ind,k) / (NO3_V(auto_ind,k) + NH4_V(auto_ind,k))) &
                                 / parm_Red_D_C_O2 &
                                 + (NH4_V(auto_ind,k) / (NO3_V(auto_ind,k) + NH4_V(auto_ind,k))) &
                                 / parm_Remin_D_C_O2)
            end if
          else
            if (photoC(auto_ind,k) > c0) then
              o2_production(k) = o2_production(k) + photoC(auto_ind,k) &
                               * ((NO3_V(auto_ind,k) / (NO3_V(auto_ind,k) + NH4_V(auto_ind,k) + Nfix(auto_ind,k))) &
                                  / parm_Red_D_C_O2 &
                                  + (NH4_V(auto_ind,k) / (NO3_V(auto_ind,k) + NH4_V(auto_ind,k) + Nfix(auto_ind,k))) &
                                  / parm_Remin_D_C_O2 &
                                  + (Nfix(auto_ind,k)  / (NO3_V(auto_ind,k) + NH4_V(auto_ind,k) + Nfix(auto_ind,k))) &
                                  / parm_Red_D_C_O2_diaz)
            end if
          end if
        end do

        o2_consumption(k) = (O2_loc(k) - parm_o2_min) / parm_o2_min_delta
        o2_consumption(k) = min(max(o2_consumption(k), c0), c1)
        o2_consumption(k) = o2_consumption(k) * ((POC_remin(k) * (c1 - POCremin_refract) + DOC_remin(k) + DOCr_remin(k) &
                                                  - (sed_denitrif(k) * denitrif_C_N) - other_remin(k) &
                                                  + sum(zoo_loss_dic(:,k)) + sum(zoo_graze_dic(:,k))  &
                                                  + sum(auto_loss_dic(:,k)) + sum(auto_graze_dic(:,k))) &
                                                 / parm_Remin_D_C_O2 + (c2 * nitrif(k)))
        o2_consumption(k) = o2_consumption_scalef(k) * o2_consumption(k)

        interior_tendencies(o2_ind,k) = o2_production(k) - o2_consumption(k)

      end do
    end associate

  end subroutine compute_local_tendencies

  !***********************************************************************

  subroutine update_particulate_terms_from_prior_level(k, POC, POP, P_CaCO3, &
       P_CaCO3_ALT_CO2, P_SiO2, dust, P_iron, QA_dust_def)

    use marbl_interior_tendency_share_mod, only : marbl_interior_tendency_share_update_particle_flux_from_above

    integer (int_kind)                 , intent(in)    :: k ! vertical model level
    type(column_sinking_particle_type) , intent(inout) :: POC, POP, P_CaCO3, P_CaCO3_ALT_CO2, P_SiO2, dust, P_iron
    real(r8)                           , intent(inout) :: QA_dust_def(:) !(km)

    ! NOTE(bja, 2015-04) assume that k == ksurf condition was handled by
    ! call to marbl_set_surface_particulate_terms()
    if (k > 1) then
      !-----------------------------------------------------------------------
      ! NOTE: incoming fluxes are outgoing fluxes from previous level
      !
      ! initialize loss to sediments = 0
      !-----------------------------------------------------------------------
      call marbl_interior_tendency_share_update_particle_flux_from_above(k, P_CaCO3)
      call marbl_interior_tendency_share_update_particle_flux_from_above(k, P_CaCO3_ALT_CO2)
      call marbl_interior_tendency_share_update_particle_flux_from_above(k, P_SiO2)
      call marbl_interior_tendency_share_update_particle_flux_from_above(k, dust)
      call marbl_interior_tendency_share_update_particle_flux_from_above(k, POC)
      call marbl_interior_tendency_share_update_particle_flux_from_above(k, POP)
      call marbl_interior_tendency_share_update_particle_flux_from_above(k, P_iron)
      QA_dust_def(k) = QA_dust_def(k-1)
    end if

  end subroutine update_particulate_terms_from_prior_level

  !***********************************************************************

end module marbl_interior_tendency_mod
