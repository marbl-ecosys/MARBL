module marbl_abio_dic_surface_flux_mod

    use marbl_kinds_mod, only : int_kind
    use marbl_kinds_mod, only : r8

    use marbl_constants_mod, only : c0
    use marbl_constants_mod, only : c1
    use marbl_constants_mod, only : rho_sw

    use marbl_settings_mod, only : unit_system_type
    use marbl_settings_mod, only : xkw_coeff
    use marbl_settings_mod, only : del_ph
    use marbl_settings_mod, only : phlo_surf_init
    use marbl_settings_mod, only : phhi_surf_init
    use marbl_settings_mod, only : abio_dic_on

    use marbl_interface_private_types, only : marbl_tracer_index_type
    use marbl_interface_private_types, only : marbl_surface_flux_saved_state_indexing_type
    use marbl_interface_private_types, only : marbl_surface_flux_forcing_indexing_type
    use marbl_interface_private_types, only : marbl_surface_flux_internal_type
    use marbl_interface_private_types, only : co2calc_coeffs_type
    use marbl_interface_private_types, only : co2calc_state_type

    use marbl_interface_public_types, only : marbl_diagnostics_type
    use marbl_interface_public_types, only : marbl_saved_state_type
    use marbl_interface_public_types, only : marbl_forcing_fields_type

    use marbl_co2calc_mod, only : marbl_co2calc_surface

    use marbl_schmidt_number_mod, only : schmidt_co2_surf

    use marbl_abio_dic_diagnostics_mod, only : marbl_abio_dic_diagnostics_surface_flux_compute

    use marbl_logging, only : marbl_log_type

    implicit none
    private

    public :: marbl_abio_dic_surface_flux_compute

contains

  subroutine marbl_abio_dic_surface_flux_compute(&
      num_elements, &
      tracers_at_surface, &
      marbl_tracer_indices, &
      saved_state_ind, &
      surface_flux_forcing_ind, &
      surface_flux_forcings, &
      unit_system, &
      saved_state, &
      surface_flux_internal, &
      surface_flux_diags, &
      surface_fluxes, &
      co2calc_coeffs, &
      co2calc_state, &
      marbl_status_log)

    integer(int_kind),                                  intent(in)    :: num_elements
    real (r8),                                          intent(in)    :: tracers_at_surface(:,:)
    type(marbl_tracer_index_type),                      intent(in)    :: marbl_tracer_indices
    type(marbl_surface_flux_saved_state_indexing_type), intent(in)    :: saved_state_ind
    type(marbl_surface_flux_forcing_indexing_type),     intent(in)    :: surface_flux_forcing_ind
    type(marbl_forcing_fields_type),                    intent(in)    :: surface_flux_forcings(:)
    type(unit_system_type),                             intent(in)    :: unit_system
    type(marbl_saved_state_type),                       intent(inout) :: saved_state
    type(marbl_surface_flux_internal_type),             intent(inout) :: surface_flux_internal
    type(marbl_diagnostics_type),                       intent(inout) :: surface_flux_diags
    real(r8),                                           intent(inout) :: surface_fluxes(:, :)
    type(co2calc_coeffs_type),                          intent(inout) :: co2calc_coeffs
    type(co2calc_state_type),                           intent(inout) :: co2calc_state
    type(marbl_log_type),                               intent(inout) :: marbl_status_log

    real(r8) :: alk_surf(num_elements)     ! local alkalinity
    real(r8) :: phlo(num_elements)         ! lower bound for ph in solver
    real(r8) :: phhi(num_elements)         ! upper bound for ph in solver
    real(r8) :: SiO2(num_elements)         ! Abiotic silicate
    real(r8) :: PO4(num_elements)          ! Abiotic phosphate
    real(r8) :: R14C_ocn(num_elements)     ! Rocn = DIC14/DIC
    real(r8) :: R14C_atm(num_elements)     ! Ratm = 1+ D14C/1000
    real(r8) :: fg_dic(num_elements)       ! Carbon gas flux
    real(r8) :: fg_di14c(num_elements)     ! Isotopic carbon gas flux
    real(r8) :: co2star(num_elements)
    real(r8) :: dco2star(num_elements)
    real(r8) :: pco2surf(num_elements)
    real(r8) :: dpco2(num_elements)
    real(r8) :: co3(num_elements)

    ! Return immediately if not running with abiotic dic tracer module
    if (.not. abio_dic_on) return

    associate(&
        ! Forcing fields from GCM
        ifrac   => surface_flux_forcings(surface_flux_forcing_ind%ifrac_id)%field_0d, &
        sst     => surface_flux_forcings(surface_flux_forcing_ind%sst_id)%field_0d, &
        sss     => surface_flux_forcings(surface_flux_forcing_ind%sss_id)%field_0d, &
        ! TODO: need to add abio_xco2_id to surface flux forcing inds
        xco2    => surface_flux_forcings(surface_flux_forcing_ind%xco2_id)%field_0d, &
        ap_used => surface_flux_forcings(surface_flux_forcing_ind%atm_pressure_id)%field_0d, &
        u10_sqr => surface_flux_forcings(surface_flux_forcing_ind%u10_sqr_id)%field_0d, &
        d14c    => surface_flux_forcings(surface_flux_forcing_ind%d14c_id)%field_0d,  &
        ! Values computed for abio and bio
        piston_velocity => surface_flux_internal%piston_velocity(:), &
        xkw_ice         => surface_flux_internal%xkw_ice(:), &
        schmidt_co2     => surface_flux_internal%schmidt_co2(:), &
        pv_co2          => surface_flux_internal%pv_co2(:), &
       ! Saved state
        ph_surf => saved_state%state(saved_state_ind%abio_ph_surf)%field_2d, &
        ! Tracer indices
        dic_ind      => marbl_tracer_indices%abio_dic_ind, &
        di14c_ind    => marbl_tracer_indices%abio_di14c_ind, &
        abio_ind_beg => marbl_tracer_indices%abio_dic%ind_beg, &
        abio_ind_end => marbl_tracer_indices%abio_dic%ind_end &
        )

    !-----------------------------------------------------------------------
    !  abio fluxes initially set to 0
    !-----------------------------------------------------------------------

    surface_fluxes(:,abio_ind_beg:abio_ind_end) = c0

    !-----------------------------------------------------------------------
    ! Set C14 ratios
    !-----------------------------------------------------------------------
    where (tracers_at_surface(:,dic_ind) > c0)
      R14C_ocn = tracers_at_surface(:,di14c_ind) / tracers_at_surface(:,dic_ind)
    else where
      R14C_ocn = c0
    end where
    R14C_atm = c1 + d14c(:) / 1000._r8

    !-----------------------------------------------------------------------
    !  Use constant concentrations of silicate and phosphate
    !-----------------------------------------------------------------------

    ! These come from  Orr et al. GMD 2017
    ! 7.5 nmol / g -> mmol/m3 or nmol/cm3
    SiO2 = (7.5_r8 * (unit_system%nmol2mol_prefix * unit_system%mass2g)) * rho_sw
    ! 0.5 nmol / g -> mmol/m3 or nmol/cm3
    PO4  = (0.5_r8 * (unit_system%nmol2mol_prefix * unit_system%mass2g)) * rho_sw

    !-----------------------------------------------------------------------
    ! Compute CO2 flux
    !-----------------------------------------------------------------------

    where (ph_surf(:) /= c0)
      phlo(:) = ph_surf(:) - del_ph
      phhi(:) = ph_surf(:) + del_ph
    elsewhere
      phlo(:) = phlo_surf_init
      phhi(:) = phhi_surf_init
    end where

    ! In POP, ALK_bar_global = 2310._r8 microeq/kg = 2310._r8 neq/g
    !         and ocn_ref_salinity comes from shr_const (SHR_CONST_OCN_REF_SAL = 34.7)
    ! Orr et al  eq (27):
    ! 2297 micromol / kg
    ! Sbar = "global- and annual- mean salinity"
    alk_surf(:) = (2310._r8 * (unit_system%nmol2mol_prefix * unit_system%mass2g))  * rho_sw * sss(:) / 34.7_r8

    ! Note the following computes a new ph_surf
    ! pass in sections of surface_flux_forcings instead of associated vars because of problems with intel/15.0.3
    call marbl_co2calc_surface(&
         num_elements  = num_elements, &
         lcomp_co2calc_coeffs = .true., &
         dic_in = tracers_at_surface(:,dic_ind), &
         xco2_in = xco2(:), &
         ta_in = alk_surf(:), &
         pt_in = PO4(:), &
         sit_in = SiO2(:), &
         temp = sst(:), &
         salt = sss(:), &
         atmpres = ap_used(:), &
         unit_system = unit_system, &
         co2calc_coeffs = co2calc_coeffs, &
         co2calc_state = co2calc_state, &
         co3 = co3(:), &
         co2star = co2star, &
         dco2star = dco2star, &
         pco2surf = pco2surf, &
         dpco2 = dpco2, &
         phlo = phlo, &
         phhi = phhi, &
         ph = ph_surf, &
         marbl_status_log = marbl_status_log)

    fg_dic(:) = pv_co2(:) * dco2star(:)
    fg_di14c(:) = pv_co2(:) * ((dco2star(:) + co2star(:)) * R14C_atm(:) - co2star(:) * R14C_ocn(:))
    surface_fluxes(:, dic_ind) = surface_fluxes(:, dic_ind) + fg_dic(:)
    surface_fluxes(:, di14c_ind) = surface_fluxes(:, di14c_ind) + fg_di14c(:)

    ! update abiotic DIC diagnostics

    call marbl_abio_dic_diagnostics_surface_flux_compute( &
         ifrac, &
         piston_velocity, &
         ap_used, &
         xco2, &
         d14c, &
         schmidt_co2, &
         pv_co2, &
         co2star, &
         dco2star, &
         pco2surf, &
         dpco2, &
         ph_surf, &
         alk_surf, &
         fg_dic, &
         fg_di14c, &
         surface_flux_diags)

    end associate


  end subroutine marbl_abio_dic_surface_flux_compute

end module marbl_abio_dic_surface_flux_mod
