! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_surface_flux_mod

  !-----------------------------------------------------------------------
  !  The following are used extensively in this ecosys, so are used at
  !  the module level. The use statements for variables that are only needed
  !  locally are located at the module subprogram level.
  !-----------------------------------------------------------------------

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8

  use marbl_settings_mod, only : lflux_gas_o2
  use marbl_settings_mod, only : lflux_gas_co2
  use marbl_settings_mod, only : ladjust_bury_coeff
  use marbl_settings_mod, only : autotroph_cnt
  use marbl_settings_mod, only : del_ph
  use marbl_settings_mod, only : phhi_surf_init
  use marbl_settings_mod, only : phlo_surf_init

  use marbl_interface_private_types, only : marbl_surface_flux_share_type
  use marbl_interface_private_types, only : marbl_surface_flux_internal_type
  use marbl_interface_private_types, only : marbl_tracer_index_type
  use marbl_interface_private_types, only : marbl_surface_flux_forcing_indexing_type

  use marbl_interface_public_types, only : marbl_saved_state_type
  use marbl_interface_public_types, only : marbl_surface_flux_output_type
  use marbl_interface_public_types, only : marbl_forcing_fields_type

  use marbl_diagnostics_mod , only : marbl_diagnostics_surface_flux_compute

  use marbl_logging         , only : marbl_log_type

  implicit none
  private

  !-----------------------------------------------------------------------
  !  public/private member procedure declarations
  !-----------------------------------------------------------------------

  public  :: marbl_surface_flux_compute

  !*****************************************************************************

contains

  !***********************************************************************

  subroutine marbl_surface_flux_compute( &
       num_elements,                    &
       surface_flux_forcing_ind,        &
       surface_flux_forcings,           &
       tracers_at_surface,              &
       surface_fluxes,                  &
       marbl_tracer_indices,            &
       saved_state,                     &
       saved_state_ind,                 &
       surface_flux_output,             &
       surface_flux_internal,           &
       surface_flux_share,              &
       surface_flux_diags,              &
       glo_avg_fields_surface_flux,     &
       marbl_status_log)

    !  Compute surface fluxes for base tracers

    use marbl_interface_public_types, only : sfo_ind
    use marbl_interface_public_types, only : marbl_diagnostics_type
    use marbl_interface_private_types, only : marbl_surface_flux_saved_state_indexing_type
    use marbl_schmidt_number_mod, only : schmidt_co2_surf
    use marbl_oxygen, only : schmidt_o2_surf
    use marbl_co2calc_mod, only : marbl_co2calc_surface
    use marbl_co2calc_mod, only : co2calc_coeffs_type
    use marbl_co2calc_mod, only : co2calc_state_type
    use marbl_oxygen, only : o2sat_surf
    use marbl_nhx_surface_emis_mod, only : marbl_nhx_surface_emis_compute
    use marbl_settings_mod, only : lcompute_nhx_surface_emis
    use marbl_settings_mod, only : xkw_coeff
    use marbl_surface_flux_share_mod, only : marbl_surface_flux_share_export_variables
    use marbl_ciso_surface_flux_mod, only : marbl_ciso_surface_flux_compute
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_C_input
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_P_input
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_Si_input

    integer (int_kind)                        , intent(in)    :: num_elements
    type(marbl_surface_flux_forcing_indexing_type), intent(in) :: surface_flux_forcing_ind
    type(marbl_forcing_fields_type)           , intent(in)    :: surface_flux_forcings(:)
    real (r8)                                 , intent(in)    :: tracers_at_surface(:,:)
    real (r8)                                 , intent(out)   :: surface_fluxes(:,:)
    type(marbl_tracer_index_type)             , intent(in)    :: marbl_tracer_indices
    type(marbl_saved_state_type)              , intent(inout) :: saved_state
    type(marbl_surface_flux_saved_state_indexing_type), intent(in) :: saved_state_ind
    type(marbl_surface_flux_internal_type)    , intent(inout) :: surface_flux_internal
    type(marbl_surface_flux_output_type)      , intent(inout) :: surface_flux_output
    type(marbl_surface_flux_share_type)       , intent(inout) :: surface_flux_share
    type(marbl_diagnostics_type)              , intent(inout) :: surface_flux_diags
    real (r8)                                 , intent(out)   :: glo_avg_fields_surface_flux(:,:)
    type(marbl_log_type)                      , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_surface_flux_mod:marbl_surface_flux_compute'

    integer (int_kind) :: auto_ind                 ! autotroph functional group index
    real (r8)          :: phlo(num_elements)       ! lower bound for ph in solver
    real (r8)          :: phhi(num_elements)       ! upper bound for ph in solver
    real (r8)          :: xkw_ice(num_elements)    ! common portion of piston vel., (1-fice)*xkw (cm/s)
    real (r8)          :: o2sat_1atm(num_elements) ! o2 saturation @ 1 atm (mmol/m^3)
    real (r8)          :: totalChl_loc(num_elements)  ! local value of totalChl
    real (r8)          :: flux_o2_loc(num_elements)   ! local value of o2 flux
    type(co2calc_coeffs_type), dimension(num_elements) :: co2calc_coeffs
    type(co2calc_state_type),  dimension(num_elements) :: co2calc_state
    !-----------------------------------------------------------------------

    associate(                                             &
         ind                  => surface_flux_forcing_ind, &

         ifrac        => surface_flux_forcings(surface_flux_forcing_ind%ifrac_id)%field_0d,        &
         sst          => surface_flux_forcings(surface_flux_forcing_ind%sst_id)%field_0d,          &
         sss          => surface_flux_forcings(surface_flux_forcing_ind%sss_id)%field_0d,          &
         xco2         => surface_flux_forcings(surface_flux_forcing_ind%xco2_id)%field_0d,         &
         xco2_alt_co2 => surface_flux_forcings(surface_flux_forcing_ind%xco2_alt_co2_id)%field_0d, &
         ap_used      => surface_flux_forcings(surface_flux_forcing_ind%atm_pressure_id)%field_0d, &
         u10_sqr      => surface_flux_forcings(surface_flux_forcing_ind%u10_sqr_id)%field_0d,      &
         dust_flux_in => surface_flux_forcings(surface_flux_forcing_ind%dust_flux_id)%field_0d,    &
         iron_flux_in => surface_flux_forcings(surface_flux_forcing_ind%iron_flux_id)%field_0d,    &
         nox_flux     => surface_flux_forcings(surface_flux_forcing_ind%nox_flux_id)%field_0d,     &
         nhy_flux     => surface_flux_forcings(surface_flux_forcing_ind%nhy_flux_id)%field_0d,     &

         piston_velocity      => surface_flux_internal%piston_velocity(:),                       &
         flux_co2             => surface_flux_internal%flux_co2(:),                              &
         co2star              => surface_flux_internal%co2star(:),                               &
         dco2star             => surface_flux_internal%dco2star(:),                              &
         pco2surf             => surface_flux_internal%pco2surf(:),                              &
         dpco2                => surface_flux_internal%dpco2(:),                                 &
         co3                  => surface_flux_internal%co3(:),                                   &
         co2star_alt          => surface_flux_internal%co2star_alt(:),                           &
         dco2star_alt         => surface_flux_internal%dco2star_alt(:),                          &
         pco2surf_alt         => surface_flux_internal%pco2surf_alt(:),                          &
         dpco2_alt            => surface_flux_internal%dpco2_alt(:),                             &
         schmidt_co2          => surface_flux_internal%schmidt_co2(:),                           &
         schmidt_o2           => surface_flux_internal%schmidt_o2(:),                            &
         pv_o2                => surface_flux_internal%pv_o2(:),                                 &
         pv_co2               => surface_flux_internal%pv_co2(:),                                &
         o2sat                => surface_flux_internal%o2sat(:),                                 &
         flux_alt_co2         => surface_flux_internal%flux_alt_co2(:),                          &
         nhx_surface_emis     => surface_flux_internal%nhx_surface_emis(:),                      &

         ph_prev_surf         => saved_state%state(saved_state_ind%ph_surf)%field_2d,               &
         ph_prev_alt_co2_surf => saved_state%state(saved_state_ind%ph_alt_co2_surf)%field_2d,       &

         po4_ind           => marbl_tracer_indices%po4_ind,                                     &
         no3_ind           => marbl_tracer_indices%no3_ind,                                     &
         sio3_ind          => marbl_tracer_indices%sio3_ind,                                     &
         nh4_ind           => marbl_tracer_indices%nh4_ind,                                     &
         fe_ind            => marbl_tracer_indices%fe_ind,                                      &
         o2_ind            => marbl_tracer_indices%o2_ind,                                      &
         dic_ind           => marbl_tracer_indices%dic_ind,                                     &
         dic_alt_co2_ind   => marbl_tracer_indices%dic_alt_co2_ind,                             &
         alk_ind           => marbl_tracer_indices%alk_ind,                                     &
         alk_alt_co2_ind   => marbl_tracer_indices%alk_alt_co2_ind                              &
         )

    !-----------------------------------------------------------------------
    !  fluxes initially set to 0
    !-----------------------------------------------------------------------

    surface_fluxes(:, :) = c0

    !-----------------------------------------------------------------------
    !  Compute total chlorophyll
    !-----------------------------------------------------------------------

    if (sfo_ind%totalChl_id.ne.0) then
      totalChl_loc = c0
      do auto_ind = 1,autotroph_cnt
        totalChl_loc = totalChl_loc +                                         &
          max(c0, tracers_at_surface(:,marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind))
      end do
      surface_flux_output%sfo(sfo_ind%totalChl_id)%forcing_field = totalChl_loc
    end if

    !-----------------------------------------------------------------------
    !  calculate gas flux quantities if necessary
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  compute CO2 flux, computing disequilibrium one row at a time
    !-----------------------------------------------------------------------

    if (lflux_gas_o2 .or. lflux_gas_co2) then

       !-----------------------------------------------------------------------
       !  Compute XKW_ICE. XKW is zero over land, so XKW_ICE is too.
       !-----------------------------------------------------------------------

       piston_velocity = xkw_coeff*u10_sqr(:)
       xkw_ice(:) = (c1 - ifrac(:)) * piston_velocity

       !-----------------------------------------------------------------------
       !  compute O2 flux
       !-----------------------------------------------------------------------

       if (lflux_gas_o2) then
          schmidt_o2(:) = schmidt_o2_surf(num_elements, sst)

          o2sat_1atm(:) = o2sat_surf(num_elements, sst, sss)

          pv_o2(:) = xkw_ice(:) * sqrt(660.0_r8 / schmidt_o2(:))
          o2sat(:) = ap_used(:) * o2sat_1atm(:)
          flux_o2_loc(:) = pv_o2(:) * (o2sat(:) - tracers_at_surface(:, o2_ind))
          surface_fluxes(:, o2_ind) = surface_fluxes(:, o2_ind) + flux_o2_loc(:)
          if (sfo_ind%flux_o2_id.ne.0) then
            surface_flux_output%sfo(sfo_ind%flux_o2_id)%forcing_field = flux_o2_loc
          end if
       else
          schmidt_o2(:) = c0
          pv_o2(:)      = c0
          o2sat(:)      = c0
       endif  ! lflux_gas_o2

       !-----------------------------------------------------------------------
       !  compute CO2 flux, computing disequilibrium
       !-----------------------------------------------------------------------

       if (lflux_gas_co2) then

          schmidt_co2(:) = schmidt_co2_surf(num_elements, sst)

          pv_co2(:) = xkw_ice(:) * sqrt(660.0_r8 / schmidt_co2(:))

          !-----------------------------------------------------------------------
          !  Set FLUX_CO2
          !-----------------------------------------------------------------------

          where (ph_prev_surf(:) /= c0)
             phlo(:) = ph_prev_surf(:) - del_ph
             phhi(:) = ph_prev_surf(:) + del_ph
          elsewhere
             phlo(:) = phlo_surf_init
             phhi(:) = phhi_surf_init
          end where

          ! Note the following computes a new ph_prev_surf
          ! pass in sections of surface_flux_forcings instead of associated vars because of problems with intel/15.0.3
          call marbl_co2calc_surface(                                      &
               num_elements     = num_elements,                            &
               lcomp_co2calc_coeffs = .true.,                              &
               dic_in     = tracers_at_surface(:,dic_ind),                 &
               xco2_in    = surface_flux_forcings(ind%xco2_id)%field_0d,   &
               ta_in      = tracers_at_surface(:,alk_ind),                 &
               pt_in      = tracers_at_surface(:,po4_ind),                 &
               sit_in     = tracers_at_surface(:,sio3_ind),                &
               temp       = surface_flux_forcings(ind%sst_id)%field_0d,    &
               salt       = surface_flux_forcings(ind%sss_id)%field_0d,    &
               atmpres    = surface_flux_forcings(ind%atm_pressure_id)%field_0d, &
               co2calc_coeffs = co2calc_coeffs,                            &
               co2calc_state = co2calc_state,                              &
               co3        = co3,                                           &
               co2star    = co2star,                                       &
               dco2star   = dco2star,                                      &
               pco2surf   = pco2surf,                                      &
               dpco2      = dpco2,                                         &
               phlo       = phlo,                                          &
               phhi       = phhi,                                          &
               ph         = ph_prev_surf,                                  &
               marbl_status_log = marbl_status_log)

          if (marbl_status_log%labort_marbl) then
             call marbl_status_log%log_error_trace('marbl_co2calc_surface() with flux_co2', subname)
             return
          end if

          if (marbl_status_log%lwarning) then
             call marbl_status_log%log_warning_trace('marbl_co2calc_surface() with flux_co2', subname)
          end if

          flux_co2(:) = pv_co2(:) * dco2star(:)
          if (sfo_ind%flux_co2_id.ne.0) then
            surface_flux_output%sfo(sfo_ind%flux_co2_id)%forcing_field = flux_co2
          end if

          !-------------------------------------------------------------------
          !  The following variables need to be shared with other modules,
          !  and are now defined in marbl_share as targets.
          !-------------------------------------------------------------------

          call marbl_surface_flux_share_export_variables(surface_flux_internal, tracers_at_surface, &
               marbl_tracer_indices, surface_flux_share)

          !-----------------------------------------------------------------------
          !  Set flux_alt_co2
          !-----------------------------------------------------------------------

          where (ph_prev_alt_co2_surf(:) /= c0)
             phlo(:) = ph_prev_alt_co2_surf(:) - del_ph
             phhi(:) = ph_prev_alt_co2_surf(:) + del_ph
          elsewhere
             phlo(:) = phlo_surf_init
             phhi(:) = phhi_surf_init
          end where

          ! Note the following computes a new ph_prev_alt_co2
          ! pass in sections of surface_flux_forcings instead of associated vars because of problems with intel/15.0.3
          call marbl_co2calc_surface(                                      &
               num_elements     = num_elements,                            &
               lcomp_co2calc_coeffs = .false.,                             &
               dic_in     = tracers_at_surface(:,dic_alt_co2_ind),         &
               xco2_in    = surface_flux_forcings(ind%xco2_alt_co2_id)%field_0d, &
               ta_in      = tracers_at_surface(:,alk_alt_co2_ind),         &
               pt_in      = tracers_at_surface(:,po4_ind),                 &
               sit_in     = tracers_at_surface(:,sio3_ind),                &
               temp       = surface_flux_forcings(ind%sst_id)%field_0d,    &
               salt       = surface_flux_forcings(ind%sss_id)%field_0d,    &
               atmpres    = surface_flux_forcings(ind%atm_pressure_id)%field_0d, &
               co2calc_coeffs = co2calc_coeffs,                            &
               co2calc_state = co2calc_state,                              &
               co3        = co3,                                           &
               co2star    = co2star_alt,                                   &
               dco2star   = dco2star_alt,                                  &
               pco2surf   = pco2surf_alt,                                  &
               dpco2      = dpco2_alt,                                     &
               phlo       = phlo,                                          &
               phhi       = phhi,                                          &
               ph         = ph_prev_alt_co2_surf,                          &
               marbl_status_log = marbl_status_log)

          if (marbl_status_log%labort_marbl) then
             call marbl_status_log%log_error_trace('marbl_co2calc_surface() with flux_alt_co2', subname)
! NOTE ignore problems with ALT_CO2 for now, including comment out return statement
             dco2star_alt(:) = 0.0
             marbl_status_log%labort_marbl = .false.
!            return
          end if

          if (marbl_status_log%lwarning) then
             call marbl_status_log%log_warning_trace('marbl_co2calc_surface() with flux_alt_co2', subname)
          end if

          flux_alt_co2(:) = pv_co2(:) * dco2star_alt(:)

          !-----------------------------------------------------------------------
          !  set air-sea co2 gas flux named field, converting units from
          !  nmol/cm^2/s (positive down) to kg CO2/m^2/s (positive down)
          !-----------------------------------------------------------------------

          surface_fluxes(:, dic_ind)         = surface_fluxes(:, dic_ind)         + flux_co2(:)
          surface_fluxes(:, dic_alt_co2_ind) = surface_fluxes(:, dic_alt_co2_ind) + FLUX_ALT_CO2(:)

       else
          schmidt_co2(:) = c0
          pv_co2(:)      = c0
       endif  !  lflux_gas_co2

    endif  ! lflux_gas_o2 .or. lflux_gas_co2

    !-----------------------------------------------------------------------
    !  compute NHx emissions
    !  FIXME: ensure that ph is computed, even if lflux_gas_co2=.false.
    !-----------------------------------------------------------------------

    if (lcompute_nhx_surface_emis) then
      call marbl_nhx_surface_emis_compute(                   &
           num_elements     = num_elements,                  &
           nh4              = tracers_at_surface(:,nh4_ind), &
           ph               = ph_prev_surf,                  &
           sst              = sst,                           &
           sss              = sss,                           &
           u10_sqr          = u10_sqr,                       &
           atmpres          = ap_used,                       &
           ifrac            = ifrac,                         &
           nhx_surface_emis = nhx_surface_emis)

      if (sfo_ind%flux_nhx_id.ne.0) then
         surface_flux_output%sfo(sfo_ind%flux_nhx_id)%forcing_field = nhx_surface_emis
      end if

      surface_fluxes(:, nh4_ind) = surface_fluxes(:, nh4_ind) - nhx_surface_emis(:)
    endif

    !-----------------------------------------------------------------------
    !  calculate iron and dust fluxes if necessary
    !-----------------------------------------------------------------------

    surface_fluxes(:, fe_ind) = surface_fluxes(:, fe_ind) + iron_flux_in(:)

    !-----------------------------------------------------------------------
    !  Add phosphate and silicate from dust after Krishnamurthy et al. (2010)
    !  factors convert from g/cm2/s to nmol/cm2/s
    !  ( P frac in dust by weight) * ( P solubility) / ( P molecular weight) * (mol->nmol)
    !  (Si frac in dust by weight) * (Si solubility) / (Si molecular weight) * (mol->nmol)
    !-----------------------------------------------------------------------

    surface_fluxes(:, po4_ind) = surface_fluxes(:, po4_ind)   + (dust_flux_in * (0.00105_r8 *  0.15_r8 / 30.974_r8 * 1.0e9_r8))

    surface_fluxes(:, sio3_ind) = surface_fluxes(:, sio3_ind) + (dust_flux_in * (  0.308_r8 * 0.075_r8 / 28.085_r8 * 1.0e9_r8))

    !-----------------------------------------------------------------------
    !  calculate nox and nhy fluxes
    !-----------------------------------------------------------------------

    surface_fluxes(:, no3_ind) = surface_fluxes(:, no3_ind) + nox_flux(:)
    surface_fluxes(:, nh4_ind) = surface_fluxes(:, nh4_ind) + nhy_flux(:)

    !-----------------------------------------------------------------------
    !  Apply NO & NH fluxes to alkalinity
    !-----------------------------------------------------------------------

    surface_fluxes(:, alk_ind)         = surface_fluxes(:, alk_ind) + &
         surface_fluxes(:, nh4_ind) - surface_fluxes(:, no3_ind)
    surface_fluxes(:, alk_alt_co2_ind) = surface_fluxes(:, alk_alt_co2_ind) + &
         surface_fluxes(:, nh4_ind) - surface_fluxes(:, no3_ind)

    !-----------------------------------------------------------------------
    ! Compute surface flux-related diagnostics
    !-----------------------------------------------------------------------

    call marbl_diagnostics_surface_flux_compute(              &
         surface_flux_forcing_ind = ind,                      &
         surface_flux_forcings    = surface_flux_forcings,    &
         surface_flux_internal    = surface_flux_internal,    &
         marbl_tracer_indices     = marbl_tracer_indices,     &
         saved_state              = saved_state,              &
         saved_state_ind          = saved_state_ind,          &
         surface_flux_diags       = surface_flux_diags)

    !-----------------------------------------------------------------------
    ! Compute carbon isotopes surface fluxes
    !-----------------------------------------------------------------------

    call marbl_ciso_surface_flux_compute(                                             &
         num_elements                = num_elements,                                  &
         surface_flux_forcing_ind    = surface_flux_forcing_ind,                      &
         surface_flux_forcings       = surface_flux_forcings,                         &
         tracers_at_surface          = tracers_at_surface,                            &
         surface_fluxes              = surface_fluxes,                                &
         marbl_tracer_indices        = marbl_tracer_indices,                          &
         marbl_surface_flux_share    = surface_flux_share,                            &
         marbl_surface_flux_diags    = surface_flux_diags)

    !-----------------------------------------------------------------------

    end associate

    if (ladjust_bury_coeff) then
       associate(                                                                                  &
          flux_co2     => surface_flux_internal%flux_co2(:),                                       &
          ext_C_flux   => surface_flux_forcings(surface_flux_forcing_ind%ext_C_flux_id)%field_0d,  &
          ext_P_flux   => surface_flux_forcings(surface_flux_forcing_ind%ext_P_flux_id)%field_0d,  &
          ext_Si_flux  => surface_flux_forcings(surface_flux_forcing_ind%ext_Si_flux_id)%field_0d, &

          dic_ind      => marbl_tracer_indices%dic_ind,                                        &
          doc_ind      => marbl_tracer_indices%doc_ind,                                        &
          docr_ind     => marbl_tracer_indices%docr_ind,                                       &
          po4_ind      => marbl_tracer_indices%po4_ind,                                        &
          dop_ind      => marbl_tracer_indices%dop_ind,                                        &
          dopr_ind     => marbl_tracer_indices%dopr_ind,                                       &
          sio3_ind     => marbl_tracer_indices%sio3_ind                                        &
          )

          glo_avg_fields_surface_flux(:,glo_avg_field_ind_surface_flux_C_input) = &
             ext_C_flux(:) + surface_fluxes(:,dic_ind) - flux_co2(:) + surface_fluxes(:,doc_ind) + surface_fluxes(:,docr_ind)

          glo_avg_fields_surface_flux(:,glo_avg_field_ind_surface_flux_P_input) = &
             ext_P_flux(:) + surface_fluxes(:,po4_ind) + surface_fluxes(:,dop_ind) + surface_fluxes(:,dopr_ind)

          glo_avg_fields_surface_flux(:,glo_avg_field_ind_surface_flux_Si_input) = &
             ext_Si_flux(:) + surface_fluxes(:,sio3_ind)
       end associate
    end if

  end subroutine marbl_surface_flux_compute

  !***********************************************************************

end module marbl_surface_flux_mod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
