! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_surface_mod

  !  Multispecies ecosystem based on Doney et al. 1996, Moore et al., 2002
  !  Based on POP Global NCAR Nitrogen Ecosystem Model
  !  version 0.0 (June 15th, 1998) from S.C. Doney.
  !  Based on Doney et al., 1996 model.
  !  Climate and Global Dynamics, NCAR
  !  (doney@whoi.edu)
  !
  !  Version 1.0
  !  Multispecies, multiple limiting nutrient version of ecosystem
  !  based on mixed layer model of Moore et al.(2002).  Implemented here with
  !  fixed elemental ratios and including only the diatoms and small
  !  phytoplankton, with a parameterization of calcification,
  !  by Keith Lindsay and Keith Moore, Fall 2001 - Spring 2002.
  !  Calcification parameterization based on Moore et al. 2002.
  !
  !  Version 2.0, January 2003
  !    Adds diazotrophs as a phytoplankton group, (based on Moore et al., 2002a)
  !    Allows for variable fe/C for all phytoplankton groups
  !     Allows for variable si/C for the diatoms
  !     Adds explicit tracers for DON, DOP, DOFe
  !     variable remin length scale for detrital soft POM and bSi f(temperature)
  !     Extensive modifications to iron scavenging parameterization
  !     Addition of a sedimentary dissolved iron source,
  !        (implemented in ballast code as excess remin in bottom cell)
  !        coded by J.K. Moore, (jkmoore@uci.edu)
  !
  !   Version 2.01. March 2003
  !     corrected O2 bug
  !     corrected grazing parameter z_grz bug at depth
  !     dust dissolution at depth releases iron,
  !     increased length scale for dust diss., increased hard fraction dust
  !     no deep ocean reduction in scavenging rates,
  !     increase bSi OC/ballast ratio 0.3 -> 0.35,
  !     corrected bug in diazotroph photoadaptation, and diat and sp adapatation
  !
  !   Version 2.02.
  !     corrected bug in Fe_scavenge (units for dust), May 2003
  !     changed C/N/P ratios to 117/16/1 (Anderson & Sarmiento, 1994)
  !
  !   Version 2.03., July 2003
  !     Remin of DOM no longer temperature dependent,
  !     new iron scavenging parameterization added,
  !     some dissolution of hard fraction of ballast materials added
  !
  !   Version 2.1, September 2003
  !     modfied iron scavenging and dust dissolution at depth
  !
  !   Version 2.11, March 2004
  !     fixed bug in iron scavenging code, replace dust and POC flux_in w/ flux_out
  !
  !   Version 2.12, April 2004 - Final version for GBC paper revision,
  !     (Questions/comments, Keith Moore - jkmoore@uci.edu
  !
  !   References
  !   Doney, S.C., Glover, D.M., Najjar, R.G., 1996. A new coupled, one-dimensional
  !   biological-physical model for the upper ocean: applications to the JGOFS
  !   Bermuda Time-Series Study (BATS) site. Deep-Sea Res. II, 43: 591-624.
  !
  !   Moore, JK, Doney, SC, Kleypas, JA, Glover, DM, Fung, IY, 2002. An intermediate
  !   complexity marine ecosystem model for the global domain. Deep-Sea Res. II, 49:
  !   403-462.
  !
  !   Moore, JK, Doney, SC, Glover, DM, Fung, IY, 2002. Iron cycling and nutrient
  !   limitation patterns in surface waters of the world ocean. Deep-Sea Res. II,
  !   49: 463-507.

  !-----------------------------------------------------------------------
  !  The following are used extensively in this ecosys, so are used at
  !  the module level. The use statements for variables that are only needed
  !  locally are located at the module subprogram level.
  !-----------------------------------------------------------------------

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8

  use marbl_settings_mod, only : autotroph_cnt
  use marbl_settings_mod, only : zooplankton_cnt
  use marbl_settings_mod, only : max_grazer_prey_cnt
  use marbl_settings_mod, only : ciso_on
  use marbl_settings_mod, only : lsource_sink
  use marbl_settings_mod, only : lflux_gas_o2
  use marbl_settings_mod, only : lflux_gas_co2
  use marbl_settings_mod, only : init_bury_coeff_opt
  use marbl_settings_mod, only : ladjust_bury_coeff
  use marbl_settings_mod, only : autotrophs
  use marbl_settings_mod, only : zooplankton
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
  use marbl_settings_mod, only : f_graze_CaCO3_REMIN
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
  use marbl_settings_mod, only : grazing
  use marbl_settings_mod, only : PON_bury_coeff
  use marbl_settings_mod, only : del_ph
  use marbl_settings_mod, only : phhi_surf_init
  use marbl_settings_mod, only : phlo_surf_init

  use marbl_interface_private_types, only : marbl_surface_forcing_share_type
  use marbl_interface_private_types, only : marbl_surface_forcing_internal_type
  use marbl_interface_private_types, only : marbl_tracer_index_type
  use marbl_interface_private_types, only : marbl_surface_forcing_indexing_type

  use marbl_interface_public_types, only : marbl_saved_state_type
  use marbl_interface_public_types, only : marbl_surface_forcing_output_type
  use marbl_interface_public_types, only : marbl_forcing_fields_type

  use marbl_diagnostics_mod , only : marbl_diagnostics_set_surface_forcing
  use marbl_diagnostics_mod , only : marbl_diagnostics_set_interior_forcing

  use marbl_logging         , only : marbl_log_type

  implicit none
  private

  !-----------------------------------------------------------------------
  !  public/private member procedure declarations
  !-----------------------------------------------------------------------

  public  :: marbl_set_surface_forcing

  !*****************************************************************************

contains

  !***********************************************************************

  subroutine marbl_set_surface_forcing( &
       num_elements,                    &
       surface_forcing_ind,             &
       surface_input_forcings,          &
       surface_vals,                    &
       surface_tracer_fluxes,           &
       marbl_tracer_indices,            &
       saved_state,                     &
       saved_state_ind,                 &
       surface_forcing_output,          &
       surface_forcing_internal,        &
       surface_forcing_share,           &
       surface_forcing_diags,           &
       glo_avg_fields_surface,          &
       marbl_status_log)

    !  Compute surface forcing fluxes

    use marbl_interface_public_types, only : sfo_ind
    use marbl_interface_public_types, only : marbl_diagnostics_type
    use marbl_interface_private_types, only : marbl_surface_saved_state_indexing_type
    use marbl_schmidt_number_mod, only : schmidt_co2_surf
    use marbl_oxygen, only : schmidt_o2_surf
    use marbl_co2calc_mod, only : marbl_co2calc_surface
    use marbl_co2calc_mod, only : co2calc_coeffs_type
    use marbl_co2calc_mod, only : co2calc_state_type
    use marbl_oxygen, only : o2sat_surf
    use marbl_nhx_surface_emis_mod, only : marbl_comp_nhx_surface_emis
    use marbl_settings_mod, only : lcompute_nhx_surface_emis
    use marbl_settings_mod, only : xkw_coeff
    use marbl_ciso_mod, only : marbl_ciso_set_surface_forcing
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_C_input
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_P_input
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_Si_input

    implicit none

    integer (int_kind)                        , intent(in)    :: num_elements
    type(marbl_surface_forcing_indexing_type) , intent(in)    :: surface_forcing_ind
    type(marbl_forcing_fields_type)           , intent(in)    :: surface_input_forcings(:)
    real (r8)                                 , intent(in)    :: surface_vals(:,:)
    real (r8)                                 , intent(out)   :: surface_tracer_fluxes(:,:)
    type(marbl_tracer_index_type)             , intent(in)    :: marbl_tracer_indices
    type(marbl_saved_state_type)              , intent(inout) :: saved_state
    type(marbl_surface_saved_state_indexing_type), intent(in) :: saved_state_ind
    type(marbl_surface_forcing_internal_type) , intent(inout) :: surface_forcing_internal
    type(marbl_surface_forcing_output_type)   , intent(inout) :: surface_forcing_output
    type(marbl_surface_forcing_share_type)    , intent(inout) :: surface_forcing_share
    type(marbl_diagnostics_type)              , intent(inout) :: surface_forcing_diags
    real (r8)                                 , intent(out)   :: glo_avg_fields_surface(:,:)
    type(marbl_log_type)                      , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_mod:marbl_set_surface_forcing'

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

    associate(                                                                                      &
         ind                  => surface_forcing_ind,                                               &

         ifrac        => surface_input_forcings(surface_forcing_ind%ifrac_id)%field_0d,            &
         sst          => surface_input_forcings(surface_forcing_ind%sst_id)%field_0d,              &
         sss          => surface_input_forcings(surface_forcing_ind%sss_id)%field_0d,              &
         xco2         => surface_input_forcings(surface_forcing_ind%xco2_id)%field_0d,             &
         xco2_alt_co2 => surface_input_forcings(surface_forcing_ind%xco2_alt_co2_id)%field_0d,     &
         ap_used      => surface_input_forcings(surface_forcing_ind%atm_pressure_id)%field_0d,     &
         u10_sqr      => surface_input_forcings(surface_forcing_ind%u10_sqr_id)%field_0d,          &
         dust_flux_in => surface_input_forcings(surface_forcing_ind%dust_flux_id)%field_0d,        &
         iron_flux_in => surface_input_forcings(surface_forcing_ind%iron_flux_id)%field_0d,        &
         nox_flux     => surface_input_forcings(surface_forcing_ind%nox_flux_id)%field_0d,         &
         nhy_flux     => surface_input_forcings(surface_forcing_ind%nhy_flux_id)%field_0d,         &

         piston_velocity      => surface_forcing_internal%piston_velocity(:),                       &
         flux_co2             => surface_forcing_internal%flux_co2(:),                              &
         co2star              => surface_forcing_internal%co2star(:),                               &
         dco2star             => surface_forcing_internal%dco2star(:),                              &
         pco2surf             => surface_forcing_internal%pco2surf(:),                              &
         dpco2                => surface_forcing_internal%dpco2(:),                                 &
         co3                  => surface_forcing_internal%co3(:),                                   &
         co2star_alt          => surface_forcing_internal%co2star_alt(:),                           &
         dco2star_alt         => surface_forcing_internal%dco2star_alt(:),                          &
         pco2surf_alt         => surface_forcing_internal%pco2surf_alt(:),                          &
         dpco2_alt            => surface_forcing_internal%dpco2_alt(:),                             &
         schmidt_co2          => surface_forcing_internal%schmidt_co2(:),                           &
         schmidt_o2           => surface_forcing_internal%schmidt_o2(:),                            &
         pv_o2                => surface_forcing_internal%pv_o2(:),                                 &
         pv_co2               => surface_forcing_internal%pv_co2(:),                                &
         o2sat                => surface_forcing_internal%o2sat(:),                                 &
         flux_alt_co2         => surface_forcing_internal%flux_alt_co2(:),                          &
         nhx_surface_emis     => surface_forcing_internal%nhx_surface_emis(:),                      &

         stf                  => surface_tracer_fluxes(:,:),                                        &

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
         alk_alt_co2_ind   => marbl_tracer_indices%alk_alt_co2_ind,                             &

         pv_surf_fields       => surface_forcing_share%pv_surf_fields(:),                           & ! out
         dic_surf_fields      => surface_forcing_share%dic_surf_fields(:),                          & ! out
         co2star_surf_fields  => surface_forcing_share%co2star_surf_fields(:),                      & ! out
         dco2star_surf_fields => surface_forcing_share%dco2star_surf_fields(:),                     & ! out
         co3_surf_fields      => surface_forcing_share%co3_surf_fields(:)                           & ! out
         )

    !-----------------------------------------------------------------------
    !  fluxes initially set to 0
    !-----------------------------------------------------------------------

    stf(:, :) = c0

    !-----------------------------------------------------------------------
    !  Compute total chlorophyll
    !-----------------------------------------------------------------------

    if (sfo_ind%totalChl_id.ne.0) then
      totalChl_loc = c0
      do auto_ind = 1,size(autotrophs)
        totalChl_loc = totalChl_loc +                                         &
          max(c0, surface_vals(:,marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind))
      end do
      surface_forcing_output%sfo(sfo_ind%totalChl_id)%forcing_field = totalChl_loc
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
          flux_o2_loc(:) = pv_o2(:) * (o2sat(:) - surface_vals(:, o2_ind))
          stf(:, o2_ind) = stf(:, o2_ind) + flux_o2_loc(:)
          if (sfo_ind%flux_o2_id.ne.0) then
            surface_forcing_output%sfo(sfo_ind%flux_o2_id)%forcing_field = flux_o2_loc
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
          ! pass in sections of surface_input_forcings instead of associated vars because of problems with intel/15.0.3
          call marbl_co2calc_surface(                                      &
               num_elements     = num_elements,                            &
               lcomp_co2calc_coeffs = .true.,                              &
               dic_in     = surface_vals(:,dic_ind),                       &
               xco2_in    = surface_input_forcings(ind%xco2_id)%field_0d,  &
               ta_in      = surface_vals(:,alk_ind),                       &
               pt_in      = surface_vals(:,po4_ind),                       &
               sit_in     = surface_vals(:,sio3_ind),                      &
               temp       = surface_input_forcings(ind%sst_id)%field_0d,   &
               salt       = surface_input_forcings(ind%sss_id)%field_0d,   &
               atmpres    = surface_input_forcings(ind%atm_pressure_id)%field_0d, &
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
            surface_forcing_output%sfo(sfo_ind%flux_co2_id)%forcing_field = flux_co2
          end if

          !-------------------------------------------------------------------
          !  The following variables need to be shared with other modules,
          !  and are now defined in marbl_share as targets.
          !-------------------------------------------------------------------

          if (ciso_on) then
             pv_surf_fields(:)       = pv_co2(:)
             dic_surf_fields(:)      = surface_vals(:,dic_ind)
             co2star_surf_fields(:)  = co2star(:)
             dco2star_surf_fields(:) = dco2star(:)
             co3_surf_fields(:)      = co3(:)
          endif

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
          ! pass in sections of surface_input_forcings instead of associated vars because of problems with intel/15.0.3
          call marbl_co2calc_surface(                                      &
               num_elements     = num_elements,                            &
               lcomp_co2calc_coeffs = .false.,                             &
               dic_in     = surface_vals(:,dic_alt_co2_ind),               &
               xco2_in    = surface_input_forcings(ind%xco2_alt_co2_id)%field_0d, &
               ta_in      = surface_vals(:,alk_alt_co2_ind),               &
               pt_in      = surface_vals(:,po4_ind),                       &
               sit_in     = surface_vals(:,sio3_ind),                      &
               temp       = surface_input_forcings(ind%sst_id)%field_0d,   &
               salt       = surface_input_forcings(ind%sss_id)%field_0d,   &
               atmpres    = surface_input_forcings(ind%atm_pressure_id)%field_0d, &
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
             return
          end if

          if (marbl_status_log%lwarning) then
             call marbl_status_log%log_warning_trace('marbl_co2calc_surface() with flux_alt_co2', subname)
          end if

          flux_alt_co2(:) = pv_co2(:) * dco2star_alt(:)

          !-----------------------------------------------------------------------
          !  set air-sea co2 gas flux named field, converting units from
          !  nmol/cm^2/s (positive down) to kg CO2/m^2/s (positive down)
          !-----------------------------------------------------------------------

          stf(:, dic_ind)         = stf(:, dic_ind)         + flux_co2(:)
          stf(:, dic_alt_co2_ind) = stf(:, dic_alt_co2_ind) + FLUX_ALT_CO2(:)

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
      call marbl_comp_nhx_surface_emis(                &
           num_elements     = num_elements,            &
           nh4              = surface_vals(:,nh4_ind), &
           ph               = ph_prev_surf,            &
           sst              = sst,                     &
           sss              = sss,                     &
           u10_sqr          = u10_sqr,                 &
           atmpres          = ap_used,                 &
           ifrac            = ifrac,                   &
           nhx_surface_emis = nhx_surface_emis)

      if (sfo_ind%flux_nhx_id.ne.0) then
         surface_forcing_output%sfo(sfo_ind%flux_nhx_id)%forcing_field = nhx_surface_emis
      end if

      stf(:, nh4_ind) = stf(:, nh4_ind) - nhx_surface_emis(:)
    endif

    !-----------------------------------------------------------------------
    !  calculate iron and dust fluxes if necessary
    !-----------------------------------------------------------------------

    stf(:, fe_ind) = stf(:, fe_ind) + iron_flux_in(:)

    !-----------------------------------------------------------------------
    !  Add phosphate and silicate from dust after Krishnamurthy et al. (2010)
    !  factors convert from g/cm2/s to nmol/cm2/s
    !  ( P frac in dust by weight) * ( P solubility) / ( P molecular weight) * (mol->nmol)
    !  (Si frac in dust by weight) * (Si solubility) / (Si molecular weight) * (mol->nmol)
    !-----------------------------------------------------------------------

    stf(:, po4_ind) = stf(:, po4_ind)   + (dust_flux_in * (0.00105_r8 *  0.15_r8 / 30.974_r8 * 1.0e9_r8))

    stf(:, sio3_ind) = stf(:, sio3_ind) + (dust_flux_in * (  0.308_r8 * 0.075_r8 / 28.085_r8 * 1.0e9_r8))

    !-----------------------------------------------------------------------
    !  calculate nox and nhy fluxes if necessary
    !-----------------------------------------------------------------------

    if (surface_forcing_ind%nox_flux_id.ne.0) then
      stf(:, no3_ind) = stf(:, no3_ind) + nox_flux(:)
    endif

    if (surface_forcing_ind%nhy_flux_id.ne.0) then
      stf(:, nh4_ind) = stf(:, nh4_ind) + nhy_flux(:)
    endif

    !-----------------------------------------------------------------------
    !  Apply NO & NH fluxes to alkalinity
    !-----------------------------------------------------------------------

    stf(:, alk_ind)         = stf(:, alk_ind)         + stf(:, nh4_ind) - stf(:, no3_ind)
    stf(:, alk_alt_co2_ind) = stf(:, alk_alt_co2_ind) + stf(:, nh4_ind) - stf(:, no3_ind)

    !-----------------------------------------------------------------------
    ! Set surface forcing diagnostics
    !-----------------------------------------------------------------------

    call marbl_diagnostics_set_surface_forcing(               &
         surface_forcing_ind      = ind,                      &
         surface_input_forcings   = surface_input_forcings,   &
         surface_forcing_internal = surface_forcing_internal, &
         marbl_tracer_indices     = marbl_tracer_indices,     &
         saved_state              = saved_state,              &
         saved_state_ind          = saved_state_ind,          &
         surface_forcing_diags    = surface_forcing_diags)

    !-----------------------------------------------------------------------
    ! Compute carbon isotopes surface fluxes
    !-----------------------------------------------------------------------

    if (ciso_on) then
       ! pass in sections of surface_input_forcings instead of associated vars because of problems with intel/15.0.3
       call marbl_ciso_set_surface_forcing(                                              &
            num_elements                = num_elements,                                  &
            sst                         = surface_input_forcings(ind%sst_id)%field_0d,   &
            d13c                        = surface_input_forcings(ind%d13c_id)%field_0d,  &
            d14c                        = surface_input_forcings(ind%d14c_id)%field_0d,  &
            surface_vals                = surface_vals,                                  &
            stf                         = surface_tracer_fluxes,                         &
            marbl_tracer_indices        = marbl_tracer_indices,                          &
            marbl_surface_forcing_share = surface_forcing_share,                         &
            marbl_surface_forcing_diags = surface_forcing_diags)
    end if

    !-----------------------------------------------------------------------

    end associate

    if (ladjust_bury_coeff) then
       associate(                                                                              &
          stf          => surface_tracer_fluxes(:,:),                                          &
          flux_co2     => surface_forcing_internal%flux_co2(:),                                &
          ext_C_flux   => surface_input_forcings(surface_forcing_ind%ext_C_flux_id)%field_0d,  &
          ext_P_flux   => surface_input_forcings(surface_forcing_ind%ext_P_flux_id)%field_0d,  &
          ext_Si_flux  => surface_input_forcings(surface_forcing_ind%ext_Si_flux_id)%field_0d, &

          dic_ind      => marbl_tracer_indices%dic_ind,                                        &
          doc_ind      => marbl_tracer_indices%doc_ind,                                        &
          docr_ind     => marbl_tracer_indices%docr_ind,                                       &
          po4_ind      => marbl_tracer_indices%po4_ind,                                        &
          dop_ind      => marbl_tracer_indices%dop_ind,                                        &
          dopr_ind     => marbl_tracer_indices%dopr_ind,                                       &
          sio3_ind     => marbl_tracer_indices%sio3_ind                                        &
          )

          glo_avg_fields_surface(:,glo_avg_field_ind_surface_C_input) = &
             ext_C_flux(:) + stf(:,dic_ind) - flux_co2(:) + stf(:,doc_ind) + stf(:,docr_ind)

          glo_avg_fields_surface(:,glo_avg_field_ind_surface_P_input) = &
             ext_P_flux(:) + stf(:,po4_ind) + stf(:,dop_ind) + stf(:,dopr_ind)

          glo_avg_fields_surface(:,glo_avg_field_ind_surface_Si_input) = &
             ext_Si_flux(:) + stf(:,sio3_ind)
       end associate
    end if

  end subroutine marbl_set_surface_forcing

  !***********************************************************************

end module marbl_surface_mod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
