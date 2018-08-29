! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_ciso_surface_flux_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : c1000

  use marbl_interface_public_types, only : marbl_diagnostics_type

  use marbl_interface_private_types, only : marbl_surface_flux_share_type
  use marbl_interface_private_types, only : marbl_tracer_index_type

  use marbl_settings_mod, only : ciso_on

  implicit none
  private

  !-----------------------------------------------------------------------
  !  public/private declarations
  !-----------------------------------------------------------------------

  public  :: marbl_ciso_surface_flux_compute

  !***********************************************************************

contains

  !***********************************************************************

  subroutine marbl_ciso_surface_flux_compute(&
       num_elements,                         &
       surface_flux_forcing_ind,             &
       surface_flux_forcings,                &
       tracers_at_surface,                   &
       surface_fluxes,                       &
       marbl_tracer_indices,                 &
       marbl_surface_flux_share,             &
       marbl_surface_flux_diags)

    !  Compute surface fluxes for ciso tracers

    use marbl_constants_mod, only : R13C_std
    use marbl_constants_mod, only : R14C_std
    use marbl_interface_private_types, only : marbl_surface_flux_forcing_indexing_type
    use marbl_ciso_diagnostics_mod, only : marbl_ciso_diagnostics_surface_flux_compute
    use marbl_interface_public_types, only : marbl_forcing_fields_type

    integer (int_kind),                             intent(in)    :: num_elements
    type(marbl_surface_flux_forcing_indexing_type), intent(in)    :: surface_flux_forcing_ind
    type(marbl_forcing_fields_type),                intent(in)    :: surface_flux_forcings(:)
    real(r8),                                       intent(in)    :: tracers_at_surface(:,:)
    type(marbl_surface_flux_share_type),            intent(in)    :: marbl_surface_flux_share
    real(r8),                                       intent(inout) :: surface_fluxes(:, :)
    type(marbl_tracer_index_type),                  intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type),                   intent(inout) :: marbl_surface_flux_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real (r8), dimension(num_elements) :: &
         R13C_DIC,                        & ! 13C/12C ratio in DIC
         R14C_DIC,                        & ! 14C/12C ratio in total DIC
         R13C_atm,                        & ! 13C/12C ratio in atmospheric CO2
         R14C_atm,                        & ! 14C/12C ratio in atmospheric CO2
         flux,                            & ! gas flux of CO2 (nmol/cm^2/s)
         flux13,                          & ! gas flux of 13CO2 (nmol/cm^2/s)
         flux14,                          & ! gas flux of 14CO2 (nmol/cm^2/s)
         flux13_as,                       & ! air-to-sea gas flux of 13CO2 (nmol/cm^2/s)
         flux14_as,                       & ! air-to-sea gas flux of 14CO2 (nmol/cm^2/s)
         flux13_sa,                       & ! sea-to-air gas flux of 13CO2 (nmol/cm^2/s)
         flux14_sa                          ! sea-to-air gas flux of 14CO2 (nmol/cm^2/s)

    real (r8), dimension(num_elements) :: &
         eps_aq_g_surf,                   & ! equilibrium fractionation (CO2_gaseous <-> CO2_aq)
         alpha_aq_g_surf,                 & ! alpha_xxx_g_surf => eps = ( alpa -1 ) * 1000
         eps_dic_g_surf,                  & ! equilibrium fractionation between total DIC and gaseous CO2
         alpha_dic_g_surf,                & ! alpha_xxx_g_surf => eps = ( alpa -1 ) * 1000
         frac_co3,                        & ! carbonate fraction fCO3 = [CO3--]/DIC
         alpha_aq_g_surf_14c,             & ! for 14C, with fractionation being twice as large for 14C than for 13C
         alpha_dic_g_surf_14c               ! for 14C, with fractionation being twice as large for 14C than for 13C

    ! local parameters for 13C, Zhang et al, 1995, Geochim. et Cosmochim. Acta, 59 (1), 107-114
    real(r8) ::            &
         alpha_k,          & ! eps = ( alpa -1 ) * 1000
         alpha_k_14c         ! for 14C, with fractionation being twice as large for 14C than for 13C

    ! kinetic fraction during gas transfer (per mil) (air-sea CO2 exchange)
    ! average of Zhang et al 1995 values of -0.81 at 21C and -0.95 at 5C
    real(r8), parameter :: eps_k = -0.88_r8
    !-----------------------------------------------------------------------

    ! Return immediately if not running with carbon isotope tracer module
    if (.not. ciso_on) return

    associate(                                                                      &
         sst  => surface_flux_forcings(surface_flux_forcing_ind%sst_id)%field_0d,   &
         d13c => surface_flux_forcings(surface_flux_forcing_ind%d13c_id)%field_0d,  &
         d14c => surface_flux_forcings(surface_flux_forcing_ind%d14c_id)%field_0d,  &

         pv                  => marbl_surface_flux_share%pv_surf_fields       , & ! in/out
         dic                 => marbl_surface_flux_share%dic_surf_fields      , & ! in/out DIC values for solver
         co2star             => marbl_surface_flux_share%co2star_surf_fields  , & ! in/out CO2STAR from solver
         dco2star            => marbl_surface_flux_share%dco2star_surf_fields , & ! in/out DCO2STAR from solver
         co3_surf_fields     => marbl_surface_flux_share%co3_surf_fields      , & ! in/out

         di13c_ind          => marbl_tracer_indices%di13c_ind                  , &
         do13ctot_ind       => marbl_tracer_indices%do13ctot_ind               , &
         di14c_ind          => marbl_tracer_indices%di14c_ind                  , &
         do14ctot_ind       => marbl_tracer_indices%do14ctot_ind               , &
         ciso_ind_beg       => marbl_tracer_indices%ciso%ind_beg               , &
         ciso_ind_end       => marbl_tracer_indices%ciso%ind_end                 &
         )

    !-----------------------------------------------------------------------
    !  ciso fluxes initially set to 0
    !-----------------------------------------------------------------------

    surface_fluxes(:,ciso_ind_beg:ciso_ind_end) = c0

    !-----------------------------------------------------------------------
    !     initialize R13C_atm  and R14C_atm
    !-----------------------------------------------------------------------

    R13C_atm(:) = R13C_std * ( c1 + d13c(:) / c1000 )
    R14C_atm(:) = R14C_std * ( c1 + d14c(:) / c1000 )

    !-----------------------------------------------------------------------
    !     compute 13C02 flux, based on CO2 flux calculated in ecosystem model
    !     Zhang et al, 1995, Geochim. et Cosmochim. Acta, 59 (1), 107-114
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !     compute R13C_DIC in surface ocean (assuming that DIC is 12C)
    !-----------------------------------------------------------------------

    where ( dic(:) /= c0 )
       R13C_dic(:) = tracers_at_surface(:,di13c_ind) / dic(:)
       R14C_dic(:) = tracers_at_surface(:,di14c_ind) / dic(:)
       frac_co3(:) = CO3_SURF_fields(:) / dic(:)
    elsewhere
       R13C_dic(:) = c0
       R14C_dic(:) = c0
       frac_co3(:) = c0
    end where

    !-----------------------------------------------------------------------
    !     individal discrimination factor of each species with respect to
    !     gaseous CO2, temperature dependent, based on Zhang et al. 95
    !-----------------------------------------------------------------------
    eps_aq_g_surf(:)   = 0.0049_r8 * sst(:) - 1.31_r8

    !-----------------------------------------------------------------------
    !     compute the equilibrium discrimination factor between dic and
    !     gaseous CO2
    !-----------------------------------------------------------------------
    !     function of T and carbonate fraction (frac_co3)
    !     based on the empirical relationship from the measured
    !     e_dic_g_surf of Zhang et al. 1995
    !---------------------------------------------------------------------

    eps_dic_g_surf(:) = 0.014_r8 * sst(:) * frac_co3(:) - 0.105_r8 * sst(:) + 10.53_r8

    !-----------------------------------------------------------------------
    !     compute alpha coefficients from eps :  eps = ( alpha -1 ) * 1000
    !     => alpha = 1 + eps / 1000
    !-----------------------------------------------------------------------

    alpha_k             = c1 + eps_k             / c1000
    alpha_aq_g_surf(:)  = c1 + eps_aq_g_surf(:)  / c1000
    alpha_dic_g_surf(:) = c1 + eps_dic_g_surf(:) / c1000

    ! Fractionation is twice as large for 14C than for 13C, so eps needs to be multiplied by 2 for 14C
    alpha_k_14c             = c1 + eps_k * 2.0_r8            / c1000
    alpha_aq_g_surf_14c(:)  = c1 + eps_aq_g_surf(:) *2.0_r8  / c1000
    alpha_dic_g_surf_14c(:) = c1 + eps_dic_g_surf(:) *2.0_r8 / c1000

    !-----------------------------------------------------------------------
    !     compute 13C flux and C flux
    !-----------------------------------------------------------------------

    flux13(:) = pv(:) * alpha_k * alpha_aq_g_surf(:) * &
         (( co2star(:) + dco2star(:) ) * r13c_atm(:) - co2star(:) * r13c_dic(:) / alpha_dic_g_surf(:))

    flux14(:) = pv(:) * alpha_k_14c * alpha_aq_g_surf_14c(:) * &
         (( co2star(:) + dco2star(:) ) * r14c_atm(:) - co2star(:) * r14c_dic(:) / alpha_dic_g_surf_14c(:))

    flux(:) = pv(:) * dco2star(:)

    !-----------------------------------------------------------------------
    !     compute fluxes in and out
    !-----------------------------------------------------------------------

    flux13_as(:) = pv(:) * alpha_k     * alpha_aq_g_surf(:)     * ((co2star(:) + dco2star(:)) * r13c_atm(:))
    flux14_as(:) = pv(:) * alpha_k_14c * alpha_aq_g_surf_14c(:) * ((co2star(:) + dco2star(:)) * r14c_atm(:))

    flux13_sa(:) = pv(:) * alpha_k     * alpha_aq_g_surf(:)     * (co2star(:) * r13c_dic(:) / alpha_dic_g_surf(:))
    flux14_sa(:) = pv(:) * alpha_k_14c * alpha_aq_g_surf_14c(:) * (co2star(:) * r14c_dic(:) / alpha_dic_g_surf_14c(:))

    !-----------------------------------------------------------------------
    !     end of 13C computation for gass exchange
    !-----------------------------------------------------------------------

    surface_fluxes(:,di13c_ind) = surface_fluxes(:,di13c_ind) + flux13(:)
    surface_fluxes(:,di14c_ind) = surface_fluxes(:,di14c_ind) + flux14(:)

    ! update carbon isotope diagnostics
    ! FIXME #18: the following arguments need to be group into a derived type

    call marbl_ciso_diagnostics_surface_flux_compute( &
         num_elements,   &
         d13c,           &
         d14c,           &
         flux,           &
         flux13,         &
         flux14,         &
         flux13_as,      &
         flux14_as,      &
         flux13_sa,      &
         flux14_sa,      &
         R13C_dic,       &
         R14C_dic,       &
         R13C_atm,       &
         R14C_atm,       &
         eps_aq_g_surf,  &
         eps_dic_g_surf, &
         marbl_surface_flux_diags)

    end associate

  end subroutine marbl_ciso_surface_flux_compute

end module marbl_ciso_surface_flux_mod
