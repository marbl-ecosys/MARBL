module marbl_parms

  !-----------------------------------------------------------------------------
  !   This module manages BGC-specific parameters.
  !
  !   Most of the variables are not parameters in the Fortran sense. In the
  !   the Fortran sense, they are vanilla module variables most of which are
  !   associated with the MARBL namelist (marbl_parms_nml)
  !
  !   In addition to containing all the namelist variables, this modules also
  !   handles initializing the variables in &marbl_parms_nml to default values
  !   and then reading that specific namelist.
  !
  !   This module also writes parameter values to the status log.
  !-----------------------------------------------------------------------------

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_config_mod, only : marbl_config_and_parms_type

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : c2
  use marbl_constants_mod, only : c1000
  use marbl_constants_mod, only : dps
  use marbl_constants_mod, only : molw_Fe

  use marbl_internal_types, only : autotroph_parms_type
  use marbl_internal_types, only : zooplankton_parms_type
  use marbl_internal_types, only : grazing_parms_type

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt
  use marbl_sizes, only : grazer_prey_cnt

  use marbl_logging, only: marbl_log_type

  implicit none

  !-----------------------------------------------------------------------------
  !   public/private declarations
  !   all module variables are public and should have their values preserved
  !-----------------------------------------------------------------------------

  public
  save

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_parms_nml
  !---------------------------------------------------------------------

  real(kind=r8), target :: &
       parm_Fe_bioavail,           & ! fraction of Fe flux that is bioavailable
       parm_o2_min,                & ! min O2 needed for prod & consump. (nmol/cm^3)
       parm_o2_min_delta,          & ! width of min O2 range (nmol/cm^3)
       parm_kappa_nitrif_per_day,  & ! nitrification inverse time constant (1/day)
       parm_kappa_nitrif,          & ! nitrification inverse time constant (1/sec) (derived from parm_kappa_nitrif_per_day)
       parm_nitrif_par_lim,        & ! PAR limit for nitrif. (W/m^2)
       parm_labile_ratio,          & ! fraction of loss to DOC that routed directly to DIC (non-dimensional)
       parm_init_POC_bury_coeff,   & ! initial scale factor for burial of POC, PON
       parm_init_POP_bury_coeff,   & ! initial scale factor for burial of POP
       parm_init_bSi_bury_coeff,   & ! initial scale factor burial of bSi
       parm_Fe_scavenge_rate0,     & ! scavenging base rate for Fe
       parm_Lig_scavenge_rate0,    & ! scavenging base rate for bound ligand
       parm_FeLig_scavenge_rate0,  & ! scavenging base rate for bound iron
       parm_Lig_degrade_rate0,     & ! Fe-binding ligand bacterial degradation base rate coefficient
       parm_Fe_desorption_rate0,   & ! desorption rate for scavenged Fe from particles
       parm_f_prod_sp_CaCO3,       & ! fraction of sp prod. as CaCO3 prod.
       parm_POC_diss,              & ! base POC diss len scale
       parm_SiO2_diss,             & ! base SiO2 diss len scale
       parm_CaCO3_diss,            & ! base CaCO3 diss len scale
       parm_sed_denitrif_coeff,    & ! global scaling factor for sed_denitrif
       bury_coeff_rmean_timescale_years

  real(kind=r8), dimension(4), target :: &
       parm_scalelen_z,       & ! depths of prescribed scalelen values
       parm_scalelen_vals       ! prescribed scalelen values

  type(zooplankton_parms_type), target :: zooplankton(zooplankton_cnt)
  type(autotroph_parms_type),   target :: autotrophs(autotroph_cnt)
  type(grazing_parms_type),     target :: grazing(grazer_prey_cnt, zooplankton_cnt)

  real(r8),                target :: iron_frac_in_dust            ! fraction by weight of iron in dust
  real(r8),                target :: iron_frac_in_bc              ! fraction by weight of iron in black carbon
  character(len=char_len), target :: caco3_bury_thres_opt         ! option of threshold of caco3 burial ['fixed_depth', 'omega_calc']
  real(r8),                target :: caco3_bury_thres_depth       ! threshold depth for caco3_bury_thres_opt='fixed_depth'
  ! -----------
  ! PON_sed_loss = PON_bury_coeff * Q * POC_sed_loss
  ! factor is used to avoid overburying PON like POC
  ! is when total C burial is matched to C riverine input
  ! -----------
  real(r8),                target :: PON_bury_coeff
  character(len=char_len), target :: ciso_fract_factors           ! option for which biological fractionation calculation to use

  character(len=char_len), allocatable, target, dimension(:) :: tracer_restore_vars

  !---------------------------------------------------------------------
  !  BGC parameters that are not part of marbl_parms_nml
  !---------------------------------------------------------------------

  ! Redfield Ratios, dissolved & particulate
  real(kind=r8), parameter :: &
       Q_10      =   1.7_r8,                                 & ! factor for temperature dependence (non-dim)
       xkw_coeff =   6.97e-9_r8,                             & ! in s/cm, from a = 0.251 cm/hr s^2/m^2 in Wannikhof 2014
       parm_Red_D_C_P  = 117.0_r8,                           & ! carbon:phosphorus
       parm_Red_D_N_P  =  16.0_r8,                           & ! nitrogen:phosphorus
       parm_Red_D_O2_P = 170.0_r8,                           & ! oxygen:phosphorus
       parm_Remin_D_O2_P = 138.0_r8,                         & ! oxygen:phosphorus
       parm_Red_P_C_P  = parm_Red_D_C_P,                     & ! carbon:phosphorus
       parm_Red_D_C_N  = parm_Red_D_C_P/parm_Red_D_N_P,      & ! carbon:nitrogen
       parm_Red_P_C_N  = parm_Red_D_C_N,                     & ! carbon:nitrogen
       parm_Red_D_C_O2 = parm_Red_D_C_P/parm_Red_D_O2_P,     & ! carbon:oxygen
       parm_Remin_D_C_O2 = parm_Red_D_C_P/parm_Remin_D_O2_P, & ! carbon:oxygen
       parm_Red_P_C_O2 = parm_Red_D_C_O2,                    & ! carbon:oxygen
       parm_Red_Fe_C   = 3.0e-6_r8,                          & ! iron:carbon
       parm_Red_D_C_O2_diaz = parm_Red_D_C_P/150.0_r8          ! carbon:oxygen
                                                               ! for diazotrophs

  ! Misc. Rate constants
  real(kind=r8), parameter :: &
       dust_Fe_scavenge_scale  = 1.0e9         !dust scavenging scale factor

  ! dust_to_Fe: conversion of dust to iron (nmol Fe/g Dust)
  ! dust remin gDust = 0.035 gFe       mol Fe     1e9 nmolFe
  !                    --------- *  ----------- * ----------
  !                      gDust      molw_Fe gFe      molFe
  real(kind=r8), parameter :: dust_to_Fe = 0.035_r8 / molw_Fe * 1.0e9_r8

  ! parameters related to Iron binding ligands
  integer (int_kind), parameter :: Lig_cnt = 1 ! valid values are 1 or 2
  real(kind=r8), parameter :: &
       remin_to_Lig = 0.0001_r8

  ! Partitioning of phytoplankton growth, grazing and losses
  ! All f_* variables are fractions and are non-dimensional
  real(kind=r8), parameter :: &
      caco3_poc_min         = 0.40_r8,  & ! minimum proportionality between
                                          !   QCaCO3 and grazing losses to POC
                                          !   (mmol C/mmol CaCO3)
      spc_poc_fac           = 0.13_r8,  & ! small phyto grazing factor (1/mmolC)
      f_graze_sp_poc_lim    = 0.36_r8,  &
      f_photosp_CaCO3       = 0.40_r8,  & ! proportionality between small phyto
                                          !    production and CaCO3 production
      f_graze_CaCO3_remin   = 0.33_r8,  & ! fraction of spCaCO3 grazing which is remin
      f_graze_si_remin      = 0.50_r8,  & ! fraction of diatom Si grazing which is remin
      f_toDON               = 0.70_r8,  & ! fraction DON relative to DOC
      f_toDOP               = 0.15_r8     ! fraction of remaining_P to DOP


  ! fixed ratios
  real(kind=r8), parameter :: &
       r_Nfix_photo=1.25_r8         ! N fix relative to C fix (non-dim)

  ! SET parmaeters and RATIOS for N/C, P/C, SiO3/C, Fe/C, etc...
  real(kind=r8), parameter :: &
      Q             = 16.0_r8 / 117.0_r8, & !N/C ratio (mmol/mmol) of phyto & zoo
      Qp_zoo        = c1 / 117.0_r8,      & !P/C ratio (mmol/mmol) zoo
      Qfe_zoo       = 3.0e-6_r8,          & !zooplankton Fe/C ratio
      gQsi_0        = 0.137_r8,           & !initial Si/C ratio for growth
      gQsi_max      = 0.685_r8,           & !max Si/C ratio for growth
      gQsi_min      = 0.0457_r8,          & !min Si/C ratio for growth
      QCaCO3_max    = 1.0_r8,             & !max QCaCO3 for implicit calcifiers
      QCaCO3_max_exp = 2.0_r8,            & !max QCaCO3 for explicit calcifiers
      ! parameters in GalbraithMartiny Pquota Model^M
      PquotaSlope     = 7.0_r8,        &
      PquotaIntercept = 5.571_r8,      &
      PquotaMinNP     = 0.00854701_r8, &
      ! carbon:nitrogen ratio for denitrification
      denitrif_C_N  = parm_Red_D_C_P/136.0_r8

  ! loss term threshold parameters, chl:c ratios
  real(kind=r8), parameter :: &
      thres_z1_auto     =  80.0e2_r8, & !autotroph threshold = C_loss_thres for z shallower than this (cm)
      thres_z2_auto     = 120.0e2_r8, & !autotroph threshold = 0 for z deeper than this (cm)
      thres_z1_zoo      = 110.0e2_r8, & !zooplankton threshold = C_loss_thres for z shallower than this (cm)
      thres_z2_zoo      = 150.0e2_r8, & !zooplankton threshold = 0 for z deeper than this (cm)
      CaCO3_temp_thres1 = 4.0_r8,   & !upper temp threshold for CaCO3 prod
      CaCO3_temp_thres2 = -2.0_r8,  & !lower temp threshold
      CaCO3_sp_thres    = 2.5_r8      ! bloom condition thres (mmolC/m3)

  ! fraction of incoming shortwave assumed to be PAR
  real(kind=r8), parameter :: &
       f_qsw_par = 0.45_r8   ! PAR fraction

  ! DOM parameters for refractory components and DOP uptake
  real(kind=r8), parameter :: &
       DOC_reminR_light  = (c1/(365.0_r8*15.0_r8)) * dps, & ! remin rate for semi-labile DOC, 1/15yr
       DON_reminR_light  = (c1/(365.0_r8*15.0_r8)) * dps, & ! remin rate for semi-labile DON, 1/15yr
       DOP_reminR_light  = (c1/(365.0_r8*60.0_r8)) * dps, & ! remin rate for semi-labile DOP, 1/60yr
       DOC_reminR_dark   = (c1/(365.0_r8*6.0_r8)) * dps,  & ! remin rate in the dark, 1/6yr
       DON_reminR_dark   = (c1/(365.0_r8*5.5_r8)) * dps,  & ! remin rate in the dark, 1/5.5yr
       DOP_reminR_dark   = (c1/(365.0_r8*4.5_r8)) * dps     ! remin rate in the dark, 1/4.5yr

  real(kind=r8), parameter :: &
       DOCr_reminR0      = (c1/(365.0_r8*16000.0_r8)) * dps, & ! remin rate for refractory DOC, 1/16000yr
       DONr_reminR0      = (c1/(365.0_r8*9500.0_r8)) * dps,  & ! remin rate for refractory DON, 1/9500yr
       DOPr_reminR0      = (c1/(365.0_r8*5500.0_r8)) * dps,  & ! remin rate for refractory DOP, 1/5500yr
       DOMr_reminR_photo = (c1/(365.0_r8*18.0_r8)) * dps       ! additional remin from photochemistry, 1/18yrs over top 10m

  real(kind=r8), parameter :: &
       DOCprod_refract  = 0.01_r8,                   & ! fraction of DOCprod to refractory pool
       DONprod_refract  = 0.0115_r8,                 & ! fraction of DONprod to refractory pool
       DOPprod_refract  = 0.003_r8,                  & ! fraction of DOPprod to refractory pool
       POCremin_refract = DOCprod_refract * 0.06_r8, & ! fraction of POCremin to refractory pool
       PONremin_refract = DONprod_refract * 0.03_r8, & ! fraction of POCremin to refractory pool
       POPremin_refract = DOPprod_refract * 0.06_r8    ! fraction of POCremin to refractory pool

  !---------------------------------------------------------------------
  !  Auxiliary variables (str -> int conversions, indices, etc)
  !---------------------------------------------------------------------

  integer (int_kind)            :: caco3_bury_thres_iopt
  integer (int_kind), parameter :: caco3_bury_thres_iopt_fixed_depth = 1
  integer (int_kind), parameter :: caco3_bury_thres_iopt_omega_calc  = 2

  ! grazing functions
  integer (kind=int_kind), parameter ::           &
         grz_fnc_michaelis_menten = 1,       &
         grz_fnc_sigmoidal        = 2

  !*****************************************************************************

  public :: &
       marbl_parms_read_namelist, &
       marbl_parms_set_defaults

  ! Variables used from other modules should be private
  ! (So we don't accidentally use them from this module)
  private :: r8, int_kind, log_kind, char_len
  private :: c1, dps
  private :: zooplankton_parms_type, autotroph_parms_type, grazing_parms_type
  private :: autotroph_cnt, zooplankton_cnt, grazer_prey_cnt
  private :: marbl_log_type

contains

  !*****************************************************************************

  subroutine marbl_parms_set_defaults(km)
    ! assign default values to all module variables

    ! NOTE: defaults values below, of vars in the marbl_parms framework, may be overridden at runtime
    !       through either a namelist read or a put call from marbl_config_and_parms_type class

    use marbl_sizes           , only : marbl_total_tracer_cnt
    use marbl_config_mod      , only : autotrophs_config
    use marbl_config_mod      , only : zooplankton_config

    integer,              intent(in)    :: km ! max number of levels

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    integer :: m, n
    !---------------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  &marbl_parms_nml
    !-----------------------------------------------------------------------

    parm_Fe_bioavail           = 1.0_r8         ! in marbl_parms framework, see NOTE above
    parm_o2_min                = 5.0_r8         ! in marbl_parms framework, see NOTE above
    parm_o2_min_delta          = 5.0_r8         ! in marbl_parms framework, see NOTE above
    parm_kappa_nitrif_per_day  = 0.06_r8        ! in marbl_parms framework, see NOTE above
    parm_nitrif_par_lim        = 1.0_r8         ! in marbl_parms framework, see NOTE above
    parm_labile_ratio          = 0.94_r8        ! in marbl_parms framework, see NOTE above
    parm_init_POC_bury_coeff   = 1.1_r8         ! in marbl_parms framework, see NOTE above
    parm_init_POP_bury_coeff   = 1.1_r8         ! in marbl_parms framework, see NOTE above
    parm_init_bSi_bury_coeff   = 1.0_r8         ! in marbl_parms framework, see NOTE above
    parm_Fe_scavenge_rate0     = 15.0_r8        ! in marbl_parms framework, see NOTE above
    parm_Lig_scavenge_rate0    = 0.015_r8       ! in marbl_parms framework, see NOTE above
    parm_FeLig_scavenge_rate0  = 1.3_r8         ! in marbl_parms framework, see NOTE above
    parm_Lig_degrade_rate0     = 0.000094_r8    ! in marbl_parms framework, see NOTE above
    parm_Fe_desorption_rate0   = 1.0e-6_r8      ! in marbl_parms framework, see NOTE above
    parm_f_prod_sp_CaCO3       = 0.070_r8       ! in marbl_parms framework, see NOTE above
    parm_POC_diss              = 100.0e2_r8     ! in marbl_parms framework, see NOTE above
    parm_SiO2_diss             = 770.0e2_r8     ! in marbl_parms framework, see NOTE above
    parm_CaCO3_diss            = 500.0e2_r8     ! in marbl_parms framework, see NOTE above
    parm_sed_denitrif_coeff    = 1.0_r8         ! in marbl_parms framework, see NOTE above
    bury_coeff_rmean_timescale_years = 10.0_r8  ! in marbl_parms framework, see NOTE above
    parm_scalelen_z    = (/ 100.0e2_r8, 250.0e2_r8, 500.0e2_r8, 1000.0e2_r8 /) ! in marbl_parms framework, see NOTE above
    parm_scalelen_vals = (/     1.0_r8,     2.2_r8,     4.0_r8,      5.0_r8 /) ! in marbl_parms framework, see NOTE above

    ! Autotrophs
    do n=1,autotroph_cnt
      select case (trim(autotrophs_config(n)%sname))
        case ('sp')
          autotrophs(n)%kFe             = 0.03e-3_r8         ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kPO4            = 0.005_r8           ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kDOP            = 0.3_r8             ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kNO3            = 0.22_r8            ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kNH4            = 0.01_r8            ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kSiO3           = 0.0_r8             ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kCO2            = 0.0_r8             !!!! added
          autotrophs(n)%Qp_fixed        =  Qp_zoo            ! only used for lvariable_PtoC=.false.
          autotrophs(n)%gQfe_0          = 35.0e-6_r8
          autotrophs(n)%gQfe_min        = 3.0e-6_r8
          autotrophs(n)%alphaPI_per_day = 0.39_r8
          autotrophs(n)%PCref_per_day   = 5.0_r8
          autotrophs(n)%thetaN_max      = 2.5_r8
          autotrophs(n)%loss_thres      = 0.01_r8
          autotrophs(n)%loss_thres2     = 0.0_r8
          autotrophs(n)%temp_thres      = -10.0_r8
          autotrophs(n)%mort_per_day    = 0.1_r8
          autotrophs(n)%mort2_per_day   = 0.01_r8
          autotrophs(n)%agg_rate_max    = 0.5_r8
          autotrophs(n)%agg_rate_min    = 0.01_r8
          autotrophs(n)%loss_poc        = 0.0_r8

        case ('diat')
          autotrophs(n)%kFe             = 0.05e-3_r8         ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kPO4            = 0.05_r8            ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kDOP            = 0.5_r8             ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kNO3            = 0.5_r8             ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kNH4            = 0.05_r8            ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kSiO3           = 0.7_r8             ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kCO2            = 0.0_r8             !!!! added
          autotrophs(n)%Qp_fixed        =  Qp_zoo            ! only used for lvariable_PtoC=.false.
          autotrophs(n)%gQfe_0          = 35.0e-6_r8
          autotrophs(n)%gQfe_min        = 3.0e-6_r8
          autotrophs(n)%alphaPI_per_day = 0.29_r8
          autotrophs(n)%PCref_per_day   = 5.2_r8
          autotrophs(n)%thetaN_max      = 4.0_r8
          autotrophs(n)%loss_thres      = 0.02_r8
          autotrophs(n)%loss_thres2     = 0.0_r8
          autotrophs(n)%temp_thres      = -10.0_r8
          autotrophs(n)%mort_per_day    = 0.1_r8
          autotrophs(n)%mort2_per_day   = 0.01_r8
          autotrophs(n)%agg_rate_max    = 0.5_r8
          autotrophs(n)%agg_rate_min    = 0.02_r8
          autotrophs(n)%loss_poc        = 0.0_r8

        case ('diaz')
          autotrophs(n)%kFe             = 0.045e-3_r8        ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kPO4            = 0.015_r8           ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kDOP            = 0.075_r8           ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kNO3            = 2.0_r8             ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kNH4            = 0.2_r8             ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kSiO3           = 0.0_r8             ! in marbl_parms framework, see NOTE above
          autotrophs(n)%kCO2            = 0.0_r8             !!!! added
          autotrophs(n)%Qp_fixed        = 0.32_r8 * Qp_zoo   ! only used for lvariable_PtoC=.false.
          autotrophs(n)%gQfe_0          = 70.0e-6_r8
          autotrophs(n)%gQfe_min        = 6.0e-6_r8
          autotrophs(n)%alphaPI_per_day = 0.39_r8
          autotrophs(n)%PCref_per_day   = 2.2_r8
          autotrophs(n)%thetaN_max      = 2.5_r8
          autotrophs(n)%loss_thres      = 0.02_r8
          autotrophs(n)%loss_thres2     = 0.001_r8
          autotrophs(n)%temp_thres      = 15.0_r8
          autotrophs(n)%mort_per_day    = 0.1_r8
          autotrophs(n)%mort2_per_day   = 0.01_r8
          autotrophs(n)%agg_rate_max    = 0.5_r8
          autotrophs(n)%agg_rate_min    = 0.01_r8
          autotrophs(n)%loss_poc        = 0.0_r8

        case ('cocco')
        autotrophs(n)%kFe             = 0.03e-3_r8         ! in marbl_parms framework, see NOTE above
        autotrophs(n)%kPO4            = 0.006_r8           ! in marbl_parms framework, see NOTE above
        autotrophs(n)%kDOP            = 0.3_r8             ! in marbl_parms framework, see NOTE above
        autotrophs(n)%kNO3            = 0.35_r8            ! in marbl_parms framework, see NOTE above
        autotrophs(n)%kNH4            = 0.012_r8            ! in marbl_parms framework, see NOTE above
        autotrophs(n)%kSiO3           = 0.0_r8             ! in marbl_parms framework, see NOTE above
        autotrophs(n)%kCO2            = 2.12_r8             !!!! added Km for CO2 for coccos
        autotrophs(n)%Qp_fixed        =  Qp_zoo            ! only used for lvariable_PtoC=.false.
        autotrophs(n)%gQfe_0          = 15.0e-6_r8
        autotrophs(n)%gQfe_min        = 3.0e-6_r8
        autotrophs(n)%alphaPI_per_day = 0.30_r8
        autotrophs(n)%PCref_per_day   = 5.0_r8
        autotrophs(n)%thetaN_max      = 3.1_r8
        autotrophs(n)%loss_thres      = 0.01_r8
        autotrophs(n)%loss_thres2     = 0.0_r8
        autotrophs(n)%temp_thres      = 0.0_r8
        autotrophs(n)%mort_per_day    = 0.1_r8
        autotrophs(n)%mort2_per_day   = 0.01_r8
        autotrophs(n)%agg_rate_max    = 0.5_r8
        autotrophs(n)%agg_rate_min    = 0.01_r8
        autotrophs(n)%loss_poc        = 0.0_r8

        case DEFAULT
          autotrophs(n)%kFe             = c0
          autotrophs(n)%kPO4            = c0
          autotrophs(n)%kDOP            = c0
          autotrophs(n)%kNO3            = c0
          autotrophs(n)%kNH4            = c0
          autotrophs(n)%kSiO3           = c0
          autotrophs(n)%kCO2            = c0
          autotrophs(n)%Qp_fixed        = c0
          autotrophs(n)%gQfe_0          = c0
          autotrophs(n)%gQfe_min        = c0
          autotrophs(n)%alphaPI_per_day = c0
          autotrophs(n)%PCref_per_day   = c0
          autotrophs(n)%thetaN_max      = c0
          autotrophs(n)%loss_thres      = c0
          autotrophs(n)%loss_thres2     = c0
          autotrophs(n)%temp_thres      = c0
          autotrophs(n)%mort_per_day    = c0
          autotrophs(n)%mort2_per_day   = c0
          autotrophs(n)%agg_rate_max    = c0
          autotrophs(n)%agg_rate_min    = c0
          autotrophs(n)%loss_poc        = c0
      end select
    end do

    ! zooplankton
    ! TODO: add do loop and select case
    do n=1,zooplankton_cnt
      select case (trim(zooplankton_config(n)%sname))
        case ('zoo')
          zooplankton(n)%z_mort_0_per_day   = 0.1_r8     ! in marbl_parms framework, see NOTE above
          zooplankton(n)%z_mort2_0_per_day  = 0.4_r8     ! in marbl_parms framework, see NOTE above
          zooplankton(n)%loss_thres         = 0.075_r8   ! in marbl_parms framework, see NOTE above
        case DEFAULT
          zooplankton(n)%z_mort_0_per_day   = c0
          zooplankton(n)%z_mort2_0_per_day  = c0
          zooplankton(n)%loss_thres         = c0
      end select
    end do

    ! predator-prey relationships
    do n=1,zooplankton_cnt
      do m=1,grazer_prey_cnt

        ! Properties that are the same for all grazers
        grazing(m,n)%auto_ind(:)      = 0
        grazing(m,n)%auto_ind(1)      = m
        grazing(m,n)%zoo_ind          = -1

        ! Properties that depend on m & n
        if ((trim(zooplankton_config(n)%sname).eq.'zoo').and.                 &
            (trim(autotrophs_config(m)%sname).eq.'sp')) then
          grazing(m,n)%z_umax_0_per_day = 3.3_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%z_grz            = 1.2_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_zoo        = 0.3_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_poc        = 0.0_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_doc        = 0.06_r8   ! in marbl_parms framework, see NOTE above
          grazing(m,n)%f_zoo_detr       = 0.12_r8   ! in marbl_parms framework, see NOTE above
          grazing(m,n)%grazing_function = grz_fnc_michaelis_menten   ! in marbl_parms framework, see NOTE above
        elseif ((trim(zooplankton_config(n)%sname).eq.'zoo').and.             &
                (trim(autotrophs_config(m)%sname).eq.'diat')) then
          grazing(m,n)%z_umax_0_per_day = 3.05_r8
          grazing(m,n)%z_grz            = 1.2_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_zoo        = 0.25_r8   ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_poc        = 0.38_r8   ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_doc        = 0.06_r8   ! in marbl_parms framework, see NOTE above
          grazing(m,n)%f_zoo_detr       = 0.24_r8   ! in marbl_parms framework, see NOTE above
          grazing(m,n)%grazing_function = grz_fnc_michaelis_menten   ! in marbl_parms framework, see NOTE above
        elseif ((trim(zooplankton_config(n)%sname).eq.'zoo').and.             &
                (trim(autotrophs_config(m)%sname).eq.'diaz')) then
          grazing(m,n)%z_umax_0_per_day = 3.1_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%z_grz            = 1.2_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_zoo        = 0.3_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_poc        = 0.1_r8    ! in marbl_parms framework, see NOTE above
          grazing(m,n)%graze_doc        = 0.06_r8   ! in marbl_parms framework, see NOTE above
          grazing(m,n)%f_zoo_detr       = 0.12_r8   ! in marbl_parms framework, see NOTE above
          grazing(m,n)%grazing_function = grz_fnc_michaelis_menten   ! in marbl_parms framework, see NOTE above
        elseif ((trim(zooplankton_config(n)%sname).eq.'zoo').and.             &
        (trim(autotrophs_config(m)%sname).eq.'cocco')) then
        grazing(m,n)%z_umax_0_per_day = 3.2_r8    ! in marbl_parms framework, see NOTE above
        grazing(m,n)%z_grz            = 1.2_r8    ! in marbl_parms framework, see NOTE above
        grazing(m,n)%graze_zoo        = 0.25_r8    ! in marbl_parms framework, see NOTE above
        grazing(m,n)%graze_poc        = 0.3_r8    ! in marbl_parms framework, see NOTE above
        grazing(m,n)%graze_doc        = 0.06_r8   ! in marbl_parms framework, see NOTE above
        grazing(m,n)%f_zoo_detr       = 0.18_r8   ! in marbl_parms framework, see NOTE above
        grazing(m,n)%grazing_function = grz_fnc_michaelis_menten   ! in marbl_parms framework, see NOTE above
        else
          grazing(m,n)%z_umax_0_per_day = c0
          grazing(m,n)%z_grz            = c0
          grazing(m,n)%graze_zoo        = c0
          grazing(m,n)%graze_poc        = c0
          grazing(m,n)%graze_doc        = c0
          grazing(m,n)%f_zoo_detr       = c0
          grazing(m,n)%grazing_function = grz_fnc_michaelis_menten
        end if
      end do
    end do

    iron_frac_in_dust      = 0.035_r8 * 0.01_r8    ! in marbl_parms framework, see NOTE above
    iron_frac_in_bc        = 0.06_r8               ! in marbl_parms framework, see NOTE above
    caco3_bury_thres_opt   = 'omega_calc'          ! in marbl_parms framework, see NOTE above
    caco3_bury_thres_depth = 3000.0e2              ! in marbl_parms framework, see NOTE above
    PON_bury_coeff         = 0.5_r8                ! in marbl_parms framework, see NOTE above
    ciso_fract_factors     = 'Rau'                 ! in marbl_parms framework, see NOTE above

    ! FIXME #69: not thread-safe!
    if (.not.allocated(tracer_restore_vars)) &
      allocate(tracer_restore_vars(marbl_total_tracer_cnt))

    ! initialize namelist variables to default values
    tracer_restore_vars = ''

  end subroutine marbl_parms_set_defaults

  !*****************************************************************************

  subroutine marbl_parms_read_namelist(nl_buffer, marbl_status_log)

    use marbl_namelist_mod, only : marbl_namelist

    character(len=*),     intent(in)    :: nl_buffer(:)
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_parms:marbl_parms_read_namelist'
    character(len=char_len)     :: log_message

    character(len=len(nl_buffer)) :: tmp_nl_buffer

    integer (int_kind)           :: n                           ! index for looping over tracers
    integer (int_kind)           :: nml_error                   ! namelist i/o error flag
    integer (int_kind)           :: zoo_ind                     ! zooplankton functional group index

    NAMELIST /marbl_parms_nml/ &
         parm_Fe_bioavail, &
         parm_o2_min, &
         parm_o2_min_delta, &
         parm_kappa_nitrif_per_day, &
         parm_nitrif_par_lim, &
         parm_labile_ratio, &
         parm_init_POC_bury_coeff, &
         parm_init_POP_bury_coeff, &
         parm_init_bSi_bury_coeff, &
         parm_Fe_scavenge_rate0, &
         parm_Lig_scavenge_rate0, &
         parm_FeLig_scavenge_rate0, &
         parm_Lig_degrade_rate0, &
         parm_Fe_desorption_rate0, &
         parm_f_prod_sp_CaCO3, &
         parm_POC_diss, &
         parm_SiO2_diss, &
         parm_CaCO3_diss, &
         parm_sed_denitrif_coeff, &
         iron_frac_in_dust, &
         iron_frac_in_bc, &
         caco3_bury_thres_opt, &
         caco3_bury_thres_depth, &
         PON_bury_coeff, &
         ciso_fract_factors, &
         tracer_restore_vars, &
         bury_coeff_rmean_timescale_years, &
         parm_scalelen_z, &
         parm_scalelen_vals, &
         autotrophs, &
         zooplankton, &
         grazing

    !---------------------------------------------------------------------------
    ! read the &marbl_parms_nml namelist
    !---------------------------------------------------------------------------
    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_parms_nml', marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('marbl_namelist', subname)
      return
    end if

    read(tmp_nl_buffer, nml=marbl_parms_nml, iostat=nml_error)
    if (nml_error /= 0) then
      write(log_message, "(A)") 'error reading &marbl_parms_nml'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

  end subroutine marbl_parms_read_namelist

  !*****************************************************************************

  subroutine marbl_define_parameters(this, marbl_status_log)

    use marbl_config_mod, only : ciso_on
    use marbl_config_mod, only : log_add_var_error
    use marbl_config_mod, only : autotrophs_config
    use marbl_config_mod, only : zooplankton_config
    use marbl_config_mod, only : grazing_config

    class(marbl_config_and_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_parms:marbl_define_parameters'
    character(len=char_len)     :: log_message

    character(len=char_len) :: sname, lname, units, datatype, group, category
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    character(len=char_len) :: prefix, comment
    integer :: m, n, cnt

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%parameters has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    this%cnt = 0
    allocate(this%vars(this%cnt))
    allocate(this%categories(0))

    !-----------------!
    ! marbl_parms_nml !
    !-----------------!

    category  = 'general parmeters'

    sname     = 'parm_Fe_bioavail'
    lname     = 'Fraction of Fe flux that is bioavailable'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_Fe_bioavail
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_o2_min'
    lname     = 'Minimum O2 needed for production and consumption'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_o2_min
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_o2_min_delta'
    lname     = 'Width of minimum O2 range'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_o2_min_delta
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_kappa_nitrif_per_day'
    lname     = 'Nitrification inverse time constant'
    units     = '1/day'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_kappa_nitrif_per_day
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_nitrif_par_lim'
    lname     = 'PAR limit for nitrification'
    units     = 'W/m^2'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_nitrif_par_lim
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_labile_ratio'
    lname     = 'Fraction of loss to DOC that is routed directly to DIC'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_labile_ratio
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_init_POC_bury_coeff'
    lname     = 'initial scale factor for burial of POC, PON'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_init_POC_bury_coeff
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_init_POP_bury_coeff'
    lname     = 'initial scale factor for burial of POP'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_init_POP_bury_coeff
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_init_bSi_bury_coeff'
    lname     = 'initial scale factor for burial of bSi'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_init_bSi_bury_coeff
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_Fe_scavenge_rate0'
    lname     = 'scavenging base rate for Fe'
    units     = '1/yr'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_Fe_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_Lig_scavenge_rate0'
    lname     = 'scavenging base rate for bound ligand'
    units     = '1/yr'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_Lig_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_FeLig_scavenge_rate0'
    lname     = 'scavenging base rate for bound iron'
    units     = '1/yr'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_FeLig_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_Lig_degrade_rate0'
    lname     = 'Fe-binding ligand bacterial degradation rate coefficient'
    units     = '1'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_Lig_degrade_rate0
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_Fe_desorption_rate0'
    lname     = 'desorption rate for scavenged Fe from particles'
    units     = '1/cm'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_Fe_desorption_rate0
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_f_prod_sp_CaCO3'
    lname     = 'Fraction of sp production as CaCO3 production'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_f_prod_sp_CaCO3
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_POC_diss'
    lname     = 'base POC dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_POC_diss
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_SiO2_diss'
    lname     = 'base SiO2 dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_SiO2_diss
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_CaCO3_diss'
    lname     = 'base CaCO3 dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_CaCO3_diss
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_sed_denitrif_coeff'
    lname     = 'global scaling factor for sed_denitrif'
    units     = '1'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_sed_denitrif_coeff
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'bury_coeff_rmean_timescale_years'
    lname     = 'Timescale for bury coefficient running means'
    units     = 'yr'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => bury_coeff_rmean_timescale_years
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    category  = 'Scale lengths'

    sname     = 'parm_scalelen_z'
    lname     = 'Depths of prescribed scale length values'
    units     = 'cm'
    group     = 'marbl_parms_nml'
    call this%add_var_1d_r8(sname, lname, units, group, category,             &
                              parm_scalelen_z, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_r8', subname)
      return
    end if

    sname     = 'parm_scalelen_vals'
    lname     = 'Prescribed scale length values'
    units     = 'cm'
    group     = 'marbl_parms_nml'
    call this%add_var_1d_r8(sname, lname, units, group, category,             &
                              parm_scalelen_vals, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_r8', subname)
      return
    end if

    do n=1,size(autotrophs)
      write(prefix, "(A,I0,A)") 'autotrophs(', n, ')%'
      write(category, "(A,1X,I0)") 'autotroph', n
      write(comment, "(2A)") 'autotroph short name = ',                       &
                             trim(autotrophs_config(n)%sname)

      write(sname, "(2A)") trim(prefix), 'kFe'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%kFe
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kPO4'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%kPO4
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kDOP'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%kDOP
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kNO3'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%kNO3
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kNH4'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%kNH4
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kSiO3'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%kSiO3
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if


!!!!added for cocco C lim
write(sname, "(2A)") trim(prefix), 'kCO2'
lname    = 'nutrient uptake half-sat constants'
units    = 'nmol/cm^3'
datatype = 'real'
group    = 'marbl_parms_nml'
rptr     => autotrophs(n)%kCO2
call this%add_var(sname, lname, units, datatype, group, category,     &
marbl_status_log, rptr=rptr, comment=comment)
if (marbl_status_log%labort_marbl) then
call log_add_var_error(marbl_status_log, sname, subname)
return
end if



      write(sname, "(2A)") trim(prefix), 'Qp_fixed'
      lname    = 'P/C ratio when using fixed P/C ratios'
      units    = 'unitless'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%Qp_fixed
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'gQfe_0'
      lname    = 'initial Fe/C ratio for growth'
      units    = 'unitless'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%gQFe_0
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'gQfe_min'
      lname    = 'minimum Fe/C ratio for growth'
      units    = 'unitless'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%gQFe_min
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'alphaPi_per_day'
      lname    = 'Initial slope of P_I curve (GD98)'
      units    = 'mmol C m^2 / (mg Chl W day)'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%alphaPi_per_day
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'PCref_per_day'
      lname    = 'max C-spec growth rate at Tref'
      units    = '1/day'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%PCref_per_day
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'thetaN_max'
      lname    = 'max thetaN (Chl/N)'
      units    = 'mg Chl / mmol N'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%thetaN_max
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres'
      lname    = 'concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%loss_thres
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres2'
      lname    = 'concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%loss_thres2
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'temp_thres'
      lname    = 'Temperature where concentration threshold and photosynthesis rate drop'
      units    = 'deg C'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%temp_thres
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'mort_per_day'
      lname    = 'linear mortality rate'
      units    = '1/day'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%mort_per_day
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'mort2_per_day'
      lname    = 'quadratic mortality rate'
      units    = '1/day/(mmol C/m^3)'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%mort2_per_day
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'agg_rate_max'
      lname    = 'Maximum agg rate'
      units    = '1/d'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%agg_rate_max
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'agg_rate_min'
      lname    = 'Minimum agg rate'
      units    = '1/d'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%agg_rate_min
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_poc'
      lname    = 'routing of loss term'
      units    = 'unitless'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => autotrophs(n)%loss_poc
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

    do n=1,size(zooplankton)
      write(prefix, "(A,I0,A)") 'zooplankton(', n, ')%'
      write(category, "(A,1X,I0)") 'zooplankton', n
      write(comment, "(2A)") 'zooplankton short name = ',                     &
                             trim(zooplankton_config(n)%sname)

      write(sname, "(2A)") trim(prefix), 'z_mort_0_per_day'
      lname    = 'Linear mortality rate'
      units    = '1/day'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => zooplankton(n)%z_mort_0_per_day
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres'
      lname    = 'Concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => zooplankton(n)%loss_thres
      call this%add_var(sname, lname, units, datatype, group, category,     &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'z_mort2_0_per_day'
      lname    = 'Quadratic mortality rate'
      units    = '1/day/(mmol C / m^3)'
      datatype = 'real'
      group    = 'marbl_parms_nml'
      rptr     => zooplankton(n)%z_mort2_0_per_day
      call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr, comment=comment)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

    do n=1,zooplankton_cnt
      do m=1,grazer_prey_cnt
        write(prefix, "(A,I0,A,I0,A)") 'grazing(', m, ',', n, ')%'
        write(category, "(A,1X,I0,1X,I0)") 'grazing', m, n

        write(sname, "(2A)") trim(prefix), 'grazing_function'
        lname    = 'functional form of grazing parmaeterization'
        units    = 'unitless'
        datatype = 'integer'
        group    = 'marbl_parms_nml'
        iptr     => grazing(m,n)%grazing_function
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, iptr=iptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'z_umax_0_per_day'
        lname    = 'max zoo growth rate at Tref'
        units    = '1/day'
        datatype = 'real'
        group    = 'marbl_parms_nml'
        rptr     => grazing(m,n)%z_umax_0_per_day
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'z_grz'
        lname    = 'Grazing coefficient'
        units    = '(mmol C/m^3)^2'
        datatype = 'real'
        group    = 'marbl_parms_nml'
        rptr     => grazing(m,n)%z_grz
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'graze_zoo'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        group    = 'marbl_parms_nml'
        rptr     => grazing(m,n)%graze_zoo
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'graze_poc'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        group    = 'marbl_parms_nml'
        rptr     => grazing(m,n)%graze_poc
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'graze_doc'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        group    = 'marbl_parms_nml'
        rptr     => grazing(m,n)%graze_doc
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'f_zoo_detr'
        lname    = 'Fraction of zoo losses to detrital'
        units    = 'unitless'
        datatype = 'real'
        group    = 'marbl_parms_nml'
        rptr     => grazing(m,n)%f_zoo_detr
        call this%add_var(sname, lname, units, datatype, group, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        cnt = grazing_config(m,n)%auto_ind_cnt
        if (cnt .gt. 0) then
          write(sname, "(2A)") trim(prefix), 'auto_ind'
          lname     = 'Indices of autotrophs in class'
          units     = 'unitless'
          group     = 'marbl_parms_nml'
          call this%add_var_1d_int(sname, lname, units, group, category,      &
                            grazing(m,n)%auto_ind(1:cnt),                     &
                            marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_status_log%log_error_trace('add_var_1d_int', subname)
            return
          end if
        end if

        cnt = grazing_config(m,n)%zoo_ind_cnt
        if (cnt .gt. 0) then
          write(sname, "(2A)") trim(prefix), 'zoo_ind'
          lname     = 'Indices of autotrophs in class'
          units     = 'unitless'
          group     = 'marbl_parms_nml'
          call this%add_var_1d_int(sname, lname, units, group, category,      &
                                   grazing(m,n)%zoo_ind(1:cnt),               &
                                   marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_status_log%log_error_trace('add_var_1d_int', subname)
            return
          end if
        end if

      end do
    end do

    category  = 'general parmeters'

    sname     = 'iron_frac_in_dust'
    lname     = 'Fraction by weight of iron in dust'
    units     = 'unitless (kg/kg)'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => iron_frac_in_dust
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'iron_frac_in_bc'
    lname     = 'Fraction by weight of iron in black carbon'
    units     = 'unitless (kg/kg)'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => iron_frac_in_bc
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'caco3_bury_thres_opt'
    lname     = 'Option for CaCO3 burial threshold'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_parms_nml'
    sptr      => caco3_bury_thres_opt
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'caco3_bury_thres_depth'
    lname     = 'Threshold depth for CaCO3 burial (if using fixed_depth option)'
    units     = 'cm'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => caco3_bury_thres_depth
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'PON_bury_coeff'
    lname     = 'scale factor for burial of PON'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => PON_bury_coeff
    call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    if (ciso_on) then
      sname     = 'ciso_fract_factors'
      lname     = 'Optiob for which biological fractionation calculation to use'
      units     = 'unitless'
      datatype  = 'string'
      group     = 'marbl_parms_nml'
      sptr      => ciso_fract_factors
      call this%add_var(sname, lname, units, datatype, group, category,       &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if
    end if

    category  = 'tracer restoring'

    sname     = 'tracer_restore_vars'
    lname     = 'Tracer names for tracers that are restored'
    units     = 'unitless'
    group     = 'marbl_parms_nml'
    call this%add_var_1d_str(sname, lname, units, group, category,            &
                               tracer_restore_vars, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_str', subname)
      return
    end if

  end subroutine marbl_define_parameters

  !*****************************************************************************

  subroutine set_derived_parms(marbl_status_log)

    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_parms:set_derived_parms'
    character(len=char_len) :: log_message

    character(len=char_len) :: sname_in, sname_out
    integer :: m, n

    call marbl_status_log%log_header('Setting derived parms', subname)

    select case (caco3_bury_thres_opt)
    case ('fixed_depth')
       caco3_bury_thres_iopt = caco3_bury_thres_iopt_fixed_depth
    case ('omega_calc')
       caco3_bury_thres_iopt = caco3_bury_thres_iopt_omega_calc
    case default
       write(log_message, "(2A)") "unknown caco3_bury_thres_opt: ", trim(caco3_bury_thres_opt)
       call marbl_status_log%log_error(log_message, subname)
       return
    end select

    parm_kappa_nitrif = dps * parm_kappa_nitrif_per_day
    call print_single_derived_parm('parm_kappa_nitrif_per_day', 'parm_kappa_nitrif', &
         parm_kappa_nitrif, subname, marbl_status_log)

    do n = 1, autotroph_cnt
       autotrophs(n)%alphaPI = dps * autotrophs(n)%alphaPI_per_day
       write(sname_in,  "(A,I0,A)") 'autotrophs(', n, ')%alphaPI_per_day'
       write(sname_out, "(A,I0,A)") 'autotrophs(', n, ')%alphaPI'
       call print_single_derived_parm(sname_in, sname_out, &
            autotrophs(n)%alphaPI, subname, marbl_status_log)

       autotrophs(n)%PCref = dps * autotrophs(n)%PCref_per_day
       write(sname_in,  "(A,I0,A)") 'autotrophs(', n, ')%PCref_per_day'
       write(sname_out, "(A,I0,A)") 'autotrophs(', n, ')%PCref'
       call print_single_derived_parm(sname_in, sname_out, &
            autotrophs(n)%PCref, subname, marbl_status_log)

       autotrophs(n)%mort = dps * autotrophs(n)%mort_per_day
       write(sname_in,  "(A,I0,A)") 'autotrophs(', n, ')%mort_per_day'
       write(sname_out, "(A,I0,A)") 'autotrophs(', n, ')%mort'
       call print_single_derived_parm(sname_in, sname_out, &
            autotrophs(n)%mort, subname, marbl_status_log)

       autotrophs(n)%mort2 = dps * autotrophs(n)%mort2_per_day
       write(sname_in,  "(A,I0,A)") 'autotrophs(', n, ')%mort2_per_day'
       write(sname_out, "(A,I0,A)") 'autotrophs(', n, ')%mort2'
       call print_single_derived_parm(sname_in, sname_out, &
            autotrophs(n)%mort2, subname, marbl_status_log)
    end do

    do n = 1, zooplankton_cnt
       zooplankton(n)%z_mort_0 = dps * zooplankton(n)%z_mort_0_per_day
       write(sname_in,  "(A,I0,A)") 'zooplankton(', n, ')%z_mort_0_per_day'
       write(sname_out, "(A,I0,A)") 'zooplankton(', n, ')%z_mort_0'
       call print_single_derived_parm(sname_in, sname_out, &
            zooplankton(n)%z_mort_0, subname, marbl_status_log)

       zooplankton(n)%z_mort2_0 = dps * zooplankton(n)%z_mort2_0_per_day
       write(sname_in,  "(A,I0,A)") 'zooplankton(', n, ')%z_mort2_0_per_day'
       write(sname_out, "(A,I0,A)") 'zooplankton(', n, ')%z_mort2_0'
       call print_single_derived_parm(sname_in, sname_out, &
            zooplankton(n)%z_mort2_0, subname, marbl_status_log)
    end do

    do n = 1, zooplankton_cnt
       do m = 1, grazer_prey_cnt
          grazing(m,n)%z_umax_0 = dps * grazing(m,n)%z_umax_0_per_day
          write(sname_in,  "(A,I0,A,I0,A)") 'grazing(', m, ',', n, ')%z_umax_0_per_day'
          write(sname_out, "(A,I0,A,I0,A)") 'grazing(', m, ',', n, ')%z_umax_0'
          call print_single_derived_parm(sname_in, sname_out, &
               grazing(m,n)%z_umax_0, subname, marbl_status_log)
       end do
    end do

  end subroutine set_derived_parms

  !*****************************************************************************

  subroutine print_single_derived_parm(sname_in, sname_out, val_out, subname, marbl_status_log)

    character(len=*), intent(in) :: sname_in
    character(len=*), intent(in) :: sname_out
    real(kind=r8),    intent(in) :: val_out
    character(len=*), intent(in) :: subname
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=char_len) :: log_message

    write(log_message, "(2A,E24.16,3A)") &
         trim(sname_out), ' = ', val_out, ' (from ', trim(sname_in), ')'
    call marbl_status_log%log_noerror(log_message, subname)

  end subroutine print_single_derived_parm

  !*****************************************************************************

end module marbl_parms
