module marbl_settings_mod

  !-----------------------------------------------------------------------------
  !   This module manages BGC-specific parameters.
  !
  !   Most of the variables are not parameters in the Fortran sense. In the
  !   the Fortran sense, they are vanilla module variables that can be set
  !   by marbl_instance%put_setting() calls from the GCM.
  !
  !   In addition to containing all the parameters, this module also handles
  !   initializing the variables to default values and then applying any
  !   changes made via put_setting().
  !
  !   This module also writes parameter values to the status log.
  !-----------------------------------------------------------------------------

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : c2
  use marbl_constants_mod, only : c1000
  use marbl_constants_mod, only : dps
  use marbl_constants_mod, only : molw_Fe

  use marbl_pft_mod, only : autotroph_settings_type
  use marbl_pft_mod, only : zooplankton_settings_type
  use marbl_pft_mod, only : grazing_relationship_settings_type

  use marbl_logging, only: marbl_log_type

  implicit none

  !-----------------------------------------------------------------------------
  !   public/private declarations
  !   all module variables are public and should have their values preserved
  !-----------------------------------------------------------------------------

  public
  save

  !---------------------------------------------------------------------------
  !  Datatypes for marbl_instance%settings
  !---------------------------------------------------------------------------

  type, private :: marbl_single_setting_ll_type
    ! Metadata
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: datatype
    integer                 :: category_ind ! used for sorting output list
    character(len=char_len) :: comment      ! used to add comment in log
    type(marbl_single_setting_ll_type), pointer :: next => NULL()
    ! Actual parameter data
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()
  end type marbl_single_setting_ll_type

  type, private :: marbl_setting_ptr
    type(marbl_single_setting_ll_type), pointer :: ptr => NULL()
  end type marbl_setting_ptr

  type, public :: marbl_settings_type
    logical, private :: init_called = .false.
    integer, private :: cnt = 0
    character(len=char_len), dimension(:), private, pointer :: categories
    type(marbl_single_setting_ll_type),    private, pointer :: vars => NULL()
    type(marbl_single_setting_ll_type),    private, pointer :: VarsFromPut => NULL()
    type(marbl_single_setting_ll_type),    private, pointer :: LastVarFromPut => NULL()
    type(marbl_setting_ptr), dimension(:), private, allocatable :: varArray
  contains
    procedure :: add_var
    procedure :: add_var_1d_r8
    procedure :: add_var_1d_int
    procedure :: add_var_1d_str
    procedure :: finalize_vars
    procedure :: inquire_id
    procedure :: inquire_metadata
    procedure :: get_cnt
    procedure :: put
    procedure :: get
    procedure :: destruct
  end type marbl_settings_type

  !---------------------------------------------------------------------
  !  BGC parameters that are currently hard-coded
  !---------------------------------------------------------------------

  ! Redfield Ratios, dissolved & particulate
  real(r8), parameter :: &
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
  real(r8), parameter :: &
       dust_Fe_scavenge_scale  = 1.0e9_r8      !dust scavenging scale factor

  ! dust_to_Fe: conversion of dust to iron (nmol Fe/g Dust)
  ! dust remin gDust = 0.035 gFe       mol Fe     1e9 nmolFe
  !                    --------- *  ----------- * ----------
  !                      gDust      molw_Fe gFe      molFe
  real(r8), parameter :: dust_to_Fe = 0.035_r8 / molw_Fe * 1.0e9_r8

  ! parameters related to Iron binding ligands
  integer (int_kind), parameter :: Lig_cnt = 1 ! valid values are 1 or 2
  real(r8), parameter :: remin_to_Lig = 0.0001_r8

  ! Partitioning of phytoplankton growth, grazing and losses
  ! All f_* variables are fractions and are non-dimensional
  real(r8), parameter :: &
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
  real(r8), parameter :: r_Nfix_photo=1.25_r8    ! N fix relative to C fix (non-dim)

  ! SET parmaeters and RATIOS for N/C, P/C, SiO3/C, Fe/C, etc...
  real(r8), parameter :: &
      Q             = 16.0_r8 / 117.0_r8, & !N/C ratio (mmol/mmol) of phyto & zoo
      Qfe_zoo       = 3.0e-6_r8,          & !zooplankton Fe/C ratio
      QCaCO3_max    = 0.4_r8,             & !max QCaCO3
      ! parameters in GalbraithMartiny Pquota Model^M
      PquotaSlope     = 7.0_r8,        &
      PquotaIntercept = 5.571_r8,      &
      PquotaMinNP     = 0.00854701_r8, &
      ! carbon:nitrogen ratio for denitrification
      denitrif_C_N  = parm_Red_D_C_P/136.0_r8

  ! loss term threshold parameters, chl:c ratios
  real(r8), parameter :: &
      thres_z1_auto     =  80.0e2_r8, & !autotroph threshold = C_loss_thres for z shallower than this (cm)
      thres_z2_auto     = 120.0e2_r8, & !autotroph threshold = 0 for z deeper than this (cm)
      thres_z1_zoo      = 110.0e2_r8, & !zooplankton threshold = C_loss_thres for z shallower than this (cm)
      thres_z2_zoo      = 150.0e2_r8, & !zooplankton threshold = 0 for z deeper than this (cm)
      CaCO3_temp_thres1 = 4.0_r8,   & !upper temp threshold for CaCO3 prod
      CaCO3_temp_thres2 = -2.0_r8,  & !lower temp threshold
      CaCO3_sp_thres    = 2.5_r8      ! bloom condition thres (mmolC/m3)

  ! fraction of incoming shortwave assumed to be PAR
  real(r8), parameter :: f_qsw_par = 0.45_r8   ! PAR fraction

  ! DOM parameters for refractory components and DOP uptake
  real(r8), parameter :: &
       DOC_reminR_light  = (c1/(365.0_r8*15.0_r8)) * dps, & ! remin rate for semi-labile DOC, 1/15yr
       DON_reminR_light  = (c1/(365.0_r8*15.0_r8)) * dps, & ! remin rate for semi-labile DON, 1/15yr
       DOP_reminR_light  = (c1/(365.0_r8*60.0_r8)) * dps, & ! remin rate for semi-labile DOP, 1/60yr
       DOC_reminR_dark   = (c1/(365.0_r8*6.0_r8)) * dps,  & ! remin rate in the dark, 1/6yr
       DON_reminR_dark   = (c1/(365.0_r8*5.5_r8)) * dps,  & ! remin rate in the dark, 1/5.5yr
       DOP_reminR_dark   = (c1/(365.0_r8*4.5_r8)) * dps     ! remin rate in the dark, 1/4.5yr

  real(r8), parameter :: &
       DOCr_reminR0      = (c1/(365.0_r8*16000.0_r8)) * dps, & ! remin rate for refractory DOC, 1/16000yr
       DONr_reminR0      = (c1/(365.0_r8*9500.0_r8)) * dps,  & ! remin rate for refractory DON, 1/9500yr
       DOPr_reminR0      = (c1/(365.0_r8*5500.0_r8)) * dps,  & ! remin rate for refractory DOP, 1/5500yr
       DOMr_reminR_photo = (c1/(365.0_r8*18.0_r8)) * dps       ! additional remin from photochemistry, 1/18yrs over top 10m

  real(r8), parameter :: &
       DOCprod_refract  = 0.01_r8,                   & ! fraction of DOCprod to refractory pool
       DONprod_refract  = 0.0115_r8,                 & ! fraction of DONprod to refractory pool
       DOPprod_refract  = 0.003_r8,                  & ! fraction of DOPprod to refractory pool
       POCremin_refract = DOCprod_refract * 0.06_r8, & ! fraction of POCremin to refractory pool
       PONremin_refract = DONprod_refract * 0.03_r8, & ! fraction of POCremin to refractory pool
       POPremin_refract = DOPprod_refract * 0.06_r8    ! fraction of POCremin to refractory pool

  ! pH parameters
  real (r8), parameter :: phlo_surf_init = 7.0_r8 ! low bound for surface ph for no prev soln
  real (r8), parameter :: phhi_surf_init = 9.0_r8 ! high bound for surface ph for no prev soln
  real (r8), parameter :: phlo_3d_init = 6.0_r8   ! low bound for subsurface ph for no prev soln
  real (r8), parameter :: phhi_3d_init = 9.0_r8   ! high bound for subsurface ph for no prev soln
  real (r8), parameter :: del_ph = 0.20_r8        ! delta-ph for prev soln

  !---------------------------------------------------------------------------------------------
  !  Variables defined in marbl_settings_define_general_parms, marbl_settings_define_PFT_counts,
  !  marbl_settings_define_PFT_derived_types, or marbl_settings_define_tracer_dependent
  !
  ! CESM NOTE: defaults values are set in the corresponding marbl_settings_set_defaults routines
  !            but may be overridden at run time through a put_setting() call (use user_nl_pop
  !            to change parameter value)
  !---------------------------------------------------------------------------------------------

  !  marbl_settings_mod_general_parms
  !    parameters with no dependencies on other parameter values
  !-------------------------------------------------------------

  character(len=char_len), target :: PFT_defaults             ! Set up PFT parameters based on known classes, e.g. 'CESM2'
                                                              ! (or set to 'user-specified' and use put_setting())
  logical(log_kind), target :: ciso_on                        ! control whether ciso tracer module is active
  logical(log_kind), target :: lsource_sink                   ! control which portion of code is executed, useful for debugging
  logical(log_kind), target :: ciso_lsource_sink              ! control which portion of carbon isotope code is executed, useful for debugging
  logical(log_kind), target :: lecovars_full_depth_tavg       ! should base ecosystem vars be written full depth
  logical(log_kind), target :: ciso_lecovars_full_depth_tavg  ! should carbon isotope vars be written full depth
  logical(log_kind), target :: lflux_gas_o2                   ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: lflux_gas_co2                  ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: lcompute_nhx_surface_emis      ! control if NHx emissions are computed
  logical(log_kind), target :: lvariable_PtoC                 ! control if PtoC ratios in autotroph_settings vary
  logical(log_kind), target :: ladjust_bury_coeff             ! control if bury coefficients are adjusted (rather than constant)
                                                              !   bury coefficients (POC_bury_coeff, POP_bury_coeff, bSi_bury_coeff)
                                                              !   reside in marbl_particulate_share_type; when ladjust_bury_coeff is
                                                              !   .true., bury coefficients are adjusted to preserve C, P, Si
                                                              !   inventories on timescales exceeding bury_coeff_rmean_timescale_years
                                                              !   (this is done primarily in spinup runs)
  logical(log_kind), target :: lo2_consumption_scalef         ! Apply o2_consumption_scalef to o2 consumption (and request it as a forcing)
  logical(log_kind), target :: lp_remin_scalef                ! Apply p_remin_scalef to particulate remin (and request it as a forcing)

  character(len=char_len), target :: init_bury_coeff_opt

  integer(int_kind), target :: &
       particulate_flux_ref_depth    ! reference depth for particulate flux diagnostics (m)

  real(r8), target :: &
       Jint_Ctot_thres_molpm2pyr,  & ! MARBL will abort if abs(Jint_Ctot) exceeds this threshold
       Jint_Ctot_thres,            & ! MARBL will abort if abs(Jint_Ctot) exceeds this threshold (derived from Jint_Ctot_thres_molpm2pyr)
       Jint_Ntot_thres,            & ! MARBL will abort if abs(Jint_Ntot) exceeds this threshold (derived from Jint_Ctot_thres)
       Jint_Ptot_thres,            & ! MARBL will abort if abs(Jint_Ptot) exceeds this threshold (derived from Jint_Ctot_thres)
       Jint_Sitot_thres,           & ! MARBL will abort if abs(Jint_Sitot) exceeds this threshold (derived from Jint_Ctot_thres)
       Jint_Fetot_thres,           & ! MARBL will abort if abs(Jint_Fetot) exceeds this threshold (derived from Jint_Ctot_thres)
       CISO_Jint_13Ctot_thres,     & ! MARBL will abort if abs(CISO_Jint_13Ctot) exceeds this threshold (derived from Jint_Ctot_thres)
       CISO_Jint_14Ctot_thres,     & ! MARBL will abort if abs(CISO_Jint_14Ctot) exceeds this threshold (derived from Jint_Ctot_thres)
       gQsi_0,                     & ! initial Si/C ratio for growth
       gQsi_max,                   & ! max Si/C ratio for growth
       gQsi_min,                   & ! min Si/C ratio for growth
       gQ_Fe_kFe_thres,            & ! Fe:kFe ratio threshold in uptake ratio computations
       gQ_Si_kSi_thres,            & ! Si:kSi ratio threshold in uptake ratio computations
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
       parm_SiO2_gamma,            & ! SiO2 gamma (fraction of production -> hard subclass)
       parm_hPOC_SiO2_ratio,       & ! hPOC to SiO2 ratio
       parm_CaCO3_diss,            & ! base CaCO3 diss len scale
       parm_CaCO3_gamma,           & ! CaCO3 gamma (fraction of production -> hard subclass)
       parm_hPOC_CaCO3_ratio,      & ! hPOC to CaCO3 ratio
       parm_hPOC_dust_ratio,       & ! hPOC to dust ratio
       o2_sf_o2_range_hi,          & ! o2_scalefactor is applied to diss length scales for O2 less than this
       o2_sf_o2_range_lo,          & ! o2_scalefactor is constant for O2 less than this
       o2_sf_val_lo_o2,            & ! o2_scalefactor constant for O2 less than o2_sf_o2_range_lo
       parm_sed_denitrif_coeff,    & ! global scaling factor for sed_denitrif
       bury_coeff_rmean_timescale_years

  real(r8), dimension(4), target :: &
       parm_scalelen_z,       & ! depths of prescribed scalelen values
       parm_scalelen_vals       ! prescribed scalelen values

  character(len=char_len), target :: caco3_bury_thres_opt         ! option of threshold of caco3 burial ['fixed_depth', 'omega_calc']
  real(r8),                target :: caco3_bury_thres_depth       ! threshold depth for caco3_bury_thres_opt='fixed_depth'
  real(r8),                target :: caco3_bury_thres_omega_calc  ! omega calcite threshold for caco3_bury_thres_opt='omega_calc'
  ! -----------
  ! PON_sed_loss = PON_bury_coeff * Q * POC_sed_loss
  ! factor is used to avoid overburying PON like POC
  ! is when total C burial is matched to C riverine input
  ! -----------
  real(r8),                target :: PON_bury_coeff
  real(r8),                target :: POM_bury_frac_max
  real(r8),                target :: bSi_bury_frac_max
  character(len=char_len), target :: ciso_fract_factors           ! option for which biological fractionation calculation to use

  !  marbl_settings_define_PFT_counts
  !    Parameters determining array size for PFT derived types
  !    (can not be set until PFT_defaults is set)
  !-------------------------------------------------------------

  integer(int_kind), target :: autotroph_cnt                  ! number of autotroph classes
  integer(int_kind), target :: zooplankton_cnt                ! number of zooplankton classes
  integer(int_kind), target :: max_grazer_prey_cnt            ! max number of biomass aggregates grazed by a zooplankton class

  !  marbl_settings_define_PFT_derived_types
  !    Parameters associated with the PFT classes
  !    (can not be set until autotroph_cnt, zooplankton_cnt
  !     are max_grazer_prey_cnt are known)
  !-------------------------------------------------------------

  type(autotroph_settings_type),            allocatable, target :: autotroph_settings(:)
  type(zooplankton_settings_type),          allocatable, target :: zooplankton_settings(:)
  type(grazing_relationship_settings_type), allocatable, target :: grazing_relationship_settings(:,:)

  !  marbl_settings_define_tracer_dependent
  !    parameters that can not be set until MARBL knows what tracers
  !    have been enabled.
  !    Currently just tracer_restore_vars (which has dimension of
  !    tracer_cnt; also, only valid values are tracer short names)
  !-----------------------------------------------------------------

  ! FIXME #69: this array is allocated in marbl_init_mod:marbl_init_tracers()
  !            and that allocation is not ideal for threaded runs
  character(len=char_len), allocatable, target, dimension(:) :: tracer_restore_vars

  !---------------------------------------------------------------------
  !  Auxiliary variables (str -> int conversions, indices, etc)
  !---------------------------------------------------------------------

  integer (int_kind)            :: caco3_bury_thres_iopt
  integer (int_kind), parameter :: caco3_bury_thres_iopt_fixed_depth = 1
  integer (int_kind), parameter :: caco3_bury_thres_iopt_omega_calc  = 2

  !*****************************************************************************

  interface print_single_derived_parm
    module procedure print_single_derived_parm_r8
    module procedure print_single_derived_parm_int
  end interface print_single_derived_parm

  ! Functions only used in this module
  private :: add_var
  private :: add_var_1d_r8
  private :: add_var_1d_int
  private :: add_var_1d_str
  private :: finalize_vars
  private :: put
  private :: get
  private :: get_cnt
  private :: inquire_id
  private :: inquire_metadata
  private :: check_and_log_add_var_error
  private :: case_insensitive_eq
  private :: print_single_derived_parm
  private :: print_single_derived_parm_r8
  private :: print_single_derived_parm_int

contains

  !*****************************************************************************

  subroutine marbl_settings_set_defaults_general_parms()

    PFT_defaults                  = 'CESM2'         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    ciso_on                       = .false.         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    lsource_sink                  = .true.          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    ciso_lsource_sink             = .false.         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    lecovars_full_depth_tavg      = .false.         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    ciso_lecovars_full_depth_tavg = .false.         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    lflux_gas_o2                  = .true.          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    lflux_gas_co2                 = .true.          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    lcompute_nhx_surface_emis     = .true.          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    lvariable_PtoC                = .true.          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    init_bury_coeff_opt           = 'settings_file' ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    ladjust_bury_coeff            = .false.         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    lo2_consumption_scalef        = .false.         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    lp_remin_scalef               = .false.         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    particulate_flux_ref_depth    = 100             ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    Jint_Ctot_thres_molpm2pyr     = 1.0e-9_r8       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    gQsi_0                        = 0.137_r8        ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    gQsi_max                      = 0.822_r8        ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    gQsi_min                      = 0.0457_r8       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    gQ_Fe_kFe_thres               = 10.0_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    gQ_Si_kSi_thres               = 6.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_Fe_bioavail              = 1.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_o2_min                   = 5.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_o2_min_delta             = 5.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_kappa_nitrif_per_day     = 0.06_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_nitrif_par_lim           = 1.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_labile_ratio             = 0.94_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_init_POC_bury_coeff      = 2.54_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_init_POP_bury_coeff      = 0.36_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_init_bSi_bury_coeff      = 1.53_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_Fe_scavenge_rate0        = 22.0_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_Lig_scavenge_rate0       = 0.015_r8        ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_FeLig_scavenge_rate0     = 1.2_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_Lig_degrade_rate0        = 0.000094_r8     ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_Fe_desorption_rate0      = 1.0e-6_r8       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_f_prod_sp_CaCO3          = 0.070_r8        ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_POC_diss                 = 100.0e2_r8      ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_SiO2_diss                = 650.0e2_r8      ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_SiO2_gamma               = 0.00_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_hPOC_SiO2_ratio          = 0.01_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_CaCO3_diss               = 500.0e2_r8      ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_CaCO3_gamma              = 0.02_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_hPOC_CaCO3_ratio         = 0.01_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_hPOC_dust_ratio          = 0.01_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    o2_sf_o2_range_hi             = 45.0_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    o2_sf_o2_range_lo             =  5.0_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    o2_sf_val_lo_o2               =  2.6_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_sed_denitrif_coeff       = 1.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    bury_coeff_rmean_timescale_years = 10.0_r8      ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_scalelen_z    = (/ 100.0e2_r8, 250.0e2_r8, 500.0e2_r8, 1000.0e2_r8 /)  ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_scalelen_vals = (/     1.0_r8,     3.6_r8,     4.7_r8,      4.8_r8 /)  ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    caco3_bury_thres_opt          = 'omega_calc'    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    caco3_bury_thres_depth        = 3000.0e2_r8     ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    caco3_bury_thres_omega_calc   = 0.89_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    PON_bury_coeff                = 0.5_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    POM_bury_frac_max             = 0.8_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    bSi_bury_frac_max             = 1.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    ciso_fract_factors            = 'Laws'          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above

  end subroutine marbl_settings_set_defaults_general_parms

  !*****************************************************************************

  subroutine marbl_settings_set_defaults_PFT_counts(marbl_status_log)

    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_set_defaults_PFT_counts'
    character(len=char_len)     :: log_message

    select case (trim(PFT_defaults))
      case ('CESM2')
        autotroph_cnt                 = 3
        zooplankton_cnt               = 1
        max_grazer_prey_cnt           = 3
      case ('user-specified')
        ! User must change these with put_setting()
        autotroph_cnt                 = -1       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
        zooplankton_cnt               = -1       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
        max_grazer_prey_cnt           = -1       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
      case DEFAULT
        write(log_message, "(3A)") "'", trim(PFT_defaults), "'' is not a valid value for PFT_defaults"
        call marbl_status_log%log_error(log_message, subname)
    end select

  end subroutine marbl_settings_set_defaults_PFT_counts

  !*****************************************************************************

  subroutine marbl_settings_set_defaults_PFT_derived_types(marbl_status_log)

    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_set_defaults_PFT_derived_types'
    character(len=char_len)     :: log_message
    integer                     :: m, n

    if (.not. all((/allocated(autotroph_settings), &
                    allocated(zooplankton_settings), &
                    allocated(grazing_relationship_settings)/))) then
      write(log_message, '(A)') 'One of {autotroph,zooplankton,grazing_relationship}_settings has not been allocated!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    select case (trim(PFT_defaults))
      case ('CESM2')
        call autotroph_settings(1)%set_to_default('sp', marbl_status_log)
        call autotroph_settings(2)%set_to_default('diat', marbl_status_log)
        call autotroph_settings(3)%set_to_default('diaz', marbl_status_log)
        call zooplankton_settings(1)%set_to_default('zoo', marbl_status_log)
        call grazing_relationship_settings(1,1)%set_to_default('sp_zoo', marbl_status_log)
        call grazing_relationship_settings(2,1)%set_to_default('diat_zoo', marbl_status_log)
        call grazing_relationship_settings(3,1)%set_to_default('diaz_zoo', marbl_status_log)
      case ('user-specified')
        do m=1,autotroph_cnt
          call autotroph_settings(m)%set_to_default('unset', marbl_status_log)
        end do
        do n=1,zooplankton_cnt
          call zooplankton_settings(n)%set_to_default('unset', marbl_status_log)
        end do
        do n=1,zooplankton_cnt
          do m=1,max_grazer_prey_cnt
            call grazing_relationship_settings(m,n)%set_to_default('unset', marbl_status_log)
          end do
        end do
      case DEFAULT
        write(log_message, "(3A)") "'", trim(PFT_defaults), "' is not a valid value for PFT_defaults"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('PFT set_to_default()', subname)
      return
    end if

  end subroutine marbl_settings_set_defaults_PFT_derived_types

  !*****************************************************************************

  subroutine marbl_settings_set_defaults_tracer_dependent(marbl_status_log)

    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_set_defaults_tracer_dependent'

    if (.not. allocated(tracer_restore_vars)) then
      call marbl_status_log%log_error('tracer_restore_vars has not been allocated!', subname)
      return
    end if

    ! initialize namelist variables to default values
    tracer_restore_vars = ''                     ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above

  end subroutine marbl_settings_set_defaults_tracer_dependent

  !*****************************************************************************

  subroutine marbl_settings_define_general_parms(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_define_general_parms'
    character(len=char_len)     :: log_message

    character(len=char_len)          :: sname, lname, units, datatype, category
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()
    logical                          :: labort_marbl_loc

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%settings has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if
    allocate(this%categories(0))
    labort_marbl_loc = .false.

    ! ----------------------
    category = 'config PFTs'
    ! ----------------------

    sname     = 'PFT_defaults'
    lname     = 'Define how PFTs are initialized'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => PFT_defaults
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, sptr=sptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    ! -----------------------
    category = 'config flags'
    ! -----------------------

    sname     = 'ciso_on'
    lname     = 'Control whether CISO tracer module is active'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => ciso_on
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'lsource_sink'
    lname     = 'Control which portions of code are executed (useful for debugging)'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lsource_sink
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'lecovars_full_depth_tavg'
    lname     = 'Are base ecosystem tracers full depth?'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lecovars_full_depth_tavg
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'ciso_lsource_sink'
    lname     = 'Control which portions of carbon isotope code are executed (useful for debugging)'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => ciso_lsource_sink
    call this%add_var(sname, lname, units, datatype, category,       &
                      marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'ciso_lecovars_full_depth_tavg'
    lname     = 'Are carbon isotope tracers full depth?'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => ciso_lecovars_full_depth_tavg
    call this%add_var(sname, lname, units, datatype, category,       &
                      marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'lflux_gas_o2'
    lname     = 'Run O2 gas flux portion of the code'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lflux_gas_o2
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'lflux_gas_co2'
    lname     = 'Run CO2 gas flux portion of the code'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lflux_gas_co2
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'lcompute_nhx_surface_emis'
    lname     = 'control if NHx emissions are computed'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lcompute_nhx_surface_emis
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'lvariable_PtoC'
    lname     = 'control if PtoC ratios in autotrophs vary'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lvariable_PtoC
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'ladjust_bury_coeff'
    lname     = 'Adjust the bury coefficient to maintain equilibrium'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => ladjust_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'lo2_consumption_scalef'
    lname     = 'Apply o2_consumption_scalef to o2 consumption (and request it as a forcing)'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lo2_consumption_scalef
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'lp_remin_scalef'
    lname     = 'Apply p_remin_scalef to particulate remin (and request it as a forcing)'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lp_remin_scalef
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    ! --------------------------
    category  = 'config strings'
    ! --------------------------

    sname     = 'init_bury_coeff_opt'
    lname     = 'How to set initial bury coefficients'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => init_bury_coeff_opt
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, sptr=sptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    ! -----------------------------
    category  = 'general parmeters'
    ! -----------------------------

    sname     = 'particulate_flux_ref_depth'
    lname     = 'reference depth for particulate flux diagnostics'
    units     = 'm'
    datatype  = 'integer'
    iptr      => particulate_flux_ref_depth
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, iptr=iptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'Jint_Ctot_thres_molpm2pyr'
    lname     = 'MARBL will abort if abs(Jint_Ctot) exceeds this threshold'
    units     = 'mol m-2 yr-1'
    datatype  = 'real'
    rptr      => Jint_Ctot_thres_molpm2pyr
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'gQsi_0'
    lname     = 'initial Si/C ratio for growth'
    units     = '1'
    datatype  = 'real'
    rptr      => gQsi_0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'gQsi_max'
    lname     = 'max Si/C ratio for growth'
    units     = '1'
    datatype  = 'real'
    rptr      => gQsi_max
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'gQsi_min'
    lname     = 'min Si/C ratio for growth'
    units     = '1'
    datatype  = 'real'
    rptr      => gQsi_min
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'gQ_Fe_kFe_thres'
    lname     = 'Fe:kFe ratio threshold in uptake ratio computations'
    units     = '1'
    datatype  = 'real'
    rptr      => gQ_Fe_kFe_thres
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'gQ_Si_kSi_thres'
    lname     = 'Si:kSi ratio threshold in uptake ratio computations'
    units     = '1'
    datatype  = 'real'
    rptr      => gQ_Si_kSi_thres
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_Fe_bioavail'
    lname     = 'Fraction of Fe flux that is bioavailable'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_Fe_bioavail
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_o2_min'
    lname     = 'Minimum O2 needed for production and consumption'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    rptr      => parm_o2_min
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_o2_min_delta'
    lname     = 'Width of minimum O2 range'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    rptr      => parm_o2_min_delta
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_kappa_nitrif_per_day'
    lname     = 'Nitrification inverse time constant'
    units     = '1/day'
    datatype  = 'real'
    rptr      => parm_kappa_nitrif_per_day
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_nitrif_par_lim'
    lname     = 'PAR limit for nitrification'
    units     = 'W/m^2'
    datatype  = 'real'
    rptr      => parm_nitrif_par_lim
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_labile_ratio'
    lname     = 'Fraction of loss to DOC that is routed directly to DIC'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_labile_ratio
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_init_POC_bury_coeff'
    lname     = 'initial scale factor for burial of POC, PON'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_init_POC_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_init_POP_bury_coeff'
    lname     = 'initial scale factor for burial of POP'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_init_POP_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_init_bSi_bury_coeff'
    lname     = 'initial scale factor for burial of bSi'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_init_bSi_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_Fe_scavenge_rate0'
    lname     = 'scavenging base rate for Fe'
    units     = '1/yr'
    datatype  = 'real'
    rptr      => parm_Fe_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_Lig_scavenge_rate0'
    lname     = 'scavenging base rate for bound ligand'
    units     = '1/yr'
    datatype  = 'real'
    rptr      => parm_Lig_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_FeLig_scavenge_rate0'
    lname     = 'scavenging base rate for bound iron'
    units     = '1/yr'
    datatype  = 'real'
    rptr      => parm_FeLig_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_Lig_degrade_rate0'
    lname     = 'Fe-binding ligand bacterial degradation rate coefficient'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_Lig_degrade_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_Fe_desorption_rate0'
    lname     = 'desorption rate for scavenged Fe from particles'
    units     = '1/cm'
    datatype  = 'real'
    rptr      => parm_Fe_desorption_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_f_prod_sp_CaCO3'
    lname     = 'Fraction of sp production as CaCO3 production'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_f_prod_sp_CaCO3
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_POC_diss'
    lname     = 'base POC dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    rptr      => parm_POC_diss
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_SiO2_diss'
    lname     = 'base SiO2 dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    rptr      => parm_SiO2_diss
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_SiO2_gamma'
    lname     = 'SiO2 gamma (fraction of production -> hard subclass)'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_SiO2_gamma
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_hPOC_SiO2_ratio'
    lname     = 'hPOC to SiO2 ratio'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_hPOC_SiO2_ratio
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_CaCO3_diss'
    lname     = 'base CaCO3 dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    rptr      => parm_CaCO3_diss
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_CaCO3_gamma'
    lname     = 'CaCO3 gamma (fraction of production -> hard subclass)'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_CaCO3_gamma
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_hPOC_CaCO3_ratio'
    lname     = 'hPOC to CaCO3 ratio'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_hPOC_CaCO3_ratio
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_hPOC_dust_ratio'
    lname     = 'hPOC to dust ratio'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_hPOC_dust_ratio
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'o2_sf_o2_range_hi'
    lname     = 'o2_scalefactor is applied to diss length scales for O2 less than this'
    units     = 'mmol/m^3'
    datatype  = 'real'
    rptr      => o2_sf_o2_range_hi
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'o2_sf_o2_range_lo'
    lname     = 'o2_scalefactor is constant for O2 less than this'
    units     = 'mmol/m^3'
    datatype  = 'real'
    rptr      => o2_sf_o2_range_lo
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'o2_sf_val_lo_o2'
    lname     = 'o2_scalefactor constant for O2 less than o2_sf_o2_range_lo'
    units     = '1'
    datatype  = 'real'
    rptr      => o2_sf_val_lo_o2
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_sed_denitrif_coeff'
    lname     = 'global scaling factor for sed_denitrif'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_sed_denitrif_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'bury_coeff_rmean_timescale_years'
    lname     = 'Timescale for bury coefficient running means'
    units     = 'yr'
    datatype  = 'real'
    rptr      => bury_coeff_rmean_timescale_years
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    ! -------------------------
    category  = 'Scale lengths'
    ! -------------------------

    sname     = 'parm_scalelen_z'
    lname     = 'Depths of prescribed scale length values'
    units     = 'cm'
    call this%add_var_1d_r8(sname, lname, units, category,             &
                              parm_scalelen_z, marbl_status_log)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'parm_scalelen_vals'
    lname     = 'Prescribed scale length values'
    units     = 'cm'
    call this%add_var_1d_r8(sname, lname, units, category,             &
                              parm_scalelen_vals, marbl_status_log)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    ! -----------------------------
    category  = 'general parmeters'
    ! -----------------------------

    sname     = 'caco3_bury_thres_opt'
    lname     = 'Option for CaCO3 burial threshold'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => caco3_bury_thres_opt
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, sptr=sptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'caco3_bury_thres_depth'
    lname     = 'Threshold depth for CaCO3 burial (if using fixed_depth option)'
    units     = 'cm'
    datatype  = 'real'
    rptr      => caco3_bury_thres_depth
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'caco3_bury_thres_omega_calc'
    lname     = 'omega calcite threshold for CaCO3 burial (if using omega_calc option)'
    units     = '1'
    datatype  = 'real'
    rptr      => caco3_bury_thres_omega_calc
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'PON_bury_coeff'
    lname     = 'scale factor for burial of PON'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => PON_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'POM_bury_frac_max'
    lname     = 'maximum bury fraction for POM'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => POM_bury_frac_max
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'bSi_bury_frac_max'
    lname     = 'maximum bury fraction for bSi'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => bSi_bury_frac_max
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'ciso_fract_factors'
    lname     = 'Option for which biological fractionation calculation to use'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => ciso_fract_factors
    call this%add_var(sname, lname, units, datatype, category,       &
                      marbl_status_log, sptr=sptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    marbl_status_log%labort_marbl = labort_marbl_loc
    if (marbl_status_log%labort_marbl) return

  end subroutine marbl_settings_define_general_parms

  !*****************************************************************************

  subroutine marbl_settings_define_PFT_counts(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_define_PFT_counts'
    character(len=char_len)     :: log_message

    character(len=char_len)    :: sname, lname, units, datatype, category
    integer(int_kind), pointer :: iptr => NULL()
    integer                    :: m,n
    logical                    :: labort_marbl_loc

    labort_marbl_loc = .false.

    ! ----------------------
    category = 'config PFTs'
    ! ----------------------

    sname     = 'autotroph_cnt'
    lname     = 'Number of autotroph classes'
    units     = 'unitless'
    datatype  = 'integer'
    iptr      => autotroph_cnt
    call this%add_var(sname, lname, units, datatype, category,                   &
                        marbl_status_log, iptr=iptr,                             &
                        nondefault_allowed=(PFT_defaults .eq. "user-specified"), &
                        nondefault_required=(PFT_defaults .eq. "user-specified"))
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'zooplankton_cnt'
    lname     = 'Number of zooplankton classes'
    units     = 'unitless'
    datatype  = 'integer'
    iptr      => zooplankton_cnt
    call this%add_var(sname, lname, units, datatype, category,                   &
                        marbl_status_log, iptr=iptr,                             &
                        nondefault_allowed=(PFT_defaults .eq. "user-specified"), &
                        nondefault_required=(PFT_defaults .eq. "user-specified"))

    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    sname     = 'max_grazer_prey_cnt'
    lname     = 'Number of grazer prey classes'
    units     = 'unitless'
    datatype  = 'integer'
    iptr      => max_grazer_prey_cnt
    call this%add_var(sname, lname, units, datatype, category,                   &
                        marbl_status_log, iptr=iptr,                             &
                        nondefault_allowed=(PFT_defaults .eq. "user-specified"), &
                        nondefault_required=(PFT_defaults .eq. "user-specified"))
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    marbl_status_log%labort_marbl = labort_marbl_loc
    if (marbl_status_log%labort_marbl) return

    ! FIXME #69: this is not ideal for threaded runs
    if (.not. allocated(autotroph_settings)) &
      allocate(autotroph_settings(autotroph_cnt))
    if (.not. allocated(zooplankton_settings)) &
      allocate(zooplankton_settings(zooplankton_cnt))
    if (.not. allocated(grazing_relationship_settings)) then
      allocate(grazing_relationship_settings(max_grazer_prey_cnt, zooplankton_cnt))
      do n=1,zooplankton_cnt
        do m=1,max_grazer_prey_cnt
          call grazing_relationship_settings(m,n)%construct(autotroph_cnt, zooplankton_cnt, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            write(log_message,"(A,I0,A,I0,A)") 'grazing_relationship_settings(', m, ',', n, ')%construct'
            call marbl_status_log%log_error_trace(log_message, subname)
            return
          end if
        end do
      end do
    end if

  end subroutine marbl_settings_define_PFT_counts

  !*****************************************************************************

  subroutine marbl_settings_define_PFT_derived_types(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_define_PFT_derived_types'

    character(len=char_len)          :: sname, lname, units, datatype, category
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    logical :: labort_marbl_loc
    integer :: m, n, cnt
    character(len=char_len) :: prefix

    labort_marbl_loc = .false.
    do n=1,autotroph_cnt
      write(prefix, "(A,I0,A)") 'autotroph_settings(', n, ')%'
      write(category, "(A,1X,I0)") 'autotroph', n

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      sptr     => autotroph_settings(n)%sname
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, sptr=sptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      sptr     => autotroph_settings(n)%lname
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, sptr=sptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'Nfixer'
      lname    = 'Flag is true if this autotroph fixes N2'
      units    = 'unitless'
      datatype = 'logical'
      lptr     => autotroph_settings(n)%Nfixer
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, lptr=lptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'imp_calcifier'
      lname    = 'Flag is true if this autotroph implicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      lptr     => autotroph_settings(n)%imp_calcifier
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, lptr=lptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'exp_calcifier'
      lname    = 'Flag is true if this autotroph explicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      lptr     => autotroph_settings(n)%exp_calcifier
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, lptr=lptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'silicifier'
      lname    = 'Flag is true if this autotroph is a silicifier'
      units    = 'unitless'
      datatype = 'logical'
      lptr     => autotroph_settings(n)%silicifier
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, lptr=lptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'kFe'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotroph_settings(n)%kFe
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'kPO4'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotroph_settings(n)%kPO4
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'kDOP'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotroph_settings(n)%kDOP
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'kNO3'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotroph_settings(n)%kNO3
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'kNH4'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotroph_settings(n)%kNH4
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'kSiO3'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotroph_settings(n)%kSiO3
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'Qp_fixed'
      lname    = 'P/C ratio when using fixed P/C ratios'
      units    = 'unitless'
      datatype = 'real'
      rptr     => autotroph_settings(n)%Qp_fixed
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'gQfe_0'
      lname    = 'initial Fe/C ratio for growth'
      units    = 'unitless'
      datatype = 'real'
      rptr     => autotroph_settings(n)%gQFe_0
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'gQfe_min'
      lname    = 'minimum Fe/C ratio for growth'
      units    = 'unitless'
      datatype = 'real'
      rptr     => autotroph_settings(n)%gQFe_min
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'alphaPi_per_day'
      lname    = 'Initial slope of P_I curve (GD98)'
      units    = 'mmol m^2 / (mg Chl W day)'
      datatype = 'real'
      rptr     => autotroph_settings(n)%alphaPi_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'PCref_per_day'
      lname    = 'max C-spec growth rate at Tref'
      units    = '1/day'
      datatype = 'real'
      rptr     => autotroph_settings(n)%PCref_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'thetaN_max'
      lname    = 'max thetaN (Chl/N)'
      units    = 'mg Chl / mmol'
      datatype = 'real'
      rptr     => autotroph_settings(n)%thetaN_max
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'loss_thres'
      lname    = 'concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotroph_settings(n)%loss_thres
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'loss_thres2'
      lname    = 'concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotroph_settings(n)%loss_thres2
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'temp_thres'
      lname    = 'Temperature where concentration threshold and photosynthesis rate drop'
      units    = 'degC'
      datatype = 'real'
      rptr     => autotroph_settings(n)%temp_thres
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'mort_per_day'
      lname    = 'linear mortality rate'
      units    = '1/day'
      datatype = 'real'
      rptr     => autotroph_settings(n)%mort_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'mort2_per_day'
      lname    = 'quadratic mortality rate'
      units    = '1/day/(mmol/m^3)'
      datatype = 'real'
      rptr     => autotroph_settings(n)%mort2_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'agg_rate_max'
      lname    = 'Maximum agg rate'
      units    = '1/d'
      datatype = 'real'
      rptr     => autotroph_settings(n)%agg_rate_max
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'agg_rate_min'
      lname    = 'Minimum agg rate'
      units    = '1/d'
      datatype = 'real'
      rptr     => autotroph_settings(n)%agg_rate_min
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'loss_poc'
      lname    = 'routing of loss term'
      units    = 'unitless'
      datatype = 'real'
      rptr     => autotroph_settings(n)%loss_poc
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    end do

    do n=1, zooplankton_cnt
      write(prefix, "(A,I0,A)") 'zooplankton_settings(', n, ')%'
      write(category, "(A,1X,I0)") 'zooplankton_settings', n

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      sptr     => zooplankton_settings(n)%sname
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, sptr=sptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      sptr     => zooplankton_settings(n)%lname
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, sptr=sptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'z_mort_0_per_day'
      lname    = 'Linear mortality rate'
      units    = '1/day'
      datatype = 'real'
      rptr     => zooplankton_settings(n)%z_mort_0_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'loss_thres'
      lname    = 'Concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => zooplankton_settings(n)%loss_thres
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

      write(sname, "(2A)") trim(prefix), 'z_mort2_0_per_day'
      lname    = 'Quadratic mortality rate'
      units    = '1/day/(mmol/m^3)'
      datatype = 'real'
      rptr     => zooplankton_settings(n)%z_mort2_0_per_day
      call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr,                 &
                        nondefault_required=(PFT_defaults .eq. 'user-specified'))
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    end do

    do n=1,zooplankton_cnt
      do m=1,max_grazer_prey_cnt
        write(prefix, "(A,I0,A,I0,A)") 'grazing_relationship_settings(', m, ',', n, ')%'
        write(category, "(A,1X,I0,1X,I0)") 'grazing_relationship_settings', m, n

        write(sname, "(2A)") trim(prefix), 'sname'
        lname    = 'Short name of grazing relationship'
        units    = 'unitless'
        datatype = 'string'
        sptr     => grazing_relationship_settings(m,n)%sname
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, sptr=sptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'lname'
        lname    = 'Long name of grazing relationship'
        units    = 'unitless'
        datatype = 'string'
        sptr     => grazing_relationship_settings(m,n)%lname
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, sptr=sptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'auto_ind_cnt'
        lname    = 'number of autotrophs in prey-class auto_ind'
        units    = 'unitless'
        datatype = 'integer'
        iptr     => grazing_relationship_settings(m,n)%auto_ind_cnt
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, iptr=iptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'zoo_ind_cnt'
        lname    = 'number of zooplankton in prey-class auto_ind'
        units    = 'unitless'
        datatype = 'integer'
        iptr     => grazing_relationship_settings(m,n)%zoo_ind_cnt
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, iptr=iptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'grazing_function'
        lname    = 'functional form of grazing parmaeterization'
        units    = 'unitless'
        datatype = 'integer'
        iptr     => grazing_relationship_settings(m,n)%grazing_function
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, iptr=iptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'z_umax_0_per_day'
        lname    = 'max zoo growth rate at Tref'
        units    = '1/day'
        datatype = 'real'
        rptr     => grazing_relationship_settings(m,n)%z_umax_0_per_day
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'z_grz'
        lname    = 'Grazing coefficient'
        units    = '(mmol/m^3)^2'
        datatype = 'real'
        rptr     => grazing_relationship_settings(m,n)%z_grz
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'graze_zoo'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        rptr     => grazing_relationship_settings(m,n)%graze_zoo
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'graze_poc'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        rptr     => grazing_relationship_settings(m,n)%graze_poc
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'graze_doc'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        rptr     => grazing_relationship_settings(m,n)%graze_doc
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        write(sname, "(2A)") trim(prefix), 'f_zoo_detr'
        lname    = 'Fraction of zoo losses to detrital'
        units    = 'unitless'
        datatype = 'real'
        rptr     => grazing_relationship_settings(m,n)%f_zoo_detr
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr,                 &
                          nondefault_required=(PFT_defaults .eq. 'user-specified'))
        call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

        cnt = grazing_relationship_settings(m,n)%auto_ind_cnt
        if (cnt .gt. 0) then
          write(sname, "(2A)") trim(prefix), 'auto_ind'
          lname     = 'Indices of autotrophs in class'
          units     = 'unitless'
          call this%add_var_1d_int(sname, lname, units, category,      &
                            grazing_relationship_settings(m,n)%auto_ind(1:cnt),      &
                            marbl_status_log,                          &
                            nondefault_required=(PFT_defaults .eq. 'user-specified'))
          call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)
        end if

        cnt = grazing_relationship_settings(m,n)%zoo_ind_cnt
        if (cnt .gt. 0) then
          write(sname, "(2A)") trim(prefix), 'zoo_ind'
          lname     = 'Indices of autotrophs in class'
          units     = 'unitless'
          call this%add_var_1d_int(sname, lname, units, category,       &
                                   grazing_relationship_settings(m,n)%zoo_ind(1:cnt), &
                                   marbl_status_log,                    &
                                   nondefault_required=(PFT_defaults .eq. 'user-specified'))
          call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)
        end if

      end do
    end do

    marbl_status_log%labort_marbl = labort_marbl_loc
    if (marbl_status_log%labort_marbl) return

  end subroutine marbl_settings_define_PFT_derived_types

  !*****************************************************************************

  subroutine marbl_settings_define_tracer_dependent(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_define_tracer_dependent'
    character(len=char_len) :: sname, lname, units, category
    logical                 :: labort_marbl_loc

    labort_marbl_loc = .false.
    ! ----------------------------
    category  = 'tracer restoring'
    ! ----------------------------

    sname     = 'tracer_restore_vars'
    lname     = 'Tracer names for tracers that are restored'
    units     = 'unitless'
    call this%add_var_1d_str(sname, lname, units, category,            &
                               tracer_restore_vars, marbl_status_log)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    marbl_status_log%labort_marbl = labort_marbl_loc
    if (marbl_status_log%labort_marbl) return

  end subroutine marbl_settings_define_tracer_dependent

  !*****************************************************************************

  subroutine marbl_settings_set_all_derived(marbl_status_log)

    use marbl_constants_mod, only : mpercm, yps
    use marbl_constants_mod, only : R13C_std, R14C_std

    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_set_all_derived'
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
    call print_single_derived_parm('caco3_bury_thres_opt', 'caco3_bury_thres_iopt', &
         caco3_bury_thres_iopt, subname, marbl_status_log)

    parm_kappa_nitrif = dps * parm_kappa_nitrif_per_day
    call print_single_derived_parm('parm_kappa_nitrif_per_day', 'parm_kappa_nitrif', &
         parm_kappa_nitrif, subname, marbl_status_log)

    Jint_Ctot_thres = 1.0e9_r8 * mpercm**2 * yps * Jint_Ctot_thres_molpm2pyr
    call print_single_derived_parm('Jint_Ctot_thres_molpm2pyr', 'Jint_Ctot_thres', &
         Jint_Ctot_thres, subname, marbl_status_log)

    Jint_Ntot_thres = Q * Jint_Ctot_thres
    call print_single_derived_parm('Jint_Ctot_thres', 'Jint_Ntot_thres', &
         Jint_Ntot_thres, subname, marbl_status_log)

    Jint_Ptot_thres = (c1/parm_Red_D_C_P) * Jint_Ctot_thres
    call print_single_derived_parm('Jint_Ctot_thres', 'Jint_Ptot_thres', &
         Jint_Ptot_thres, subname, marbl_status_log)

    Jint_Sitot_thres = gQsi_0 * Jint_Ctot_thres
    call print_single_derived_parm('Jint_Ctot_thres', 'Jint_Sitot_thres', &
         Jint_Sitot_thres, subname, marbl_status_log)

    Jint_Fetot_thres = parm_Red_Fe_C * Jint_Ctot_thres
    call print_single_derived_parm('Jint_Ctot_thres', 'Jint_Fetot_thres', &
         Jint_Fetot_thres, subname, marbl_status_log)

    CISO_Jint_13Ctot_thres = R13C_std * Jint_Ctot_thres
    call print_single_derived_parm('Jint_Ctot_thres', 'CISO_Jint_13Ctot_thres', &
         CISO_Jint_13Ctot_thres, subname, marbl_status_log)

    CISO_Jint_14Ctot_thres = R14C_std * Jint_Ctot_thres
    call print_single_derived_parm('Jint_Ctot_thres', 'CISO_Jint_14Ctot_thres', &
         CISO_Jint_14Ctot_thres, subname, marbl_status_log)

    call marbl_status_log%log_noerror('', subname)

    do n = 1, autotroph_cnt
       autotroph_settings(n)%alphaPI = dps * autotroph_settings(n)%alphaPI_per_day
       write(sname_in,  "(A,I0,A)") 'autotroph_settings(', n, ')%alphaPI_per_day'
       write(sname_out, "(A,I0,A)") 'autotroph_settings(', n, ')%alphaPI'
       call print_single_derived_parm(sname_in, sname_out, &
            autotroph_settings(n)%alphaPI, subname, marbl_status_log)

       autotroph_settings(n)%PCref = dps * autotroph_settings(n)%PCref_per_day
       write(sname_in,  "(A,I0,A)") 'autotroph_settings(', n, ')%PCref_per_day'
       write(sname_out, "(A,I0,A)") 'autotroph_settings(', n, ')%PCref'
       call print_single_derived_parm(sname_in, sname_out, &
            autotroph_settings(n)%PCref, subname, marbl_status_log)

       autotroph_settings(n)%mort = dps * autotroph_settings(n)%mort_per_day
       write(sname_in,  "(A,I0,A)") 'autotroph_settings(', n, ')%mort_per_day'
       write(sname_out, "(A,I0,A)") 'autotroph_settings(', n, ')%mort'
       call print_single_derived_parm(sname_in, sname_out, &
            autotroph_settings(n)%mort, subname, marbl_status_log)

       autotroph_settings(n)%mort2 = dps * autotroph_settings(n)%mort2_per_day
       write(sname_in,  "(A,I0,A)") 'autotroph_settings(', n, ')%mort2_per_day'
       write(sname_out, "(A,I0,A)") 'autotroph_settings(', n, ')%mort2'
       call print_single_derived_parm(sname_in, sname_out, &
            autotroph_settings(n)%mort2, subname, marbl_status_log)
    end do

    call marbl_status_log%log_noerror('', subname)

    do n = 1, zooplankton_cnt
       zooplankton_settings(n)%z_mort_0 = dps * zooplankton_settings(n)%z_mort_0_per_day
       write(sname_in,  "(A,I0,A)") 'zooplankton_settings(', n, ')%z_mort_0_per_day'
       write(sname_out, "(A,I0,A)") 'zooplankton_settings(', n, ')%z_mort_0'
       call print_single_derived_parm(sname_in, sname_out, &
            zooplankton_settings(n)%z_mort_0, subname, marbl_status_log)

       zooplankton_settings(n)%z_mort2_0 = dps * zooplankton_settings(n)%z_mort2_0_per_day
       write(sname_in,  "(A,I0,A)") 'zooplankton_settings(', n, ')%z_mort2_0_per_day'
       write(sname_out, "(A,I0,A)") 'zooplankton_settings(', n, ')%z_mort2_0'
       call print_single_derived_parm(sname_in, sname_out, &
            zooplankton_settings(n)%z_mort2_0, subname, marbl_status_log)
    end do

    call marbl_status_log%log_noerror('', subname)

    do n = 1, zooplankton_cnt
       do m = 1, max_grazer_prey_cnt
          grazing_relationship_settings(m,n)%z_umax_0 = dps * grazing_relationship_settings(m,n)%z_umax_0_per_day
          write(sname_in,  "(A,I0,A,I0,A)") 'grazing_relationship_settings(', m, ',', n, ')%z_umax_0_per_day'
          write(sname_out, "(A,I0,A,I0,A)") 'grazing_relationship_settings(', m, ',', n, ')%z_umax_0'
          call print_single_derived_parm(sname_in, sname_out, &
               grazing_relationship_settings(m,n)%z_umax_0, subname, marbl_status_log)
       end do
    end do

  end subroutine marbl_settings_set_all_derived

  !*****************************************************************************

  subroutine marbl_settings_string_to_var(value, marbl_status_log, rval, ival, lval, sval)

    character(len=*),            intent(in)    :: value
    type(marbl_log_type),        intent(inout) :: marbl_status_log
    real(r8),          optional, intent(out)   :: rval
    integer(int_kind), optional, intent(out)   :: ival
    logical(log_kind), optional, intent(out)   :: lval
    character(len=*),  optional, intent(out)   :: sval

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_string_to_var'
    character(len_trim(value))  :: val_loc
    character(len=char_len)     :: log_message
    integer :: ioerr, last_char

    val_loc = adjustl(trim(value))

    ! Real value requested?
    if (present(rval)) then
      read(value, *, iostat=ioerr) rval
      if (ioerr .ne. 0) then
        write(log_message, "(2A)") trim(value), ' is not a valid real value'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    end if

    ! Integer value requested?
    if (present(ival)) then
      read(value, *, iostat=ioerr) ival
      if (ioerr .ne. 0) then
        write(log_message, "(2A)") trim(value), ' is not a valid integer value'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    end if

    ! Logical value requested?
    if (present(lval)) then
      read(value, *, iostat=ioerr) lval
      if (ioerr .ne. 0) then
        write(log_message, "(2A)") trim(value), ' is not a valid logical value'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    end if

    ! String value requested?
    if (present(sval)) then
      ! Error checking:
      ! (a) empty string not allowed
      ! (b) first character must be ' or "
      ! (c) first and last character must match
      last_char = len_trim(val_loc)
      if (last_char .eq. 0) then
        log_message = "Empty string is not acceptable"
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      if ((val_loc(1:1) .ne. '"') .and. (val_loc(1:1) .ne. "'")) then
        write(log_message,"(3A)") "String value must be in quotes ", &
              trim(val_loc), " is not acceptable"
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      if (val_loc(1:1) .ne. val_loc(last_char:last_char)) then
        write(log_message,"(3A)") "String value must be in quotes ", &
              trim(val_loc), " is not acceptable"
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
      sval = val_loc(2:last_char-1)

    end if

  end subroutine marbl_settings_string_to_var

!*****************************************************************************

  subroutine marbl_settings_consistency_check(lallow_glo_ops, marbl_status_log)

    logical,              intent(in)    :: lallow_glo_ops
    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_consistency_check'
    character(len=char_len) :: log_message

    !  Abort if GCM doesn't support global ops but configuration requires them
    if (ladjust_bury_coeff .and. (.not.lallow_glo_ops)) then
      write(log_message,'(2A)') 'Can not run with ladjust_bury_coeff = ',     &
             '.true. unless GCM can perform global operations'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

  end subroutine marbl_settings_consistency_check

!*****************************************************************************

  subroutine add_var(this, sname, lname, units, datatype, category,    &
                     marbl_status_log, rptr, iptr, lptr, sptr,         &
                     nondefault_allowed, nondefault_required, comment)

    class(marbl_settings_type),                 intent(inout) :: this
    character(len=*),                           intent(in)    :: sname
    character(len=*),                           intent(in)    :: lname
    character(len=*),                           intent(in)    :: units
    character(len=*),                           intent(in)    :: datatype
    character(len=*),                           intent(in)    :: category
    type(marbl_log_type),                       intent(inout) :: marbl_status_log
    real(r8),                optional, pointer, intent(in)    :: rptr
    integer,                 optional, pointer, intent(in)    :: iptr
    logical,                 optional, pointer, intent(in)    :: lptr
    character(len=char_len), optional, pointer, intent(in)    :: sptr
    logical,                 optional,          intent(in)    :: nondefault_allowed
    logical,                 optional,          intent(in)    :: nondefault_required
    character(len=char_len), optional,          intent(in)    :: comment

    character(len=*), parameter :: subname = 'marbl_settings_mod:add_var'

    type(marbl_single_setting_ll_type), pointer :: new_entry, ll_ptr, ll_prev
    character(len=char_len), dimension(:), pointer :: new_categories
    integer :: cat_ind
    character(len=char_len) :: log_message, alternate_sname, tmp_sval
    logical :: put_success, datatype_match, nondefault_val
    logical :: allow_nondefault, require_nondefault, put_called

    if (present(nondefault_allowed)) then
      allow_nondefault = nondefault_allowed
    else
      allow_nondefault = .true.
    end if

    if (present(nondefault_required)) then
      require_nondefault = nondefault_required
    else
      require_nondefault = .false.
    end if

    if (require_nondefault .and. (.not. allow_nondefault)) then
      write(log_message, "(A)") "Variable ", trim(sname), &
            " requires user to set a value but does not allow value to change"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    ! 1) Determine category ID
    do cat_ind = 1, size(this%categories)
      if (trim(category) .eq. trim(this%categories(cat_ind))) then
        exit
      end if
    end do
    if (cat_ind .gt. size(this%categories)) then
      allocate(new_categories(cat_ind))
      new_categories(1:size(this%categories)) = this%categories
      new_categories(cat_ind) = category
      deallocate(this%categories)
      this%categories => new_categories
    end if

    ! 2) Error checking
    ll_ptr => this%vars
    nullify(ll_prev) ! avoid 'll_prev' may be used uninitialized warning from gfortran
    do while (associated(ll_ptr))
      if (case_insensitive_eq(trim(sname), trim(ll_ptr%short_name))) then
        write(log_message, "(A,1X,A)") trim(sname), "has been added twice"
        call marbl_status_log%log_error(log_message, subname)
      end if

      ! (b) Ensure pointers do not point to same target as other variables
      if (present(rptr)) then
        if (associated(rptr, ll_ptr%rptr)) then
          write(log_message, "(4A)") trim(sname), " and ", trim(ll_ptr%short_name), &
                                     " both point to same variable in memory."
          call marbl_status_log%log_error(log_message, subname)
        end if
      end if
      if (present(iptr)) then
        if (associated(iptr, ll_ptr%iptr)) then
          write(log_message, "(4A)") trim(sname), " and ", trim(ll_ptr%short_name), &
                                     " both point to same variable in memory."
          call marbl_status_log%log_error(log_message, subname)
        end if
      end if
      if (present(lptr)) then
        if (associated(lptr, ll_ptr%lptr)) then
          write(log_message, "(4A)") trim(sname), " and ", trim(ll_ptr%short_name), &
                                     " both point to same variable in memory."
          call marbl_status_log%log_error(log_message, subname)
        end if
      end if
      if (present(sptr)) then
        if (associated(sptr, ll_ptr%sptr)) then
          write(log_message, "(4A)") trim(sname), " and ", trim(ll_ptr%short_name), &
                                     " both point to same variable in memory."
          call marbl_status_log%log_error(log_message, subname)
        end if
      end if

      if (marbl_status_log%labort_marbl) return
      ll_prev => ll_ptr
      ll_ptr => ll_ptr%next
    end do

    ! 3) Create new entry
    !    All pointer components of new_entry are nullified in the type definition
    !    via => NULL() statements
    allocate(new_entry)
    select case (trim(datatype))
      case ('real')
        if (present(rptr)) then
          new_entry%rptr => rptr
        else
          write(log_message, "(A)")                                           &
               "Defining real parameter but rptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('integer')
        if (present(iptr)) then
          new_entry%iptr => iptr
        else
          write(log_message, "(A)")                                           &
               "Defining integer parameter but iptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('logical')
        if (present(lptr)) then
          new_entry%lptr => lptr
        else
          write(log_message, "(A)")                                           &
               "Defining logical parameter but lptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('string')
        if (present(sptr)) then
          new_entry%sptr => sptr
        else
          write(log_message, "(A)")                                           &
               "Defining string parameter but aptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case DEFAULT
        write(log_message, "(2A)") "Unknown datatype: ", trim(datatype)
        call marbl_status_log%log_error(log_message, subname)
        return
    end select
    new_entry%short_name   = trim(sname)
    new_entry%long_name    = trim(lname)
    new_entry%units        = trim(units)
    new_entry%datatype     = trim(datatype)
    new_entry%category_ind = cat_ind
    if (present(comment)) then
      new_entry%comment = comment
    else
      new_entry%comment = ''
    end if

    ! 4) Append new entry to list
    if (.not.associated(this%vars)) then
      this%vars => new_entry
    else
      ll_prev%next => new_entry
    end if

    ! 5) Was there a put_setting() call to change this variable?
    nullify(ll_prev)
    ll_ptr => this%VarsFromPut
    ! If new_entry%short_name = 'varname(1)' then it should match either 'varname(1)' or 'varname'
    ! Use alternate_sname to hold potential alternate match
    alternate_sname = ''
    if (len_trim(new_entry%short_name) .ge. 3) then
      if (new_entry%short_name(len_trim(new_entry%short_name)-2:len_trim(new_entry%short_name)) .eq. '(1)') then
        alternate_sname = new_entry%short_name(1:len_trim(new_entry%short_name)-3)
      end if
    end if
    put_called = .false.
    do while (associated(ll_ptr))
      if (case_insensitive_eq(ll_ptr%short_name, new_entry%short_name) .or. &
          case_insensitive_eq(ll_ptr%short_name, alternate_sname)) then
        put_called = .true.
        ! 5a) Look to see if put_setting used the inputline interface
        if (trim(ll_ptr%datatype) .eq. "unknown") then
          select case (new_entry%datatype)
            case ("real")
              allocate(ll_ptr%rptr)
              call marbl_settings_string_to_var(ll_ptr%sptr, marbl_status_log, rval = ll_ptr%rptr)
            case ("integer")
              allocate(ll_ptr%iptr)
              call marbl_settings_string_to_var(ll_ptr%sptr, marbl_status_log, ival = ll_ptr%iptr)
            case ("string")
              call marbl_settings_string_to_var(ll_ptr%sptr, marbl_status_log, sval = tmp_sval)
              ll_ptr%sptr = tmp_sval
            case ("logical")
              allocate(ll_ptr%lptr)
              call marbl_settings_string_to_var(ll_ptr%sptr, marbl_status_log, lval = ll_ptr%lptr)
          end select
          if (marbl_status_log%labort_marbl) then
            call marbl_status_log%log_error_trace('marbl_settings_string_to_var', subname)
            return
          end if
          ll_ptr%datatype = new_entry%datatype
        end if
        ! 5b) Look to see if an integer value was explicitly put for a real variable
        if (associated(ll_ptr%iptr).and.associated(new_entry%rptr)) then
          allocate(ll_ptr%rptr)
          ll_ptr%rptr = real(ll_ptr%iptr,r8)
        end if
        ! 5c) Actually update the new entry in the linked list
        nondefault_val = .false.
        ! Allow update if the datatypes match and either the values are the same
        ! or a non-default value is allowed
        select case (new_entry%datatype)
          case ("real")
            datatype_match = associated(ll_ptr%rptr)
            if (datatype_match) &
              nondefault_val = .not. (ll_ptr%rptr .eq. new_entry%rptr)
            put_success = (datatype_match .and. (allow_nondefault .or. (.not. nondefault_val)))
            if (put_success) new_entry%rptr = ll_ptr%rptr
          case ("integer")
            datatype_match = associated(ll_ptr%iptr)
            if (datatype_match) &
              nondefault_val = .not. (ll_ptr%iptr .eq. new_entry%iptr)
            put_success = (datatype_match .and. (allow_nondefault .or. (.not. nondefault_val)))
            if (put_success) new_entry%iptr = ll_ptr%iptr
          case ("string")
            datatype_match = associated(ll_ptr%sptr)
            if (datatype_match) &
              nondefault_val = .not. (ll_ptr%sptr .eq. new_entry%sptr)
            put_success = (datatype_match .and. (allow_nondefault .or. (.not. nondefault_val)))
            if (put_success) new_entry%sptr = ll_ptr%sptr
          case ("logical")
            datatype_match = associated(ll_ptr%lptr)
            if (datatype_match) &
              nondefault_val = .not. (ll_ptr%lptr .eqv. new_entry%lptr)
            put_success = (datatype_match .and. (allow_nondefault .or. (.not. nondefault_val)))
            if (put_success) new_entry%lptr = ll_ptr%lptr
        end select
        ! Abort if the put() failed
        if (.not. put_success) then
            write(log_message, "(3A)") "put_setting(", trim(ll_ptr%short_name), ") failed..."
            call marbl_status_log%log_error(log_message, subname)
          if (.not. datatype_match) then
            write(log_message, "(4A)") "...the datatype was incorrect; expecting ", &
                                       trim(new_entry%datatype), " but user provided ", &
                                       trim(ll_ptr%datatype)
            call marbl_status_log%log_error(log_message, subname)
          end if
          if (nondefault_val .and. (.not. allow_nondefault)) then
            write(log_message, "(3A)") "... ", trim(ll_ptr%short_name), &
                                       " can not be changed in the current configuration"
            call marbl_status_log%log_error(log_message, subname)
          end if
          return
        end if

        ! 5b) Remove entry from VarsFromPut list
        !     Different procedure if ll_ptr is first entry in list
        if (associated(ll_ptr,this%VarsFromPut)) then
          this%VarsFromPut => ll_ptr%next
          deallocate(ll_ptr)
          ll_ptr => this%VarsFromPut
        else
          ll_prev%next => ll_ptr%next
          deallocate(ll_ptr)
          ll_ptr => ll_prev%next
        end if
      else
        ! 5c) Once we are past first entry, ll_prev%next => ll_ptr
        ll_prev => ll_ptr
        ll_ptr => ll_ptr%next
      end if
    end do
    ! 5d) Error checking: was put_setting() called if variable requires it?
    if (require_nondefault .and. (.not. put_called)) then
      write(log_message, "(3A)") "User must provide value for ", trim(sname), " via put_setting()"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    ! 6) Increment count
    this%cnt = this%cnt + 1

  end subroutine add_var

  !*****************************************************************************

  subroutine add_var_1d_r8(this, sname, lname, units, category, r8array,      &
                           marbl_status_log, nondefault_allowed, nondefault_required)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: category
    real(kind=r8), dimension(:), target, intent(in)    :: r8array
    type(marbl_log_type),                intent(inout) :: marbl_status_log
    logical, optional,                   intent(in)    :: nondefault_allowed
    logical, optional,                   intent(in)    :: nondefault_required

    character(len=*), parameter :: subname = 'marbl_settings_mod:add_var_1d_r8'

    character(len=char_len) :: sname_loc
    real(r8), pointer       :: rptr => NULL()
    integer                 :: n
    logical                 :: labort_marbl_loc

    labort_marbl_loc = .false.
    do n=1,size(r8array)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      rptr => r8array(n)
      call this%add_var(sname_loc, lname, units, 'real', category, marbl_status_log, &
                          rptr=rptr, nondefault_allowed=nondefault_allowed,          &
                          nondefault_required=nondefault_required)
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)
    end do

    marbl_status_log%labort_marbl = labort_marbl_loc
    if (marbl_status_log%labort_marbl) return

  end subroutine add_var_1d_r8

  !*****************************************************************************

  subroutine add_var_1d_int(this, sname, lname, units, category, intarray,     &
                            marbl_status_log, nondefault_allowed, nondefault_required)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: category
    integer, dimension(:), target,       intent(in)    :: intarray
    type(marbl_log_type),                intent(inout) :: marbl_status_log
    logical, optional,                   intent(in)    :: nondefault_allowed
    logical, optional,                   intent(in)    :: nondefault_required

    character(len=*), parameter :: subname = 'marbl_settings_mod:add_var_1d_int'

    character(len=char_len) :: sname_loc
    integer, pointer        :: iptr => NULL()
    integer                 :: n
    logical                 :: labort_marbl_loc

    labort_marbl_loc = .false.
    do n=1,size(intarray)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      iptr => intarray(n)
      call this%add_var(sname_loc, lname, units, 'integer', category, marbl_status_log, &
                          iptr=iptr, nondefault_allowed=nondefault_allowed,             &
                          nondefault_required=nondefault_required)
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)
    end do

    marbl_status_log%labort_marbl = labort_marbl_loc
    if (marbl_status_log%labort_marbl) return

  end subroutine add_var_1d_int

  !*****************************************************************************

  subroutine add_var_1d_str(this, sname, lname, units, category, strarray,     &
                            marbl_status_log, nondefault_allowed, nondefault_required)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: category
    character(len=char_len),     target, intent(in)    :: strarray(:)
    type(marbl_log_type),                intent(inout) :: marbl_status_log
    logical, optional,                   intent(in)    :: nondefault_allowed
    logical, optional,                   intent(in)    :: nondefault_required

    character(len=*), parameter :: subname = 'marbl_settings_mod:add_var_1d_str'

    character(len=char_len)          :: sname_loc
    character(len=char_len), pointer :: sptr => NULL()
    integer                          :: n
    logical                          :: labort_marbl_loc

    labort_marbl_loc = .false.
    do n=1,size(strarray)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      sptr => strarray(n)
      call this%add_var(sname_loc, lname, units, 'string', category, marbl_status_log, &
                          sptr=sptr, nondefault_allowed=nondefault_allowed,            &
                          nondefault_required=nondefault_required)
      call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)
    end do

    marbl_status_log%labort_marbl = labort_marbl_loc
    if (marbl_status_log%labort_marbl) return

  end subroutine add_var_1d_str

  !*****************************************************************************

  subroutine finalize_vars(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:finalize_vars'
    character(len=char_len)     :: log_message

    character(len=7)        :: logic
    integer                 :: i, cat_ind
    type(marbl_single_setting_ll_type), pointer :: ll_ptr

    ! (1) Lock data type (put calls will now cause MARBL to abort)
    this%init_called = .true.

    ! (2) Abort if anything is left in this%VarsFromPut
    if (associated(this%VarsFromPut)) then
      ll_ptr => this%VarsFromPut
      do while (associated(ll_ptr))
        write(log_message, "(2A)") "Unrecognized varname from put_setting(): ", &
                                   trim(ll_ptr%short_name)
        call marbl_status_log%log_error(log_message, subname)
        ll_ptr => ll_ptr%next
      end do
      return
    end if

    call marbl_status_log%log_header("Tunable Parameters", subname)

    do cat_ind = 1,size(this%categories)
      ll_ptr => this%vars
      do while (associated(ll_ptr))
        if (ll_ptr%category_ind .eq. cat_ind) then
        ! (3) write parameter to log_message (format depends on datatype)
          select case(trim(ll_ptr%datatype))
            case ('string')
              write(log_message, "(4A)") trim(ll_ptr%short_name), " = '",  &
                                         trim(ll_ptr%sptr), "'"
            case ('real')
              write(log_message, "(2A,E25.17)") trim(ll_ptr%short_name),   &
                                                " = ", ll_ptr%rptr
            case ('integer')
              write(log_message, "(2A,I0)") trim(ll_ptr%short_name), " = ", &
                                            ll_ptr%iptr
            case ('logical')
              if (ll_ptr%lptr) then
                logic = '.true.'
              else
                logic = '.false.'
              end if
              write(log_message, "(3A)") trim(ll_ptr%short_name), " = ",   &
                                         trim(logic)
            case DEFAULT
              write(log_message, "(2A)") trim(ll_ptr%datatype),            &
                                         ' is not a valid datatype for parameter'
              call marbl_status_log%log_error(log_message, subname)
              return
          end select

          ! (4) Write log_message to the log
          if (ll_ptr%comment.ne.'') then
            if (len_trim(log_message) + 3 + len_trim(ll_ptr%comment) .le. len(log_message)) then
              write(log_message, "(3A)") trim(log_message), ' ! ',                  &
                                         trim(ll_ptr%comment)
            else
              call marbl_status_log%log_noerror(&
                   '! WARNING: omitting comment on line below because including it exceeds max length for log message', &
                   subname)
            end if
          endif
          call marbl_status_log%log_noerror(log_message, subname)
        end if
        ll_ptr => ll_ptr%next
      end do  ! ll_ptr
      if (cat_ind .ne. size(this%categories)) then
        call marbl_status_log%log_noerror('', subname)
      end if
    end do  ! cat_ind

    ! (5) Set up array of pointers
    if (allocated(this%varArray)) then
      write(log_message, "(A)") "Already allocated memory for varArray!"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if
    allocate(this%varArray(this%cnt))
    ll_ptr => this%vars
    do i = 1,this%cnt
      this%varArray(i)%ptr => ll_ptr
      ll_ptr => ll_ptr%next
    end do

  end subroutine finalize_vars

  !*****************************************************************************

  subroutine put(this, var, marbl_status_log, rval, ival, lval, sval, uval)

    class(marbl_settings_type), intent(inout) :: this
    character(len=*),           intent(in)    :: var
    type(marbl_log_type),       intent(inout) :: marbl_status_log
    real(r8),         optional, intent(in)    :: rval
    integer,          optional, intent(in)    :: ival
    logical,          optional, intent(in)    :: lval
    character(len=*), optional, intent(in)    :: sval
    character(len=*), optional, intent(in)    :: uval

    type(marbl_single_setting_ll_type), pointer :: new_entry
    character(len=*), parameter :: subname = 'marbl_settings_mod:put'
    character(len=char_len) :: log_message

    call marbl_status_log%construct()
    if (this%init_called) then
      write(log_message, "(3A)") "Can not put ", trim(adjustl(var)), ", init has already been called"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    allocate(new_entry)
    new_entry%short_name = adjustl(var)

    if (present(rval)) then
      allocate(new_entry%rptr)
      new_entry%rptr = rval
      new_entry%datatype = 'real'
    end if
    if (present(ival)) then
      allocate(new_entry%iptr)
      new_entry%iptr = ival
      new_entry%datatype = 'integer'
    end if
    if (present(lval)) then
      allocate(new_entry%lptr)
      new_entry%lptr = lval
      new_entry%datatype = 'logical'
    end if
    if (present(sval)) then
      allocate(new_entry%sptr)
      new_entry%sptr = sval
      new_entry%datatype = 'string'
    end if
    if (present(uval)) then
      allocate(new_entry%sptr)
      new_entry%sptr = uval
      new_entry%datatype = 'unknown'
    end if

    if (.not.associated(this%VarsFromPut)) then
      this%VarsFromPut => new_entry
    else
      this%LastVarFromPut%next => new_entry
    end if
    this%LastVarFromPut => new_entry

  end subroutine put

  !*****************************************************************************

  subroutine get(this, var, marbl_status_log, rval, ival, lval, sval)

    class(marbl_settings_type), intent(in)    :: this
    character(len=*),           intent(in)    :: var
    real(r8),         optional, intent(out)   :: rval
    integer,          optional, intent(out)   :: ival
    logical,          optional, intent(out)   :: lval
    character(len=*), optional, intent(out)   :: sval
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:get'
    character(len=char_len)     :: log_message

    type(marbl_single_setting_ll_type), pointer :: ll_ptr
    integer :: cnt

    call marbl_status_log%construct()
    cnt = 0
    if (present(rval)) cnt = cnt + 1
    if (present(ival)) cnt = cnt + 1
    if (present(lval)) cnt = cnt + 1
    if (present(sval)) cnt = cnt + 1

    if (cnt.gt.1) then
      write(log_message, "(A)") 'Must provide just one of rval, ival, lval, or sval to get()'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    ll_ptr => this%vars
    do while (associated(ll_ptr))
      if (case_insensitive_eq((ll_ptr%short_name), trim(var))) exit
      ll_ptr => ll_ptr%next
    end do
    if (.not.associated(ll_ptr)) then
      write(log_message, "(2A)") trim(var), 'not found!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    select case(trim(ll_ptr%datatype))
      case ('real')
        if (present(rval)) then
          rval = ll_ptr%rptr
        else
          write(log_message, "(2A)") trim(var), ' requires real value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('integer')
        if (present(ival)) then
          ival = ll_ptr%iptr
        else
          write(log_message, "(2A)") trim(var), ' requires integer value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('logical')
        if (present(lval)) then
          lval = ll_ptr%lptr
        else
          write(log_message, "(2A)") trim(var), ' requires logical value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('string')
        if (present(sval)) then
          if (len(sval).lt.len(trim(ll_ptr%sptr))) then
            write(log_message, "(2A,I0,A,I0,A)") trim(var), ' requires ',     &
              len(trim(ll_ptr%sptr)), ' bytes to store, but only ',           &
              len(sval), ' are provided.'
            call marbl_status_log%log_error(log_message, subname)
            return
          end if
          sval = trim(ll_ptr%sptr)
        else
          write(log_message, "(2A)") trim(var), ' requires string value'
          call marbl_status_log%log_error(log_message, subname)
        end if
    end select

  end subroutine get

  !*****************************************************************************

  subroutine destruct(this)

    class(marbl_settings_type), intent(inout) :: this

    type(marbl_single_setting_ll_type), pointer :: ll_next

    ! Empty vars linked list
    do while (associated(this%vars))
      ll_next => this%vars%next
      deallocate(this%vars)
      this%vars => ll_next
    end do

    ! Empty VarsFromPut linked list (should already be empty)
    do while (associated(this%VarsFromPut))
      ll_next => this%VarsFromPut
      deallocate(this%VarsFromPut)
      this%VarsFromPut => ll_next
    end do

    ! Nullify LastVarFromPut
    nullify(this%LastVarFromPut)

    ! Deallocate varArray
    if (allocated(this%varArray)) deallocate(this%varArray)
    this%cnt=0
    this%init_called = .false.

  end subroutine destruct

  !*****************************************************************************

  function get_cnt(this) result(cnt)
    class(marbl_settings_type), intent(in) :: this
    integer(int_kind) :: cnt

    cnt = this%cnt

  end function get_cnt

  !*****************************************************************************

  function inquire_id(this, var, marbl_status_log) result(id)

    class(marbl_settings_type), intent(in)    :: this
    character(len=*),           intent(in)    :: var
    type(marbl_log_type),       intent(inout) :: marbl_status_log
    integer(int_kind) :: id

    character(len=*), parameter :: subname = 'marbl_settings_mod:inquire_id'
    character(len=char_len)     :: log_message
    integer(int_kind) :: n

    id = -1
    do n=1,this%cnt
      if (case_insensitive_eq(trim(var), trim(this%varArray(n)%ptr%short_name))) then
        id = n
        return
      end if
    end do

    write(log_message, "(2A)") "No match for variable named ", trim(var)
    call marbl_status_log%log_error(log_message, subname)

  end function inquire_id

  !*****************************************************************************

  subroutine inquire_metadata(this, id, sname, lname, units, &
                              datatype)

    class(marbl_settings_type), intent(in)    :: this
    integer(int_kind),          intent(in)    :: id
    character(len=*), optional, intent(out)   :: sname, lname, units
    character(len=*), optional, intent(out)   :: datatype

    if (present(sname)) then
      sname = this%varArray(id)%ptr%short_name
    end if

    if (present(lname)) then
      lname = this%varArray(id)%ptr%long_name
    end if

    if (present(units)) then
      units = this%varArray(id)%ptr%units
    end if

    if (present(datatype)) then
      datatype = this%varArray(id)%ptr%datatype
    end if

  end subroutine inquire_metadata

  !***********************************************************************

  subroutine check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

    type(marbl_log_type), intent(inout) :: marbl_status_log
    character(len=*),     intent(in)    :: sname
    character(len=*),     intent(in)    :: subname
    logical,              intent(inout) :: labort_marbl_loc
    character(len=char_len) :: routine_name

    if (marbl_status_log%labort_marbl) then
      labort_marbl_loc = marbl_status_log%labort_marbl
      write(routine_name,"(3A)") "this%add_var(", trim(sname), ")"
      call marbl_status_log%log_error_trace(routine_name, subname)
      marbl_status_log%labort_marbl = .false.
    end if

  end subroutine check_and_log_add_var_error

  !***********************************************************************

  function case_insensitive_eq(str1, str2) result(same_str)

    ! This routine is necessary to allow put statements to use different case
    ! than the add_var() routine. For example, MARBL calls add_var with the
    ! string 'parm_Fe_bioavail' but it's okay for the GCM to call put with the
    ! string 'parm_fe_bioavail'

    character(len=*), intent(in) :: str1, str2
    logical :: same_str

    integer :: int_char(2)
    integer :: i, j

    ! Assume strings are not the same
    same_str = .false.

    ! If strings are not the same length, they can't be equal!
    if (len_trim(str1).ne.len_trim(str2)) then
      return
    end if


    do i=1,len_trim(str1)
      ! character by character compare, convert letters to lower-case
      int_char(1) = iachar(str1(i:i))
      int_char(2) = iachar(str2(i:i))
      do j=1,2
        if ((int_char(j) .ge. iachar('A')) .and. (int_char(j) .le. iachar('Z'))) &
          int_char(j) = int_char(j) + iachar('a') - iachar('A')
      end do
      ! return if characters are not the same
      if (int_char(1).ne.int_char(2)) return
    end do

    ! If test made it this far, strings match
    same_str = .true.

  end function case_insensitive_eq

  !*****************************************************************************

  subroutine print_single_derived_parm_r8(sname_in, sname_out, val_out, subname, marbl_status_log)

    character(len=*), intent(in) :: sname_in
    character(len=*), intent(in) :: sname_out
    real(r8),         intent(in) :: val_out
    character(len=*), intent(in) :: subname
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=char_len) :: log_message

    write(log_message, "(2A,E25.17,3A)") &
         trim(sname_out), ' = ', val_out, ' (value computed from ', trim(sname_in), ')'
    call marbl_status_log%log_noerror(log_message, subname)

  end subroutine print_single_derived_parm_r8

  !*****************************************************************************

  subroutine print_single_derived_parm_int(sname_in, sname_out, val_out, subname, marbl_status_log)

    character(len=*),  intent(in) :: sname_in
    character(len=*),  intent(in) :: sname_out
    integer(int_kind), intent(in) :: val_out
    character(len=*),  intent(in) :: subname
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=char_len) :: log_message

    write(log_message, "(2A,I0,3A)") &
         trim(sname_out), ' = ', val_out, ' (value computed from ', trim(sname_in), ')'
    call marbl_status_log%log_noerror(log_message, subname)

  end subroutine print_single_derived_parm_int

  !*****************************************************************************

end module marbl_settings_mod
