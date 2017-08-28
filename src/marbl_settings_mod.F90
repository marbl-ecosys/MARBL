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

  use marbl_internal_types, only : autotroph_type
  use marbl_internal_types, only : zooplankton_type
  use marbl_internal_types, only : grazing_type

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
       dust_Fe_scavenge_scale  = 1.0e9         !dust scavenging scale factor

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
      Qp_zoo        = c1 / 117.0_r8,      & !P/C ratio (mmol/mmol) zoo
      Qfe_zoo       = 3.0e-6_r8,          & !zooplankton Fe/C ratio
      gQsi_0        = 0.137_r8,           & !initial Si/C ratio for growth
      gQsi_max      = 0.685_r8,           & !max Si/C ratio for growth
      gQsi_min      = 0.0457_r8,          & !min Si/C ratio for growth
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

  !----------------------------------------------------------------------------------------------
  !  Variables defined in marbl_settings_define_pre_tracers1, marbl_settings_define_pre_tracers2,
  !  or marbl_settings_define_post_tracers
  !
  ! NOTE: defaults values are set in the corresponding marbl_settings_set_defaults routines
  !       but may be overridden at runtime through a put call [marbl_instance%put_setting()]
  !----------------------------------------------------------------------------------------------

  !  marbl_settings_mod_pre_tracers1
  !---------------------------------

  logical(log_kind), target ::  ciso_on                       ! control whether ciso tracer module is active
  logical(log_kind), target ::  lsource_sink                  ! control which portion of code is executed, useful for debugging
  logical(log_kind), target :: ciso_lsource_sink              ! control which portion of carbon isotope code is executed, useful for debugging
  logical(log_kind), target :: lecovars_full_depth_tavg       ! should base ecosystem vars be written full depth
  logical(log_kind), target :: ciso_lecovars_full_depth_tavg  ! should carbon isotope vars be written full depth
  logical(log_kind), target :: lflux_gas_o2                   ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: lflux_gas_co2                  ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: lcompute_nhx_surface_emis      ! control if NHx emissions are computed
  logical(log_kind), target :: lvariable_PtoC                 ! control if PtoC ratios in autotrophs vary
  logical(log_kind), target :: ladjust_bury_coeff             ! control if bury coefficients are adjusted (rather than constant)
                                                              !   bury coefficients (POC_bury_coeff, POP_bury_coeff, bSi_bury_coeff)
                                                              !   reside in marbl_particulate_share_type; when ladjust_bury_coeff is
                                                              !   .true., bury coefficients are adjusted to preserve C, P, Si
                                                              !   inventories on timescales exceeding bury_coeff_rmean_timescale_years
                                                              !   (this is done primarily in spinup runs)

  character(len=char_len), target :: init_bury_coeff_opt

  real(r8), target :: &
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

  real(r8), dimension(4), target :: &
       parm_scalelen_z,       & ! depths of prescribed scalelen values
       parm_scalelen_vals       ! prescribed scalelen values

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

  !  marbl_settings_define_pre_tracers2
  !------------------------------------

  type(autotroph_type),   allocatable, target :: autotrophs(:)
  type(zooplankton_type), allocatable, target :: zooplankton(:)
  type(grazing_type),     allocatable, target :: grazing(:,:)

  !  marbl_settings_define_post_tracers
  !  (some components of autotrophs, zooplankton,
  !  and grazing are also set in post_tracers)
  !----------------------------------------------

  ! FIXME #69: this array is allocated in marbl_init_mod:marbl_init_tracers()
  !            and that allocation is not ideal for threaded runs
  character(len=char_len), allocatable, target, dimension(:) :: tracer_restore_vars

  !---------------------------------------------------------------------
  !  Auxiliary variables (str -> int conversions, indices, etc)
  !---------------------------------------------------------------------

  integer (int_kind)            :: caco3_bury_thres_iopt
  integer (int_kind), parameter :: caco3_bury_thres_iopt_fixed_depth = 1
  integer (int_kind), parameter :: caco3_bury_thres_iopt_omega_calc  = 2

  ! grazing functions
  integer (int_kind), parameter ::           &
         grz_fnc_michaelis_menten = 1,       &
         grz_fnc_sigmoidal        = 2

  !*****************************************************************************

  ! Variables used from other modules should be private
  ! (So we don't accidentally use them from this module)
  private :: r8, int_kind, log_kind, char_len
  private :: c1, dps
  private :: zooplankton_type, autotroph_type, grazing_type
  private :: autotroph_cnt, zooplankton_cnt, grazer_prey_cnt
  private :: marbl_log_type

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
  private :: log_add_var_error
  private :: case_insensitive_eq
  private :: print_single_derived_parm
  private :: print_single_derived_parm_r8
  private :: print_single_derived_parm_int

contains

  !*****************************************************************************

  subroutine marbl_settings_set_defaults_pre_tracers1()

    !-----------------------------------------------------------------------
    !  Default values
    !-----------------------------------------------------------------------

    autotroph_cnt                 = 3
    zooplankton_cnt               = 1
    grazer_prey_cnt               = 3
    ciso_on                       = .false.
    lsource_sink                  = .true.
    ciso_lsource_sink             = .true.
    lecovars_full_depth_tavg      = .false.
    ciso_lecovars_full_depth_tavg = .false.
    lflux_gas_o2                  = .true.
    lflux_gas_co2                 = .true.
    lcompute_nhx_surface_emis     = .true.
    lvariable_PtoC                = .true.
    init_bury_coeff_opt           = 'nml'
    ladjust_bury_coeff            = .false.
    parm_Fe_bioavail           = 1.0_r8         ! in marbl_settings framework, see NOTE in module var declaration
    parm_o2_min                = 5.0_r8         ! in marbl_settings framework, see NOTE in module var declaration
    parm_o2_min_delta          = 5.0_r8         ! in marbl_settings framework, see NOTE in module var declaration
    parm_kappa_nitrif_per_day  = 0.06_r8        ! in marbl_settings framework, see NOTE in module var declaration
    parm_nitrif_par_lim        = 1.0_r8         ! in marbl_settings framework, see NOTE in module var declaration
    parm_labile_ratio          = 0.94_r8        ! in marbl_settings framework, see NOTE in module var declaration
    parm_init_POC_bury_coeff   = 1.1_r8         ! in marbl_settings framework, see NOTE in module var declaration
    parm_init_POP_bury_coeff   = 1.1_r8         ! in marbl_settings framework, see NOTE in module var declaration
    parm_init_bSi_bury_coeff   = 1.0_r8         ! in marbl_settings framework, see NOTE in module var declaration
    parm_Fe_scavenge_rate0     = 15.0_r8        ! in marbl_settings framework, see NOTE in module var declaration
    parm_Lig_scavenge_rate0    = 0.015_r8       ! in marbl_settings framework, see NOTE in module var declaration
    parm_FeLig_scavenge_rate0  = 1.3_r8         ! in marbl_settings framework, see NOTE in module var declaration
    parm_Lig_degrade_rate0     = 0.000094_r8    ! in marbl_settings framework, see NOTE in module var declaration
    parm_Fe_desorption_rate0   = 1.0e-6_r8      ! in marbl_settings framework, see NOTE in module var declaration
    parm_f_prod_sp_CaCO3       = 0.070_r8       ! in marbl_settings framework, see NOTE in module var declaration
    parm_POC_diss              = 100.0e2_r8     ! in marbl_settings framework, see NOTE in module var declaration
    parm_SiO2_diss             = 770.0e2_r8     ! in marbl_settings framework, see NOTE in module var declaration
    parm_CaCO3_diss            = 500.0e2_r8     ! in marbl_settings framework, see NOTE in module var declaration
    parm_sed_denitrif_coeff    = 1.0_r8         ! in marbl_settings framework, see NOTE in module var declaration
    bury_coeff_rmean_timescale_years = 10.0_r8  ! in marbl_settings framework, see NOTE in module var declaration
    parm_scalelen_z    = (/ 100.0e2_r8, 250.0e2_r8, 500.0e2_r8, 1000.0e2_r8 /) ! in marbl_settings framework, see NOTE in module var declaration
    parm_scalelen_vals = (/     1.0_r8,     2.2_r8,     4.0_r8,      5.0_r8 /) ! in marbl_settings framework, see NOTE in module var declaration

    iron_frac_in_dust      = 0.035_r8 * 0.01_r8    ! in marbl_settings framework, see NOTE in module var declaration
    iron_frac_in_bc        = 0.06_r8               ! in marbl_settings framework, see NOTE in module var declaration
    caco3_bury_thres_opt   = 'omega_calc'          ! in marbl_settings framework, see NOTE in module var declaration
    caco3_bury_thres_depth = 3000.0e2              ! in marbl_settings framework, see NOTE in module var declaration
    PON_bury_coeff         = 0.5_r8                ! in marbl_settings framework, see NOTE in module var declaration
    ciso_fract_factors     = 'Rau'                 ! in marbl_settings framework, see NOTE in module var declaration

  end subroutine marbl_settings_set_defaults_pre_tracers1

  !*****************************************************************************

  subroutine marbl_settings_set_defaults_pre_tracers2(marbl_status_log)

    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_set_defaults_pre_tracers2'
    character(len=char_len)     :: log_message
    integer :: m, n

    ! Allocate memory
    ! FIXME #69: this is not ideal for threaded runs
    if (.not.allocated(autotrophs)) then
      allocate(autotrophs(autotroph_cnt))
      allocate(zooplankton(zooplankton_cnt))
      allocate(grazing(grazer_prey_cnt, zooplankton_cnt))
      do n=1,zooplankton_cnt
        do m=1,grazer_prey_cnt
          call grazing(m,n)%construct(autotroph_cnt, zooplankton_cnt, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            write(log_message,"(A,I0,A,I0,A)") 'grazing(', m, ',', n, ')%construct'
            call marbl_status_log%log_error_trace(log_message, subname)
            return
          end if
        end do
      end do
    end if

    !-----------------------------------------------------------------------
    !  Default values
    !-----------------------------------------------------------------------

    do m=1,autotroph_cnt
      select case (m)
        case (1)
          autotrophs(m)%sname = 'sp'
          autotrophs(m)%lname = 'Small Phyto'
          autotrophs(m)%Nfixer = .false.
          autotrophs(m)%imp_calcifier = .true.
          autotrophs(m)%exp_calcifier = .false.
          autotrophs(m)%silicifier = .false.
          autotrophs(m)%kFe             = 0.03e-3_r8         ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kPO4            = 0.005_r8           ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kDOP            = 0.3_r8             ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kNO3            = 0.25_r8            ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kNH4            = 0.01_r8            ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kSiO3           = 0.0_r8             ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%Qp_fixed        =  Qp_zoo            ! only used for lvariable_PtoC=.false.
          autotrophs(m)%gQfe_0          = 35.0e-6_r8
          autotrophs(m)%gQfe_min        = 3.0e-6_r8
          autotrophs(m)%alphaPI_per_day = 0.39_r8
          autotrophs(m)%PCref_per_day   = 5.0_r8
          autotrophs(m)%thetaN_max      = 2.5_r8
          autotrophs(m)%loss_thres      = 0.01_r8
          autotrophs(m)%loss_thres2     = 0.0_r8
          autotrophs(m)%temp_thres      = -10.0_r8
          autotrophs(m)%mort_per_day    = 0.1_r8
          autotrophs(m)%mort2_per_day   = 0.01_r8
          autotrophs(m)%agg_rate_max    = 0.5_r8
          autotrophs(m)%agg_rate_min    = 0.01_r8
          autotrophs(m)%loss_poc        = 0.0_r8
        case (2)
          autotrophs(m)%sname = 'diat'
          autotrophs(m)%lname = 'Diatom'
          autotrophs(m)%Nfixer = .false.
          autotrophs(m)%imp_calcifier = .false.
          autotrophs(m)%exp_calcifier = .false.
          autotrophs(m)%silicifier = .true.
          autotrophs(m)%kFe             = 0.06e-3_r8         ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kPO4            = 0.05_r8            ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kDOP            = 0.5_r8             ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kNO3            = 0.5_r8             ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kNH4            = 0.05_r8            ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kSiO3           = 0.7_r8             ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%Qp_fixed        =  Qp_zoo            ! only used for lvariable_PtoC=.false.
          autotrophs(m)%gQfe_0          = 35.0e-6_r8
          autotrophs(m)%gQfe_min        = 3.0e-6_r8
          autotrophs(m)%alphaPI_per_day = 0.29_r8
          autotrophs(m)%PCref_per_day   = 5.0_r8
          autotrophs(m)%thetaN_max      = 4.0_r8
          autotrophs(m)%loss_thres      = 0.02_r8
          autotrophs(m)%loss_thres2     = 0.0_r8
          autotrophs(m)%temp_thres      = -10.0_r8
          autotrophs(m)%mort_per_day    = 0.1_r8
          autotrophs(m)%mort2_per_day   = 0.01_r8
          autotrophs(m)%agg_rate_max    = 0.5_r8
          autotrophs(m)%agg_rate_min    = 0.02_r8
          autotrophs(m)%loss_poc        = 0.0_r8
        case (3)
          autotrophs(m)%sname = 'diaz'
          autotrophs(m)%lname = 'Diazotroph'
          autotrophs(m)%Nfixer = .true.
          autotrophs(m)%imp_calcifier = .false.
          autotrophs(m)%exp_calcifier = .false.
          autotrophs(m)%silicifier = .false.
          autotrophs(m)%kFe             = 0.045e-3_r8        ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kPO4            = 0.015_r8           ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kDOP            = 0.075_r8           ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kNO3            = 2.0_r8             ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kNH4            = 0.2_r8             ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%kSiO3           = 0.0_r8             ! in marbl_settings framework, see NOTE in module var declaration
          autotrophs(m)%Qp_fixed        = 0.32_r8 * Qp_zoo   ! only used for lvariable_PtoC=.false.
          autotrophs(m)%gQfe_0          = 70.0e-6_r8
          autotrophs(m)%gQfe_min        = 6.0e-6_r8
          autotrophs(m)%alphaPI_per_day = 0.39_r8
          autotrophs(m)%PCref_per_day   = 2.2_r8
          autotrophs(m)%thetaN_max      = 2.5_r8
          autotrophs(m)%loss_thres      = 0.02_r8
          autotrophs(m)%loss_thres2     = 0.001_r8
          autotrophs(m)%temp_thres      = 15.0_r8
          autotrophs(m)%mort_per_day    = 0.1_r8
          autotrophs(m)%mort2_per_day   = 0.01_r8
          autotrophs(m)%agg_rate_max    = 0.5_r8
          autotrophs(m)%agg_rate_min    = 0.01_r8
          autotrophs(m)%loss_poc        = 0.0_r8
        case DEFAULT
          write(autotrophs(m)%sname,"(A,I3.3)") 'auto', n
          write(autotrophs(m)%lname,"(A,I0)") 'Autotroph number ', n
          autotrophs(m)%Nfixer        = .false.
          autotrophs(m)%imp_calcifier = .false.
          autotrophs(m)%exp_calcifier = .false.
          autotrophs(m)%silicifier    = .false.
          autotrophs(m)%kFe             = c0
          autotrophs(m)%kPO4            = c0
          autotrophs(m)%kDOP            = c0
          autotrophs(m)%kNO3            = c0
          autotrophs(m)%kNH4            = c0
          autotrophs(m)%kSiO3           = c0
          autotrophs(m)%Qp_fixed        = c0
          autotrophs(m)%gQfe_0          = c0
          autotrophs(m)%gQfe_min        = c0
          autotrophs(m)%alphaPI_per_day = c0
          autotrophs(m)%PCref_per_day   = c0
          autotrophs(m)%thetaN_max      = c0
          autotrophs(m)%loss_thres      = c0
          autotrophs(m)%loss_thres2     = c0
          autotrophs(m)%temp_thres      = c0
          autotrophs(m)%mort_per_day    = c0
          autotrophs(m)%mort2_per_day   = c0
          autotrophs(m)%agg_rate_max    = c0
          autotrophs(m)%agg_rate_min    = c0
          autotrophs(m)%loss_poc        = c0
        end select
    end do

    do n=1,zooplankton_cnt
      select case (n)
        case (1)
          zooplankton(1)%sname = 'zoo'
          zooplankton(1)%lname = 'Zooplankton'
          zooplankton(n)%z_mort_0_per_day   = 0.1_r8     ! in marbl_settings framework, see NOTE in module var declaration
          zooplankton(n)%z_mort2_0_per_day  = 0.4_r8     ! in marbl_settings framework, see NOTE in module var declaration
          zooplankton(n)%loss_thres         = 0.075_r8   ! in marbl_settings framework, see NOTE in module var declaration
        case DEFAULT
          write(zooplankton(n)%sname, "(A,I3.3)") 'zoo', n
          write(zooplankton(n)%lname, "(A,I0)") 'Zooplankton number ', n
          zooplankton(n)%z_mort_0_per_day   = c0
          zooplankton(n)%z_mort2_0_per_day  = c0
          zooplankton(n)%loss_thres         = c0
      end select
    end do

    ! predator-prey relationships
    do n=1,zooplankton_cnt
      do m=1,grazer_prey_cnt
        select case (1000*n + m)
          case (1001)
            grazing(m,n)%sname = 'grz_sp_zoo'
            grazing(m,n)%lname = 'Grazing of sp by zoo'
            grazing(m,n)%auto_ind_cnt = 1
            grazing(m,n)%zoo_ind_cnt = 0
            grazing(m,n)%z_umax_0_per_day = 3.3_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%z_grz            = 1.2_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_zoo        = 0.3_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_poc        = 0.0_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_doc        = 0.06_r8   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%f_zoo_detr       = 0.12_r8   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%grazing_function = grz_fnc_michaelis_menten   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%auto_ind(1) = 1
          case (1002)
            grazing(m,n)%sname = 'grz_diat_zoo'
            grazing(m,n)%lname = 'Grazing of diat by zoo'
            grazing(m,n)%auto_ind_cnt = 1
            grazing(m,n)%zoo_ind_cnt = 0
            grazing(m,n)%z_umax_0_per_day = 3.05_r8
            grazing(m,n)%z_grz            = 1.2_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_zoo        = 0.25_r8   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_poc        = 0.38_r8   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_doc        = 0.06_r8   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%f_zoo_detr       = 0.24_r8   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%grazing_function = grz_fnc_michaelis_menten   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%auto_ind(1) = 2
          case (1003)
            grazing(m,n)%sname = 'grz_diaz_zoo'
            grazing(m,n)%lname = 'Grazing of diaz by zoo'
            grazing(m,n)%auto_ind_cnt = 1
            grazing(m,n)%zoo_ind_cnt = 0
            grazing(m,n)%z_umax_0_per_day = 3.1_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%z_grz            = 1.2_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_zoo        = 0.3_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_poc        = 0.1_r8    ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%graze_doc        = 0.06_r8   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%f_zoo_detr       = 0.12_r8   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%grazing_function = grz_fnc_michaelis_menten   ! in marbl_settings framework, see NOTE in module var declaration
            grazing(m,n)%auto_ind(1) = 3
          case DEFAULT
            write(grazing(m,n)%sname, "(A,I3.3,A,I3.3)") 'grz_', m, '_', n
            write(grazing(m,n)%lname, "(A,I0,A,I0)") 'Grazing of prey class ', m, ' by grazer ', n
            grazing(m,n)%auto_ind_cnt = 0
            grazing(m,n)%zoo_ind_cnt  = 0
            grazing(m,n)%auto_ind     = -1
            grazing(m,n)%zoo_ind      = -1
            grazing(m,n)%z_umax_0_per_day = c0
            grazing(m,n)%z_grz            = c0
            grazing(m,n)%graze_zoo        = c0
            grazing(m,n)%graze_poc        = c0
            grazing(m,n)%graze_doc        = c0
            grazing(m,n)%f_zoo_detr       = c0
            grazing(m,n)%grazing_function = grz_fnc_michaelis_menten
        end select
      end do
    end do

  end subroutine marbl_settings_set_defaults_pre_tracers2

  !*****************************************************************************

  subroutine marbl_settings_set_defaults_post_tracers(marbl_status_log)
    ! allocate memory for allocatable parameters
    ! assign default values to all parameters

    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_set_defaults_post_tracers'

    !-----------------------------------------------------------------------
    !  Default values
    !-----------------------------------------------------------------------

    if (.not. allocated(tracer_restore_vars)) then
      call marbl_status_log%log_error('tracer_restore_vars has not been allocated!', subname)
      return
    end if

    ! initialize namelist variables to default values
    tracer_restore_vars = ''

  end subroutine marbl_settings_set_defaults_post_tracers

  !*****************************************************************************

  subroutine marbl_settings_define_pre_tracers1(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_define_pre_tracers1'
    character(len=char_len)     :: log_message

    character(len=char_len)          :: sname, lname, units, datatype, category
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%settings has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if
    allocate(this%categories(0))

    !----------!
    ! Settings !
    !----------!

    category  = 'config ints'

    sname     = 'autotroph_cnt'
    lname     = 'Number of autotroph classes'
    units     = 'unitless'
    datatype  = 'integer'
    iptr      => autotroph_cnt
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'zooplankton_cnt'
    lname     = 'Number of zooplankton classes'
    units     = 'unitless'
    datatype  = 'integer'
    iptr      => zooplankton_cnt
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'grazer_prey_cnt'
    lname     = 'Number of grazer prey classes'
    units     = 'unitless'
    datatype  = 'integer'
    iptr      => grazer_prey_cnt
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    category  = 'config flags'

    sname     = 'ciso_on'
    lname     = 'Control whether CISO tracer module is active'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => ciso_on
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lsource_sink'
    lname     = 'Control which portions of code are executed (useful for debugging)'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lsource_sink
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lecovars_full_depth_tavg'
    lname     = 'Are base ecosystem tracers full depth?'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lecovars_full_depth_tavg
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ciso_lsource_sink'
    lname     = 'Control which portions of carbon isotope code are executed (useful for debugging)'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => ciso_lsource_sink
    call this%add_var(sname, lname, units, datatype, category,       &
                      marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ciso_lecovars_full_depth_tavg'
    lname     = 'Are carbon isotope tracers full depth?'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => ciso_lecovars_full_depth_tavg
    call this%add_var(sname, lname, units, datatype, category,       &
                      marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lflux_gas_o2'
    lname     = 'Run O2 gas flux portion of the code'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lflux_gas_o2
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lflux_gas_co2'
    lname     = 'Run CO2 gas flux portion of the code'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lflux_gas_co2
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lcompute_nhx_surface_emis'
    lname     = 'control if NHx emissions are computed'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lcompute_nhx_surface_emis
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lvariable_PtoC'
    lname     = 'control if PtoC ratios in autotrophs vary'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => lvariable_PtoC
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ladjust_bury_coeff'
    lname     = 'Adjust the bury coefficient to maintain equilibrium'
    units     = 'unitless'
    datatype  = 'logical'
    lptr      => ladjust_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    category  = 'config strings'

    sname     = 'init_bury_coeff_opt'
    lname     = 'How to set initial bury coefficients'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => init_bury_coeff_opt
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    category  = 'general parmeters'

    sname     = 'parm_Fe_bioavail'
    lname     = 'Fraction of Fe flux that is bioavailable'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_Fe_bioavail
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_o2_min'
    lname     = 'Minimum O2 needed for production and consumption'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    rptr      => parm_o2_min
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_o2_min_delta'
    lname     = 'Width of minimum O2 range'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    rptr      => parm_o2_min_delta
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_kappa_nitrif_per_day'
    lname     = 'Nitrification inverse time constant'
    units     = '1/day'
    datatype  = 'real'
    rptr      => parm_kappa_nitrif_per_day
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_nitrif_par_lim'
    lname     = 'PAR limit for nitrification'
    units     = 'W/m^2'
    datatype  = 'real'
    rptr      => parm_nitrif_par_lim
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_labile_ratio'
    lname     = 'Fraction of loss to DOC that is routed directly to DIC'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_labile_ratio
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_init_POC_bury_coeff'
    lname     = 'initial scale factor for burial of POC, PON'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_init_POC_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_init_POP_bury_coeff'
    lname     = 'initial scale factor for burial of POP'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_init_POP_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_init_bSi_bury_coeff'
    lname     = 'initial scale factor for burial of bSi'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_init_bSi_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_Fe_scavenge_rate0'
    lname     = 'scavenging base rate for Fe'
    units     = '1/yr'
    datatype  = 'real'
    rptr      => parm_Fe_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_Lig_scavenge_rate0'
    lname     = 'scavenging base rate for bound ligand'
    units     = '1/yr'
    datatype  = 'real'
    rptr      => parm_Lig_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_FeLig_scavenge_rate0'
    lname     = 'scavenging base rate for bound iron'
    units     = '1/yr'
    datatype  = 'real'
    rptr      => parm_FeLig_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_Lig_degrade_rate0'
    lname     = 'Fe-binding ligand bacterial degradation rate coefficient'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_Lig_degrade_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_Fe_desorption_rate0'
    lname     = 'desorption rate for scavenged Fe from particles'
    units     = '1/cm'
    datatype  = 'real'
    rptr      => parm_Fe_desorption_rate0
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_f_prod_sp_CaCO3'
    lname     = 'Fraction of sp production as CaCO3 production'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => parm_f_prod_sp_CaCO3
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_POC_diss'
    lname     = 'base POC dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    rptr      => parm_POC_diss
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_SiO2_diss'
    lname     = 'base SiO2 dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    rptr      => parm_SiO2_diss
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_CaCO3_diss'
    lname     = 'base CaCO3 dissolution length scale'
    units     = 'cm'
    datatype  = 'real'
    rptr      => parm_CaCO3_diss
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_sed_denitrif_coeff'
    lname     = 'global scaling factor for sed_denitrif'
    units     = '1'
    datatype  = 'real'
    rptr      => parm_sed_denitrif_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'bury_coeff_rmean_timescale_years'
    lname     = 'Timescale for bury coefficient running means'
    units     = 'yr'
    datatype  = 'real'
    rptr      => bury_coeff_rmean_timescale_years
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    category  = 'Scale lengths'

    sname     = 'parm_scalelen_z'
    lname     = 'Depths of prescribed scale length values'
    units     = 'cm'
    call this%add_var_1d_r8(sname, lname, units, category,             &
                              parm_scalelen_z, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_r8', subname)
      return
    end if

    sname     = 'parm_scalelen_vals'
    lname     = 'Prescribed scale length values'
    units     = 'cm'
    call this%add_var_1d_r8(sname, lname, units, category,             &
                              parm_scalelen_vals, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_r8', subname)
      return
    end if

    category  = 'general parmeters'

    sname     = 'iron_frac_in_dust'
    lname     = 'Fraction by weight of iron in dust'
    units     = 'unitless (kg/kg)'
    datatype  = 'real'
    rptr      => iron_frac_in_dust
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'iron_frac_in_bc'
    lname     = 'Fraction by weight of iron in black carbon'
    units     = 'unitless (kg/kg)'
    datatype  = 'real'
    rptr      => iron_frac_in_bc
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'caco3_bury_thres_opt'
    lname     = 'Option for CaCO3 burial threshold'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => caco3_bury_thres_opt
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'caco3_bury_thres_depth'
    lname     = 'Threshold depth for CaCO3 burial (if using fixed_depth option)'
    units     = 'cm'
    datatype  = 'real'
    rptr      => caco3_bury_thres_depth
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'PON_bury_coeff'
    lname     = 'scale factor for burial of PON'
    units     = 'unitless'
    datatype  = 'real'
    rptr      => PON_bury_coeff
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ciso_fract_factors'
    lname     = 'Option for which biological fractionation calculation to use'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => ciso_fract_factors
    call this%add_var(sname, lname, units, datatype, category,       &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

  end subroutine marbl_settings_define_pre_tracers1

  !*****************************************************************************

  subroutine marbl_settings_define_pre_tracers2(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_define_pre_tracers2'
    character(len=char_len)     :: log_message

    character(len=char_len)          :: sname, lname, units, datatype, category
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    integer :: m, n, cnt
    character(len=char_len) :: prefix

    do n=1,autotroph_cnt
      write(prefix, "(A,I0,A)") 'autotrophs(', n, ')%'
      write(category, "(A,1X,I0)") 'autotroph', n

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      sptr     => autotrophs(n)%sname
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      sptr     => autotrophs(n)%lname
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'Nfixer'
      lname    = 'Flag is true if this autotroph fixes N2'
      units    = 'unitless'
      datatype = 'logical'
      lptr     => autotrophs(n)%Nfixer
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'imp_calcifier'
      lname    = 'Flag is true if this autotroph implicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      lptr     => autotrophs(n)%imp_calcifier
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'exp_calcifier'
      lname    = 'Flag is true if this autotroph explicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      lptr     => autotrophs(n)%exp_calcifier
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'silicifier'
      lname    = 'Flag is true if this autotroph is a silicifier'
      units    = 'unitless'
      datatype = 'logical'
      lptr     => autotrophs(n)%silicifier
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kFe'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotrophs(n)%kFe
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kPO4'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotrophs(n)%kPO4
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kDOP'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotrophs(n)%kDOP
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kNO3'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotrophs(n)%kNO3
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kNH4'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotrophs(n)%kNH4
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'kSiO3'
      lname    = 'nutrient uptake half-sat constants'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotrophs(n)%kSiO3
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'Qp_fixed'
      lname    = 'P/C ratio when using fixed P/C ratios'
      units    = 'unitless'
      datatype = 'real'
      rptr     => autotrophs(n)%Qp_fixed
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'gQfe_0'
      lname    = 'initial Fe/C ratio for growth'
      units    = 'unitless'
      datatype = 'real'
      rptr     => autotrophs(n)%gQFe_0
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'gQfe_min'
      lname    = 'minimum Fe/C ratio for growth'
      units    = 'unitless'
      datatype = 'real'
      rptr     => autotrophs(n)%gQFe_min
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'alphaPi_per_day'
      lname    = 'Initial slope of P_I curve (GD98)'
      units    = 'mmol C m^2 / (mg Chl W day)'
      datatype = 'real'
      rptr     => autotrophs(n)%alphaPi_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'PCref_per_day'
      lname    = 'max C-spec growth rate at Tref'
      units    = '1/day'
      datatype = 'real'
      rptr     => autotrophs(n)%PCref_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'thetaN_max'
      lname    = 'max thetaN (Chl/N)'
      units    = 'mg Chl / mmol N'
      datatype = 'real'
      rptr     => autotrophs(n)%thetaN_max
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres'
      lname    = 'concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotrophs(n)%loss_thres
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres2'
      lname    = 'concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => autotrophs(n)%loss_thres2
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'temp_thres'
      lname    = 'Temperature where concentration threshold and photosynthesis rate drop'
      units    = 'deg C'
      datatype = 'real'
      rptr     => autotrophs(n)%temp_thres
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'mort_per_day'
      lname    = 'linear mortality rate'
      units    = '1/day'
      datatype = 'real'
      rptr     => autotrophs(n)%mort_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'mort2_per_day'
      lname    = 'quadratic mortality rate'
      units    = '1/day/(mmol C/m^3)'
      datatype = 'real'
      rptr     => autotrophs(n)%mort2_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'agg_rate_max'
      lname    = 'Maximum agg rate'
      units    = '1/d'
      datatype = 'real'
      rptr     => autotrophs(n)%agg_rate_max
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'agg_rate_min'
      lname    = 'Minimum agg rate'
      units    = '1/d'
      datatype = 'real'
      rptr     => autotrophs(n)%agg_rate_min
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_poc'
      lname    = 'routing of loss term'
      units    = 'unitless'
      datatype = 'real'
      rptr     => autotrophs(n)%loss_poc
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

    do n=1, zooplankton_cnt
      write(prefix, "(A,I0,A)") 'zooplankton(', n, ')%'
      write(category, "(A,1X,I0)") 'zooplankton', n

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      sptr     => zooplankton(n)%sname
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      sptr     => zooplankton(n)%lname
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'z_mort_0_per_day'
      lname    = 'Linear mortality rate'
      units    = '1/day'
      datatype = 'real'
      rptr     => zooplankton(n)%z_mort_0_per_day
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'loss_thres'
      lname    = 'Concentration where losses go to zero'
      units    = 'nmol/cm^3'
      datatype = 'real'
      rptr     => zooplankton(n)%loss_thres
      call this%add_var(sname, lname, units, datatype, category,     &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'z_mort2_0_per_day'
      lname    = 'Quadratic mortality rate'
      units    = '1/day/(mmol C / m^3)'
      datatype = 'real'
      rptr     => zooplankton(n)%z_mort2_0_per_day
      call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

    do n=1,zooplankton_cnt
      do m=1,grazer_prey_cnt
        write(prefix, "(A,I0,A,I0,A)") 'grazing(', m, ',', n, ')%'
        write(category, "(A,1X,I0,1X,I0)") 'grazing', m, n

        write(sname, "(2A)") trim(prefix), 'sname'
        lname    = 'Short name of grazer'
        units    = 'unitless'
        datatype = 'string'
        sptr     => grazing(m,n)%sname
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, sptr=sptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'lname'
        lname    = 'Long name of grazer'
        units    = 'unitless'
        datatype = 'string'
        sptr     => grazing(m,n)%lname
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, sptr=sptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'auto_ind_cnt'
        lname    = 'number of autotrophs in prey-clase auto_ind'
        units    = 'unitless'
        datatype = 'integer'
        iptr     => grazing(m,n)%auto_ind_cnt
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, iptr=iptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'zoo_ind_cnt'
        lname    = 'number of zooplankton in prey-clase auto_ind'
        units    = 'unitless'
        datatype = 'integer'
        iptr     => grazing(m,n)%zoo_ind_cnt
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, iptr=iptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'grazing_function'
        lname    = 'functional form of grazing parmaeterization'
        units    = 'unitless'
        datatype = 'integer'
        iptr     => grazing(m,n)%grazing_function
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, iptr=iptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'z_umax_0_per_day'
        lname    = 'max zoo growth rate at Tref'
        units    = '1/day'
        datatype = 'real'
        rptr     => grazing(m,n)%z_umax_0_per_day
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'z_grz'
        lname    = 'Grazing coefficient'
        units    = '(mmol C/m^3)^2'
        datatype = 'real'
        rptr     => grazing(m,n)%z_grz
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'graze_zoo'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        rptr     => grazing(m,n)%graze_zoo
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'graze_poc'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        rptr     => grazing(m,n)%graze_poc
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'graze_doc'
        lname    = 'routing of grazed term (remainder goes to DIC)'
        units    = 'unitless'
        datatype = 'real'
        rptr     => grazing(m,n)%graze_doc
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'f_zoo_detr'
        lname    = 'Fraction of zoo losses to detrital'
        units    = 'unitless'
        datatype = 'real'
        rptr     => grazing(m,n)%f_zoo_detr
        call this%add_var(sname, lname, units, datatype, category,     &
                          marbl_status_log, rptr=rptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        cnt = grazing(m,n)%auto_ind_cnt
        if (cnt .gt. 0) then
          write(sname, "(2A)") trim(prefix), 'auto_ind'
          lname     = 'Indices of autotrophs in class'
          units     = 'unitless'
          call this%add_var_1d_int(sname, lname, units, category,      &
                            grazing(m,n)%auto_ind(1:cnt),              &
                            marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_status_log%log_error_trace('add_var_1d_int', subname)
            return
          end if
        end if

        cnt = grazing(m,n)%zoo_ind_cnt
        if (cnt .gt. 0) then
          write(sname, "(2A)") trim(prefix), 'zoo_ind'
          lname     = 'Indices of autotrophs in class'
          units     = 'unitless'
          call this%add_var_1d_int(sname, lname, units, category,      &
                                   grazing(m,n)%zoo_ind(1:cnt),        &
                                   marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_status_log%log_error_trace('add_var_1d_int', subname)
            return
          end if
        end if

      end do
    end do

  end subroutine marbl_settings_define_pre_tracers2

  !*****************************************************************************

  subroutine marbl_settings_define_post_tracers(this, marbl_status_log)

    class(marbl_settings_type), intent(inout) :: this
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_define_post_tracers'
    character(len=char_len) :: sname, lname, units, category

    category  = 'tracer restoring'

    sname     = 'tracer_restore_vars'
    lname     = 'Tracer names for tracers that are restored'
    units     = 'unitless'
    call this%add_var_1d_str(sname, lname, units, category,            &
                               tracer_restore_vars, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_str', subname)
      return
    end if

  end subroutine marbl_settings_define_post_tracers

  !*****************************************************************************

  subroutine marbl_settings_set_all_derived(marbl_status_log)

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

    call marbl_status_log%log_noerror('', subname)

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

    call marbl_status_log%log_noerror('', subname)

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

    call marbl_status_log%log_noerror('', subname)

    do n = 1, zooplankton_cnt
       do m = 1, grazer_prey_cnt
          grazing(m,n)%z_umax_0 = dps * grazing(m,n)%z_umax_0_per_day
          write(sname_in,  "(A,I0,A,I0,A)") 'grazing(', m, ',', n, ')%z_umax_0_per_day'
          write(sname_out, "(A,I0,A,I0,A)") 'grazing(', m, ',', n, ')%z_umax_0'
          call print_single_derived_parm(sname_in, sname_out, &
               grazing(m,n)%z_umax_0, subname, marbl_status_log)
       end do
    end do

  end subroutine marbl_settings_set_all_derived

  !*****************************************************************************

  subroutine marbl_settings_string_to_var(value, marbl_status_log, rval, ival, lval, sval)

    use ieee_arithmetic, only : ieee_is_nan

    character(len=*),            intent(in)    :: value
    type(marbl_log_type),        intent(inout) :: marbl_status_log
    real(r8),          optional, intent(out)   :: rval
    integer(int_kind), optional, intent(out)   :: ival
    logical(log_kind), optional, intent(out)   :: lval
    character(len=*),  optional, intent(out)   :: sval

    character(len=*), parameter :: subname = 'marbl_settings_mod:marbl_settings_string_to_var'
    character(len=char_len)     :: log_message
    integer :: ioerr, last_char

    ! Real value requested?
    if (present(rval)) then
      read(value, *, iostat=ioerr) rval
      if ((ioerr .ne. 0) .or. (ieee_is_nan(rval))) then
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
      last_char = len_trim(value)
      if (last_char .eq. 0) then
        log_message = "Empty string is not acceptable"
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      if ((value(1:1) .ne. '"') .and. (value(1:1) .ne. "'")) then
        write(log_message,"(3A)") "String value must be in quotes ", &
              trim(value), " is not acceptable"
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      if (value(1:1) .ne. value(last_char:last_char)) then
        write(log_message,"(3A)") "String value must be in quotes ", &
              trim(value), " is not acceptable"
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
      sval = value(2:last_char-1)
    end if

  end subroutine marbl_settings_string_to_var

!*****************************************************************************

  subroutine add_var(this, sname, lname, units, datatype, category,    &
                     marbl_status_log, rptr, iptr, lptr, sptr, comment)

    class(marbl_settings_type),                 intent(inout) :: this
    character(len=*),                           intent(in)    :: sname
    character(len=*),                           intent(in)    :: lname
    character(len=*),                           intent(in)    :: units
    character(len=*),                           intent(in)    :: datatype
    character(len=*),                           intent(in)    :: category
    type(marbl_log_type),                       intent(inout) :: marbl_status_log
    real(r8),                optional, pointer, intent(in) :: rptr
    integer,                 optional, pointer, intent(in) :: iptr
    logical,                 optional, pointer, intent(in) :: lptr
    character(len=char_len), optional, pointer, intent(in) :: sptr
    character(len=char_len), optional,          intent(in) :: comment

    character(len=*), parameter :: subname = 'marbl_settings_mod:add_var'

    type(marbl_single_setting_ll_type), pointer :: new_entry, ll_ptr, ll_prev
    character(len=char_len), dimension(:), pointer :: new_categories
    integer :: cat_ind, n
    character(len=char_len) :: log_message, alternate_sname
    logical :: put_success

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
    do while (associated(ll_ptr))
      if (case_insensitive_eq(ll_ptr%short_name, new_entry%short_name) .or. &
          case_insensitive_eq(ll_ptr%short_name, alternate_sname)) then
        ! 5a) Update variable value
        if (trim(ll_ptr%datatype) .eq. "unknown") then
          select case (new_entry%datatype)
            case ("real")
              call marbl_settings_string_to_var(ll_ptr%sptr, marbl_status_log, rval = new_entry%rptr)
            case ("integer")
              call marbl_settings_string_to_var(ll_ptr%sptr, marbl_status_log, ival = new_entry%iptr)
            case ("string")
              call marbl_settings_string_to_var(ll_ptr%sptr, marbl_status_log, sval = new_entry%sptr)
            case ("logical")
              call marbl_settings_string_to_var(ll_ptr%sptr, marbl_status_log, lval = new_entry%lptr)
          end select
          if (marbl_status_log%labort_marbl) then
            call marbl_status_log%log_error_trace('marbl_settings_string_to_var', subname)
            return
          end if
        else
          if (associated(ll_ptr%iptr).and.associated(new_entry%rptr)) then
            new_entry%rptr = real(ll_ptr%iptr,r8)
          else
            select case (new_entry%datatype)
              case ("real")
                put_success = associated(ll_ptr%rptr)
                if (put_success) new_entry%rptr = ll_ptr%rptr
              case ("integer")
                put_success = associated(ll_ptr%iptr)
                if (put_success) new_entry%iptr = ll_ptr%iptr
              case ("string")
                put_success = associated(ll_ptr%sptr)
                if (put_success) new_entry%sptr = ll_ptr%sptr
              case ("logical")
                put_success = associated(ll_ptr%lptr)
                if (put_success) new_entry%lptr = ll_ptr%lptr
            end select
            ! Abort if the put() failed
            if (.not.put_success) then
              write(log_message, "(6A)") "put_setting(", trim(ll_ptr%short_name), &
                                 ") failed because the datatype was incorrect; expecting ", &
                                 trim(new_entry%datatype), " but user provided ", &
                                 trim(ll_ptr%datatype)
              call marbl_status_log%log_error(log_message, subname)
            end if
          end if
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
        ! Once we are past first entry, ll_prev%next => ll_ptr
        ll_prev => ll_ptr
        ll_ptr => ll_ptr%next
      end if
    end do

    ! 6) Increment count
    this%cnt = this%cnt + 1

  end subroutine add_var

  !*****************************************************************************

  subroutine add_var_1d_r8(this, sname, lname, units, category,        &
                           r8array, marbl_status_log)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: category
    real(kind=r8), dimension(:), target, intent(in)    :: r8array
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:add_var_1d_r8'

    character(len=char_len) :: sname_loc
    real(r8), pointer       :: rptr => NULL()
    integer                 :: n

    do n=1,size(r8array)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      rptr => r8array(n)
      call this%add_var(sname_loc, lname, units, 'real', category,     &
                          marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine add_var_1d_r8

  !*****************************************************************************

  subroutine add_var_1d_int(this, sname, lname, units, category,       &
                            intarray, marbl_status_log)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: category
    integer, dimension(:), target,       intent(in)    :: intarray
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:add_var_1d_int'

    character(len=char_len) :: sname_loc
    integer, pointer        :: iptr => NULL()
    integer                 :: n

    do n=1,size(intarray)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      iptr => intarray(n)
      call this%add_var(sname_loc, lname, units, 'integer', category,  &
                          marbl_status_log, iptr=iptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine add_var_1d_int

  !*****************************************************************************

  subroutine add_var_1d_str(this, sname, lname, units, category,       &
                            strarray, marbl_status_log)

    class(marbl_settings_type),          intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: category
    character(len=char_len),     target, intent(in)    :: strarray(:)
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_settings_mod:add_var_1d_str'

    character(len=char_len)          :: sname_loc
    character(len=char_len), pointer :: sptr => NULL()
    integer                          :: n

    do n=1,size(strarray)
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      sptr => strarray(n)
      call this%add_var(sname_loc, lname, units, 'string', category,   &
                          marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

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
              write(log_message, "(2A,E24.16)") trim(ll_ptr%short_name),   &
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

    type(marbl_single_setting_ll_type), pointer :: new_entry, ll_ptr
    character(len=*), parameter :: subname = 'marbl_settings_mod:put'
    character(len=char_len) :: log_message

    call marbl_status_log%construct()
    if (this%init_called) then
      write(log_message, "(3A)") "Can not put ", trim(var), ", init has already been called"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    allocate(new_entry)
    new_entry%short_name = var

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

    character(len=*), parameter :: subname = 'marbl_settings_mod%get'
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

  subroutine inquire_metadata(this, id, marbl_status_log, sname, lname, units, &
                              datatype)

    class(marbl_settings_type), intent(in)    :: this
    integer(int_kind),          intent(in)    :: id
    type(marbl_log_type),       intent(inout) :: marbl_status_log
    character(len=*), optional, intent(out)   :: sname, lname, units
    character(len=*), optional, intent(out)   :: datatype

    character(len=*), parameter :: subname = 'marbl_settings_mod:inquire_metadata'
    character(len=char_len)     :: log_message

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

  subroutine log_add_var_error(marbl_status_log, sname, subname)

    type(marbl_log_type), intent(inout) :: marbl_status_log
    character(len=*),     intent(in)    :: sname
    character(len=*),     intent(in)    :: subname
    character(len=char_len) :: routine_name

    write(routine_name,"(3A)") "this%add_var(", trim(sname), ")"
    call marbl_status_log%log_error_trace(routine_name, subname)

  end subroutine log_add_var_error

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

    write(log_message, "(2A,E24.16,3A)") &
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
