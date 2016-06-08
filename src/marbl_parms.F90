module marbl_parms

  !-----------------------------------------------------------------------------
  !   This module manages BGC-specific parameters.
  !
  !   Most of the variables are not parameters in the Fortran sense. In the
  !   the Fortran sense, they are vanilla module variables associated with one
  !   of the four MARBL namelists (marbl_ecosys_base_nml, marbl_parms_nml,
  !   marbl_ciso_nml, and marbl_restore_nml)
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

  use marbl_config_mod, only : marbl_single_config_var_type
  use marbl_config_mod, only : marbl_config_vars_type
  use marbl_config_mod, only : log_add_var_error

  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : dps

  use marbl_internal_types, only : grazing_type

  use marbl_interface_types, only : marbl_tracer_read_type
  use marbl_interface_types, only : marbl_forcing_monthly_every_ts_type

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt
  use marbl_sizes, only : grazer_prey_cnt
  use marbl_sizes, only : ecosys_base_tracer_cnt
  use marbl_sizes, only : ciso_tracer_cnt

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
       parm_Fe_bioavail,      & ! fraction of Fe flux that is bioavailable
       parm_o2_min,           & ! min O2 needed for prod & consump. (nmol/cm^3)
       parm_o2_min_delta,     & ! width of min O2 range (nmol/cm^3)
       parm_kappa_nitrif,     & ! nitrification inverse time constant (1/sec)
       parm_nitrif_par_lim,   & ! PAR limit for nitrif. (W/m^2)
       parm_labile_ratio,     & ! fraction of loss to DOC that routed directly to DIC (non-dimensional)
       parm_POMbury,          & ! scale factor for burial of POC, PON, and POP
       parm_BSIbury,          & ! scale factor burial of bSi
       parm_fe_scavenge_rate0,& ! base scavenging rate (1/yr)
       parm_f_prod_sp_CaCO3,  & ! fraction of sp prod. as CaCO3 prod.
       parm_POC_diss,         & ! base POC diss len scale (cm)
       parm_SiO2_diss,        & ! base SiO2 diss len scale (cm)
       parm_CaCO3_diss,       & ! base CaCO3 diss len scale (cm)
       fe_max_scale2            ! scaling coeff. (cm^3 / nmol / yr = m^3 / mmol / yr)

  real(kind=r8), dimension(4) :: &
       parm_scalelen_z,       & ! depths of prescribed scalelen values
       parm_scalelen_vals       ! prescribed scalelen values

  type(grazing_type)     :: grazing(grazer_prey_cnt, zooplankton_cnt)
  !---------------------------------------------------------------------
  !  Variables read in via &marbl_ecosys_base_nml
  !---------------------------------------------------------------------

  character(char_len), target :: init_ecosys_option           ! namelist option for initialization of bgc
  character(char_len), target :: init_ecosys_init_file        ! filename for option 'file'
  character(char_len), target :: init_ecosys_init_file_fmt    ! file format for option 'file'
  type(marbl_tracer_read_type) :: tracer_init_ext(ecosys_base_tracer_cnt) ! namelist variable for initializing tracers
  real(r8),            target :: iron_frac_in_dust            ! fraction by weight of iron in dust
  real(r8),            target :: iron_frac_in_bc              ! fraction by weight of iron in black carbon
  character(char_len), target :: caco3_bury_thres_opt         ! option of threshold of caco3 burial ['fixed_depth', 'omega_calc']
  real(r8),            target :: caco3_bury_thres_depth       ! threshold depth for caco3_bury_thres_opt='fixed_depth'
  ! -----------
  ! PON_sed_loss = PON_bury_coeff * Q * POC_sed_loss
  ! factor is used to avoid overburying PON like POC
  ! is when total C burial is matched to C riverine input
  ! -----------
  real(r8),            target :: PON_bury_coeff
  ! -----------
  ! POP_sed_loss = POP_bury_coeff * Qp_zoo_pom * POC_sed_loss
  ! factor is used to enable forced closure of the P cycle
  ! i.e. POP_sed_loss = P inputs (riverine + atm dep)
  ! -----------
  real(r8),            target :: POP_bury_coeff
  logical(log_kind),   target :: lnutr_variable_restore      ! geographically varying nutrient restoring (maltrud)
  character(char_len), target :: nutr_rest_file              ! file containing nutrient fields
  character(char_len), target :: nutr_variable_rest_file     ! file containing variable restoring info
  character(char_len), target :: nutr_variable_rest_file_fmt ! format of file containing variable restoring info

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_ciso_nml
  !---------------------------------------------------------------------

  character(char_len), target :: ciso_init_ecosys_option        ! option for initialization of bgc
  character(char_len), target :: ciso_init_ecosys_init_file     ! filename for option 'file'
  character(char_len), target :: ciso_init_ecosys_init_file_fmt ! file format for option 'file'
  type(marbl_tracer_read_type) :: ciso_tracer_init_ext(ciso_tracer_cnt) ! namelist variable for initializing tracers
  character(char_len), target :: ciso_fract_factors             ! option for which biological fractionation calculation to use

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_restore_nml
  !---------------------------------------------------------------------

  character(len=char_len), allocatable, target, dimension(:) :: restore_short_names, &
                                                        restore_filenames,   &
                                                        restore_file_varnames
  real(r8), target :: rest_time_inv_surf, rest_time_inv_deep, rest_z0, rest_z1

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_forcing_tmp_nml
  !---------------------------------------------------------------------

  character(char_len),  target :: dust_flux_source             ! option for atmospheric dust deposition
  type(marbl_tracer_read_type) :: dust_flux_input             ! namelist input for dust_flux
  character(char_len),  target :: iron_flux_source             ! option for atmospheric iron deposition
  type(marbl_tracer_read_type) :: iron_flux_input             ! namelist input for iron_flux
  type(marbl_tracer_read_type) :: fesedflux_input                    ! namelist input for iron_flux
  character(char_len),  target :: ndep_data_type               ! type of ndep forcing
  type(marbl_tracer_read_type) :: nox_flux_monthly_input      ! namelist input for nox_flux_monthly
  type(marbl_tracer_read_type) :: nhy_flux_monthly_input      ! namelist input for nhy_flux_monthly
  integer(int_kind),    target :: ndep_shr_stream_year_first   ! first year in stream to use
  integer(int_kind),    target :: ndep_shr_stream_year_last    ! last year in stream to use
  integer(int_kind),    target :: ndep_shr_stream_year_align   ! align ndep_shr_stream_year_first with this model year
  character(char_len),  target :: ndep_shr_stream_file         ! file containing domain and input data
  real(r8),             target :: ndep_shr_stream_scale_factor ! unit conversion factor
  type(marbl_tracer_read_type) :: din_riv_flux_input          ! namelist input for din_riv_flux
  type(marbl_tracer_read_type) :: dip_riv_flux_input          ! namelist input for dip_riv_flux
  type(marbl_tracer_read_type) :: don_riv_flux_input          ! namelist input for don_riv_flux
  type(marbl_tracer_read_type) :: dop_riv_flux_input          ! namelist input for dop_riv_flux
  type(marbl_tracer_read_type) :: dsi_riv_flux_input          ! namelist input for dsi_riv_flux
  type(marbl_tracer_read_type) :: dfe_riv_flux_input          ! namelist input for dfe_riv_flux
  type(marbl_tracer_read_type) :: dic_riv_flux_input          ! namelist input for dic_riv_flux
  type(marbl_tracer_read_type) :: alk_riv_flux_input          ! namelist input for alk_riv_flux
  type(marbl_tracer_read_type) :: doc_riv_flux_input          ! namelist input for doc_riv_flux
  character(char_len)          :: gas_flux_forcing_opt        ! option for forcing gas fluxes
  character(char_len),  target :: gas_flux_forcing_file        ! file containing gas flux forcing fields
  type(marbl_tracer_read_type) :: gas_flux_fice               ! ice fraction for gas fluxes
  type(marbl_tracer_read_type) :: gas_flux_ws                 ! wind speed for gas fluxes
  type(marbl_tracer_read_type) :: gas_flux_ap                 ! atmospheric pressure for gas fluxes
  character(char_len)          :: atm_co2_opt                 ! option for atmospheric co2 concentration
  real(r8),             target :: atm_co2_const                ! value of atmospheric co2 (ppm, dry-air, 1 atm)
  character(char_len)          :: atm_alt_co2_opt             ! option for atmospheric alternative CO2
  real(r8),             target :: atm_alt_co2_const            ! value of atmospheric alternative co2 (ppm, dry-air, 1 atm)
  logical(log_kind),    target :: liron_patch                  ! flag for iron patch fertilization
  character(char_len),  target :: iron_patch_flux_filename     ! file containing name of iron patch file
  integer(int_kind),    target :: iron_patch_month             ! integer month to add patch flux
  integer(int_kind)            :: ciso_atm_model_year            ! arbitrary model year
  integer(int_kind)            :: ciso_atm_data_year             ! year in atmospheric ciso data that corresponds to ciso_atm_model_year
  integer(int_kind)            :: ciso_atm_d13c_data_nbval       ! number of values in ciso_atm_d13c_filename
  integer(int_kind)            :: ciso_atm_d14c_data_nbval       ! number of values in ciso_atm_d14c_filename
  real(r8), allocatable        :: ciso_atm_d13c_data(:)          ! atmospheric D13C values in datafile
  real(r8), allocatable        :: ciso_atm_d13c_data_yr(:)       ! date of atmospheric D13C values in datafile
  real(r8), allocatable        :: ciso_atm_d14c_data(:,:)        ! atmospheric D14C values in datafile (sh, eq, nh, in permil)
  real(r8), allocatable        :: ciso_atm_d14c_data_yr(:,:)     ! date of atmospheric D14C values in datafile (sh, eq, nh)
  real(r8)                     :: ciso_atm_d13c_const            ! atmospheric D13C constant [permil]
  real(r8)                     :: ciso_atm_d14c_const            ! atmospheric D14C constant [permil]
  character(char_len)          :: ciso_atm_d13c_opt              ! option for CO2 and D13C varying or constant forcing
  character(char_len)          :: ciso_atm_d13c_filename         ! filenames for varying atm D13C
  character(char_len)          :: ciso_atm_d14c_opt              ! option for CO2 and D13C varying or constant forcing
  character(char_len)          :: ciso_atm_d14c_filename(3)      ! filenames for varying atm D14C (one each for NH, SH, EQ)

  !---------------------------------------------------------------------
  !  Datatype for accessing parameters without namelist
  !---------------------------------------------------------------------

  type, extends(marbl_config_vars_type), public :: marbl_parms_type
  contains
    procedure :: construct        => marbl_parms_construct
  end type marbl_parms_type

  !---------------------------------------------------------------------
  !  BGC parameters that are not part of marbl_parms_nml
  !---------------------------------------------------------------------

  ! Redfield Ratios, dissolved & particulate
  real(kind=r8), parameter :: &
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
       fe_scavenge_thres1 = 0.8e-3_r8,  & !upper thres. for Fe scavenging
       dust_fescav_scale  = 1.0e9         !dust scavenging scale factor

  ! Compute iron remineralization and flux out.
  ! dust remin gDust = 0.035 gFe      mol Fe     1e9 nmolFe
  !                    --------- *  ---------- * ----------
  !                      gDust      55.847 gFe     molFe
  !
  ! dust_to_Fe          conversion - dust to iron (nmol Fe/g Dust)
  real(kind=r8), parameter :: &
       dust_to_Fe=0.035_r8/55.847_r8*1.0e9_r8

  ! Partitioning of phytoplankton growth, grazing and losses
  ! All f_* variables are fractions and are non-dimensional
  real(kind=r8), parameter :: &
      caco3_poc_min         = 0.40_r8,  & ! minimum proportionality between
                                          !   QCaCO3 and grazing losses to POC
                                          !   (mmol C/mmol CaCO3)
      spc_poc_fac           = 0.10_r8,  & ! small phyto grazing factor (1/mmolC)
      f_graze_sp_poc_lim    = 0.40_r8,  &
      f_photosp_CaCO3       = 0.40_r8,  & ! proportionality between small phyto
                                          !    production and CaCO3 production
      f_graze_CaCO3_remin   = 0.33_r8,  & ! fraction of spCaCO3 grazing which is remin
      f_graze_si_remin      = 0.50_r8,  & ! fraction of diatom Si grazing which is remin
      f_toDON               = 0.66_r8     ! fraction lower to DOM for DON

  ! fixed ratios
  real(kind=r8), parameter :: &
       r_Nfix_photo=1.25_r8         ! N fix relative to C fix (non-dim)

  ! SET FIXED RATIOS for N/C, P/C, SiO3/C, Fe/C
  ! assumes C/N/P of 117/16/1 based on Anderson and Sarmiento, 1994
  ! for diazotrophs a N/P of 45 is assumed based on Letelier & Karl, 1998
  real(kind=r8), parameter :: &
      Q             = 0.137_r8,  & !N/C ratio (mmol/mmol) of phyto & zoo
      Qp_zoo_pom    = 0.00855_r8,& !P/C ratio (mmol/mmol) zoo & pom
      Qfe_zoo       = 3.0e-6_r8, & !zooplankton fe/C ratio
      gQsi_0        = 0.137_r8,  & !initial Si/C ratio
      gQsi_max      = 0.685_r8,  & !max Si/C ratio
      gQsi_min      = 0.0457_r8, & !min Si/C ratio
      QCaCO3_max    = 0.4_r8,    & !max QCaCO3
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
       POPremin_refract = DOPprod_refract * 0.06_r8, & ! fraction of POCremin to refractory pool
       DOCriv_refract   = 0.2_r8,                    & ! fraction of DOC river input to refractory pool
       DONriv_refract   = 0.1_r8,                    & ! fraction of DON river input to refractory pool
       DOPriv_refract   = 0.025_r8                     ! fraction of DOP river input to refractory pool

  !---------------------------------------------------------------------
  !  Variables that don't match namelist read
  !---------------------------------------------------------------------

  type(marbl_forcing_monthly_every_ts_type), pointer :: fice_file             ! ice fraction, if read from file
  type(marbl_forcing_monthly_every_ts_type), pointer :: xkw_file              ! a * wind-speed ** 2, if read from file
  type(marbl_forcing_monthly_every_ts_type), pointer :: ap_file               ! atmoshperic pressure, if read from file
  type(marbl_forcing_monthly_every_ts_type), pointer :: dust_flux_file        ! surface dust flux
  type(marbl_forcing_monthly_every_ts_type), pointer :: iron_flux_file        ! iron component of surface dust flux
  type(marbl_forcing_monthly_every_ts_type), pointer :: nox_flux_monthly_file ! surface NOx species flux, added to nitrate pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: nhy_flux_monthly_file ! surface NHy species flux, added to ammonium pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: din_riv_flux_file     ! river DIN species flux, added to nitrate pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dip_riv_flux_file     ! river DIP species flux, added to phosphate pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: don_riv_flux_file     ! river DON flux, added to semi-lab don pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dop_riv_flux_file     ! river DOP flux, added to semi-lab dop pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dsi_riv_flux_file     ! river DSI flux, added to dsi pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dfe_riv_flux_file     ! river dfe flux, added to dfe pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dic_riv_flux_file     ! river dic flux, added to dic pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: alk_riv_flux_file     ! river alk flux, added to alk pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: doc_riv_flux_file     ! river doc flux, added to semi-labile DOC

  !---------------------------------------------------------------------
  !  Auxiliary variables (str -> int conversions, indices, etc)
  !---------------------------------------------------------------------

  integer (int_kind)            :: caco3_bury_thres_iopt
  integer (int_kind), parameter :: caco3_bury_thres_iopt_fixed_depth = 1
  integer (int_kind), parameter :: caco3_bury_thres_iopt_omega_calc  = 2

  integer (int_kind), parameter :: marbl_freq_opt_never    = 0
  integer (int_kind), parameter :: marbl_freq_opt_nyear    = 1
  integer (int_kind), parameter :: marbl_freq_opt_nmonth   = 2

  real(r8), dimension(:), allocatable :: inv_tau

  integer(int_kind)             :: gas_flux_forcing_iopt
  integer (int_kind), parameter :: gas_flux_forcing_iopt_drv  = 1
  integer (int_kind), parameter :: gas_flux_forcing_iopt_file = 2

  integer(int_kind)             :: atm_co2_iopt
  integer(int_kind)             :: atm_alt_co2_iopt
  integer (int_kind), parameter :: atm_co2_iopt_const         = 1
  integer (int_kind), parameter :: atm_co2_iopt_drv_prog      = 2
  integer (int_kind), parameter :: atm_co2_iopt_drv_diag      = 3

  integer (kind=int_kind), parameter :: sp_ind   = 1  ! small phytoplankton
  integer (kind=int_kind), parameter :: diat_ind = 2  ! diatoms
  integer (kind=int_kind), parameter :: diaz_ind = 3  ! diazotrophs

  ! grazing functions
  integer (kind=int_kind), parameter ::           &
         grz_fnc_michaelis_menten = 1,       &
         grz_fnc_sigmoidal        = 2

  !-----------------------------------------------------------------------
  !  input surface forcing
  !-----------------------------------------------------------------------
  ! FIXME #56 : move this option, and corresponding code to driver when
  ! surface forcing source is selected in driver, instead of MARBL

  logical (log_kind) :: liron_flux_derived
  type(marbl_forcing_monthly_every_ts_type), target :: dust_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: iron_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: fice_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: xkw_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: ap_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: nox_flux_monthly_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: nhy_flux_monthly_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: din_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dip_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: don_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dop_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dsi_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dfe_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dic_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: alk_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: doc_riv_flux_file_loc

  !*****************************************************************************

  public :: &
       marbl_parms_read_namelist, &
       marbl_parms_set_defaults

  ! Variables used from other modules should be private
  ! (So we don't accidentally use them from this module)
  private :: r8, int_kind, log_kind, char_len
  private :: c1, dps
  private :: grazing_type
  private :: autotroph_cnt, zooplankton_cnt, grazer_prey_cnt
  private :: marbl_log_type

contains

  !*****************************************************************************

  subroutine marbl_parms_set_defaults(km)
    ! assign default values to all module variables

    use marbl_constants_mod   , only : c0, c2, c1000
    use marbl_sizes           , only : marbl_total_tracer_cnt
    use marbl_living_parms_mod, only : autotrophs
    use marbl_living_parms_mod, only : zooplankton

    integer,              intent(in)    :: km ! max number of levels

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    integer :: auto_ind, zoo_ind, prey_ind, n
    !---------------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  &marbl_parms_nml
    !-----------------------------------------------------------------------

    parm_Fe_bioavail       = 1.0_r8
    parm_o2_min            = 5.0_r8
    parm_o2_min_delta      = 5.0_r8
    parm_kappa_nitrif      = 0.06_r8 * dps  ! (= 1/( days))
    parm_nitrif_par_lim    = 1.0_r8
    parm_labile_ratio      = 0.94_r8
    parm_POMbury           = 1.1_r8         ! x1 default
    parm_BSIbury           = 1.0_r8         ! x1 default
    parm_fe_scavenge_rate0 = 1.9_r8         ! x1 default
    parm_f_prod_sp_CaCO3   = 0.065_r8       ! x1 default
    parm_POC_diss          = 90.0e2_r8
    parm_SiO2_diss         = 600.0e2_r8
    parm_CaCO3_diss        = 450.0e2_r8
    fe_max_scale2          = 2200.0_r8      ! x1 default
    parm_scalelen_z    = (/ 100.0e2_r8, 250.0e2_r8, 500.0e2_r8,  750.0e2_r8 /)
    parm_scalelen_vals = (/     1.0_r8,     3.3_r8,     4.6_r8,      5.0_r8 /) ! x1 default

    ! predator-prey relationships
    zoo_ind = 1
    prey_ind = sp_ind
    grazing(prey_ind,zoo_ind)%sname            = 'grz_' // autotrophs(prey_ind)%sname // '_' // zooplankton(zoo_ind)%sname
    grazing(prey_ind,zoo_ind)%lname            = 'Grazing of ' // autotrophs(prey_ind)%sname // ' by ' // zooplankton(zoo_ind)%sname
    grazing(prey_ind,zoo_ind)%auto_ind(1)      = prey_ind
    grazing(prey_ind,zoo_ind)%auto_ind_cnt     = 1
    grazing(prey_ind,zoo_ind)%zoo_ind          = -1
    grazing(prey_ind,zoo_ind)%zoo_ind_cnt      = 0
    grazing(prey_ind,zoo_ind)%z_umax_0         = 3.25_r8 * dps
    grazing(prey_ind,zoo_ind)%z_grz            = 1.15_r8
    grazing(prey_ind,zoo_ind)%graze_zoo        = 0.3_r8
    grazing(prey_ind,zoo_ind)%graze_poc        = 0.0_r8
    grazing(prey_ind,zoo_ind)%graze_doc        = 0.06_r8
    grazing(prey_ind,zoo_ind)%f_zoo_detr       = 0.1_r8
    grazing(prey_ind,zoo_ind)%grazing_function = grz_fnc_michaelis_menten

    prey_ind = diat_ind
    grazing(prey_ind,zoo_ind)%sname            = 'grz_' // autotrophs(prey_ind)%sname // '_' // zooplankton(zoo_ind)%sname
    grazing(prey_ind,zoo_ind)%lname            = 'Grazing of ' // autotrophs(prey_ind)%sname // ' by ' // zooplankton(zoo_ind)%sname
    grazing(prey_ind,zoo_ind)%auto_ind(1)      = prey_ind
    grazing(prey_ind,zoo_ind)%auto_ind_cnt     = 1
    grazing(prey_ind,zoo_ind)%zoo_ind          = -1
    grazing(prey_ind,zoo_ind)%zoo_ind_cnt      = 0
    grazing(prey_ind,zoo_ind)%z_umax_0         = 2.9_r8 * dps
    grazing(prey_ind,zoo_ind)%z_grz            = 1.15_r8
    grazing(prey_ind,zoo_ind)%graze_zoo        = 0.3_r8
    grazing(prey_ind,zoo_ind)%graze_poc        = 0.4_r8
    grazing(prey_ind,zoo_ind)%graze_doc        = 0.06_r8
    grazing(prey_ind,zoo_ind)%f_zoo_detr       = 0.2_r8
    grazing(prey_ind,zoo_ind)%grazing_function = grz_fnc_michaelis_menten

    prey_ind = diaz_ind
    grazing(prey_ind,zoo_ind)%sname            = 'grz_' // autotrophs(prey_ind)%sname // '_' // zooplankton(zoo_ind)%sname
    grazing(prey_ind,zoo_ind)%lname            = 'Grazing of ' // autotrophs(prey_ind)%sname // ' by ' // zooplankton(zoo_ind)%sname
    grazing(prey_ind,zoo_ind)%auto_ind(1)      = prey_ind
    grazing(prey_ind,zoo_ind)%auto_ind_cnt     = 1
    grazing(prey_ind,zoo_ind)%zoo_ind          = -1
    grazing(prey_ind,zoo_ind)%zoo_ind_cnt      = 0
    grazing(prey_ind,zoo_ind)%z_umax_0         = 1.85_r8 * dps
    grazing(prey_ind,zoo_ind)%z_grz            = 1.15_r8
    grazing(prey_ind,zoo_ind)%graze_zoo        = 0.3_r8
    grazing(prey_ind,zoo_ind)%graze_poc        = 0.08_r8
    grazing(prey_ind,zoo_ind)%graze_doc        = 0.06_r8
    grazing(prey_ind,zoo_ind)%f_zoo_detr       = 0.1_r8
    grazing(prey_ind,zoo_ind)%grazing_function = grz_fnc_michaelis_menten

    !-----------------------------------------------------------------------
    !  &marbl_ecosys_base_nml
    !-----------------------------------------------------------------------

    init_ecosys_option = 'unknown'
    init_ecosys_init_file = 'unknown'
    init_ecosys_init_file_fmt = 'bin'
    iron_frac_in_dust            = 0.035_r8 * 0.01_r8
    iron_frac_in_bc              = 0.06_r8
    caco3_bury_thres_opt = 'omega_calc'
    caco3_bury_thres_depth = 3000.0e2
    PON_bury_coeff = 0.5_r8
    POP_bury_coeff = 1.0_r8
    do n = 1, ecosys_base_tracer_cnt
       tracer_init_ext(n)%mod_varname  = 'unknown'
       tracer_init_ext(n)%filename     = 'unknown'
       tracer_init_ext(n)%file_varname = 'unknown'
       tracer_init_ext(n)%scale_factor = c1
       tracer_init_ext(n)%default_val  = c0
       tracer_init_ext(n)%file_fmt     = 'bin'
    end do

    ! MNL MNL TODO: are these ever used?
    nutr_rest_file = 'unknown'
    lnutr_variable_restore      = .false.
    nutr_variable_rest_file     = 'unknown'
    nutr_variable_rest_file_fmt = 'bin'

    !-----------------------------------------------------------------------
    !  &marbl_ciso_nml
    !-----------------------------------------------------------------------

    ciso_init_ecosys_option                 = 'unknown'
    ciso_init_ecosys_init_file              = 'unknown'
    ciso_init_ecosys_init_file_fmt          = 'bin'
    ciso_fract_factors                      = 'Rau'
    do n = 1,ciso_tracer_cnt
       ciso_tracer_init_ext(n)%mod_varname  = 'unknown'
       ciso_tracer_init_ext(n)%filename     = 'unknown'
       ciso_tracer_init_ext(n)%file_varname = 'unknown'
       ciso_tracer_init_ext(n)%scale_factor = c1
       ciso_tracer_init_ext(n)%default_val  = c0
       ciso_tracer_init_ext(n)%file_fmt     = 'bin'
    end do

    !-----------------------------------------------------------------------
    !  &marbl_restore_nml
    !-----------------------------------------------------------------------

    ! FIXME #69: not thread-safe!
    if (.not.allocated(inv_tau)) &
      allocate(inv_tau(km))
    if (.not.allocated(restore_short_names)) &
      allocate(restore_short_names(marbl_total_tracer_cnt))
    if (.not.allocated(restore_filenames)) &
      allocate(restore_filenames(marbl_total_tracer_cnt))
    if (.not.allocated(restore_file_varnames)) &
      allocate(restore_file_varnames(marbl_total_tracer_cnt))

    ! initialize namelist variables to default values
    restore_short_names = ''
    restore_filenames = ''
    restore_file_varnames = ''

    rest_time_inv_surf = c0
    rest_time_inv_deep = c0
    rest_z0 = c1000
    rest_z1 = c2*c1000

    !-----------------------------------------------------------------------
    !  &marbl_forcing_tmp_nml
    !-----------------------------------------------------------------------

    gas_flux_forcing_opt  = 'drv'
    gas_flux_forcing_file = 'unknown'
    gas_flux_fice%filename     = 'unknown'
    gas_flux_fice%file_varname = 'FICE'
    gas_flux_fice%scale_factor = c1
    gas_flux_fice%default_val  = c0
    gas_flux_fice%file_fmt     = 'bin'
    gas_flux_ws%filename     = 'unknown'
    gas_flux_ws%file_varname = 'XKW'
    gas_flux_ws%scale_factor = c1
    gas_flux_ws%default_val  = c0
    gas_flux_ws%file_fmt     = 'bin'
    gas_flux_ap%filename     = 'unknown'
    gas_flux_ap%file_varname = 'P'
    gas_flux_ap%scale_factor = c1
    gas_flux_ap%default_val  = c0
    gas_flux_ap%file_fmt     = 'bin'
    dust_flux_source             = 'monthly-calendar'
    dust_flux_input%filename     = 'unknown'
    dust_flux_input%file_varname = 'dust_flux'
    dust_flux_input%scale_factor = c1
    dust_flux_input%default_val  = c0
    dust_flux_input%file_fmt     = 'bin'
    iron_flux_source             = 'monthly-calendar'
    iron_flux_input%filename     = 'unknown'
    iron_flux_input%file_varname = 'iron_flux'
    iron_flux_input%scale_factor = c1
    iron_flux_input%default_val  = c0
    iron_flux_input%file_fmt     = 'bin'
    fesedflux_input%filename     = 'unknown'
    fesedflux_input%file_varname = 'FESEDFLUXIN'
    fesedflux_input%scale_factor = c1
    fesedflux_input%default_val  = c0
    fesedflux_input%file_fmt     = 'bin'
    ndep_data_type = 'monthly-calendar'
    nox_flux_monthly_input%filename     = 'unknown'
    nox_flux_monthly_input%file_varname = 'nox_flux'
    nox_flux_monthly_input%scale_factor = c1
    nox_flux_monthly_input%default_val  = c0
    nox_flux_monthly_input%file_fmt     = 'bin'
    nhy_flux_monthly_input%filename     = 'unknown'
    nhy_flux_monthly_input%file_varname = 'nhy_flux'
    nhy_flux_monthly_input%scale_factor = c1
    nhy_flux_monthly_input%default_val  = c0
    nhy_flux_monthly_input%file_fmt     = 'bin'
    ndep_shr_stream_year_first = 1
    ndep_shr_stream_year_last  = 1
    ndep_shr_stream_year_align = 1
    ndep_shr_stream_file       = 'unknown'
    ndep_shr_stream_scale_factor = c1
    din_riv_flux_input%filename     = 'unknown'
    din_riv_flux_input%file_varname = 'din_riv_flux'
    din_riv_flux_input%scale_factor = c1
    din_riv_flux_input%default_val  = c0
    din_riv_flux_input%file_fmt     = 'nc'
    dip_riv_flux_input%filename     = 'unknown'
    dip_riv_flux_input%file_varname = 'dip_riv_flux'
    dip_riv_flux_input%scale_factor = c1
    dip_riv_flux_input%default_val  = c0
    dip_riv_flux_input%file_fmt     = 'nc'
    don_riv_flux_input%filename     = 'unknown'
    don_riv_flux_input%file_varname = 'don_riv_flux'
    don_riv_flux_input%scale_factor = c1
    don_riv_flux_input%default_val  = c0
    don_riv_flux_input%file_fmt     = 'nc'
    dop_riv_flux_input%filename     = 'unknown'
    dop_riv_flux_input%file_varname = 'dop_riv_flux'
    dop_riv_flux_input%scale_factor = c1
    dop_riv_flux_input%default_val  = c0
    dop_riv_flux_input%file_fmt     = 'nc'
    dsi_riv_flux_input%filename     = 'unknown'
    dsi_riv_flux_input%file_varname = 'dsi_riv_flux'
    dsi_riv_flux_input%scale_factor = c1
    dsi_riv_flux_input%default_val  = c0
    dsi_riv_flux_input%file_fmt     = 'nc'
    dfe_riv_flux_input%filename     = 'unknown'
    dfe_riv_flux_input%file_varname = 'dfe_riv_flux'
    dfe_riv_flux_input%scale_factor = c1
    dfe_riv_flux_input%default_val  = c0
    dfe_riv_flux_input%file_fmt     = 'nc'
    dic_riv_flux_input%filename     = 'unknown'
    dic_riv_flux_input%file_varname = 'dic_riv_flux'
    dic_riv_flux_input%scale_factor = c1
    dic_riv_flux_input%default_val  = c0
    dic_riv_flux_input%file_fmt     = 'nc'
    alk_riv_flux_input%filename     = 'unknown'
    alk_riv_flux_input%file_varname = 'alk_riv_flux'
    alk_riv_flux_input%scale_factor = c1
    alk_riv_flux_input%default_val  = c0
    alk_riv_flux_input%file_fmt     = 'nc'
    doc_riv_flux_input%filename     = 'unknown'
    doc_riv_flux_input%file_varname = 'doc_riv_flux'
    doc_riv_flux_input%scale_factor = c1
    doc_riv_flux_input%default_val  = c0
    doc_riv_flux_input%file_fmt     = 'nc'
    liron_patch              = .false.
    iron_patch_flux_filename = 'unknown_iron_patch_filename'
    iron_patch_month         = 1
    atm_co2_opt   = 'const'
    atm_co2_const = 280.0_r8
    atm_alt_co2_opt   = 'const'
    atm_alt_co2_const = 280.0_r8
    ciso_atm_d13c_opt                       = 'const'
    ciso_atm_d13c_const                     = -6.379_r8
    ciso_atm_d13c_filename                  = 'unknown'
    ciso_atm_d14c_opt                       = 'const'
    ciso_atm_d14c_const                     = 0.0_r8
    ciso_atm_d14c_filename(1)               = 'unknown'
    ciso_atm_d14c_filename(2)               = 'unknown'
    ciso_atm_d14c_filename(3)               = 'unknown'
    ciso_atm_model_year                     = 1
    ciso_atm_data_year                      = 1

  end subroutine marbl_parms_set_defaults

  !*****************************************************************************

  subroutine marbl_parms_read_namelist(nl_buffer, marbl_status_log)

    use marbl_namelist_mod, only : marbl_nl_cnt
    use marbl_namelist_mod, only : marbl_nl_buffer_size
    use marbl_namelist_mod, only : marbl_namelist

    character(marbl_nl_buffer_size), dimension(marbl_nl_cnt), intent(in) :: nl_buffer
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_parms:marbl_parms_read_namelist'
    character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
    character(len=char_len) :: log_message

    integer (int_kind)           :: n                           ! index for looping over tracers
    integer (int_kind)           :: nml_error                   ! namelist i/o error flag
    integer (int_kind)           :: zoo_ind                     ! zooplankton functional group index

    namelist /marbl_parms_nml/                                                &
         parm_Fe_bioavail, parm_o2_min, parm_o2_min_delta, parm_kappa_nitrif, &
         parm_nitrif_par_lim, parm_labile_ratio, parm_POMbury, parm_BSIbury,  &
         parm_fe_scavenge_rate0, parm_f_prod_sp_CaCO3, parm_POC_diss,         &
         parm_SiO2_diss, parm_CaCO3_diss, fe_max_scale2, parm_scalelen_z,     &
         parm_scalelen_vals, grazing

    namelist /marbl_ecosys_base_nml/                                          &
         init_ecosys_option, init_ecosys_init_file, tracer_init_ext,          &
         init_ecosys_init_file_fmt, iron_frac_in_dust, iron_frac_in_bc,       &
         caco3_bury_thres_opt, caco3_bury_thres_depth, PON_bury_coeff,        &
         POP_bury_coeff, lnutr_variable_restore, nutr_variable_rest_file,     &
         nutr_rest_file, nutr_variable_rest_file_fmt

    namelist /marbl_ciso_nml/ &
         ciso_init_ecosys_option, ciso_init_ecosys_init_file,                 &
         ciso_init_ecosys_init_file_fmt, ciso_tracer_init_ext,                &
         ciso_fract_factors

    namelist /marbl_restore_nml/                                              &
         restore_short_names, restore_filenames, restore_file_varnames,       &
         rest_time_inv_surf, rest_time_inv_deep, rest_z0, rest_z1

    namelist /marbl_forcing_tmp_nml/                                          &
         dust_flux_source, dust_flux_input, iron_flux_source,                 &
         iron_flux_input, fesedflux_input, ndep_data_type,                    &
         nox_flux_monthly_input, nhy_flux_monthly_input,                      &
         ndep_shr_stream_year_first, ndep_shr_stream_year_last,               &
         ndep_shr_stream_year_align, ndep_shr_stream_file,                    &
         ndep_shr_stream_scale_factor, din_riv_flux_input,                    &
         dip_riv_flux_input, don_riv_flux_input, dop_riv_flux_input,          &
         dsi_riv_flux_input, dfe_riv_flux_input, dic_riv_flux_input,          &
         alk_riv_flux_input, doc_riv_flux_input, gas_flux_forcing_opt,        &
         gas_flux_forcing_file, gas_flux_fice, gas_flux_ws, gas_flux_ap,      &
         atm_co2_opt, atm_co2_const, atm_alt_co2_opt, atm_alt_co2_const,      &
         liron_patch, iron_patch_flux_filename, iron_patch_month,             &
         ciso_atm_d13c_opt, ciso_atm_d13c_const, ciso_atm_d13c_filename,      &
         ciso_atm_d14c_opt, ciso_atm_d14c_const, ciso_atm_d14c_filename,      &
         ciso_atm_model_year, ciso_atm_data_year

    !---------------------------------------------------------------------------
    ! read the &marbl_parms_nml namelist
    !---------------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_parms_nml')
    read(tmp_nl_buffer, nml=marbl_parms_nml, iostat=nml_error)
    if (nml_error /= 0) then
       call marbl_status_log%log_error("Error reading marbl_parms_nml", subname)
       return
    end if

    !-----------------------------------------------------------------------
    ! read the &marbl_ecosys_base_nml namelist
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_ecosys_base_nml')
    read(tmp_nl_buffer, nml=marbl_ecosys_base_nml, iostat=nml_error)
    if (nml_error /= 0) then
      call marbl_status_log%log_error("error reading &marbl_ecosys_base_nml", subname)
      return
    end if

    !-----------------------------------------------------------------------
    !  set variables immediately dependent on namelist variables
    !-----------------------------------------------------------------------

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

    !-----------------------------------------------------------------------
    ! read the &marbl_ciso_nml namelist
    !-----------------------------------------------------------------------
    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_ciso_nml')
    read(tmp_nl_buffer, nml=marbl_ciso_nml, iostat=nml_error)
    if (nml_error /= 0) then
       call marbl_status_log%log_error("error reading &marbl_ciso_nml", subname)
       return
    end if

    !-----------------------------------------------------------------------
    ! read the &marbl_restore_nml namelist
    !-----------------------------------------------------------------------
    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_restore_nml')
    read(tmp_nl_buffer, nml=marbl_restore_nml, iostat=nml_error)
    if (nml_error /= 0) then
       call marbl_status_log%log_error("error reading &marbl_restore_nml", subname)
       return
    end if

    !-----------------------------------------------------------------------
    ! read the &marbl_forcing_tmp_nml namelist
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_forcing_tmp_nml')
    read(tmp_nl_buffer, nml=marbl_forcing_tmp_nml, iostat=nml_error)
    if (nml_error /= 0) then
      call marbl_status_log%log_error("error reading &marbl_forcing_tmp_nml", subname)
      return
    end if

    !-----------------------------------------------------------------------
    ! reassign values temporary input values to correct arrays
    !-----------------------------------------------------------------------

    if (trim(gas_flux_forcing_opt) == 'drv') then
       gas_flux_forcing_iopt = gas_flux_forcing_iopt_drv
    else if (trim(gas_flux_forcing_opt) == 'file') then
       gas_flux_forcing_iopt = gas_flux_forcing_iopt_file
    else
       write(log_message, "(2A)") "unknown gas_flux_forcing_opt: ", trim(gas_flux_forcing_opt)
       call marbl_status_log%log_error(log_message, subname)
       return
    endif

    fice_file_loc%input             = gas_flux_fice
    xkw_file_loc%input              = gas_flux_ws
    ap_file_loc%input               = gas_flux_ap
    dust_flux_file_loc%input        = dust_flux_input
    iron_flux_file_loc%input        = iron_flux_input
    nox_flux_monthly_file_loc%input = nox_flux_monthly_input
    nhy_flux_monthly_file_loc%input = nhy_flux_monthly_input
    din_riv_flux_file_loc%input     = din_riv_flux_input
    dip_riv_flux_file_loc%input     = dip_riv_flux_input
    don_riv_flux_file_loc%input     = don_riv_flux_input
    dop_riv_flux_file_loc%input     = dop_riv_flux_input
    dsi_riv_flux_file_loc%input     = dsi_riv_flux_input
    dfe_riv_flux_file_loc%input     = dfe_riv_flux_input
    dic_riv_flux_file_loc%input     = dic_riv_flux_input
    alk_riv_flux_file_loc%input     = alk_riv_flux_input
    doc_riv_flux_file_loc%input     = doc_riv_flux_input

    dust_flux_file        => dust_flux_file_loc
    iron_flux_file        => iron_flux_file_loc
    fice_file             => fice_file_loc
    xkw_file              => xkw_file_loc
    ap_file               => ap_file_loc
    nox_flux_monthly_file => nox_flux_monthly_file_loc
    nhy_flux_monthly_file => nhy_flux_monthly_file_loc
    din_riv_flux_file     => din_riv_flux_file_loc
    dip_riv_flux_file     => dip_riv_flux_file_loc
    don_riv_flux_file     => don_riv_flux_file_loc
    dop_riv_flux_file     => dop_riv_flux_file_loc
    dsi_riv_flux_file     => dsi_riv_flux_file_loc
    dfe_riv_flux_file     => dfe_riv_flux_file_loc
    dic_riv_flux_file     => dic_riv_flux_file_loc
    alk_riv_flux_file     => alk_riv_flux_file_loc
    doc_riv_flux_file     => doc_riv_flux_file_loc

    !-----------------------------------------------------------------------
    !  set variables immediately dependent on namelist variables
    !-----------------------------------------------------------------------

    select case (atm_co2_opt)
    case ('const')
       atm_co2_iopt = atm_co2_iopt_const
    case ('drv_prog')
       atm_co2_iopt = atm_co2_iopt_drv_prog
    case ('drv_diag')
       atm_co2_iopt = atm_co2_iopt_drv_diag
    case default
       write(log_message, "(2A)") "unknown atm_co2_opt: ", trim(atm_co2_opt)
       call marbl_status_log%log_error(log_message, subname)
       return
    end select

    select case (atm_alt_co2_opt)
    case ('const')
       atm_alt_co2_iopt = atm_co2_iopt_const
    case default
       write(log_message, "(2A)") "unknown atm_alt_co2_opt: ", trim(atm_alt_co2_opt)
       call marbl_status_log%log_error(log_message, subname)
       return
    end select

  end subroutine marbl_parms_read_namelist

  !*****************************************************************************

  subroutine marbl_parms_construct(this, marbl_status_log)

    class(marbl_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_parms:marbl_parms_construct'
    character(len=char_len) :: log_message
    character(len=char_len) :: sname, lname, units, datatype, group
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%parameters has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    this%cnt = 0
    allocate(this%vars(this%cnt))

    !-----------------!
    ! marbl_parms_nml !
    !-----------------!

    sname     = 'parm_Fe_bioavail'
    lname     = 'Fraction of Fe flux that is bioavailable'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_Fe_bioavail
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_kappa_nitrif'
    lname     = 'Nitrification inverse time constant'
    units     = '1/s'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_kappa_nitrif
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_POMbury'
    lname     = 'scale factor for burial of POC, PON, and POP'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_POMbury
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_BSIbury'
    lname     = 'scale factor for burial of bSi'
    units     = 'unitless'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_BSIbury
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_fe_scavenge_rate0'
    lname     = 'base scavenging rate'
    units     = '1/yr'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_fe_scavenge_rate0
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
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
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'fe_max_scale2'
    lname     = 'Scaling coefficient'
    units     = 'm^3 / mmol / yr'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => fe_max_scale2
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'parm_scalelen_z'
    lname     = 'Depths of prescribed scale length values'
    units     = 'cm'
    group     = 'marbl_parms_nml'
    call this%add_var_1d_r8(sname, lname, units, group, parm_scalelen_z,    &
                              marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_r8', subname)
      return
    end if

    sname     = 'parm_scalelen_vals'
    lname     = 'Prescribed scale length values'
    units     = 'cm'
    group     = 'marbl_parms_nml'
    call this%add_var_1d_r8(sname, lname, units, group, parm_scalelen_vals, &
                              marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_r8', subname)
      return
    end if

    !-----------------------!
    ! marbl_ecosys_base_nml !
    !-----------------------!

    sname     = 'init_ecosys_option'
    lname     = 'How base ecosys module initialized'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ecosys_base_nml'
    sptr      => init_ecosys_option
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'init_ecosys_init_file'
    lname     = 'File containing base ecosys init conditions'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ecosys_base_nml'
    sptr      => init_ecosys_init_file
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'init_ecosys_init_file_fmt'
    lname     = 'Format of file containing base ecosys init conditions'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ecosys_base_nml'
    sptr      => init_ecosys_init_file_fmt
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'iron_frac_in_dust'
    lname     = 'Fraction by weight of iron in dust'
    units     = 'unitless (kg/kg)'
    datatype  = 'real'
    group     = 'marbl_ecosys_base_nml'
    rptr      => iron_frac_in_dust
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'iron_frac_in_bc'
    lname     = 'Fraction by weight of iron in black carbon'
    units     = 'unitless (kg/kg)'
    datatype  = 'real'
    group     = 'marbl_ecosys_base_nml'
    rptr      => iron_frac_in_dust
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'caco3_bury_thres_opt'
    lname     = 'Option for CaCO3 burial threshold'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ecosys_base_nml'
    sptr      => caco3_bury_thres_opt
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'caco3_bury_thres_depth'
    lname     = 'Threshold depth for CaCO3 burial (if using fixed_depth option)'
    units     = 'cm'
    datatype  = 'real'
    group     = 'marbl_ecosys_base_nml'
    rptr      => caco3_bury_thres_depth
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'PON_bury_coeff'
    lname     = ''
    units     = ''
    datatype  = 'real'
    group     = 'marbl_ecosys_base_nml'
    rptr      => PON_bury_coeff
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'POP_bury_coeff'
    lname     = ''
    units     = ''
    datatype  = 'real'
    group     = 'marbl_ecosys_base_nml'
    rptr      => POP_bury_coeff
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lnutr_variable_restore'
    lname     = 'Flag to use spatially-varying nutrient restoring'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_ecosys_base_nml'
    lptr      => lnutr_variable_restore
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'nutr_rest_file'
    lname     = 'File containing nutrient fields'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ecosys_base_nml'
    sptr      => nutr_rest_file
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'nutr_variable_rest_file'
    lname     = 'File containing spatially-varying nutrient restoring info'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ecosys_base_nml'
    sptr      => nutr_variable_rest_file
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'nutr_variable_rest_file_fmt'
    lname     = 'Format of file containing spatially-varying nutrient restoring info'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ecosys_base_nml'
    sptr      => nutr_variable_rest_file_fmt
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    !----------------!
    ! marbl_ciso_nml !
    !----------------!

    sname     = 'ciso_init_ecosys_option'
    lname     = 'How carbon isotope module is initialized'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ciso_nml'
    sptr      => ciso_init_ecosys_option
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ciso_init_ecosys_init_file'
    lname     = 'File containing carbon isotope init conditions'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ciso_nml'
    sptr      => ciso_init_ecosys_init_file
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ciso_init_ecosys_init_file_fmt'
    lname     = 'Format of file containing carbon isotope init conditions'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ciso_nml'
    sptr      => ciso_init_ecosys_init_file_fmt
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ciso_fract_factors'
    lname     = 'Optiob for which biological fractionation calculation to use'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_ciso_nml'
    sptr      => ciso_fract_factors
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    !-------------------!
    ! marbl_restore_nml !
    !-------------------!

    sname     = 'restore_short_names'
    lname     = 'Tracer names for tracers that are restored'
    units     = 'unitless'
    group     = 'marbl_restore_nml'
    call this%add_var_1d_str(sname, lname, units, group, restore_short_names, &
                               marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_str', subname)
      return
    end if

    sname     = 'restore_filenames'
    lname     = 'Files containing tracer restoring fields'
    units     = 'unitless'
    group     = 'marbl_restore_nml'
    call this%add_var_1d_str(sname, lname, units, group, restore_filenames, &
                               marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_str', subname)
      return
    end if

    sname     = 'restore_file_varnames'
    lname     = 'Name of fields for tracer restoring (as appearing in restore_filenames)'
    units     = 'unitless'
    group     = 'marbl_restore_nml'
    call this%add_var_1d_str(sname, lname, units, group, restore_file_varnames, &
                               marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_1d_str', subname)
      return
    end if

    sname     = 'rest_time_inv_surf'
    lname     = 'Restoring time scale at rest_z0'
    units     = '1/sec'
    datatype  = 'real'
    group     = 'marbl_restore_nml'
    rptr      => rest_time_inv_surf
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'rest_time_inv_deep'
    lname     = 'Restoring time scale at rest_z1'
    units     = '1/sec'
    datatype  = 'real'
    group     = 'marbl_restore_nml'
    rptr      => rest_time_inv_deep
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'rest_z0'
    lname     = 'Above this depth, restoring time scale is rest_time_inv_surf'
    units     = 'cm'
    datatype  = 'real'
    group     = 'marbl_restore_nml'
    rptr      => rest_z0
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'rest_z1'
    lname     = 'Below this depth, restoring time scale is rest_time_inv_deep'
    units     = 'cm'
    datatype  = 'real'
    group     = 'marbl_restore_nml'
    rptr      => rest_z1
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    !-----------------------!
    ! marbl_forcing_tmp_nml !
    !-----------------------!

    sname     = 'gas_flux_forcing_file'
    lname     = 'File containing gas flux forcing fields'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => gas_flux_forcing_file
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'atm_co2_const'
    lname     = 'Value of atmospheric co2'
    units     = 'ppm (dry air; 1 atm)'
    datatype  = 'real'
    group     = 'marbl_forcing_tmp_nml'
    rptr      => atm_co2_const
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'atm_alt_co2_const'
    lname     = 'Value of atmospheric alternative co2'
    units     = 'ppm (dry air; 1 atm)'
    datatype  = 'real'
    group     = 'marbl_forcing_tmp_nml'
    rptr      => atm_alt_co2_const
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_year_first'
    lname     = 'First year to use in ndep stream file'
    units     = 'years'
    datatype  = 'integer'
    group     = 'marbl_forcing_tmp_nml'
    iptr      => ndep_shr_stream_year_first
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_year_last'
    lname     = 'Last year to use in ndep stream file'
    units     = 'years'
    datatype  = 'integer'
    group     = 'marbl_forcing_tmp_nml'
    iptr      => ndep_shr_stream_year_last
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_year_align'
    lname     = 'Align ndep_shr_stream_year_first with this model year'
    units     = 'year CE'
    datatype  = 'integer'
    group     = 'marbl_forcing_tmp_nml'
    iptr      => ndep_shr_stream_year_align
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_year_file'
    lname     = 'File containing ndep domain and input data'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => ndep_shr_stream_file
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_scale_factor'
    lname     = 'Unit conversion factor'
    units     = 'unknown'
    datatype  = 'real'
    group     = 'marbl_forcing_tmp_nml'
    rptr      => ndep_shr_stream_scale_factor
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'dust_flux_source'
    lname     = 'Option for atmospheric dust deposition'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => dust_flux_source
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'iron_flux_source'
    lname     = 'Option for atmospheric iron deposition'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => iron_flux_source
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'liron_patch'
    lname     = 'Turn on iron patch fertilization'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_forcing_tmp_nml'
    lptr      => liron_patch
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

  end subroutine marbl_parms_construct

  !*****************************************************************************

end module marbl_parms
