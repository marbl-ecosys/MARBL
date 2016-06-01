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

  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : dps

  use marbl_internal_types, only : zooplankton_type
  use marbl_internal_types, only : autotroph_type
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
  !  Variables read in via &marbl_ecosys_base_nml
  !---------------------------------------------------------------------

  type(marbl_tracer_read_type) :: tracer_init_ext(ecosys_base_tracer_cnt) ! namelist variable for initializing tracers 
  type(marbl_tracer_read_type) :: fesedflux_input                    ! namelist input for iron_flux

  character(char_len), target :: gas_flux_forcing_file        ! file containing gas flux forcing fields
  integer(int_kind),   target :: gas_flux_forcing_iopt
  integer(int_kind),   target :: atm_co2_iopt
  integer(int_kind),   target :: atm_alt_co2_iopt
  real(r8),            target :: atm_co2_const                ! value of atmospheric co2 (ppm, dry-air, 1 atm)
  real(r8),            target :: atm_alt_co2_const            ! value of atmospheric alternative co2 (ppm, dry-air, 1 atm)
  logical(log_kind),   target :: lflux_gas_o2                 ! controls which portion of code are executed usefull for debugging
  logical(log_kind),   target :: lflux_gas_co2                ! controls which portion of code are executed usefull for debugging
  character(char_len), target :: init_ecosys_option           ! namelist option for initialization of bgc
  character(char_len), target :: init_ecosys_init_file        ! filename for option 'file'
  character(char_len), target :: init_ecosys_init_file_fmt    ! file format for option 'file'

  logical(log_kind),   target :: liron_patch                  ! flag for iron patch fertilization
  character(char_len), target :: iron_patch_flux_filename     ! file containing name of iron patch file
  integer(int_kind),   target :: iron_patch_month             ! integer month to add patch flux

  character(char_len), target :: ndep_data_type               ! type of ndep forcing
  integer(int_kind), target :: ndep_shr_stream_year_first   ! first year in stream to use
  integer(int_kind), target :: ndep_shr_stream_year_last    ! last year in stream to use
  integer(int_kind), target :: ndep_shr_stream_year_align   ! align ndep_shr_stream_year_first with this model year
  character(char_len), target :: ndep_shr_stream_file         ! file containing domain and input data
  real(r8), target :: ndep_shr_stream_scale_factor ! unit conversion factor

  character(char_len), target :: dust_flux_source             ! option for atmospheric dust deposition
  character(char_len), target :: iron_flux_source             ! option for atmospheric iron deposition
  real(r8), target :: iron_frac_in_dust            ! fraction by weight of iron in dust
  real(r8), target :: iron_frac_in_bc              ! fraction by weight of iron in black carbon

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
       parm_fe_scavenge_rate0,& ! base scavenging rate
       parm_f_prod_sp_CaCO3,  & !fraction of sp prod. as CaCO3 prod.
       parm_POC_diss,         & ! base POC diss len scale
       parm_SiO2_diss,        & ! base SiO2 diss len scale
       parm_CaCO3_diss,       & ! base CaCO3 diss len scale
       fe_max_scale2            ! unitless scaling coeff.

  real(kind=r8), dimension(4) :: &
       parm_scalelen_z,       & ! depths of prescribed scalelen values
       parm_scalelen_vals       ! prescribed scalelen values

  type(zooplankton_type) :: zooplankton(zooplankton_cnt)
  type(autotroph_type)   :: autotrophs(autotroph_cnt)
  type(grazing_type)     :: grazing(grazer_prey_cnt, zooplankton_cnt)

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_ciso_nml
  !---------------------------------------------------------------------

  type(marbl_tracer_read_type) :: ciso_tracer_init_ext(ciso_tracer_cnt) ! namelist variable for initializing tracers

  logical   (log_kind)        :: ciso_lsource_sink              ! flag controlling which portion of code are executed
  character (char_len)        :: ciso_fract_factors             ! option for which biological fractionation calculation to use
  character (char_len)        :: ciso_init_ecosys_option        ! option for initialization of bgc
  character (char_len)        :: ciso_init_ecosys_init_file     ! filename for option 'file'
  character (char_len)        :: ciso_init_ecosys_init_file_fmt ! file format for option 'file'
  logical   (log_kind)        :: ciso_lecovars_full_depth_tavg  ! should ecosystem vars be written full depth

  ! ciso forcing related variables

  integer   (int_kind)        :: ciso_atm_model_year            ! arbitrary model year
  integer   (int_kind)        :: ciso_atm_data_year             ! year in atmospheric ciso data that corresponds to ciso_atm_model_year
  integer   (int_kind)        :: ciso_atm_d13c_data_nbval       ! number of values in ciso_atm_d13c_filename
  integer   (int_kind)        :: ciso_atm_d14c_data_nbval       ! number of values in ciso_atm_d14c_filename
  real      (r8), allocatable :: ciso_atm_d13c_data(:)          ! atmospheric D13C values in datafile
  real      (r8), allocatable :: ciso_atm_d13c_data_yr(:)       ! date of atmospheric D13C values in datafile
  real      (r8), allocatable :: ciso_atm_d14c_data(:,:)        ! atmospheric D14C values in datafile (sh, eq, nh, in permil)
  real      (r8), allocatable :: ciso_atm_d14c_data_yr(:,:)     ! date of atmospheric D14C values in datafile (sh, eq, nh)
  real      (r8)              :: ciso_atm_d13c_const            ! atmospheric D13C constant [permil]
  real      (r8)              :: ciso_atm_d14c_const            ! atmospheric D14C constant [permil]
  character (char_len)        :: ciso_atm_d13c_opt              ! option for CO2 and D13C varying or constant forcing
  character (char_len)        :: ciso_atm_d13c_filename         ! filenames for varying atm D13C
  character (char_len)        :: ciso_atm_d14c_opt              ! option for CO2 and D13C varying or constant forcing
  character (char_len)        :: ciso_atm_d14c_filename(3)      ! filenames for varying atm D14C (one each for NH, SH, EQ)

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_restore_nml
  !---------------------------------------------------------------------

  character(len=char_len), allocatable, dimension(:) :: restore_short_names, &
                                                        restore_filenames,   &
                                                        restore_file_varnames
  real(r8) :: rest_time_inv_surf, rest_time_inv_deep, rest_z0, rest_z1

  !---------------------------------------------------------------------
  !  Datatype for accessing parameters without namelist
  !---------------------------------------------------------------------

  type, public :: marbl_single_parms_type
    ! Metadata
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: group
    character(len=char_len) :: datatype
    ! Actual parameter data
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()
  end type marbl_single_parms_type

  type, public :: marbl_parms_type
    integer :: parms_cnt
    type(marbl_single_parms_type), dimension(:), pointer :: parms => NULL()
  contains
    procedure :: construct  => marbl_parms_construct
    procedure :: add_parms  => marbl_parms_add
    procedure :: list_parms => marbl_parms_list
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
  !  Auxiliary variables (str -> int conversions, indices, etc)
  !---------------------------------------------------------------------

  integer (int_kind), parameter :: marbl_freq_opt_never    = 0
  integer (int_kind), parameter :: marbl_freq_opt_nyear    = 1
  integer (int_kind), parameter :: marbl_freq_opt_nmonth   = 2

  integer (int_kind), parameter :: gas_flux_forcing_iopt_drv  = 1
  integer (int_kind), parameter :: gas_flux_forcing_iopt_file = 2

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

  !*****************************************************************************

  public :: &
       marbl_parms_init

  private ::                     &
       marbl_parms_set_defaults, &
       log_add_parms_error

  ! Variables used from other modules should be private
  ! (So we don't accidentally use them from this module)
  private :: r8, int_kind, log_kind
  private :: c1, dps
  private :: zooplankton_type, autotroph_type, grazing_type
  private :: autotroph_cnt, zooplankton_cnt, grazer_prey_cnt

contains

  !*****************************************************************************

  subroutine marbl_parms_set_defaults()
    ! assign default parameter values

    implicit none

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    integer :: auto_ind, zoo_ind, prey_ind
    !---------------------------------------------------------------------------
    
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

    zoo_ind = 1
    zooplankton(zoo_ind)%sname          ='zoo'
    zooplankton(zoo_ind)%lname          = 'Zooplankton'
    zooplankton(zoo_ind)%z_mort_0       = 0.1_r8 * dps
    zooplankton(zoo_ind)%z_mort2_0      = 0.4_r8 * dps
    zooplankton(zoo_ind)%loss_thres     = 0.075_r8     !zoo conc. where losses go to zero

    auto_ind = sp_ind
    autotrophs(auto_ind)%sname         = 'sp'
    autotrophs(auto_ind)%lname         = 'Small Phyto'
    autotrophs(auto_ind)%Nfixer        = .false.
    autotrophs(auto_ind)%imp_calcifier = .true.
    autotrophs(auto_ind)%exp_calcifier = .false.
    autotrophs(auto_ind)%kFe           = 0.03e-3_r8
    autotrophs(auto_ind)%kPO4          = 0.01_r8
    autotrophs(auto_ind)%kDOP          = 0.2_r8
    autotrophs(auto_ind)%kNO3          = 0.25_r8
    autotrophs(auto_ind)%kNH4          = 0.0125_r8
    autotrophs(auto_ind)%kSiO3         = 0.0_r8
    autotrophs(auto_ind)%Qp            = 0.00855_r8
    autotrophs(auto_ind)%gQfe_0        = 30.0e-6_r8
    autotrophs(auto_ind)%gQfe_min      = 3.0e-6_r8
    autotrophs(auto_ind)%alphaPI       = 0.39_r8 * dps
    autotrophs(auto_ind)%PCref         = 5.5_r8 * dps
    autotrophs(auto_ind)%thetaN_max    = 2.5_r8
    autotrophs(auto_ind)%loss_thres    = 0.02_r8
    autotrophs(auto_ind)%loss_thres2   = 0.0_r8
    autotrophs(auto_ind)%temp_thres    = -10.0_r8
    autotrophs(auto_ind)%mort          = 0.1_r8 * dps
    autotrophs(auto_ind)%mort2         = 0.01_r8 * dps
    autotrophs(auto_ind)%agg_rate_max  = 0.5_r8
    autotrophs(auto_ind)%agg_rate_min  = 0.01_r8
    autotrophs(auto_ind)%loss_poc      = 0.0_r8

    auto_ind = diat_ind
    autotrophs(auto_ind)%sname         = 'diat'
    autotrophs(auto_ind)%lname         = 'Diatom'
    autotrophs(auto_ind)%Nfixer        = .false.
    autotrophs(auto_ind)%imp_calcifier = .false.
    autotrophs(auto_ind)%exp_calcifier = .false.
    autotrophs(auto_ind)%kFe           = 0.07e-3_r8
    autotrophs(auto_ind)%kPO4          = 0.05_r8
    autotrophs(auto_ind)%kDOP          = 0.5_r8
    autotrophs(auto_ind)%kNO3          = 1.0_r8
    autotrophs(auto_ind)%kNH4          = 0.05_r8
    autotrophs(auto_ind)%kSiO3         = 0.8_r8
    autotrophs(auto_ind)%Qp            = 0.00855_r8
    autotrophs(auto_ind)%gQfe_0        = 30.0e-6_r8
    autotrophs(auto_ind)%gQfe_min      = 3.0e-6_r8
    autotrophs(auto_ind)%alphaPI       = 0.29_r8 * dps
    autotrophs(auto_ind)%PCref         = 5.5_r8 * dps
    autotrophs(auto_ind)%thetaN_max    = 4.0_r8
    autotrophs(auto_ind)%loss_thres    = 0.02_r8
    autotrophs(auto_ind)%loss_thres2   = 0.0_r8
    autotrophs(auto_ind)%temp_thres    = -10.0_r8
    autotrophs(auto_ind)%mort          = 0.1_r8 * dps
    autotrophs(auto_ind)%mort2         = 0.01_r8 * dps
    autotrophs(auto_ind)%agg_rate_max  = 0.5_r8
    autotrophs(auto_ind)%agg_rate_min  = 0.02_r8
    autotrophs(auto_ind)%loss_poc      = 0.0_r8
   

    auto_ind = diaz_ind
    autotrophs(auto_ind)%sname         = 'diaz'
    autotrophs(auto_ind)%lname         = 'Diazotroph'
    autotrophs(auto_ind)%Nfixer        = .true.
    autotrophs(auto_ind)%imp_calcifier = .false.
    autotrophs(auto_ind)%exp_calcifier = .false.
    autotrophs(auto_ind)%kFe           = 0.03e-3_r8
    autotrophs(auto_ind)%kPO4          = 0.0125_r8
    autotrophs(auto_ind)%kDOP          = 0.05_r8
    autotrophs(auto_ind)%kNO3          = 4.0_r8
    autotrophs(auto_ind)%kNH4          = 0.4_r8
    autotrophs(auto_ind)%kSiO3         = 0.0_r8
    autotrophs(auto_ind)%Qp            = 0.002735_r8
    autotrophs(auto_ind)%gQfe_0        = 60.0e-6_r8
    autotrophs(auto_ind)%gQfe_min      = 6.0e-6_r8
    autotrophs(auto_ind)%alphaPI       = 0.39_r8 * dps
    autotrophs(auto_ind)%PCref         = 1.55_r8 * dps
    autotrophs(auto_ind)%thetaN_max    = 2.5_r8
    autotrophs(auto_ind)%loss_thres    = 0.02_r8
    autotrophs(auto_ind)%loss_thres2   = 0.001_r8
    autotrophs(auto_ind)%temp_thres    = 15.0_r8
    autotrophs(auto_ind)%mort          = 0.1_r8 * dps
    autotrophs(auto_ind)%mort2         = 0.01_r8 * dps
    autotrophs(auto_ind)%agg_rate_max  = 0.5_r8
    autotrophs(auto_ind)%agg_rate_min  = 0.01_r8
    autotrophs(auto_ind)%loss_poc      = 0.0_r8
 

    !---------------------------------------------------------------------------
    ! predator-prey relationships
    !---------------------------------------------------------------------------
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

  end subroutine marbl_parms_set_defaults

  !*****************************************************************************

  subroutine marbl_parms_init(nl_buffer, marbl_status_log)

    use marbl_namelist_mod, only : marbl_nl_cnt
    use marbl_namelist_mod, only : marbl_nl_buffer_size
    use marbl_namelist_mod, only : marbl_namelist

    implicit none

    character(marbl_nl_buffer_size), dimension(marbl_nl_cnt), intent(in) :: nl_buffer
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_parms:marbl_parms_init'
    character(len=marbl_nl_buffer_size) :: tmp_nl_buffer

    integer(kind=int_kind) :: io_error

    NAMELIST /marbl_parms_nml/ &
         parm_Fe_bioavail, &
         parm_o2_min, &
         parm_o2_min_delta, &
         parm_kappa_nitrif, &
         parm_nitrif_par_lim, &
         parm_labile_ratio, &
         parm_POMbury, &
         parm_BSIbury, &
         parm_fe_scavenge_rate0, &
         parm_f_prod_sp_CaCO3, &
         parm_POC_diss, &
         parm_SiO2_diss, &
         parm_CaCO3_diss, &
         fe_max_scale2, &
         parm_scalelen_z, &
         parm_scalelen_vals, &
         autotrophs, & 
         zooplankton, &
         grazing

    !---------------------------------------------------------------------------
    ! set defaults for namelist variables before reading them
    !---------------------------------------------------------------------------

    call marbl_parms_set_defaults()

    !---------------------------------------------------------------------------
    ! read in namelist to override some defaults
    !---------------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_parms_nml')
    read(tmp_nl_buffer, nml=marbl_parms_nml, iostat=io_error)
    if (io_error /= 0) then
       call marbl_status_log%log_error("Error reading marbl_parms_nml", subname)
       return
    else
       ! FIXME #16: this is printing contents of pop_in, not the entire marbl_parms_nml
      call marbl_status_log%log_namelist('marbl_parms_nml', tmp_nl_buffer, subname)
    end if

  end subroutine marbl_parms_init

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

    if (associated(this%parms)) then
      write(log_message, "(A)") "this%parameters has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    this%parms_cnt = 0
    allocate(this%parms(this%parms_cnt))

    !-----------------------!
    ! marbl_ecosys_base_nml !
    !-----------------------!

    sname     = 'gas_flux_forcing_file'
    lname     =  'File containing gas flux forcing fields'
    units     =  'unitless'
    datatype  =  'string'
    group     =  'marbl_ecosys_base_nml'
    sptr      => gas_flux_forcing_file
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'atm_co2_const'
    lname     =  'Value of atmospheric co2'
    units     =  'ppm (dry air; 1 atm)'
    datatype  =  'real'
    group     =  'marbl_ecosys_base_nml'
    rptr      => atm_co2_const
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'atm_alt_co2_const'
    lname     =  'Value of atmospheric alternative co2'
    units     =  'ppm (dry air; 1 atm)'
    datatype  =  'real'
    group     =  'marbl_ecosys_base_nml'
    rptr      => atm_alt_co2_const
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'lflux_gas_o2'
    lname      = 'Run O2 gas flux portion of the code'
    units      = 'unitless'
    datatype   = 'logical'
    lptr      => lflux_gas_o2
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'lflux_gas_co2'
    lname      = 'Run CO2 gas flux portion of the code'
    units      = 'unitless'
    datatype   = 'logical'
    lptr      => lflux_gas_co2
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'init_ecosys_option'
    lname     =  'How base ecosys module initialized'
    units     =  'unitless'
    datatype  =  'string'
    group     =  'marbl_ecosys_base_nml'
    sptr      => init_ecosys_option
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'init_ecosys_init_file'
    lname     =  'File containing base ecosys init conditions'
    units     =  'unitless'
    datatype  =  'string'
    group     =  'marbl_ecosys_base_nml'
    sptr      => init_ecosys_init_file
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'init_ecosys_init_file_fmt'
    lname     =  'Format of file containing base ecosys init conditions'
    units     =  'unitless'
    datatype  =  'string'
    group     =  'marbl_ecosys_base_nml'
    sptr      => init_ecosys_init_file_fmt
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'liron_patch'
    lname     = 'Turn on iron patch fertilization'
    units     = 'unitless'
    datatype  = 'logical'
    group     =  'marbl_ecosys_base_nml'
    lptr      => liron_patch
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'ndep_shr_stream_year_first'
    lname     = 'First year to use in ndep stream file'
    units     = 'years'
    datatype  = 'integer'
    group     =  'marbl_ecosys_base_nml'
    iptr      => ndep_shr_stream_year_first
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'ndep_shr_stream_year_last'
    lname     = 'Last year to use in ndep stream file'
    units     = 'years'
    datatype  = 'integer'
    group     =  'marbl_ecosys_base_nml'
    iptr      => ndep_shr_stream_year_last
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'ndep_shr_stream_year_align'
    lname     = 'Align ndep_shr_stream_year_first with this model year'
    units     = 'year CE'
    datatype  = 'integer'
    group     =  'marbl_ecosys_base_nml'
    iptr      => ndep_shr_stream_year_align
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'ndep_shr_stream_year_file'
    lname     = 'File containing ndep domain and input data'
    units     = 'unitless'
    datatype  = 'string'
    group     =  'marbl_ecosys_base_nml'
    sptr      => ndep_shr_stream_file
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'ndep_shr_stream_scale_factor'
    lname     = 'Unit conversion factor'
    units     = 'unknown'
    datatype  = 'real'
    group     =  'marbl_ecosys_base_nml'
    rptr      => ndep_shr_stream_scale_factor
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'dust_flux_source'
    lname     = 'Option for atmospheric dust deposition'
    units     = 'unitless'
    datatype  = 'string'
    group     =  'marbl_ecosys_base_nml'
    sptr      => dust_flux_source
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'iron_flux_source'
    lname     = 'Option for atmospheric iron deposition'
    units     = 'unitless'
    datatype  = 'string'
    group     =  'marbl_ecosys_base_nml'
    sptr      => iron_flux_source
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'iron_frac_in_dust'
    lname     = 'Fraction by weight of iron in dust'
    units     = 'unitless (kg/kg)'
    datatype  = 'real'
    group     =  'marbl_ecosys_base_nml'
    rptr      => iron_frac_in_dust
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'iron_frac_in_bc'
    lname     = 'Fraction by weight of iron in black carbon'
    units     = 'unitless (kg/kg)'
    datatype  = 'real'
    group     =  'marbl_ecosys_base_nml'
    rptr      => iron_frac_in_dust
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    !-----------------!
    ! marbl_parms_nml !
    !-----------------!

    sname     = 'parm_Fe_bioavail'
    lname     = 'Fraction of Fe flux that is bioavailable'
    units     = 'unitless'
    datatype  = 'real'
    group     =  'marbl_parms_nml'
    rptr      => parm_Fe_bioavail 
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'parm_o2_min'
    lname     = 'Minimum O2 needed for production and consumption'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    group     =  'marbl_parms_nml'
    rptr      => parm_o2_min
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
    sname     = 'parm_o2_min_delta'
    lname     = 'Width of minimum O2 range'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    group     =  'marbl_parms_nml'
    rptr      => parm_o2_min_delta
    call this%add_parms(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_parms_error(marbl_status_log, sname, subname)
      return
    end if
    
  end subroutine marbl_parms_construct

  !*****************************************************************************

  subroutine marbl_parms_add(this, sname, lname, units, datatype, group,      &
             marbl_status_log, rptr, iptr, lptr, sptr)

    class(marbl_parms_type), intent(inout) :: this
    character(len=char_len), intent(in)    :: sname
    character(len=char_len), intent(in)    :: lname
    character(len=char_len), intent(in)    :: units
    character(len=char_len), intent(in)    :: datatype
    character(len=char_len), intent(in)    :: group
    type(marbl_log_type),    intent(inout) :: marbl_status_log
    real(r8),                optional, pointer, intent(in) :: rptr
    integer,                 optional, pointer, intent(in) :: iptr
    logical,                 optional, pointer, intent(in) :: lptr
    character(len=char_len), optional, pointer, intent(in) :: sptr

    character(*), parameter :: subname = 'marbl_parms:marbl_parms_add'
    type(marbl_single_parms_type), dimension(:), pointer :: new_parms
    integer :: old_size, id, n
    character(len=char_len) :: log_message

    if (.not.associated(this%parms)) then
      write(log_message, "(A)") 'Parms constructor must be run'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    old_size = size(this%parms)
    id = old_size+1

    ! 1) Allocate new_parms to be size N (one element larger than this%parms)
    allocate(new_parms(id))

    ! 2) copy this%parms into first N-1 elements of new_parms
    do n=1, old_size
      new_parms(n)%long_name  = this%parms(n)%long_name
      new_parms(n)%short_name = this%parms(n)%short_name
      new_parms(n)%units      = this%parms(n)%units
      new_parms(n)%datatype   = this%parms(n)%datatype
      new_parms(n)%group      = this%parms(n)%group
      if (associated(this%parms(n)%lptr)) &
        new_parms(n)%lptr => this%parms(n)%lptr
      if (associated(this%parms(n)%iptr)) &
        new_parms(n)%iptr => this%parms(n)%iptr
      if (associated(this%parms(n)%rptr)) &
        new_parms(n)%rptr => this%parms(n)%rptr
      if (associated(this%parms(n)%sptr)) &
        new_parms(n)%sptr => this%parms(n)%sptr
    end do

    ! 3) add newest parm variable
    new_parms(id)%short_name = sname
    new_parms(id)%long_name  = lname
    new_parms(id)%units      = units
    new_parms(id)%datatype   = datatype
    new_parms(id)%group      = group
    select case (trim(datatype))
      case ('real')
        if (present(rptr)) then
          new_parms(id)%rptr => rptr
        else
          write(log_message, "(A)")                                           &
               "Defining real parameter but rptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('integer')
        if (present(iptr)) then
          new_parms(id)%iptr => iptr
        else
          write(log_message, "(A)")                                           &
               "Defining integer parameter but iptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('logical')
        if (present(lptr)) then
          new_parms(id)%lptr => lptr
        else
          write(log_message, "(A)")                                           &
               "Defining logical parameter but lptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('string')
        if (present(sptr)) then
          new_parms(id)%sptr => sptr
        else
          write(log_message, "(A)")                                           &
               "Defining string parameter but aptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case DEFAULT
        write(log_message, "(2A)") "Unkown datatype: ", trim(datatype)
        call marbl_status_log%log_error(log_message, subname)
        return
    end select

    ! 4) deallocate / nullify this%parms
    deallocate(this%parms)
    nullify(this%parms)

    ! 5) point this%parms => new_parms and update parms_cnt
    this%parms => new_parms
    this%parms_cnt = id

  end subroutine marbl_parms_add

  !*****************************************************************************

  subroutine marbl_parms_list(this, marbl_status_log)

    class(marbl_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_parms:marbl_parms_list'
    character(len=char_len) :: log_message
    character(len=char_len) :: group
    character(len=7)        :: logic
    integer :: n

    group = ''
    do n=1,this%parms_cnt
      if (this%parms(n)%group.ne.group) then
        group = trim(this%parms(n)%group)
        call marbl_status_log%log_noerror('', subname)
        write(log_message, "(2A)") trim(group), ' parameter values: '
        call marbl_status_log%log_noerror(log_message, subname)
        call marbl_status_log%log_noerror('---', subname)
      end if
      select case(trim(this%parms(n)%datatype))
        case ('string')
          write(log_message, "(4A)") trim(this%parms(n)%short_name), " = '",  &
                                     trim(this%parms(n)%sptr), "'"
          call marbl_status_log%log_noerror(log_message, subname)
        case ('real')
          write(log_message, "(2A,E9.4)") trim(this%parms(n)%short_name), " = ", &
                                          this%parms(n)%rptr
          call marbl_status_log%log_noerror(log_message, subname)
        case ('integer')
          write(log_message, "(2A,I0)") trim(this%parms(n)%short_name), " = ", &
                                        this%parms(n)%iptr
          call marbl_status_log%log_noerror(log_message, subname)
        case ('logical')
          if (this%parms(n)%lptr) then
            logic = '.true.'
          else
            logic = '.false.'
          end if
          write(log_message, "(3A)") trim(this%parms(n)%short_name), " = ",   &
                                     trim(logic)
          call marbl_status_log%log_noerror(log_message, subname)
        case DEFAULT
          write(log_message, "(2A)") trim(this%parms(n)%datatype),            &
                                     ' is not a valid datatype for parameter'
          call marbl_status_log%log_error(log_message, subname)
          return
      end select
    end do

  end subroutine marbl_parms_list

  !***********************************************************************

  subroutine log_add_parms_error(marbl_status_log, sname, subname)

    type(marbl_log_type), intent(inout) :: marbl_status_log
    character(len=*),     intent(in)    :: sname
    character(len=*),     intent(in)    :: subname
    character(len=char_len) :: routine_name

    write(routine_name,"(3A)") "this%add_parms(", trim(sname), ")"
    call marbl_status_log%log_error_trace(routine_name, subname)

  end subroutine log_add_parms_error

  !*****************************************************************************

end module marbl_parms
