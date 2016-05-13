module marbl_parms

  !-----------------------------------------------------------------------------
  !   This module manages marbl parameter 
  !   Most of the variables are not parameters in the Fortran sense. In the
  !   the Fortran sense, they are vanilla module variables.
  !
  !   This modules handles initializing the variables to default values and
  !   reading them from the namelist marbl_parms. The values used are echoed
  !   to stdout for record keeping purposes.
  !-----------------------------------------------------------------------------

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind

  use marbl_internal_types, only : zooplankton_type
  use marbl_internal_types, only : autotroph_type
  use marbl_internal_types, only : grazing_type

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt
  use marbl_sizes, only : grazer_prey_cnt

  implicit none

  !-----------------------------------------------------------------------------
  !   public/private declarations
  !   all module variables are public and should have their values preserved
  !-----------------------------------------------------------------------------

  public
  save

  !-----------------------------------------------------------------------------
  ! marbl zooplankton, autotrophs and grazing arrays
  !-----------------------------------------------------------------------------

  integer (KIND=int_kind), parameter :: sp_ind   = 1  ! small phytoplankton
  integer (KIND=int_kind), parameter :: diat_ind = 2  ! diatoms
  integer (KIND=int_kind), parameter :: diaz_ind = 3  ! diazotrophs

  type(zooplankton_type) :: zooplankton(zooplankton_cnt)
  type(autotroph_type)   :: autotrophs(autotroph_cnt)
  type(grazing_type)     :: grazing(grazer_prey_cnt, zooplankton_cnt)

  !---------------------------------------------------------------------
  !  molecular weights and isotope standards
  !---------------------------------------------------------------------

  real(r8), parameter :: molw_Fe = 55.845_r8

  ! Using scaled isotopic carbon pools, so Rstd =1
  real(r8), parameter :: R13C_std = 1.0_r8  ! actual 13C/12C PDB standard ratio (Craig, 1957) = 1123.72e-5_r8
  real(r8), parameter :: R14C_std = 1.0_r8  ! actual 14C/12C NOSAMS standard ratio = 11.76e-13_r8

  !-----------------------------------------------------------------------
  !  numbers
  !-----------------------------------------------------------------------
  
  real (r8), parameter :: c0     =    0.0_r8
  real (r8), parameter :: c1     =    1.0_r8
  real (r8), parameter :: c2     =    2.0_r8
  real (r8), parameter :: c3     =    3.0_r8
  real (r8), parameter :: c4     =    4.0_r8
  real (r8), parameter :: c5     =    5.0_r8
  real (r8), parameter :: c8     =    8.0_r8
  real (r8), parameter :: c10    =   10.0_r8
  real (r8), parameter :: c16    =   16.0_r8
  real (r8), parameter :: c1000  = 1000.0_r8
  real (r8), parameter :: c10000 =10000.0_r8
  real (r8), parameter :: c1p5   =    1.5_r8
  real (r8), parameter :: p33    = c1/c3    
  real (r8), parameter :: p5     = 0.500_r8 
  real (r8), parameter :: p25    = 0.250_r8 
  real (r8), parameter :: p125   = 0.125_r8 
  real (r8), parameter :: p001   = 0.001_r8 
  real (r8), parameter :: eps    = 1.0e-10_r8
  real (r8), parameter :: eps2   = 1.0e-20_r8
  real (r8), parameter :: bignum = 1.0e+30_r8

  !-----------------------------------------------------------------------
  !  conversion factors
  !-----------------------------------------------------------------------

  real (r8) :: mpercm    = .01_r8        ! meters per cm

  !-----------------------------------------------------------------------
  !  common formats for formatted output
  !-----------------------------------------------------------------------

  character (1), parameter :: char_delim = ','
  character (9), parameter :: delim_fmt  = "(72('-'))"
  character (9), parameter :: ndelim_fmt = "(72('='))"
  character (5), parameter :: blank_fmt  = "(' ')"

  !-----------------------------------------------------------------------------
  !   epsilon values
  !-----------------------------------------------------------------------------

   real(kind=r8), parameter :: &
      epsC      = 1.00e-8, & ! small C concentration (mmol C/m^3)
      epsTinv   = 3.17e-8    ! small inverse time scale (1/year) (1/sec)

  !-----------------------------------------------------------------------------
  !   floating point constants used across marbl
  !-----------------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
       spd = 86400.0_r8,        & ! number of seconds in a day
       dps = c1 / spd,          & ! number of days in a second
       yps = c1 / (365.0_r8*spd)  ! number of years in a second


  !-----------------------------------------------------------------------------
  !   Redfield Ratios, dissolved & particulate
  !-----------------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
       parm_Red_D_C_P  = 117.0_r8,                 & ! carbon:phosphorus
       parm_Red_D_N_P  =  16.0_r8,                 & ! nitrogen:phosphorus
       parm_Red_D_O2_P = 170.0_r8,                 & ! oxygen:phosphorus
       parm_Remin_D_O2_P = 138.0_r8,               & ! oxygen:phosphorus
       parm_Red_P_C_P  = parm_Red_D_C_P,                 & ! carbon:phosphorus
       parm_Red_D_C_N  = parm_Red_D_C_P/parm_Red_D_N_P,  & ! carbon:nitrogen
       parm_Red_P_C_N  = parm_Red_D_C_N,                 & ! carbon:nitrogen
       parm_Red_D_C_O2 = parm_Red_D_C_P/parm_Red_D_O2_P, & ! carbon:oxygen
       parm_Remin_D_C_O2 = parm_Red_D_C_P/parm_Remin_D_O2_P, & ! carbon:oxygen
       parm_Red_P_C_O2 = parm_Red_D_C_O2,                & ! carbon:oxygen
       parm_Red_Fe_C   = 3.0e-6_r8,                & ! iron:carbon
       parm_Red_D_C_O2_diaz = parm_Red_D_C_P/150.0_r8! carbon:oxygen
                                                           ! for diazotrophs

  !----------------------------------------------------------------------------
  !   marbl parameters accessible via namelist input
  !----------------------------------------------------------------------------

  REAL(KIND=r8) :: &
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

  REAL(KIND=r8), DIMENSION(4) :: &
       parm_scalelen_z,       & ! depths of prescribed scalelen values
       parm_scalelen_vals       ! prescribed scalelen values

  !---------------------------------------------------------------------
  !     Misc. Rate constants
  !---------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
       fe_scavenge_thres1 = 0.8e-3_r8,  & !upper thres. for Fe scavenging
       dust_fescav_scale  = 1.0e9         !dust scavenging scale factor

  !---------------------------------------------------------------------
  !     Compute iron remineralization and flux out.
  !     dust remin gDust = 0.035 gFe      mol Fe     1e9 nmolFe
  !                        --------- *  ---------- * ----------
  !			    gDust       55.847 gFe     molFe
  !
  !     dust_to_Fe          conversion - dust to iron (nmol Fe/g Dust) 
  !---------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
       dust_to_Fe=0.035_r8/55.847_r8*1.0e9_r8
 
  !----------------------------------------------------------------------------
  !     Partitioning of phytoplankton growth, grazing and losses
  !
  !     All f_* variables are fractions and are non-dimensional
  !----------------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
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

  !----------------------------------------------------------------------------
  !     fixed ratios
  !----------------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
       r_Nfix_photo=1.25_r8         ! N fix relative to C fix (non-dim)

  !-----------------------------------------------------------------------
  !     SET FIXED RATIOS for N/C, P/C, SiO3/C, Fe/C
  !     assumes C/N/P of 117/16/1 based on Anderson and Sarmiento, 1994
  !     for diazotrophs a N/P of 45 is assumed based on Letelier & Karl, 1998
  !-----------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
      Q             = 0.137_r8,  & !N/C ratio (mmol/mmol) of phyto & zoo
      Qp_zoo_pom    = 0.00855_r8,& !P/C ratio (mmol/mmol) zoo & pom
      Qfe_zoo       = 3.0e-6_r8, & !zooplankton fe/C ratio
      gQsi_0        = 0.137_r8,  & !initial Si/C ratio
      gQsi_max      = 0.685_r8,  & !max Si/C ratio
      gQsi_min      = 0.0457_r8, & !min Si/C ratio
      QCaCO3_max    = 0.4_r8,    & !max QCaCO3
      ! carbon:nitrogen ratio for denitrification
      denitrif_C_N  = parm_Red_D_C_P/136.0_r8

  !----------------------------------------------------------------------------
  !     loss term threshold parameters, chl:c ratios
  !----------------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
      thres_z1_auto     =  80.0e2_r8, & !autotroph threshold = C_loss_thres for z shallower than this (cm)
      thres_z2_auto     = 120.0e2_r8, & !autotroph threshold = 0 for z deeper than this (cm)
      thres_z1_zoo      = 110.0e2_r8, & !zooplankton threshold = C_loss_thres for z shallower than this (cm)
      thres_z2_zoo      = 150.0e2_r8, & !zooplankton threshold = 0 for z deeper than this (cm)
      CaCO3_temp_thres1 = 4.0_r8,   & !upper temp threshold for CaCO3 prod
      CaCO3_temp_thres2 = -2.0_r8,  & !lower temp threshold
      CaCO3_sp_thres    = 2.5_r8      ! bloom condition thres (mmolC/m3)

  !---------------------------------------------------------------------
  !     grazing functions
  !---------------------------------------------------------------------

  INTEGER (INT_KIND), PARAMETER ::   &
         grz_fnc_michaelis_menten = 1,       &
         grz_fnc_sigmoidal        = 2

  !---------------------------------------------------------------------
  !     fraction of incoming shortwave assumed to be PAR
  !---------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
       f_qsw_par = 0.45_r8   ! PAR fraction

        
  !---------------------------------------------------------------------
  !     Temperature parameters
  !---------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
       Tref = 30.0_r8, & ! reference temperature (C)
       Q_10 = 1.7_r8     ! factor for temperature dependence (non-dim)

  !---------------------------------------------------------------------
  !  DOM parameters for refractory components and DOP uptake
  !---------------------------------------------------------------------

  REAL(KIND=r8), PARAMETER :: &
       DOC_reminR_light  = (c1/(365.0_r8*15.0_r8)) * dps, & ! remin rate for semi-labile DOC, 1/15yr
       DON_reminR_light  = (c1/(365.0_r8*15.0_r8)) * dps, & ! remin rate for semi-labile DON, 1/15yr
       DOP_reminR_light  = (c1/(365.0_r8*60.0_r8)) * dps, & ! remin rate for semi-labile DOP, 1/60yr
       DOC_reminR_dark   = (c1/(365.0_r8*6.0_r8)) * dps,  & ! remin rate in the dark, 1/6yr
       DON_reminR_dark   = (c1/(365.0_r8*5.5_r8)) * dps,  & ! remin rate in the dark, 1/5.5yr
       DOP_reminR_dark   = (c1/(365.0_r8*4.5_r8)) * dps     ! remin rate in the dark, 1/4.5yr

  REAL(KIND=r8), PARAMETER :: &
       DOCr_reminR0      = (c1/(365.0_r8*16000.0_r8)) * dps, & ! remin rate for refractory DOC, 1/16000yr
       DONr_reminR0      = (c1/(365.0_r8*9500.0_r8)) * dps,  & ! remin rate for refractory DON, 1/9500yr
       DOPr_reminR0      = (c1/(365.0_r8*5500.0_r8)) * dps,  & ! remin rate for refractory DOP, 1/5500yr
       DOMr_reminR_photo = (c1/(365.0_r8*18.0_r8)) * dps       ! additional remin from photochemistry, 1/18yrs over top 10m

  REAL(KIND=r8), PARAMETER :: &
       DOCprod_refract  = 0.01_r8,                   & ! fraction of DOCprod to refractory pool
       DONprod_refract  = 0.0115_r8,                 & ! fraction of DONprod to refractory pool
       DOPprod_refract  = 0.003_r8,                  & ! fraction of DOPprod to refractory pool
       POCremin_refract = DOCprod_refract * 0.06_r8, & ! fraction of POCremin to refractory pool
       PONremin_refract = DONprod_refract * 0.03_r8, & ! fraction of POCremin to refractory pool
       POPremin_refract = DOPprod_refract * 0.06_r8, & ! fraction of POCremin to refractory pool
       DOCriv_refract   = 0.2_r8,                    & ! fraction of DOC river input to refractory pool
       DONriv_refract   = 0.1_r8,                    & ! fraction of DON river input to refractory pool
       DOPriv_refract   = 0.025_r8                     ! fraction of DOP river input to refractory pool

  !*****************************************************************************

  public :: &
       marbl_params_init, &
       marbl_params_print

  private :: &
       marbl_params_set_defaults
  
contains

  !*****************************************************************************

  subroutine marbl_params_set_defaults()
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

  end subroutine marbl_params_set_defaults

  !*****************************************************************************

  subroutine marbl_params_init(nl_buffer, marbl_status_log)

    use marbl_namelist_mod, only : marbl_nl_cnt
    use marbl_namelist_mod, only : marbl_nl_buffer_size
    use marbl_namelist_mod, only : marbl_namelist
    use marbl_logging     , only: marbl_log_type

    implicit none

    character(marbl_nl_buffer_size), dimension(marbl_nl_cnt), intent(in) :: nl_buffer
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    CHARACTER(LEN=*), PARAMETER :: subname = 'marbl_parms:marbl_parms_init'
    character(len=marbl_nl_buffer_size) :: tmp_nl_buffer

    integer(kind=int_kind) :: io_error

    NAMELIST /ecosys_parms_nml/ &
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

    call marbl_params_set_defaults()

    !---------------------------------------------------------------------------
    ! read in namelist to override some defaults
    !---------------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'ecosys_parms_nml')
    read(tmp_nl_buffer, nml=ecosys_parms_nml, iostat=io_error)
    if (io_error /= 0) then
       call marbl_status_log%log_error("Error reading ecosys_parms_nml", subname)
       return
    else
       ! FIXME #16: this is printing contents of pop_in, not the entire ecosys_parms_nml
      call marbl_status_log%log_namelist('ecosys_parms_nml', tmp_nl_buffer, subname)
    end if

  end subroutine marbl_params_init

  !*****************************************************************************

  subroutine marbl_params_print(stdout)
    ! echo all parameters to the specified output file

    implicit none

    integer, intent(in) :: stdout

    integer :: zoo_ind, auto_ind, prey_ind
    
    !---------------------------------------------------------------------------

    write(stdout, *) '----------------------------------------'
    write(stdout, *) '----- marbl_parms namelist values -----'
    write(stdout, *) 'parm_Fe_bioavail       = ', parm_Fe_bioavail
    write(stdout, *) 'parm_o2_min            = ', parm_o2_min
    write(stdout, *) 'parm_o2_min_delta      = ', parm_o2_min_delta
    write(stdout, *) 'parm_kappa_nitrif      = ', parm_kappa_nitrif
    write(stdout, *) 'parm_nitrif_par_lim    = ', parm_nitrif_par_lim
    write(stdout, *) 'parm_labile_ratio      = ', parm_labile_ratio
    write(stdout, *) 'parm_POMbury           = ', parm_POMbury
    write(stdout, *) 'parm_BSIbury           = ', parm_BSIbury
    write(stdout, *) 'parm_fe_scavenge_rate0 = ', parm_fe_scavenge_rate0
    write(stdout, *) 'parm_f_prod_sp_CaCO3   = ', parm_f_prod_sp_CaCO3
    write(stdout, *) 'parm_POC_diss          = ', parm_POC_diss
    write(stdout, *) 'parm_SiO2_diss         = ', parm_SiO2_diss
    write(stdout, *) 'parm_CaCO3_diss        = ', parm_CaCO3_diss
    write(stdout, *) 'fe_max_scale2          = ', fe_max_scale2
    write(stdout, *) 'parm_scalelen_z        = ', parm_scalelen_z
    write(stdout, *) 'parm_scalelen_vals     = ', parm_scalelen_vals

    do zoo_ind = 1, zooplankton_cnt
       write(stdout, *) 'lname('      , trim(zooplankton(zoo_ind)%sname), ') = ', zooplankton(zoo_ind)%lname
       write(stdout, *) 'z_mort_0('   , trim(zooplankton(zoo_ind)%sname), ') = ', zooplankton(zoo_ind)%z_mort_0
       write(stdout, *) 'z_mort2_0('  , trim(zooplankton(zoo_ind)%sname), ') = ', zooplankton(zoo_ind)%z_mort2_0
       write(stdout, *) 'loss_thres(' , trim(zooplankton(zoo_ind)%sname), ') = ', zooplankton(zoo_ind)%loss_thres
    end do
    
    do auto_ind = 1, autotroph_cnt
       write(stdout, *) 'lname('         , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%lname
       write(stdout, *) 'Nfixer('        , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%Nfixer
       write(stdout, *) 'imp_calcifier(' , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%imp_calcifier
       write(stdout, *) 'exp_calcifier(' , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%exp_calcifier
       write(stdout, *) 'kFe('           , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%kFe
       write(stdout, *) 'kPO4('          , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%kPO4
       write(stdout, *) 'kDOP('          , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%kDOP
       write(stdout, *) 'kNO3('          , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%kNO3
       write(stdout, *) 'kNH4('          , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%kNH4
       write(stdout, *) 'kSiO3('         , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%kSiO3
       write(stdout, *) 'Qp('            , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%Qp
       write(stdout, *) 'gQfe_0('        , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%gQfe_0
       write(stdout, *) 'gQfe_min('      , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%gQfe_min
       write(stdout, *) 'alphaPI('       , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%alphaPI
       write(stdout, *) 'PCref('         , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%PCref
       write(stdout, *) 'thetaN_max('    , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%thetaN_max
       write(stdout, *) 'loss_thres('    , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%loss_thres
       write(stdout, *) 'loss_thres2('   , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%loss_thres2
       write(stdout, *) 'temp_thres('    , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%temp_thres
       write(stdout, *) 'mort('          , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%mort
       write(stdout, *) 'mort2('         , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%mort2
       write(stdout, *) 'agg_rate_max('  , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%agg_rate_max
       write(stdout, *) 'agg_rate_min('  , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%agg_rate_min
       write(stdout, *) 'loss_poc('      , trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%loss_poc
    end do

    
    do prey_ind = 1, grazer_prey_cnt
       do zoo_ind = 1, zooplankton_cnt
          associate(grazer => grazing(prey_ind, zoo_ind))
            write(stdout, *) 'lname(', trim(grazer%sname), ') = '            , grazer%lname
            write(stdout, *) 'auto_ind(', trim(grazer%sname), ') = '         , grazer%auto_ind
            write(stdout, *) 'auto_ind_cnt(', trim(grazer%sname), ') = '     , grazer%auto_ind_cnt
            write(stdout, *) 'zoo_ind(', trim(grazer%sname), ') = '          , grazer%zoo_ind
            write(stdout, *) 'zoo_ind_cnt(', trim(grazer%sname), ') = '      , grazer%zoo_ind_cnt
            write(stdout, *) 'z_umax_0(', trim(grazer%sname), ') = '         , grazer%z_umax_0
            write(stdout, *) 'z_grz(', trim(grazer%sname), ') = '            , grazer%z_grz
            write(stdout, *) 'graze_zoo(', trim(grazer%sname), ') = '        , grazer%graze_zoo
            write(stdout, *) 'graze_poc(', trim(grazer%sname), ') = '        , grazer%graze_poc
            write(stdout, *) 'graze_doc(', trim(grazer%sname), ') = '        , grazer%graze_doc
            write(stdout, *) 'f_zoo_detr(', trim(grazer%sname), ') = '       , grazer%f_zoo_detr
            write(stdout, *) 'grazing_function(', trim(grazer%sname), ') = ' , grazer%grazing_function
          end associate
       end do
    end do

    write(stdout, *) '----------------------------------------'

  end subroutine marbl_params_print

  !*****************************************************************************

end module marbl_parms
