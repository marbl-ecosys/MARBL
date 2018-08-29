! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_co2calc_mod

  !-----------------------------------------------------------------------------
  ! based upon OCMIP2 co2calc
  !-----------------------------------------------------------------------------

  use marbl_kinds_mod     , only : int_kind, r8, log_kind, char_len
  use marbl_logging       , only : marbl_log_type
  use marbl_constants_mod , only : p001,c3, c10
  use marbl_constants_mod , only : c0, p5, c1, c2, c1000, T0_Kelvin, rho_sw

  implicit none

  !-----------------------------------------------------------------------------
  !   public/private declarations
  !-----------------------------------------------------------------------------

  public  :: marbl_co2calc_surface
  public  :: marbl_co2calc_interior
  public  :: marbl_co2calc_co3_sat_vals

  private :: comp_htotal
  private :: comp_co2calc_coeffs
  private :: drtsafe
  private :: total_alkalinity
  private :: apply_pressure_correction

  !-----------------------------------------------------------------------------
  !   module parameters
  !-----------------------------------------------------------------------------

  !   The current setting of xacc, a tolerance critera, will result in co2star
  !   being accurate to 3 significant figures (xx.y). Making xacc bigger will
  !   result in faster convergence also, but this is not recommended (xacc of
  !   10**-9 drops precision to 2 significant figures).

  real(kind=r8)          , parameter :: xacc = 1e-10_r8
  integer(kind=int_kind) , parameter :: max_bracket_grow_it = 3
  integer(kind=int_kind) , parameter :: maxit = 100

  real(kind=r8)          , parameter :: salt_min = 0.1_r8
  real(kind=r8)          , parameter :: dic_min  = salt_min / 35.0_r8 * 1944.0_r8
  real(kind=r8)          , parameter :: alk_min  = salt_min / 35.0_r8 * 2225.0_r8

  !-----------------------------------------------------------------------------
  !   declarations for function coefficients & species concentrations
  !-----------------------------------------------------------------------------

  type, public :: co2calc_coeffs_type
     real(kind=r8) :: k0  ! equilibrium constants for CO2 species
     real(kind=r8) :: k1  ! equilibrium constants for CO2 species
     real(kind=r8) :: k2  ! equilibrium constants for CO2 species
     real(kind=r8) :: ff  ! fugacity of CO2
     real(kind=r8) :: kw  ! equilibrium coefficient of water
     real(kind=r8) :: kb
     real(kind=r8) :: ks
     real(kind=r8) :: kf
     real(kind=r8) :: k1p
     real(kind=r8) :: k2p
     real(kind=r8) :: k3p
     real(kind=r8) :: ksi
     real(kind=r8) :: bt
     real(kind=r8) :: st
     real(kind=r8) :: ft
  end type co2calc_coeffs_type

  type, public :: co2calc_state_type
     real(kind=r8) :: dic  ! total dissolved inorganic carbon
     real(kind=r8) :: ta   ! total alkalinity
     real(kind=r8) :: pt   ! total phosphorous
     real(kind=r8) :: sit  ! total silicon
     real(kind=r8) :: temp ! temperature (for error reporting)
     real(kind=r8) :: salt ! salinity (for error reporting)
  end type co2calc_state_type

  !*****************************************************************************

contains

  !*****************************************************************************

  subroutine marbl_co2calc_surface( &
       num_elements,                &
       lcomp_co2calc_coeffs,        &
       dic_in,                      &
       xco2_in,                     &
       ta_in,                       &
       pt_in,                       &
       sit_in,                      &
       temp,                        &
       salt,                        &
       atmpres,                     &
       co2calc_coeffs,              &
       co2calc_state,               &
       co3,                         &
       co2star,                     &
       dco2star,                    &
       pco2surf,                    &
       dpco2,                       &
       phlo,                        &
       phhi,                        &
       ph,                          &
       marbl_status_log)

    !---------------------------------------------------------------------------
    ! Calculate delta co2*, etc. from total alkalinity, total CO2, temp,
    ! salinity (s), etc.
    !---------------------------------------------------------------------------

    integer(kind=int_kind)        , intent(in)    :: num_elements
    logical(kind=log_kind)        , intent(in)    :: lcomp_co2calc_coeffs
    real(kind=r8)                 , intent(in)    :: dic_in(num_elements)   ! total inorganic carbon (nmol/cm^3)
    real(kind=r8)                 , intent(in)    :: xco2_in(num_elements)  ! atmospheric mole fraction CO2 in dry air (ppmv)
    real(kind=r8)                 , intent(in)    :: ta_in(num_elements)    ! total alkalinity (neq/cm^3)
    real(kind=r8)                 , intent(in)    :: pt_in(num_elements)    ! inorganic phosphate (nmol/cm^3)
    real(kind=r8)                 , intent(in)    :: sit_in(num_elements)   ! inorganic silicate (nmol/cm^3)
    real(kind=r8)                 , intent(in)    :: temp(num_elements)     ! temperature (degC)
    real(kind=r8)                 , intent(in)    :: salt(num_elements)     ! salinity (PSU)
    real(kind=r8)                 , intent(in)    :: atmpres(num_elements)  ! atmospheric pressure (atmosphere)
    real(kind=r8)                 , intent(inout) :: phlo(num_elements)     ! lower limit of ph range
    real(kind=r8)                 , intent(inout) :: phhi(num_elements)     ! upper limit of ph range
    real(kind=r8)                 , intent(out)   :: ph(num_elements)       ! computed ph values, for initial guess on next time step
    real(kind=r8)                 , intent(out)   :: co3(num_elements)      ! Carbonate Ion Concentration
    type(co2calc_coeffs_type)     , intent(inout) :: co2calc_coeffs(num_elements)
    type(co2calc_state_type)      , intent(inout) :: co2calc_state(num_elements)
    real(kind=r8)                 , intent(out)   :: co2star(num_elements)  ! CO2*water (nmol/cm^3)
    real(kind=r8)                 , intent(out)   :: dco2star(num_elements) ! delta CO2 (nmol/cm^3)
    real(kind=r8)                 , intent(out)   :: pco2surf(num_elements) ! oceanic pCO2 (ppmv)
    real(kind=r8)                 , intent(out)   :: dpco2(num_elements)    ! Delta pCO2, i.e, pCO2ocn - pCO2atm (ppmv)
    type(marbl_log_type), optional, intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_co2calc_mod:marbl_co2calc_surface'

    type(co2calc_state_type) :: co2calc_state_in(num_elements)
    integer(kind=int_kind)   :: n
    real(kind=r8)            :: mass_to_vol          ! (mol/kg) -> (mmol/m^3)
    real(kind=r8)            :: vol_to_mass          ! (mmol/m^3) -> (mol/kg)
    real(kind=r8)            :: co2starair           ! co2star saturation
    real(kind=r8)            :: htotal2, denom
    real(kind=r8)            :: xco2(num_elements)   ! atmospheric CO2 (atm)
    real(kind=r8)            :: htotal(num_elements) ! free concentration of H ion
    !---------------------------------------------------------------------------

    ! temp and salt are not used out of co2calc_state at this time but are
    ! set here to avoid having co2calc_state%temp and %salt uninitialized
    co2calc_state(:)%temp = temp
    co2calc_state(:)%salt = salt

    co2calc_state_in(:)%dic  = dic_in
    co2calc_state_in(:)%ta   = ta_in
    co2calc_state_in(:)%pt   = pt_in
    co2calc_state_in(:)%sit  = sit_in
    co2calc_state_in(:)%temp = temp
    co2calc_state_in(:)%salt = salt

    associate(                        &
         k1  => co2calc_coeffs(:)%k1, &
         k2  => co2calc_coeffs(:)%k2, &
         ff  => co2calc_coeffs(:)%ff, &
         dic => co2calc_state(:)%dic  &
         )

    !---------------------------------------------------------------------------
    !   set unit conversion factors
    !---------------------------------------------------------------------------

    mass_to_vol = 1e6_r8 * rho_sw
    vol_to_mass = c1 / mass_to_vol

    !---------------------------------------------------------------------------
    !   compute thermodynamic CO3 coefficients
    !---------------------------------------------------------------------------

    if (lcomp_co2calc_coeffs) then
       call comp_co2calc_coeffs(num_elements, co2calc_state_in, co2calc_coeffs)
    end if

    !---------------------------------------------------------------------------
    !   compute htotal
    !---------------------------------------------------------------------------

    call comp_htotal(num_elements, num_elements, co2calc_state_in,            &
                     co2calc_coeffs, co2calc_state, phlo, phhi, htotal,       &
                     marbl_status_log)

    if (present (marbl_status_log)) then
       if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace("comp_htotal()", subname)
          return
       end if
    end if

    !---------------------------------------------------------------------------
    !   convert xco2 from uatm to atm
    !---------------------------------------------------------------------------

    xco2(:) = xco2_in(:) * 1e-6_r8

    !---------------------------------------------------------------------------
    !   Calculate [CO2*] as defined in DOE Methods Handbook 1994 Ver.2,
    !   ORNL/CDIAC-74, Dickson and Goyet, eds. (Ch 2 p 10, Eq A.49)
    !
    !   Compute co2starair
    !---------------------------------------------------------------------------

    do n = 1, num_elements
       htotal2     = htotal(n) ** 2
       denom       = c1 / (htotal2 + k1(n) * htotal(n) + k1(n) * k2(n))
       CO3(n)      = dic(n) * k1(n) * k2(n) * denom
       co2star(n)  = dic(n) * htotal2 / (htotal2 + k1(n) * htotal(n) + k1(n) * k2(n))
       co2starair  = xco2(n) * ff(n) * atmpres(n)
       dco2star(n) = co2starair - co2star(n)
       ph(n)       = -log10(htotal(n))

       !---------------------------------------------------------------------
       !   Add two output arguments for storing pCO2surf
       !   Should we be using K0 or ff for the solubility here?
       !---------------------------------------------------------------------

       pCO2surf(n) = co2star(n) / ff(n)
       dpCO2(n)    = pCO2surf(n) - xco2(n) * atmpres(n)

       !---------------------------------------------------------------------
       !   Convert units of output arguments
       !   Note: pCO2surf and dpCO2 are calculated in atm above.
       !---------------------------------------------------------------------

       CO3(n)      = CO3(n)      * mass_to_vol
       co2star(n)  = co2star(n)  * mass_to_vol
       dco2star(n) = dco2star(n) * mass_to_vol

       pCO2surf(n) = pCO2surf(n) * 1e6_r8
       dpCO2(n)    = dpCO2(n)    * 1e6_r8
    end do

    end associate

  end subroutine marbl_co2calc_surface

  !***********************************************************************

  subroutine marbl_co2calc_interior(&
       num_elements, num_active_elements, lcomp_co2calc_coeffs,   &
       co2calc_coeffs,  co2calc_state, temp, salt, press_bar, dic_in, ta_in, pt_in, &
       sit_in, phlo, phhi, ph, H2CO3, HCO3, CO3, marbl_status_log)

    !---------------------------------------------------------------------------
    ! Calculate H2CO3, HCO3, CO3 from total alkalinity, total CO2, temp, salinity (s), etc.
    !---------------------------------------------------------------------------

    integer(kind=int_kind)    , intent(in)    :: num_elements
    integer(kind=int_kind)    , intent(in)    :: num_active_elements
    logical(kind=log_kind)    , intent(in)    :: lcomp_co2calc_coeffs
    real(kind=r8)             , intent(in)    :: temp(num_elements)      ! temperature (degC)
    real(kind=r8)             , intent(in)    :: salt(num_elements)      ! salinity (PSU)
    real(kind=r8)             , intent(in)    :: press_bar(num_elements) ! pressure at level (bars)
    real(kind=r8)             , intent(in)    :: dic_in(num_elements)    ! total inorganic carbon (nmol/cm^3)
    real(kind=r8)             , intent(in)    :: ta_in(num_elements)     ! total alkalinity (neq/cm^3)
    real(kind=r8)             , intent(in)    :: pt_in(num_elements)     ! inorganic phosphate (nmol/cm^3)
    real(kind=r8)             , intent(in)    :: sit_in(num_elements)    ! inorganic silicate (nmol/cm^3)
    type(co2calc_coeffs_type) , intent(inout) :: co2calc_coeffs(num_elements)
    type(co2calc_state_type)  , intent(inout) :: co2calc_state(num_elements)
    real(kind=r8)             , intent(inout) :: phlo(num_elements)      ! lower limit of pH range
    real(kind=r8)             , intent(inout) :: phhi(num_elements)      ! upper limit of pH range
    real(kind=r8)             , intent(out)   :: pH(num_elements)        ! computed ph values, for initial guess on next time step
    real(kind=r8)             , intent(out)   :: H2CO3(num_elements)     ! Carbonic Acid Concentration
    real(kind=r8)             , intent(out)   :: HCO3(num_elements)      ! Bicarbonate Ion Concentration
    real(kind=r8)             , intent(out)   :: CO3(num_elements)       ! Carbonate Ion Concentration
    type(marbl_log_type)      , intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_co2calc_mod:marbl_co2calc_interior'

    type(co2calc_state_type) :: co2calc_state_in(num_elements)
    integer(kind=int_kind)   :: c
    real(kind=r8)            :: mass_to_vol          ! (mol/kg) -> (mmol/m^3)
    real(kind=r8)            :: vol_to_mass          ! (mmol/m^3) -> (mol/kg)
    real(kind=r8)            :: htotal2, denom
    real(kind=r8)            :: htotal(num_elements) ! free concentration of H ion
    !---------------------------------------------------------------------------

    ! temp and salt are not used out of co2calc_state at this time but are
    ! set here to avoid having co2calc_state%temp and %salt uninitialized
    co2calc_state(:)%temp = temp
    co2calc_state(:)%salt = salt

    co2calc_state_in(:)%dic = dic_in
    co2calc_state_in(:)%ta  = ta_in
    co2calc_state_in(:)%pt  = pt_in
    co2calc_state_in(:)%sit = sit_in
    co2calc_state_in(:)%temp = temp
    co2calc_state_in(:)%salt = salt

    associate(                         &
         k0  => co2calc_coeffs(:)%k0,  &
         k1  => co2calc_coeffs(:)%k1,  &
         k2  => co2calc_coeffs(:)%k2,  &
         ff  => co2calc_coeffs(:)%ff,  &
         kw  => co2calc_coeffs(:)%kw,  &
         kb  => co2calc_coeffs(:)%kb,  &
         ks  => co2calc_coeffs(:)%ks,  &
         kf  => co2calc_coeffs(:)%kf,  &
         k1p => co2calc_coeffs(:)%k1p, &
         k2p => co2calc_coeffs(:)%k2p, &
         ksi => co2calc_coeffs(:)%ksi, &
         bt  => co2calc_coeffs(:)%bt,  &
         st  => co2calc_coeffs(:)%st,  &
         ft  => co2calc_coeffs(:)%ft,  &
         dic => co2calc_state(:)%dic,  &
         ta  => co2calc_state(:)%ta,   &
         pt  => co2calc_state(:)%pt,   &
         sit => co2calc_state(:)%sit   &
         )

    !---------------------------------------------------------------------------
    !   set unit conversion factors
    !---------------------------------------------------------------------------

    mass_to_vol = 1e6_r8 * rho_sw
    vol_to_mass = c1 / mass_to_vol

    !------------------------------------------------------------------------
    !   compute thermodynamic CO3 coefficients
    !------------------------------------------------------------------------

    if (lcomp_co2calc_coeffs) then
       call comp_co2calc_coeffs(num_elements, co2calc_state_in, co2calc_coeffs, press_bar)
    end if

    !------------------------------------------------------------------------
    !   compute htotal
    !------------------------------------------------------------------------

    call comp_htotal(num_elements, num_active_elements, co2calc_state_in,     &
                     co2calc_coeffs, co2calc_state, phlo, phhi, htotal,       &
                     marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace("comp_htotal()", subname)
       return
    end if

    !------------------------------------------------------------------------
    !   Calculate [CO2*] as defined in DOE Methods Handbook 1994 Ver.2,
    !   ORNL/CDIAC-74, Dickson and Goyet, eds. (Ch 2 p 10, Eq A.49-51)
    !------------------------------------------------------------------------

    do c = 1,num_active_elements
       htotal2  = htotal(c) ** 2
       denom    = c1 / (htotal2 + k1(c) * htotal(c) + k1(c) * k2(c))
       H2CO3(c) = dic(c) * htotal2 * denom
       HCO3(c)  = dic(c) * k1(c) * htotal(c) * denom
       CO3(c)   = dic(c) * k1(c) * k2(c) * denom
       ph(c)    = -log10(htotal(c))

       !------------------------------------------------------------------
       !   Convert units of output arguments
       !------------------------------------------------------------------

       H2CO3(c) = H2CO3(c) * mass_to_vol
       HCO3(c)  = HCO3(c) * mass_to_vol
       CO3(c)   = CO3(c) * mass_to_vol
     end do ! c loop

     ph(num_active_elements+1:num_elements)    = c0
     H2CO3(num_active_elements+1:num_elements) = c0
     HCO3(num_active_elements+1:num_elements)  = c0
     CO3(num_active_elements+1:num_elements)   = c0

    end associate

  end subroutine marbl_co2calc_interior

  !*****************************************************************************

  subroutine comp_co2calc_coeffs(num_elements, co2calc_state_in, co2calc_coeffs, press_bar)

    !---------------------------------------------------------------------------
    ! FIXME #20: the computations for the individual constants need to
    !            be broken out into separate functions and unit tested
    !---------------------------------------------------------------------------

    integer(kind=int_kind)    , intent(in)  :: num_elements
    type(co2calc_state_type)  , intent(in)  :: co2calc_state_in(num_elements)
    type(co2calc_coeffs_type) , intent(out) :: co2calc_coeffs(num_elements)
    real(kind=r8), optional   , intent(in)  :: press_bar(num_elements) ! pressure at level (bars)

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------
    real(kind=r8), dimension(num_elements) :: salt_lim ! bounded salt
    real(kind=r8), dimension(num_elements) :: tk       ! temperature (K)
    real(kind=r8), dimension(num_elements) :: is       ! ionic strength
    real(kind=r8), dimension(num_elements) :: scl      ! chlorinity
    real(kind=r8), dimension(num_elements) :: tk100
    real(kind=r8), dimension(num_elements) :: tk1002
    real(kind=r8), dimension(num_elements) :: invtk
    real(kind=r8), dimension(num_elements) :: dlogtk
    real(kind=r8), dimension(num_elements) :: is2
    real(kind=r8), dimension(num_elements) :: sqrtis
    real(kind=r8), dimension(num_elements) :: s2
    real(kind=r8), dimension(num_elements) :: sqrts
    real(kind=r8), dimension(num_elements) :: invRtk
    real(kind=r8), dimension(num_elements) :: arg
    real(kind=r8), dimension(num_elements) :: log_1_m_1p005em3_s
    real(kind=r8), dimension(num_elements) :: log_1_p_tot_sulfate_div_ks
    !---------------------------------------------------------------------------

    associate(                         &
         k0  => co2calc_coeffs(:)%k0,  &
         k1  => co2calc_coeffs(:)%k1,  &
         k2  => co2calc_coeffs(:)%k2,  &
         ff  => co2calc_coeffs(:)%ff,  &
         kw  => co2calc_coeffs(:)%kw,  &
         kb  => co2calc_coeffs(:)%kb,  &
         ks  => co2calc_coeffs(:)%ks,  &
         kf  => co2calc_coeffs(:)%kf,  &
         k1p => co2calc_coeffs(:)%k1p, &
         k2p => co2calc_coeffs(:)%k2p, &
         k3p => co2calc_coeffs(:)%k3p, &
         ksi => co2calc_coeffs(:)%ksi, &
         bt  => co2calc_coeffs(:)%bt,  &
         st  => co2calc_coeffs(:)%st,  &
         ft  => co2calc_coeffs(:)%ft,  &
         temp => co2calc_state_in(:)%temp, &
         salt => co2calc_state_in(:)%salt  &
         )

    !---------------------------------------------------------------------------
    !   Calculate all constants needed to convert between various
    !   measured carbon species. References for each equation are
    !   noted in the code.  Once calculated, the constants are stored
    !   and passed in the common block "const". The original version
    !   of this code was based on the code by Dickson in Version 2 of
    !   "Handbook of Methods for the Analysis of the Various Parameters
    !   of the Carbon Dioxide System in Seawater", DOE, 1994 (SOP No. 3,
    !   p25-26).
    !   Derive simple terms used more than once
    !---------------------------------------------------------------------------

    salt_lim = max(salt,salt_min)
    tk       = T0_Kelvin + temp
    tk100    = tk * 1e-2_r8
    tk1002   = tk100 * tk100
    invtk    = c1 / tk
    dlogtk   = log(tk)
    invRtk   = (c1 / 83.1451_r8) * invtk

    is       = 19.924_r8 * salt_lim / (c1000 - 1.005_r8 * salt_lim)
    is2      = is * is
    sqrtis   = sqrt(is)
    sqrts    = sqrt(salt_lim)
    s2       = salt_lim * salt_lim
    scl      = salt_lim / 1.80655_r8

    arg = c1 - 0.001005_r8 * salt_lim
    log_1_m_1p005em3_s = log(arg)

    !---------------------------------------------------------------------------
    !   f = k0(1-pH2O)*correction term for non-ideality
    !   Weiss & Price (1980, Mar. Chem., 8, 347-359;
    !                 Eq 13 with table 6 values)
    !---------------------------------------------------------------------------

    arg = -162.8301_r8 + 218.2968_r8 / tk100 + &
         90.9241_r8 * (dlogtk + log(1e-2_r8)) - 1.47696_r8 * tk1002 + &
         salt_lim * (.025695_r8 - .025225_r8 * tk100 + 0.0049867_r8 * tk1002)
    ff = exp(arg)

    !---------------------------------------------------------------------------
    !   K0 from Weiss 1974
    !---------------------------------------------------------------------------

    arg = 93.4517_r8 / tk100 - 60.2409_r8 + 23.3585_r8 * (dlogtk + log(1e-2_r8)) + &
         salt_lim * (.023517_r8 - 0.023656_r8 * tk100 + 0.0047036_r8 * tk1002)
    k0 = exp(arg)

    !---------------------------------------------------------------------------
    !   k1 = [H][HCO3]/[H2CO3]
    !   k2 = [H][CO3]/[HCO3]
    !      Lueker, Dickson, Keeling (2000) using Mehrbach et al. data on total scale
    !   pressure correction from Millero 1995, p. 675
    !      w/ typo corrections from CO2SYS
    !---------------------------------------------------------------------------

    ! total pH scale
    arg = 3633.86_r8 * invtk - 61.2172_r8 + &
          9.67770_r8 * dlogtk - 0.011555_r8 * salt_lim + &
          0.0001152_r8 * s2
    arg = -log(c10) * arg
    k1 = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-25.5_r8, 0.1271_r8, c0/), &
         Kappa_coefs = (/-3.08_r8, 0.0877_r8, c0/), &
         therm_coef = k1, &
         press_bar = press_bar)

    ! total pH scale
    arg = 471.78_r8 * invtk + 25.9290_r8 - &
          3.16967_r8 * dlogtk - 0.01781_r8 * salt_lim + 0.0001122_r8 * s2
    arg = -log(c10) * arg
    k2 = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-15.82_r8, -0.0219_r8, c0/), &
         Kappa_coefs = (/1.13_r8, -0.1475_r8, c0/), &
         therm_coef = k2, &
         press_bar = press_bar)

    !---------------------------------------------------------------------------
    !   kb = [H][BO2]/[HBO2]
    !   Millero p.669 (1995) using data from Dickson (1990)
    !   CO2SYS states that this in on total pH scale
    !   pressure correction from Millero 1979, p. 1657
    !      omitting salinity contribution
    !---------------------------------------------------------------------------

    arg = (-8966.90_r8 - 2890.53_r8 * sqrts - &
         77.942_r8 * salt_lim + 1.728_r8 * salt_lim * sqrts - &
         0.0996_r8 * s2) * invtk + &
         (148.0248_r8 + 137.1942_r8 * sqrts + 1.62142_r8 * salt_lim) + &
         (-24.4344_r8 - 25.085_r8 * sqrts - 0.2474_r8 * salt_lim) * dlogtk + &
         0.053105_r8 * sqrts * tk
    kb = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-29.48_r8, 0.1622_r8, -0.002608_r8/), &
         Kappa_coefs = (/-2.84_r8, c0, c0/), &
         therm_coef = kb, &
         press_bar = press_bar)

    !---------------------------------------------------------------------------
    !   k1p = [H][H2PO4]/[H3PO4]
    !   DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)
    !   pressure correction from Millero 1995, p. 675
    !      w/ typo corrections from CO2SYS
    !---------------------------------------------------------------------------

    arg = -4576.752_r8 * invtk + 115.525_r8 - &
         18.453_r8 * dlogtk + &
         (-106.736_r8 * invtk + 0.69171_r8) * sqrts + &
         (-0.65643_r8 * invtk - 0.01844_r8) * salt_lim
    k1p = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-14.51_r8, 0.1211_r8, -0.000321_r8/), &
         Kappa_coefs = (/-2.67_r8, 0.0427_r8, c0/), &
         therm_coef = k1p, &
         press_bar = press_bar)

    !---------------------------------------------------------------------------
    !   k2p = [H][HPO4]/[H2PO4]
    !   DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))
    !   pressure correction from Millero 1995, p. 675
    !      w/ typo corrections from CO2SYS
    !---------------------------------------------------------------------------

    arg = -8814.715_r8 * invtk + 172.0883_r8 - &
         27.927_r8 * dlogtk + &
         (-160.340_r8 * invtk + 1.3566_r8) * sqrts + &
         (0.37335_r8 * invtk - 0.05778_r8) * salt_lim
    k2p = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-23.12_r8, 0.1758_r8, -0.002647_r8/), &
         Kappa_coefs = (/-5.15_r8, 0.09_r8, c0/), &
         therm_coef = k2p, &
         press_bar = press_bar)

    !---------------------------------------------------------------------------
    !   k3p = [H][PO4]/[HPO4]
    !   DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)
    !   pressure correction from Millero 1995, p. 675
    !      w/ typo corrections from CO2SYS
    !---------------------------------------------------------------------------

    arg = -3070.75_r8 * invtk - 18.141_r8 + &
         (17.27039_r8 * invtk + 2.81197_r8) * sqrts + &
         (-44.99486_r8 * invtk - 0.09984_r8) * salt_lim
    k3p = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-26.57_r8, 0.202_r8, -0.003042_r8/), &
         Kappa_coefs = (/-4.08_r8, 0.0714_r8, c0/), &
         therm_coef = k3p, &
         press_bar = press_bar)

    !---------------------------------------------------------------------------
    !   ksi = [H][SiO(OH)3]/[Si(OH)4]
    !   Millero p.671 (1995) using data from Yao and Millero (1995)
    !   pressure correction from Millero 1995, p. 675
    !      w/ typo corrections from CO2SYS
    !      apply boric acid values
    !---------------------------------------------------------------------------

    arg = -8904.2_r8 * invtk + 117.385_r8 - &
         19.334_r8 * dlogtk + &
         (-458.79_r8 * invtk + 3.5913_r8) * sqrtis + &
         (188.74_r8 * invtk - 1.5998_r8) * is + &
         (-12.1652_r8 * invtk + 0.07871_r8) * is2 + &
         log_1_m_1p005em3_s
    ksi = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-29.48_r8, 0.1622_r8, -0.002608_r8/), &
         Kappa_coefs = (/-2.84_r8, c0, c0/), &
         therm_coef = ksi, &
         press_bar = press_bar)

    !---------------------------------------------------------------------------
    !   kw = [H][OH]
    !   Millero p.670 (1995) using composite data
    !   following DOE Handbook, 0.015 substracted from constant to
    !   approximately convert from SWS pH scale to total pH scale
    !   pressure correction from Millero 1983
    !      note that deltaV coeffs in Millero 1995 are those actually
    !      freshwater deltaV coeffs from Millero 1983
    !---------------------------------------------------------------------------

    arg = -13847.26_r8 * invtk + 148.9652_r8 - 23.6521_r8 * dlogtk + &
         (118.67_r8 * invtk - 5.977_r8 + 1.0495_r8 * dlogtk) * sqrts - &
         0.01615_r8 * salt_lim
    kw = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-20.02_r8, 0.1119_r8, -0.001409_r8/), &
         Kappa_coefs = (/-5.13_r8, 0.0794_r8, c0/), &
         therm_coef = kw, &
         press_bar = press_bar)

    !---------------------------------------------------------------------------
    !   ks = [H][SO4]/[HSO4], free pH scale
    !   Dickson (1990, J. chem. Thermodynamics 22, 113)
    !   pressure correction from Millero 1995, p. 675
    !      w/ typo corrections from CO2SYS
    !---------------------------------------------------------------------------

    arg = -4276.1_r8 * invtk + 141.328_r8 - 23.093_r8 * dlogtk + &
         (-13856.0_r8 * invtk + 324.57_r8 - 47.986_r8 * dlogtk) * sqrtis + &
         (35474.0_r8 * invtk - 771.54_r8 + 114.723_r8 * dlogtk) * is - &
         2698.0_r8 * invtk * is * sqrtis + &
         1776.0_r8 * invtk * is2 + &
         log_1_m_1p005em3_s
    ks  = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-18.03_r8, 0.0466_r8, 0.000316_r8/), &
         Kappa_coefs = (/-4.53_r8, 0.09_r8, c0/), &
         therm_coef = ks, &
         press_bar = press_bar)

    !---------------------------------------------------------------------
    !   kf = [H][F]/[HF]
    !   Dickson and Riley (1979) -- change pH scale to total
    !   pressure correction from Millero 1995, p. 675
    !      w/ typo corrections from CO2SYS
    !---------------------------------------------------------------------

    arg = c1 + (0.1400_r8 / 96.062_r8) * (scl) / ks(:)
    log_1_p_tot_sulfate_div_ks = log(arg)
    arg = 1590.2_r8 * invtk - 12.641_r8 + 1.525_r8 * sqrtis + &
         log_1_m_1p005em3_s + log_1_p_tot_sulfate_div_ks
    kf  = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-9.78_r8, -0.009_r8, -0.000942_r8/), &
         Kappa_coefs = (/-3.91_r8, 0.054_r8, c0/), &
         therm_coef = kf, &
         press_bar = press_bar)

    !---------------------------------------------------------------------
    !   Calculate concentrations for borate, sulfate, and fluoride
    !   bt : Uppstrom (1974)
    !   st : Morris & Riley (1966)
    !   ft : Riley (1965)
    !---------------------------------------------------------------------

    bt(:) = 0.000232_r8 / 10.811_r8 * scl
    st(:) = 0.14_r8 / 96.062_r8 * scl
    ft(:) = 0.000067_r8 / 18.9984_r8 * scl

    end associate

  end subroutine comp_co2calc_coeffs

  !*****************************************************************************

  subroutine comp_htotal(num_elements, num_active_elements, co2calc_state_in, &
                  co2calc_coeffs, co2calc_state, phlo, phhi, htotal,          &
                  marbl_status_log)

    !---------------------------------------------------------------------------
    ! Calculate htotal (free concentration of H ion)
    !---------------------------------------------------------------------------

    integer(kind=int_kind)        , intent(in)    :: num_elements
    integer(kind=int_kind)        , intent(in)    :: num_active_elements
    type(co2calc_state_type)      , intent(in)    :: co2calc_state_in(num_elements)
    type(co2calc_coeffs_type)     , intent(inout) :: co2calc_coeffs(num_elements)
    type(co2calc_state_type)      , intent(inout) :: co2calc_state(num_elements)
    real(kind=r8)                 , intent(inout) :: phlo(num_elements)   ! lower limit of pH range
    real(kind=r8)                 , intent(inout) :: phhi(num_elements)   ! upper limit of pH range
    real(kind=r8)                 , intent(out)   :: htotal(num_elements) ! free concentration of H ion
    type(marbl_log_type), optional, intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_co2calc_mod:comp_htotal'

    integer(kind=int_kind)   :: c
    real(kind=r8)            :: mass_to_vol                          ! (mol/kg) -> (mmol/m^3)
    real(kind=r8)            :: vol_to_mass                          ! (mmol/m^3) -> (mol/kg)
    real(kind=r8)            :: x1(num_elements), x2(num_elements)   ! bounds on htotal for solver
    !---------------------------------------------------------------------------

    associate(                               &
          k1  => co2calc_coeffs(:)%k1,       &
          k2  => co2calc_coeffs(:)%k2,       &
          kw  => co2calc_coeffs(:)%kw,       &
          kb  => co2calc_coeffs(:)%kb,       &
          ks  => co2calc_coeffs(:)%ks,       &
          kf  => co2calc_coeffs(:)%kf,       &
          k1p => co2calc_coeffs(:)%k1p,      &
          k2p => co2calc_coeffs(:)%k2p,      &
          k3p => co2calc_coeffs(:)%k3p,      &
          ksi => co2calc_coeffs(:)%ksi,      &
          bt  => co2calc_coeffs(:)%bt,       &
          st  => co2calc_coeffs(:)%st,       &
          ft  => co2calc_coeffs(:)%ft,       &
          dic => co2calc_state(:)%dic,       &
          ta  => co2calc_state(:)%ta,        &
          pt  => co2calc_state(:)%pt,        &
          sit => co2calc_state(:)%sit,       &
          dic_in => co2calc_state_in(:)%dic, &
          ta_in  => co2calc_state_in(:)%ta,  &
          pt_in  => co2calc_state_in(:)%pt,  &
          sit_in => co2calc_state_in(:)%sit  &
          )

    !---------------------------------------------------------------------------
    !   set unit conversion factors
    !---------------------------------------------------------------------------

    mass_to_vol = 1e6_r8 * rho_sw
    vol_to_mass = c1 / mass_to_vol

    !---------------------------------------------------------------------------
    !   convert tracer units to per mass
    !---------------------------------------------------------------------------

    do c = 1,num_active_elements
       dic(c)  = max(dic_in(c),dic_min) * vol_to_mass
       ta(c)   = max(ta_in(c),alk_min)  * vol_to_mass
       pt(c)   = max(pt_in(c),c0)       * vol_to_mass
       sit(c)  = max(sit_in(c),c0)      * vol_to_mass

       x1(c) = c10 ** (-phhi(c))
       x2(c) = c10 ** (-phlo(c))
    end do ! c loop

    !---------------------------------------------------------------------------
    !   If DIC and TA are known then either a root finding or iterative
    !   method must be used to calculate htotal. In this case we use
    !   the Newton-Raphson "safe" method taken from "Numerical Recipes"
    !   (function "rtsafe.f" with error trapping removed).
    !
    !   As currently set, this procedure iterates about 12 times. The
    !   x1 and x2 values set below will accomodate ANY oceanographic
    !   values. If an initial guess of the pH is known, then the
    !   number of iterations can be reduced to about 5 by narrowing
    !   the gap between x1 and x2. It is recommended that the first
    !   few time steps be run with x1 and x2 set as below. After that,
    !   set x1 and x2 to the previous value of the pH +/- ~0.5.
    !---------------------------------------------------------------------------

    call drtsafe(num_elements, num_active_elements, k1, k2, co2calc_state_in, &
                 co2calc_coeffs, co2calc_state, x1, x2, xacc, htotal,         &
                 marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace("drtsafe", subname)
       return
    end if

    end associate

  end subroutine comp_htotal

  !*****************************************************************************

  subroutine drtsafe(num_elements, num_active_elements, k1, k2, co2calc_state_in, &
                     co2calc_coeffs, co2calc_state, x1, x2, xacc, soln,           &
                     marbl_status_log)

    !---------------------------------------------------------------------------
    !   Vectorized version of drtsafe, which was a modified version of
    !      Numerical Recipes algorithm.
    !   Keith Lindsay, Oct 1999
    !
    !   Algorithm comment :
    !      Iteration from Newtons method is used unless it leaves
    !      bracketing interval or the dx is > 0.5 the previous dx.
    !      In that case, bisection method is used.
    !---------------------------------------------------------------------------

    integer(kind=int_kind)        , intent(in)    :: num_elements
    integer(kind=int_kind)        , intent(in)    :: num_active_elements
    real(kind=r8)                 , intent(in)    :: k1(num_elements)
    real(kind=r8)                 , intent(in)    :: k2(num_elements)
    type(co2calc_state_type)      , intent(in)    :: co2calc_state_in(num_elements)
    type(co2calc_coeffs_type)     , intent(in)    :: co2calc_coeffs(num_elements)
    type(co2calc_state_type)      , intent(in)    :: co2calc_state(num_elements)
    real(kind=r8)                 , intent(in)    :: xacc
    real(kind=r8)                 , intent(inout) :: x1(num_elements)
    real(kind=r8)                 , intent(inout) :: x2(num_elements)
    real(kind=r8)                 , intent(out)   :: soln(num_elements)
    type(marbl_log_type)          , intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_co2calc_mod:drtsafe'
    character(len=char_len)     :: log_message

    logical(kind=log_kind)                          :: leave_bracket, dx_decrease
    logical(kind=log_kind)                          :: abort
    logical(kind=log_kind), dimension(num_elements) :: mask
    logical(kind=log_kind), dimension(num_elements) :: active_mask
    integer(kind=int_kind)                          :: c, it
    real(kind=r8)                                   :: temp
    real(kind=r8), dimension(num_elements) :: xlo, xhi, flo, fhi, f, df, dxold, dx
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !   bracket root at each location and set up first iteration
    !---------------------------------------------------------------------------

    active_mask = .false.
    active_mask(1:num_active_elements) = .true.

    mask = active_mask
    abort = .false.

    it = 0

    do
       call total_alkalinity(num_elements, mask, k1, k2, x1, co2calc_coeffs, co2calc_state, flo, df)
       call total_alkalinity(num_elements, mask, k1, k2, x2, co2calc_coeffs, co2calc_state, fhi, df)

       where ( mask )
          mask = (flo > c0 .AND. fhi > c0) .OR. &
                 (flo < c0 .AND. fhi < c0)
       end where

       if (.not. ANY(mask)) EXIT

       it = it + 1

       do c = 1,num_active_elements
          if (mask(c)) then
             ! Log a warning message if bounding box end points have same sign
             ! (no guarantee that there is a root on the interval)

             ! Iteration number
             WRITE(log_message,"(3A,1X,A,I0)") '(', subname, ')', 'it = ', it
             call marbl_status_log%log_warning(log_message, subname, c)

             ! x1 & f
             WRITE(log_message,"(3A,1X,A,2E15.7e3)") '(', subname, ')',       &
                  'x1,f = ', x1(c), flo(c)
             call marbl_status_log%log_warning(log_message, subname, c)

             ! x2 & f
             WRITE(log_message,"(3A,1X,A,2E15.7e3)") '(', subname, ')',       &
                  'x2,f = ', x2(c), fhi(c)
             call marbl_status_log%log_warning(log_message, subname, c)

             ! Error if iteration count exceeds max_bracket_grow_it
             if (it > max_bracket_grow_it) then
                ! Error message
                log_message = "bounding bracket for pH solution not found"
                call marbl_status_log%log_error(log_message, subname, c)

                ! Original state values
                ! total DIC
                WRITE(log_message,"(3A,1X,A,E15.7e3)") '(', subname, ')',     &
                      'dic = ', co2calc_state_in(c)%dic
                call marbl_status_log%log_error(log_message, subname, c)

                ! total alkalinity
                WRITE(log_message,"(3A,1X,A,E15.7e3)") '(', subname, ')',     &
                      'ta = ', co2calc_state_in(c)%ta
                call marbl_status_log%log_error(log_message, subname, c)

                ! total phosphorus
                WRITE(log_message,"(3A,1X,A,E15.7e3)") '(', subname, ')',     &
                      'pt = ', co2calc_state_in(c)%pt
                call marbl_status_log%log_error(log_message, subname, c)

                ! total silicon
                WRITE(log_message,"(3A,1X,A,E15.7e3)") '(', subname, ')',     &
                      'sit = ', co2calc_state_in(c)%sit
                call marbl_status_log%log_error(log_message, subname, c)

                ! temperature
                WRITE(log_message,"(3A,1X,A,E15.7e3)") '(', subname, ')',     &
                      'temp = ', co2calc_state_in(c)%temp
                call marbl_status_log%log_error(log_message, subname, c)

                ! salinity
                WRITE(log_message,"(3A,1X,A,E15.7e3)") '(', subname, ')',     &
                      'salt = ', co2calc_state_in(c)%salt
                call marbl_status_log%log_error(log_message, subname, c)

                abort = .true.
             end if
          end if
       end do
       if (abort) return

       where ( mask )
          dx = sqrt(x2 / x1)
          x2 = x2 * dx
          x1 = x1 / dx
       end where
    end do

    mask = active_mask

    do c = 1,num_active_elements
       if (flo(c) .LT. c0) then
          xlo(c) = x1(c)
          xhi(c) = x2(c)
       else
          xlo(c) = x2(c)
          xhi(c) = x1(c)
          temp = flo(c)
          flo(c) = fhi(c)
          fhi(c) = temp
       end if
       soln(c) = p5 * (xlo(c) + xhi(c))
       dxold(c) = ABS(xlo(c) - xhi(c))
       dx(c) = dxold(c)
    end do

    call total_alkalinity(num_elements, mask, k1, k2, soln, co2calc_coeffs, co2calc_state, f, df)

    !---------------------------------------------------------------------------
    !   perform iterations, zeroing mask when a location has converged
    !---------------------------------------------------------------------------

    do it = 1,maxit
       do c = 1,num_active_elements
          if (mask(c)) then
             leave_bracket = ((soln(c) - xhi(c)) * df(c) - f(c)) * &
                  ((soln(c) - xlo(c)) * df(c) - f(c)) .GE. 0
             dx_decrease = ABS(c2 * f(c)) .LE. ABS(dxold(c) * df(c))
             if (leave_bracket .OR. .not. dx_decrease) then
                dxold(c) = dx(c)
                dx(c) = p5 * (xhi(c) - xlo(c))
                soln(c) = xlo(c) + dx(c)
                if (xlo(c) .EQ. soln(c)) mask(c) = .false.
             else
                dxold(c) = dx(c)
                dx(c) = -f(c) / df(c)
                temp = soln(c)
                soln(c) = soln(c) + dx(c)
                if (temp .EQ. soln(c)) mask(c) = .false.
             end if
             if (ABS(dx(c)) .LT. xacc) mask(c) = .false.
          end if
       end do

       if (.not. ANY(mask)) return

       call total_alkalinity(num_elements, mask, k1, k2, soln, co2calc_coeffs, co2calc_state, f, df)

       do c = 1,num_elements
          if (mask(c)) then
             if (f(c) .LT. c0) then
                xlo(c) = soln(c)
                flo(c) = f(c)
             else
                xhi(c) = soln(c)
                fhi(c) = f(c)
             end if
          end if
       end do

    end do ! iteration loop

    call marbl_status_log%log_error("lack of convergence in drtsafe", &
                                    "marbl_co2calc::drtsafe")
    return

  end subroutine drtsafe

  !*****************************************************************************

  subroutine total_alkalinity(num_elements, mask, k1, k2, x, co2calc_coeffs, &
             co2calc_state, fn, df)

    !---------------------------------------------------------------------------
    !   This routine computes TA as a function of DIC, htotal and constants.
    !   It also calculates the derivative of this function with respect to
    !   htotal. It is used in the iterative solution for htotal. In the call
    !   "x" is the input value for htotal, "fn" is the calculated value for
    !   TA and "df" is the value for dTA/dhtotal.
    !---------------------------------------------------------------------------

    integer(kind=int_kind)    , intent(in)  :: num_elements
    logical(kind=log_kind)    , intent(in)  :: mask(num_elements)
    real(kind=r8)             , intent(in)  :: k1(num_elements)
    real(kind=r8)             , intent(in)  :: k2(num_elements)
    real(kind=r8)             , intent(in)  :: x(num_elements)
    type(co2calc_coeffs_type) , intent(in)  :: co2calc_coeffs(num_elements)
    type(co2calc_state_type)  , intent(in)  :: co2calc_state(num_elements)
    real(kind=r8)             , intent(out) :: fn(num_elements)
    real(kind=r8)             , intent(out) :: df(num_elements)

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------
    integer(kind=int_kind) :: c
    real(kind=r8) :: x1, x1_r, x2, x2_r, x3, k12, k12p, k123p
    real(kind=r8) :: a, a_r, a2_r, da, b, b_r, b2_r, db, c_tmp, c_r
    real(kind=r8) :: kb_p_x1_r, ksi_p_x1_r, c1_p_c_ks_x1_r_r, c1_p_kf_x1_r_r
    !---------------------------------------------------------------------------

    associate(                         &
         kw  => co2calc_coeffs(:)%kw,  &
         kb  => co2calc_coeffs(:)%kb,  &
         ks  => co2calc_coeffs(:)%ks,  &
         kf  => co2calc_coeffs(:)%kf,  &
         k1p => co2calc_coeffs(:)%k1p, &
         k2p => co2calc_coeffs(:)%k2p, &
         k3p => co2calc_coeffs(:)%k3p, &
         ksi => co2calc_coeffs(:)%ksi, &
         bt  => co2calc_coeffs(:)%bt,  &
         st  => co2calc_coeffs(:)%st,  &
         ft  => co2calc_coeffs(:)%ft,  &
         dic => co2calc_state(:)%dic,  &
         ta  => co2calc_state(:)%ta,   &
         pt  => co2calc_state(:)%pt,   &
         sit => co2calc_state(:)%sit   &
         )

    do c = 1,num_elements
       if (mask(c)) then
          x1 = x(c)
          x1_r = c1 / x1
          x2 = x1 * x1
          x2_r = x1_r * x1_r
          x3 = x2 * x1
          k12 = k1(c) * k2(c)
          k12p = k1p(c) * k2p(c)
          k123p = k12p * k3p(c)
          a = x3 + k1p(c) * x2 + k12p * x1 + k123p
          a_r = c1 / a
          a2_r = a_r * a_r
          da = c3 * x2 + c2 * k1p(c) * x1 + k12p
          b = x2 + k1(c) * x1 + k12
          b_r = c1 / b
          b2_r = b_r * b_r
          db = c2 * x1 + k1(c)
          c_tmp = c1 + st(c) / ks(c)
          c_r = c1 / c_tmp
          kb_p_x1_r = c1 / (kb(c) + x1)
          ksi_p_x1_r = c1 / (ksi(c) + x1)
          c1_p_c_ks_x1_r_r = c1 / (c1 + c_tmp * ks(c) * x1_r)
          c1_p_kf_x1_r_r = c1 / (c1 + kf(c) * x1_r)

          !---------------------------------------------------------------------
          !   fn = hco3+co3+borate+oh+hpo4+2*po4+silicate-hfree-hso4-hf-h3po4-ta
          !---------------------------------------------------------------------

          fn(c) = k1(c) * dic(c) * x1 * b_r &
               + c2 * dic(c) * k12 * b_r &
               + bt(c) * kb(c) * kb_p_x1_r &
               + kw(c) * x1_r &
               + pt(c) * k12p * x1 * a_r &
               + c2 * pt(c) * k123p * a_r &
               + sit(c) * ksi(c) * ksi_p_x1_r &
               - x1 * c_r &
               - st(c) * c1_p_c_ks_x1_r_r &
               - ft(c) * c1_p_kf_x1_r_r &
               - pt(c) * x3 * a_r &
               - ta(c)

          !---------------------------------------------------------------------
          !   df = d(fn)/dx
          !---------------------------------------------------------------------

          df(c) = k1(c) * dic(c) * (b - x1 * db) * b2_r &
               - c2 * dic(c) * k12 * db * b2_r &
               - bt(c) * kb(c) * kb_p_x1_r * kb_p_x1_r &
               - kw(c) * x2_r &
               + (pt(c) * k12p * (a - x1 * da)) * a2_r &
               - c2 * pt(c) * k123p * da * a2_r &
               - sit(c) * ksi(c) * ksi_p_x1_r * ksi_p_x1_r &
               - c1 * c_r &
               - st(c) * c1_p_c_ks_x1_r_r * c1_p_c_ks_x1_r_r * (c_tmp * ks(c) * x2_r) &
               - ft(c) * c1_p_kf_x1_r_r * c1_p_kf_x1_r_r * kf(c) * x2_r &
               - pt(c) * x2 * (c3 * a - x1 * da) * a2_r

       end if ! if mask
    end do ! c loop

    end associate

  end subroutine total_alkalinity

  !*****************************************************************************

  subroutine marbl_co2calc_co3_sat_vals(&
       num_elements, num_active_elements, temp, salt, press_bar, &
       co3_sat_calc, co3_sat_arag)

    !---------------------------------------------------------------------------
    ! Calculate co3 concentration at calcite and aragonite saturation
    ! from temp, salinity (s), press
    !---------------------------------------------------------------------------

    integer(kind=int_kind)                           , intent(in)  :: num_elements
    integer(kind=int_kind)                           , intent(in)  :: num_active_elements
    real(kind=r8)          , dimension(num_elements) , intent(in)  :: temp         ! temperature (degC)
    real(kind=r8)          , dimension(num_elements) , intent(in)  :: salt         ! salinity (psu)
    real(kind=r8)          , dimension(num_elements) , intent(in)  :: press_bar    ! pressure at level k (bars)
    real(kind=r8)          , dimension(num_elements) , intent(out) :: co3_sat_calc ! co3 concentration at calcite saturation
    real(kind=r8)          , dimension(num_elements) , intent(out) :: co3_sat_arag ! co3 concentration at aragonite saturation

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------
    real(kind=r8) :: &
         mass_to_vol     ! (mol/kg) -> (mmol/m^3)

    real(kind=r8), dimension(num_elements) :: &
         salt_lim,                            & ! bounded salt
         tk,                                  & ! temperature (K)
         log10tk,invtk,sqrts,s15,invRtk,arg,  &
         K_calc,                              & ! thermodynamic constant for calcite
         K_arag,                              & ! thermodynamic constant for aragonite
         inv_Ca                                 ! inverse of Calcium concentration (mol/kg)

    integer(kind=int_kind) :: n
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !   set unit conversion factors
    !---------------------------------------------------------------------------

    mass_to_vol = 1e6_r8 * rho_sw

    salt_lim = max(salt(:),salt_min)
    tk       = T0_Kelvin + temp(:)
    log10tk  = log10(tk)
    invtk    = c1 / tk
    invRtk   = (c1 / 83.1451_r8) * invtk

    sqrts    = sqrt(salt_lim)
    s15      = sqrts * salt_lim

    !------------------------------------------------------------------------
    !   Compute K_calc, K_arag, apply pressure factor
    !   Mucci, Amer. J. of Science 283:781-799, 1983 & Millero 1979
    !------------------------------------------------------------------------

    arg = -171.9065_r8 - 0.077993_r8 * tk + 2839.319_r8 * invtk + 71.595_r8 * log10tk + &
         (-0.77712_r8 + 0.0028426_r8 * tk + 178.34_r8 * invtk) * sqrts - &
         0.07711_r8 * salt_lim + 0.0041249_r8 * s15
    arg = log(c10) * arg
    K_calc = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-48.76_r8, 0.5304_r8, c0/), &
         Kappa_coefs = (/-11.76_r8, 0.3692_r8, c0/), &
         therm_coef = K_calc, &
         press_bar = press_bar)

    arg = -171.945_r8 - 0.077993_r8 * tk + 2903.293_r8 * invtk + 71.595_r8 * log10tk + &
         (-0.068393_r8 + 0.0017276_r8 * tk + 88.135_r8 * invtk) * sqrts - &
         0.10018_r8 * salt_lim + 0.0059415_r8 * s15
    arg = log(c10) * arg
    K_arag = exp(arg)

    call apply_pressure_correction(num_elements, temp, invRtk, &
         deltaV_coefs = (/-45.96_r8, 0.5304_r8, c0/), &
         Kappa_coefs = (/-11.76_r8, 0.3692_r8, c0/), &
         therm_coef = K_arag, &
         press_bar = press_bar)

    do n=1,num_active_elements

       !------------------------------------------------------------------
       !   Compute CO3 concentration at calcite & aragonite saturation
       !------------------------------------------------------------------

       inv_Ca(n) = (35.0_r8 / 0.01028_r8) / salt_lim(n)
       co3_sat_calc(n) = K_calc(n) * inv_Ca(n)
       co3_sat_arag(n) = K_arag(n) * inv_Ca(n)

       !------------------------------------------------------------------
       !   Convert units of output arguments
       !------------------------------------------------------------------

       co3_sat_calc(n) = co3_sat_calc(n) * mass_to_vol
       co3_sat_arag(n) = co3_sat_arag(n) * mass_to_vol

    end do

    co3_sat_calc(num_active_elements+1:num_elements) = c0
    co3_sat_arag(num_active_elements+1:num_elements) = c0

  end subroutine marbl_co2calc_co3_sat_vals

  !*****************************************************************************

  subroutine apply_pressure_correction(num_elements, temp, invRtk, &
       deltaV_coefs, Kappa_coefs, therm_coef, press_bar)

    integer,       intent(in)    :: num_elements
    real(kind=r8), intent(in)    :: temp(num_elements)
    real(kind=r8), intent(in)    :: invRtk(num_elements)
    real(kind=r8), intent(in)    :: deltaV_coefs(0:2)
    real(kind=r8), intent(in)    :: Kappa_coefs(0:2)
    real(kind=r8), intent(inout) :: therm_coef(num_elements)
    real(kind=r8), optional, intent(in) :: press_bar(num_elements)

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------
    real(kind=r8), dimension(num_elements) :: deltaV, Kappa, lnKfac, Kfac
    !---------------------------------------------------------------------------

    if (.not. present(press_bar)) return

    deltaV = deltaV_coefs(0) + (deltaV_coefs(1) + deltaV_coefs(2) * temp) * temp
    Kappa  = (Kappa_coefs(0) + (Kappa_coefs(1) + Kappa_coefs(2) * temp) * temp) * p001
    lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
    Kfac = exp(lnKfac)
    therm_coef = therm_coef * Kfac

  end subroutine apply_pressure_correction

  !*****************************************************************************

end module marbl_co2calc_mod
