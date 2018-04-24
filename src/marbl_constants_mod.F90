module marbl_constants_mod

  !---------------------------------------------------------------------------
  !  This module contains non-BGC related parameters. There are three general
  !  categories:
  !    * Mathematical constants (c0, c1, etc)
  !    * Unit conversions (spy, spd, mpercm, etc)
  !    * Physical parameters (T0_Kelvin, rho_sw, etc)
  !
  !  BGC-specific parameters are in marbl_parms.
  !---------------------------------------------------------------------------

  use marbl_kinds_mod, only : r8

  implicit none
  public
  save

  !---------------------------------------------------------------------
  !  Mathematical constants
  !---------------------------------------------------------------------

  real(kind=r8), parameter, public ::        &
      c0     =    0.0_r8                   , &
      c1     =    1.0_r8                   , &
      c2     =    2.0_r8                   , &
      c3     =    3.0_r8                   , &
      c4     =    4.0_r8                   , &
      c10    =   10.0_r8                   , &
      c1000  = 1000.0_r8                   , &
      p001   =    0.001_r8                 , &
      p5     =    0.5_r8                   , &
      pi     =    3.14159265358979323846_r8

  !---------------------------------------------------------------------
  !  Unit Conversion
  !---------------------------------------------------------------------

  real(kind=r8), parameter, public :: &
      sphr      = 3600.0_r8,    & ! number of seconds in an hour
      spd       = 86400.0_r8,   & ! number of seconds in a day
      dpy       = 365.0_r8,     & ! number of days in a year
      spy       = dpy*spd,      & ! number of seconds in a year
      hrps      = c1 / sphr,    & ! number of hours in a second
      dps       = c1 / spd,     & ! number of days in a second
      ypd       = c1 / dpy,     & ! number of years in a day
      yps       = c1 / spy,     & ! number of years in a second
      cmperm    = 100.0_r8,     & ! cm per meter
      mpercm    = .01_r8          ! meters per cm

  !---------------------------------------------------------------------
  !  Physical Constants
  !---------------------------------------------------------------------

  real(kind=r8), parameter, public :: &
      vonkar    =   0.4_r8,            & ! von Karman constant
      T0_Kelvin = 273.15_r8,           & ! freezing T of fresh water (K)
      Tref      =  30.0_r8,            & ! reference temperature (degC)
      rho_sw    =   1.026_r8,          & ! density of salt water (g/cm^3)
      epsC      =   1.00e-8,           & ! small C concentration (mmol C/m^3)
      epsTinv   =   3.17e-8,           & ! small inverse time scale (1/year) (1/sec)
      molw_Fe   =  55.845_r8,          & ! molecular weight of iron (gFe / mol Fe)
      R13C_std  =   1.0_r8,            & ! actual 13C/12C PDB standard ratio (Craig, 1957) = 1123.72e-5_r8
      R14C_std =    1.0_r8               ! actual 14C/12C NOSAMS standard ratio = 11.76e-13_r8

end module marbl_constants_mod
