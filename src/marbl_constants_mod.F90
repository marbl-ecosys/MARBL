module marbl_constants_mod

  use marbl_kinds_mod, only : r8

  implicit none
  public
  save

  ! General constants
  real(kind=r8), parameter, public ::        &
      c0     =    0.0_r8                   , &
      c1     =    1.0_r8                   , &
      c2     =    2.0_r8                   , &
      c3     =    3.0_r8                   , &
      c10    =   10.0_r8                   , &
      c1000  = 1000.0_r8                   , &
      p001   =    0.001_r8                 , &
      p5     =    0.5_r8                   , &
      pi     =    3.14159265358979323846_r8

! Unit Conversion
  real(kind=r8), parameter, public :: &
      spd       = 86400.0_r8,          & ! number of seconds in a day
      spy       = 365.0_r8*spd,        & ! number of seconds in a year
      dps       = c1 / spd,            & ! number of days in a second
      yps       = c1 / (365.0_r8*spd), & ! number of years in a second
      mpercm    = .01_r8                 ! meters per cm

! Physical Constants
  real(kind=r8), parameter, public :: &
      T0_Kelvin = 273.15_r8,           & ! freezing T of fresh water (deg K)
      rho_sw    =   1.026_r8,          & ! density of salt water (g/cm^3)
      epsC      =   1.00e-8,           & ! small C concentration (mmol C/m^3)
      epsTinv   =   3.17e-8,           & ! small inverse time scale (1/year) (1/sec)
      molw_Fe   =  55.845_r8,          & ! molecular weight of iron
      R13C_std  =   1.0_r8,            & ! actual 13C/12C PDB standard ratio (Craig, 1957) = 1123.72e-5_r8 
      R14C_std =    1.0_r8               ! actual 14C/12C NOSAMS standard ratio = 11.76e-13_r8

end module marbl_constants_mod
