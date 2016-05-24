module marbl_constants_mod

  use marbl_kinds_mod, only : r8

  implicit none
  public
  save

  real(kind=r8), parameter, public :: &
      c0     =    0.0_r8       , &
      c1     =    1.0_r8       , &
      c2     =    2.0_r8       , &
      c3     =    3.0_r8       , &
      c10    =   10.0_r8       , &
      c1000  = 1000.0_r8       , &
      p001   =  0.001_r8       , &
      p5     =    0.5_r8
  
  real(kind=r8), parameter, public :: &
      T0_Kelvin = 273.15_R8,           & ! freezing T of fresh water (deg K)
      rho_sw    =   1.026_R8,          & ! density of salt water (g/cm^3)
      spd       = 86400.0_r8,          & ! number of seconds in a day
      dps       = c1 / spd,            & ! number of days in a second
      yps       = c1 / (365.0_r8*spd), & ! number of years in a second
      mpercm    = .01_r8                 ! meters per cm

end module marbl_constants_mod
