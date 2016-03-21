module marbl_constants_mod

  use marbl_kinds_mod, only : r8

  implicit none
  public
  save

  real(kind=r8), parameter, public :: &
      c0     =    0.0_r8       , &
      c1     =    1.0_r8       , &
      c2     =    2.0_r8       , &
      c1000  = 1000.0_r8       , &
      p5     =    0.5_r8
  
  real(kind=r8), parameter, public :: &
      T0_Kelvin = 273.15_R8    , & ! freezing T of fresh water          ~ K
      rho_sw    =   1.026_R8       ! density of salt water (g/cm^3)

end module marbl_constants_mod
