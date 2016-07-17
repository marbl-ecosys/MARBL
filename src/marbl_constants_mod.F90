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
      cmperm    = 100.0_r8     , & ! cm per meter
      mpercm    = 0.01_r8      , & ! meters per cm
      sperhr    = 3600.0_r8    , & ! seconds per hour
      hrpers    = c1/sperhr        ! hours per second
  
  real(kind=r8), parameter, public :: &
      vonkar    = 0.4_r8       , & ! von Karman constant
      T0_Kelvin = 273.15_r8    , & ! freezing T of fresh water (K)
      rho_sw    =   1.026_r8       ! density of salt water (g/cm^3)

end module marbl_constants_mod
