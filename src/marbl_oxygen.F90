! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-

module marbl_oxygen

  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8

  use marbl_constants_mod, only : c0

  implicit none

  private

  public :: &
       schmidt_o2_surf, &
       o2sat_surf,      &
       o2sat_scalar

contains

  !*****************************************************************************

  function schmidt_o2_surf(n, sst, surface_mask)

    ! !DESCRIPTION:
    !  Compute Schmidt number of O2 in seawater as function of SST
    !  where surface_mask is non-zero. Give zero where surface_mask is 0.
    !
    !  ref : Keeling et al, Global Biogeochem. Cycles, Vol. 12,
    !        No. 1, pp. 141-163, March 1998

    integer(int_kind)  , intent(in) :: n
    real (r8)          , intent(in) :: sst(n)
    real (r8)          , intent(in) :: surface_mask(n)

    real (r8) :: schmidt_o2_surf(n)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real (r8), parameter :: a = 1638.0_r8
    real (r8), parameter :: b = 81.83_r8
    real (r8), parameter :: c = 1.483_r8
    real (r8), parameter :: d = 0.008004_r8
    !-----------------------------------------------------------------------

    where (surface_mask(:) /= c0) 
       schmidt_o2_surf = a + sst * (-b + sst * (c + sst * (-d)))
    elsewhere
       schmidt_o2_surf = c0
    end where

  end function schmidt_o2_surf

  !*****************************************************************************

  function o2sat_surf(n, sst, sss, surface_mask)

    !  Computes oxygen saturation concentration at 1 atm total pressure
    !  in mmol/m^3 given the temperature (t, in deg C) and the salinity (s,
    !  in permil) where surface_mask is non-zero. Give zero where surface_mask is 0.
    !
    !  FROM GARCIA AND GORDON (1992), LIMNOLOGY and OCEANOGRAPHY.
    !  THE FORMULA USED IS FROM PAGE 1310, EQUATION (8).
    !
    !  *** NOTE: THE "A_3*TS^2" TERM (IN THE PAPER) IS INCORRECT. ***
    !  *** IT SHOULD NOT BE THERE.                                ***
    !
    !  O2SAT IS DEFINED BETWEEN T(freezing) <= T <= 40(deg C) AND
    !  0 permil <= S <= 42 permil
    !  CHECK VALUE:  T = 10.0 deg C, S = 35.0 permil,
    !  O2SAT = 282.015 mmol/m^3

    use marbl_constants_mod, only : T0_Kelvin

    integer(int_kind),            intent(in) :: n
    real (r8),                    intent(in) :: SST(n) ! sea surface temperature (C)
    real (r8),                    intent(in) :: SSS(n) ! sea surface salinity (psu)
    real (r8)         , optional, intent(in) :: surface_mask(n)

    real (r8) :: O2SAT_surf(n)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real (r8) :: ts(n)
    real (r8) :: o2sat(n)
    !  coefficients in expansion
    real (r8), parameter :: a_0 =  2.00907_r8
    real (r8), parameter :: a_1 =  3.22014_r8
    real (r8), parameter :: a_2 =  4.05010_r8
    real (r8), parameter :: a_3 =  4.94457_r8
    real (r8), parameter :: a_4 = -2.56847E-1_r8
    real (r8), parameter :: a_5 =  3.88767_r8    
    real (r8), parameter :: b_0 = -6.24523E-3_r8
    real (r8), parameter :: b_1 = -7.37614E-3_r8
    real (r8), parameter :: b_2 = -1.03410E-2_r8
    real (r8), parameter :: b_3 = -8.17083E-3_r8
    real (r8), parameter :: c_0 = -4.88682E-7_r8
    !-----------------------------------------------------------------------

    ! set default
    o2sat_surf(:) = c0

    if (present(surface_mask)) then

       where (surface_mask(:) /= c0) 

          ts(:)    = log( ((t0_kelvin + 25.0_r8) - sst(:)) / (t0_kelvin + sst(:)) )
          o2sat(:) = exp(a_0+ts(:)*(a_1+ts(:)*(a_2+ts(:)*(a_3+ts(:)*(a_4+ts(:)*a_5)))) + &
                         sss(:)*( (b_0+ts(:)*(b_1+ts(:)*(b_2+ts(:)*b_3))) + sss(:)*c_0 ))

          !  convert from ml/l to mmol/m^3
          o2sat_surf(:) = o2sat(:) / 0.0223916_r8

       end where
    endif

  end function O2SAT_surf

  !*****************************************************************************

  function o2sat_scalar(sst, sss)

    ! !DESCRIPTION:
    !
    !  Computes oxygen saturation concentration at 1 atm total pressure
    !  in mmol/m^3 given the temperature (t, in deg C) and the salinity (s,
    !  in permil) where surface_mask is non-zero. Give zero where surface_mask is 0.
    !
    !  FROM GARCIA AND GORDON (1992), LIMNOLOGY and OCEANOGRAPHY.
    !  THE FORMULA USED IS FROM PAGE 1310, EQUATION (8).
    !
    !  *** NOTE: THE "A_3*TS^2" TERM (IN THE PAPER) IS INCORRECT. ***
    !  *** IT SHOULD NOT BE THERE.                                ***
    !
    !  O2SAT IS DEFINED BETWEEN T(freezing) <= T <= 40(deg C) AND
    !  0 permil <= S <= 42 permil
    !  CHECK VALUE:  T = 10.0 deg C, S = 35.0 permil,
    !  O2SAT = 282.015 mmol/m^3

    use marbl_constants_mod, only : T0_Kelvin

    real (r8), intent(in) :: sst    ! sea surface temperature (C)
    real (r8), intent(in) :: sss    ! sea surface salinity (psu)

    real (r8) :: o2sat_scalar

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real (r8) :: ts
    real (r8) :: o2sat
    !  coefficients in expansion
    real (r8), parameter :: a_0 =  2.00907_r8
    real (r8), parameter :: a_1 =  3.22014_r8
    real (r8), parameter :: a_2 =  4.05010_r8
    real (r8), parameter :: a_3 =  4.94457_r8
    real (r8), parameter :: a_4 = -2.56847E-1_r8
    real (r8), parameter :: a_5 =  3.88767_r8    
    real (r8), parameter :: b_0 = -6.24523E-3_r8
    real (r8), parameter :: b_1 = -7.37614E-3_r8
    real (r8), parameter :: b_2 = -1.03410E-2_r8
    real (r8), parameter :: b_3 = -8.17083E-3_r8
    real (r8), parameter :: c_0 = -4.88682E-7_r8
    !-----------------------------------------------------------------------

    ts = log( ((t0_kelvin + 25.0_r8) - sst) / (t0_kelvin + sst) )

    o2sat = exp(a_0+ts*(a_1+ts*(a_2+ts*(a_3+ts*(a_4+ts*a_5)))) + &
         sss*( (b_0+ts*(b_1+ts*(b_2+ts*b_3))) + sss*c_0 ))


    !  Convert from ml/l to mmol/m^3
    o2sat_scalar = o2sat / 0.0223916_r8

  end function o2sat_scalar

end module marbl_oxygen
