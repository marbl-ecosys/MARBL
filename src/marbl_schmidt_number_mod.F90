module marbl_schmidt_number_mod

  implicit none

  private

  public :: schmidt_co2_surf
  public :: schmidt_nh3_surf
  public :: schmidt_nh3_air

  !*****************************************************************************

contains

  !*****************************************************************************

  function schmidt_co2_surf(n, sst, surface_mask)

    !  Compute Schmidt number of CO2 in seawater as function of SST
    !  where surface_mask is non-zero. Give zero where surface_mask is zero.
    !
    !  ref : Wanninkhof, J. Geophys. Res, Vol. 97, No. C5,
    !  pp. 7373-7382, May 15, 1992

    use marbl_kinds_mod, only : r8, int_kind
    use marbl_parms    , only : c0

    implicit none

    integer(int_kind)  , intent(in) :: n
    real (r8)          , intent(in) :: sst(n)
    real (r8)          , intent(in) :: surface_mask(n)

    real (r8) :: schmidt_co2_surf(n)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real (r8), parameter :: a = 2073.1_r8
    real (r8), parameter :: b = 125.62_r8
    real (r8), parameter :: c = 3.6276_r8
    real (r8), parameter :: d = 0.043219_r8
    !-----------------------------------------------------------------------

    where (surface_mask(:) /= c0)
       schmidt_co2_surf = a + sst * (-b + sst * (c + sst * (-d)))
    elsewhere
       schmidt_co2_surf = c0
    end where

  end function schmidt_co2_surf

  !*****************************************************************************

  function schmidt_nh3_surf(n, sst, surface_mask)

    !  Compute Schmidt number of NH3 in seawater as function of SST
    !  where surface_mask is non-zero. Give zero where surface_mask is zero.
    !
    !  ref : M. T. Johnson, Ocean Science, Vol. 6, pp. 913-932, 2010
    !
    !  Values for NH3 were computed for sst=[-2:2:36], sss=35 using R software
    !  from ref, and the results were fit with a cubic polynomial

    use marbl_kinds_mod, only : r8, int_kind
    use marbl_parms    , only : c0

    implicit none

    integer(int_kind)  , intent(in) :: n
    real (r8)          , intent(in) :: sst(n)
    real (r8)          , intent(in) :: surface_mask(n)

    real (r8) :: schmidt_nh3_surf(n)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    real (r8), parameter :: a =  1.8107e+03_r8
    real (r8), parameter :: b = -1.1392e+02_r8
    real (r8), parameter :: c =  3.3260e+00_r8
    real (r8), parameter :: d = -3.7661e-02_r8

    !-----------------------------------------------------------------------

    where (surface_mask(:) /= c0)
       schmidt_nh3_surf = a + sst * (b + sst * (c + sst * d))
    elsewhere
       schmidt_nh3_surf = c0
    end where

  end function schmidt_nh3_surf

  !*****************************************************************************

  function schmidt_nh3_air(n, sst, atmpres, surface_mask)

    !  Compute Schmidt number of NH3 in air as function of atmospheric pressure
    !  where surface_mask is non-zero. Give zero where surface_mask is zero.
    !
    !  ref : M. T. Johnson, Ocean Science, Vol. 6, pp. 913-932, 2010
    !
    !  Values for NH3 were computed for sst=[-2:2:36], atmpres=1.0 using R software
    !  from ref, and the results were fit with a cubic polynomial
    !
    !  schmidt_nh3_air scales quadratically with atmpres

    use marbl_kinds_mod, only : r8, int_kind
    use marbl_parms    , only : c0

    implicit none

    integer(int_kind)  , intent(in) :: n
    real (r8)          , intent(in) :: atmpres(n)
    real (r8)          , intent(in) :: sst(n)
    real (r8)          , intent(in) :: surface_mask(n)

    real (r8) :: schmidt_nh3_air(n)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    real (r8), parameter :: a =  7.5376e-01_r8
    real (r8), parameter :: b =  4.5671e-04_r8
    real (r8), parameter :: c = -2.2696e-05_r8
    real (r8), parameter :: d =  1.8366e-07_r8

    !-----------------------------------------------------------------------

    where (surface_mask(:) /= c0)
       schmidt_nh3_air = a + sst * (b + sst * (c + sst * d))
       schmidt_nh3_air = atmpres * atmpres * schmidt_nh3_air
    elsewhere
       schmidt_nh3_air = c0
    end where

  end function schmidt_nh3_air

end module marbl_schmidt_number_mod
