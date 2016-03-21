module marbl_schmidt_number_mod

  implicit none

  private

  public :: schmidt_co2_surf

  !*****************************************************************************

contains
  
  !*****************************************************************************

  function schmidt_co2_surf(n, sst, surface_mask)

    !  Compute Schmidt number of CO2 in seawater as function of SST
    !  where surface_mask is non-zero. Give zero where surface_mask is zero.
    !
    !  ref : Wanninkhof, J. Geophys. Res, Vol. 97, No. C5,
    !  pp. 7373-7382, May 15, 1992
    
    use marbl_kinds_mod, only : r8, log_kind, int_kind
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

end module marbl_schmidt_number_mod
