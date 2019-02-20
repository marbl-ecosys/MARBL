module marbl_schmidt_number_mod

  implicit none

  private

  public :: schmidt_co2_surf
  public :: schmidt_nh3_surf
  public :: schmidt_nh3_air

  !*****************************************************************************

contains

  !*****************************************************************************

  function schmidt_co2_surf(n, sst_in)

    !  Compute Schmidt number of CO2 in seawater as function of SST
    !
    !  range of validity of fit is -2:40
    !
    !  Ref : Wanninkhof 2014, Relationship between wind speed
    !        and gas exchange over the ocean revisited,
    !        Limnol. Oceanogr.: Methods, 12,
    !        doi:10.4319/lom.2014.12.351

    use marbl_kinds_mod     , only : r8, int_kind

    integer(int_kind)  , intent(in) :: n
    real (r8)          , intent(in) :: sst_in(n)

    real (r8) :: schmidt_co2_surf(n)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind)    :: i
    real (r8)            :: sst(n)

    real (r8), parameter :: a = 2116.8_r8
    real (r8), parameter :: b = -136.25_r8
    real (r8), parameter :: c =    4.7353_r8
    real (r8), parameter :: d =   -0.092307_r8
    real (r8), parameter :: e =    0.0007555_r8

    !-----------------------------------------------------------------------

    do i = 1, n
       sst(i) = max(-2.0_r8, min(40.0_r8, sst_in(i)))
       schmidt_co2_surf(i) = a + sst(i) * (b + sst(i) * (c + sst(i) * (d + sst(i) * e)))
    end do

  end function schmidt_co2_surf

  !*****************************************************************************

  function schmidt_nh3_surf(n, sst_in)

    !  Compute Schmidt number of NH3 in seawater as function of SST
    !
    !  ref : M. T. Johnson, Ocean Science, Vol. 6, pp. 913-932, 2010
    !
    !  Values for NH3 were computed for sst=[-2:1:40], sss=35 using R software
    !  from ref, and the results were fit with a polynomial
    !  following Wanninkhof 2014, a 4th order polynomial is used

    use marbl_kinds_mod     , only : r8, int_kind

    integer(int_kind)  , intent(in) :: n
    real (r8)          , intent(in) :: sst_in(n)

    real (r8) :: schmidt_nh3_surf(n)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind)    :: i
    real (r8)            :: sst(n)

    real (r8), parameter :: a =  1.809018e+03_r8
    real (r8), parameter :: b = -1.199578e+02_r8
    real (r8), parameter :: c =  4.369922e+00_r8
    real (r8), parameter :: d = -8.824558e-02_r8
    real (r8), parameter :: e =  7.377934e-04_r8

    !-----------------------------------------------------------------------

    do i = 1, n
       sst(i) = max(-2.0_r8, min(40.0_r8, sst_in(i)))
       schmidt_nh3_surf(i) = a + sst(i) * (b + sst(i) * (c + sst(i) * (d + sst(i) * e)))
    end do

  end function schmidt_nh3_surf

  !*****************************************************************************

  function schmidt_nh3_air(n, sst_in, atmpres)

    !  Compute Schmidt number of NH3 in air as function of atmospheric pressure
    !
    !  ref : M. T. Johnson, Ocean Science, Vol. 6, pp. 913-932, 2010
    !
    !  Values for NH3 were computed for sst=[-2:1:40], atmpres=1.0 using R software
    !  from ref, and the results were fit with a polynomial
    !  following Wanninkhof 2014, a 4th order polynomial is used
    !
    !  schmidt_nh3_air scales quadratically with atmpres

    use marbl_kinds_mod     , only : r8, int_kind

    integer(int_kind)  , intent(in) :: n
    real (r8)          , intent(in) :: atmpres(n)
    real (r8)          , intent(in) :: sst_in(n)

    real (r8) :: schmidt_nh3_air(n)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind)    :: i
    real (r8)            :: sst(n)

    real (r8), parameter :: a =  7.537648e-01_r8
    real (r8), parameter :: b =  4.728550e-04_r8
    real (r8), parameter :: c = -2.546799e-05_r8
    real (r8), parameter :: d =  3.187285e-07_r8
    real (r8), parameter :: e = -1.985024e-09_r8

    !-----------------------------------------------------------------------

    do i = 1, n
       sst(i) = max(-2.0_r8, min(40.0_r8, sst_in(i)))
       schmidt_nh3_air(i) = a + sst(i) * (b + sst(i) * (c + sst(i) * (d + sst(i) * e)))
       schmidt_nh3_air(i) = atmpres(i) * atmpres(i) * schmidt_nh3_air(i)
    end do

  end function schmidt_nh3_air

end module marbl_schmidt_number_mod
