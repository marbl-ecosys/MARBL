! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_nhx_surface_emis_mod

  ! Compute sea-to-air NHx emissions

  ! based on: Paulot, F., et al. (2015), Global oceanic emission of ammonia:
  ! Constraints from seawater and atmospheric observations, Global Biogeochem.
  ! Cycles, 29, 1165-1178, doi:10.1002/2015GB005106

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8

  implicit none
  private

  !-----------------------------------------------------------------------
  ! public member procedure declarations
  !-----------------------------------------------------------------------

  public :: marbl_nhx_surface_emis_compute

  !***********************************************************************

contains

  !***********************************************************************

  subroutine marbl_nhx_surface_emis_compute( &
       num_elements,                         &
       nh4,                                  &
       ph,                                   &
       sst,                                  &
       sss,                                  &
       u10_sqr,                              &
       atmpres,                              &
       ifrac,                                &
       nhx_surface_emis)

    use marbl_constants_mod, only : c0
    use marbl_constants_mod, only : c1
    use marbl_constants_mod, only : mpercm

    integer(int_kind)  , intent(in)  :: num_elements
    real (r8)          , intent(in)  :: nh4(num_elements)
    real (r8)          , intent(in)  :: ph(num_elements)
    real (r8)          , intent(in)  :: sst(num_elements)
    real (r8)          , intent(in)  :: sss(num_elements)
    real (r8)          , intent(in)  :: u10_sqr(num_elements) ! (cm/s)^2
    real (r8)          , intent(in)  :: atmpres(num_elements) ! (atm)
    real (r8)          , intent(in)  :: ifrac(num_elements)
    real (r8)          , intent(out) :: nhx_surface_emis(num_elements)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    real (r8) :: u10_rms_mps(num_elements) ! (m/s)
    real (r8) :: kw_nh3(num_elements)
    real (r8) :: kg_nh3(num_elements)
    real (r8) :: Hstar_nhx(num_elements)
    real (r8) :: K(num_elements)

    u10_rms_mps(:) = mpercm * sqrt(u10_sqr(:))

    call marbl_comp_kw_nh3(num_elements, sst, u10_rms_mps, kw_nh3)

    call marbl_comp_kg_nh3(num_elements, sst, u10_rms_mps, atmpres, kg_nh3)

    call marbl_comp_Hstar_nhx(num_elements, ph, sst, sss, Hstar_nhx)

    K(:) = c1 / (c1 / kg_nh3(:) + Hstar_nhx / kw_nh3(:))
    nhx_surface_emis(:) = (c1 - ifrac(:)) * K(:) * Hstar_nhx(:) * max(nh4(:),c0)

  end subroutine marbl_nhx_surface_emis_compute

  !***********************************************************************

  subroutine marbl_comp_kw_nh3( &
       num_elements,            &
       sst,                     &
       u10_rms_mps,             &
       kw_nh3)

    use marbl_schmidt_number_mod, only : schmidt_nh3_surf
    use marbl_constants_mod     , only : hrps

    integer(int_kind)  , intent(in)  :: num_elements
    real (r8)          , intent(in)  :: sst(num_elements)
    real (r8)          , intent(in)  :: u10_rms_mps(num_elements)
    real (r8)          , intent(out) :: kw_nh3(num_elements)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    real (r8) :: schmidt_nh3(num_elements) ! Schmidt number for NH3 in seawater

    !-----------------------------------------------------------------------

    schmidt_nh3(:) = schmidt_nh3_surf(num_elements, sst)

    !-----------------------------------------------------------------------
    ! Nightingale et al. 2000, GRL
    ! convert from Nightingale result units of cm/h to cm/s
    !-----------------------------------------------------------------------

    kw_nh3(:) = hrps * u10_rms_mps(:) * (0.061_r8 + 0.24_r8 * u10_rms_mps(:)) * sqrt(600.0_r8 / schmidt_nh3(:))

  end subroutine marbl_comp_kw_nh3

  !***********************************************************************

  subroutine marbl_comp_kg_nh3( &
       num_elements,            &
       sst,                     &
       u10_rms_mps,             &
       atmpres,                 &
       kg_nh3)

    use marbl_schmidt_number_mod, only : schmidt_nh3_air
    use marbl_constants_mod     , only : c1
    use marbl_constants_mod     , only : c2
    use marbl_constants_mod     , only : vonkar
    use marbl_constants_mod     , only : cmperm

    integer(int_kind)  , intent(in)  :: num_elements
    real (r8)          , intent(in)  :: sst(num_elements)
    real (r8)          , intent(in)  :: u10_rms_mps(num_elements)
    real (r8)          , intent(in)  :: atmpres(num_elements)
    real (r8)          , intent(out) :: kg_nh3(num_elements) ! (cm/s)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    real (r8) :: schmidt_nh3(num_elements)   ! Schmidt number for NH3 in air
    real (r8) :: ustar_div_u10(num_elements) ! ustar divided by u10
    real (r8) :: ustar(num_elements)         ! friction velocity (m/s)

    !-----------------------------------------------------------------------

    schmidt_nh3(:) = schmidt_nh3_air(num_elements, sst, atmpres)

    !-----------------------------------------------------------------------
    ! M.T. Johnson, Ocean Science, Vol. 6, pp. 913-932, 2010
    ! (15) k_a = 1e-3 + ustar / (13.3 * sqrt(schmidt_nh3) + C_D ^ (-1/2) - 5 + log(schmidt_nh3)/(2*vonkar)
    ! where
    ! (10) C_D = (ustar/u10)^2
    ! (11) C_D = 6.1e-4 + 6.3e-5 * u10
    ! (10) & (11) => ustar/u10 = sqrt(C_D) = sqrt(6.1e-4 + 6.3e-5 * u10)
    ! (10) => C_D ^ (-1/2) = u10/ustar
    !-----------------------------------------------------------------------

    ustar_div_u10(:) = sqrt(6.1e-4_r8 + 6.3e-5_r8 * u10_rms_mps(:))
    ustar(:) = ustar_div_u10(:) * u10_rms_mps(:)
    kg_nh3(:) = 1.0e-3_r8 + ustar(:) &
         / (13.3_r8 * sqrt(schmidt_nh3(:)) + c1 / ustar_div_u10(:) - 5.0_r8 + log(schmidt_nh3(:)) / (c2 * vonkar))
    kg_nh3(:) = cmperm * kg_nh3(:)

  end subroutine marbl_comp_kg_nh3

  !***********************************************************************

  subroutine marbl_comp_Hstar_nhx( &
       num_elements,               &
       ph,                         &
       sst,                        &
       sss,                        &
       Hstar_nhx)

    use marbl_constants_mod, only : c1
    use marbl_constants_mod, only : T0_Kelvin

    integer(int_kind)  , intent(in)  :: num_elements
    real (r8)          , intent(in)  :: ph(num_elements)
    real (r8)          , intent(in)  :: sst(num_elements)
    real (r8)          , intent(in)  :: sss(num_elements)
    real (r8)          , intent(out) :: Hstar_nhx(num_elements)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    real (r8) :: pKa(num_elements)
    real (r8) :: sstK_inv(num_elements)
    real (r8) :: H_nhx(num_elements)

    !-----------------------------------------------------------------------
    ! pKa : Bell et al., Environ Chem, Vol. 5, p. 258, 2008
    !-----------------------------------------------------------------------

    pKa(:) = 10.0423_r8 - 0.0315536_r8 * sst(:) + 0.003071_r8 * sss(:)
    sstK_inv(:) = c1 / (sst(:) + T0_Kelvin)
    H_nhx(:) = (273.15_r8 / 17.93_r8) * sstK_inv(:) * exp(9.7_r8 - 4092._r8 * sstK_inv(:))
    Hstar_nhx(:) = H_nhx(:) / (c1 + 10.0_r8 ** (pka(:) - ph(:)))

  end subroutine marbl_comp_Hstar_nhx

end module marbl_nhx_surface_emis_mod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
