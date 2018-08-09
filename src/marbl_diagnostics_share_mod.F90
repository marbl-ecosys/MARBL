module marbl_diagnostics_share_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind

  use marbl_constants_mod, only : c0

  implicit none
  private

  type :: marbl_surface_flux_diagnostics_indexing_type
     integer(int_kind) :: ECOSYS_IFRAC
     integer(int_kind) :: ECOSYS_XKW
     integer(int_kind) :: ECOSYS_ATM_PRESS
     integer(int_kind) :: PV_O2
     integer(int_kind) :: SCHMIDT_O2
     integer(int_kind) :: O2SAT
     integer(int_kind) :: CO2STAR
     integer(int_kind) :: DCO2STAR
     integer(int_kind) :: pCO2SURF
     integer(int_kind) :: DpCO2
     integer(int_kind) :: PV_CO2
     integer(int_kind) :: SCHMIDT_CO2
     integer(int_kind) :: DIC_GAS_FLUX
     integer(int_kind) :: PH
     integer(int_kind) :: ATM_CO2
     integer(int_kind) :: CO2STAR_ALT_CO2
     integer(int_kind) :: DCO2STAR_ALT_CO2
     integer(int_kind) :: pCO2SURF_ALT_CO2
     integer(int_kind) :: DpCO2_ALT_CO2
     integer(int_kind) :: DIC_GAS_FLUX_ALT_CO2
     integer(int_kind) :: PH_ALT_CO2
     integer(int_kind) :: ATM_ALT_CO2
     integer(int_kind) :: IRON_FLUX
     integer(int_kind) :: DUST_FLUX
     integer(int_kind) :: NOx_FLUX
     integer(int_kind) :: NHy_FLUX
     integer(int_kind) :: NHx_SURFACE_EMIS

     integer(int_kind) :: CISO_DI13C_GAS_FLUX       ! di13c flux
     integer(int_kind) :: CISO_DI14C_GAS_FLUX       ! di14c flux
     integer(int_kind) :: CISO_DI13C_AS_GAS_FLUX    ! air-sea di13c flux
     integer(int_kind) :: CISO_DI14C_AS_GAS_FLUX    ! air-sea di14c flux
     integer(int_kind) :: CISO_DI13C_SA_GAS_FLUX    ! sea-air di13c flux
     integer(int_kind) :: CISO_DI14C_SA_GAS_FLUX    ! sea-air di14c flux
     integer(int_kind) :: CISO_d13C_GAS_FLUX        ! surface ocean delta 13C
     integer(int_kind) :: CISO_d14C_GAS_FLUX        ! surface ocean delta 14C
     integer(int_kind) :: CISO_R13C_DIC_SURF        ! 13C/12C ratio in total DIC
     integer(int_kind) :: CISO_R14C_DIC_SURF        ! 14C/12C ratio in total DIC
     integer(int_kind) :: CISO_R13C_atm             ! atmospheric ratio of 13C/12C
     integer(int_kind) :: CISO_R14C_atm             ! atmospheric ratio of 14C/12C
     integer(int_kind) :: CISO_D13C_atm             ! atmospheric delta13C in permil
     integer(int_kind) :: CISO_D14C_atm             ! atmospheric delta14C in permil
     integer(int_kind) :: CISO_eps_aq_g_surf        ! tavg id for eps_aq_g_surf
     integer(int_kind) :: CISO_eps_dic_g_surf       ! tavg id for eps_dic_g_surf
  end type marbl_surface_flux_diagnostics_indexing_type

  type :: marbl_interior_diagnostics_indexing_type
    ! General 2D diags
    integer(int_kind) :: zsatcalc
    integer(int_kind) :: zsatarag
    integer(int_kind) :: O2_ZMIN
    integer(int_kind) :: O2_ZMIN_DEPTH
    integer(int_kind) :: photoC_TOT_zint
    integer(int_kind) :: photoC_TOT_zint_100m
    integer(int_kind) :: photoC_NO3_TOT_zint
    integer(int_kind) :: photoC_NO3_TOT_zint_100m
    integer(int_kind) :: DOC_prod_zint
    integer(int_kind) :: DOC_prod_zint_100m
    integer(int_kind) :: DOC_remin_zint
    integer(int_kind) :: DOC_remin_zint_100m
    integer(int_kind) :: DOCr_remin_zint
    integer(int_kind) :: DOCr_remin_zint_100m
    integer(int_kind) :: Jint_Ctot
    integer(int_kind) :: Jint_Ntot
    integer(int_kind) :: Jint_Ptot
    integer(int_kind) :: Jint_Sitot
    integer(int_kind) :: Jint_Fetot

    ! Particulate 2D diags
    integer(int_kind) :: calcToFloor
    integer(int_kind) :: calcToSed
    integer(int_kind) :: calcToSed_ALT_CO2
    integer(int_kind) :: pocToFloor
    integer(int_kind) :: pocToSed
    integer(int_kind) :: ponToSed
    integer(int_kind) :: SedDenitrif
    integer(int_kind) :: OtherRemin
    integer(int_kind) :: popToSed
    integer(int_kind) :: bsiToSed
    integer(int_kind) :: dustToSed
    integer(int_kind) :: pfeToSed

    ! Autotroph 2D diags
    integer(int_kind), allocatable :: N_lim_surf(:)
    integer(int_kind), allocatable :: N_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: P_lim_surf(:)
    integer(int_kind), allocatable :: P_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: Fe_lim_surf(:)
    integer(int_kind), allocatable :: Fe_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: SiO3_lim_surf(:)
    integer(int_kind), allocatable :: SiO3_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: light_lim_surf(:)
    integer(int_kind), allocatable :: light_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: photoC_zint(:)
    integer(int_kind), allocatable :: photoC_zint_100m(:)
    integer(int_kind), allocatable :: photoC_NO3_zint(:)
    integer(int_kind), allocatable :: CaCO3_form_zint(:)
    integer(int_kind), allocatable :: CaCO3_form_zint_100m(:)
    integer(int_kind), allocatable :: auto_graze_zint(:)
    integer(int_kind), allocatable :: auto_graze_zint_100m(:)
    integer(int_kind), allocatable :: auto_graze_poc_zint(:)
    integer(int_kind), allocatable :: auto_graze_poc_zint_100m(:)
    integer(int_kind), allocatable :: auto_graze_doc_zint(:)
    integer(int_kind), allocatable :: auto_graze_doc_zint_100m(:)
    integer(int_kind), allocatable :: auto_graze_zoo_zint(:)
    integer(int_kind), allocatable :: auto_graze_zoo_zint_100m(:)
    integer(int_kind), allocatable :: auto_loss_zint(:)
    integer(int_kind), allocatable :: auto_loss_zint_100m(:)
    integer(int_kind), allocatable :: auto_loss_poc_zint(:)
    integer(int_kind), allocatable :: auto_loss_poc_zint_100m(:)
    integer(int_kind), allocatable :: auto_loss_doc_zint(:)
    integer(int_kind), allocatable :: auto_loss_doc_zint_100m(:)
    integer(int_kind), allocatable :: auto_agg_zint(:)
    integer(int_kind), allocatable :: auto_agg_zint_100m(:)
    integer(int_kind) :: tot_CaCO3_form_zint
    integer(int_kind) :: tot_CaCO3_form_zint_100m

    ! Zooplankton 2D diags
    integer(int_kind), allocatable :: zoo_loss_zint(:)
    integer(int_kind), allocatable :: zoo_loss_zint_100m(:)
    integer(int_kind), allocatable :: zoo_loss_poc_zint(:)
    integer(int_kind), allocatable :: zoo_loss_poc_zint_100m(:)
    integer(int_kind), allocatable :: zoo_loss_doc_zint(:)
    integer(int_kind), allocatable :: zoo_loss_doc_zint_100m(:)
    integer(int_kind), allocatable :: zoo_graze_zint(:)
    integer(int_kind), allocatable :: zoo_graze_zint_100m(:)
    integer(int_kind), allocatable :: zoo_graze_poc_zint(:)
    integer(int_kind), allocatable :: zoo_graze_poc_zint_100m(:)
    integer(int_kind), allocatable :: zoo_graze_doc_zint(:)
    integer(int_kind), allocatable :: zoo_graze_doc_zint_100m(:)
    integer(int_kind), allocatable :: zoo_graze_zoo_zint(:)
    integer(int_kind), allocatable :: zoo_graze_zoo_zint_100m(:)
    integer(int_kind), allocatable :: x_graze_zoo_zint(:)
    integer(int_kind), allocatable :: x_graze_zoo_zint_100m(:)

    ! General 3D diags
    integer(int_kind) :: insitu_temp
    integer(int_kind) :: CO3
    integer(int_kind) :: HCO3
    integer(int_kind) :: H2CO3
    integer(int_kind) :: pH_3D
    integer(int_kind) :: CO3_ALT_CO2
    integer(int_kind) :: HCO3_ALT_CO2
    integer(int_kind) :: H2CO3_ALT_CO2
    integer(int_kind) :: pH_3D_ALT_CO2
    integer(int_kind) :: co3_sat_calc
    integer(int_kind) :: co3_sat_arag
    integer(int_kind) :: NITRIF
    integer(int_kind) :: DENITRIF
    integer(int_kind) :: O2_PRODUCTION
    integer(int_kind) :: O2_CONSUMPTION_SCALEF
    integer(int_kind) :: O2_CONSUMPTION
    integer(int_kind) :: AOU
    integer(int_kind) :: PAR_avg
    integer(int_kind) :: auto_graze_TOT
    integer(int_kind) :: photoC_TOT
    integer(int_kind) :: photoC_NO3_TOT
    integer(int_kind) :: DOC_prod
    integer(int_kind) :: DOC_remin
    integer(int_kind) :: DOCr_remin
    integer(int_kind) :: DON_prod
    integer(int_kind) :: DON_remin
    integer(int_kind) :: DONr_remin
    integer(int_kind) :: DOP_prod
    integer(int_kind) :: DOP_remin
    integer(int_kind) :: DOPr_remin
    integer(int_kind) :: DOP_loss_P_bal
    integer(int_kind) :: Fe_scavenge
    integer(int_kind) :: Fe_scavenge_rate
    integer(int_kind) :: Lig_prod
    integer(int_kind) :: Lig_loss
    integer(int_kind) :: Lig_scavenge
    integer(int_kind) :: Fefree
    integer(int_kind) :: Lig_photochem
    integer(int_kind) :: Lig_deg
    integer(int_kind) :: fesedflux

    ! Particulate 2D diags
    integer(int_kind) :: POC_FLUX_at_ref_depth
    integer(int_kind) :: POP_FLUX_at_ref_depth
    integer(int_kind) :: CaCO3_FLUX_at_ref_depth
    integer(int_kind) :: SiO2_FLUX_at_ref_depth
    integer(int_kind) :: P_iron_FLUX_at_ref_depth
    integer(int_kind) :: POC_PROD_zint
    integer(int_kind) :: POC_PROD_zint_100m
    integer(int_kind) :: POC_REMIN_DOCr_zint
    integer(int_kind) :: POC_REMIN_DOCr_zint_100m
    integer(int_kind) :: POC_REMIN_DIC_zint
    integer(int_kind) :: POC_REMIN_DIC_zint_100m
    integer(int_kind) :: CaCO3_PROD_zint
    integer(int_kind) :: CaCO3_PROD_zint_100m
    integer(int_kind) :: CaCO3_REMIN_zint
    integer(int_kind) :: CaCO3_REMIN_zint_100m

    ! Particulate 3D diags
    integer(int_kind) :: P_REMIN_SCALEF
    integer(int_kind) :: POC_FLUX_IN
    integer(int_kind) :: POC_sFLUX_IN
    integer(int_kind) :: POC_hFLUX_IN
    integer(int_kind) :: POC_PROD
    integer(int_kind) :: POC_REMIN_DOCr
    integer(int_kind) :: POC_REMIN_DIC
    integer(int_kind) :: POP_FLUX_IN
    integer(int_kind) :: POP_PROD
    integer(int_kind) :: POP_REMIN_DOPr
    integer(int_kind) :: POP_REMIN_PO4
    integer(int_kind) :: PON_REMIN_DONr
    integer(int_kind) :: PON_REMIN_NH4
    integer(int_kind) :: CaCO3_FLUX_IN
    integer(int_kind) :: CaCO3_PROD
    integer(int_kind) :: CaCO3_REMIN
    integer(int_kind) :: CaCO3_ALT_CO2_FLUX_IN
    integer(int_kind) :: CaCO3_ALT_CO2_PROD
    integer(int_kind) :: CaCO3_ALT_CO2_REMIN
    integer(int_kind) :: SiO2_FLUX_IN
    integer(int_kind) :: SiO2_PROD
    integer(int_kind) :: SiO2_REMIN
    integer(int_kind) :: dust_FLUX_IN
    integer(int_kind) :: dust_REMIN
    integer(int_kind) :: P_iron_FLUX_IN
    integer(int_kind) :: P_iron_PROD
    integer(int_kind) :: P_iron_REMIN

    ! Autotroph 3D diags
    integer(int_kind), allocatable :: Qp(:)
    integer(int_kind), allocatable :: photoC(:)
    integer(int_kind), allocatable :: photoC_NO3(:)
    integer(int_kind), allocatable :: photoFe(:)
    integer(int_kind), allocatable :: photoNO3(:)
    integer(int_kind), allocatable :: photoNH4(:)
    integer(int_kind), allocatable :: DOP_uptake(:)
    integer(int_kind), allocatable :: PO4_uptake(:)
    integer(int_kind), allocatable :: auto_graze(:)
    integer(int_kind), allocatable :: auto_graze_poc(:)
    integer(int_kind), allocatable :: auto_graze_doc(:)
    integer(int_kind), allocatable :: auto_graze_zoo(:)
    integer(int_kind), allocatable :: auto_loss(:)
    integer(int_kind), allocatable :: auto_loss_poc(:)
    integer(int_kind), allocatable :: auto_loss_doc(:)
    integer(int_kind), allocatable :: auto_agg(:)
    integer(int_kind), allocatable :: bSi_form(:)
    integer(int_kind), allocatable :: CaCO3_form(:)
    integer(int_kind), allocatable :: Nfix(:)
    integer(int_kind) :: tot_bSi_form
    integer(int_kind) :: tot_CaCO3_form
    integer(int_kind) :: tot_Nfix

    ! Zooplankton 3D diags
    integer(int_kind), allocatable :: zoo_loss(:)
    integer(int_kind), allocatable :: zoo_loss_poc(:)
    integer(int_kind), allocatable :: zoo_loss_doc(:)
    integer(int_kind), allocatable :: zoo_graze(:)
    integer(int_kind), allocatable :: zoo_graze_poc(:)
    integer(int_kind), allocatable :: zoo_graze_doc(:)
    integer(int_kind), allocatable :: zoo_graze_zoo(:)
    integer(int_kind), allocatable :: x_graze_zoo(:)

     !  ciso ids for nonstandard 3d fields
     integer (int_kind) :: CISO_PO13C_FLUX_IN                                 ! po13c flux into cell
     integer (int_kind) :: CISO_PO14C_FLUX_IN                                 ! po14c flux into cell
     integer (int_kind) :: CISO_PO13C_PROD                                    ! po13c production
     integer (int_kind) :: CISO_PO14C_PROD                                    ! po14c production
     integer (int_kind) :: CISO_PO13C_REMIN                                   ! po13c remineralization
     integer (int_kind) :: CISO_PO14C_REMIN                                   ! po14c remineralization
     integer (int_kind) :: CISO_Ca13CO3_PROD                                  ! ca13co3 production
     integer (int_kind) :: CISO_Ca14CO3_PROD                                  ! ca14co3 production
     integer (int_kind) :: CISO_Ca13CO3_REMIN                                 ! ca13co3 remineralization
     integer (int_kind) :: CISO_Ca14CO3_REMIN                                 ! ca14co3 remineralization
     integer (int_kind) :: CISO_Ca13CO3_FLUX_IN                               ! ca13co3 flux into cell
     integer (int_kind) :: CISO_Ca14CO3_FLUX_IN                               ! ca14co3 flux into cell
     integer (int_kind) :: CISO_photo13C_TOT                                  ! total 13C fixation
     integer (int_kind) :: CISO_photo14C_TOT                                  ! total 14C fixation
     integer (int_kind) :: CISO_photo13C_TOT_zint                             ! total 13C fixation vertical integral
     integer (int_kind) :: CISO_photo14C_TOT_zint                             ! total 14C fixation vertical integral

     ! ciso ids for  MORE nonstandard 3d fields
     integer (int_kind), allocatable :: CISO_eps_autotroph(:)       ! epsilon for each autotroph
     integer (int_kind), allocatable :: CISO_mui_to_co2star(:)      ! mui_to_co2star for each autotroph
     integer (int_kind), allocatable :: CISO_Ca13CO3_form(:)        ! Ca13CO3 formation
     integer (int_kind), allocatable :: CISO_Ca14CO3_form(:)        ! Ca14CO3 formation
     integer (int_kind), allocatable :: CISO_Ca13CO3_form_zint(:)   ! Ca13CO3 formation vertical integral 0-100 m
     integer (int_kind), allocatable :: CISO_Ca14CO3_form_zint(:)   ! Ca14CO3 formation vertical integral 0-100 m
     integer (int_kind), allocatable :: CISO_photo13C(:)            ! 13C fixation
     integer (int_kind), allocatable :: CISO_photo14C(:)            ! 14C fixation
     integer (int_kind), allocatable :: CISO_photo13C_zint(:)       ! 13C fixation vertical integral
     integer (int_kind), allocatable :: CISO_photo14C_zint(:)       ! 14C fixation vertical integral
     integer (int_kind), allocatable :: CISO_d13C(:)                ! d13C of autotroph carbon
     integer (int_kind), allocatable :: CISO_d14C(:)                ! d14C of autotroph carbon
     integer (int_kind), allocatable :: CISO_autotrophCaCO3_d14C(:) ! d14C of autotrophCaCO3
     integer (int_kind), allocatable :: CISO_autotrophCaCO3_d13C(:) ! d13C of autotrophCaCO3

     integer (int_kind) :: CISO_eps_aq_g                                      ! eps_aq_g
     integer (int_kind) :: CISO_eps_dic_g                                     ! eps_dic_g
     integer (int_kind) :: CISO_DO13Ctot_prod                                 ! do13ctot production
     integer (int_kind) :: CISO_DO14Ctot_prod                                 ! do14ctot production
     integer (int_kind) :: CISO_DO13Ctot_remin                                ! do13ctot remineralization
     integer (int_kind) :: CISO_DO14Ctot_remin                                ! do14ctot remineralization
     integer (int_kind) :: CISO_Jint_13Ctot                                   ! vertically integrated source sink term, 13Ctot
     integer (int_kind) :: CISO_Jint_14Ctot                                   ! vertically integrated source sink term, 14Ctot
     integer (int_kind) :: CISO_zoototC_d13C                                  ! d13C of total zooC
     integer (int_kind) :: CISO_zoototC_d14C                                  ! d14C of total zooC
     integer (int_kind) :: CISO_DOCtot_d13C                                   ! d13C of DOCtot
     integer (int_kind) :: CISO_DOCtot_d14C                                   ! d14C of DOCtot
     integer (int_kind) :: CISO_DIC_d13C                                      ! d13C of DIC
     integer (int_kind) :: CISO_DIC_d14C                                      ! d14C of DIC
     integer (int_kind) :: calcToSed_13C                                      ! calcite flux sedimentary burial
     integer (int_kind) :: calcToSed_14C                                      ! calcite flux sedimentary burial
     integer (int_kind) :: pocToSed_13C                                       ! poc burial flux to sediments
     integer (int_kind) :: pocToSed_14C                                       ! poc burial flux to sediments

     ! restoring 3D diags
     integer(int_kind), dimension(:), allocatable :: restore_tend
   contains
     procedure, public :: lconstructed => interior_diag_ind_constructed
     procedure, public :: destruct => interior_diag_ind_destructor
  end type marbl_interior_diagnostics_indexing_type

  !-----------------------------------------------------------------------
  !  Indices for diagnostic values written to tavg files
  !-----------------------------------------------------------------------

  type(marbl_surface_flux_diagnostics_indexing_type) :: marbl_surface_flux_diag_ind
  type(marbl_interior_diagnostics_indexing_type) :: marbl_interior_diag_ind

  public :: marbl_surface_flux_diag_ind
  public :: marbl_interior_diag_ind
  public :: marbl_diagnostics_share_compute_vertical_integrals

  !***********************************************************************

contains

  !*****************************************************************************

  subroutine marbl_diagnostics_share_compute_vertical_integrals(integrand, delta_z, kmt, &
       full_depth_integral, near_surface_integral, integrated_terms, shallow_depth)

    real(kind=r8) , intent(in)             :: integrand(:)
    real(kind=r8) , intent(in)             :: delta_z(:)
    integer       , intent(in)             :: kmt
    ! For some vertical integral diagnostics, we need to add additional terms
    ! that have already been integrated, so they are separated from the
    ! integrand
    real(kind=r8) , intent(in)  , optional :: integrated_terms(:)
    real(kind=r8) , intent(in)  , optional :: shallow_depth
    real(kind=r8) , intent(out) , optional :: full_depth_integral
    real(kind=r8) , intent(out) , optional :: near_surface_integral

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: k
    real(kind=r8)      :: integrated_terms_used(size(integrand))
    real(kind=r8)      :: zw
    real(kind=r8)      :: ztop
    real(kind=r8)      :: shallow_depth_used
    !-----------------------------------------------------------------------

    if (present(integrated_terms)) then
       integrated_terms_used = integrated_terms
    else
       integrated_terms_used = c0
    end if

    if (present(shallow_depth)) then
      shallow_depth_used = shallow_depth
    else
      shallow_depth_used = 100.0e2_r8
    end if

    if (present(full_depth_integral)) then
       full_depth_integral = sum(delta_z(1:kmt)*integrand(1:kmt) + integrated_terms_used(1:kmt))
    end if

    if (present(near_surface_integral)) then
       ! initialize integral to zero
       near_surface_integral = c0
       ztop = c0
       zw = c0
       do k=1,kmt
          zw = zw + delta_z(k)
          near_surface_integral = near_surface_integral +                     &
                              min(shallow_depth_used-ztop,delta_z(k))*integrand(k)
          if (zw.le.shallow_depth_used) then
             near_surface_integral = near_surface_integral + integrated_terms_used(k)
          else
             exit
          end if
          ztop = zw
       end do
    end if

  end subroutine marbl_diagnostics_share_compute_vertical_integrals

  !*****************************************************************************

  function interior_diag_ind_constructed(this) result(constructed)

    class(marbl_interior_diagnostics_indexing_type), intent(inout) :: this
    logical(log_kind) :: constructed

    constructed = allocated(this%restore_tend)

  end function interior_diag_ind_constructed

  !*****************************************************************************

  subroutine interior_diag_ind_destructor(this)

    use marbl_settings_mod, only : ciso_on

    class(marbl_interior_diagnostics_indexing_type), intent(inout) :: this

    if (this%lconstructed()) then
      deallocate(this%N_lim_surf)
      deallocate(this%N_lim_Cweight_avg_100m)
      deallocate(this%P_lim_surf)
      deallocate(this%P_lim_Cweight_avg_100m)
      deallocate(this%Fe_lim_surf)
      deallocate(this%Fe_lim_Cweight_avg_100m)
      deallocate(this%SiO3_lim_surf)
      deallocate(this%SiO3_lim_Cweight_avg_100m)
      deallocate(this%light_lim_surf)
      deallocate(this%light_lim_Cweight_avg_100m)
      deallocate(this%photoC_zint)
      deallocate(this%photoC_zint_100m)
      deallocate(this%photoC_NO3_zint)
      deallocate(this%CaCO3_form_zint)
      deallocate(this%CaCO3_form_zint_100m)
      deallocate(this%auto_graze_zint)
      deallocate(this%auto_graze_zint_100m)
      deallocate(this%auto_graze_poc_zint)
      deallocate(this%auto_graze_poc_zint_100m)
      deallocate(this%auto_graze_doc_zint)
      deallocate(this%auto_graze_doc_zint_100m)
      deallocate(this%auto_graze_zoo_zint)
      deallocate(this%auto_graze_zoo_zint_100m)
      deallocate(this%auto_loss_zint)
      deallocate(this%auto_loss_zint_100m)
      deallocate(this%auto_loss_poc_zint)
      deallocate(this%auto_loss_poc_zint_100m)
      deallocate(this%auto_loss_doc_zint)
      deallocate(this%auto_loss_doc_zint_100m)
      deallocate(this%auto_agg_zint)
      deallocate(this%auto_agg_zint_100m)
      deallocate(this%zoo_loss_zint)
      deallocate(this%zoo_loss_zint_100m)
      deallocate(this%zoo_loss_poc_zint)
      deallocate(this%zoo_loss_poc_zint_100m)
      deallocate(this%zoo_loss_doc_zint)
      deallocate(this%zoo_loss_doc_zint_100m)
      deallocate(this%zoo_graze_zint)
      deallocate(this%zoo_graze_zint_100m)
      deallocate(this%zoo_graze_poc_zint)
      deallocate(this%zoo_graze_poc_zint_100m)
      deallocate(this%zoo_graze_doc_zint)
      deallocate(this%zoo_graze_doc_zint_100m)
      deallocate(this%zoo_graze_zoo_zint)
      deallocate(this%zoo_graze_zoo_zint_100m)
      deallocate(this%x_graze_zoo_zint)
      deallocate(this%x_graze_zoo_zint_100m)
      deallocate(this%Qp)
      deallocate(this%photoC)
      deallocate(this%photoC_NO3)
      deallocate(this%photoFe)
      deallocate(this%photoNO3)
      deallocate(this%photoNH4)
      deallocate(this%DOP_uptake)
      deallocate(this%PO4_uptake)
      deallocate(this%auto_graze)
      deallocate(this%auto_graze_poc)
      deallocate(this%auto_graze_doc)
      deallocate(this%auto_graze_zoo)
      deallocate(this%auto_loss)
      deallocate(this%auto_loss_poc)
      deallocate(this%auto_loss_doc)
      deallocate(this%auto_agg)
      deallocate(this%bSi_form)
      deallocate(this%CaCO3_form)
      deallocate(this%Nfix)
      deallocate(this%zoo_loss)
      deallocate(this%zoo_loss_poc)
      deallocate(this%zoo_loss_doc)
      deallocate(this%zoo_graze)
      deallocate(this%zoo_graze_poc)
      deallocate(this%zoo_graze_doc)
      deallocate(this%zoo_graze_zoo)
      deallocate(this%x_graze_zoo)
      if (ciso_on) then
        deallocate(this%CISO_eps_autotroph)
        deallocate(this%CISO_mui_to_co2star)
        deallocate(this%CISO_Ca13CO3_form)
        deallocate(this%CISO_Ca14CO3_form)
        deallocate(this%CISO_Ca13CO3_form_zint)
        deallocate(this%CISO_Ca14CO3_form_zint)
        deallocate(this%CISO_photo13C)
        deallocate(this%CISO_photo14C)
        deallocate(this%CISO_photo13C_zint)
        deallocate(this%CISO_photo14C_zint)
        deallocate(this%CISO_d13C)
        deallocate(this%CISO_d14C)
        deallocate(this%CISO_autotrophCaCO3_d14C)
        deallocate(this%CISO_autotrophCaCO3_d13C)
      end if
      deallocate(this%restore_tend)
    end if

  end subroutine interior_diag_ind_destructor

  !***********************************************************************

end module marbl_diagnostics_share_mod