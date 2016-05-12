! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_diagnostics_mod

  use marbl_kinds_mod       , only : r8
  use marbl_kinds_mod       , only : int_kind
  use marbl_kinds_mod       , only : log_kind
  use marbl_kinds_mod       , only : char_len

  use marbl_sizes           , only : marbl_total_tracer_cnt
  use marbl_sizes           , only : autotroph_cnt
  use marbl_sizes           , only : zooplankton_cnt

  use marbl_parms           , only : autotrophs
  use marbl_parms           , only : zooplankton
  use marbl_parms           , only : c0
  use marbl_parms           , only : c1

  use marbl_internal_types  , only : carbonate_type
  use marbl_internal_types  , only : zooplankton_type
  use marbl_internal_types  , only : autotroph_type
  use marbl_internal_types  , only : zooplankton_secondary_species_type
  use marbl_internal_types  , only : autotroph_secondary_species_type
  use marbl_internal_types  , only : dissolved_organic_matter_type
  use marbl_internal_types  , only : column_sinking_particle_type
  use marbl_internal_types  , only : marbl_PAR_type
  use marbl_internal_types  , only : marbl_particulate_share_type
  use marbl_internal_types  , only : marbl_interior_share_type
  use marbl_internal_types  , only : marbl_autotroph_share_type
  use marbl_internal_types  , only : marbl_zooplankton_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_internal_type
  use marbl_internal_types , only : marbl_tracer_index_type

  use marbl_interface_types , only : marbl_domain_type
  use marbl_interface_types , only : marbl_tracer_metadata_type
  use marbl_interface_types , only : marbl_saved_state_type
  use marbl_interface_types , only : marbl_interior_forcing_input_type
  use marbl_interface_types , only : marbl_surface_forcing_output_type
  use marbl_interface_types , only : sfo_ind
  use marbl_interface_types , only : marbl_surface_forcing_indexing_type
  use marbl_interface_types , only : marbl_diagnostics_type

  use marbl_logging,          only : marbl_log_type

  implicit none
  public

  !-----------------------------------------------------------------------
  !  public/private member procedure declarations
  !-----------------------------------------------------------------------

  public :: marbl_diagnostics_init
  public :: marbl_diagnostics_set_interior_forcing
  public :: marbl_diagnostics_set_surface_forcing

  private :: store_diagnostics_carbonate
  private :: store_diagnostics_nitrification
  private :: store_diagnostics_autotrophs
  private :: store_diagnostics_autotroph_sums
  private :: store_diagnostics_particulates
  private :: store_diagnostics_oxygen
  private :: store_diagnostics_PAR
  private :: store_diagnostics_misc
  private :: store_diagnostics_zooplankton
  private :: store_diagnostics_dissolved_organic_matter
  private :: store_diagnostics_carbon_fluxes
  private :: store_diagnostics_nitrogen_fluxes
  private :: store_diagnostics_phosphorus_fluxes
  private :: store_diagnostics_silicon_fluxes
  private :: store_diagnostics_iron_fluxes
  private :: compute_saturation_depth
  private :: linear_root

  !-----------------------------------------------------------------------
  !  Largest possible size for each class of diagnostics
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  !  indices for diagnostic values written to tavg files
  !-----------------------------------------------------------------------

  type, private :: marbl_interior_diagnostics_indexing_type
    ! General 2D diags
    integer(int_kind) :: zsatcalc
    integer(int_kind) :: zsatarag
    integer(int_kind) :: O2_ZMIN
    integer(int_kind) :: O2_ZMIN_DEPTH
    integer(int_kind) :: photoC_TOT_zint
    integer(int_kind) :: photoC_NO3_TOT_zint
    integer(int_kind) :: Jint_Ctot
    integer(int_kind) :: Jint_100m_Ctot
    integer(int_kind) :: Jint_Ntot
    integer(int_kind) :: Jint_100m_Ntot
    integer(int_kind) :: Jint_Ptot
    integer(int_kind) :: Jint_100m_Ptot
    integer(int_kind) :: Jint_Sitot
    integer(int_kind) :: Jint_100m_Sitot
    integer(int_kind) :: Jint_Fetot
    integer(int_kind) :: Jint_100m_Fetot

    ! Particulate 2D diags
    integer(int_kind) :: calcToSed
    integer(int_kind) :: pocToSed
    integer(int_kind) :: ponToSed
    integer(int_kind) :: SedDenitrif
    integer(int_kind) :: OtherRemin
    integer(int_kind) :: popToSed
    integer(int_kind) :: bsiToSed
    integer(int_kind) :: dustToSed
    integer(int_kind) :: pfeToSed

    ! Autotroph 2D diags
    integer(int_kind), dimension(autotroph_cnt) :: photoC_zint
    integer(int_kind), dimension(autotroph_cnt) :: photoC_NO3_zint
    integer(int_kind), dimension(autotroph_cnt) :: CaCO3_form_zint
    integer(int_kind) :: tot_CaCO3_form_zint

    ! General 3D diags
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
    integer(int_kind) :: Fe_scavenge
    integer(int_kind) :: Fe_scavenge_rate

    ! Particulate 3D diags
    integer(int_kind) :: POC_FLUX_IN
    integer(int_kind) :: POC_PROD
    integer(int_kind) :: POC_REMIN
    integer(int_kind) :: POC_REMIN_DIC
    integer(int_kind) :: PON_REMIN_NH4
    integer(int_kind) :: POP_REMIN_PO4
    integer(int_kind) :: CaCO3_FLUX_IN
    integer(int_kind) :: CaCO3_PROD
    integer(int_kind) :: CaCO3_REMIN
    integer(int_kind) :: SiO2_FLUX_IN
    integer(int_kind) :: SiO2_PROD
    integer(int_kind) :: SiO2_REMIN
    integer(int_kind) :: dust_FLUX_IN
    integer(int_kind) :: dust_REMIN
    integer(int_kind) :: P_iron_FLUX_IN
    integer(int_kind) :: P_iron_PROD
    integer(int_kind) :: P_iron_REMIN

    ! Autotroph 3D diags
    integer(int_kind), dimension(autotroph_cnt) :: N_lim
    integer(int_kind), dimension(autotroph_cnt) :: P_lim
    integer(int_kind), dimension(autotroph_cnt) :: Fe_lim
    integer(int_kind), dimension(autotroph_cnt) :: SiO3_lim
    integer(int_kind), dimension(autotroph_cnt) :: light_lim
    integer(int_kind), dimension(autotroph_cnt) :: photoC
    integer(int_kind), dimension(autotroph_cnt) :: photoC_NO3
    integer(int_kind), dimension(autotroph_cnt) :: photoFe
    integer(int_kind), dimension(autotroph_cnt) :: photoNO3
    integer(int_kind), dimension(autotroph_cnt) :: photoNH4
    integer(int_kind), dimension(autotroph_cnt) :: DOP_uptake
    integer(int_kind), dimension(autotroph_cnt) :: PO4_uptake
    integer(int_kind), dimension(autotroph_cnt) :: auto_graze
    integer(int_kind), dimension(autotroph_cnt) :: auto_graze_poc
    integer(int_kind), dimension(autotroph_cnt) :: auto_graze_doc
    integer(int_kind), dimension(autotroph_cnt) :: auto_graze_zoo
    integer(int_kind), dimension(autotroph_cnt) :: auto_loss
    integer(int_kind), dimension(autotroph_cnt) :: auto_loss_poc
    integer(int_kind), dimension(autotroph_cnt) :: auto_loss_doc
    integer(int_kind), dimension(autotroph_cnt) :: auto_agg
    integer(int_kind), dimension(autotroph_cnt) :: bSi_form
    integer(int_kind), dimension(autotroph_cnt) :: CaCO3_form
    integer(int_kind), dimension(autotroph_cnt) :: Nfix
    integer(int_kind) :: tot_bSi_form
    integer(int_kind) :: tot_CaCO3_form
    integer(int_kind) :: tot_Nfix

    ! zooplankton 3D diags
    integer(int_kind), dimension(zooplankton_cnt) :: zoo_loss
    integer(int_kind), dimension(zooplankton_cnt) :: zoo_loss_poc
    integer(int_kind), dimension(zooplankton_cnt) :: zoo_loss_doc
    integer(int_kind), dimension(zooplankton_cnt) :: zoo_graze
    integer(int_kind), dimension(zooplankton_cnt) :: zoo_graze_poc
    integer(int_kind), dimension(zooplankton_cnt) :: zoo_graze_doc
    integer(int_kind), dimension(zooplankton_cnt) :: zoo_graze_zoo
    integer(int_kind), dimension(zooplankton_cnt) :: x_graze_zoo

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
     integer (int_kind), dimension(autotroph_cnt) :: CISO_eps_autotroph       ! epsilon for each autotroph
     integer (int_kind), dimension(autotroph_cnt) :: CISO_mui_to_co2star      ! mui_to_co2star for each autotroph
     integer (int_kind), dimension(autotroph_cnt) :: CISO_Ca13CO3_form        ! Ca13CO3 formation
     integer (int_kind), dimension(autotroph_cnt) :: CISO_Ca14CO3_form        ! Ca14CO3 formation
     integer (int_kind), dimension(autotroph_cnt) :: CISO_Ca13CO3_form_zint   ! Ca13CO3 formation vertical integral 0-100 m
     integer (int_kind), dimension(autotroph_cnt) :: CISO_Ca14CO3_form_zint   ! Ca14CO3 formation vertical integral 0-100 m
     integer (int_kind), dimension(autotroph_cnt) :: CISO_photo13C            ! 13C fixation
     integer (int_kind), dimension(autotroph_cnt) :: CISO_photo14C            ! 14C fixation
     integer (int_kind), dimension(autotroph_cnt) :: CISO_photo13C_zint       ! 13C fixation vertical integral
     integer (int_kind), dimension(autotroph_cnt) :: CISO_photo14C_zint       ! 14C fixation vertical integral
     integer (int_kind), dimension(autotroph_cnt) :: CISO_d13C                ! if for d13C of autotroph carbon
     integer (int_kind), dimension(autotroph_cnt) :: CISO_d14C                ! if for d14C of autotroph carbon
     integer (int_kind), dimension(autotroph_cnt) :: CISO_autotrophCaCO3_d14C ! if for d14C of autotrophCaCO3
     integer (int_kind), dimension(autotroph_cnt) :: CISO_autotrophCaCO3_d13C ! if for d13C of autotrophCaCO3

     integer (int_kind) :: CISO_eps_aq_g                                      ! eps_aq_g
     integer (int_kind) :: CISO_eps_dic_g                                     ! eps_dic_g
     integer (int_kind) :: CISO_DO13C_prod                                    ! do13c production
     integer (int_kind) :: CISO_DO14C_prod                                    ! do14c production
     integer (int_kind) :: CISO_DO13C_remin                                   ! do13c remineralization
     integer (int_kind) :: CISO_DO14C_remin                                   ! do14c remineralization
     integer (int_kind) :: CISO_Jint_13Ctot                                   ! vertically integrated source sink term, 13Ctot
     integer (int_kind) :: CISO_Jint_14Ctot                                   ! vertically integrated source sink term, 14Ctot
     integer (int_kind) :: CISO_Jint_100m_13Ctot                              ! vertically integrated source sink term, 0-100m, 13Ctot
     integer (int_kind) :: CISO_Jint_100m_14Ctot                              ! vertically integrated source sink term, 0-100m, 14Ctot
     integer (int_kind) :: CISO_zooC_d13C                                     ! if for d13C of zooC
     integer (int_kind) :: CISO_zooC_d14C                                     ! if for d14C of zooC
     integer (int_kind) :: CISO_DOC_d13C                                      ! if for d13C of DOC
     integer (int_kind) :: CISO_DOC_d14C                                      ! if for d14C of DOC
     integer (int_kind) :: CISO_DIC_d13C                                      ! if for d13C of DIC
     integer (int_kind) :: CISO_DIC_d14C                                      ! if for d14C of DIC
     integer (int_kind) :: calcToSed_13C                                      ! calcite flux sedimentary burial
     integer (int_kind) :: calcToSed_14C                                      ! calcite flux sedimentary burial
     integer (int_kind) :: pocToSed_13C                                       ! poc burial flux to sediments
     integer (int_kind) :: pocToSed_14C                                       ! poc burial flux to sediments

  end type marbl_interior_diagnostics_indexing_type
  type(marbl_interior_diagnostics_indexing_type), public :: marbl_interior_diag_ind

  !***********************************************************************

  type marbl_surface_forcing_diagnostics_indexing_type
     integer(int_kind) :: ECOSYS_IFRAC
     integer(int_kind) :: ECOSYS_XKW
     integer(int_kind) :: ECOSYS_ATM_PRESS
     integer(int_kind) :: PV_O2
     integer(int_kind) :: SCHMIDT_O2
     integer(int_kind) :: O2SAT
     integer(int_kind) :: O2_GAS_FLUX
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
     integer(int_kind) :: DIN_RIV_FLUX
     integer(int_kind) :: DIP_RIV_FLUX
     integer(int_kind) :: DON_RIV_FLUX
     integer(int_kind) :: DONr_RIV_FLUX
     integer(int_kind) :: DOP_RIV_FLUX
     integer(int_kind) :: DOPr_RIV_FLUX
     integer(int_kind) :: DSI_RIV_FLUX
     integer(int_kind) :: DFE_RIV_FLUX
     integer(int_kind) :: DIC_RIV_FLUX
     integer(int_kind) :: ALK_RIV_FLUX
     integer(int_kind) :: DOC_RIV_FLUX
     integer(int_kind) :: DOCr_RIV_FLUX
     
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
     integer(int_kind) :: CISO_DI13C_RIV_FLUX       ! river input of DI13C
     integer(int_kind) :: CISO_DI14C_RIV_FLUX       ! river input of DI14C
     integer(int_kind) :: CISO_DO13C_RIV_FLUX       ! river input of DO13C
     integer(int_kind) :: CISO_DO14C_RIV_FLUX       ! river input of DO14C
     integer(int_kind) :: CISO_eps_aq_g_surf        ! tavg id for eps_aq_g_surf
     integer(int_kind) :: CISO_eps_dic_g_surf       ! tavg id for eps_dic_g_surf
     integer(int_kind) :: CISO_GLOBAL_D14C          ! tavg id for the global averaged atmos. D14C (debugging)
  end type marbl_surface_forcing_diagnostics_indexing_type
  type(marbl_surface_forcing_diagnostics_indexing_type), public :: marbl_surface_forcing_diag_ind

  !***********************************************************************

contains

  !***********************************************************************

  subroutine marbl_diagnostics_init( &
       ciso_on,                      &
       marbl_domain,                 &
       marbl_tracer_metadata,        &
       marbl_tracer_indices,         &
       marbl_interior_forcing_diags, &
       marbl_interior_restore_diags, &
       marbl_surface_forcing_diags,  &
       marbl_status_log)

    logical (log_kind)                , intent(in)    :: ciso_on 
    type(marbl_domain_type)           , intent(in)    :: marbl_domain
    type(marbl_tracer_metadata_type)  , intent(in)    :: marbl_tracer_metadata(:) ! descriptors for each tracer
    type(marbl_tracer_index_type)     , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)      , intent(inout) :: marbl_interior_forcing_diags
    type(marbl_diagnostics_type)      , intent(inout) :: marbl_interior_restore_diags
    type(marbl_diagnostics_type)      , intent(inout) :: marbl_surface_forcing_diags
    type(marbl_log_type)              , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer :: n, tmp_id
    logical :: count_only ! true => count the diagnostics, false => add the diagnostics
    integer :: imode      ! imode = 1, count_only is true, otherwise count_only is false
    logical :: truncate
    integer :: num_interior_diags
    integer :: num_restore_diags
    integer :: num_forcing_diags
    character(len=char_len) :: lname, sname, units, vgrid

    character(*), parameter :: subname = 'marbl_diagnostics_mod:marbl_diagnostics_init'
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------
    ! Surface forcing diagnostics
    !-----------------------------------------------------------------

    num_forcing_diags  = 0
    num_interior_diags = 0
    num_restore_diags  = 0

    do imode = 1,2
    
       if (imode == 1) then
          count_only = .true.
       else
          count_only = .false.
          associate(                                                                &
               num_elements_interior => marbl_domain%num_elements_interior_forcing, &
               num_elements_forcing  => marbl_domain%num_elements_surface_forcing,  &
               num_levels            => marbl_domain%km                             &
               )
          call marbl_surface_forcing_diags%construct (num_forcing_diags , num_elements_forcing , num_levels)
          call marbl_interior_forcing_diags%construct(num_interior_diags, num_elements_interior, num_levels)
          call marbl_interior_restore_diags%construct(num_restore_diags , num_elements_interior, num_levels)
          end associate
       end if

       associate(                                  &
            ind => marbl_surface_forcing_diag_ind, &
            diags => marbl_surface_forcing_diags   &
            )

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Ice Fraction for ecosys fluxes'
          sname    = 'ECOSYS_IFRAC'
          units    = 'fraction'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ECOSYS_IFRAC, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'XKW for ecosys fluxes'
          sname    = 'ECOSYS_XKW'
          units    = 'cm/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ECOSYS_XKW, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Atmospheric Pressure for ecosys fluxes'
          sname    = 'ECOSYS_ATM_PRESS'
          units    = 'atmospheres'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ECOSYS_ATM_PRESS, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'PV_O2'
          sname    = 'PV_O2'
          units    = 'cm/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%PV_O2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'O2 Schmidt Number'
          sname    = 'SCHMIDT_O2'
          units    = 'none'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%SCHMIDT_O2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'O2 Saturation'
          sname    = 'O2SAT'
          units    = 'mmol/m^3'      ! = nmol/cm^3
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%O2SAT, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Dissolved Oxygen Surface Flux'
          sname    = 'STF_O2'
          units    = 'mmol/m^3 cm/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%O2_GAS_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'CO2 Star'
          sname    = 'CO2STAR'
          units    = 'mmol/m^3'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%CO2STAR, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'D CO2 Star'
          sname    = 'DCO2STAR'
          units    = 'mmol/m^3'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DCO2STAR, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'surface pCO2'
          sname    = 'pCO2SURF'
          units    = 'ppmv'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%pCO2SURF, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'D pCO2'
          sname    = 'DpCO2'
          units    = 'ppmv'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DpCO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'CO2 Piston Velocity'
          sname    = 'PV_CO2'
          units    = 'cm/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%PV_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'CO2 Schmidt Number'
          sname    = 'SCHMIDT_CO2'
          units    = 'none'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%SCHMIDT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'DIC Surface Gas Flux'
          sname    = 'FG_CO2'
          units    = 'mmol/m^3 cm/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DIC_GAS_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Surface pH'
          sname    = 'PH'
          units    = 'none'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%PH, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Atmospheric CO2'
          sname    = 'ATM_CO2'
          units    = 'ppmv'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ATM_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'CO2 Star, Alternative CO2'
          sname    = 'CO2STAR_ALT_CO2'
          units    = 'mmol/m^3'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%CO2STAR_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'D CO2 Star, Alternative CO2'
          sname    = 'DCO2STAR_ALT_CO2'
          units    = 'mmol/m^3'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DCO2STAR_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'surface pCO2, Alternative CO2'
          sname    = 'pCO2SURF_ALT_CO2'
          units    = 'ppmv'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%pCO2SURF_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'D pCO2, Alternative CO2'
          sname    = 'DpCO2_ALT_CO2'
          units    = 'ppmv'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DpCO2_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'DIC Surface Gas Flux, Alternative CO2'
          sname    = 'FG_ALT_CO2'
          units    = 'mmol/m^3 cm/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DIC_GAS_FLUX_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Surface pH, Alternative CO2'
          sname    = 'PH_ALT_CO2'
          units    = 'none'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%PH_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Atmospheric Alternative CO2'
          sname    = 'ATM_ALT_CO2'
          units    = 'ppmv'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ATM_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Atmospheric Iron Flux'
          sname    = 'IRON_FLUX'
          units    = 'mmol/m^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%IRON_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Dust Flux'
          sname    = 'DUST_FLUX'
          units    = 'g/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DUST_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of NOx from Atmosphere'
          sname    = 'NOx_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%NOx_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of NHy from Atmosphere'
          sname    = 'NHy_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%NHy_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DIN from rivers'
          sname    = 'DIN_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DIN_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DIP from rivers'
          sname    = 'DIP_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DIP_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DON from rivers'
          sname    = 'DON_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DON_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DONr from rivers'
          sname    = 'DONr_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DONr_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DOP from rivers'
          sname    = 'DOP_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOP_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DOPr from rivers'
          sname    = 'DOPr_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOPr_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DSI from rivers'
          sname    = 'DSI_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DSI_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DFE from rivers'
          sname    = 'DFE_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DFE_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DIC from rivers'
          sname    = 'DIC_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DIC_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of ALK from rivers'
          sname    = 'ALK_RIV_FLUX'
          units    = 'alk/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ALK_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DOC from rivers'
          sname    = 'DOC_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOC_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_forcing_diags = num_forcing_diags + 1
       else
          lname    = 'Flux of DOCr from rivers'
          sname    = 'DOCr_RIV_FLUX'
          units    = 'nmol/cm^2/s'
          vgrid    = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOCr_RIV_FLUX, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       !-----------------------------------------------------------------------
       !  2D fields related to C13/C14 surface fluxes
       !-----------------------------------------------------------------------
       
       if (ciso_on) then
          
          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'DI13C Surface Gas Flux'
             sname    = 'CISO_FG_13CO2'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DI13C_GAS_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'DI13C Surface Air-Sea Gas Flux'
             sname    = 'CISO_FG_as_13CO2'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DI13C_AS_GAS_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'DI13C Surface Sea-Air Gas Flux'
             sname    = 'CISO_FG_sa_13CO2'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DI13C_SA_GAS_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'D13C Surface GAS FLUX'
             sname    = 'CISO_FG_d13C'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_d13C_GAS_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'Atmospheric Delta 13C in permil'
             sname    = 'CISO_D13C_atm'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_D13C_atm, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = '13C/12C ratio in total DIC'
             sname    = 'CISO_R13C_DIC_surf'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_R13C_DIC_surf, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = '13C/12C ratio in atmosphere'
             sname    = 'CISO_R13C_atm'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_R13C_atm, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'Flux of DI13C from rivers'
             sname    = 'CISO_DI13C_RIV_FLUX'
             units    = 'nmol/cm^2/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DI13C_RIV_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'Flux of DO13C from rivers'
             sname    = 'CISO_DO13C_RIV_FLUX'
             units    = 'nmol/cm^2/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DO13C_RIV_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'Surface equilibrium fractionation (CO2_gaseous <-> CO2_aq)'
             sname    = 'CISO_eps_aq_g_surf'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_eps_aq_g_surf, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'Surface equilibrium fractionation between total DIC and gaseous CO2'
             sname    = 'CISO_eps_dic_g_surf'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_eps_dic_g_surf, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'DI14C Surface Gas Flux'
             sname    = 'CISO_FG_14CO2'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DI14C_GAS_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'DI14C Surface Air-Sea Gas Flux'
             sname    = 'CISO_FG_as_14CO2'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DI14C_AS_GAS_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'DI14C Surface Sea-Air Gas Flux'
             sname    = 'CISO_FG_sa_14CO2'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DI14C_SA_GAS_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'D14C Surface GAS FLUX'
             sname    = 'CISO_FG_d14C'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_d14C_GAS_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'Atmospheric Delta 14C in permil'
             sname    = 'CISO_D14C_atm'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_D14C_atm, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = '14C/12C ratio in total DIC'
             sname    = 'CISO_R14C_DIC_surf'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_R14C_DIC_surf, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = '14C/12C ratio in atmosphere'
             sname    = 'CISO_R14C_atm'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_R14C_atm, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
            end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'Flux of DI14C from rivers'
             sname    = 'CISO_DI14C_RIV_FLUX'
             units    = 'nmol/cm^2/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DI14C_RIV_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'Flux of DO14C from rivers'
             sname    = 'CISO_DO14C_RIV_FLUX'
             units    = 'nmol/cm^2/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DO14C_RIV_FLUX, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_forcing_diags = num_forcing_diags + 1
          else
             lname    = 'GLOBAL_D14C'
             sname    = 'CISO_GLOBAL_D14C'
             units    = 'permil'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_GLOBAL_D14C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if
       end if

       end associate

       !-----------------------------------------------------------------
       ! Interior diagnostics
       !-----------------------------------------------------------------
       
       associate(                                 &
            ind => marbl_interior_diag_ind,       &
            diags => marbl_interior_forcing_diags &
            )

       ! General 2D diags
       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Calcite Saturation Depth'
          sname = 'zsatcalc'
          units = 'cm'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%zsatcalc, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Aragonite Saturation Depth'
          sname = 'zsatarag'
          units = 'cm'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%zsatarag, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Minimum of O2'
          sname = 'O2_ZMIN'
          units = 'mmol/m^3'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%O2_ZMIN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Depth of Vertical Minimum of O2'
          sname = 'O2_ZMIN_DEPTH'
          units = 'cm'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%O2_ZMIN_DEPTH, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Total C Fixation Vertical Integral'
          sname = 'photoC_TOT_zint'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%photoC_TOT_zint, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Total C Fixation from NO3 Vertical Integral'
          sname = 'photoC_NO3_TOT_zint'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%photoC_NO3_TOT_zint, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ctot'
          sname = 'Jint_Ctot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_Ctot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ctot, 0-100m'
          sname = 'Jint_100m_Ctot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_100m_Ctot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ntot'
          sname = 'Jint_Ntot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_Ntot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ntot, 0-100m'
          sname = 'Jint_100m_Ntot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_100m_Ntot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ptot'
          sname = 'Jint_Ptot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_Ptot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ptot, 0-100m'
          sname = 'Jint_100m_Ptot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_100m_Ptot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Sitot'
          sname = 'Jint_Sitot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_Sitot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Sitot, 0-100m'
          sname = 'Jint_100m_Sitot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_100m_Sitot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Fetot'
          sname = 'Jint_Fetot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_Fetot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Fetot, 0-100m'
          sname = 'Jint_100m_Fetot'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Jint_100m_Fetot, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       ! Particulate 2D diags
       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'CaCO3 Flux to Sediments'
          sname = 'calcToSed'
          units = 'nmolC/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%calcToSed, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'POC Flux to Sediments'
          sname = 'pocToSed'
          units = 'nmolC/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%pocToSed, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'nitrogen burial Flux to Sediments'
          sname = 'ponToSed'
          units = 'nmolN/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ponToSed, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'nitrogen loss in Sediments'
          sname = 'SedDenitrif'
          units = 'nmolN/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%SedDenitrif, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'non-oxic,non-dentr remin in Sediments'
          sname = 'OtherRemin'
          units = 'nmolC/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%OtherRemin, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'phosphorus Flux to Sediments'
          sname = 'popToSed'
          units = 'nmolP/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%popToSed, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'biogenic Si Flux to Sediments'
          sname = 'bsiToSed'
          units = 'nmolSi/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%bsiToSed, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'dust Flux to Sediments'
          sname = 'dustToSed'
          units = 'g/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%dustToSed, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'pFe Flux to Sediments'
          sname = 'pfeToSed'
          units = 'nmolFe/cm^2/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%pfeToSed, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       ! Autotroph 2D diags
       do n=1,autotroph_cnt
          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' C Fixation Vertical Integral'
             sname = 'photoC_' // trim(autotrophs(n)%sname) // '_zint'
             units = 'mmol/m^3 cm/s'
             vgrid = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%photoC_zint(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' C Fixation from NO3 Vertical Integral'
             sname = 'photoC_NO3_' // trim(autotrophs(n)%sname) // '_zint'
             units = 'mmol/m^3 cm/s'
             vgrid = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%photoC_NO3_zint(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             if (marbl_tracer_indices%auto_inds(n)%CaCO3_ind.gt.0) then
                lname = trim(autotrophs(n)%lname) // ' CaCO3 Formation Vertical Integral'
                sname = trim(autotrophs(n)%sname) // '_CaCO3_form_zint'
                units = 'mmol/m^3 cm/s'
                vgrid = 'none'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CaCO3_form_zint(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             else
                ind%CaCO3_form_zint(n) = -1
             end if
          end if
       end do

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Total CaCO3 Formation Vertical Integral'
          sname = 'CaCO3_form_zint'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%tot_CaCO3_form_zint, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       ! General 3D diags
       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Carbonate Ion Concentration'
          sname = 'CO3'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%CO3, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Bicarbonate Ion Concentration'
          sname = 'HCO3'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%HCO3, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Carbonic Acid Concentration'
          sname = 'H2CO3'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%H2CO3, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'pH'
          sname = 'pH_3D'
          units = 'none'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ph_3D, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Carbonate Ion Concentration, Alternative CO2'
          sname = 'CO3_ALT_CO2'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%CO3_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Bicarbonate Ion Concentration, Alternative CO2'
          sname = 'HCO3_ALT_CO2'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%HCO3_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Carbonic Acid Concentration, Alternative CO2'
          sname = 'H2CO3_ALT_CO2'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%H2CO3_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'pH, Alternative CO2'
          sname = 'pH_3D_ALT_CO2'
          units = 'none'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%ph_3D_ALT_CO2, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'CO3 concentration at calcite saturation'
          sname = 'co3_sat_calc'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%co3_sat_calc, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'CO3 concentration at aragonite saturation'
          sname = 'co3_sat_arag'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%co3_sat_arag, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Nitrification'
          sname = 'NITRIF'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%NITRIF, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Denitrification'
          sname = 'DENITRIF'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DENITRIF, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'O2 Production'
          sname = 'O2_PRODUCTION'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%O2_PRODUCTION, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'O2 Consumption'
          sname = 'O2_CONSUMPTION'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%O2_CONSUMPTION, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Apparent O2 Utilization'
          sname = 'AOU'
          units = 'mmol/m^3'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%AOU, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'PAR Average over Model Cell'
          sname = 'PAR_avg'
          units = 'W/m^2'
          vgrid = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%PAR_avg, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Total Autotroph Grazing'
          sname = 'graze_auto_TOT'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%auto_graze_TOT, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Total C Fixation'
          sname = 'photoC_TOT'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%photoC_TOT, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Total C Fixation from NO3'
          sname = 'photoC_NO3_TOT'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%photoC_NO3_TOT, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DOC Production'
          sname = 'DOC_prod'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOC_prod, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DOC Remineralization'
          sname = 'DOC_remin'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOC_remin, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DOCr Remineralization'
          sname = 'DOCr_remin'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOCr_remin, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DON Production'
          sname = 'DON_prod'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DON_prod, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DON Remineralization'
          sname = 'DON_remin'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DON_remin, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DONr Remineralization'
          sname = 'DONr_remin'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DONr_remin, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DOP Production'
          sname = 'DOP_prod'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOP_prod, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DOP Remineralization'
          sname = 'DOP_remin'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOP_remin, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'DOPr Remineralization'
          sname = 'DOPr_remin'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%DOPr_remin, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Iron Scavenging'
          sname = 'Fe_scavenge'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Fe_scavenge, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Iron Scavenging Rate'
          sname = 'Fe_scavenge_rate'
          units = '1/y'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%Fe_scavenge_rate, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       ! Particulate 3D diags
       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'POC Flux into Cell'
          sname = 'POC_FLUX_IN'
          units = 'mmol/m^3 cm/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%POC_FLUX_IN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'POC Production'
          sname = 'POC_PROD'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%POC_PROD, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'POC Remineralization'
          sname = 'POC_REMIN'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%POC_REMIN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'POC Remineralization routed to DIC'
          sname = 'POC_REMIN_DIC'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%POC_REMIN_DIC, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'PON Remineralization routed to NH4'
          sname = 'PON_REMIN_NH4'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%PON_REMIN_NH4, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'POP Remineralization routed to PO4'
          sname = 'POP_REMIN_PO4'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%POP_REMIN_PO4, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'CaCO3 Flux into Cell'
          sname = 'CaCO3_FLUX_IN'
          units = 'mmol/m^3 cm/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%CaCO3_FLUX_IN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'CaCO3 Production'
          sname = 'CaCO3_PROD'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%CaCO3_PROD, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'CaCO3 Remineralization'
          sname = 'CaCO3_REMIN'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%CaCO3_REMIN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'SiO2 Flux into Cell'
          sname = 'SiO2_FLUX_IN'
          units = 'mmol/m^3 cm/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%SiO2_FLUX_IN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'SiO2 Production'
          sname = 'SiO2_PROD'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%SiO2_PROD, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'SiO2 Remineralization'
          sname = 'SiO2_REMIN'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%SiO2_REMIN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Dust Flux into Cell'
          sname = 'dust_FLUX_IN'
          units = 'ng/s/m^2'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%dust_FLUX_IN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'Dust Remineralization'
          sname = 'dust_REMIN'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%dust_REMIN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'P_iron Flux into Cell'
          sname = 'P_iron_FLUX_IN'
          units = 'mmol/m^3 cm/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%P_iron_FLUX_IN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'P_iron Production'
          sname = 'P_iron_PROD'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%P_iron_PROD, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname = 'P_iron Remineralization'
          sname = 'P_iron_REMIN'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%P_iron_REMIN, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       ! Autotroph 3D diags
       do n= 1,autotroph_cnt

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' N Limitation'
             sname = trim(autotrophs(n)%sname) // '_N_lim'
             units = 'none'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%N_lim(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if
          
          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' P Limitation'
             sname = trim(autotrophs(n)%sname) // '_P_lim'
             units = 'none'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%P_lim(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Fe Limitation'
             sname = trim(autotrophs(n)%sname) // '_Fe_lim'
             units = 'none'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%Fe_lim(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             if (autotrophs(n)%kSiO3 > c0) then
                lname = trim(autotrophs(n)%lname) // ' SiO3 Limitation'
                sname = trim(autotrophs(n)%sname) // '_SiO3_lim'
                units = 'none'
                vgrid = 'layer_avg'
                truncate = .true.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%SiO3_lim(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             else
                ind%SiO3_lim(n) = -1
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Light Limitation'
             sname = trim(autotrophs(n)%sname) // '_light_lim'
             units = 'none'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%light_lim(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' C Fixation'
             sname = 'photoC_' // trim(autotrophs(n)%sname)
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%photoC(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' C Fixation from NO3'
             sname = 'photoC_NO3_' // trim(autotrophs(n)%sname)
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%photoC_NO3(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Fe Uptake'
             sname = 'photoFe_' // trim(autotrophs(n)%sname)
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%photoFe(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' NO3 Uptake'
             sname = 'photoNO3_' // trim(autotrophs(n)%sname)
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%photoNO3(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' NH4 Uptake'
             sname = 'photoNH4_' // trim(autotrophs(n)%sname)
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%photoNH4(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' DOP Uptake'
             sname = 'DOP_' // trim(autotrophs(n)%sname) // '_uptake'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%DOP_uptake(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' PO4 Uptake'
             sname = 'PO4_' // trim(autotrophs(n)%sname) // '_uptake'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%PO4_uptake(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Grazing'
             sname = 'graze_' // trim(autotrophs(n)%sname)
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%auto_graze(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Grazing to POC'
             sname = 'graze_' // trim(autotrophs(n)%sname) // '_poc'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%auto_graze_poc(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Grazing to DOC'
             sname = 'graze_' // trim(autotrophs(n)%sname) // '_doc'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%auto_graze_doc(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Grazing to ZOO'
             sname = 'graze_' // trim(autotrophs(n)%sname) // '_zoo'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%auto_graze_zoo(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Loss'
             sname = trim(autotrophs(n)%sname) // '_loss'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%auto_loss(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Loss to POC'
             sname = trim(autotrophs(n)%sname) // '_loss_poc'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%auto_loss_poc(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Loss to DOC'
             sname = trim(autotrophs(n)%sname) // '_loss_doc'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%auto_loss_doc(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname = trim(autotrophs(n)%lname) // ' Aggregate'
             sname = trim(autotrophs(n)%sname) // '_agg'
             units = 'mmol/m^3/s'
             vgrid = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%auto_agg(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             if (marbl_tracer_indices%auto_inds(n)%Si_ind.gt.0) then
                lname = trim(autotrophs(n)%lname) // ' Si Uptake'
                ! FIXME #22 - eventually add _
                sname = trim(autotrophs(n)%sname) // 'bSi_form'
                !sname = trim(autotrophs(n)%sname) // '_bSi_form'
                units = 'mmol/m^3/s'
                vgrid = 'layer_avg'
                truncate = .true.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%bSi_form(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             else
                ind%bSi_form(n) = -1
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             if (marbl_tracer_indices%auto_inds(n)%CaCO3_ind.gt.0) then
                lname = trim(autotrophs(n)%lname) // ' CaCO3 Formation'
                sname = trim(autotrophs(n)%sname) // '_CaCO3_form'
                units = 'mmol/m^3/s'
                vgrid = 'layer_avg'
                truncate = .true.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CaCO3_form(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             else
                ind%CaCO3_form(n) = -1
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             if (autotrophs(n)%Nfixer) then
                lname = trim(autotrophs(n)%lname) // ' N Fixation'
                sname = trim(autotrophs(n)%sname) // '_Nfix'
                units = 'mmol/m^3/s'
                vgrid = 'layer_avg'
                truncate = .true.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%Nfix(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             else
                ind%Nfix(n) = -1
             end if
          end if

       end do ! end do-loop for atutroph_cnt

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname    = 'Total Si Uptake'
          sname    = 'bSi_form'
          units    = 'mmol/m^3/s'
          vgrid    = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%tot_bSi_form, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname    = 'Total CaCO3 Formation'
          sname    = 'CaCO3_form'
          units    = 'mmol/m^3/s'
          vgrid    = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%tot_CaCO3_form, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       if (count_only) then
          num_interior_diags = num_interior_diags + 1
       else
          lname    = 'Total N Fixation'
          sname    = 'Nfix'
          units    = 'mmol/m^3/s'
          vgrid    = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
               ind%tot_Nfix, marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end if

       ! Zooplankton 3D diags
       do n = 1,zooplankton_cnt

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = trim(zooplankton(n)%lname) // ' Loss'
             sname    = trim(zooplankton(n)%sname) // '_loss'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%zoo_loss(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = trim(zooplankton(n)%lname) // ' Loss to POC'
             sname    = trim(zooplankton(n)%sname) // '_loss_poc'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%zoo_loss_poc(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = trim(zooplankton(n)%lname) // ' Loss to DOC'
             sname    = trim(zooplankton(n)%sname) // '_loss_doc'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%zoo_loss_doc(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = trim(zooplankton(n)%lname) // ' grazing loss'
             sname    = 'graze_' // trim(zooplankton(n)%sname)
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%zoo_graze(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = trim(zooplankton(n)%lname) // ' grazing loss to POC'
             sname    = 'graze_' // trim(zooplankton(n)%sname) // '_poc'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%zoo_graze_poc(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = trim(zooplankton(n)%lname) // ' grazing loss to DOC'
             sname    = 'graze_' // trim(zooplankton(n)%sname) // '_doc'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%zoo_graze_doc(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = trim(zooplankton(n)%lname) // ' grazing loss to ZOO'
             sname    = 'graze_' // trim(zooplankton(n)%sname) // '_zoo'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%zoo_graze_zoo(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = trim(zooplankton(n)%lname) // ' grazing gain'
             sname    = 'x_graze_' // trim(zooplankton(n)%sname)
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%x_graze_zoo(n), marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

       end do

       if (ciso_on) then

          !  nonstandard 3D fields

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'PO13C Flux into Cell'
             sname    = 'CISO_PO13C_FLUX_IN'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_PO13C_FLUX_IN, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'PO13C Production'
             sname    = 'CISO_PO13C_PROD'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_PO13C_PROD, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'PO13C Remineralization'
             sname    = 'CISO_PO13C_REMIN'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_PO13C_REMIN, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'DO13C Production'
             sname    = 'CISO_DO13C_prod'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DO13C_prod, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'DO13C Remineralization'
             sname    = 'CISO_DO13C_remin'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DO13C_remin, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Ca13CO3 flux into cell'
             sname    = 'CISO_Ca13CO3_FLUX_IN'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Ca13CO3_FLUX_IN, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Ca13CO3 Production'
             sname    = 'CISO_Ca13CO3_PROD'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Ca13CO3_PROD, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Ca13CO3 Remineralization'
             sname    = 'CISO_Ca13CO3_REMIN'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Ca13CO3_REMIN, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Total 13C Fixation'
             sname    = 'CISO_photo13C_TOT'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_photo13C_TOT, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'd13C of DIC'
             sname    = 'CISO_DIC_d13C'
             units    = 'permil'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DIC_d13C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'd13C of DOC'
             sname    = 'CISO_DOC_d13C'
             units    = 'permil'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DOC_d13C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'd13C of zooC'
             sname    = 'CISO_zooC_d13C'
             units    = 'permil'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_zooC_d13C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'PO14C Flux into Cell'
             sname    = 'CISO_PO14C_FLUX_IN'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_PO14C_FLUX_IN, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'PO14C Production'
             sname    = 'CISO_PO14C_PROD'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_PO14C_PROD, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'PO14C Remineralization'
             sname    = 'CISO_PO14C_REMIN'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_PO14C_REMIN, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'DO14C Production'
             sname    = 'CISO_DO14C_prod'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DO14C_prod, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'DO14C Remineralization'
             sname    = 'CISO_DO14C_remin'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DO14C_remin, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Ca14CO3 flux into cell'
             sname    = 'CISO_Ca14CO3_FLUX_IN'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Ca14CO3_FLUX_IN, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Ca14CO3 Production'
             sname    = 'CISO_Ca14CO3_PROD'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Ca14CO3_PROD, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Ca14CO3 Remineralization'
             sname    = 'CISO_Ca14CO3_REMIN'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Ca14CO3_REMIN, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Total 14C Fixation'
             sname    = 'CISO_photo14C_TOT'
             units    = 'mmol/m^3/s'
             vgrid    = 'layer_avg'
             truncate = .true.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_photo14C_TOT, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'd14C of DIC'
             sname    = 'CISO_DIC_d14C'
             units    = 'permil'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DIC_d14C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'd14C of DOC'
             sname    = 'CISO_DOC_d14C'
             units    = 'permil'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_DOC_d14C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'd14C of zooC'
             sname    = 'CISO_zooC_d14C'
             units    = 'permil'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_zooC_d14C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          !  Nonstandard 2D fields

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Total 13C Fixation Vertical Integral'
             sname    = 'CISO_photo13C_TOT_zint'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_photo13C_TOT_zint, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Total 14C Fixation Vertical Integral'
             sname    = 'CISO_photo14C_TOT_zint'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_photo14C_TOT_zint, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = '13Ctot Source Sink Term Vertical Integral'
             sname    = 'CISO_Jint_13Ctot'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Jint_13Ctot, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = '14Ctot Source Sink Term Vertical Integral'
             sname    = 'CISO_Jint_14Ctot'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Jint_14Ctot, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = '13Ctot Source Sink Term Vertical Integral, 0-100m'
             sname    = 'CISO_Jint_100m_13Ctot'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Jint_100m_13Ctot, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = '14Ctot Source Sink Term Vertical Integral, 0-100m'
             sname    = 'CISO_Jint_100m_14Ctot'
             units    = 'mmol/m^3 cm/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_Jint_100m_14Ctot, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          !  Nonstandard autotroph 2D and 3D fields for each autotroph

          do n = 1, autotroph_cnt

             !FIXME - the comments seem to be needed below - need to fix this
             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' Ca13CO3 Formation'
                sname    = 'CISO_' // trim(autotrophs(n)%sname) // '_Ca13CO3_form'
                units    = 'mmol/m^3/s'
                vgrid    = 'layer_avg'
                truncate = .true.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_Ca13CO3_form(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' Ca13CO3 Formation Vertical Integral'
                sname    = trim(sname) // '_zint'
                units    = 'mmol/m^3 cm/s' 
                vgrid    = 'none'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_Ca13CO3_form_zint(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' Ca14CO3 Formation'
                sname    = 'CISO_' // trim(autotrophs(n)%sname) // '_Ca14CO3_form'
                units    = 'mmol/m^3/s'
                vgrid    = 'layer_avg'
                truncate = .true.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_Ca14CO3_form(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' Ca14CO3 Formation Vertical Integral'
                sname    = trim(sname) // '_zint'
                units    = 'mmol/m^3 cm/s' 
                vgrid    = 'none'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_Ca14CO3_form_zint(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' d13C of CaCO3'
                sname    = 'CISO_autotrophCaCO3_d13C_' // trim(autotrophs(n)%sname)
                units    = 'mmol/m^3/s'
                vgrid    = 'layer_avg'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_autotrophCaCO3_d13C(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' d14C of CaCO3'
                sname    = 'CISO_autotrophCaCO3_d14C_' // trim(autotrophs(n)%sname)
                units    = 'mmol/m^3/s'
                vgrid    = 'layer_avg'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_autotrophCaCO3_d14C(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' 13C Fixation'
                sname    = 'CISO_photo13C_' // trim(autotrophs(n)%sname)
                units    = 'mmol/m^3/s'
                vgrid    = 'layer_avg'
                truncate = .true.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_photo13C(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' 14C Fixation'
                sname    = 'CISO_photo14C_' // trim(autotrophs(n)%sname)
                units    = 'mmol/m^3/s'
                vgrid    = 'layer_avg'
                truncate = .true.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_photo14C(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' 13C Fixation Vertical Integral'
                sname    = 'CISO_photo13C_' // trim(autotrophs(n)%sname) // '_zint'
                units    = 'mmol/m^3 cm/s'
                vgrid    = 'none'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_photo13C_zint(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' 14C Fixation Vertical Integral'
                sname    = 'CISO_photo14C_' // trim(autotrophs(n)%sname) // '_zint'
                units    = 'mmol/m^3 cm/s'
                vgrid    = 'none'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_photo14C_zint(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' discrimination factor (eps)'
                sname    = 'CISO_eps_autotroph_' // trim(autotrophs(n)%sname)
                units    = 'permil'
                vgrid    = 'layer_avg'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_eps_autotroph(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' d13C'
                sname    = 'CISO_d13C_' // trim(autotrophs(n)%sname)
                units    = 'permil'
                vgrid    = 'layer_avg'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_d13C(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' d14C'
                sname    = 'CISO_d14C_' // trim(autotrophs(n)%sname)
                units    = 'permil'
                vgrid    = 'layer_avg'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_d14C(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

             if (count_only) then
                num_interior_diags = num_interior_diags + 1
             else
                lname    = trim(autotrophs(n)%lname) // ' instanteous growth rate over [CO2*]'
                sname    = 'CISO_mui_to_co2star_' // trim(autotrophs(n)%sname)
                units    = 'm^3/mmol C/s'
                vgrid    = 'layer_avg'
                truncate = .false.
                call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
                     ind%CISO_mui_to_co2star(n), marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_diagnostics_error(marbl_status_log, sname, subname)
                  return
                end if
             end if

          end do

          !  More nonstandard 3D fields

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Equilibrium fractionation (CO2_gaseous <-> CO2_aq)'
             sname    = 'CISO_eps_aq_g'
             units    = 'permil'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_eps_aq_g, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Equilibrium fractionation between total DIC and gaseous CO2'
             sname    = 'CISO_eps_dic_g'
             units    = 'permil'
             vgrid    = 'layer_avg'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%CISO_eps_dic_g, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          !  Vars to sum up burial in sediments (2D)

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Ca13CO3 Flux to Sediments'
             sname    = 'calcToSed_13C'
             units    = 'nmolC/cm^2/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%calcToSed_13C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'PO13C Flux to Sediments'
             sname    = 'pocToSed_13C'
             units    = 'nmolC/cm^2/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%pocToSed_13C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'Ca14CO3 Flux to Sediments'
             sname    = 'calcToSed_14C'
             units    = 'nmolC/cm^2/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%calcToSed_14C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

          if (count_only) then
             num_interior_diags = num_interior_diags + 1
          else
             lname    = 'PO14C Flux to Sediments'
             sname    = 'pocToSed_14C'
             units    = 'nmolC/cm^2/s'
             vgrid    = 'none'
             truncate = .false.
             call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
                  ind%pocToSed_14C, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if

       end if  ! end of if ciso_on

       end associate

       !-----------------------------------------------------------------
       ! Restoring diagnostics
       !-----------------------------------------------------------------
       
       associate(                        &
            diags => marbl_interior_restore_diags &
            )
       
       do n = 1,marbl_total_tracer_cnt
          ! Note that tmp_id is a temp variable because restoring diagnostics
          ! have same indexing as the ecosys tracers
          if (count_only) then
             num_restore_diags = num_restore_diags + 1
          else
             lname = trim(marbl_tracer_metadata(n)%long_name) // " Restoring"
             sname = trim(marbl_tracer_metadata(n)%short_name) // "_RESTORE"
             units = 'mmol/m^3'
             vgrid = 'layer_avg'
             call diags%add_diagnostic(lname, sname, units, vgrid, .false.,   &
                  tmp_id, marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_diagnostics_error(marbl_status_log, sname, subname)
               return
             end if
          end if
       end do
       
       end associate

    end do  ! end of imode loop

    !-----------------------------------------------------------------
    ! Initialize all diagnostics to zero
    !-----------------------------------------------------------------

    call marbl_interior_forcing_diags%set_to_zero(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace(&
           'marbl_interior_forcing_diags%set_to_zero', subname)
      return
    end if
    call marbl_interior_restore_diags%set_to_zero(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace(&
           'marbl_interior_restore_diags%set_to_zero', subname)
      return
    end if
    call marbl_surface_forcing_diags%set_to_zero(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace(&
           'marbl_surface_forcing_diags%set_to_zero', subname)
      return
    end if

  end subroutine marbl_diagnostics_init

  !***********************************************************************

  subroutine marbl_diagnostics_set_interior_forcing ( &
       domain,                                        &
       interior_forcing_input,                        &
       dtracers,                                      &
       marbl_tracer_indices,                          &
       carbonate,                                     &
       autotroph_secondary_species,                   &         
       zooplankton_secondary_species,                 &
       dissolved_organic_matter,                      &
       marbl_particulate_share,                       &
       marbl_PAR,                                     &
       PON_remin, PON_sed_loss,                       &
       POP_remin,  POP_sed_loss,                      &
       sed_denitrif, other_remin, nitrif, denitrif,   &
       column_o2, o2_production, o2_consumption,      &
       fe_scavenge, fe_scavenge_rate,                 &
       marbl_interior_forcing_diags,                  &
       marbl_status_log)
       
    implicit none

    type (marbl_domain_type)                  , intent(in) :: domain                                
    type (marbl_interior_forcing_input_type)  , intent(in) :: interior_forcing_input
    real (r8)                                 , intent(in) :: dtracers(:,:) ! (marbl_total_tracer_cnt, km) computed source/sink terms
    type(marbl_tracer_index_type)             , intent(in) :: marbl_tracer_indices
    type (carbonate_type)                     , intent(in) :: carbonate(domain%km)
    type (autotroph_secondary_species_type)   , intent(in) :: autotroph_secondary_species(autotroph_cnt, domain%km)
    type (zooplankton_secondary_species_type) , intent(in) :: zooplankton_secondary_species(zooplankton_cnt, domain%km)
    type (dissolved_organic_matter_type)      , intent(in) :: dissolved_organic_matter(domain%km)
    type (marbl_particulate_share_type)       , intent(in) :: marbl_particulate_share
    type (marbl_PAR_type)                     , intent(in) :: marbl_PAR
    real (r8)                                 , intent(in) :: PON_remin(domain%km)        ! remin of PON
    real (r8)                                 , intent(in) :: PON_sed_loss(domain%km)     ! loss of PON to sediments
    real (r8)                                 , intent(in) :: POP_remin(domain%km)        ! remin of POP
    real (r8)                                 , intent(in) :: POP_sed_loss(domain%km)     ! loss of POP to sediments
    real (r8)                                 , intent(in) :: sed_denitrif(domain%km)     ! sedimentary denitrification (nmol N/cm^3/sec)
    real (r8)                                 , intent(in) :: other_remin(domain%km)      ! organic C remin not due oxic or denitrif (nmolC/cm^3/sec)
    real (r8)                                 , intent(in) :: nitrif(domain%km)           ! nitrification (NH4 -> NO3) (mmol N/m^3/sec)
    real (r8)                                 , intent(in) :: denitrif(domain%km)         ! WC nitrification (NO3 -> N2) (mmol N/m^3/sec)
    real (r8)                                 , intent(in) :: column_o2(:)
    real (r8)                                 , intent(in) :: o2_production(:)
    real (r8)                                 , intent(in) :: o2_consumption(:)
    real (r8)                                 , intent(in) :: fe_scavenge_rate(domain%km) ! annual scavenging rate of iron as % of ambient
    real (r8)                                 , intent(in) :: fe_scavenge(domain%km)      ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
    type (marbl_diagnostics_type)             , intent(inout) :: marbl_interior_forcing_diags
    type (marbl_log_type)                     , intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_diagnostics_mod:marbl_diagnostics_set_interior_forcing'

    !-----------------------------------------------------------------

    associate(                                                            &
         POC             => marbl_particulate_share%POC,                  &
         P_CaCO3         => marbl_particulate_share%P_CaCO3,              &
         P_SiO2          => marbl_particulate_share%P_SiO2,               &
         dust            => marbl_particulate_share%dust,                 &
         P_iron          => marbl_particulate_share%P_iron,               &
         PAR             => marbl_PAR                                     &
         )

    call marbl_interior_forcing_diags%set_to_zero(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace(&
           'marbl_interior_forcing_diags%set_to_zero', subname)
      return
    end if

    call store_diagnostics_carbonate(domain, &
         carbonate, marbl_interior_forcing_diags)

    call store_diagnostics_autotrophs(domain, &
         autotroph_secondary_species, marbl_interior_forcing_diags)

    call store_diagnostics_autotroph_sums(domain, &
         autotroph_secondary_species, marbl_interior_forcing_diags)

    call store_diagnostics_zooplankton(&
         zooplankton_secondary_species, marbl_interior_forcing_diags)

    call store_diagnostics_particulates(domain, &
         marbl_particulate_share, &
         PON_remin, PON_sed_loss, POP_remin, POP_sed_loss, &
         sed_denitrif, other_remin, marbl_interior_forcing_diags)

    call store_diagnostics_carbon_fluxes(domain, POC, P_CaCO3, dtracers,      &
         marbl_tracer_indices, marbl_interior_forcing_diags)

    call store_diagnostics_nitrification(&
         nitrif, denitrif, marbl_interior_forcing_diags)

    call store_diagnostics_oxygen(domain, &
         interior_forcing_input, &
         column_o2, o2_production, o2_consumption, marbl_interior_forcing_diags)

    call store_diagnostics_PAR(domain, &
         PAR%col_frac(:), PAR%avg(:,:), marbl_interior_forcing_diags)

    call store_diagnostics_dissolved_organic_matter(domain, &
         dissolved_organic_matter, fe_scavenge, fe_scavenge_rate, marbl_interior_forcing_diags)

    call store_diagnostics_nitrogen_fluxes(domain, &
         PON_sed_loss, denitrif, sed_denitrif, autotroph_secondary_species, dtracers, &
         marbl_tracer_indices, marbl_interior_forcing_diags)

    call store_diagnostics_phosphorus_fluxes(domain, POP_sed_loss, dtracers,  &
         marbl_tracer_indices, marbl_interior_forcing_diags)

    call store_diagnostics_silicon_fluxes(domain, P_SiO2, dtracers,           &
         marbl_tracer_indices, marbl_interior_forcing_diags)

    call store_diagnostics_iron_fluxes(domain, P_iron, dust,                  &
         interior_forcing_input%fesedflux, dtracers, marbl_tracer_indices,    &
         marbl_interior_forcing_diags)
         

    end associate

  end subroutine marbl_diagnostics_set_interior_forcing

  !***********************************************************************

  subroutine marbl_diagnostics_set_surface_forcing( &
       surface_forcing_ind,                         &
       surface_input_forcings,                      &
       surface_forcing_internal,                    &
       surface_tracer_fluxes,                       &
       marbl_tracer_indices,                        &
       saved_state,                                 &
       surface_forcing_output,                      &
       surface_forcing_diags)

    ! !DESCRIPTION:
    !  Compute surface fluxes for ecosys tracer module.

    use marbl_share_mod       , only : lflux_gas_o2
    use marbl_share_mod       , only : lflux_gas_co2
    use marbl_share_mod       , only : iron_flux_file     
    use marbl_share_mod       , only : din_riv_flux_file     
    use marbl_share_mod       , only : dip_riv_flux_file          
    use marbl_share_mod       , only : don_riv_flux_file          
    use marbl_share_mod       , only : dop_riv_flux_file          
    use marbl_share_mod       , only : dsi_riv_flux_file          
    use marbl_share_mod       , only : dfe_riv_flux_file          
    use marbl_share_mod       , only : dic_riv_flux_file          
    use marbl_share_mod       , only : alk_riv_flux_file          
    use marbl_parms           , only : mpercm

    implicit none

    type(marbl_surface_forcing_indexing_type) , intent(in)    :: surface_forcing_ind
    real(r8)                                  , intent(in)    :: surface_input_forcings(:,:)
    real(r8)                                  , intent(in)    :: surface_tracer_fluxes(:,:)
    type(marbl_tracer_index_type)             , intent(in)    :: marbl_tracer_indices
    type(marbl_saved_state_type)              , intent(in)    :: saved_state 
    type(marbl_surface_forcing_internal_type) , intent(in)    :: surface_forcing_internal
    type(marbl_surface_forcing_output_type)   , intent(in)    :: surface_forcing_output
    type(marbl_diagnostics_type)              , intent(inout) :: surface_forcing_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_diagnostics_mod:marbl_diagnostics_set_surface_forcing'
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  calculate gas flux quantities if necessary
    !-----------------------------------------------------------------------

    associate(                                                                                  &
         ind_diag          => marbl_surface_forcing_diag_ind,                                   &
         ind_forc          => surface_forcing_ind,                                              &

         diags             => surface_forcing_diags%diags(:),                                   &
         xkw               => surface_input_forcings(:,surface_forcing_ind%xkw_id),             &
         xco2              => surface_input_forcings(:,surface_forcing_ind%xco2_id),            &
         xco2_alt_co2      => surface_input_forcings(:,surface_forcing_ind%xco2_alt_co2_id),    &
         ap_used           => surface_input_forcings(:,surface_forcing_ind%atm_pressure_id),    &
         ifrac             => surface_input_forcings(:,surface_forcing_ind%ifrac_id),           &
         dust_flux_in      => surface_input_forcings(:,surface_forcing_ind%dust_flux_id),       &

         flux_alt_co2      => surface_forcing_internal%flux_alt_co2,                            &
         co2star           => surface_forcing_internal%co2star,                                 &
         dco2star          => surface_forcing_internal%dco2star,                                &
         pco2surf          => surface_forcing_internal%pco2surf,                                &
         dpco2             => surface_forcing_internal%dpco2,                                   &
         co2star_alt       => surface_forcing_internal%co2star_alt,                             &
         dco2star_alt      => surface_forcing_internal%dco2star_alt,                            &
         pco2surf_alt      => surface_forcing_internal%pco2surf_alt,                            &
         dpco2_alt         => surface_forcing_internal%dpco2_alt,                               &
         pv_co2            => surface_forcing_internal%pv_co2,                                  &
         pv_o2             => surface_forcing_internal%pv_o2,                                   &
         schmidt_co2       => surface_forcing_internal%schmidt_co2,                             &
         schmidt_o2        => surface_forcing_internal%schmidt_o2,                              &
         o2sat             => surface_forcing_internal%o2sat,                                   &
         iron_flux_in      => surface_forcing_internal%iron_flux,                               &


         ph_prev           => saved_state%ph_prev_surf,                                         &
         ph_prev_alt_co2   => saved_state%ph_prev_alt_co2_surf,                                 &

         stf               => surface_tracer_fluxes(:,:),                                       &

         po4_ind           => marbl_tracer_indices%po4_ind,                                     &
         no3_ind           => marbl_tracer_indices%no3_ind,                                     &
         sio3_ind          => marbl_tracer_indices%sio3_ind,                                     &
         nh4_ind           => marbl_tracer_indices%nh4_ind,                                     &
         fe_ind            => marbl_tracer_indices%fe_ind,                                      &
         o2_ind            => marbl_tracer_indices%o2_ind,                                      &
         dic_ind           => marbl_tracer_indices%dic_ind,                                     &
         dic_alt_co2_ind   => marbl_tracer_indices%dic_alt_co2_ind,                             &
         alk_ind           => marbl_tracer_indices%alk_ind,                                     &
         doc_ind           => marbl_tracer_indices%doc_ind,                                     &
         don_ind           => marbl_tracer_indices%don_ind,                                     &
         dop_ind           => marbl_tracer_indices%dop_ind,                                     &
         dopr_ind          => marbl_tracer_indices%dopr_ind,                                    &
         donr_ind          => marbl_tracer_indices%donr_ind,                                    &
         docr_ind          => marbl_tracer_indices%docr_ind                                     &
         )

    if (lflux_gas_o2 .or. lflux_gas_co2) then

       diags(ind_diag%ECOSYS_IFRAC)%field_2d(:)     = ifrac(:)
       diags(ind_diag%ECOSYS_XKW)%field_2d(:)       = xkw(:)
       diags(ind_diag%ECOSYS_ATM_PRESS)%field_2d(:) = ap_used(:)

    endif  ! lflux_gas_o2 .or. lflux_gas_co2

    if (lflux_gas_o2) then

       diags(ind_diag%PV_O2)%field_2d(:)      = pv_o2(:)
       diags(ind_diag%SCHMIDT_O2)%field_2d(:) = schmidt_o2(:)
       diags(ind_diag%O2SAT)%field_2d(:)      = o2sat(:)
       
    endif  ! lflux_gas_o2

    !-----------------------------------------------------------------------
    !  compute CO2 flux, computing disequilibrium one row at a time
    !-----------------------------------------------------------------------
    
    if (lflux_gas_co2) then
       
       diags(ind_diag%CO2STAR)%field_2d(:)              = co2star(:)
       diags(ind_diag%DCO2STAR)%field_2d(:)             = dco2star(:)
       diags(ind_diag%pCO2SURF)%field_2d(:)             = pco2surf(:)
       diags(ind_diag%DpCO2)%field_2d(:)                = dpco2(:)
       
       diags(ind_diag%CO2STAR_ALT_CO2)%field_2d(:)      = co2star_alt(:)
       diags(ind_diag%DCO2STAR_ALT_CO2)%field_2d(:)     = dco2star_alt(:)
       diags(ind_diag%pCO2SURF_ALT_CO2)%field_2d(:)     = pco2surf_alt(:)
       diags(ind_diag%DpCO2_ALT_CO2)%field_2d(:)        = dpco2_alt(:)
       
       diags(ind_diag%PV_CO2)%field_2d(:)               = pv_co2(:)
       diags(ind_diag%SCHMIDT_CO2)%field_2d(:)          = schmidt_co2(:)
       if (sfo_ind%flux_co2_id.ne.0) then
         diags(ind_diag%DIC_GAS_FLUX)%field_2d(:)       =                     &
                surface_forcing_output%sfo(sfo_ind%flux_co2_id)%forcing_field
       end if
       diags(ind_diag%PH)%field_2d(:)                   = ph_prev(:)
       diags(ind_diag%ATM_CO2)%field_2d(:)              = xco2(:)
      
       diags(ind_diag%DIC_GAS_FLUX_ALT_CO2)%field_2d(:) = flux_alt_co2(:)
       diags(ind_diag%PH_ALT_CO2)%field_2d(:)           = ph_prev_alt_co2(:)
       diags(ind_diag%ATM_ALT_CO2)%field_2d(:)          = xco2_alt_co2(:)
       
    endif  !  lflux_gas_co2

    !-----------------------------------------------------------------------
    !  calculate iron and dust fluxes if necessary
    !-----------------------------------------------------------------------

    ! multiply IRON flux by mpercm (.01) to convert from model units (cm/s)(mmol/m^3) to mmol/s/m^2

    ! FIXME #56 : need better conditional here, perhaps based on iron_flux_id /= 0
!   if (iron_flux_file%has_data) then
       diags(ind_diag%IRON_FLUX)%field_2d(:) = iron_flux_in(:) * mpercm
!   endif

    !-----------------------------------------------------------------------
    !  calculate nox and nhy fluxes if necessary
    !-----------------------------------------------------------------------

    if (ind_forc%nox_flux_id.ne.0) then
       diags(ind_diag%NOx_FLUX)%field_2d(:) = surface_input_forcings(:, ind_forc%nox_flux_id)
    endif
    if (ind_forc%nox_flux_id.ne.0) then
       diags(ind_diag%NHy_FLUX)%field_2d(:) = surface_input_forcings(:, ind_forc%nhy_flux_id)
    endif

    !-----------------------------------------------------------------------
    !  calculate river bgc fluxes if necessary
    !-----------------------------------------------------------------------

    if (din_riv_flux_file%has_data) then
       diags(ind_diag%DIN_RIV_FLUX)%field_2d(:) = surface_input_forcings(:, ind_forc%din_riv_flux_id)
    endif
    if (dsi_riv_flux_file%has_data) then
       diags(ind_diag%DSI_RIV_FLUX)%field_2d(:) = surface_input_forcings(:, ind_forc%dsi_riv_flux_id)
    endif
    if (dfe_riv_flux_file%has_data) then
       diags(ind_diag%DFE_RIV_FLUX)%field_2d(:) = surface_input_forcings(:, ind_forc%dfe_riv_flux_id)
    endif
    if (dic_riv_flux_file%has_data) then
       diags(ind_diag%DIC_RIV_FLUX)%field_2d(:) = surface_input_forcings(:, ind_forc%dic_riv_flux_id)
    endif
    if (alk_riv_flux_file%has_data) then
       diags(ind_diag%ALK_RIV_FLUX)%field_2d(:) = surface_input_forcings(:, ind_forc%alk_riv_flux_id)
    endif
    diags(ind_diag%O2_GAS_FLUX)%field_2d(:)   = stf(:, o2_ind)
    diags(ind_diag%DIP_RIV_FLUX)%field_2d(:)  = stf(:, po4_ind)
    diags(ind_diag%DON_RIV_FLUX)%field_2d(:)  = stf(:, don_ind)
    diags(ind_diag%DONr_RIV_FLUX)%field_2d(:) = stf(:, donr_ind)
    diags(ind_diag%DOP_RIV_FLUX)%field_2d(:)  = stf(:, dop_ind)
    diags(ind_diag%DOPr_RIV_FLUX)%field_2d(:) = stf(:, dopr_ind)
    diags(ind_diag%DOC_RIV_FLUX)%field_2d(:)  = stf(:, doc_ind)
    diags(ind_diag%DOCr_RIV_FLUX)%field_2d(:) = stf(:, docr_ind)

    ! FIXME #63 : reported units of DUST_FLUX are g/cm^2/s, so this comment doesn't make sense
    ! multiply DUST flux by mpercm (.01) to convert from model units (cm/s)(mmol/m^3) to mmol/s/m^2
    diags(ind_diag%DUST_FLUX)%field_2d(:) = DUST_FLUX_IN(:)*mpercm

    end associate

  end subroutine marbl_diagnostics_set_surface_forcing

  !***********************************************************************

  subroutine store_diagnostics_carbonate(marbl_domain, carbonate, marbl_interior_diags)

    type(marbl_domain_type)      , intent(in)    :: marbl_domain
    type(carbonate_type)         , intent(in)    :: carbonate(:)
    type(marbl_diagnostics_type) , intent(inout) :: marbl_interior_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: k
    real(r8) :: zsat_calcite, zsat_aragonite
    !-----------------------------------------------------------------------
    
    associate(                                               &
         km                => marbl_domain%km,               &
         diags             => marbl_interior_diags%diags,    &
         ind               => marbl_interior_diag_ind,       &
         CO3               => carbonate(:)%CO3,              &
         CO3_sat_calcite   => carbonate(:)%CO3_sat_calcite,  &
         CO3_sat_aragonite => carbonate(:)%CO3_sat_aragonite &
         )

    ! Find depth where CO3 = CO3_sat_calcite or CO3_sat_argonite
    diags(ind%zsatcalc)%field_2d(1) =  compute_saturation_depth(marbl_domain, CO3, CO3_sat_calcite)
    diags(ind%zsatarag)%field_2d(1) =  compute_saturation_depth(marbl_domain, CO3, CO3_sat_aragonite)

    do k = 1, km
       diags(ind%CO3)%field_3d(k, 1)           = carbonate(k)%CO3
       diags(ind%HCO3)%field_3d(k, 1)          = carbonate(k)%HCO3
       diags(ind%H2CO3)%field_3d(k, 1)         = carbonate(k)%H2CO3
       diags(ind%pH_3D)%field_3d(k, 1)         = carbonate(k)%pH
       diags(ind%CO3_ALT_CO2)%field_3d(k, 1)   = carbonate(k)%CO3_ALT_CO2
       diags(ind%HCO3_ALT_CO2)%field_3d(k, 1)  = carbonate(k)%HCO3_ALT_CO2
       diags(ind%H2CO3_ALT_CO2)%field_3d(k, 1) = carbonate(k)%H2CO3_ALT_CO2
       diags(ind%pH_3D_ALT_CO2)%field_3d(k, 1) = carbonate(k)%pH_ALT_CO2
       diags(ind%co3_sat_calc)%field_3d(k, 1)  = carbonate(k)%CO3_sat_calcite
       diags(ind%co3_sat_arag)%field_3d(k, 1)  = carbonate(k)%CO3_sat_aragonite
    end do

    end associate

  end subroutine store_diagnostics_carbonate

  !***********************************************************************

  function compute_saturation_depth(marbl_domain, CO3, sat_val)

    type(marbl_domain_type) , intent(in) :: marbl_domain
    real(r8)                , intent(in) :: CO3(:)
    real(r8)                , intent(in) :: sat_val(:)

    real(r8) :: compute_saturation_depth

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real(r8) :: anomaly(marbl_domain%km) ! CO3 concentration above saturation at level
    integer  :: k
    !-----------------------------------------------------------------------

    associate(                    &
         kmt => marbl_domain%kmt, &
         zt  => marbl_domain%zt,  &
         zw  => marbl_domain%zw   &
         )

    anomaly(:) = CO3(:) - sat_val(:)

    if (all(anomaly(1:kmt).gt.c0)) then
       compute_saturation_depth = zw(kmt)
    elseif (anomaly(1).le.c0) then
       compute_saturation_depth = c0
    else
       do k=2,kmt
          if (anomaly(k).le.c0) exit
       end do

       ! saturation depth is location of root of anomaly
       compute_saturation_depth = linear_root(zt(k-1:k), anomaly(k-1:k))
    end if

    end associate

  end function compute_saturation_depth

  !***********************************************************************

  function linear_root(x,y)
    ! TO-DO (MNL): if we end up with a marbl_math_mod, this can be generalized
    !              to a better root-finding routine; otherwise maybe we compute
    !              the root inside compute_saturation_depth rather than as a
    !              separate function?

    real(kind=r8), dimension(2), intent(in) :: x,y
    real(kind=r8) :: linear_root

    real(kind=r8) :: m_inv

    if (y(1)*y(2).gt.c0) then
       ! TO-DO (MNL): do we have a marbl_abort() routine? How do I exit if we
       !              hit this error?
       print*, "MNL MNL MNL: can not find root, y-values are same sign!"
    end if
    if (y(2).eq.c0) then
       linear_root = x(2)
    else
       m_inv = (x(2)-x(1))/(y(2)-y(1))
       linear_root = x(1)-m_inv*y(1)
    end if

  end function linear_root

  !***********************************************************************

  subroutine store_diagnostics_nitrification(nitrif, denitrif, marbl_interior_diags)

    real(r8)                     , intent(in)    :: nitrif(:)
    real(r8)                     , intent(in)    :: denitrif(:)
    type(marbl_diagnostics_type) , intent(inout) :: marbl_interior_diags

    associate(                                   &
         diags => marbl_interior_diags%diags,    &
         ind   => marbl_interior_diag_ind        &
         )

    diags(ind%NITRIF)%field_3d(:, 1)   = nitrif
    diags(ind%DENITRIF)%field_3d(:, 1) = denitrif

    end associate

  end subroutine store_diagnostics_nitrification

  !***********************************************************************

  subroutine store_diagnostics_autotrophs(marbl_domain, &
       autotroph_secondary_species, marbl_interior_diags)

    type(marbl_domain_type)                , intent(in)    :: marbl_domain
    type(autotroph_secondary_species_type) , intent(in)    :: autotroph_secondary_species(:,:) ! autotroph_cnt, km
    type(marbl_diagnostics_type)           , intent(inout) :: marbl_interior_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n
    !-----------------------------------------------------------------------

    associate(                                     &
         diags   => marbl_interior_diags%diags,    &
         ind     => marbl_interior_diag_ind,       &
         kmt     => marbl_domain%kmt,              &
         delta_z => marbl_domain%delta_z           &
         )

    diags(ind%tot_CaCO3_form_zint)%field_2d(1) = c0
    diags(ind%tot_bSi_form)%field_3d(:, 1) = c0
    diags(ind%tot_Nfix)%field_3d(:, 1) = c0
    diags(ind%tot_CaCO3_form)%field_3d(:, 1) = c0
    do n = 1, autotroph_cnt
       diags(ind%N_lim(n))%field_3d(:, 1)       = autotroph_secondary_species(n,:)%VNtot
       diags(ind%Fe_lim(n))%field_3d(:, 1)      = autotroph_secondary_species(n,:)%VFe
       diags(ind%P_lim(n))%field_3d(:, 1)       = autotroph_secondary_species(n,:)%VPtot

       if (ind%SiO3_lim(n).ne.-1) then
          diags(ind%SiO3_lim(n))%field_3d(:, 1) = autotroph_secondary_species(n,:)%VSiO3
       end if

       diags(ind%light_lim(n))%field_3d(:, 1)   = autotroph_secondary_species(n,:)%light_lim
       diags(ind%photoNO3(n))%field_3d(:, 1)    = autotroph_secondary_species(n,:)%NO3_V
       diags(ind%photoNH4(n))%field_3d(:, 1)    = autotroph_secondary_species(n,:)%NH4_V
       diags(ind%PO4_uptake(n))%field_3d(:, 1)  = autotroph_secondary_species(n,:)%PO4_V
       diags(ind%DOP_uptake(n))%field_3d(:, 1)  = autotroph_secondary_species(n,:)%DOP_V
       diags(ind%photoFE(n))%field_3d(:, 1)     = autotroph_secondary_species(n,:)%photoFe

       if (ind%bSi_form(n).ne.-1) then
          diags(ind%bSi_form(n))%field_3d(:, 1)  = autotroph_secondary_species(n,:)%photoSi
          diags(ind%tot_bSi_form)%field_3d(:, 1) = diags(ind%tot_bSi_form)%field_3d(:, 1) + &
               diags(ind%bSi_form(n))%field_3d(:, 1)
       endif

       if (ind%CaCO3_form(n).ne.-1) then
          diags(ind%CaCO3_form(n))%field_3d(:, 1)  = autotroph_secondary_species(n,:)%CaCO3_PROD
          diags(ind%tot_CaCO3_form)%field_3d(:, 1) = diags(ind%tot_CaCO3_form)%field_3d(:, 1) + &
               diags(ind%CaCO3_form(n))%field_3d(:, 1)
       end if

       if (ind%Nfix(n).ne.-1) then
          diags(ind%Nfix(n))%field_3d(:, 1)  = autotroph_secondary_species(n,:)%Nfix
          diags(ind%tot_Nfix)%field_3d(:, 1) = diags(ind%tot_Nfix)%field_3d(:, 1) + &
               diags(ind%Nfix(n))%field_3d(:, 1)
       end if

       diags(ind%auto_graze(n))%field_3d(:, 1)     = autotroph_secondary_species(n,:)%auto_graze
       diags(ind%auto_graze_poc(n))%field_3d(:, 1) = autotroph_secondary_species(n,:)%auto_graze_poc
       diags(ind%auto_graze_doc(n))%field_3d(:, 1) = autotroph_secondary_species(n,:)%auto_graze_doc
       diags(ind%auto_graze_zoo(n))%field_3d(:, 1) = autotroph_secondary_species(n,:)%auto_graze_zoo
       diags(ind%auto_loss(n))%field_3d(:, 1)      = autotroph_secondary_species(n,:)%auto_loss
       diags(ind%auto_loss_poc(n))%field_3d(:, 1)  = autotroph_secondary_species(n,:)%auto_loss_poc
       diags(ind%auto_loss_doc(n))%field_3d(:, 1)  = autotroph_secondary_species(n,:)%auto_loss_doc
       diags(ind%auto_agg(n))%field_3d(:, 1)       = autotroph_secondary_species(n,:)%auto_agg
       diags(ind%photoC(n))%field_3d(:, 1)         = autotroph_secondary_species(n,:)%photoC

       diags(ind%photoC_NO3(n))%field_3d(:, 1) = c0
       where (autotroph_secondary_species(n,:)%VNtot > c0)
          diags(ind%photoC_NO3(n))%field_3d(:, 1) = autotroph_secondary_species(n,:)%photoC * &
               (autotroph_secondary_species(n,:)%VNO3 / autotroph_secondary_species(n,:)%VNtot)
       end where

       ! vertical integrals
       if (ind%CaCO3_form_zint(n).ne.-1) then
          call compute_vertical_integrals(autotroph_secondary_species(n,:)%CaCO3_PROD, &
               delta_z, kmt, full_depth_integral=diags(ind%CaCO3_form_zint(n))%field_2d(1))

          diags(ind%tot_CaCO3_form_zint)%field_2d(1) = diags(ind%tot_CaCO3_form_zint)%field_2d(1) + &
               diags(ind%CaCO3_form_zint(n))%field_2d(1)
       end if

       call compute_vertical_integrals(autotroph_secondary_species(n,:)%photoC, &
            delta_z, kmt, full_depth_integral=diags(ind%photoC_zint(n))%field_2d(1))

       call compute_vertical_integrals(diags(ind%photoC_NO3(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%photoC_NO3_zint(n))%field_2d(1))
    end do ! do n

    end associate

  end subroutine store_diagnostics_autotrophs

  !***********************************************************************

  subroutine store_diagnostics_autotroph_sums(marbl_domain, &
       autotroph_secondary_species, marbl_interior_diags)

    type(marbl_domain_type)                , intent(in)    :: marbl_domain
    type(autotroph_secondary_species_type) , intent(in)    :: autotroph_secondary_species(:,:) ! autotroph_cnt, km
    type(marbl_diagnostics_type)           , intent(inout) :: marbl_interior_diags

    integer(int_kind) :: n

    associate(                                     &
         ind     => marbl_interior_diag_ind,       &
         diags   => marbl_interior_diags%diags,    &
         delta_z => marbl_domain%delta_z           &
         )

    diags(ind%auto_graze_TOT)%field_3d(:, 1) = sum(autotroph_secondary_species%auto_graze, dim=1)
    diags(ind%photoC_TOT)%field_3d(:, 1) = sum(autotroph_secondary_species%photoC, dim=1)

    diags(ind%photoC_NO3_TOT)%field_3d(:, 1) = c0
    do n = 1, autotroph_cnt
       where (autotroph_secondary_species(n,:)%VNtot > c0)
          diags(ind%photoC_NO3_TOT)%field_3d(:, 1) = diags(ind%photoC_NO3_TOT)%field_3d(:, 1) + &
               (autotroph_secondary_species(n,:)%VNO3 /  &
               autotroph_secondary_species(n,:)%VNtot) * &
               autotroph_secondary_species(n,:)%photoC
       end where
    end do

    diags(ind%photoC_TOT_zint)%field_2d(1) = sum(delta_z * sum(autotroph_secondary_species%photoC, dim=1))
    diags(ind%photoC_NO3_TOT_zint)%field_2d(1) = sum(delta_z * diags(ind%photoC_NO3_TOT)%field_3d(:, 1))

    end associate

  end subroutine store_diagnostics_autotroph_sums

  !***********************************************************************

  subroutine store_diagnostics_particulates(marbl_domain, &
       marbl_particulate_share, &
       PON_remin, PON_sed_loss, POP_remin, POP_sed_loss, &
       sed_denitrif, other_remin, marbl_interior_forcing_diags)

    !-----------------------------------------------------------------------
    ! - Set tavg variables.
    ! - Accumulte losses of BGC tracers to sediments
    !-----------------------------------------------------------------------

    use marbl_parms , only : POCremin_refract
    use marbl_parms , only : PONremin_refract
    use marbl_parms , only : POPremin_refract

    implicit none

    type(marbl_domain_type)            , intent(in)    :: marbl_domain
    type(marbl_particulate_share_type) , intent(in)    :: marbl_particulate_share
    real(r8), dimension(:)             , intent(in)    :: PON_remin    ! km
    real(r8), dimension(:)             , intent(in)    :: PON_sed_loss ! km
    real(r8), dimension(:)             , intent(in)    :: POP_remin    ! km
    real(r8), dimension(:)             , intent(in)    :: POP_sed_loss ! km
    real(r8), dimension(:)             , intent(in)    :: sed_denitrif ! km
    real(r8), dimension(:)             , intent(in)    :: other_remin  ! km
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_interior_forcing_diags
    !-----------------------------------------------------------------------

    associate(                                          &
         ind     => marbl_interior_diag_ind,            &
         diags   => marbl_interior_forcing_diags%diags, &
         delta_z => marbl_domain%delta_z,               &
         POC     => marbl_particulate_share%POC,        &
         P_CaCO3 => marbl_particulate_share%P_CaCO3,    &
         P_SiO2  => marbl_particulate_share%P_SiO2,     &
         dust    => marbl_particulate_share%dust,       &
         P_iron  => marbl_particulate_share%P_iron      &
         )

    diags(ind%POC_FLUX_IN)%field_3d(:, 1)    = POC%sflux_in + POC%hflux_in
    diags(ind%POC_PROD)%field_3d(:, 1)       = POC%prod
    diags(ind%POC_REMIN)%field_3d(:, 1)      = POC%remin

    diags(ind%POC_REMIN_DIC)%field_3d(:, 1)  = POC%remin * (c1 - POCremin_refract)
    diags(ind%PON_REMIN_NH4)%field_3d(:, 1)  = PON_remin * (c1 - PONremin_refract)
    diags(ind%POP_REMIN_PO4)%field_3d(:, 1)  = POP_remin * (c1 - POPremin_refract)

    diags(ind%CaCO3_FLUX_IN)%field_3d(:, 1)  = P_CaCO3%sflux_in + P_CaCO3%hflux_in
    diags(ind%CaCO3_PROD)%field_3d(:, 1)     = P_CaCO3%prod
    diags(ind%CaCO3_REMIN)%field_3d(:, 1)    = P_CaCO3%remin

    diags(ind%SiO2_FLUX_IN)%field_3d(:, 1)   = P_SiO2%sflux_in + P_SiO2%hflux_in
    diags(ind%SiO2_PROD)%field_3d(:, 1)      = P_SiO2%prod
    diags(ind%SiO2_REMIN)%field_3d(:, 1)     = P_SiO2%remin

    diags(ind%dust_FLUX_IN)%field_3d(:, 1)   = dust%sflux_in + dust%hflux_in
    diags(ind%dust_REMIN)%field_3d(:, 1)     = P_SiO2%remin

    diags(ind%P_iron_FLUX_IN)%field_3d(:, 1) = P_iron%sflux_in + P_iron%hflux_in
    diags(ind%P_iron_PROD)%field_3d(:, 1)    = P_iron%prod
    diags(ind%P_iron_REMIN)%field_3d(:, 1)   = P_iron%remin

    diags(ind%calcToSed)%field_2d(1)   = sum(P_CaCO3%sed_loss)
    diags(ind%bsiToSed)%field_2d(1)    = sum(P_SiO2%sed_loss)
    diags(ind%pocToSed)%field_2d(1)    = sum(POC%sed_loss)
    diags(ind%SedDenitrif)%field_2d(1) = sum(sed_denitrif * delta_z)
    diags(ind%OtherRemin)%field_2d(1)  = sum(other_remin * delta_z)
    diags(ind%ponToSed)%field_2d(1)    = sum(PON_sed_loss)
    diags(ind%popToSed)%field_2d(1)    = sum(POP_sed_loss)
    diags(ind%dustToSed)%field_2d(1)   = sum(dust%sed_loss)
    diags(ind%pfeToSed)%field_2d(1)    = sum(P_iron%sed_loss)

    end associate

  end subroutine store_diagnostics_particulates

   !***********************************************************************

  subroutine store_diagnostics_oxygen(marbl_domain, marbl_interior_forcing_input, &
       column_o2, o2_production, o2_consumption, marbl_interior_diags)

    use marbl_oxygen, only : o2sat_scalar

    type(marbl_domain_type)                 , intent(in)    :: marbl_domain
    type(marbl_interior_forcing_input_type) , intent(in)    :: marbl_interior_forcing_input
    real(r8)                                , intent(in)    :: column_o2(:)
    real(r8)                                , intent(in)    :: o2_production(:)
    real(r8)                                , intent(in)    :: o2_consumption(:)
    type(marbl_diagnostics_type)            , intent(inout) :: marbl_interior_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: k, min_ind
    !-----------------------------------------------------------------------

    ! Find min_o2 and depth_min_o2
    associate(                                                    &
         temperature => marbl_interior_forcing_input%temperature, &
         salinity    => marbl_interior_forcing_input%salinity,    &

         kmt         => marbl_domain%kmt,                         &
         zt          => marbl_domain%zt,                          &
         diags       => marbl_interior_diags%diags,               &
         ind         => marbl_interior_diag_ind                   &
         )

    min_ind = minloc(column_o2(1:kmt), dim=1)

    diags(ind%O2_ZMIN)%field_2d(1)       = column_o2(min_ind)
    diags(ind%O2_ZMIN_DEPTH)%field_2d(1) = zt(min_ind)

    diags(ind%O2_PRODUCTION)%field_3d(:, 1)  = o2_production
    diags(ind%O2_CONSUMPTION)%field_3d(:, 1) = o2_consumption

    do k=1,kmt
       diags(ind%AOU)%field_3d(k, 1) = O2SAT_scalar(temperature(k), salinity(k)) - column_o2(k)
    end do

    end associate

  end subroutine store_diagnostics_oxygen

   !***********************************************************************

  subroutine store_diagnostics_PAR( marbl_domain, PAR_col_frac, PAR_avg, marbl_interior_diags)

    type(marbl_domain_type)      , intent(in)    :: marbl_domain
    real(r8)                     , intent(in)    :: PAR_col_frac(:)
    real(r8)                     , intent(in)    :: PAR_avg(:,:)
    type(marbl_diagnostics_type) , intent(inout) :: marbl_interior_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer :: k
    !-----------------------------------------------------------------------

    associate(                                   &
         km    => marbl_domain%km,               &
         diags => marbl_interior_diags%diags,    &
         ind   => marbl_interior_diag_ind        &
         )

    do k=1,km
       diags(ind%PAR_avg)%field_3d(k, 1) = sum(PAR_col_frac(:)*PAR_avg(k,:))
    end do

    end associate

  end subroutine store_diagnostics_PAR

  !***********************************************************************

  subroutine store_diagnostics_misc(marbl_interior_diags)
    type(marbl_diagnostics_type), intent(inout) :: marbl_interior_diags
  end subroutine store_diagnostics_misc

  !***********************************************************************

  subroutine store_diagnostics_zooplankton(zooplankton_secondary_species, marbl_interior_diags)

    type(zooplankton_secondary_species_type) , intent(in)    :: zooplankton_secondary_species(:,:)
    type(marbl_diagnostics_type)             , intent(inout) :: marbl_interior_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n
    !-----------------------------------------------------------------------

    associate(                                   &
         diags => marbl_interior_diags%diags,    &
         ind   => marbl_interior_diag_ind        &
         )

    do n = 1, zooplankton_cnt
       diags(ind%zoo_loss(n))%field_3d(:, 1)      = zooplankton_secondary_species(n,:)%zoo_loss
       diags(ind%zoo_loss_poc(n))%field_3d(:, 1)  = zooplankton_secondary_species(n,:)%zoo_loss_poc
       diags(ind%zoo_loss_doc(n))%field_3d(:, 1)  = zooplankton_secondary_species(n,:)%zoo_loss_doc
       diags(ind%zoo_graze(n))%field_3d(:, 1)     = zooplankton_secondary_species(n,:)%zoo_graze
       diags(ind%zoo_graze_poc(n))%field_3d(:, 1) = zooplankton_secondary_species(n,:)%zoo_graze_poc
       diags(ind%zoo_graze_doc(n))%field_3d(:, 1) = zooplankton_secondary_species(n,:)%zoo_graze_doc
       diags(ind%zoo_graze_zoo(n))%field_3d(:, 1) = zooplankton_secondary_species(n,:)%zoo_graze_zoo
       diags(ind%x_graze_zoo(n))%field_3d(:, 1)   = zooplankton_secondary_species(n,:)%x_graze_zoo
    end do

    end associate

  end subroutine store_diagnostics_zooplankton

  !***********************************************************************

  subroutine store_diagnostics_dissolved_organic_matter(marbl_domain, &
       dissolved_organic_matter, fe_scavenge, fe_scavenge_rate, marbl_diags)

    type(marbl_domain_type)      , intent(in)    :: marbl_domain
    type(dissolved_organic_matter_type) , intent(in)    :: dissolved_organic_matter(:) ! (km)
    real(r8)                            , intent(in)    :: fe_scavenge(:)              ! (km)
    real(r8)                            , intent(in)    :: fe_scavenge_rate(:)         ! (km)
    type(marbl_diagnostics_type)        , intent(inout) :: marbl_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: k
    !-----------------------------------------------------------------------

    associate(                            &
         km    => marbl_domain%km,        &
         diags => marbl_diags%diags,      &
         ind   => marbl_interior_diag_ind &
         )

    do k = 1, km
       diags(ind%DOC_prod)%field_3d(k, 1)         = dissolved_organic_matter(k)%DOC_prod
       diags(ind%DOC_remin)%field_3d(k, 1)        = dissolved_organic_matter(k)%DOC_remin
       diags(ind%DOCr_remin)%field_3d(k, 1)       = dissolved_organic_matter(k)%DOCr_remin
       diags(ind%DON_prod)%field_3d(k, 1)         = dissolved_organic_matter(k)%DON_prod
       diags(ind%DON_remin)%field_3d(k, 1)        = dissolved_organic_matter(k)%DON_remin
       diags(ind%DONr_remin)%field_3d(k, 1)       = dissolved_organic_matter(k)%DONr_remin
       diags(ind%DOP_prod)%field_3d(k, 1)         = dissolved_organic_matter(k)%DOP_prod
       diags(ind%DOP_remin)%field_3d(k, 1)        = dissolved_organic_matter(k)%DOP_remin
       diags(ind%DOPr_remin)%field_3d(k, 1)       = dissolved_organic_matter(k)%DOPr_remin

       diags(ind%Fe_scavenge)%field_3d(k, 1)      = Fe_scavenge(k)
       diags(ind%Fe_scavenge_rate)%field_3d(k, 1) = Fe_scavenge_rate(k)
    end do

    end associate

  end subroutine store_diagnostics_dissolved_organic_matter

  !***********************************************************************

  subroutine store_diagnostics_carbon_fluxes(marbl_domain, POC, P_CaCO3,      &
             dtracers, marbl_tracer_indices, marbl_diags)

    type(marbl_domain_type)     , intent(in)    :: marbl_domain
    type(column_sinking_particle_type) , intent(in)    :: POC
    type(column_sinking_particle_type) , intent(in)    :: P_CaCO3
    real(r8)                           , intent(in)    :: dtracers(:,:) ! marbl_total_tracer_cnt, km
    type(marbl_tracer_index_type)      , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n, auto_ind
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                               &
         diags   => marbl_diags%diags,       &
         ind     => marbl_interior_diag_ind, &
         kmt     => marbl_domain%kmt,        &
         delta_z => marbl_domain%delta_z,    &

         dic_ind  => marbl_tracer_indices%dic_ind,                            &
         doc_ind  => marbl_tracer_indices%doc_ind,                            &
         docr_ind => marbl_tracer_indices%docr_ind                            &
         )

    ! vertical integrals
    work = dtracers(dic_ind,:) + dtracers(doc_ind,:) +                        &
         dtracers(docr_ind,:) +                                               &
         sum(dtracers(marbl_tracer_indices%zoo_inds(:)%C_ind,:), dim=1) +     &
         sum(dtracers(marbl_tracer_indices%auto_inds(:)%C_ind,:),dim=1)
    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n.gt.0) then
          work = work + dtracers(n,:)
       end if
    end do
    call compute_vertical_integrals(work, delta_z, kmt,                       &
         full_depth_integral=diags(ind%Jint_Ctot)%field_2d(1),                &
         near_surface_integral=diags(ind%Jint_100m_Ctot)%field_2d(1),         &
         integrated_terms = POC%sed_loss + P_CaCO3%sed_loss)

    end associate

  end subroutine store_diagnostics_carbon_fluxes

  !***********************************************************************

  subroutine store_diagnostics_nitrogen_fluxes(marbl_domain, &
       PON_sed_loss, denitrif, sed_denitrif, autotroph_secondary_species,     &
       dtracers, marbl_tracer_indices, marbl_diags)

    use marbl_parms, only : Q

    type(marbl_domain_type)         , intent(in)    :: marbl_domain
    real(r8)                               , intent(in)    :: PON_sed_loss(:) ! km
    real(r8)                               , intent(in)    :: denitrif(:)     ! km
    real(r8)                               , intent(in)    :: sed_denitrif(:) ! km
    type(autotroph_secondary_species_type) , intent(in)    :: autotroph_secondary_species(:,:)
    real(r8)                               , intent(in)    :: dtracers(:,:)      ! marbl_total_tracer_cnt, km
    type(marbl_tracer_index_type)          , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)           , intent(inout) :: marbl_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                               &
         diags   => marbl_diags%diags,       &
         ind     => marbl_interior_diag_ind, &
         kmt     => marbl_domain%kmt,        &
         delta_z => marbl_domain%delta_z,    &

         no3_ind  => marbl_tracer_indices%no3_ind,                            &
         nh4_ind  => marbl_tracer_indices%nh4_ind,                            &
         don_ind  => marbl_tracer_indices%don_ind,                            &
         donr_ind => marbl_tracer_indices%donr_ind                            &
         )

    ! vertical integrals
    work = dtracers(no3_ind,:) + dtracers(nh4_ind,:) +                        &
           dtracers(don_ind,:) + dtracers(donr_ind,:) +                       &
           Q * sum(dtracers(marbl_tracer_indices%zoo_inds(:)%C_ind,:), dim=1) +  &
           Q * sum(dtracers(marbl_tracer_indices%auto_inds(:)%C_ind,:), dim=1) + &
           denitrif(:) + sed_denitrif(:)
    ! subtract out N fixation
    do n = 1, autotroph_cnt
       if (autotrophs(n)%Nfixer) then
          work = work - autotroph_secondary_species(n,:)%Nfix
       end if
    end do
    call compute_vertical_integrals(work, delta_z, kmt,                       &
         full_depth_integral=diags(ind%Jint_Ntot)%field_2d(1),                &
         near_surface_integral=diags(ind%Jint_100m_Ntot)%field_2d(1),         &
         integrated_terms = PON_sed_loss)

    end associate

  end subroutine store_diagnostics_nitrogen_fluxes

  !***********************************************************************

  subroutine store_diagnostics_phosphorus_fluxes(marbl_domain,                &
       POP_sed_loss, dtracers, marbl_tracer_indices, marbl_diags)

    use marbl_parms,  only : Qp_zoo_pom

    type(marbl_domain_type) , intent(in)    :: marbl_domain
    real(r8)                       , intent(in)    :: POP_sed_loss(:) ! km
    real(r8)                       , intent(in)    :: dtracers(:,:)    ! marbl_total_tracer_cnt, km
    type(marbl_tracer_index_type)  , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)   , intent(inout) :: marbl_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                               &
         diags   => marbl_diags%diags,       &
         ind     => marbl_interior_diag_ind, &
         kmt     => marbl_domain%kmt,        &
         delta_z => marbl_domain%delta_z,    &

         po4_ind  => marbl_tracer_indices%po4_ind,                            &
         dop_ind  => marbl_tracer_indices%dop_ind,                            &
         dopr_ind => marbl_tracer_indices%dopr_ind                            &
         )

    ! vertical integrals
    work = dtracers(po4_ind,:) + dtracers(dop_ind,:) + dtracers(dopr_ind,:)
    do n = 1, zooplankton_cnt
       work = work + Qp_zoo_pom * dtracers(marbl_tracer_indices%zoo_inds(n)%C_ind,:)
    end do
    do n = 1, autotroph_cnt
       work = work + autotrophs(n)%Qp * dtracers(marbl_tracer_indices%auto_inds(n)%C_ind,:)
    end do
    call compute_vertical_integrals(work, delta_z, kmt,                       &
         full_depth_integral=diags(ind%Jint_Ptot)%field_2d(1),                &
         near_surface_integral=diags(ind%Jint_100m_Ptot)%field_2d(1),         &
         integrated_terms = POP_sed_loss)

    end associate

  end subroutine store_diagnostics_phosphorus_fluxes

  !***********************************************************************

  subroutine store_diagnostics_silicon_fluxes(marbl_domain, &
       P_SiO2, dtracers, marbl_tracer_indices, marbl_diags)

    type(marbl_domain_type)            , intent(in)    :: marbl_domain
    type(column_sinking_particle_type) , intent(in)    :: P_SiO2
    real(r8)                           , intent(in)    :: dtracers(:,:) ! marbl_total_tracer_cnt, km
    type(marbl_tracer_index_type)      , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                               &
         diags   => marbl_diags%diags,       &
         ind     => marbl_interior_diag_ind, &
         kmt     => marbl_domain%kmt,        &
         zw      => marbl_domain%zw,         &
         delta_z => marbl_domain%delta_z     &
         )

    ! vertical integrals
    work = dtracers(marbl_tracer_indices%sio3_ind,:)
    do n = 1, autotroph_cnt
       if (marbl_tracer_indices%auto_inds(n)%Si_ind > 0) then
          work = work + dtracers(marbl_tracer_indices%auto_inds(n)%Si_ind,:)
       end if
    end do
    call compute_vertical_integrals(work, delta_z, kmt,                       &
         full_depth_integral=diags(ind%Jint_Sitot)%field_2d(1),               &
         near_surface_integral=diags(ind%Jint_100m_Sitot)%field_2d(1),        &
         integrated_terms = P_SiO2%sed_loss)

    end associate

  end subroutine store_diagnostics_silicon_fluxes

  !***********************************************************************

  subroutine store_diagnostics_iron_fluxes(marbl_domain, P_iron, dust, &
             fesedflux, dtracers, marbl_tracer_indices, marbl_diags)

    use marbl_parms     , only : Qfe_zoo
    use marbl_parms     , only : dust_to_Fe

    type(marbl_domain_type)            , intent(in)    :: marbl_domain
    type(column_sinking_particle_type) , intent(in)    :: P_iron
    type(column_sinking_particle_type) , intent(in)    :: dust
    real(r8), dimension(:)             , intent(in)    :: fesedflux  ! km
    real(r8), dimension(:,:)           , intent(in)    :: dtracers ! marbl_total_tracer_cnt, km
    type(marbl_tracer_index_type)      , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                               &
         diags   => marbl_diags%diags,       &
         ind     => marbl_interior_diag_ind, &
         kmt     => marbl_domain%kmt,        &
         zw      => marbl_domain%zw,         &
         delta_z => marbl_domain%delta_z,    &
         fe_ind  => marbl_tracer_indices%fe_ind &
         )

    ! vertical integrals
    work = dtracers(fe_ind, :) +                                              &
           sum(dtracers(marbl_tracer_indices%auto_inds(:)%Fe_ind, :),dim=1) + &
           Qfe_zoo * sum(dtracers(marbl_tracer_indices%zoo_inds(:)%C_ind, :),dim=1) - &
           dust%remin(:) * dust_to_Fe
    call compute_vertical_integrals(work, delta_z, kmt,                       &
         full_depth_integral=diags(ind%Jint_Fetot)%field_2d(1),               &
         near_surface_integral=diags(ind%Jint_100m_Fetot)%field_2d(1),        &
         integrated_terms = P_iron%sed_loss - fesedflux)

    end associate

  end subroutine store_diagnostics_iron_fluxes

  !*****************************************************************************

  subroutine store_diagnostics_ciso_interior(&
       marbl_domain,        &
       autotroph_d13C,      &
       autotroph_d14C,      &
       autotrophCaCO3_d13C, &
       autotrophCaCO3_d14C, &
       DIC_d13C,            &
       DIC_d14C,            &
       DOC_d13C,            &
       DOC_d14C,            &
       zooC_d13C,           &
       zooC_d14C,           &
       photo13C,            &
       photo14C,            &
       eps_autotroph,       &
       mui_to_co2star,      &
       Ca13CO3_prod,        &
       Ca14CO3_prod,        &
       DO13C_prod,          &
       DO14C_prod,          &
       DO13C_remin,         &
       DO14C_remin,         &
       eps_aq_g,            &
       eps_dic_g,           &
       PO13C,               &
       PO14C,               &
       P_Ca13CO3,           &
       P_Ca14CO3,           &
       dtracers,            &
       marbl_tracer_indices,&
       marbl_diags)

    !---------------------------------------------------------------------
    ! !DESCRIPTION:
    !  Update marbl_interior_ciso_diags data type 
    !---------------------------------------------------------------------

    implicit none

    type(marbl_domain_type), intent(in)    :: marbl_domain

    real (r8), intent(in),  dimension(autotroph_cnt, marbl_domain%km) :: &
         autotroph_d13C      , & ! d13C of autotroph C
         autotroph_d14C      , & ! d14C of autotroph C
         autotrophCaCO3_d13C , & ! d13C of autotrophCaCO3
         autotrophCaCO3_d14C , & ! d14C of autotrophCaCO3
         photo13C            , & ! Carbon autotroph 13C-fixation (mmol C/m^3/sec)
         photo14C            , & ! Carbon autotroph 14C-fixation (mmol C/m^3/sec)
         eps_autotroph       , & ! Permil fractionation (or discrimination factor) for Carbon autotroph types sp, diat, diaz
         mui_to_co2star      , & ! Carbon autotroph instanteous growth rate over [CO2*] (m^3 /mmol C /s)
         Ca13CO3_prod        , & ! prod. of 13C CaCO3 by small phyto (mmol CaCO3/m^3/sec)
         Ca14CO3_prod            ! prod. of 13C CaCO3 by small phyto (mmol CaCO3/m^3/sec)

    real (r8), intent(in),  dimension(marbl_domain%km) :: &
         DIC_d13C    , & ! d13C of DIC
         DOC_d13C    , & ! d13C of DOC
         zooC_d13C   , & ! d13C of zooC
         DIC_d14C    , & ! d14C of DIC
         DOC_d14C    , & ! d14C of DOC
         zooC_d14C   , & ! d14C of zooC
         DO13C_prod  , & ! production of 13C DOC (mmol C/m^3/sec)
         DO13C_remin , & ! remineralization of 13C DOC (mmol C/m^3/sec)
         DO14C_prod  , & ! production of 13C DOC (mmol C/m^3/sec)
         DO14C_remin , & ! remineralization of 13C DOC (mmol C/m^3/sec)
         eps_aq_g    , & ! equilibrium fractionation (CO2_gaseous <-> CO2_aq)
         eps_dic_g       ! equilibrium fractionation between total DIC and gaseous CO2

    real (r8), intent(in) :: dtracers(:,:) ! (marbl_total_tracer_cnt, km) computed source/sink terms

    type(marbl_tracer_index_type), intent(in)      :: marbl_tracer_indices

    type(column_sinking_particle_type), intent(in) :: &
         PO13C,        &  ! base units = nmol 13C
         PO14C,        &  ! base units = nmol 14C
         P_Ca13CO3,    &  ! base units = nmol 13C CaCO3
         P_Ca14CO3        ! base units = nmol 14C CaCO3

    type(marbl_diagnostics_type), intent(inout) :: &
         marbl_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: k, n, auto_ind
    real (r8)          :: work(marbl_domain%km)
    !-----------------------------------------------------------------------

    associate( &
         km      => marbl_domain%km,         &
         kmt     => marbl_domain%kmt,        &
         zw      => marbl_domain%zw,         &
         delta_z => marbl_domain%delta_z,    &
         diags   => marbl_diags%diags,       &
         ind     => marbl_interior_diag_ind,  &
         di13c_ind  => marbl_tracer_indices%di13c_ind,                   &
         do13c_ind  => marbl_tracer_indices%do13c_ind,                   &
         zoo13c_ind => marbl_tracer_indices%zoo13c_ind,                  &
         di14c_ind  => marbl_tracer_indices%di14c_ind,                   &
         do14c_ind  => marbl_tracer_indices%do14c_ind,                   &
         zoo14c_ind => marbl_tracer_indices%zoo14c_ind                   &
         )

    diags(ind%calcToSed_13C)%field_2d(1) = sum(P_Ca13CO3%sed_loss)
    diags(ind%calcToSed_14C)%field_2d(1) = sum(P_Ca14CO3%sed_loss)

    diags(ind%pocToSed_13C)%field_2d(1)  = sum(PO13C%sed_loss)
    diags(ind%pocToSed_14C)%field_2d(1)  = sum(PO14C%sed_loss)

    diags(ind%CISO_photo13C_TOT)%field_3d(:, 1) = sum(photo13C, dim=1)
    diags(ind%CISO_photo14C_TOT)%field_3d(:, 1) = sum(photo14C, dim=1)

    diags(ind%CISO_photo13C_TOT_zint)%field_2d(1) = sum(delta_z * sum(photo13C, dim=1))
    diags(ind%CISO_photo14C_TOT_zint)%field_2d(1) = sum(delta_z * sum(photo14C, dim=1))

    ! Vertical integrals - CISO_Jint_13Ctot and Jint_100m_13Ctot

    work(:) = dtracers(di13c_ind,:) + dtracers(do13c_ind,:) + dtracers(zoo13C_ind,:) &
         + sum(dtracers(marbl_tracer_indices%auto_inds(:)%C13_ind,:), dim=1)
    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
       if (n > 0) then
          work = work + dtracers(n,:)
       end if
    end do
    call compute_vertical_integrals(work, delta_z, kmt,                       &
         full_depth_integral=diags(ind%CISO_Jint_13Ctot)%field_2d(1),         &
         near_surface_integral=diags(ind%CISO_Jint_100m_13Ctot)%field_2d(1),  &
         integrated_terms = PO13C%sed_loss + P_Ca13CO3%sed_loss)

    ! Vertical integral - CISO_Jint_14Ctot and Jint_100m_14Ctot

    work(:) = dtracers(di14c_ind,:) + dtracers(do14c_ind,:) + dtracers(zoo14C_ind,:) &
         + sum(dtracers(marbl_tracer_indices%auto_inds(:)%C14_ind,:), dim=1)
    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
       if (n > 0) then
          work = work + dtracers(n,:)
       end if
    end do
    call compute_vertical_integrals(work, delta_z, kmt,                       &
         full_depth_integral=diags(ind%CISO_Jint_14Ctot)%field_2d(1),         &
         near_surface_integral=diags(ind%CISO_Jint_100m_14Ctot)%field_2d(1),  &
         integrated_terms = PO14C%sed_loss + P_Ca14CO3%sed_loss)

    ! Other vertical integrals

    do n = 1,autotroph_cnt
       call compute_vertical_integrals(photo13C(n,:), delta_z, kmt,           &
            full_depth_integral=diags(ind%CISO_photo13C_zint(n))%field_2d(1))
       
       call compute_vertical_integrals(photo14C(n,:), delta_z, kmt,           &
            full_depth_integral=diags(ind%CISO_photo14C_zint(n))%field_2d(1))
       
       call compute_vertical_integrals(Ca13CO3_prod(n,:), delta_z, kmt,       &
            full_depth_integral=diags(ind%CISO_Ca13CO3_form_zint(n))%field_2d(1))
       
       call compute_vertical_integrals(Ca14CO3_prod(n,:), delta_z, kmt,       &
            full_depth_integral=diags(ind%CISO_Ca14CO3_form_zint(n))%field_2d(1))
    end do

    do k = 1,km
       do n = 1, autotroph_cnt
          diags(ind%CISO_d13C(n))%field_3d(k, 1)                = autotroph_d13C(n,k)
          diags(ind%CISO_d14C(n))%field_3d(k, 1)                = autotroph_d14C(n,k)

          diags(ind%CISO_autotrophCaCO3_d13C(n))%field_3d(k, 1) = autotrophCaCO3_d13C(n,k)
          diags(ind%CISO_autotrophCaCO3_d14C(n))%field_3d(k, 1) = autotrophCaCO3_d14C(n,k)

          diags(ind%CISO_photo13C(n))%field_3d(k, 1)            = photo13C(n,k)
          diags(ind%CISO_photo14C(n))%field_3d(k, 1)            = photo14C(n,k)

          diags(ind%CISO_eps_autotroph(n))%field_3d(k, 1)       = eps_autotroph(n,k)

          diags(ind%CISO_mui_to_co2star(n))%field_3d(k, 1)      = mui_to_co2star(n,k)

          if (autotrophs(n)%imp_calcifier) then
             diags(ind%CISO_Ca13CO3_form(n))%field_3d(k, 1)     = Ca13CO3_prod(n,k)
             diags(ind%CISO_Ca14CO3_form(n))%field_3d(k, 1)     = Ca14CO3_prod(n,k)
          end if
       end do  ! end loop over autotrophs
    end do  ! end loop over k
    
    do k = 1,km
       diags(ind%CISO_DIC_d13C)%field_3d(k, 1)        = DIC_d13C(k)
       diags(ind%CISO_DIC_d14C)%field_3d(k, 1)        = DIC_d14C(k)

       diags(ind%CISO_DOC_d13C)%field_3d(k, 1)        = DOC_d13C(k)
       diags(ind%CISO_DOC_d14C)%field_3d(k, 1)        = DOC_d14C(k)

       diags(ind%CISO_DO13C_prod)%field_3d(k, 1)      = DO13C_prod(k)   
       diags(ind%CISO_DO14C_prod)%field_3d(k, 1)      = DO14C_prod(k)      

       diags(ind%CISO_DO13C_remin)%field_3d(k, 1)     = DO13C_remin(k)     
       diags(ind%CISO_DO14C_remin)%field_3d(k, 1)     = DO14C_remin(k)     

       diags(ind%CISO_zooC_d13C)%field_3d(k, 1)       = zooC_d13C(k)
       diags(ind%CISO_zooC_d14C)%field_3d(k, 1)       = zooC_d14C(k)

       diags(ind%CISO_eps_aq_g)%field_3d(k, 1)        = eps_aq_g(k)        
       diags(ind%CISO_eps_dic_g)%field_3d(k, 1)       = eps_dic_g(k)       

       diags(ind%CISO_Ca13CO3_flux_in)%field_3d(k, 1) = P_Ca13CO3%sflux_in(k) + P_Ca13CO3%hflux_in(k)
       diags(ind%CISO_Ca14CO3_flux_in)%field_3d(k, 1) = P_Ca14CO3%sflux_in(k) + P_Ca14CO3%hflux_in(k)

       diags(ind%CISO_Ca13CO3_prod)%field_3d(k, 1)    = P_Ca13CO3%prod(k)
       diags(ind%CISO_Ca14CO3_prod)%field_3d(k, 1)    = P_Ca14CO3%prod(k)

       diags(ind%CISO_Ca13CO3_remin)%field_3d(k, 1)   = P_Ca13CO3%remin(k)
       diags(ind%CISO_Ca14CO3_remin)%field_3d(k, 1)   = P_Ca14CO3%remin(k)

       diags(ind%CISO_PO13C_flux_in)%field_3d(k, 1)   = PO13C%sflux_in(k) + PO13C%hflux_in(k)
       diags(ind%CISO_PO14C_flux_in)%field_3d(k, 1)   = PO14C%sflux_in(k) + PO14C%hflux_in(k)

       diags(ind%CISO_PO13C_prod)%field_3d(k, 1)      = PO13C%prod(k)
       diags(ind%CISO_PO14C_prod)%field_3d(k, 1)      = PO14C%prod(k)

       diags(ind%CISO_PO13C_remin)%field_3d(k, 1)     = PO13C%remin(k)
       diags(ind%CISO_PO14C_remin)%field_3d(k, 1)     = PO14C%remin(k)
    end do

    end associate

  end subroutine store_diagnostics_ciso_interior

  !***********************************************************************

  subroutine store_diagnostics_ciso_surface_forcing( &
       num_elements,   &
       D13C,           &
       D14C,           &
       D14C_glo_avg,   &
       FLUX,           &
       FLUX13,         &
       FLUX14,         &
       FLUX_as,        &
       FLUX13_as,      &
       FLUX14_as,      &
       FLUX_sa,        &
       FLUX13_sa,      &
       FLUX14_sa,      &
       R13C_DIC,       &
       R14C_DIC,       &
       R13C_atm,       &
       R14C_atm,       &
       di13c_riv_flux, &
       do13c_riv_flux, &
       di14c_riv_flux, &
       do14c_riv_flux, &
       eps_aq_g_surf,  &
       eps_dic_g_surf, &
       marbl_surface_forcing_diags)

    ! !DESCRIPTION:
    !  Compute surface fluxes for ecosys tracer module.

    use marbl_parms        , only : R13c_std, R14c_std
    use marbl_constants_mod, only : c1000

    implicit none

    integer (int_kind)                 , intent(in)    :: num_elements
    real (r8), dimension(num_elements) , intent(in)    :: D13C           ! atm 13co2 value
    real (r8), dimension(num_elements) , intent(in)    :: D14C           ! atm 14co2 value
    real (r8), dimension(num_elements) , intent(in)    :: D14C_glo_avg
    real (r8), dimension(num_elements) , intent(in)    :: FLUX           ! gas flux of CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX13         ! gas flux of 13CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX14         ! gas flux of 14CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX_as        ! air-to-sea gas flux of CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX13_as      ! air-to-sea gas flux of 13CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX14_as      ! air-to-sea gas flux of 14CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX_sa        ! sea-to-air gas flux of CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX13_sa      ! sea-to-air gas flux of 13CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX14_sa      ! sea-to-air gas flux of 14CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: R13C_DIC       ! 13C/12C ratio in DIC
    real (r8), dimension(num_elements) , intent(in)    :: R14C_DIC       ! 14C/12C ratio in total DIC
    real (r8), dimension(num_elements) , intent(in)    :: R13C_atm       ! 13C/12C ratio in atmospheric CO2
    real (r8), dimension(num_elements) , intent(in)    :: R14C_atm       ! 14C/12C ratio in atmospheric CO2
    real (r8), dimension(num_elements) , intent(in)    :: di13c_riv_flux ! River input of DI13C
    real (r8), dimension(num_elements) , intent(in)    :: do13c_riv_flux ! River input of DO13C
    real (r8), dimension(num_elements) , intent(in)    :: di14c_riv_flux ! River input of DI14C
    real (r8), dimension(num_elements) , intent(in)    :: do14c_riv_flux ! River input of DO14C
    real (r8), dimension(num_elements) , intent(in)    :: eps_aq_g_surf  ! equilibrium fractionation (CO2_gaseous <-> CO2_aq)
    real (r8), dimension(num_elements) , intent(in)    :: eps_dic_g_surf ! equilibrium fractionation between total DIC and gaseous CO2
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_surface_forcing_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_diagnostics_mod:store_diagnostics_ciso_surface_forcing'
    !-----------------------------------------------------------------------

    associate(                                          &
         diags => marbl_surface_forcing_diags%diags,    &
         ind   => marbl_surface_forcing_diag_ind        &
         )

    !-----------------------------------------------------------------------
    !    Tavg variables
    !-----------------------------------------------------------------------

    diags(ind%CISO_DI13C_GAS_FLUX)%field_2d(:) = FLUX13(:)
    diags(ind%CISO_DI14C_GAS_FLUX)%field_2d(:) = FLUX14(:)

    diags(ind%CISO_DI13C_AS_GAS_FLUX)%field_2d(:) = FLUX13_as(:)
    diags(ind%CISO_DI14C_AS_GAS_FLUX)%field_2d(:) = FLUX14_as(:)

    diags(ind%CISO_DI13C_SA_GAS_FLUX)%field_2d(:) = FLUX13_sa(:)
    diags(ind%CISO_DI14C_SA_GAs_FLUX)%field_2d(:) = FLUX14_sa(:)

    where ( FLUX(:) /= c0 )
       diags(ind%CISO_D13C_GAS_FLUX)%field_2d(:) = ( FLUX13(:) / FLUX(:) / R13C_std - c1 ) * c1000
       diags(ind%CISO_D14C_GAS_FLUX)%field_2d(:) = ( FLUX14(:) / FLUX(:) / R14C_std - c1 ) * c1000
    elsewhere
       diags(ind%CISO_D13C_GAS_FLUX)%field_2d(:) = c0
       diags(ind%CISO_D14C_GAS_FLUX)%field_2d(:) = c0
    endwhere

    diags(ind%CISO_R13C_DIC_surf)%field_2d(:) = R13C_DIC(:)
    diags(ind%CISO_R14C_DIC_surf)%field_2d(:) = R14C_DIC(:)

    diags(ind%ciso_r13c_atm)%field_2d(:) = R13C_atm(:)
    diags(ind%ciso_r14c_atm)%field_2d(:) = R14C_atm(:)

    diags(ind%ciso_d13c_atm)%field_2d(:) = d13c(:)
    diags(ind%ciso_d14c_atm)%field_2d(:) = d14c(:)        

    diags(ind%CISO_eps_aq_g_surf)%field_2d(:)  = eps_aq_g_surf(:)
    diags(ind%CISO_eps_dic_g_surf)%field_2d(:) = eps_dic_g_surf(:)

    !-------------------------------------------------------------------------
    ! River input of isotopic DIC and DOC.
    ! River input of BGC tracers in marbl_mod is currently constant and from file
    ! So the isotopic carbon input is also done very simplified with one value
    ! globally, even though data shows it should vary from river to river.
    !
    ! Using constant delta values of
    ! D13C=-10 permil for DIC (Mook 1986, Raymond et al 2004)
    ! D13C=-27.6 permil for DOC (Raymond et al 2004)
    ! D14C=-50 permil for DOC (Raymond et al 2004), Gruber et al
    ! D14C= atmos_D14C - 50 permil for DIC (based on very few data points and 
    !       discussion with N. Gruber)
    !-------------------------------------------------------------------------
    
    diags(ind%CISO_DI13C_RIV_FLUX)%field_2d(:) = di13c_riv_flux(:)
    diags(ind%CISO_DO13C_RIV_FLUX)%field_2d(:) = do13c_riv_flux(:)

    diags(ind%CISO_DI14C_RIV_FLUX)%field_2d(:) = di14c_riv_flux(:)
    diags(ind%CISO_DO14C_RIV_FLUX)%field_2d(:) = do14c_riv_flux(:)

    diags(ind%CISO_GLOBAL_D14C)%field_2d(:)    = D14C_glo_avg(:) ! all the values are identical

    end associate

  end subroutine store_diagnostics_ciso_surface_forcing

  !*****************************************************************************

  subroutine compute_vertical_integrals(integrand, delta_z, kmt, &
       full_depth_integral, near_surface_integral, integrated_terms)

    real(kind=r8) , intent(in)             :: integrand(:)
    real(kind=r8) , intent(in)             :: delta_z(:)
    integer       , intent(in)             :: kmt
    ! For some vertical integral diagnostics, we need to add additional terms
    ! that have already been integrated, so they are separated from the
    ! integrand
    real(kind=r8) , intent(in)  , optional :: integrated_terms(:)
    real(kind=r8) , intent(out) , optional :: full_depth_integral
    real(kind=r8) , intent(out) , optional :: near_surface_integral

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: k
    real(kind=r8)      :: integrated_terms_used(size(integrand))
    real(kind=r8)      :: zw
    real(kind=r8)      :: ztop
    real(kind=r8)      :: shallow_depth = 100.0e2_r8
    !-----------------------------------------------------------------------

    if (present(integrated_terms)) then
       integrated_terms_used = integrated_terms
    else
       integrated_terms_used = c0
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
                              min(shallow_depth-ztop,delta_z(k))*integrand(k)
          if (zw.le.shallow_depth) then
             near_surface_integral = near_surface_integral + integrated_terms_used(k)
          else
             exit
          end if
          ztop = zw
       end do
    end if

  end subroutine compute_vertical_integrals

  subroutine log_add_diagnostics_error(marbl_status_log, sname, subname)

    type(marbl_log_type), intent(inout) :: marbl_status_log
    character(len=*),     intent(in)    :: sname, subname
    character(len=char_len) :: routine_name

    write(routine_name,"(3A)") "diags%add_diagnostic(", trim(sname), ")"
    call marbl_status_log%log_error_trace(routine_name, subname)

  end subroutine log_add_diagnostics_error

end module marbl_diagnostics_mod
