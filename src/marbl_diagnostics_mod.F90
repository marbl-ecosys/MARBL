! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_diagnostics_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_settings_mod, only : autotroph_cnt
  use marbl_settings_mod, only : zooplankton_cnt
  use marbl_settings_mod, only : autotroph_settings
  use marbl_settings_mod, only : zooplankton_settings

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1

  use marbl_interface_private_types, only : carbonate_type
  use marbl_interface_private_types, only : dissolved_organic_matter_type
  use marbl_interface_private_types, only : column_sinking_particle_type
  use marbl_interface_private_types, only : marbl_PAR_type
  use marbl_interface_private_types, only : autotroph_local_type
  use marbl_interface_private_types, only : autotroph_derived_terms_type
  use marbl_interface_private_types, only : zooplankton_derived_terms_type
  use marbl_interface_private_types, only : marbl_particulate_share_type
  use marbl_interface_private_types, only : marbl_surface_flux_internal_type
  use marbl_interface_private_types, only : marbl_tracer_index_type

  use marbl_interface_public_types, only : marbl_domain_type
  use marbl_interface_public_types, only : marbl_tracer_metadata_type
  use marbl_interface_public_types, only : marbl_forcing_fields_type
  use marbl_interface_public_types, only : marbl_saved_state_type
  use marbl_interface_public_types, only : marbl_diagnostics_type

  use marbl_logging, only : marbl_log_type
  use marbl_logging, only : marbl_logging_add_diagnostics_error

  use marbl_diagnostics_share_mod, only : marbl_surface_flux_diag_ind
  use marbl_diagnostics_share_mod, only : marbl_interior_tendency_diag_ind
  use marbl_diagnostics_share_mod, only : marbl_diagnostics_share_compute_vertical_integrals

  implicit none
  public

  !-----------------------------------------------------------------------
  !  public/private member procedure declarations
  !-----------------------------------------------------------------------

  public :: marbl_diagnostics_init
  public :: marbl_diagnostics_interior_tendency_compute
  public :: marbl_diagnostics_surface_flux_compute

  private :: store_diagnostics_carbonate
  private :: store_diagnostics_nitrification
  private :: store_diagnostics_autotrophs
  private :: store_diagnostics_particulates
  private :: store_diagnostics_oxygen
  private :: store_diagnostics_PAR
  private :: store_diagnostics_zooplankton
  private :: store_diagnostics_dissolved_organic_matter
  private :: store_diagnostics_iron_cycle
  private :: store_diagnostics_carbon_fluxes
  private :: store_diagnostics_nitrogen_fluxes
  private :: store_diagnostics_phosphorus_fluxes
  private :: store_diagnostics_silicon_fluxes
  private :: store_diagnostics_iron_fluxes
  private :: compute_saturation_depth

  !***********************************************************************

contains

  !***********************************************************************

  subroutine marbl_diagnostics_init(  &
       marbl_domain,                  &
       marbl_tracer_metadata,         &
       marbl_tracer_indices,          &
       marbl_interior_tendency_diags, &
       marbl_surface_flux_diags,      &
       marbl_status_log)

    use marbl_settings_mod, only : lo2_consumption_scalef
    use marbl_settings_mod, only : lp_remin_scalef
    use marbl_settings_mod, only : lvariable_PtoC
    use marbl_settings_mod, only : particulate_flux_ref_depth
    use marbl_ciso_diagnostics_mod, only : marbl_ciso_diagnostics_init

    type(marbl_domain_type)           , intent(in)    :: marbl_domain
    type(marbl_tracer_metadata_type)  , intent(in)    :: marbl_tracer_metadata(:) ! descriptors for each tracer
    type(marbl_tracer_index_type)     , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)      , intent(inout) :: marbl_interior_tendency_diags
    type(marbl_diagnostics_type)      , intent(inout) :: marbl_surface_flux_diags
    type(marbl_log_type)              , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer :: n
    logical :: truncate
    character(len=char_len) :: lname, sname, units, vgrid
    character(len=char_len) :: particulate_flux_ref_depth_str

    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:marbl_diagnostics_init'
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------
    ! Surface forcing diagnostics
    !-----------------------------------------------------------------

    call marbl_surface_flux_diags%construct(marbl_domain%num_elements_surface_flux, marbl_domain%km)
    call marbl_interior_tendency_diags%construct(marbl_domain%num_elements_interior_tendency, marbl_domain%km)

    associate(                                    &
              ind => marbl_surface_flux_diag_ind, &
              diags => marbl_surface_flux_diags   &
             )

      lname    = 'Ice Fraction for ecosys fluxes'
      sname    = 'ECOSYS_IFRAC'
      units    = 'fraction'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%ECOSYS_IFRAC, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'XKW for ecosys fluxes'
      sname    = 'ECOSYS_XKW'
      units    = 'cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%ECOSYS_XKW, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Atmospheric Pressure for ecosys fluxes'
      sname    = 'ECOSYS_ATM_PRESS'
      units    = 'atmospheres'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%ECOSYS_ATM_PRESS, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'PV_O2'
      sname    = 'PV_O2'
      units    = 'cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%PV_O2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'O2 Schmidt Number'
      sname    = 'SCHMIDT_O2'
      units    = '1'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%SCHMIDT_O2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'O2 Saturation'
      sname    = 'O2SAT'
      units    = 'mmol/m^3'      ! = nmol/cm^3
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%O2SAT, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'CO2 Star'
      sname    = 'CO2STAR'
      units    = 'mmol/m^3'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CO2STAR, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'D CO2 Star'
      sname    = 'DCO2STAR'
      units    = 'mmol/m^3'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DCO2STAR, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'surface pCO2'
      sname    = 'pCO2SURF'
      units    = 'ppmv'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%pCO2SURF, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'D pCO2'
      sname    = 'DpCO2'
      units    = 'ppmv'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DpCO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'CO2 Piston Velocity'
      sname    = 'PV_CO2'
      units    = 'cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%PV_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'CO2 Schmidt Number'
      sname    = 'SCHMIDT_CO2'
      units    = '1'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%SCHMIDT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'DIC Surface Gas Flux'
      sname    = 'FG_CO2'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DIC_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Surface pH'
      sname    = 'PH'
      units    = '1'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%PH, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Atmospheric CO2'
      sname    = 'ATM_CO2'
      units    = 'ppmv'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%ATM_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'CO2 Star, Alternative CO2'
      sname    = 'CO2STAR_ALT_CO2'
      units    = 'mmol/m^3'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CO2STAR_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'D CO2 Star, Alternative CO2'
      sname    = 'DCO2STAR_ALT_CO2'
      units    = 'mmol/m^3'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DCO2STAR_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'surface pCO2, Alternative CO2'
      sname    = 'pCO2SURF_ALT_CO2'
      units    = 'ppmv'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%pCO2SURF_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'D pCO2, Alternative CO2'
      sname    = 'DpCO2_ALT_CO2'
      units    = 'ppmv'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DpCO2_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'DIC Surface Gas Flux, Alternative CO2'
      sname    = 'FG_ALT_CO2'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DIC_GAS_FLUX_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Surface pH, Alternative CO2'
      sname    = 'PH_ALT_CO2'
      units    = '1'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%PH_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Atmospheric Alternative CO2'
      sname    = 'ATM_ALT_CO2'
      units    = 'ppmv'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%ATM_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Atmospheric Iron Flux'
      sname    = 'IRON_FLUX'
      units    = 'mmol/m^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%IRON_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Dust Flux'
      sname    = 'DUST_FLUX'
      units    = 'g/cm^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DUST_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Flux of NOx from Atmosphere'
      sname    = 'NOx_FLUX'
      units    = 'nmol/cm^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%NOx_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Flux of NHy from Atmosphere'
      sname    = 'NHy_FLUX'
      units    = 'nmol/cm^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%NHy_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Emission of NHx to Atmosphere'
      sname    = 'NHx_SURFACE_EMIS'
      units    = 'nmol/cm^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%NHx_SURFACE_EMIS, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

    end associate

    !-----------------------------------------------------------------
    ! Interior diagnostics
    !-----------------------------------------------------------------

    associate(                                         &
              ind => marbl_interior_tendency_diag_ind, &
              diags => marbl_interior_tendency_diags   &
             )

      ! General 2D diags
      lname = 'Calcite Saturation Depth'
      sname = 'zsatcalc'
      units = 'cm'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%zsatcalc, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Aragonite Saturation Depth'
      sname = 'zsatarag'
      units = 'cm'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%zsatarag, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Minimum of O2'
      sname = 'O2_ZMIN'
      units = 'mmol/m^3'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%O2_ZMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Depth of Vertical Minimum of O2'
      sname = 'O2_ZMIN_DEPTH'
      units = 'cm'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%O2_ZMIN_DEPTH, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Total C Fixation Vertical Integral'
      sname = 'photoC_TOT_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%photoC_TOT_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Total C Fixation Vertical Integral, 0-100m'
      sname = 'photoC_TOT_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%photoC_TOT_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Total C Fixation from NO3 Vertical Integral'
      sname = 'photoC_NO3_TOT_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%photoC_NO3_TOT_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Total C Fixation from NO3 Vertical Integral, 0-100m'
      sname = 'photoC_NO3_TOT_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%photoC_NO3_TOT_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of DOC Production'
      sname = 'DOC_prod_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOC_prod_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of DOC Production, 0-100m'
      sname = 'DOC_prod_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOC_prod_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of DOC Remineralization'
      sname = 'DOC_remin_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOC_remin_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of DOC Remineralization, 0-100m'
      sname = 'DOC_remin_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOC_remin_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of DOCr Remineralization'
      sname = 'DOCr_remin_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOCr_remin_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of DOCr Remineralization, 0-100m'
      sname = 'DOCr_remin_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOCr_remin_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ctot'
      sname = 'Jint_Ctot'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Jint_Ctot, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ntot'
      sname = 'Jint_Ntot'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Jint_Ntot, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Ptot'
      sname = 'Jint_Ptot'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Jint_Ptot, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Sitot'
      sname = 'Jint_Sitot'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Jint_Sitot, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of Conservative Subterms of Source Sink Term for Fetot'
      sname = 'Jint_Fetot'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Jint_Fetot, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      ! Particulate 2D diags
      lname = 'CaCO3 Flux Hitting Sea Floor'
      sname = 'calcToFloor'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%calcToFloor, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Flux to Sediments'
      sname = 'calcToSed'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%calcToSed, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Flux to Sediments, Alternative CO2'
      sname = 'calcToSed_ALT_CO2'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%calcToSed_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POC Flux Hitting Sea Floor'
      sname = 'pocToFloor'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%pocToFloor, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POC Flux to Sediments'
      sname = 'pocToSed'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%pocToSed, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'nitrogen burial Flux to Sediments'
      sname = 'ponToSed'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%ponToSed, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'nitrogen loss in Sediments'
      sname = 'SedDenitrif'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%SedDenitrif, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'non-oxic,non-dentr remin in Sediments'
      sname = 'OtherRemin'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%OtherRemin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'phosphorus Flux to Sediments'
      sname = 'popToSed'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%popToSed, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'biogenic Si Flux to Sediments'
      sname = 'bsiToSed'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%bsiToSed, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'dust Flux to Sediments'
      sname = 'dustToSed'
      units = 'g/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%dustToSed, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'pFe Flux to Sediments'
      sname = 'pfeToSed'
      units = 'nmol/cm^2/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%pfeToSed, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      ! Autotroph 2D diags
      if (.not.ind%lconstructed()) then
        allocate(ind%N_lim_surf(autotroph_cnt))
        allocate(ind%N_lim_Cweight_avg_100m(autotroph_cnt))
        allocate(ind%P_lim_surf(autotroph_cnt))
        allocate(ind%P_lim_Cweight_avg_100m(autotroph_cnt))
        allocate(ind%Fe_lim_surf(autotroph_cnt))
        allocate(ind%Fe_lim_Cweight_avg_100m(autotroph_cnt))
        allocate(ind%SiO3_lim_surf(autotroph_cnt))
        allocate(ind%SiO3_lim_Cweight_avg_100m(autotroph_cnt))
        allocate(ind%light_lim_surf(autotroph_cnt))
        allocate(ind%light_lim_Cweight_avg_100m(autotroph_cnt))
        allocate(ind%photoC_zint(autotroph_cnt))
        allocate(ind%photoC_zint_100m(autotroph_cnt))
        allocate(ind%photoC_NO3_zint(autotroph_cnt))
        allocate(ind%CaCO3_form_zint(autotroph_cnt))
        allocate(ind%CaCO3_form_zint_100m(autotroph_cnt))
        allocate(ind%auto_graze_zint(autotroph_cnt))
        allocate(ind%auto_graze_zint_100m(autotroph_cnt))
        allocate(ind%auto_graze_poc_zint(autotroph_cnt))
        allocate(ind%auto_graze_poc_zint_100m(autotroph_cnt))
        allocate(ind%auto_graze_doc_zint(autotroph_cnt))
        allocate(ind%auto_graze_doc_zint_100m(autotroph_cnt))
        allocate(ind%auto_graze_zoo_zint(autotroph_cnt))
        allocate(ind%auto_graze_zoo_zint_100m(autotroph_cnt))
        allocate(ind%auto_loss_zint(autotroph_cnt))
        allocate(ind%auto_loss_zint_100m(autotroph_cnt))
        allocate(ind%auto_loss_poc_zint(autotroph_cnt))
        allocate(ind%auto_loss_poc_zint_100m(autotroph_cnt))
        allocate(ind%auto_loss_doc_zint(autotroph_cnt))
        allocate(ind%auto_loss_doc_zint_100m(autotroph_cnt))
        allocate(ind%auto_agg_zint(autotroph_cnt))
        allocate(ind%auto_agg_zint_100m(autotroph_cnt))
      end if
      do n=1,autotroph_cnt
        lname = trim(autotroph_settings(n)%lname) // ' N Limitation, Surface'
        sname = trim(autotroph_settings(n)%sname) // '_N_lim_surf'
        units = '1'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%N_lim_surf(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' N Limitation, carbon biomass weighted average over 0-100m'
        sname = trim(autotroph_settings(n)%sname) // '_N_lim_Cweight_avg_100m'
        units = '1'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%N_lim_Cweight_avg_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' P Limitation, Surface'
        sname = trim(autotroph_settings(n)%sname) // '_P_lim_surf'
        units = '1'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%P_lim_surf(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' P Limitation, carbon biomass weighted average over 0-100m'
        sname = trim(autotroph_settings(n)%sname) // '_P_lim_Cweight_avg_100m'
        units = '1'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%P_lim_Cweight_avg_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Fe Limitation, Surface'
        sname = trim(autotroph_settings(n)%sname) // '_Fe_lim_surf'
        units = '1'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%Fe_lim_surf(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Fe Limitation, carbon biomass weighted average over 0-100m'
        sname = trim(autotroph_settings(n)%sname) // '_Fe_lim_Cweight_avg_100m'
        units = '1'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%Fe_lim_Cweight_avg_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        if (autotroph_settings(n)%silicifier) then
          lname = trim(autotroph_settings(n)%lname) // ' SiO3 Limitation, Surface'
          sname = trim(autotroph_settings(n)%sname) // '_SiO3_lim_surf'
          units = '1'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
               ind%SiO3_lim_surf(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if

          lname = trim(autotroph_settings(n)%lname) // ' SiO3 Limitation, carbon biomass weighted average over 0-100m'
          sname = trim(autotroph_settings(n)%sname) // '_SiO3_lim_Cweight_avg_100m'
          units = '1'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
               ind%SiO3_lim_Cweight_avg_100m(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
        else
          ind%SiO3_lim_surf(n) = -1
          ind%SiO3_lim_Cweight_avg_100m(n) = -1
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Light Limitation, Surface'
        sname = trim(autotroph_settings(n)%sname) // '_light_lim_surf'
        units = '1'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%light_lim_surf(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Light Limitation, carbon biomass weighted average over 0-100m'
        sname = trim(autotroph_settings(n)%sname) // '_light_lim_Cweight_avg_100m'
        units = '1'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%light_lim_Cweight_avg_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' C Fixation Vertical Integral'
        sname = 'photoC_' // trim(autotroph_settings(n)%sname) // '_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%photoC_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' C Fixation Vertical Integral, 0-100m'
        sname = 'photoC_' // trim(autotroph_settings(n)%sname) // '_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%photoC_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' C Fixation from NO3 Vertical Integral'
        sname = 'photoC_NO3_' // trim(autotroph_settings(n)%sname) // '_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%photoC_NO3_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        if (autotroph_settings(n)%imp_calcifier .or. autotroph_settings(n)%exp_calcifier) then
          lname = trim(autotroph_settings(n)%lname) // ' CaCO3 Formation Vertical Integral'
          sname = trim(autotroph_settings(n)%sname) // '_CaCO3_form_zint'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
               ind%CaCO3_form_zint(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
        else
          ind%CaCO3_form_zint(n) = -1
        end if

        if (autotroph_settings(n)%imp_calcifier .or. autotroph_settings(n)%exp_calcifier) then
          lname = trim(autotroph_settings(n)%lname) // ' CaCO3 Formation Vertical Integral, 0-100m'
          sname = trim(autotroph_settings(n)%sname) // '_CaCO3_form_zint_100m'
          units = 'mmol/m^3 cm/s'
          vgrid = 'none'
          truncate = .false.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
               ind%CaCO3_form_zint_100m(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
        else
          ind%CaCO3_form_zint_100m(n) = -1
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing Vertical Integral'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing Vertical Integral, 0-100m'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to POC Vertical Integral'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_poc_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_poc_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to POC Vertical Integral, 0-100m'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_poc_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_poc_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to DOC Vertical Integral'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_doc_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_doc_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to DOC Vertical Integral, 0-100m'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_doc_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_doc_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to ZOO Vertical Integral'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_zoo_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_zoo_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to ZOO Vertical Integral, 0-100m'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_zoo_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_zoo_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss Vertical Integral'
        sname = trim(autotroph_settings(n)%sname) // '_loss_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss Vertical Integral, 0-100m'
        sname = trim(autotroph_settings(n)%sname) // '_loss_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss to POC Vertical Integral'
        sname = trim(autotroph_settings(n)%sname) // '_loss_poc_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss_poc_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss to POC Vertical Integral, 0-100m'
        sname = trim(autotroph_settings(n)%sname) // '_loss_poc_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss_poc_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss to DOC Vertical Integral'
        sname = trim(autotroph_settings(n)%sname) // '_loss_doc_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss_doc_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss to DOC Vertical Integral, 0-100m'
        sname = trim(autotroph_settings(n)%sname) // '_loss_doc_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss_doc_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Aggregation Vertical Integral'
        sname = trim(autotroph_settings(n)%sname) // '_agg_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_agg_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Aggregation Vertical Integral, 0-100m'
        sname = trim(autotroph_settings(n)%sname) // '_agg_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_agg_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if
      end do

      lname = 'Total CaCO3 Formation Vertical Integral'
      sname = 'CaCO3_form_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%tot_CaCO3_form_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Total CaCO3 Formation Vertical Integral, 0-100m'
      sname = 'CaCO3_form_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%tot_CaCO3_form_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      ! Zooplankton 2D diags
      if (.not.ind%lconstructed()) then
        allocate(ind%zoo_loss_zint(zooplankton_cnt))
        allocate(ind%zoo_loss_zint_100m(zooplankton_cnt))
        allocate(ind%zoo_loss_poc_zint(zooplankton_cnt))
        allocate(ind%zoo_loss_poc_zint_100m(zooplankton_cnt))
        allocate(ind%zoo_loss_doc_zint(zooplankton_cnt))
        allocate(ind%zoo_loss_doc_zint_100m(zooplankton_cnt))
        allocate(ind%zoo_graze_zint(zooplankton_cnt))
        allocate(ind%zoo_graze_zint_100m(zooplankton_cnt))
        allocate(ind%zoo_graze_poc_zint(zooplankton_cnt))
        allocate(ind%zoo_graze_poc_zint_100m(zooplankton_cnt))
        allocate(ind%zoo_graze_doc_zint(zooplankton_cnt))
        allocate(ind%zoo_graze_doc_zint_100m(zooplankton_cnt))
        allocate(ind%zoo_graze_zoo_zint(zooplankton_cnt))
        allocate(ind%zoo_graze_zoo_zint_100m(zooplankton_cnt))
        allocate(ind%x_graze_zoo_zint(zooplankton_cnt))
        allocate(ind%x_graze_zoo_zint_100m(zooplankton_cnt))
      end if
      do n = 1,zooplankton_cnt
        lname = trim(zooplankton_settings(n)%lname) // ' Loss Vertical Integral'
        sname = trim(zooplankton_settings(n)%sname) // '_loss_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Loss Vertical Integral, 0-100m'
        sname = trim(zooplankton_settings(n)%sname) // '_loss_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Loss to POC Vertical Integral'
        sname = trim(zooplankton_settings(n)%sname) // '_loss_poc_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss_poc_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Loss to POC Vertical Integral, 0-100m'
        sname = trim(zooplankton_settings(n)%sname) // '_loss_poc_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss_poc_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Loss to DOC Vertical Integral'
        sname = trim(zooplankton_settings(n)%sname) // '_loss_doc_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss_doc_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Loss to DOC Vertical Integral, 0-100m'
        sname = trim(zooplankton_settings(n)%sname) // '_loss_doc_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss_doc_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing Vertical Integral'
        sname = 'graze_' // trim(zooplankton_settings(n)%sname) // '_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing Vertical Integral, 0-100m'
        sname = 'graze_' // trim(zooplankton_settings(n)%sname) // '_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing to POC Vertical Integral'
        sname = 'graze_' // trim(zooplankton_settings(n)%sname) // '_poc_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_poc_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing to POC Vertical Integral, 0-100m'
        sname = 'graze_' // trim(zooplankton_settings(n)%sname) // '_poc_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_poc_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing to DOC Vertical Integral'
        sname = 'graze_' // trim(zooplankton_settings(n)%sname) // '_doc_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_doc_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing to DOC Vertical Integral, 0-100m'
        sname = 'graze_' // trim(zooplankton_settings(n)%sname) // '_doc_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_doc_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing to ZOO Vertical Integral'
        sname = 'graze_' // trim(zooplankton_settings(n)%sname) // '_zoo_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_zoo_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing to ZOO Vertical Integral, 0-100m'
        sname = 'graze_' // trim(zooplankton_settings(n)%sname) // '_zoo_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_zoo_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing Gain Vertical Integral'
        sname = 'x_graze_' // trim(zooplankton_settings(n)%sname) // '_zint'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%x_graze_zoo_zint(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(zooplankton_settings(n)%lname) // ' Grazing Gain Vertical Integral, 0-100m'
        sname = 'x_graze_' // trim(zooplankton_settings(n)%sname) // '_zint_100m'
        units = 'mmol/m^3 cm/s'
        vgrid = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%x_graze_zoo_zint_100m(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if
      end do

      ! General 3D diags
      lname = 'in situ temperature'
      sname = 'insitu_temp'
      units = 'degC'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%insitu_temp, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Carbonate Ion Concentration'
      sname = 'CO3'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CO3, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Bicarbonate Ion Concentration'
      sname = 'HCO3'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%HCO3, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Carbonic Acid Concentration'
      sname = 'H2CO3'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%H2CO3, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'pH'
      sname = 'pH_3D'
      units = '1'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%ph_3D, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Carbonate Ion Concentration, Alternative CO2'
      sname = 'CO3_ALT_CO2'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CO3_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Bicarbonate Ion Concentration, Alternative CO2'
      sname = 'HCO3_ALT_CO2'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%HCO3_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Carbonic Acid Concentration, Alternative CO2'
      sname = 'H2CO3_ALT_CO2'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%H2CO3_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'pH, Alternative CO2'
      sname = 'pH_3D_ALT_CO2'
      units = '1'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%ph_3D_ALT_CO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CO3 concentration at calcite saturation'
      sname = 'co3_sat_calc'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%co3_sat_calc, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CO3 concentration at aragonite saturation'
      sname = 'co3_sat_arag'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%co3_sat_arag, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Nitrification'
      sname = 'NITRIF'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%NITRIF, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Denitrification'
      sname = 'DENITRIF'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DENITRIF, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'O2 Production'
      sname = 'O2_PRODUCTION'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%O2_PRODUCTION, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      if (lo2_consumption_scalef) then
        lname = 'O2 Consumption Scale Factor'
        sname = 'O2_CONSUMPTION_SCALEF'
        units = '1'
        vgrid = 'layer_avg'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
             ind%O2_CONSUMPTION_SCALEF, marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if
      end if

      lname = 'O2 Consumption'
      sname = 'O2_CONSUMPTION'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%O2_CONSUMPTION, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Apparent O2 Utilization'
      sname = 'AOU'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%AOU, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'PAR Average over Model Cell'
      sname = 'PAR_avg'
      units = 'W/m^2'
      vgrid = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%PAR_avg, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Total Autotroph Grazing'
      sname = 'graze_auto_TOT'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%auto_graze_TOT, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Total C Fixation'
      sname = 'photoC_TOT'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%photoC_TOT, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Total C Fixation from NO3'
      sname = 'photoC_NO3_TOT'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%photoC_NO3_TOT, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DOC Production'
      sname = 'DOC_prod'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOC_prod, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DOC Remineralization'
      sname = 'DOC_remin'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOC_remin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DOCr Remineralization'
      sname = 'DOCr_remin'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOCr_remin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DON Production'
      sname = 'DON_prod'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DON_prod, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DON Remineralization'
      sname = 'DON_remin'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DON_remin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DONr Remineralization'
      sname = 'DONr_remin'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DONr_remin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DOP Production'
      sname = 'DOP_prod'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOP_prod, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DOP Remineralization'
      sname = 'DOP_remin'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOP_remin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DOPr Remineralization'
      sname = 'DOPr_remin'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOPr_remin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'DOP loss, due to P budget balancing'
      sname = 'DOP_loss_P_bal'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%DOP_loss_P_bal, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Iron Scavenging'
      sname = 'Fe_scavenge'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Fe_scavenge, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Iron Scavenging Rate'
      sname = 'Fe_scavenge_rate'
      units = '1/y'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Fe_scavenge_rate, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Production of Fe-binding Ligand'
      sname = 'Lig_prod'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Lig_prod, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Loss of Fe-binding Ligand'
      sname = 'Lig_loss'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Lig_loss, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Loss of Fe-binding Ligand from Scavenging'
      sname = 'Lig_scavenge'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Lig_scavenge, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Fe not bound to Ligand'
      sname = 'Fefree'
      units = 'mmol/m^3'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Fefree, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Loss of Fe-binding Ligand from UV radiation'
      sname = 'Lig_photochem'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Lig_photochem, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Loss of Fe-binding Ligand from Bacterial Degradation'
      sname = 'Lig_deg'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%Lig_deg, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Iron Sediment Flux'
      sname = 'FESEDFLUX'
      units = 'nmol/cm^2/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%fesedflux, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      ! Particulate 2D diags

      write(particulate_flux_ref_depth_str, "(I0,A)") particulate_flux_ref_depth, 'm'

      lname = 'POC Flux at ' // trim(particulate_flux_ref_depth_str)
      sname = 'POC_FLUX_' // trim(particulate_flux_ref_depth_str)
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_FLUX_at_ref_depth, marbl_status_log, ref_depth=particulate_flux_ref_depth)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POP Flux at ' // trim(particulate_flux_ref_depth_str)
      sname = 'POP_FLUX_' // trim(particulate_flux_ref_depth_str)
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POP_FLUX_at_ref_depth, marbl_status_log, ref_depth=particulate_flux_ref_depth)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Flux at ' // trim(particulate_flux_ref_depth_str)
      sname = 'CaCO3_FLUX_' // trim(particulate_flux_ref_depth_str)
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_FLUX_at_ref_depth, marbl_status_log, ref_depth=particulate_flux_ref_depth)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'SiO2 Flux at ' // trim(particulate_flux_ref_depth_str)
      sname = 'SiO2_FLUX_' // trim(particulate_flux_ref_depth_str)
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%SiO2_FLUX_at_ref_depth, marbl_status_log, ref_depth=particulate_flux_ref_depth)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'P_iron Flux at ' // trim(particulate_flux_ref_depth_str)
      sname = 'P_iron_FLUX_' // trim(particulate_flux_ref_depth_str)
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%P_iron_FLUX_at_ref_depth, marbl_status_log, ref_depth=particulate_flux_ref_depth)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of POC Production'
      sname = 'POC_PROD_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_PROD_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of POC Production, 0-100m'
      sname = 'POC_PROD_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_PROD_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of POC Remineralization routed to DOCr'
      sname = 'POC_REMIN_DOCr_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_REMIN_DOCr_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of POC Remineralization routed to DOCr, 0-100m'
      sname = 'POC_REMIN_DOCr_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_REMIN_DOCr_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of POC Remineralization routed to DIC'
      sname = 'POC_REMIN_DIC_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_REMIN_DIC_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of POC Remineralization routed to DIC, 0-100m'
      sname = 'POC_REMIN_DIC_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_REMIN_DIC_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of CaCO3 Production'
      sname = 'CaCO3_PROD_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_PROD_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of CaCO3 Production, 0-100m'
      sname = 'CaCO3_PROD_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_PROD_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of CaCO3 Remineralization'
      sname = 'CaCO3_REMIN_zint'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_REMIN_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Vertical Integral of CaCO3 Remineralization, 0-100m'
      sname = 'CaCO3_REMIN_zint_100m'
      units = 'mmol/m^3 cm/s'
      vgrid = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_REMIN_zint_100m, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      ! Particulate 3D diags
      if (lp_remin_scalef) then
        lname = 'Particulate Remin Scale Factor'
        sname = 'P_REMIN_SCALEF'
        units = '1'
        vgrid = 'layer_avg'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
             ind%P_REMIN_SCALEF, marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if
      end if

      lname = 'POC Flux into Cell'
      sname = 'POC_FLUX_IN'
      units = 'mmol/m^3 cm/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POC sFlux into Cell'
      sname = 'POC_sFLUX_IN'
      units = 'mmol/m^3 cm/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_sFLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POC hFlux into Cell'
      sname = 'POC_hFLUX_IN'
      units = 'mmol/m^3 cm/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_hFLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POC Production'
      sname = 'POC_PROD'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POC Remineralization routed to DOCr'
      sname = 'POC_REMIN_DOCr'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_REMIN_DOCr, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POC Remineralization routed to DIC'
      sname = 'POC_REMIN_DIC'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POC_REMIN_DIC, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POP Flux into Cell'
      sname = 'POP_FLUX_IN'
      units = 'mmol/m^3 cm/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POP_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POP Production'
      sname = 'POP_PROD'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POP_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POP Remineralization routed to DOPr'
      sname = 'POP_REMIN_DOPr'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POP_REMIN_DOPr, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'POP Remineralization routed to PO4'
      sname = 'POP_REMIN_PO4'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%POP_REMIN_PO4, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'PON Remineralization routed to DONr'
      sname = 'PON_REMIN_DONr'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%PON_REMIN_DONr, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'PON Remineralization routed to NH4'
      sname = 'PON_REMIN_NH4'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%PON_REMIN_NH4, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Flux into Cell'
      sname = 'CaCO3_FLUX_IN'
      units = 'mmol/m^3 cm/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Production'
      sname = 'CaCO3_PROD'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Remineralization'
      sname = 'CaCO3_REMIN'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Flux into Cell, Alternative CO2'
      sname = 'CaCO3_ALT_CO2_FLUX_IN'
      units = 'mmol/m^3 cm/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_ALT_CO2_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Production, Alternative CO2'
      sname = 'CaCO3_ALT_CO2_PROD'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_ALT_CO2_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'CaCO3 Remineralization, Alternative CO2'
      sname = 'CaCO3_ALT_CO2_REMIN'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%CaCO3_ALT_CO2_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'SiO2 Flux into Cell'
      sname = 'SiO2_FLUX_IN'
      units = 'mmol/m^3 cm/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%SiO2_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'SiO2 Production'
      sname = 'SiO2_PROD'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%SiO2_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'SiO2 Remineralization'
      sname = 'SiO2_REMIN'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%SiO2_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Dust Flux into Cell'
      sname = 'dust_FLUX_IN'
      units = 'g/cm^2/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%dust_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'Dust Remineralization'
      sname = 'dust_REMIN'
      units = 'g/cm^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%dust_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'P_iron Flux into Cell'
      sname = 'P_iron_FLUX_IN'
      units = 'mmol/m^3 cm/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%P_iron_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'P_iron Production'
      sname = 'P_iron_PROD'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%P_iron_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname = 'P_iron Remineralization'
      sname = 'P_iron_REMIN'
      units = 'mmol/m^3/s'
      vgrid = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%P_iron_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      ! Autotroph 3D diags
      if (.not.ind%lconstructed()) then
        allocate(ind%Qp(autotroph_cnt))
        allocate(ind%photoC(autotroph_cnt))
        allocate(ind%photoC_NO3(autotroph_cnt))
        allocate(ind%photoFe(autotroph_cnt))
        allocate(ind%photoNO3(autotroph_cnt))
        allocate(ind%photoNH4(autotroph_cnt))
        allocate(ind%DOP_uptake(autotroph_cnt))
        allocate(ind%PO4_uptake(autotroph_cnt))
        allocate(ind%auto_graze(autotroph_cnt))
        allocate(ind%auto_graze_poc(autotroph_cnt))
        allocate(ind%auto_graze_doc(autotroph_cnt))
        allocate(ind%auto_graze_zoo(autotroph_cnt))
        allocate(ind%auto_loss(autotroph_cnt))
        allocate(ind%auto_loss_poc(autotroph_cnt))
        allocate(ind%auto_loss_doc(autotroph_cnt))
        allocate(ind%auto_agg(autotroph_cnt))
        allocate(ind%bSi_form(autotroph_cnt))
        allocate(ind%CaCO3_form(autotroph_cnt))
        allocate(ind%Nfix(autotroph_cnt))
      end if
      do n=1,autotroph_cnt
        if (lvariable_PtoC) then
          lname = trim(autotroph_settings(n)%lname) // ' P:C ratio'
          sname = trim(autotroph_settings(n)%sname) // '_Qp'
          units = '1'
          vgrid = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
               ind%Qp(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
        else
          ind%Qp(n) = -1
        end if

        lname = trim(autotroph_settings(n)%lname) // ' C Fixation'
        sname = 'photoC_' // trim(autotroph_settings(n)%sname)
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%photoC(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' C Fixation from NO3'
        sname = 'photoC_NO3_' // trim(autotroph_settings(n)%sname)
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%photoC_NO3(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Fe Uptake'
        sname = 'photoFe_' // trim(autotroph_settings(n)%sname)
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%photoFe(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' NO3 Uptake'
        sname = 'photoNO3_' // trim(autotroph_settings(n)%sname)
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%photoNO3(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' NH4 Uptake'
        sname = 'photoNH4_' // trim(autotroph_settings(n)%sname)
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%photoNH4(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' DOP Uptake'
        sname = 'DOP_' // trim(autotroph_settings(n)%sname) // '_uptake'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%DOP_uptake(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' PO4 Uptake'
        sname = 'PO4_' // trim(autotroph_settings(n)%sname) // '_uptake'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%PO4_uptake(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing'
        sname = 'graze_' // trim(autotroph_settings(n)%sname)
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to POC'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_poc'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_poc(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to DOC'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_doc'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_doc(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Grazing to ZOO'
        sname = 'graze_' // trim(autotroph_settings(n)%sname) // '_zoo'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_graze_zoo(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss'
        sname = trim(autotroph_settings(n)%sname) // '_loss'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss to POC'
        sname = trim(autotroph_settings(n)%sname) // '_loss_poc'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss_poc(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Loss to DOC'
        sname = trim(autotroph_settings(n)%sname) // '_loss_doc'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_loss_doc(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname = trim(autotroph_settings(n)%lname) // ' Aggregation'
        sname = trim(autotroph_settings(n)%sname) // '_agg'
        units = 'mmol/m^3/s'
        vgrid = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%auto_agg(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        if (autotroph_settings(n)%silicifier) then
          lname = trim(autotroph_settings(n)%lname) // ' Si Uptake'
          sname = trim(autotroph_settings(n)%sname) // '_bSi_form'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
               ind%bSi_form(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
        else
          ind%bSi_form(n) = -1
        end if

        if (autotroph_settings(n)%imp_calcifier .or. autotroph_settings(n)%exp_calcifier) then
          lname = trim(autotroph_settings(n)%lname) // ' CaCO3 Formation'
          sname = trim(autotroph_settings(n)%sname) // '_CaCO3_form'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
               ind%CaCO3_form(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
        else
          ind%CaCO3_form(n) = -1
        end if

        if (autotroph_settings(n)%Nfixer) then
          lname = trim(autotroph_settings(n)%lname) // ' N Fixation'
          sname = trim(autotroph_settings(n)%sname) // '_Nfix'
          units = 'mmol/m^3/s'
          vgrid = 'layer_avg'
          truncate = .true.
          call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
               ind%Nfix(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
        else
          ind%Nfix(n) = -1
        end if

      end do ! end do-loop for autotroph_cnt

      lname    = 'Total Si Uptake'
      sname    = 'bSi_form'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%tot_bSi_form, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Total CaCO3 Formation'
      sname    = 'CaCO3_form'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%tot_CaCO3_form, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      lname    = 'Total N Fixation'
      sname    = 'Nfix'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,     &
           ind%tot_Nfix, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
        return
      end if

      ! Zooplankton 3D diags
      if (.not.ind%lconstructed()) then
        allocate(ind%zoo_loss(zooplankton_cnt))
        allocate(ind%zoo_loss_poc(zooplankton_cnt))
        allocate(ind%zoo_loss_doc(zooplankton_cnt))
        allocate(ind%zoo_graze(zooplankton_cnt))
        allocate(ind%zoo_graze_poc(zooplankton_cnt))
        allocate(ind%zoo_graze_doc(zooplankton_cnt))
        allocate(ind%zoo_graze_zoo(zooplankton_cnt))
        allocate(ind%x_graze_zoo(zooplankton_cnt))
      end if
      do n = 1,zooplankton_cnt
        lname    = trim(zooplankton_settings(n)%lname) // ' Loss'
        sname    = trim(zooplankton_settings(n)%sname) // '_loss'
        units    = 'mmol/m^3/s'
        vgrid    = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname    = trim(zooplankton_settings(n)%lname) // ' Loss to POC'
        sname    = trim(zooplankton_settings(n)%sname) // '_loss_poc'
        units    = 'mmol/m^3/s'
        vgrid    = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss_poc(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname    = trim(zooplankton_settings(n)%lname) // ' Loss to DOC'
        sname    = trim(zooplankton_settings(n)%sname) // '_loss_doc'
        units    = 'mmol/m^3/s'
        vgrid    = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_loss_doc(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname    = trim(zooplankton_settings(n)%lname) // ' grazing loss'
        sname    = 'graze_' // trim(zooplankton_settings(n)%sname)
        units    = 'mmol/m^3/s'
        vgrid    = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname    = trim(zooplankton_settings(n)%lname) // ' grazing loss to POC'
        sname    = 'graze_' // trim(zooplankton_settings(n)%sname) // '_poc'
        units    = 'mmol/m^3/s'
        vgrid    = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_poc(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname    = trim(zooplankton_settings(n)%lname) // ' grazing loss to DOC'
        sname    = 'graze_' // trim(zooplankton_settings(n)%sname) // '_doc'
        units    = 'mmol/m^3/s'
        vgrid    = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_doc(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname    = trim(zooplankton_settings(n)%lname) // ' grazing loss to ZOO'
        sname    = 'graze_' // trim(zooplankton_settings(n)%sname) // '_zoo'
        units    = 'mmol/m^3/s'
        vgrid    = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%zoo_graze_zoo(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname    = trim(zooplankton_settings(n)%lname) // ' grazing gain'
        sname    = 'x_graze_' // trim(zooplankton_settings(n)%sname)
        units    = 'mmol/m^3/s'
        vgrid    = 'layer_avg'
        truncate = .true.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%x_graze_zoo(n), marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

      end do

      !-----------------------------------------------------------------
      ! CISO diagnostics
      !-----------------------------------------------------------------

      call marbl_ciso_diagnostics_init(marbl_interior_tendency_diags, marbl_surface_flux_diags, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace("marbl_ciso_diagnostics_init()", subname)
        return
      end if

       !-----------------------------------------------------------------
       ! Restoring diagnostics
       !-----------------------------------------------------------------

       ! only allocate this component of ind once
       ! within a single task, it is shared across different instances of MARBL
       ! FIXME #60: this approach is not thread-safe
       !    i.e. if this is called from 2 threads simulatneously, a race condition
       !    on the allocation status check and allocation is introduced
       if (.not.ind%lconstructed()) then
          allocate(ind%restore_tend(marbl_tracer_indices%total_cnt))
       end if

       do n = 1,marbl_tracer_indices%total_cnt
          ! restoring tendency
          lname = trim(marbl_tracer_metadata(n)%long_name) // " Restoring Tendency"
          sname = trim(marbl_tracer_metadata(n)%short_name) // "_RESTORE_TEND"
          units = trim(marbl_tracer_metadata(n)%tend_units)
          vgrid = 'layer_avg'
          call diags%add_diagnostic(lname, sname, units, vgrid, .false.,   &
               ind%restore_tend(n), marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
            return
          end if
       end do

    end associate

    !-----------------------------------------------------------------
    ! Initialize all diagnostics to zero
    !-----------------------------------------------------------------

    call marbl_interior_tendency_diags%set_to_zero(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace(&
           'marbl_interior_tendency_diags%set_to_zero', subname)
      return
    end if
    call marbl_surface_flux_diags%set_to_zero(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace(&
           'marbl_surface_flux_diags%set_to_zero', subname)
      return
    end if

  end subroutine marbl_diagnostics_init

  !***********************************************************************

  subroutine marbl_diagnostics_interior_tendency_compute( &
       domain,                                        &
       interior_tendency_forcing_ind,                 &
       interior_tendency_forcings,                    &
       temperature,                                   &
       interior_tendencies,                           &
       marbl_tracer_indices,                          &
       carbonate,                                     &
       autotroph_local,                               &
       autotroph_derived_terms,                       &
       zooplankton_derived_terms,                     &
       dissolved_organic_matter,                      &
       marbl_particulate_share,                       &
       PAR,                                           &
       PON_remin, PON_sed_loss,                       &
       sed_denitrif, other_remin, nitrif, denitrif,   &
       column_o2, o2_production, o2_consumption,      &
       fe_scavenge, fe_scavenge_rate,                 &
       Lig_prod, Lig_loss, Lig_scavenge, Fefree,      &
       Lig_photochem, Lig_deg,                        &
       interior_restore,                              &
       marbl_interior_tendency_diags,                 &
       marbl_status_log)

    use marbl_interface_private_types , only : marbl_interior_tendency_forcing_indexing_type

    type (marbl_domain_type),                            intent(in)    :: domain
    type(marbl_interior_tendency_forcing_indexing_type), intent(in)    :: interior_tendency_forcing_ind
    type(marbl_forcing_fields_type),                     intent(in)    :: interior_tendency_forcings(:)
    real (r8),                                           intent(in)    :: temperature(domain%km) ! in situ temperature
    real(r8),                                            intent(in)    :: interior_tendencies(:,:) ! (tracer_cnt, km) computed source/sink terms
    type(marbl_tracer_index_type),                       intent(in)    :: marbl_tracer_indices
    type (carbonate_type),                               intent(in)    :: carbonate
    type (autotroph_local_type),                         intent(in)    :: autotroph_local
    type (autotroph_derived_terms_type),                 intent(in)    :: autotroph_derived_terms
    type (zooplankton_derived_terms_type),               intent(in)    :: zooplankton_derived_terms
    type (dissolved_organic_matter_type),                intent(in)    :: dissolved_organic_matter
    type (marbl_particulate_share_type),                 intent(in)    :: marbl_particulate_share
    type (marbl_PAR_type),                               intent(in)    :: PAR
    real (r8),                                           intent(in)    :: PON_remin(domain%km)        ! remin of PON
    real (r8),                                           intent(in)    :: PON_sed_loss(domain%km)     ! loss of PON to sediments
    real (r8),                                           intent(in)    :: sed_denitrif(domain%km)     ! sedimentary denitrification (nmol N/cm^3/sec)
    real (r8),                                           intent(in)    :: other_remin(domain%km)      ! organic C remin not due oxic or denitrif (nmolC/cm^3/sec)
    real (r8),                                           intent(in)    :: nitrif(domain%km)           ! nitrification (NH4 -> NO3) (mmol N/m^3/sec)
    real (r8),                                           intent(in)    :: denitrif(domain%km)         ! WC nitrification (NO3 -> N2) (mmol N/m^3/sec)
    real (r8),                                           intent(in)    :: column_o2(:)
    real (r8),                                           intent(in)    :: o2_production(:)
    real (r8),                                           intent(in)    :: o2_consumption(:)
    real (r8),                                           intent(in)    :: fe_scavenge_rate(domain%km) ! annual scavenging rate of iron as % of ambient
    real (r8),                                           intent(in)    :: fe_scavenge(domain%km)      ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
    real (r8),                                           intent(in)    :: Lig_prod(domain%km)
    real (r8),                                           intent(in)    :: Lig_loss(domain%km)
    real (r8),                                           intent(in)    :: Lig_scavenge(domain%km)
    real (r8),                                           intent(in)    :: Fefree(domain%km)
    real (r8),                                           intent(in)    :: Lig_photochem(domain%km)
    real (r8),                                           intent(in)    :: Lig_deg(domain%km)
    real (r8),                                           intent(in)    :: interior_restore(:,:)       ! (tracer_cnt, km) local restoring terms for nutrients (mmol ./m^3/sec)
    type (marbl_diagnostics_type),                       intent(inout) :: marbl_interior_tendency_diags
    type (marbl_log_type),                               intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:marbl_diagnostics_interior_tendency_compute'

    !-----------------------------------------------------------------

    call marbl_interior_tendency_diags%set_to_zero(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace(&
           'marbl_interior_tendency_diags%set_to_zero', subname)
      return
    end if

    associate( &
         kmt   => domain%kmt, &
         diags => marbl_interior_tendency_diags%diags, &
         ind   => marbl_interior_tendency_diag_ind &
         )
    diags(ind%insitu_temp)%field_3d(1:kmt, 1) = temperature(1:kmt)
    end associate

    call store_diagnostics_carbonate(domain, carbonate,                       &
         marbl_interior_tendency_diags, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('store_diagnostics_carbonate', subname)
      return
    end if

    call store_diagnostics_autotrophs(domain, autotroph_local, autotroph_derived_terms, marbl_interior_tendency_diags)

    call store_diagnostics_zooplankton(domain, zooplankton_derived_terms, marbl_interior_tendency_diags)

    call store_diagnostics_particulates(domain, &
         interior_tendency_forcing_ind, interior_tendency_forcings, &
         marbl_particulate_share, &
         PON_remin, PON_sed_loss, &
         sed_denitrif, other_remin, marbl_interior_tendency_diags)

    associate( POC     => marbl_particulate_share%POC, &
               P_CaCO3 => marbl_particulate_share%P_CaCO3 )
    call store_diagnostics_carbon_fluxes(domain, POC, P_CaCO3, interior_tendencies, &
         marbl_tracer_indices, marbl_interior_tendency_diags, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('store_diagnostics_carbon_fluxes', subname)
      return
    end if
    end associate

    call store_diagnostics_nitrification(&
         nitrif, denitrif, marbl_interior_tendency_diags)

    call store_diagnostics_oxygen(domain, &
         interior_tendency_forcing_ind, interior_tendency_forcings, &
         interior_tendency_forcings(interior_tendency_forcing_ind%potemp_id)%field_1d(1,:), &
         interior_tendency_forcings(interior_tendency_forcing_ind%salinity_id)%field_1d(1,:), &
         column_o2, o2_production, o2_consumption, marbl_interior_tendency_diags)

    call store_diagnostics_PAR(domain, &
         PAR%col_frac(:), PAR%avg(:,:), marbl_interior_tendency_diags)

    call store_diagnostics_dissolved_organic_matter(domain, &
         dissolved_organic_matter, marbl_interior_tendency_diags)

    call store_diagnostics_iron_cycle( &
         fe_scavenge, fe_scavenge_rate, Lig_prod, Lig_loss, Lig_scavenge, &
         Fefree, Lig_photochem, Lig_deg, marbl_interior_tendency_diags)

    call store_diagnostics_nitrogen_fluxes(domain, &
         PON_sed_loss, denitrif, sed_denitrif, autotroph_derived_terms, interior_tendencies, &
         marbl_tracer_indices, marbl_interior_tendency_diags, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('store_diagnostics_nitrogen_fluxes', subname)
      return
    end if

    associate( POP => marbl_particulate_share%POP )
    call store_diagnostics_phosphorus_fluxes(domain, POP, &
         autotroph_derived_terms, interior_tendencies, &
         marbl_tracer_indices, marbl_interior_tendency_diags, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('store_diagnostics_phosphorus_fluxes', subname)
      return
    end if
    end associate

    associate( P_SiO2 => marbl_particulate_share%P_SiO2 )
    call store_diagnostics_silicon_fluxes(domain, P_SiO2, interior_tendencies,           &
         marbl_tracer_indices, marbl_interior_tendency_diags, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('store_diagnostics_silicon_fluxes', subname)
      return
    end if
    end associate

    associate( dust   => marbl_particulate_share%dust, &
               P_iron => marbl_particulate_share%P_iron )
    call store_diagnostics_iron_fluxes(domain, P_iron, dust,                  &
         interior_tendency_forcings(interior_tendency_forcing_ind%fesedflux_id)%field_1d(1,:),  &
         interior_tendencies, marbl_tracer_indices, marbl_interior_tendency_diags, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('store_diagnostics_iron_fluxes', subname)
      return
    end if
    end associate

    call store_diagnostics_interior_restore(interior_restore,                 &
                                            marbl_interior_tendency_diags)

  end subroutine marbl_diagnostics_interior_tendency_compute

  !***********************************************************************

  subroutine marbl_diagnostics_surface_flux_compute( &
       surface_flux_forcing_ind,                    &
       surface_flux_forcings,                       &
       surface_flux_internal,                       &
       marbl_tracer_indices,                        &
       saved_state,                                 &
       saved_state_ind,                             &
       surface_flux_diags)

    ! !DESCRIPTION:
    !  Compute surface fluxes for ecosys tracer module.

    use marbl_interface_private_types , only : marbl_surface_flux_forcing_indexing_type
    use marbl_interface_private_types , only : marbl_surface_flux_saved_state_indexing_type
    use marbl_settings_mod   , only : lflux_gas_o2
    use marbl_settings_mod   , only : lflux_gas_co2
    use marbl_constants_mod  , only : mpercm

    type(marbl_surface_flux_forcing_indexing_type), intent(in) :: surface_flux_forcing_ind
    type(marbl_forcing_fields_type)           , intent(in)    :: surface_flux_forcings(:)
    type(marbl_tracer_index_type)             , intent(in)    :: marbl_tracer_indices
    type(marbl_saved_state_type)              , intent(in)    :: saved_state
    type(marbl_surface_flux_saved_state_indexing_type), intent(in) :: saved_state_ind
    type(marbl_surface_flux_internal_type)    , intent(in)    :: surface_flux_internal
    type(marbl_diagnostics_type)              , intent(inout) :: surface_flux_diags

    associate(                                                                                  &
         ind_diag          => marbl_surface_flux_diag_ind,                                      &

         diags             => surface_flux_diags%diags(:),                                              &
         xco2              => surface_flux_forcings(surface_flux_forcing_ind%xco2_id)%field_0d,         &
         xco2_alt_co2      => surface_flux_forcings(surface_flux_forcing_ind%xco2_alt_co2_id)%field_0d, &
         ap_used           => surface_flux_forcings(surface_flux_forcing_ind%atm_pressure_id)%field_0d, &
         ifrac             => surface_flux_forcings(surface_flux_forcing_ind%ifrac_id)%field_0d,        &
         dust_flux_in      => surface_flux_forcings(surface_flux_forcing_ind%dust_flux_id)%field_0d,    &
         iron_flux_in      => surface_flux_forcings(surface_flux_forcing_ind%iron_flux_id)%field_0d,    &
         nox_flux          => surface_flux_forcings(surface_flux_forcing_ind%nox_flux_id)%field_0d,     &
         nhy_flux          => surface_flux_forcings(surface_flux_forcing_ind%nhy_flux_id)%field_0d,     &

         piston_velocity   => surface_flux_internal%piston_velocity,                         &
         flux_co2          => surface_flux_internal%flux_co2,                                &
         flux_alt_co2      => surface_flux_internal%flux_alt_co2,                            &
         co2star           => surface_flux_internal%co2star,                                 &
         dco2star          => surface_flux_internal%dco2star,                                &
         pco2surf          => surface_flux_internal%pco2surf,                                &
         dpco2             => surface_flux_internal%dpco2,                                   &
         co2star_alt       => surface_flux_internal%co2star_alt,                             &
         dco2star_alt      => surface_flux_internal%dco2star_alt,                            &
         pco2surf_alt      => surface_flux_internal%pco2surf_alt,                            &
         dpco2_alt         => surface_flux_internal%dpco2_alt,                               &
         pv_co2            => surface_flux_internal%pv_co2,                                  &
         pv_o2             => surface_flux_internal%pv_o2,                                   &
         schmidt_co2       => surface_flux_internal%schmidt_co2,                             &
         schmidt_o2        => surface_flux_internal%schmidt_o2,                              &
         o2sat             => surface_flux_internal%o2sat,                                   &
         nhx_surface_emis  => surface_flux_internal%nhx_surface_emis,                        &


         ph_prev           => saved_state%state(saved_state_ind%ph_surf)%field_2d,              &
         ph_prev_alt_co2   => saved_state%state(saved_state_ind%ph_alt_co2_surf)%field_2d,      &

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

    !-----------------------------------------------------------------------
    !  calculate gas flux quantities if necessary
    !-----------------------------------------------------------------------

    if (lflux_gas_o2 .or. lflux_gas_co2) then

       diags(ind_diag%ECOSYS_IFRAC)%field_2d(:)     = ifrac(:)
       diags(ind_diag%ECOSYS_XKW)%field_2d(:)       = piston_velocity(:)
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
       diags(ind_diag%DIC_GAS_FLUX)%field_2d(:)         = flux_co2(:)
       diags(ind_diag%PH)%field_2d(:)                   = ph_prev(:)
       diags(ind_diag%ATM_CO2)%field_2d(:)              = xco2(:)

       diags(ind_diag%DIC_GAS_FLUX_ALT_CO2)%field_2d(:) = flux_alt_co2(:)
       diags(ind_diag%PH_ALT_CO2)%field_2d(:)           = ph_prev_alt_co2(:)
       diags(ind_diag%ATM_ALT_CO2)%field_2d(:)          = xco2_alt_co2(:)

    endif  !  lflux_gas_co2

    !-----------------------------------------------------------------------
    !  calculate nox and nhy fluxes and nhx emissions
    !-----------------------------------------------------------------------

    diags(ind_diag%NOx_FLUX)%field_2d(:) = nox_flux
    diags(ind_diag%NHy_FLUX)%field_2d(:) = nhy_flux
    diags(ind_diag%NHx_SURFACE_EMIS)%field_2d(:) = nhx_surface_emis(:)

    !-----------------------------------------------------------------------
    !  calculate dust flux and, if necessary, iron flux
    !-----------------------------------------------------------------------

    diags(ind_diag%DUST_FLUX)%field_2d(:) = DUST_FLUX_IN(:)
    ! multiply IRON flux by mpercm (.01) to convert from model units (cm/s)(mmol/m^3) to mmol/s/m^2
    if (ind_diag%IRON_FLUX.ne.0) then
       diags(ind_diag%IRON_FLUX)%field_2d(:) = iron_flux_in(:) * mpercm
    endif

    end associate

  end subroutine marbl_diagnostics_surface_flux_compute

  !***********************************************************************

  subroutine store_diagnostics_carbonate(marbl_domain, carbonate,             &
             marbl_interior_diags, marbl_status_log)

    type(marbl_domain_type)      , intent(in)    :: marbl_domain
    type(carbonate_type)         , intent(in)    :: carbonate
    type(marbl_diagnostics_type) , intent(inout) :: marbl_interior_diags
    type(marbl_log_type)         , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:store_diagnostics_carbonate'
    !-----------------------------------------------------------------------

    associate(                                                  &
         km                => marbl_domain%km,                  &
         diags             => marbl_interior_diags%diags,       &
         ind               => marbl_interior_tendency_diag_ind, &
         CO3               => carbonate%CO3(:),                 &
         CO3_sat_calcite   => carbonate%CO3_sat_calcite(:),     &
         CO3_sat_aragonite => carbonate%CO3_sat_aragonite(:)    &
         )

    ! Find depth where CO3 = CO3_sat_calcite or CO3_sat_argonite
    diags(ind%zsatcalc)%field_2d(1) =  compute_saturation_depth(marbl_domain, &
                                               CO3, CO3_sat_calcite,          &
                                               marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('compute_sat_depth (calcite)',    &
           subname)
      return
    end if
    diags(ind%zsatarag)%field_2d(1) =  compute_saturation_depth(marbl_domain, &
                                               CO3, CO3_sat_aragonite,        &
                                               marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('compute_sat_depth (aragonite)',  &
           subname)
      return
    end if

    diags(ind%CO3)%field_3d(:, 1)           = carbonate%CO3(:)
    diags(ind%HCO3)%field_3d(:, 1)          = carbonate%HCO3(:)
    diags(ind%H2CO3)%field_3d(:, 1)         = carbonate%H2CO3(:)
    diags(ind%pH_3D)%field_3d(:, 1)         = carbonate%pH(:)
    diags(ind%CO3_ALT_CO2)%field_3d(:, 1)   = carbonate%CO3_ALT_CO2(:)
    diags(ind%HCO3_ALT_CO2)%field_3d(:, 1)  = carbonate%HCO3_ALT_CO2(:)
    diags(ind%H2CO3_ALT_CO2)%field_3d(:, 1) = carbonate%H2CO3_ALT_CO2(:)
    diags(ind%pH_3D_ALT_CO2)%field_3d(:, 1) = carbonate%pH_ALT_CO2(:)
    diags(ind%co3_sat_calc)%field_3d(:, 1)  = carbonate%CO3_sat_calcite(:)
    diags(ind%co3_sat_arag)%field_3d(:, 1)  = carbonate%CO3_sat_aragonite(:)

    end associate

  end subroutine store_diagnostics_carbonate

  !***********************************************************************

  function compute_saturation_depth(marbl_domain, CO3, sat_val, marbl_status_log)

    use marbl_utils_mod, only : marbl_utils_linear_root

    type(marbl_domain_type) , intent(in)    :: marbl_domain
    real(r8)                , intent(in)    :: CO3(:)
    real(r8)                , intent(in)    :: sat_val(:)
    type(marbl_log_type)    , intent(inout) :: marbl_status_log

    real(r8) :: compute_saturation_depth

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:compute_saturation_depth'

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
       compute_saturation_depth = marbl_utils_linear_root(zt(k-1:k), anomaly(k-1:k), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_status_log%log_error_trace('marbl_utils_linear_root', subname)
         return
       end if
    end if

    end associate

  end function compute_saturation_depth

  !***********************************************************************

  subroutine store_diagnostics_nitrification(nitrif, denitrif, marbl_interior_diags)

    real(r8)                     , intent(in)    :: nitrif(:)
    real(r8)                     , intent(in)    :: denitrif(:)
    type(marbl_diagnostics_type) , intent(inout) :: marbl_interior_diags

    associate(                                     &
         diags => marbl_interior_diags%diags,      &
         ind   => marbl_interior_tendency_diag_ind &
         )

    diags(ind%NITRIF)%field_3d(:, 1)   = nitrif
    diags(ind%DENITRIF)%field_3d(:, 1) = denitrif

    end associate

  end subroutine store_diagnostics_nitrification

  !***********************************************************************

  subroutine store_diagnostics_autotrophs(marbl_domain, &
       autotroph_local, autotroph_derived_terms, marbl_interior_diags)

    type(marbl_domain_type),            intent(in)    :: marbl_domain
    type(autotroph_local_type),         intent(in)    :: autotroph_local
    type(autotroph_derived_terms_type), intent(in)    :: autotroph_derived_terms
    type(marbl_diagnostics_type),       intent(inout) :: marbl_interior_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n
    real(r8) :: autotrophC_weight(marbl_domain%km)
    real(r8) :: autotrophC_zint_100m
    real(r8) :: limterm(marbl_domain%km)
    !-----------------------------------------------------------------------

    associate(                                        &
         diags   => marbl_interior_diags%diags,       &
         ind     => marbl_interior_tendency_diag_ind, &
         kmt     => marbl_domain%kmt,                 &
         delta_z => marbl_domain%delta_z              &
         )

    diags(ind%tot_bSi_form)%field_3d(:, 1) = c0
    diags(ind%tot_CaCO3_form)%field_3d(:, 1) = c0
    diags(ind%tot_Nfix)%field_3d(:, 1) = c0
    diags(ind%auto_graze_TOT)%field_3d(:, 1) = c0
    diags(ind%photoC_TOT)%field_3d(:, 1) = c0
    diags(ind%photoC_NO3_TOT)%field_3d(:, 1) = c0
    diags(ind%tot_CaCO3_form_zint)%field_2d(1) = c0
    diags(ind%tot_CaCO3_form_zint_100m)%field_2d(1) = c0
    diags(ind%photoC_TOT_zint)%field_2d(1) = c0
    diags(ind%photoC_TOT_zint_100m)%field_2d(1) = c0
    diags(ind%photoC_NO3_TOT_zint)%field_2d(1) = c0

    do n = 1, autotroph_cnt
       ! compute biomass weighted average of limitation terms over 0..100m
       autotrophC_weight(:) = autotroph_local%C(n,:)
       call marbl_diagnostics_share_compute_vertical_integrals(autotrophC_weight, delta_z, kmt, &
            near_surface_integral=autotrophC_zint_100m)

       ! if biomass integral is zero, treat biomass as 1.0, in order to weight all layers equally wrt biomass
       if (autotrophC_zint_100m == c0) then
         autotrophC_weight(:) = c1
         call marbl_diagnostics_share_compute_vertical_integrals(autotrophC_weight, delta_z, kmt, &
              near_surface_integral=autotrophC_zint_100m)
       end if

       ! normalize weight, so that its integral is 1
       autotrophC_weight(:) = autotrophC_weight(:) / autotrophC_zint_100m

       diags(ind%N_lim_surf(n))%field_2d(1) = autotroph_derived_terms%VNtot(n,1)
       limterm = autotroph_derived_terms%VNtot(n,:) * autotrophC_weight(:)
       call marbl_diagnostics_share_compute_vertical_integrals(limterm, delta_z, kmt, &
            near_surface_integral=diags(ind%N_lim_Cweight_avg_100m(n))%field_2d(1))

       diags(ind%P_lim_surf(n))%field_2d(1) = autotroph_derived_terms%VPtot(n,1)
       limterm = autotroph_derived_terms%VPtot(n,:) * autotrophC_weight(:)
       call marbl_diagnostics_share_compute_vertical_integrals(limterm, delta_z, kmt, &
            near_surface_integral=diags(ind%P_lim_Cweight_avg_100m(n))%field_2d(1))

       diags(ind%Fe_lim_surf(n))%field_2d(1) = autotroph_derived_terms%VFe(n,1)
       limterm = autotroph_derived_terms%VFe(n,:) * autotrophC_weight(:)
       call marbl_diagnostics_share_compute_vertical_integrals(limterm, delta_z, kmt, &
            near_surface_integral=diags(ind%Fe_lim_Cweight_avg_100m(n))%field_2d(1))

       if (ind%SiO3_lim_surf(n).ne.-1) then
          diags(ind%SiO3_lim_surf(n))%field_2d(1) = autotroph_derived_terms%VSiO3(n,1)
       endif
       if (ind%SiO3_lim_Cweight_avg_100m(n).ne.-1) then
          limterm = autotroph_derived_terms%VSiO3(n,:) * autotrophC_weight(:)
          call marbl_diagnostics_share_compute_vertical_integrals(limterm, delta_z, kmt, &
               near_surface_integral=diags(ind%SiO3_lim_Cweight_avg_100m(n))%field_2d(1))
       endif

       diags(ind%light_lim_surf(n))%field_2d(1) = autotroph_derived_terms%light_lim(n,1)
       limterm = autotroph_derived_terms%light_lim(n,:) * autotrophC_weight(:)
       call marbl_diagnostics_share_compute_vertical_integrals(limterm, delta_z, kmt, &
            near_surface_integral=diags(ind%light_lim_Cweight_avg_100m(n))%field_2d(1))

       if (ind%Qp(n).ne.-1) then
         diags(ind%Qp(n))%field_3d(:, 1)        = autotroph_derived_terms%Qp(n,:)
       end if

       diags(ind%photoNO3(n))%field_3d(:, 1)    = autotroph_derived_terms%NO3_V(n,:)
       diags(ind%photoNH4(n))%field_3d(:, 1)    = autotroph_derived_terms%NH4_V(n,:)
       diags(ind%PO4_uptake(n))%field_3d(:, 1)  = autotroph_derived_terms%PO4_V(n,:)
       diags(ind%DOP_uptake(n))%field_3d(:, 1)  = autotroph_derived_terms%DOP_V(n,:)
       diags(ind%photoFE(n))%field_3d(:, 1)     = autotroph_derived_terms%photoFe(n,:)

       if (ind%bSi_form(n).ne.-1) then
          diags(ind%bSi_form(n))%field_3d(:, 1)  = autotroph_derived_terms%photoSi(n,:)
          diags(ind%tot_bSi_form)%field_3d(:, 1) = diags(ind%tot_bSi_form)%field_3d(:, 1) + &
               diags(ind%bSi_form(n))%field_3d(:, 1)
       endif

       if (ind%CaCO3_form(n).ne.-1) then
          diags(ind%CaCO3_form(n))%field_3d(:, 1)  = autotroph_derived_terms%CaCO3_form(n,:)
          diags(ind%tot_CaCO3_form)%field_3d(:, 1) = diags(ind%tot_CaCO3_form)%field_3d(:, 1) + &
               diags(ind%CaCO3_form(n))%field_3d(:, 1)
       end if

       if (ind%Nfix(n).ne.-1) then
          diags(ind%Nfix(n))%field_3d(:, 1)  = autotroph_derived_terms%Nfix(n,:)
          diags(ind%tot_Nfix)%field_3d(:, 1) = diags(ind%tot_Nfix)%field_3d(:, 1) + &
               diags(ind%Nfix(n))%field_3d(:, 1)
       end if

       diags(ind%auto_graze(n))%field_3d(:, 1)     = autotroph_derived_terms%auto_graze(n,:)
       diags(ind%auto_graze_TOT)%field_3d(:, 1)    = diags(ind%auto_graze_TOT)%field_3d(:, 1) + &
            autotroph_derived_terms%auto_graze(n,:)
       diags(ind%auto_graze_poc(n))%field_3d(:, 1) = autotroph_derived_terms%auto_graze_poc(n,:)
       diags(ind%auto_graze_doc(n))%field_3d(:, 1) = autotroph_derived_terms%auto_graze_doc(n,:)
       diags(ind%auto_graze_zoo(n))%field_3d(:, 1) = autotroph_derived_terms%auto_graze_zoo(n,:)
       diags(ind%auto_loss(n))%field_3d(:, 1)      = autotroph_derived_terms%auto_loss(n,:)
       diags(ind%auto_loss_poc(n))%field_3d(:, 1)  = autotroph_derived_terms%auto_loss_poc(n,:)
       diags(ind%auto_loss_doc(n))%field_3d(:, 1)  = autotroph_derived_terms%auto_loss_doc(n,:)
       diags(ind%auto_agg(n))%field_3d(:, 1)       = autotroph_derived_terms%auto_agg(n,:)
       diags(ind%photoC(n))%field_3d(:, 1)         = autotroph_derived_terms%photoC(n,:)
       diags(ind%photoC_TOT)%field_3d(:, 1)        = diags(ind%photoC_TOT)%field_3d(:, 1) + &
            autotroph_derived_terms%photoC(n,:)

       diags(ind%photoC_NO3(n))%field_3d(:, 1) = c0
       where (autotroph_derived_terms%VNtot(n,:) > c0)
          diags(ind%photoC_NO3(n))%field_3d(:, 1) = autotroph_derived_terms%photoC(n,:) * &
               (autotroph_derived_terms%VNO3(n,:) / autotroph_derived_terms%VNtot(n,:))

          diags(ind%photoC_NO3_TOT)%field_3d(:, 1) = diags(ind%photoC_NO3_TOT)%field_3d(:, 1) + &
               diags(ind%photoC_NO3(n))%field_3d(:, 1)
       end where

       ! per-autotroph vertical integrals and their sums
       if (ind%CaCO3_form_zint(n).ne.-1) then
          call marbl_diagnostics_share_compute_vertical_integrals(autotroph_derived_terms%CaCO3_form(n,:), &
               delta_z, kmt, full_depth_integral=diags(ind%CaCO3_form_zint(n))%field_2d(1), &
               near_surface_integral=diags(ind%CaCO3_form_zint_100m(n))%field_2d(1))

          diags(ind%tot_CaCO3_form_zint)%field_2d(1) = diags(ind%tot_CaCO3_form_zint)%field_2d(1) + &
               diags(ind%CaCO3_form_zint(n))%field_2d(1)

          diags(ind%tot_CaCO3_form_zint_100m)%field_2d(1) = diags(ind%tot_CaCO3_form_zint_100m)%field_2d(1) + &
               diags(ind%CaCO3_form_zint_100m(n))%field_2d(1)
       end if

       call marbl_diagnostics_share_compute_vertical_integrals(autotroph_derived_terms%photoC(n,:), &
            delta_z, kmt, full_depth_integral=diags(ind%photoC_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%photoC_zint_100m(n))%field_2d(1))

       diags(ind%photoC_TOT_zint)%field_2d(1) = diags(ind%photoC_TOT_zint)%field_2d(1) + &
            diags(ind%photoC_zint(n))%field_2d(1)

       diags(ind%photoC_TOT_zint_100m)%field_2d(1) = diags(ind%photoC_TOT_zint_100m)%field_2d(1) + &
            diags(ind%photoC_zint_100m(n))%field_2d(1)

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%photoC_NO3(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%photoC_NO3_zint(n))%field_2d(1))

       diags(ind%photoC_NO3_TOT_zint)%field_2d(1) = diags(ind%photoC_NO3_TOT_zint)%field_2d(1) + &
            diags(ind%photoC_NO3_zint(n))%field_2d(1)

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%auto_graze(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%auto_graze_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%auto_graze_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%auto_graze_poc(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%auto_graze_poc_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%auto_graze_poc_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%auto_graze_doc(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%auto_graze_doc_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%auto_graze_doc_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%auto_graze_zoo(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%auto_graze_zoo_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%auto_graze_zoo_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%auto_loss(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%auto_loss_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%auto_loss_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%auto_loss_poc(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%auto_loss_poc_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%auto_loss_poc_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%auto_loss_doc(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%auto_loss_doc_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%auto_loss_doc_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%auto_agg(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%auto_agg_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%auto_agg_zint_100m(n))%field_2d(1))
    end do ! do n

    end associate

  end subroutine store_diagnostics_autotrophs

  !***********************************************************************

  subroutine store_diagnostics_particulates(marbl_domain, &
       interior_tendency_forcing_ind, interior_tendency_forcings, &
       marbl_particulate_share, &
       PON_remin, PON_sed_loss, &
       sed_denitrif, other_remin, marbl_interior_tendency_diags)

    !-----------------------------------------------------------------------
    ! - Set tavg variables.
    ! - Accumulte losses of BGC tracers to sediments
    !-----------------------------------------------------------------------

    use marbl_interface_private_types , only : marbl_interior_tendency_forcing_indexing_type
    use marbl_settings_mod, only : lp_remin_scalef
    use marbl_settings_mod, only : POCremin_refract
    use marbl_settings_mod, only : PONremin_refract
    use marbl_settings_mod, only : POPremin_refract

    type(marbl_domain_type)            , intent(in)    :: marbl_domain
    type(marbl_interior_tendency_forcing_indexing_type), intent(in) :: interior_tendency_forcing_ind
    type(marbl_forcing_fields_type)           , intent(in) :: interior_tendency_forcings(:)
    type(marbl_particulate_share_type) , intent(in)    :: marbl_particulate_share
    real(r8), dimension(:)             , intent(in)    :: PON_remin    ! km
    real(r8), dimension(:)             , intent(in)    :: PON_sed_loss ! km
    real(r8), dimension(:)             , intent(in)    :: sed_denitrif ! km
    real(r8), dimension(:)             , intent(in)    :: other_remin  ! km
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_interior_tendency_diags

    associate(                                                       &
         ind             => marbl_interior_tendency_diag_ind,        &
         diags           => marbl_interior_tendency_diags%diags,     &
         delta_z         => marbl_domain%delta_z,                    &
         kmt             => marbl_domain%kmt,                        &
         zw              => marbl_domain%zw,                         &
         POC             => marbl_particulate_share%POC,             &
         POP             => marbl_particulate_share%POP,             &
         P_CaCO3         => marbl_particulate_share%P_CaCO3,         &
         P_CaCO3_ALT_CO2 => marbl_particulate_share%P_CaCO3_ALT_CO2, &
         P_SiO2          => marbl_particulate_share%P_SiO2,          &
         dust            => marbl_particulate_share%dust,            &
         P_iron          => marbl_particulate_share%P_iron           &
         )

    if (lp_remin_scalef) then
       diags(ind%P_REMIN_SCALEF)%field_3d(:, 1) = &
            interior_tendency_forcings(interior_tendency_forcing_ind%p_remin_scalef_id)%field_1d(1,:)
    endif
    diags(ind%POC_FLUX_at_ref_depth)%field_2d(1) = POC%flux_at_ref_depth
    diags(ind%POC_FLUX_IN)%field_3d(:, 1)        = POC%sflux_in + POC%hflux_in
    diags(ind%POC_sFLUX_IN)%field_3d(:, 1)       = POC%sflux_in
    diags(ind%POC_hFLUX_IN)%field_3d(:, 1)       = POC%hflux_in
    diags(ind%POC_PROD)%field_3d(:, 1)           = POC%prod
    call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%POC_PROD)%field_3d(:, 1), &
         delta_z, kmt, full_depth_integral=diags(ind%POC_PROD_zint)%field_2d(1), &
         near_surface_integral=diags(ind%POC_PROD_zint_100m)%field_2d(1))
    diags(ind%POC_REMIN_DOCr)%field_3d(:, 1)     = POC%remin * POCremin_refract
    call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%POC_REMIN_DOCr)%field_3d(:, 1), &
         delta_z, kmt, full_depth_integral=diags(ind%POC_REMIN_DOCr_zint)%field_2d(1), &
         near_surface_integral=diags(ind%POC_REMIN_DOCr_zint_100m)%field_2d(1))
    diags(ind%POC_REMIN_DIC)%field_3d(:, 1)      = POC%remin * (c1 - POCremin_refract)
    call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%POC_REMIN_DIC)%field_3d(:, 1), &
         delta_z, kmt, full_depth_integral=diags(ind%POC_REMIN_DIC_zint)%field_2d(1), &
         near_surface_integral=diags(ind%POC_REMIN_DIC_zint_100m)%field_2d(1))

    diags(ind%POP_FLUX_at_ref_depth)%field_2d(1) = POP%flux_at_ref_depth
    diags(ind%POP_FLUX_IN)%field_3d(:, 1)        = POP%sflux_in + POP%hflux_in
    diags(ind%POP_PROD)%field_3d(:, 1)           = POP%prod
    diags(ind%POP_REMIN_DOPr)%field_3d(:, 1)     = POP%remin * POPremin_refract
    diags(ind%POP_REMIN_PO4)%field_3d(:, 1)      = POP%remin * (c1 - POPremin_refract)

    diags(ind%PON_REMIN_DONr)%field_3d(:, 1) = PON_remin * PONremin_refract
    diags(ind%PON_REMIN_NH4)%field_3d(:, 1)  = PON_remin * (c1 - PONremin_refract)

    diags(ind%CaCO3_FLUX_at_ref_depth)%field_2d(1) = P_CaCO3%flux_at_ref_depth
    diags(ind%CaCO3_FLUX_IN)%field_3d(:, 1)        = P_CaCO3%sflux_in + P_CaCO3%hflux_in
    diags(ind%CaCO3_PROD)%field_3d(:, 1)           = P_CaCO3%prod
    call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%CaCO3_PROD)%field_3d(:, 1), &
         delta_z, kmt, full_depth_integral=diags(ind%CaCO3_PROD_zint)%field_2d(1), &
         near_surface_integral=diags(ind%CaCO3_PROD_zint_100m)%field_2d(1))
    diags(ind%CaCO3_REMIN)%field_3d(:, 1)          = P_CaCO3%remin
    call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%CaCO3_REMIN)%field_3d(:, 1), &
         delta_z, kmt, full_depth_integral=diags(ind%CaCO3_REMIN_zint)%field_2d(1), &
         near_surface_integral=diags(ind%CaCO3_REMIN_zint_100m)%field_2d(1))

    diags(ind%CaCO3_ALT_CO2_FLUX_IN)%field_3d(:, 1) = P_CaCO3_ALT_CO2%sflux_in + P_CaCO3_ALT_CO2%hflux_in
    diags(ind%CaCO3_ALT_CO2_PROD)%field_3d(:, 1)    = P_CaCO3_ALT_CO2%prod
    diags(ind%CaCO3_ALT_CO2_REMIN)%field_3d(:, 1)   = P_CaCO3_ALT_CO2%remin

    diags(ind%SiO2_FLUX_at_ref_depth)%field_2d(1) = P_SiO2%flux_at_ref_depth
    diags(ind%SiO2_FLUX_IN)%field_3d(:, 1)        = P_SiO2%sflux_in + P_SiO2%hflux_in
    diags(ind%SiO2_PROD)%field_3d(:, 1)           = P_SiO2%prod
    diags(ind%SiO2_REMIN)%field_3d(:, 1)          = P_SiO2%remin

    diags(ind%dust_FLUX_IN)%field_3d(:, 1) = dust%sflux_in + dust%hflux_in
    diags(ind%dust_REMIN)%field_3d(:, 1)   = dust%remin

    diags(ind%P_iron_FLUX_at_ref_depth)%field_2d(1) = P_iron%flux_at_ref_depth
    diags(ind%P_iron_FLUX_IN)%field_3d(:, 1)        = P_iron%sflux_in + P_iron%hflux_in
    diags(ind%P_iron_PROD)%field_3d(:, 1)           = P_iron%prod
    diags(ind%P_iron_REMIN)%field_3d(:, 1)          = P_iron%remin

    diags(ind%calcToFloor)%field_2d(1)       = P_CaCO3%to_floor
    diags(ind%calcToSed)%field_2d(1)         = sum(P_CaCO3%sed_loss)
    diags(ind%calcToSed_ALT_CO2)%field_2d(1) = sum(P_CaCO3_ALT_CO2%sed_loss)
    diags(ind%bsiToSed)%field_2d(1)          = sum(P_SiO2%sed_loss)
    diags(ind%pocToFloor)%field_2d(1)        = POC%to_floor
    diags(ind%pocToSed)%field_2d(1)          = sum(POC%sed_loss)
    diags(ind%SedDenitrif)%field_2d(1)       = sum(sed_denitrif * delta_z)
    diags(ind%OtherRemin)%field_2d(1)        = sum(other_remin * delta_z)
    diags(ind%ponToSed)%field_2d(1)          = sum(PON_sed_loss)
    diags(ind%popToSed)%field_2d(1)          = sum(POP%sed_loss)
    diags(ind%dustToSed)%field_2d(1)         = sum(dust%sed_loss)
    diags(ind%pfeToSed)%field_2d(1)          = sum(P_iron%sed_loss)

    end associate

  end subroutine store_diagnostics_particulates

   !***********************************************************************

  subroutine store_diagnostics_oxygen(marbl_domain, &
       interior_tendency_forcing_ind, interior_tendency_forcings, potemp, salinity, &
       column_o2, o2_production, o2_consumption, marbl_interior_diags)

    use marbl_interface_private_types , only : marbl_interior_tendency_forcing_indexing_type
    use marbl_settings_mod, only : lo2_consumption_scalef
    use marbl_oxygen, only : o2sat_scalar

    type(marbl_domain_type)                 , intent(in)    :: marbl_domain
    type(marbl_interior_tendency_forcing_indexing_type), intent(in) :: interior_tendency_forcing_ind
    type(marbl_forcing_fields_type)           , intent(in) :: interior_tendency_forcings(:)
    real(r8)                                , intent(in)    :: potemp(:)
    real(r8)                                , intent(in)    :: salinity(:)
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
         kmt         => marbl_domain%kmt,                         &
         zt          => marbl_domain%zt,                          &
         diags       => marbl_interior_diags%diags,               &
         ind         => marbl_interior_tendency_diag_ind          &
         )

    if (lo2_consumption_scalef) then
       diags(ind%O2_CONSUMPTION_SCALEF)%field_3d(:, 1) = &
            interior_tendency_forcings(interior_tendency_forcing_ind%o2_consumption_scalef_id)%field_1d(1,:)
    endif

    min_ind = minloc(column_o2(1:kmt), dim=1)

    diags(ind%O2_ZMIN)%field_2d(1)       = column_o2(min_ind)
    diags(ind%O2_ZMIN_DEPTH)%field_2d(1) = zt(min_ind)

    diags(ind%O2_PRODUCTION)%field_3d(:, 1)  = o2_production
    diags(ind%O2_CONSUMPTION)%field_3d(:, 1) = o2_consumption

    do k=1,kmt
       diags(ind%AOU)%field_3d(k, 1) = O2SAT_scalar(potemp(k), salinity(k)) - column_o2(k)
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

    associate(                                     &
         km    => marbl_domain%km,                 &
         diags => marbl_interior_diags%diags,      &
         ind   => marbl_interior_tendency_diag_ind &
         )

    do k=1,km
       diags(ind%PAR_avg)%field_3d(k, 1) = sum(PAR_col_frac(:)*PAR_avg(k,:))
    end do

    end associate

  end subroutine store_diagnostics_PAR

  !***********************************************************************

  subroutine store_diagnostics_zooplankton(marbl_domain, &
       zooplankton_derived_terms, marbl_interior_diags)

    type(marbl_domain_type),              intent(in)    :: marbl_domain
    type(zooplankton_derived_terms_type), intent(in)    :: zooplankton_derived_terms
    type(marbl_diagnostics_type),         intent(inout) :: marbl_interior_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: n
    !-----------------------------------------------------------------------

    associate(                                        &
         diags   => marbl_interior_diags%diags,       &
         ind     => marbl_interior_tendency_diag_ind, &
         kmt     => marbl_domain%kmt,                 &
         delta_z => marbl_domain%delta_z              &
         )

    do n = 1, zooplankton_cnt
       diags(ind%zoo_loss(n))%field_3d(:, 1)      = zooplankton_derived_terms%zoo_loss(n,:)
       diags(ind%zoo_loss_poc(n))%field_3d(:, 1)  = zooplankton_derived_terms%zoo_loss_poc(n,:)
       diags(ind%zoo_loss_doc(n))%field_3d(:, 1)  = zooplankton_derived_terms%zoo_loss_doc(n,:)
       diags(ind%zoo_graze(n))%field_3d(:, 1)     = zooplankton_derived_terms%zoo_graze(n,:)
       diags(ind%zoo_graze_poc(n))%field_3d(:, 1) = zooplankton_derived_terms%zoo_graze_poc(n,:)
       diags(ind%zoo_graze_doc(n))%field_3d(:, 1) = zooplankton_derived_terms%zoo_graze_doc(n,:)
       diags(ind%zoo_graze_zoo(n))%field_3d(:, 1) = zooplankton_derived_terms%zoo_graze_zoo(n,:)
       diags(ind%x_graze_zoo(n))%field_3d(:, 1)   = zooplankton_derived_terms%x_graze_zoo(n,:)

       ! vertical integrals

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%zoo_loss(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%zoo_loss_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%zoo_loss_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%zoo_loss_poc(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%zoo_loss_poc_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%zoo_loss_poc_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%zoo_loss_doc(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%zoo_loss_doc_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%zoo_loss_doc_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%zoo_graze(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%zoo_graze_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%zoo_graze_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%zoo_graze_poc(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%zoo_graze_poc_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%zoo_graze_poc_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%zoo_graze_doc(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%zoo_graze_doc_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%zoo_graze_doc_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%zoo_graze_zoo(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%zoo_graze_zoo_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%zoo_graze_zoo_zint_100m(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%x_graze_zoo(n))%field_3d(:, 1), &
            delta_z, kmt, full_depth_integral=diags(ind%x_graze_zoo_zint(n))%field_2d(1), &
            near_surface_integral=diags(ind%x_graze_zoo_zint_100m(n))%field_2d(1))
    end do

    end associate

  end subroutine store_diagnostics_zooplankton

  !***********************************************************************

  subroutine store_diagnostics_dissolved_organic_matter(marbl_domain, &
       dissolved_organic_matter, marbl_diags)

    type(marbl_domain_type)            , intent(in)    :: marbl_domain
    type(dissolved_organic_matter_type), intent(in)    :: dissolved_organic_matter
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_diags

    associate(                                       &
         delta_z => marbl_domain%delta_z,            &
         kmt     => marbl_domain%kmt,                &
         diags   => marbl_diags%diags,               &
         ind     => marbl_interior_tendency_diag_ind &
         )

    diags(ind%DOC_prod)%field_3d(:, 1)         = dissolved_organic_matter%DOC_prod(:)
    diags(ind%DOC_remin)%field_3d(:, 1)        = dissolved_organic_matter%DOC_remin(:)
    diags(ind%DOCr_remin)%field_3d(:, 1)       = dissolved_organic_matter%DOCr_remin(:)
    diags(ind%DON_prod)%field_3d(:, 1)         = dissolved_organic_matter%DON_prod(:)
    diags(ind%DON_remin)%field_3d(:, 1)        = dissolved_organic_matter%DON_remin(:)
    diags(ind%DONr_remin)%field_3d(:, 1)       = dissolved_organic_matter%DONr_remin(:)
    diags(ind%DOP_prod)%field_3d(:, 1)         = dissolved_organic_matter%DOP_prod(:)
    diags(ind%DOP_remin)%field_3d(:, 1)        = dissolved_organic_matter%DOP_remin(:)
    diags(ind%DOPr_remin)%field_3d(:, 1)       = dissolved_organic_matter%DOPr_remin(:)
    diags(ind%DOP_loss_P_bal)%field_3d(:, 1)   = dissolved_organic_matter%DOP_loss_P_bal(:)

    call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%DOC_prod)%field_3d(:,1), &
         delta_z, kmt, full_depth_integral=diags(ind%DOC_prod_zint)%field_2d(1), &
         near_surface_integral=diags(ind%DOC_prod_zint_100m)%field_2d(1))

    call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%DOC_remin)%field_3d(:,1), &
         delta_z, kmt, full_depth_integral=diags(ind%DOC_remin_zint)%field_2d(1), &
         near_surface_integral=diags(ind%DOC_remin_zint_100m)%field_2d(1))

    call marbl_diagnostics_share_compute_vertical_integrals(diags(ind%DOCr_remin)%field_3d(:,1), &
         delta_z, kmt, full_depth_integral=diags(ind%DOCr_remin_zint)%field_2d(1), &
         near_surface_integral=diags(ind%DOCr_remin_zint_100m)%field_2d(1))

    end associate

  end subroutine store_diagnostics_dissolved_organic_matter

  !***********************************************************************

  subroutine store_diagnostics_iron_cycle(&
       fe_scavenge, fe_scavenge_rate, Lig_prod, Lig_loss, Lig_scavenge, &
       Fefree, Lig_photochem, Lig_deg, marbl_diags)

    real(r8)                            , intent(in)    :: fe_scavenge(:)      ! (km)
    real(r8)                            , intent(in)    :: fe_scavenge_rate(:) ! (km)
    real(r8)                            , intent(in)    :: Lig_prod(:)         ! (km)
    real(r8)                            , intent(in)    :: Lig_loss(:)         ! (km)
    real(r8)                            , intent(in)    :: Lig_scavenge(:)     ! (km)
    real(r8)                            , intent(in)    :: Fefree(:)           ! (km)
    real(r8)                            , intent(in)    :: Lig_photochem(:)    ! (km)
    real(r8)                            , intent(in)    :: Lig_deg(:)          ! (km)
    type(marbl_diagnostics_type)        , intent(inout) :: marbl_diags

    associate(                                     &
         diags => marbl_diags%diags,               &
         ind   => marbl_interior_tendency_diag_ind &
         )

    diags(ind%Fe_scavenge)%field_3d(:, 1)      = Fe_scavenge(:)
    diags(ind%Fe_scavenge_rate)%field_3d(:, 1) = Fe_scavenge_rate(:)
    diags(ind%Lig_prod)%field_3d(:, 1)         = Lig_prod(:)
    diags(ind%Lig_loss)%field_3d(:, 1)         = Lig_loss(:)
    diags(ind%Lig_scavenge)%field_3d(:, 1)     = Lig_scavenge(:)
    diags(ind%Fefree)%field_3d(:, 1)           = Fefree(:)
    diags(ind%Lig_photochem)%field_3d(:, 1)    = Lig_photochem(:)
    diags(ind%Lig_deg)%field_3d(:, 1)          = Lig_deg(:)

    end associate

  end subroutine store_diagnostics_iron_cycle

  !***********************************************************************

  subroutine store_diagnostics_carbon_fluxes(marbl_domain, POC, P_CaCO3, interior_tendencies, &
             marbl_tracer_indices, marbl_diags, marbl_status_log)

    use marbl_settings_mod, only : Jint_Ctot_thres

    type(marbl_domain_type)     , intent(in)    :: marbl_domain
    type(column_sinking_particle_type) , intent(in)    :: POC
    type(column_sinking_particle_type) , intent(in)    :: P_CaCO3
    real(r8)                           , intent(in)    :: interior_tendencies(:,:)         ! tracer_cnt, km
    type(marbl_tracer_index_type)      , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_diags
    type(marbl_log_type)               , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:store_diagnostics_carbon_fluxes'
    character(len=char_len)     :: log_message
    integer(int_kind) :: n, auto_ind
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                                        &
         diags   => marbl_diags%diags,                &
         ind     => marbl_interior_tendency_diag_ind, &

         kmt     => marbl_domain%kmt,     &
         delta_z => marbl_domain%delta_z, &

         dic_ind  => marbl_tracer_indices%dic_ind, &
         doc_ind  => marbl_tracer_indices%doc_ind, &
         docr_ind => marbl_tracer_indices%docr_ind &
         )

    ! vertical integrals
    work = interior_tendencies(dic_ind,:) + interior_tendencies(doc_ind,:) +             &
         interior_tendencies(docr_ind,:) +                                               &
         sum(interior_tendencies(marbl_tracer_indices%zoo_inds(:)%C_ind,:), dim=1) +     &
         sum(interior_tendencies(marbl_tracer_indices%auto_inds(:)%C_ind,:),dim=1)

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n .gt. 0) then
          work = work + interior_tendencies(n,:)
       end if
    end do

    call marbl_diagnostics_share_compute_vertical_integrals(work, delta_z, kmt, &
         full_depth_integral=diags(ind%Jint_Ctot)%field_2d(1),                  &
         integrated_terms = POC%sed_loss + P_CaCO3%sed_loss)

    if (abs(diags(ind%Jint_Ctot)%field_2d(1)) .gt. Jint_Ctot_thres) then
       write(log_message,"(A,E11.3e3,A,E11.3e3)") &
            'abs(Jint_Ctot)=', abs(diags(ind%Jint_Ctot)%field_2d(1)), &
            ' exceeds Jint_Ctot_thres=', Jint_Ctot_thres
       call marbl_status_log%log_error(log_message, subname, ElemInd=1)
       return
    end if

    end associate

  end subroutine store_diagnostics_carbon_fluxes

  !***********************************************************************

  subroutine store_diagnostics_nitrogen_fluxes(marbl_domain, &
       PON_sed_loss, denitrif, sed_denitrif, autotroph_derived_terms, interior_tendencies, &
       marbl_tracer_indices, marbl_diags, marbl_status_log)

    use marbl_settings_mod, only : Q
    use marbl_settings_mod, only : Jint_Ntot_thres

    type(marbl_domain_type),            intent(in)    :: marbl_domain
    real(r8),                           intent(in)    :: PON_sed_loss(:) ! km
    real(r8),                           intent(in)    :: denitrif(:)     ! km
    real(r8),                           intent(in)    :: sed_denitrif(:) ! km
    type(autotroph_derived_terms_type), intent(in)    :: autotroph_derived_terms
    real(r8),                           intent(in)    :: interior_tendencies(:,:)         ! tracer_cnt, km
    type(marbl_tracer_index_type),      intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type),       intent(inout) :: marbl_diags
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:store_diagnostics_nitrogen_fluxes'
    character(len=char_len)     :: log_message
    integer(int_kind) :: n
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                                        &
         diags   => marbl_diags%diags,                &
         ind     => marbl_interior_tendency_diag_ind, &

         kmt     => marbl_domain%kmt,     &
         delta_z => marbl_domain%delta_z, &

         no3_ind  => marbl_tracer_indices%no3_ind, &
         nh4_ind  => marbl_tracer_indices%nh4_ind, &
         don_ind  => marbl_tracer_indices%don_ind, &
         donr_ind => marbl_tracer_indices%donr_ind &
         )

    ! vertical integrals
    work = interior_tendencies(no3_ind,:) + interior_tendencies(nh4_ind,:) +                &
           interior_tendencies(don_ind,:) + interior_tendencies(donr_ind,:) +               &
           Q * sum(interior_tendencies(marbl_tracer_indices%zoo_inds(:)%C_ind,:), dim=1) +  &
           Q * sum(interior_tendencies(marbl_tracer_indices%auto_inds(:)%C_ind,:), dim=1) + &
           denitrif(:) + sed_denitrif(:)

    ! subtract out N fixation
    do n = 1, autotroph_cnt
       if (autotroph_settings(n)%Nfixer) then
          work = work - autotroph_derived_terms%Nfix(n,:)
       end if
    end do

    call marbl_diagnostics_share_compute_vertical_integrals(work, delta_z, kmt, &
         full_depth_integral=diags(ind%Jint_Ntot)%field_2d(1),                  &
         integrated_terms = PON_sed_loss)

    if (abs(diags(ind%Jint_Ntot)%field_2d(1)) .gt. Jint_Ntot_thres) then
       write(log_message,"(A,E11.3e3,A,E11.3e3)") &
            'abs(Jint_Ntot)=', abs(diags(ind%Jint_Ntot)%field_2d(1)), &
            ' exceeds Jint_Ntot_thres=', Jint_Ntot_thres
       call marbl_status_log%log_error(log_message, subname, ElemInd=1)
       return
    end if

    end associate

  end subroutine store_diagnostics_nitrogen_fluxes

  !***********************************************************************

  subroutine store_diagnostics_phosphorus_fluxes(marbl_domain, POP, &
       autotroph_derived_terms, interior_tendencies, &
       marbl_tracer_indices, marbl_diags, marbl_status_log)

    use marbl_pft_mod, only : Qp_zoo
    use marbl_settings_mod, only : lvariable_PtoC
    use marbl_settings_mod, only : Jint_Ptot_thres

    type(marbl_domain_type),            intent(in)    :: marbl_domain
    type(column_sinking_particle_type), intent(in)    :: POP
    type(autotroph_derived_terms_type), intent(in)    :: autotroph_derived_terms
    real(r8),                           intent(in)    :: interior_tendencies(:,:)         ! tracer_cnt, km
    type(marbl_tracer_index_type),      intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type),       intent(inout) :: marbl_diags
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:store_diagnostics_phosphorus_fluxes'
    character(len=char_len)     :: log_message
    integer(int_kind) :: n
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                                         &
         diags    => marbl_diags%diags,                &
         ind      => marbl_interior_tendency_diag_ind, &
         kmt      => marbl_domain%kmt,                 &
         delta_z  => marbl_domain%delta_z,             &
         po4_ind  => marbl_tracer_indices%po4_ind,     &
         dop_ind  => marbl_tracer_indices%dop_ind,     &
         dopr_ind => marbl_tracer_indices%dopr_ind     &
         )

    ! vertical integrals
    work = interior_tendencies(po4_ind,:) + interior_tendencies(dop_ind,:) + interior_tendencies(dopr_ind,:) + &
         Qp_zoo * sum(interior_tendencies(marbl_tracer_indices%zoo_inds(:)%C_ind,:), dim=1)

    if (lvariable_PtoC) then
       work = work + sum(interior_tendencies(marbl_tracer_indices%auto_inds(:)%P_ind,:),dim=1)
    else
       do n = 1, autotroph_cnt
          work = work + autotroph_derived_terms%Qp(n,:) * interior_tendencies(marbl_tracer_indices%auto_inds(n)%C_ind,:)
       end do
    endif

    call marbl_diagnostics_share_compute_vertical_integrals(work, delta_z, kmt, &
         full_depth_integral=diags(ind%Jint_Ptot)%field_2d(1),                  &
         integrated_terms = POP%sed_loss)

    if (abs(diags(ind%Jint_Ptot)%field_2d(1)) .gt. Jint_Ptot_thres) then
       write(log_message,"(A,E11.3e3,A,E11.3e3)") &
            'abs(Jint_Ptot)=', abs(diags(ind%Jint_Ptot)%field_2d(1)), &
            ' exceeds Jint_Ptot_thres=', Jint_Ptot_thres
       call marbl_status_log%log_error(log_message, subname, ElemInd=1)
       return
    end if

    end associate

  end subroutine store_diagnostics_phosphorus_fluxes

  !***********************************************************************

  subroutine store_diagnostics_silicon_fluxes(marbl_domain, P_SiO2, interior_tendencies, &
       marbl_tracer_indices, marbl_diags, marbl_status_log)

    use marbl_settings_mod, only : Jint_Sitot_thres

    type(marbl_domain_type)            , intent(in)    :: marbl_domain
    type(column_sinking_particle_type) , intent(in)    :: P_SiO2
    real(r8)                           , intent(in)    :: interior_tendencies(:,:)         ! tracer_cnt, km
    type(marbl_tracer_index_type)      , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_diags
    type(marbl_log_type)               , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:store_diagnostics_silicon_fluxes'
    character(len=char_len)     :: log_message
    integer(int_kind) :: n
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                                        &
         diags   => marbl_diags%diags,                &
         ind     => marbl_interior_tendency_diag_ind, &
         kmt     => marbl_domain%kmt,                 &
         zw      => marbl_domain%zw,                  &
         delta_z => marbl_domain%delta_z              &
         )

    ! vertical integrals
    work = interior_tendencies(marbl_tracer_indices%sio3_ind,:)

    do n = 1, autotroph_cnt
       if (marbl_tracer_indices%auto_inds(n)%Si_ind > 0) then
          work = work + interior_tendencies(marbl_tracer_indices%auto_inds(n)%Si_ind,:)
       end if
    end do

    call marbl_diagnostics_share_compute_vertical_integrals(work, delta_z, kmt, &
         full_depth_integral=diags(ind%Jint_Sitot)%field_2d(1),                 &
         integrated_terms = P_SiO2%sed_loss)

    if (abs(diags(ind%Jint_Sitot)%field_2d(1)) .gt. Jint_Sitot_thres) then
       write(log_message,"(A,E11.3e3,A,E11.3e3)") &
            'abs(Jint_Sitot)=', abs(diags(ind%Jint_Sitot)%field_2d(1)), &
            ' exceeds Jint_Sitot_thres=', Jint_Sitot_thres
       call marbl_status_log%log_error(log_message, subname, ElemInd=1)
       return
    end if

    end associate

  end subroutine store_diagnostics_silicon_fluxes

  !***********************************************************************

  subroutine store_diagnostics_iron_fluxes(marbl_domain, P_iron, dust, &
             fesedflux, interior_tendencies, marbl_tracer_indices, marbl_diags, marbl_status_log)

    use marbl_settings_mod, only : Qfe_zoo
    use marbl_settings_mod, only : dust_to_Fe
    use marbl_settings_mod, only : Jint_Fetot_thres

    type(marbl_domain_type)            , intent(in)    :: marbl_domain
    type(column_sinking_particle_type) , intent(in)    :: P_iron
    type(column_sinking_particle_type) , intent(in)    :: dust
    real(r8)                           , intent(in)    :: fesedflux(:)          ! km
    real(r8)                           , intent(in)    :: interior_tendencies(:,:)         ! tracer_cnt, km
    type(marbl_tracer_index_type)      , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_diags
    type(marbl_log_type)               , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_diagnostics_mod:store_diagnostics_iron_fluxes'
    character(len=char_len)     :: log_message
    real(r8), dimension(marbl_domain%km) :: work
    !-----------------------------------------------------------------------

    associate(                                        &
         diags   => marbl_diags%diags,                &
         ind     => marbl_interior_tendency_diag_ind, &
         kmt     => marbl_domain%kmt,                 &
         zw      => marbl_domain%zw,                  &
         delta_z => marbl_domain%delta_z,             &
         fe_ind  => marbl_tracer_indices%fe_ind       &
         )

    diags(ind%fesedflux)%field_3d(:,1) = fesedflux(:)
    ! vertical integrals
    work = interior_tendencies(fe_ind, :) +                                              &
           sum(interior_tendencies(marbl_tracer_indices%auto_inds(:)%Fe_ind, :),dim=1) + &
           Qfe_zoo * sum(interior_tendencies(marbl_tracer_indices%zoo_inds(:)%C_ind, :),dim=1) - &
           dust%remin(:) * dust_to_Fe

    call marbl_diagnostics_share_compute_vertical_integrals(work, delta_z, kmt, &
         full_depth_integral=diags(ind%Jint_Fetot)%field_2d(1),                 &
         integrated_terms = P_iron%sed_loss - fesedflux)

    if (abs(diags(ind%Jint_Fetot)%field_2d(1)) .gt. Jint_Fetot_thres) then
       write(log_message,"(A,E11.3e3,A,E11.3e3)") &
            'abs(Jint_Fetot)=', abs(diags(ind%Jint_Fetot)%field_2d(1)), &
            ' exceeds Jint_Fetot_thres=', Jint_Fetot_thres
       call marbl_status_log%log_error(log_message, subname, ElemInd=1)
       return
    end if

    end associate

  end subroutine store_diagnostics_iron_fluxes

  !***********************************************************************

  subroutine store_diagnostics_interior_restore(interior_restore, marbl_diags)

    real(r8), dimension(:,:)           , intent(in)    :: interior_restore
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_diags

    integer :: n

    associate(                                       &
         diags   => marbl_diags%diags,               &
         ind     => marbl_interior_tendency_diag_ind &
         )

    do n=1, size(ind%restore_tend)
       diags(ind%restore_tend(n))%field_3d(:,1) = interior_restore(n,:)
    end do

    end associate

  end subroutine store_diagnostics_interior_restore

  !*****************************************************************************

end module marbl_diagnostics_mod
