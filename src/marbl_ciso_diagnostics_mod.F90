module marbl_ciso_diagnostics_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : c1000

  use marbl_settings_mod, only : autotroph_cnt
  use marbl_settings_mod, only : autotroph_settings

  use marbl_interface_public_types, only : marbl_diagnostics_type

  use marbl_logging, only : marbl_log_type
  use marbl_logging, only : marbl_logging_add_diagnostics_error

  use marbl_diagnostics_share_mod, only : marbl_surface_flux_diag_ind
  use marbl_diagnostics_share_mod, only : marbl_interior_tendency_diag_ind
  use marbl_diagnostics_share_mod, only : marbl_diagnostics_share_compute_vertical_integrals

  implicit none
  private

  public :: marbl_ciso_diagnostics_init
  public :: marbl_ciso_diagnostics_surface_flux_compute
  public :: store_diagnostics_ciso_interior

contains

  !***********************************************************************

  subroutine marbl_ciso_diagnostics_init( &
       marbl_interior_tendency_diags, &
       marbl_surface_flux_diags,  &
       marbl_status_log)

    use marbl_settings_mod, only : ciso_on

    type(marbl_diagnostics_type), intent(inout) :: marbl_interior_tendency_diags
    type(marbl_diagnostics_type), intent(inout) :: marbl_surface_flux_diags
    type(marbl_log_type),         intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_ciso_diagnostics_mod:marbl_ciso_diagnostics_init'
    integer :: n
    logical :: truncate
    character(len=char_len) :: lname, sname, units, vgrid

    if (.not. ciso_on) return

    !-----------------------------------------------------------------
    ! Surface forcing diagnostics
    !-----------------------------------------------------------------

    associate(                                    &
              ind => marbl_surface_flux_diag_ind, &
              diags => marbl_surface_flux_diags   &
             )

      lname    = 'DI13C Surface Gas Flux'
      sname    = 'CISO_FG_13CO2'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DI13C_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DI13C Surface Air-Sea Gas Flux'
      sname    = 'CISO_FG_as_13CO2'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DI13C_AS_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DI13C Surface Sea-Air Gas Flux'
      sname    = 'CISO_FG_sa_13CO2'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DI13C_SA_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'D13C Surface GAS FLUX'
      sname    = 'CISO_FG_d13C'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_d13C_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Atmospheric Delta 13C in permil'
      sname    = 'CISO_D13C_atm'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_D13C_atm, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = '13C/12C ratio in total DIC'
      sname    = 'CISO_R13C_DIC_surf'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_R13C_DIC_surf, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = '13C/12C ratio in atmosphere'
      sname    = 'CISO_R13C_atm'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_R13C_atm, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Surface equilibrium fractionation (CO2_gaseous <-> CO2_aq)'
      sname    = 'CISO_eps_aq_g_surf'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_eps_aq_g_surf, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Surface equilibrium fractionation between total DIC and gaseous CO2'
      sname    = 'CISO_eps_dic_g_surf'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_eps_dic_g_surf, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DI14C Surface Gas Flux'
      sname    = 'CISO_FG_14CO2'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DI14C_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DI14C Surface Air-Sea Gas Flux'
      sname    = 'CISO_FG_as_14CO2'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DI14C_AS_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DI14C Surface Sea-Air Gas Flux'
      sname    = 'CISO_FG_sa_14CO2'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DI14C_SA_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'D14C Surface GAS FLUX'
      sname    = 'CISO_FG_d14C'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_d14C_GAS_FLUX, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Atmospheric Delta 14C in permil'
      sname    = 'CISO_D14C_atm'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_D14C_atm, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = '14C/12C ratio in total DIC'
      sname    = 'CISO_R14C_DIC_surf'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_R14C_DIC_surf, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = '14C/12C ratio in atmosphere'
      sname    = 'CISO_R14C_atm'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_R14C_atm, marbl_status_log)
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

      !  nonstandard 3D fields
      lname    = 'PO13C Flux into Cell'
      sname    = 'CISO_PO13C_FLUX_IN'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_PO13C_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'PO13C Production'
      sname    = 'CISO_PO13C_PROD'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_PO13C_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'PO13C Remineralization'
      sname    = 'CISO_PO13C_REMIN'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_PO13C_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DO13Ctot Production'
      sname    = 'CISO_DO13Ctot_prod'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DO13Ctot_prod, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DO13Ctot Remineralization'
      sname    = 'CISO_DO13Ctot_remin'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DO13Ctot_remin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Ca13CO3 flux into cell'
      sname    = 'CISO_Ca13CO3_FLUX_IN'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_Ca13CO3_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Ca13CO3 Production'
      sname    = 'CISO_Ca13CO3_PROD'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_Ca13CO3_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Ca13CO3 Remineralization'
      sname    = 'CISO_Ca13CO3_REMIN'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_Ca13CO3_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Total 13C Fixation'
      sname    = 'CISO_photo13C_TOT'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_photo13C_TOT, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'd13C of DIC'
      sname    = 'CISO_DIC_d13C'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DIC_d13C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'd13C of DOCtot'
      sname    = 'CISO_DOCtot_d13C'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DOCtot_d13C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'd13C of total zooC'
      sname    = 'CISO_zoototC_d13C'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_zoototC_d13C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'PO14C Flux into Cell'
      sname    = 'CISO_PO14C_FLUX_IN'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_PO14C_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'PO14C Production'
      sname    = 'CISO_PO14C_PROD'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_PO14C_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'PO14C Remineralization'
      sname    = 'CISO_PO14C_REMIN'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_PO14C_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DO14Ctot Production'
      sname    = 'CISO_DO14Ctot_prod'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DO14Ctot_prod, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'DO14Ctot Remineralization'
      sname    = 'CISO_DO14Ctot_remin'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DO14Ctot_remin, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Ca14CO3 flux into cell'
      sname    = 'CISO_Ca14CO3_FLUX_IN'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_Ca14CO3_FLUX_IN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Ca14CO3 Production'
      sname    = 'CISO_Ca14CO3_PROD'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_Ca14CO3_PROD, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Ca14CO3 Remineralization'
      sname    = 'CISO_Ca14CO3_REMIN'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_Ca14CO3_REMIN, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Total 14C Fixation'
      sname    = 'CISO_photo14C_TOT'
      units    = 'mmol/m^3/s'
      vgrid    = 'layer_avg'
      truncate = .true.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_photo14C_TOT, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'd14C of DIC'
      sname    = 'CISO_DIC_d14C'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DIC_d14C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'd14C of DOCtot'
      sname    = 'CISO_DOCtot_d14C'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_DOCtot_d14C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'd14C of total zooC'
      sname    = 'CISO_zoototC_d14C'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_zoototC_d14C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      !  Nonstandard 2D fields

      lname    = 'Total 13C Fixation Vertical Integral'
      sname    = 'CISO_photo13C_TOT_zint'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_photo13C_TOT_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Total 14C Fixation Vertical Integral'
      sname    = 'CISO_photo14C_TOT_zint'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_photo14C_TOT_zint, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = '13Ctot Source Sink Term Vertical Integral'
      sname    = 'CISO_Jint_13Ctot'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_Jint_13Ctot, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = '14Ctot Source Sink Term Vertical Integral'
      sname    = 'CISO_Jint_14Ctot'
      units    = 'mmol/m^3 cm/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_Jint_14Ctot, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      !  Nonstandard autotroph 2D and 3D fields for each autotroph
      if (.not.ind%lconstructed()) then
       allocate(ind%CISO_eps_autotroph(autotroph_cnt))
       allocate(ind%CISO_mui_to_co2star(autotroph_cnt))
       allocate(ind%CISO_Ca13CO3_form(autotroph_cnt))
       allocate(ind%CISO_Ca14CO3_form(autotroph_cnt))
       allocate(ind%CISO_Ca13CO3_form_zint(autotroph_cnt))
       allocate(ind%CISO_Ca14CO3_form_zint(autotroph_cnt))
       allocate(ind%CISO_photo13C(autotroph_cnt))
       allocate(ind%CISO_photo14C(autotroph_cnt))
       allocate(ind%CISO_photo13C_zint(autotroph_cnt))
       allocate(ind%CISO_photo14C_zint(autotroph_cnt))
       allocate(ind%CISO_d13C(autotroph_cnt))
       allocate(ind%CISO_d14C(autotroph_cnt))
       allocate(ind%CISO_autotrophCaCO3_d13C(autotroph_cnt))
       allocate(ind%CISO_autotrophCaCO3_d14C(autotroph_cnt))
      end if
      do n = 1, autotroph_cnt
       if (autotroph_settings(n)%imp_calcifier .or. autotroph_settings(n)%exp_calcifier) then
         lname    = trim(autotroph_settings(n)%lname) // ' Ca13CO3 Formation'
         sname    = 'CISO_' // trim(autotroph_settings(n)%sname) // '_Ca13CO3_form'
         units    = 'mmol/m^3/s'
         vgrid    = 'layer_avg'
         truncate = .true.
         call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
              ind%CISO_Ca13CO3_form(n), marbl_status_log)
         if (marbl_status_log%labort_marbl) then
           call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
           return
         end if

         lname    = trim(autotroph_settings(n)%lname) // ' Ca13CO3 Formation Vertical Integral'
         sname    = 'CISO_' // trim(autotroph_settings(n)%sname) // '_Ca13CO3_form_zint'
         units    = 'mmol/m^3 cm/s'
         vgrid    = 'none'
         truncate = .false.
         call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
              ind%CISO_Ca13CO3_form_zint(n), marbl_status_log)
         if (marbl_status_log%labort_marbl) then
           call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
           return
         end if

         lname    = trim(autotroph_settings(n)%lname) // ' Ca14CO3 Formation'
         sname    = 'CISO_' // trim(autotroph_settings(n)%sname) // '_Ca14CO3_form'
         units    = 'mmol/m^3/s'
         vgrid    = 'layer_avg'
         truncate = .true.
         call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
              ind%CISO_Ca14CO3_form(n), marbl_status_log)
         if (marbl_status_log%labort_marbl) then
           call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
           return
         end if

         lname    = trim(autotroph_settings(n)%lname) // ' Ca14CO3 Formation Vertical Integral'
         sname    = 'CISO_' // trim(autotroph_settings(n)%sname) // '_Ca14CO3_form_zint'
         units    = 'mmol/m^3 cm/s'
         vgrid    = 'none'
         truncate = .false.
         call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
              ind%CISO_Ca14CO3_form_zint(n), marbl_status_log)
         if (marbl_status_log%labort_marbl) then
           call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
           return
         end if

         lname    = trim(autotroph_settings(n)%lname) // ' d13C of CaCO3'
         sname    = 'CISO_autotrophCaCO3_d13C_' // trim(autotroph_settings(n)%sname)
         units    = 'mmol/m^3/s'
         vgrid    = 'layer_avg'
         truncate = .false.
         call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
              ind%CISO_autotrophCaCO3_d13C(n), marbl_status_log)
         if (marbl_status_log%labort_marbl) then
           call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
           return
         end if

         lname    = trim(autotroph_settings(n)%lname) // ' d14C of CaCO3'
         sname    = 'CISO_autotrophCaCO3_d14C_' // trim(autotroph_settings(n)%sname)
         units    = 'mmol/m^3/s'
         vgrid    = 'layer_avg'
         truncate = .false.
         call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
              ind%CISO_autotrophCaCO3_d14C(n), marbl_status_log)
         if (marbl_status_log%labort_marbl) then
           call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
           return
         end if
       else
         ind%CISO_Ca13CO3_form(n) = 0
         ind%CISO_Ca13CO3_form_zint(n) = 0
         ind%CISO_Ca14CO3_form(n) = 0
         ind%CISO_Ca14CO3_form_zint(n) = 0
         ind%CISO_autotrophCaCO3_d13C(n) = 0
         ind%CISO_autotrophCaCO3_d14C(n) = 0
       end if ! calcifier

       lname    = trim(autotroph_settings(n)%lname) // ' 13C Fixation'
       sname    = 'CISO_photo13C_' // trim(autotroph_settings(n)%sname)
       units    = 'mmol/m^3/s'
       vgrid    = 'layer_avg'
       truncate = .true.
       call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
            ind%CISO_photo13C(n), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
       end if

       lname    = trim(autotroph_settings(n)%lname) // ' 14C Fixation'
       sname    = 'CISO_photo14C_' // trim(autotroph_settings(n)%sname)
       units    = 'mmol/m^3/s'
       vgrid    = 'layer_avg'
       truncate = .true.
       call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
            ind%CISO_photo14C(n), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
       end if

       lname    = trim(autotroph_settings(n)%lname) // ' 13C Fixation Vertical Integral'
       sname    = 'CISO_photo13C_' // trim(autotroph_settings(n)%sname) // '_zint'
       units    = 'mmol/m^3 cm/s'
       vgrid    = 'none'
       truncate = .false.
       call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
            ind%CISO_photo13C_zint(n), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
       end if

       lname    = trim(autotroph_settings(n)%lname) // ' 14C Fixation Vertical Integral'
       sname    = 'CISO_photo14C_' // trim(autotroph_settings(n)%sname) // '_zint'
       units    = 'mmol/m^3 cm/s'
       vgrid    = 'none'
       truncate = .false.
       call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
            ind%CISO_photo14C_zint(n), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
       end if

       lname    = trim(autotroph_settings(n)%lname) // ' discrimination factor (eps)'
       sname    = 'CISO_eps_autotroph_' // trim(autotroph_settings(n)%sname)
       units    = 'permil'
       vgrid    = 'layer_avg'
       truncate = .false.
       call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
            ind%CISO_eps_autotroph(n), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
       end if

       lname    = trim(autotroph_settings(n)%lname) // ' d13C'
       sname    = 'CISO_d13C_' // trim(autotroph_settings(n)%sname)
       units    = 'permil'
       vgrid    = 'layer_avg'
       truncate = .false.
       call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
            ind%CISO_d13C(n), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
       end if

       lname    = trim(autotroph_settings(n)%lname) // ' d14C'
       sname    = 'CISO_d14C_' // trim(autotroph_settings(n)%sname)
       units    = 'permil'
       vgrid    = 'layer_avg'
       truncate = .false.
       call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
            ind%CISO_d14C(n), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
       end if

       lname    = trim(autotroph_settings(n)%lname) // ' instanteous growth rate over [CO2*]'
       sname    = 'CISO_mui_to_co2star_' // trim(autotroph_settings(n)%sname)
       units    = 'm^3/mmol/s'
       vgrid    = 'layer_avg'
       truncate = .false.
       call diags%add_diagnostic(lname, sname, units, vgrid, truncate, &
            ind%CISO_mui_to_co2star(n), marbl_status_log)
       if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
       end if

      end do

      !  More nonstandard 3D fields

      lname    = 'Equilibrium fractionation (CO2_gaseous <-> CO2_aq)'
      sname    = 'CISO_eps_aq_g'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_eps_aq_g, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Equilibrium fractionation between total DIC and gaseous CO2'
      sname    = 'CISO_eps_dic_g'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%CISO_eps_dic_g, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      !  Vars to sum up burial in sediments (2D)

      lname    = 'Ca13CO3 Flux to Sediments'
      sname    = 'calcToSed_13C'
      units    = 'nmol/cm^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%calcToSed_13C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'PO13C Flux to Sediments'
      sname    = 'pocToSed_13C'
      units    = 'nmol/cm^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%pocToSed_13C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Ca14CO3 Flux to Sediments'
      sname    = 'calcToSed_14C'
      units    = 'nmol/cm^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%calcToSed_14C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'PO14C Flux to Sediments'
      sname    = 'pocToSed_14C'
      units    = 'nmol/cm^2/s'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%pocToSed_14C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

    end associate

  end subroutine marbl_ciso_diagnostics_init

  !***********************************************************************

  subroutine marbl_ciso_diagnostics_surface_flux_compute( &
       num_elements,   &
       D13C,           &
       D14C,           &
       FLUX,           &
       FLUX13,         &
       FLUX14,         &
       FLUX13_as,      &
       FLUX14_as,      &
       FLUX13_sa,      &
       FLUX14_sa,      &
       R13C_DIC,       &
       R14C_DIC,       &
       R13C_atm,       &
       R14C_atm,       &
       eps_aq_g_surf,  &
       eps_dic_g_surf, &
       marbl_surface_flux_diags)

    ! !DESCRIPTION:
    !  Compute surface fluxes for ecosys tracer module.

    use marbl_constants_mod, only : R13C_std, R14C_std

    integer (int_kind)                 , intent(in)    :: num_elements
    real (r8), dimension(num_elements) , intent(in)    :: D13C           ! atm 13co2 value
    real (r8), dimension(num_elements) , intent(in)    :: D14C           ! atm 14co2 value
    real (r8), dimension(num_elements) , intent(in)    :: FLUX           ! gas flux of CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX13         ! gas flux of 13CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX14         ! gas flux of 14CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX13_as      ! air-to-sea gas flux of 13CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX14_as      ! air-to-sea gas flux of 14CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX13_sa      ! sea-to-air gas flux of 13CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: FLUX14_sa      ! sea-to-air gas flux of 14CO2 (nmol/cm^2/s)
    real (r8), dimension(num_elements) , intent(in)    :: R13C_DIC       ! 13C/12C ratio in DIC
    real (r8), dimension(num_elements) , intent(in)    :: R14C_DIC       ! 14C/12C ratio in total DIC
    real (r8), dimension(num_elements) , intent(in)    :: R13C_atm       ! 13C/12C ratio in atmospheric CO2
    real (r8), dimension(num_elements) , intent(in)    :: R14C_atm       ! 14C/12C ratio in atmospheric CO2
    real (r8), dimension(num_elements) , intent(in)    :: eps_aq_g_surf  ! equilibrium fractionation (CO2_gaseous <-> CO2_aq)
    real (r8), dimension(num_elements) , intent(in)    :: eps_dic_g_surf ! equilibrium fractionation between total DIC and gaseous CO2
    type(marbl_diagnostics_type)       , intent(inout) :: marbl_surface_flux_diags

    associate(                                       &
         diags => marbl_surface_flux_diags%diags,    &
         ind   => marbl_surface_flux_diag_ind        &
         )

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

    end associate

  end subroutine marbl_ciso_diagnostics_surface_flux_compute

  !*****************************************************************************

  subroutine store_diagnostics_ciso_interior(&
       marbl_domain,        &
       autotroph_d13C,      &
       autotroph_d14C,      &
       autotrophCaCO3_d13C, &
       autotrophCaCO3_d14C, &
       photo13C,            &
       photo14C,            &
       eps_autotroph,       &
       mui_to_co2star,      &
       Ca13CO3_prod,        &
       Ca14CO3_prod,        &
       DIC_d13C,            &
       DIC_d14C,            &
       DOCtot_d13C,         &
       DOCtot_d14C,         &
       zoototC_d13C,        &
       zoototC_d14C,        &
       DO13Ctot_prod,       &
       DO14Ctot_prod,       &
       DO13Ctot_remin,      &
       DO14Ctot_remin,      &
       eps_aq_g,            &
       eps_dic_g,           &
       decay_14Ctot,        &
       PO13C,               &
       PO14C,               &
       P_Ca13CO3,           &
       P_Ca14CO3,           &
       interior_tendencies, &
       marbl_tracer_indices,&
       marbl_diags,         &
       marbl_status_log)

    !---------------------------------------------------------------------
    ! !DESCRIPTION:
    !  Update marbl_interior_ciso_diags data type
    !---------------------------------------------------------------------

    use marbl_settings_mod, only : CISO_Jint_13Ctot_thres
    use marbl_settings_mod, only : CISO_Jint_14Ctot_thres
    use marbl_interface_public_types, only : marbl_domain_type
    use marbl_interface_private_types, only : column_sinking_particle_type
    use marbl_interface_private_types, only : marbl_tracer_index_type

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
         DIC_d13C       , & ! d13C of DIC
         DIC_d14C       , & ! d14C of DIC
         DOCtot_d13C    , & ! d13C of DOCtot
         DOCtot_d14C    , & ! d14C of DOCtot
         zoototC_d13C   , & ! d13C of total zooC
         zoototC_d14C   , & ! d14C of total zooC
         DO13Ctot_prod  , & ! production of 13C DOCtot (mmol C/m^3/sec)
         DO14Ctot_prod  , & ! production of 14C DOCtot (mmol C/m^3/sec)
         DO13Ctot_remin , & ! remineralization of 13C DOCtot (mmol C/m^3/sec)
         DO14Ctot_remin , & ! remineralization of 14C DOCtot (mmol C/m^3/sec)
         eps_aq_g       , & ! equilibrium fractionation (CO2_gaseous <-> CO2_aq)
         eps_dic_g      , & ! equilibrium fractionation between total DIC and gaseous CO2
         decay_14Ctot       ! 14C decay loss term

    type(column_sinking_particle_type), intent(in) :: &
         PO13C,        &  ! base units = nmol 13C
         PO14C,        &  ! base units = nmol 14C
         P_Ca13CO3,    &  ! base units = nmol 13C CaCO3
         P_Ca14CO3        ! base units = nmol 14C CaCO3

    real (r8), intent(in) :: interior_tendencies(:,:) ! (tracer_cnt, km) computed source/sink terms

    type(marbl_tracer_index_type), intent(in) :: marbl_tracer_indices

    type(marbl_diagnostics_type), intent(inout) :: marbl_diags

    type(marbl_log_type), intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_ciso_diagnostics_mod:store_diagnostics_ciso_interior'
    character(len=char_len)     :: log_message
    integer (int_kind) :: k, n, auto_ind
    real (r8)          :: work(marbl_domain%km)
    !-----------------------------------------------------------------------

    associate( &
         km      => marbl_domain%km,         &
         kmt     => marbl_domain%kmt,        &
         zw      => marbl_domain%zw,         &
         delta_z => marbl_domain%delta_z,    &
         diags   => marbl_diags%diags,       &
         ind     => marbl_interior_tendency_diag_ind, &
         di13c_ind     => marbl_tracer_indices%di13c_ind,        &
         do13ctot_ind  => marbl_tracer_indices%do13ctot_ind,     &
         zootot13C_ind => marbl_tracer_indices%zootot13C_ind,    &
         di14c_ind     => marbl_tracer_indices%di14c_ind,        &
         do14ctot_ind  => marbl_tracer_indices%do14ctot_ind,     &
         zootot14C_ind => marbl_tracer_indices%zootot14C_ind     &
         )

    diags(ind%calcToSed_13C)%field_2d(1) = sum(P_Ca13CO3%sed_loss)
    diags(ind%calcToSed_14C)%field_2d(1) = sum(P_Ca14CO3%sed_loss)

    diags(ind%pocToSed_13C)%field_2d(1)  = sum(PO13C%sed_loss)
    diags(ind%pocToSed_14C)%field_2d(1)  = sum(PO14C%sed_loss)

    diags(ind%CISO_photo13C_TOT)%field_3d(:, 1) = sum(photo13C, dim=1)
    diags(ind%CISO_photo14C_TOT)%field_3d(:, 1) = sum(photo14C, dim=1)

    diags(ind%CISO_photo13C_TOT_zint)%field_2d(1) = sum(delta_z * sum(photo13C, dim=1))
    diags(ind%CISO_photo14C_TOT_zint)%field_2d(1) = sum(delta_z * sum(photo14C, dim=1))

    ! Vertical integrals - CISO_Jint_13Ctot

    work(:) = interior_tendencies(di13c_ind,:) + interior_tendencies(do13ctot_ind,:) + interior_tendencies(zootot13C_ind,:) &
         + sum(interior_tendencies(marbl_tracer_indices%auto_inds(:)%C13_ind,:), dim=1)
    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
       if (n > 0) then
          work = work + interior_tendencies(n,:)
       end if
    end do
    call marbl_diagnostics_share_compute_vertical_integrals(work, delta_z, kmt, &
         full_depth_integral=diags(ind%CISO_Jint_13Ctot)%field_2d(1),           &
         integrated_terms = PO13C%sed_loss + P_Ca13CO3%sed_loss)

    if (abs(diags(ind%CISO_Jint_13Ctot)%field_2d(1)) .gt. CISO_Jint_13Ctot_thres) then
       write(log_message,"(A,E11.3e3,A,E11.3e3)") &
            'abs(CISO_Jint_13Ctot)=', abs(diags(ind%CISO_Jint_13Ctot)%field_2d(1)), &
            ' exceeds CISO_Jint_13Ctot_thres=', CISO_Jint_13Ctot_thres
       call marbl_status_log%log_error(log_message, subname, ElemInd=1)
       return
    end if

    ! Vertical integral - CISO_Jint_14Ctot

    work(:) = interior_tendencies(di14c_ind,:) + interior_tendencies(do14ctot_ind,:) + interior_tendencies(zootot14C_ind,:) &
         + sum(interior_tendencies(marbl_tracer_indices%auto_inds(:)%C14_ind,:), dim=1) + decay_14Ctot
    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
       if (n > 0) then
          work = work + interior_tendencies(n,:)
       end if
    end do
    call marbl_diagnostics_share_compute_vertical_integrals(work, delta_z, kmt, &
         full_depth_integral=diags(ind%CISO_Jint_14Ctot)%field_2d(1),           &
         integrated_terms = PO14C%sed_loss + P_Ca14CO3%sed_loss)

    if (abs(diags(ind%CISO_Jint_14Ctot)%field_2d(1)) .gt. CISO_Jint_14Ctot_thres) then
       write(log_message,"(A,E11.3e3,A,E11.3e3)") &
            'abs(CISO_Jint_14Ctot)=', abs(diags(ind%CISO_Jint_14Ctot)%field_2d(1)), &
            ' exceeds CISO_Jint_14Ctot_thres=', CISO_Jint_14Ctot_thres
       call marbl_status_log%log_error(log_message, subname, ElemInd=1)
       return
    end if

    ! Other vertical integrals

    do n = 1,autotroph_cnt
       call marbl_diagnostics_share_compute_vertical_integrals(photo13C(n,:), delta_z, kmt, &
            full_depth_integral=diags(ind%CISO_photo13C_zint(n))%field_2d(1))

       call marbl_diagnostics_share_compute_vertical_integrals(photo14C(n,:), delta_z, kmt, &
            full_depth_integral=diags(ind%CISO_photo14C_zint(n))%field_2d(1))

       if (ind%CISO_Ca13CO3_form_zint(n) .gt. 0) &
         call marbl_diagnostics_share_compute_vertical_integrals(Ca13CO3_prod(n,:), delta_z, kmt, &
              full_depth_integral=diags(ind%CISO_Ca13CO3_form_zint(n))%field_2d(1))

       if (ind%CISO_Ca14CO3_form_zint(n) .gt. 0) &
         call marbl_diagnostics_share_compute_vertical_integrals(Ca14CO3_prod(n,:), delta_z, kmt, &
              full_depth_integral=diags(ind%CISO_Ca14CO3_form_zint(n))%field_2d(1))
    end do

    do k = 1,km
       do n = 1, autotroph_cnt
          diags(ind%CISO_d13C(n))%field_3d(k, 1)                = autotroph_d13C(n,k)
          diags(ind%CISO_d14C(n))%field_3d(k, 1)                = autotroph_d14C(n,k)

          if (ind%CISO_autotrophCaCO3_d13C(n) .gt. 0) &
            diags(ind%CISO_autotrophCaCO3_d13C(n))%field_3d(k, 1) = autotrophCaCO3_d13C(n,k)
          if (ind%CISO_autotrophCaCO3_d14C(n) .gt. 0) &
            diags(ind%CISO_autotrophCaCO3_d14C(n))%field_3d(k, 1) = autotrophCaCO3_d14C(n,k)

          diags(ind%CISO_photo13C(n))%field_3d(k, 1)            = photo13C(n,k)
          diags(ind%CISO_photo14C(n))%field_3d(k, 1)            = photo14C(n,k)

          diags(ind%CISO_eps_autotroph(n))%field_3d(k, 1)       = eps_autotroph(n,k)

          diags(ind%CISO_mui_to_co2star(n))%field_3d(k, 1)      = mui_to_co2star(n,k)

          if (ind%CISO_Ca13CO3_form(n) .gt. 0) &
             diags(ind%CISO_Ca13CO3_form(n))%field_3d(k, 1)     = Ca13CO3_prod(n,k)
          if (ind%CISO_Ca14CO3_form(n) .gt. 0) &
             diags(ind%CISO_Ca14CO3_form(n))%field_3d(k, 1)     = Ca14CO3_prod(n,k)
       end do  ! end loop over autotroph_cnt
    end do  ! end loop over km

    do k = 1,km
       diags(ind%CISO_DIC_d13C)%field_3d(k, 1)        = DIC_d13C(k)
       diags(ind%CISO_DIC_d14C)%field_3d(k, 1)        = DIC_d14C(k)

       diags(ind%CISO_DOCtot_d13C)%field_3d(k, 1)     = DOCtot_d13C(k)
       diags(ind%CISO_DOCtot_d14C)%field_3d(k, 1)     = DOCtot_d14C(k)

       diags(ind%CISO_DO13Ctot_prod)%field_3d(k, 1)   = DO13Ctot_prod(k)
       diags(ind%CISO_DO14Ctot_prod)%field_3d(k, 1)   = DO14Ctot_prod(k)

       diags(ind%CISO_DO13Ctot_remin)%field_3d(k, 1)  = DO13Ctot_remin(k)
       diags(ind%CISO_DO14Ctot_remin)%field_3d(k, 1)  = DO14Ctot_remin(k)

       diags(ind%CISO_zoototC_d13C)%field_3d(k, 1)    = zoototC_d13C(k)
       diags(ind%CISO_zoototC_d14C)%field_3d(k, 1)    = zoototC_d14C(k)

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

end module marbl_ciso_diagnostics_mod
