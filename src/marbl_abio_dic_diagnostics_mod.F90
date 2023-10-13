module marbl_abio_dic_diagnostics_mod

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_settings_mod, only : unit_system_type
  use marbl_settings_mod, only : labio_derivative_diags

  use marbl_interface_public_types, only : marbl_diagnostics_type


  use marbl_logging, only : marbl_log_type
  use marbl_logging, only : marbl_logging_add_diagnostics_error

  use marbl_diagnostics_share_mod, only : marbl_interior_tendency_diag_ind
  use marbl_diagnostics_share_mod, only : marbl_surface_flux_diag_ind

  implicit none
  private

  public :: marbl_abio_dic_diagnostics_init
  public :: marbl_abio_dic_diagnostics_surface_flux_compute
  public :: marbl_abio_dic_diagnostics_interior_tendency_compute

contains

  !***********************************************************************

  subroutine marbl_abio_dic_diagnostics_init( &
       unit_system, &
       marbl_interior_tendency_diags, &
       marbl_surface_flux_diags, &
       marbl_status_log)

    use marbl_settings_mod, only : abio_dic_on

    type(unit_system_type),       intent(in)    :: unit_system
    type(marbl_diagnostics_type), intent(inout) :: marbl_interior_tendency_diags
    type(marbl_diagnostics_type), intent(inout) :: marbl_surface_flux_diags
    type(marbl_log_type),         intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_abio_dic_diagnostics_mod:marbl_abio_dic_diagnostics_init'
    logical :: truncate
    character(len=char_len) :: lname, sname, units, vgrid
    character(len=char_len) :: vel_units

    if (.not. abio_dic_on) return

    !-----------------------------------------------------------------
    ! Units for some diagnostics depend on unit system
    !-----------------------------------------------------------------
    write(vel_units, "(2A)") trim(unit_system%L), '/s'

    !-----------------------------------------------------------------
    ! Surface forcing diagnostics
    !-----------------------------------------------------------------

    associate(&
              ind => marbl_surface_flux_diag_ind, &
              diags => marbl_surface_flux_diags &
             )

      lname    = 'CO2 Atmospheric Partial Pressure for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_pCO2'
      units    = 'ppm'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_pCO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Atmospheric Delta 14C for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_D14C_atm'
      units    = 'permil'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_D14C_atm, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'CO2 Star for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_CO2STAR'
      units    = unit_system%conc_units
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_CO2STAR, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'D CO2 Star for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_DCO2STAR'
      units    = unit_system%conc_units
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_DCO2STAR, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Surface pCO2 for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_pCO2SURF'
      units    = 'ppmv'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_pCO2SURF, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'D pCO2 for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_DpCO2'
      units    = 'ppmv'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_DpCO2, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Surface pH for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_PH'
      units    = '1'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_PH, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Surface Alkalinity for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_ALK_SURF'
      units    = unit_system%alk_conc_units
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_ALK_SURF, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Surface Gas Flux of Abiotic DIC'
      sname    = 'ABIO_FG_DIC'
      units    = unit_system%conc_flux_units
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
           ind%ABIO_FG_DIC, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Surface Gas Flux of Abiotic DI14C'
      sname    = 'ABIO_FG_DI14C'
      units    = unit_system%conc_flux_units
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_FG_DI14C, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      if (labio_derivative_diags) then
        lname    = 'Derivative of ABIO_FG_DIC wrt ABIO_DIC'
        sname    = 'd_SF_ABIO_DIC_d_ABIO_DIC'
        units    = vel_units
        vgrid    = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%d_SF_ABIO_DIC_d_ABIO_DIC, marbl_status_log)
        if (marbl_status_log%labort_marbl) then
         call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
         return
        end if

        lname    = 'Derivative of ABIO_FG_DI14C wrt ABIO_DIC'
        sname    = 'd_SF_ABIO_DI14C_d_ABIO_DIC'
        units    = vel_units
        vgrid    = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%d_SF_ABIO_DI14C_d_ABIO_DIC, marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if

        lname    = 'Derivative of ABIO_FG_DI14C wrt ABIO_DI14C'
        sname    = 'd_SF_ABIO_DI14C_d_ABIO_DI14C'
        units    = vel_units
        vgrid    = 'none'
        truncate = .false.
        call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
             ind%d_SF_ABIO_DI14C_d_ABIO_DI14C, marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
          return
        end if
      end if

    end associate

    !-----------------------------------------------------------------
    ! Interior tendency diagnostics
    !-----------------------------------------------------------------

    associate(&
              ind => marbl_interior_tendency_diag_ind, &
              diags => marbl_interior_tendency_diags &
             )

      lname    = 'Oceanic Delta 14C for Abiotic DIC Tracer Fluxes'
      sname    = 'ABIO_D14C_ocn'
      units    = 'permil'
      vgrid    = 'layer_avg'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
           ind%ABIO_D14C_ocn, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      end associate

    end subroutine marbl_abio_dic_diagnostics_init

  !***********************************************************************

  subroutine marbl_abio_dic_diagnostics_surface_flux_compute( &
       xco2, &
       d14c, &
       co2star, &
       dco2star, &
       pco2surf, &
       dpco2, &
       ph_surf, &
       alk_surf, &
       fg_dic, &
       fg_di14c, &
       derivative_terms, &
       marbl_surface_flux_diags)

    ! !DESCRIPTION:
    !  Compute surface fluxes for ecosys tracer module.

    real (r8), dimension(:),      intent(in)    :: xco2
    real (r8), dimension(:),      intent(in)    :: d14c
    real (r8), dimension(:),      intent(in)    :: co2star
    real (r8), dimension(:),      intent(in)    :: dco2star
    real (r8), dimension(:),      intent(in)    :: pco2surf
    real (r8), dimension(:),      intent(in)    :: dpco2
    real (r8), dimension(:),      intent(in)    :: ph_surf
    real (r8), dimension(:),      intent(in)    :: alk_surf
    real (r8), dimension(:),      intent(in)    :: fg_dic
    real (r8), dimension(:),      intent(in)    :: fg_di14c
    real (r8), dimension(:,:),    intent(in)    :: derivative_terms
    type(marbl_diagnostics_type), intent(inout) :: marbl_surface_flux_diags

    associate(                                       &
         diags => marbl_surface_flux_diags%diags,    &
         ind   => marbl_surface_flux_diag_ind        &
         )

    diags(ind%ABIO_pCO2)%field_2d(:) = xco2(:)
    diags(ind%ABIO_D14C_atm)%field_2d(:) = d14c(:)
    diags(ind%ABIO_CO2STAR)%field_2d(:) = co2star(:)
    diags(ind%ABIO_DCO2STAR)%field_2d(:) = dco2star(:)
    diags(ind%ABIO_pCO2SURF)%field_2d(:) = pco2surf(:)
    diags(ind%ABIO_DpCO2)%field_2d(:) = dpco2(:)
    diags(ind%ABIO_PH)%field_2d(:) = ph_surf(:)
    diags(ind%ABIO_ALK_SURF)%field_2d(:) = alk_surf(:)
    diags(ind%ABIO_FG_DIC)%field_2d(:) = fg_dic(:)
    diags(ind%ABIO_FG_DI14C)%field_2d(:) = fg_di14c(:)
    if (labio_derivative_diags) then
      diags(ind%d_SF_ABIO_DIC_d_ABIO_DIC)%field_2d(:) = derivative_terms(:,1)
      diags(ind%d_SF_ABIO_DI14C_d_ABIO_DIC)%field_2d(:) = derivative_terms(:,2)
      diags(ind%d_SF_ABIO_DI14C_d_ABIO_DI14C)%field_2d(:) = derivative_terms(:,3)
    end if

    end associate

  end subroutine marbl_abio_dic_diagnostics_surface_flux_compute

  !*****************************************************************************

  subroutine marbl_abio_dic_diagnostics_interior_tendency_compute(dic, di14c, interior_tendency_diags)

 !---------------------------------------------------------------------
 ! !DESCRIPTION:
 !  Update marbl_interior_abio_dic_diags data type
 !---------------------------------------------------------------------

    real(r8),                      intent(in)    :: dic(:)
    real(r8),                      intent(in)    :: di14c(:)
    type(marbl_diagnostics_type),  intent(inout) :: interior_tendency_diags

    associate(&
        ! diagnostics
        diags => interior_tendency_diags%diags, &
        ind   => marbl_interior_tendency_diag_ind &
       )

      where (dic(:) == c0)
        diags(ind%ABIO_D14C_ocn)%field_3d(:,1) = c0
      elsewhere
        diags(ind%ABIO_D14C_ocn)%field_3d(:,1) = (di14c(:)/dic(:) - c1) * 1000._r8
      end where
    end associate

end subroutine marbl_abio_dic_diagnostics_interior_tendency_compute

  !***********************************************************************

end module marbl_abio_dic_diagnostics_mod
