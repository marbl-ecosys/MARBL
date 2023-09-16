module marbl_abio_dic_diagnostics_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_settings_mod, only : unit_system_type

  use marbl_interface_public_types, only : marbl_diagnostics_type

  use marbl_logging, only : marbl_log_type
  use marbl_logging, only : marbl_logging_add_diagnostics_error

  use marbl_diagnostics_share_mod, only : marbl_surface_flux_diag_ind

  implicit none
  private

  public :: marbl_abio_dic_diagnostics_init
  public :: marbl_abio_dic_diagnostics_surface_flux_compute

contains

  !***********************************************************************

  subroutine marbl_abio_dic_diagnostics_init( &
       unit_system, &
       marbl_surface_flux_diags,  &
       marbl_status_log)

    use marbl_settings_mod, only : abio_dic_on

    type(unit_system_type),       intent(in)    :: unit_system
    type(marbl_diagnostics_type), intent(inout) :: marbl_surface_flux_diags
    type(marbl_log_type),         intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_abio_dic_diagnostics_mod:marbl_abio_dic_diagnostics_init'
    integer :: n
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

    associate(                                    &
              ind => marbl_surface_flux_diag_ind, &
              diags => marbl_surface_flux_diags   &
             )

      lname    = 'Ice Fraction for Abiotic DIC tracer fluxes'
      sname    = 'ABIO_IFRAC'
      units    = 'fraction'
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_IFRAC, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'XKW for Abiotic DIC tracer fluxes'
      sname    = 'ABIO_XKW'
      units    = vel_units
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_XKW, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'Surface Alkalinity for Abiotic DIC tracer fluxes'
      sname    = 'ABIO_ALK_SURF'
      units    = unit_system%alk_conc_units
      vgrid    = 'none'
      truncate = .false.
      call diags%add_diagnostic(lname, sname, units, vgrid, truncate,  &
          ind%ABIO_XKW, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
       call marbl_logging_add_diagnostics_error(marbl_status_log, sname, subname)
       return
      end if

      lname    = 'CO2 atmospheric partial pressure for Abiotic DIC tracer fluxes'
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

      lname    = 'Surface pCO2 for Abiotic DIC tracer fluxes'
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

      lname    = 'D CO2 Star for Abiotic DIC tracer fluxes'
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

      lname    = 'CO2 Star for Abiotic DIC tracer fluxes'
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

    end associate

  end subroutine marbl_abio_dic_diagnostics_init

  !***********************************************************************

  subroutine marbl_abio_dic_diagnostics_surface_flux_compute( &
       num_elements, &
       ifrac, &
       xkw, &
       alk_surf, &
       pco2, &
       pco2surf, &
       dco2star, &
       co2star, &
       marbl_surface_flux_diags)

    ! !DESCRIPTION:
    !  Compute surface fluxes for ecosys tracer module.

    use marbl_constants_mod, only : R13C_std, R14C_std

    integer (int_kind),                 intent(in)    :: num_elements
    real (r8), dimension(num_elements), intent(in)    :: ifrac
    real (r8), dimension(num_elements), intent(in)    :: xkw
    real (r8), dimension(num_elements), intent(in)    :: alk_surf
    real (r8), dimension(num_elements), intent(in)    :: pco2
    real (r8), dimension(num_elements), intent(in)    :: pco2surf
    real (r8), dimension(num_elements), intent(in)    :: dco2star
    real (r8), dimension(num_elements), intent(in)    :: co2star
    type(marbl_diagnostics_type),       intent(inout) :: marbl_surface_flux_diags

    associate(                                       &
         diags => marbl_surface_flux_diags%diags,    &
         ind   => marbl_surface_flux_diag_ind        &
         )

    diags(ind%ABIO_IFRAC)%field_2d(:) = ifrac(:)
    diags(ind%ABIO_XKW)%field_2d(:) = xkw(:)
    diags(ind%ABIO_ALK_SURF)%field_2d(:) = alk_surf(:)
    diags(ind%ABIO_pCO2)%field_2d(:) = pco2(:)
    diags(ind%ABIO_pCO2SURF)%field_2d(:) = pco2surf(:)
    diags(ind%ABIO_DCO2STAR)%field_2d(:) = dco2star(:)
    diags(ind%ABIO_CO2STAR)%field_2d(:) = co2star(:)

    end associate

  end subroutine marbl_abio_dic_diagnostics_surface_flux_compute

  !***********************************************************************

end module marbl_abio_dic_diagnostics_mod
