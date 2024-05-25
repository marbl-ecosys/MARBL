module marbl_init_tracer_metadata_mod

  use marbl_kinds_mod, only : int_kind

  use marbl_interface_public_types, only : marbl_tracer_metadata_type

  use marbl_interface_private_types, only : marbl_tracer_index_type

  use marbl_settings_mod, only : base_bio_on
  use marbl_settings_mod, only : abio_dic_on
  use marbl_settings_mod, only : ciso_on
  use marbl_settings_mod, only : autotroph_cnt
  use marbl_settings_mod, only : zooplankton_cnt
  use marbl_settings_mod, only : unit_system_type

  implicit none
  private

  public :: marbl_init_tracer_metadata

  contains

  subroutine marbl_init_tracer_metadata(unit_system, marbl_tracer_indices, marbl_tracer_metadata)

    !  Set tracer and forcing metadata

    use marbl_settings_mod, only : lecovars_full_depth_tavg

    type(unit_system_type),           intent(in)  :: unit_system
    type(marbl_tracer_index_type),    intent(in)  :: marbl_tracer_indices
    type(marbl_tracer_metadata_type), intent(out) :: marbl_tracer_metadata(:)   ! descriptors for each tracer

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    integer(int_kind) :: n        ! index for looping over tracers
    integer(int_kind) :: zoo_ind  ! zooplankton functional group index
    integer(int_kind) :: auto_ind ! autotroph functional group index

    !-----------------------------------------------------------------------
    ! initialize tracer metatdata
    !-----------------------------------------------------------------------

    marbl_tracer_metadata(:)%lfull_depth_tavg   = .true.

    if (base_bio_on) then
      call init_non_autotroph_tracer_metadata('PO4', 'Dissolved Inorganic Phosphate', 'base_bio', &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%po4_ind))
      call init_non_autotroph_tracer_metadata('NO3', 'Dissolved Inorganic Nitrate', 'base_bio',   &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%no3_ind))
      call init_non_autotroph_tracer_metadata('SiO3', 'Dissolved Inorganic Silicate', 'base_bio', &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%sio3_ind))
      call init_non_autotroph_tracer_metadata('NH4', 'Dissolved Ammonia', 'base_bio',             &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%nh4_ind))
      call init_non_autotroph_tracer_metadata('Fe', 'Dissolved Inorganic Iron', 'base_bio',       &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%fe_ind))
      call init_non_autotroph_tracer_metadata('Lig', 'Iron Binding Ligand', 'base_bio',           &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%lig_ind))
      call init_non_autotroph_tracer_metadata('O2', 'Dissolved Oxygen', 'base_bio',               &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%o2_ind))
      call init_non_autotroph_tracer_metadata('DIC', 'Dissolved Inorganic Carbon', 'base_bio',    &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%dic_ind))
      call init_non_autotroph_tracer_metadata('ALK', 'Alkalinity', 'base_bio',                    &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%alk_ind))
      call init_non_autotroph_tracer_metadata('DOC', 'Dissolved Organic Carbon', 'base_bio',      &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%doc_ind))
      call init_non_autotroph_tracer_metadata('DON', 'Dissolved Organic Nitrogen', 'base_bio',    &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%don_ind))
      call init_non_autotroph_tracer_metadata('DOP', 'Dissolved Organic Phosphorus', 'base_bio',  &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%dop_ind))
      call init_non_autotroph_tracer_metadata('DOPr', 'Refractory DOP', 'base_bio',               &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%dopr_ind))
      call init_non_autotroph_tracer_metadata('DONr', 'Refractory DON', 'base_bio',               &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%donr_ind))
      call init_non_autotroph_tracer_metadata('DOCr', 'Refractory DOC', 'base_bio',               &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%docr_ind))
      call init_non_autotroph_tracer_metadata('DIC_ALT_CO2', 'Dissolved Inorganic Carbon, Alternative CO2', &
                'base_bio', unit_system, marbl_tracer_metadata(marbl_tracer_indices%dic_alt_co2_ind))
      call init_non_autotroph_tracer_metadata('ALK_ALT_CO2', 'Alkalinity, Alternative CO2', &
                'base_bio', unit_system, marbl_tracer_metadata(marbl_tracer_indices%alk_alt_co2_ind))

      call init_zooplankton_tracer_metadata(marbl_tracer_metadata,        &
           marbl_tracer_indices, unit_system)

      call init_autotroph_tracer_metadata(marbl_tracer_metadata,          &
           marbl_tracer_indices, unit_system)

      !-----------------------------------------------------------------------
      !  set lfull_depth_tavg flag for short-lived ecosystem tracers
      !-----------------------------------------------------------------------

      ! Should be done in marbl_diagnostics, and without the _tavg name
      do zoo_ind = 1, zooplankton_cnt
        n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
        marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
        marbl_tracer_metadata(n)%tracer_module_name = 'base_bio'
      end do

      do auto_ind = 1, autotroph_cnt
        n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
        marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
        marbl_tracer_metadata(n)%tracer_module_name = 'base_bio'

        n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
        marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
        marbl_tracer_metadata(n)%tracer_module_name = 'base_bio'

        n = marbl_tracer_indices%auto_inds(auto_ind)%N_ind
        if (n > 0) then
            marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
            marbl_tracer_metadata(n)%tracer_module_name = 'base_bio'
        endif

        n = marbl_tracer_indices%auto_inds(auto_ind)%P_ind
        if (n > 0) then
            marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
            marbl_tracer_metadata(n)%tracer_module_name = 'base_bio'
        endif

        n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
        marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
        marbl_tracer_metadata(n)%tracer_module_name = 'base_bio'

        n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
        if (n > 0) then
            marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
            marbl_tracer_metadata(n)%tracer_module_name = 'base_bio'
          endif

        n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
        if (n > 0) then
            marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
            marbl_tracer_metadata(n)%tracer_module_name = 'base_bio'
          endif
      end do
    end if

    if (ciso_on) then
      call init_non_autotroph_tracer_metadata('DI13C', 'Dissolved Inorganic Carbon-13', 'ciso',    &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%di13c_ind))
      call init_non_autotroph_tracer_metadata('DO13Ctot', 'Dissolved Organic Carbon-13 (semi-labile+refractory)', &
                'ciso', unit_system, marbl_tracer_metadata(marbl_tracer_indices%do13ctot_ind))
      call init_non_autotroph_tracer_metadata('zootot13C', 'Zooplankton Carbon-13 (sum over all zooplankton)', &
                'ciso', unit_system, marbl_tracer_metadata(marbl_tracer_indices%zootot13C_ind))
      call init_non_autotroph_tracer_metadata('DI14C', 'Dissolved Inorganic Carbon-14', 'ciso',    &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%di14c_ind))
      call init_non_autotroph_tracer_metadata('DO14Ctot', 'Dissolved Organic Carbon-14 (semi-labile+refractory)', &
                'ciso', unit_system, marbl_tracer_metadata(marbl_tracer_indices%do14ctot_ind))
      call init_non_autotroph_tracer_metadata('zootot14C', 'Zooplankton Carbon-14 (sum over all zooplankton)', &
                'ciso', unit_system, marbl_tracer_metadata(marbl_tracer_indices%zootot14C_ind))
    end if

    if (abio_dic_on) then
      call init_non_autotroph_tracer_metadata('ABIO_DIC', 'Abiotic Dissolved Inorganic Carbon', 'abio_dic', &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%abio_dic_ind))
      call init_non_autotroph_tracer_metadata('ABIO_DI14C', 'Abiotic Dissolved Inorganic Carbon-14', 'abio_dic', &
                unit_system, marbl_tracer_metadata(marbl_tracer_indices%abio_di14c_ind))
    end if

  end subroutine marbl_init_tracer_metadata

  !***********************************************************************

  subroutine init_non_autotroph_tracer_metadata(short_name, long_name, tracer_module_name, unit_system, &
                                                      marbl_tracer_metadata)

    !-----------------------------------------------------------------------
    !  initialize non-autotroph tracer_d values and accumulate
    !  non_living_biomass_ecosys_tracer_cnt
    !-----------------------------------------------------------------------

    character(len=*),                 intent(in)    :: short_name
    character(len=*),                 intent(in)    :: long_name
    character(len=*),                 intent(in)    :: tracer_module_name
    type(unit_system_type),           intent(in)    :: unit_system
    type(marbl_tracer_metadata_type), intent(inout) :: marbl_tracer_metadata

    marbl_tracer_metadata%short_name = short_name
    marbl_tracer_metadata%long_name  = long_name
    marbl_tracer_metadata%tracer_module_name = tracer_module_name
    if ((trim(short_name) == "ALK") .or. &
        (trim(short_name) == "ALK_ALT_CO2")) then
       marbl_tracer_metadata%units      = unit_system%alk_conc_units
       marbl_tracer_metadata%tend_units = unit_system%alk_conc_tend_units
       marbl_tracer_metadata%flux_units = unit_system%alk_conc_flux_units
    else
       marbl_tracer_metadata%units      = unit_system%conc_units
       marbl_tracer_metadata%tend_units = unit_system%conc_tend_units
       marbl_tracer_metadata%flux_units = unit_system%conc_flux_units
    endif

  end subroutine init_non_autotroph_tracer_metadata

  !***********************************************************************

  subroutine init_zooplankton_tracer_metadata(marbl_tracer_metadata, &
             marbl_tracer_indices, unit_system)

    !-----------------------------------------------------------------------
    !  initialize zooplankton tracer_d values and tracer indices
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : zooplankton_settings

    type(marbl_tracer_metadata_type), intent(inout) :: marbl_tracer_metadata(:)    ! descriptors for each tracer
    type(marbl_tracer_index_type),    intent(in)    :: marbl_tracer_indices
    type(unit_system_type),           intent(in)    :: unit_system

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n, zoo_ind            ! zooplankton functional group index
    !-----------------------------------------------------------------------

    do zoo_ind = 1, zooplankton_cnt
       n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
       marbl_tracer_metadata(n)%short_name = trim(zooplankton_settings(zoo_ind)%sname) // 'C'
       marbl_tracer_metadata(n)%long_name  = trim(zooplankton_settings(zoo_ind)%lname) // ' Carbon'
       marbl_tracer_metadata(n)%units      = unit_system%conc_units
       marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
       marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units
    end do

  end subroutine init_zooplankton_tracer_metadata

  !***********************************************************************

  subroutine init_autotroph_tracer_metadata(marbl_tracer_metadata,      &
             marbl_tracer_indices, unit_system)

    !-----------------------------------------------------------------------
    !  initialize autotroph tracer_d values and tracer indices
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : autotroph_settings

    type(marbl_tracer_metadata_type), intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type(marbl_tracer_index_type),    intent(in)    :: marbl_tracer_indices
    type(unit_system_type),           intent(in)    :: unit_system

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n, auto_ind
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
       marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Chl'
       marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Chlorophyll'
       marbl_tracer_metadata(n)%units      = 'mg/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mg/m^3/s'
       if (unit_system%unit_system == 'cgs') then
          marbl_tracer_metadata(n)%flux_units = 'mg/m^3 cm/s'
       else
          marbl_tracer_metadata(n)%flux_units = 'mg/m^2/s'
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
       marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'C'
       marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Carbon'
       marbl_tracer_metadata(n)%units      = unit_system%conc_units
       marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
       marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units

       n = marbl_tracer_indices%auto_inds(auto_ind)%N_ind
       if (n.gt.0) then
           marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'N'
           marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Nitrogen'
           marbl_tracer_metadata(n)%units      = unit_system%conc_units
           marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
           marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%P_ind
       if (n.gt.0) then
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'P'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Phosphorus'
          marbl_tracer_metadata(n)%units      = unit_system%conc_units
          marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
          marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
       marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Fe'
       marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Iron'
       marbl_tracer_metadata(n)%units      = unit_system%conc_units
       marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
       marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units

       n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
       if (n .gt. 0) then
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Si'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Silicon'
          marbl_tracer_metadata(n)%units      = unit_system%conc_units
          marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
          marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n .gt. 0) then
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'CaCO3'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' CaCO3'
          marbl_tracer_metadata(n)%units      = unit_system%conc_units
          marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
          marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units
       endif

       ! ciso tracers
       if (ciso_on) then
          n = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // '13C'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Carbon-13'
          marbl_tracer_metadata(n)%units      = unit_system%conc_units
          marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
          marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units

          n = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // '14C'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Carbon-14'
          marbl_tracer_metadata(n)%units      = unit_system%conc_units
          marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
          marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units

          n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
          if (n .gt. 0) then
             marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Ca13CO3'
             marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Ca13CO3'
             marbl_tracer_metadata(n)%units      = unit_system%conc_units
             marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
             marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units
          end if

          n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
          if (n .gt. 0) then
             marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Ca14CO3'
             marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Ca14CO3'
             marbl_tracer_metadata(n)%units      = unit_system%conc_units
             marbl_tracer_metadata(n)%tend_units = unit_system%conc_tend_units
             marbl_tracer_metadata(n)%flux_units = unit_system%conc_flux_units
          end if
       end if

    end do

  end subroutine init_autotroph_tracer_metadata

  !***********************************************************************

end module marbl_init_tracer_metadata_mod
