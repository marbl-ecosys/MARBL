module marbl_init_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : char_len

  use marbl_interface_public_types, only : marbl_tracer_metadata_type
  use marbl_interface_public_types, only : marbl_forcing_fields_type

  use marbl_interface_private_types, only : marbl_tracer_index_type

  use marbl_logging, only : marbl_log_type

  use marbl_settings_mod, only : autotroph_cnt
  use marbl_settings_mod, only : zooplankton_cnt

  implicit none
  private

  public :: marbl_init_log_and_timers
  public :: marbl_init_parameters_pre_tracers
  public :: marbl_init_parameters_post_tracers
  public :: marbl_init_tracers
  public :: marbl_init_bury_coeff
  public :: marbl_init_forcing_fields

contains

  !***********************************************************************

  subroutine marbl_init_log_and_timers(marbl_timers, timer_ids, marbl_status_log)

    use marbl_interface_private_types, only : marbl_internal_timers_type
    use marbl_interface_private_types, only : marbl_timer_indexing_type

    type(marbl_internal_timers_type), intent(out)   :: marbl_timers
    type(marbl_timer_indexing_type),  intent(out)   :: timer_ids
    type(marbl_log_type),             intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_log_and_timers'

    ! Construct status log
    call marbl_status_log%construct()

    ! Set up timers
    call marbl_timers%setup(timer_ids, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("setup_timers()", subname)
      return
    end if

  end subroutine marbl_init_log_and_timers

  !***********************************************************************

  subroutine marbl_init_parameters_pre_tracers(marbl_settings, marbl_status_log)

    use marbl_settings_mod, only : marbl_settings_type
    use marbl_settings_mod, only : marbl_settings_set_defaults_general_parms
    use marbl_settings_mod, only : marbl_settings_define_general_parms
    use marbl_settings_mod, only : marbl_settings_set_defaults_PFT_counts
    use marbl_settings_mod, only : marbl_settings_define_PFT_counts
    use marbl_settings_mod, only : marbl_settings_set_defaults_PFT_derived_types
    use marbl_settings_mod, only : marbl_settings_define_PFT_derived_types

    type(marbl_settings_type),  intent(inout) :: marbl_settings
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    ! local variables
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_parameters_pre_tracers'

    !---------------------------------------------------------------------------
    ! set default values for basic settings
    !---------------------------------------------------------------------------

    call marbl_settings_set_defaults_general_parms()

    !---------------------------------------------------------------------------
    ! Add general settings to list of allowable put / get vars
    !---------------------------------------------------------------------------

    call marbl_settings_define_general_parms(marbl_settings, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("marbl_settings_define_general_parms()", subname)
      return
    end if

    !---------------------------------------------------------------------------
    ! Add PFT counts to list of allowable put / get vars
    !---------------------------------------------------------------------------

    call marbl_settings_set_defaults_PFT_counts(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("marbl_settings_set_defaults_PFT_counts()", subname)
      return
    end if

    call marbl_settings_define_PFT_counts(marbl_settings, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("marbl_settings_define_PFT_counts()", subname)
      return
    end if

    !---------------------------------------------------------------------------
    ! Add components of PFT derived types to list of allowable put / get vars
    !---------------------------------------------------------------------------

    call marbl_settings_set_defaults_PFT_derived_types(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("marbl_settings_set_defaults_PFT_derived_types()", subname)
      return
    end if

    call marbl_settings_define_PFT_derived_types(marbl_settings, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("marbl_settings_define_PFT_derived_types()", subname)
      return
    end if

  end subroutine marbl_init_parameters_pre_tracers

  !***********************************************************************

  ! FIXME #81: Will need to pass num_elements_interior_tendency to this routine
  !            (And add a dimension to tracers to count elements)
  subroutine marbl_init_tracers(num_levels, &
                                num_elements_surface_flux, &
                                tracer_indices, &
                                tracers_at_surface, &
                                surface_fluxes, &
                                tracers, &
                                interior_tendencies, &
                                tracer_metadata, &
                                marbl_status_log)

    use marbl_settings_mod, only : ciso_on
    use marbl_settings_mod, only : lvariable_PtoC
    use marbl_settings_mod, only : autotroph_settings
    use marbl_settings_mod, only : zooplankton_settings
    use marbl_settings_mod, only : tracer_restore_vars
    use marbl_ciso_init_mod, only : marbl_ciso_init_tracer_metadata

    integer(int_kind),                             intent(in)    :: num_levels
    integer(int_kind),                             intent(in)    :: num_elements_surface_flux
    type(marbl_tracer_index_type),    pointer,     intent(out)   :: tracer_indices
    real(r8),                         allocatable, intent(out)   :: tracers_at_surface(:,:)
    real(r8),                         allocatable, intent(out)   :: surface_fluxes(:,:)
    real(r8),                         allocatable, intent(out)   :: tracers(:,:)
    real(r8),                         allocatable, intent(out)   :: interior_tendencies(:,:)
    type(marbl_tracer_metadata_type), allocatable, intent(out)   :: tracer_metadata(:)
    type(marbl_log_type),                          intent(inout) :: marbl_status_log

    ! local variables
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_tracers'
    character(len=char_len) :: log_message
    integer :: i

    ! Construct tracer indices
    allocate(tracer_indices)
    call tracer_indices%construct(ciso_on, lvariable_PtoC, autotroph_settings, zooplankton_settings, &
                                  marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("tracer_indices%construct", subname)
      return
    end if

    ! Allocate memory for tracers
    allocate(tracers_at_surface(num_elements_surface_flux, tracer_indices%total_cnt))
    allocate(surface_fluxes(num_elements_surface_flux, tracer_indices%total_cnt))
    allocate(tracers(tracer_indices%total_cnt, num_levels))
    allocate(interior_tendencies(tracer_indices%total_cnt, num_levels))
    allocate(tracer_metadata(tracer_indices%total_cnt))
    if (.not.allocated(tracer_restore_vars)) &
      allocate(tracer_restore_vars(tracer_indices%total_cnt))

    ! Set up tracer metadata
    call marbl_init_tracer_metadata(tracer_metadata, tracer_indices)
    call marbl_ciso_init_tracer_metadata(tracer_metadata, tracer_indices)

    ! Log what tracers are being used
    call marbl_status_log%log_header('MARBL Tracer indices', subname)
    do i=1,tracer_indices%total_cnt
      write(log_message, "(I3,2A)") i, '. ', trim(tracer_metadata(i)%short_name)
      call marbl_status_log%log_noerror(log_message, subname)
    end do

100 format(A, ' tracer module contains ', I0, ' tracers; indices are ', I0, ' to ', I0)
    if (tracer_indices%ecosys_base%cnt.gt.0) then
      write(log_message, 100) 'ecosys_base', &
                              tracer_indices%ecosys_base%cnt, &
                              tracer_indices%ecosys_base%ind_beg, &
                              tracer_indices%ecosys_base%ind_end
      call marbl_status_log%log_noerror(log_message, subname)
    end if
    if (tracer_indices%ciso%cnt.gt.0) then
      write(log_message, 100) 'ciso', &
                              tracer_indices%ciso%cnt, &
                              tracer_indices%ciso%ind_beg, &
                              tracer_indices%ciso%ind_end
      call marbl_status_log%log_noerror(log_message, subname)
    end if

  end subroutine marbl_init_tracers

  !***********************************************************************

  subroutine marbl_init_tracer_metadata(marbl_tracer_metadata, marbl_tracer_indices)

    !  Set tracer and forcing metadata

    use marbl_settings_mod, only : lecovars_full_depth_tavg

    type (marbl_tracer_metadata_type), intent(out)   :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type(marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    integer (int_kind) :: n        ! index for looping over tracers
    integer (int_kind) :: zoo_ind  ! zooplankton functional group index
    integer (int_kind) :: auto_ind ! autotroph functional group index

    !-----------------------------------------------------------------------
    ! initialize tracer metatdata
    !-----------------------------------------------------------------------

    marbl_tracer_metadata(:)%lfull_depth_tavg   = .true.
    marbl_tracer_metadata(:)%tracer_module_name = 'ecosys'

    call marbl_init_non_autotroph_tracers_metadata(marbl_tracer_metadata,     &
         marbl_tracer_indices)

    call marbl_init_zooplankton_tracer_metadata(marbl_tracer_metadata,        &
         marbl_tracer_indices)

    call marbl_init_autotroph_tracer_metadata(marbl_tracer_metadata,          &
         marbl_tracer_indices)

    !-----------------------------------------------------------------------
    !  set lfull_depth_tavg flag for short-lived ecosystem tracers
    !-----------------------------------------------------------------------

    ! Should be done in marbl_diagnostics, and without the _tavg name
    do zoo_ind = 1, zooplankton_cnt
       n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
    end do

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%P_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
       endif
    end do

  end subroutine marbl_init_tracer_metadata

  !***********************************************************************

  subroutine marbl_init_parameters_post_tracers(marbl_settings, marbl_status_log)

    use marbl_settings_mod, only : marbl_settings_type
    use marbl_settings_mod, only : marbl_settings_set_defaults_tracer_dependent
    use marbl_settings_mod, only : marbl_settings_define_tracer_dependent

    type(marbl_settings_type),  intent(inout) :: marbl_settings
    type(marbl_log_type),       intent(inout) :: marbl_status_log

    ! local variables
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_parameters_post_tracers'

    ! set default values for parameters
    call marbl_settings_set_defaults_tracer_dependent(marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("marbl_settings_set_defaults_tracer_dependent()", subname)
      return
    end if

    ! construct parameters_type
    call marbl_settings_define_tracer_dependent(marbl_settings, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("marbl_settings_define_tracer_dependent()", subname)
      return
    end if

  end subroutine marbl_init_parameters_post_tracers

  !***********************************************************************

  subroutine marbl_init_bury_coeff(marbl_particulate_share, marbl_status_log)

    use marbl_logging, only : marbl_log_type
    use marbl_settings_mod, only : init_bury_coeff_opt
    use marbl_settings_mod, only : ladjust_bury_coeff
    use marbl_settings_mod, only : parm_init_POC_bury_coeff
    use marbl_settings_mod, only : parm_init_POP_bury_coeff
    use marbl_settings_mod, only : parm_init_bSi_bury_coeff
    use marbl_interface_private_types, only : marbl_particulate_share_type

    type(marbl_particulate_share_type), intent(inout) :: marbl_particulate_share
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_bury_coeff'

    !---------------------------------------------------------------------------

    ! if ladjust_bury_coeff is true, then bury coefficients are set at runtime
    ! so they do not need to be initialized here

    if (.not. ladjust_bury_coeff) then
       if (init_bury_coeff_opt == 'settings_file') then
          marbl_particulate_share%POC_bury_coeff = parm_init_POC_bury_coeff
          marbl_particulate_share%POP_bury_coeff = parm_init_POP_bury_coeff
          marbl_particulate_share%bSi_bury_coeff = parm_init_bSi_bury_coeff
       else
          call marbl_status_log%log_error("ladjust_bury_coeff=.false., init_bury_coeff_opt='GCM' not implemented", subname)
          return
       end if
    end if

  end subroutine marbl_init_bury_coeff

  !***********************************************************************

  subroutine marbl_init_forcing_fields(domain, &
                                       tracer_metadata, &
                                       surface_flux_forcing_ind, &
                                       surface_flux_forcings, &
                                       interior_tendency_forcing_ind, &
                                       interior_tendency_forcings, &
                                       marbl_status_log)

    use marbl_interface_public_types, only : marbl_domain_type
    use marbl_interface_private_types, only : marbl_surface_flux_forcing_indexing_type
    use marbl_interface_private_types, only : marbl_interior_tendency_forcing_indexing_type
    use marbl_settings_mod, only : ciso_on
    use marbl_settings_mod, only : lflux_gas_o2
    use marbl_settings_mod, only : lflux_gas_co2
    use marbl_settings_mod, only : ladjust_bury_coeff
    use marbl_settings_mod, only : tracer_restore_vars

    type(marbl_domain_type),                             intent(in)    :: domain
    type(marbl_tracer_metadata_type),                    intent(in)    :: tracer_metadata(:)
    type(marbl_surface_flux_forcing_indexing_type),      intent(out)   :: surface_flux_forcing_ind
    type(marbl_forcing_fields_type), allocatable,        intent(out)   :: surface_flux_forcings(:)
    type(marbl_interior_tendency_forcing_indexing_type), intent(out)   :: interior_tendency_forcing_ind
    type(marbl_forcing_fields_type), allocatable,        intent(out)   :: interior_tendency_forcings(:)
    type(marbl_log_type),                                intent(inout) :: marbl_status_log

    ! Local variables
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_forcing_fields'
    character(len=char_len) :: log_message
    integer :: num_surface_flux_forcing_fields
    integer :: num_interior_tendency_forcing_fields
    integer :: i

    associate(&
         num_elements_surface_flux  => domain%num_elements_surface_flux, &
         num_PAR_subcols       => domain%num_PAR_subcols,                &
         num_levels            => domain%km                              &
         )

      ! Construct indices for surface and interior forcing
      call surface_flux_forcing_ind%construct(ciso_on,                        &
                                              lflux_gas_o2,                   &
                                              lflux_gas_co2,                  &
                                              ladjust_bury_coeff,             &
                                              num_surface_flux_forcing_fields)
      call interior_tendency_forcing_ind%construct(tracer_metadata%short_name,           &
                                                   tracer_restore_vars,                  &
                                                   domain%num_PAR_subcols,               &
                                                   num_interior_tendency_forcing_fields, &
                                                   marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace("interior_tendency_forcing_ind%construct", subname)
        return
      end if

      ! Initialize surface forcing fields
      allocate(surface_flux_forcings(num_surface_flux_forcing_fields))
      call marbl_init_surface_flux_forcing_fields(                   &
           num_elements                 = num_elements_surface_flux, &
           surface_flux_forcing_indices = surface_flux_forcing_ind,  &
           surface_flux_forcings        = surface_flux_forcings,     &
           marbl_status_log             = marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace("marbl_init_surface_flux_forcing_fields()", subname)
        return
      end if

      ! Initialize interior forcing fields
      allocate(interior_tendency_forcings(num_interior_tendency_forcing_fields))
      call marbl_init_interior_tendency_forcing_fields(                               &
           num_elements                      = domain%num_elements_interior_tendency, &
           interior_tendency_forcing_indices = interior_tendency_forcing_ind,         &
           tracer_metadata                   = tracer_metadata,                       &
           num_PAR_subcols                   = num_PAR_subcols,                       &
           num_levels                        = num_levels,                            &
           interior_tendency_forcings        = interior_tendency_forcings,            &
           marbl_status_log                  = marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace("marbl_init_interior_tendency_forcing_fields()", subname)
        return
      end if

      !--------------------------------------------------------------------
      ! Report what forcings are required from the driver
      !--------------------------------------------------------------------

      call marbl_status_log%log_header('MARBL-Required Forcing Fields', subname)
      call marbl_status_log%log_noerror('Surface:', subname)
      do i=1,size(surface_flux_forcings)
        write(log_message, "(2A)") '* ', trim(surface_flux_forcings(i)%metadata%varname)
        call marbl_status_log%log_noerror(log_message, subname)
      end do

      call marbl_status_log%log_noerror('', subname)
      call marbl_status_log%log_noerror('Interior:', subname)
      do i=1,size(interior_tendency_forcings)
        write(log_message, "(2A)") '* ', trim(interior_tendency_forcings(i)%metadata%varname)
        call marbl_status_log%log_noerror(log_message, subname)
      end do

    end associate

  end subroutine marbl_init_forcing_fields

  !***********************************************************************

  subroutine marbl_init_non_autotroph_tracer_metadata(short_name, long_name, &
                                                      marbl_tracer_metadata)

    !-----------------------------------------------------------------------
    !  initialize non-autotroph tracer_d values and accumulate
    !  non_living_biomass_ecosys_tracer_cnt
    !-----------------------------------------------------------------------

    character(len=*),                 intent(in)    :: short_name
    character(len=*),                 intent(in)    :: long_name
    type(marbl_tracer_metadata_type), intent(inout) :: marbl_tracer_metadata

    marbl_tracer_metadata%short_name = short_name
    marbl_tracer_metadata%long_name  = long_name
    if ((trim(short_name) == "ALK") .or. &
        (trim(short_name) == "ALK_ALT_CO2")) then
       marbl_tracer_metadata%units      = 'meq/m^3'
       marbl_tracer_metadata%tend_units = 'meq/m^3/s'
       marbl_tracer_metadata%flux_units = 'meq/m^3 cm/s'
    else
       marbl_tracer_metadata%units      = 'mmol/m^3'
       marbl_tracer_metadata%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata%flux_units = 'mmol/m^3 cm/s'
    endif

  end subroutine marbl_init_non_autotroph_tracer_metadata

  !***********************************************************************

  subroutine marbl_init_non_autotroph_tracers_metadata(marbl_tracer_metadata, &
             marbl_tracer_indices)

    !-----------------------------------------------------------------------
    !  initialize non-autotroph tracer_d values and accumulate
    !  non_living_biomass_ecosys_tracer_cnt
    !-----------------------------------------------------------------------

    type(marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)             ! descriptors for each tracer
    type(marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices

    call marbl_init_non_autotroph_tracer_metadata('PO4', 'Dissolved Inorganic Phosphate', &
               marbl_tracer_metadata(marbl_tracer_indices%po4_ind))
    call marbl_init_non_autotroph_tracer_metadata('NO3', 'Dissolved Inorganic Nitrate',   &
               marbl_tracer_metadata(marbl_tracer_indices%no3_ind))
    call marbl_init_non_autotroph_tracer_metadata('SiO3', 'Dissolved Inorganic Silicate', &
               marbl_tracer_metadata(marbl_tracer_indices%sio3_ind))
    call marbl_init_non_autotroph_tracer_metadata('NH4', 'Dissolved Ammonia',             &
               marbl_tracer_metadata(marbl_tracer_indices%nh4_ind))
    call marbl_init_non_autotroph_tracer_metadata('Fe', 'Dissolved Inorganic Iron',       &
               marbl_tracer_metadata(marbl_tracer_indices%fe_ind))
    call marbl_init_non_autotroph_tracer_metadata('Lig', 'Iron Binding Ligand',           &
               marbl_tracer_metadata(marbl_tracer_indices%lig_ind))
    call marbl_init_non_autotroph_tracer_metadata('O2', 'Dissolved Oxygen',               &
               marbl_tracer_metadata(marbl_tracer_indices%o2_ind))
    call marbl_init_non_autotroph_tracer_metadata('DIC', 'Dissolved Inorganic Carbon',    &
               marbl_tracer_metadata(marbl_tracer_indices%dic_ind))
    call marbl_init_non_autotroph_tracer_metadata('ALK', 'Alkalinity',                    &
               marbl_tracer_metadata(marbl_tracer_indices%alk_ind))
    call marbl_init_non_autotroph_tracer_metadata('DOC', 'Dissolved Organic Carbon',      &
               marbl_tracer_metadata(marbl_tracer_indices%doc_ind))
    call marbl_init_non_autotroph_tracer_metadata('DON', 'Dissolved Organic Nitrogen',    &
               marbl_tracer_metadata(marbl_tracer_indices%don_ind))
    call marbl_init_non_autotroph_tracer_metadata('DOP', 'Dissolved Organic Phosphorus',  &
               marbl_tracer_metadata(marbl_tracer_indices%dop_ind))
    call marbl_init_non_autotroph_tracer_metadata('DOPr', 'Refractory DOP',               &
               marbl_tracer_metadata(marbl_tracer_indices%dopr_ind))
    call marbl_init_non_autotroph_tracer_metadata('DONr', 'Refractory DON',               &
               marbl_tracer_metadata(marbl_tracer_indices%donr_ind))
    call marbl_init_non_autotroph_tracer_metadata('DOCr', 'Refractory DOC',               &
               marbl_tracer_metadata(marbl_tracer_indices%docr_ind))

    call marbl_init_non_autotroph_tracer_metadata('DIC_ALT_CO2', 'Dissolved Inorganic Carbon, Alternative CO2', &
               marbl_tracer_metadata(marbl_tracer_indices%dic_alt_co2_ind))
    call marbl_init_non_autotroph_tracer_metadata('ALK_ALT_CO2', 'Alkalinity, Alternative CO2', &
               marbl_tracer_metadata(marbl_tracer_indices%alk_alt_co2_ind))

  end subroutine marbl_init_non_autotroph_tracers_metadata

  !***********************************************************************

  subroutine marbl_init_zooplankton_tracer_metadata(marbl_tracer_metadata, &
             marbl_tracer_indices)

    !-----------------------------------------------------------------------
    !  initialize zooplankton tracer_d values and tracer indices
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : zooplankton_settings

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)             ! descriptors for each tracer
    type (marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n, zoo_ind            ! zooplankton functional group index
    !-----------------------------------------------------------------------

    do zoo_ind = 1, zooplankton_cnt
       n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
       marbl_tracer_metadata(n)%short_name = trim(zooplankton_settings(zoo_ind)%sname) // 'C'
       marbl_tracer_metadata(n)%long_name  = trim(zooplankton_settings(zoo_ind)%lname) // ' Carbon'
       marbl_tracer_metadata(n)%units      = 'mmol/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
    end do

  end subroutine marbl_init_zooplankton_tracer_metadata

  !***********************************************************************

  subroutine marbl_init_autotroph_tracer_metadata(marbl_tracer_metadata,      &
             marbl_tracer_indices)

    !-----------------------------------------------------------------------
    !  initialize autotroph tracer_d values and tracer indices
    !-----------------------------------------------------------------------

    use marbl_settings_mod, only : autotroph_settings

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type    (marbl_tracer_index_type) , intent(in)    :: marbl_tracer_indices

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
       marbl_tracer_metadata(n)%flux_units = 'mg/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
       marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'C'
       marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Carbon'
       marbl_tracer_metadata(n)%units      = 'mmol/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%P_ind
       if (n.gt.0) then
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'P'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Phosphorus'
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
       marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Fe'
       marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Iron'
       marbl_tracer_metadata(n)%units      = 'mmol/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
       if (n .gt. 0) then
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Si'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Silicon'
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n .gt. 0) then
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'CaCO3'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' CaCO3'
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif
    end do

  end subroutine marbl_init_autotroph_tracer_metadata

  !***********************************************************************

  subroutine marbl_init_surface_flux_forcing_fields(num_elements, surface_flux_forcing_indices, &
                                        surface_flux_forcings, marbl_status_log)

    !  Initialize the surface forcing_fields datatype with information from the
    !  namelist read
    !

    use marbl_interface_private_types, only : marbl_surface_flux_forcing_indexing_type

    integer,                                        intent(in)    :: num_elements
    type(marbl_surface_flux_forcing_indexing_type), intent(in)    :: surface_flux_forcing_indices
    type(marbl_forcing_fields_type),                intent(out)   :: surface_flux_forcings(:)
    type(marbl_log_type),                           intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_surface_flux_forcing_fields'
    character(len=char_len)     :: log_message

    integer :: id
    logical :: found
    !-----------------------------------------------------------------------

    associate(ind => surface_flux_forcing_indices)

      surface_flux_forcings(:)%metadata%varname = ''
      do id=1,size(surface_flux_forcings)
        found = .false.

        ! Square of 10m wind
        if (id .eq. ind%u10_sqr_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'u10_sqr'
          surface_flux_forcings(id)%metadata%field_units   = 'cm^2/s^2'
        end if

        ! Sea-surface salinity
        if (id .eq. ind%sss_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'sss'
          surface_flux_forcings(id)%metadata%field_units   = 'psu'
        end if

        ! Sea-surface temperature
        if (id .eq. ind%sst_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'sst'
          surface_flux_forcings(id)%metadata%field_units   = 'degC'
        end if

        ! Ice Fraction
        if (id .eq. ind%ifrac_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'Ice Fraction'
          surface_flux_forcings(id)%metadata%field_units   = 'unitless'
        end if

        ! Dust Flux
        if (id .eq. ind%dust_flux_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'Dust Flux'
          surface_flux_forcings(id)%metadata%field_units   = 'g/cm^2/s'
        end if

        ! Iron Flux
        if (id .eq. ind%iron_flux_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'Iron Flux'
          surface_flux_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! NOx Flux
        if (id .eq. ind%nox_flux_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'NOx Flux'
          surface_flux_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! NHy Flux
        if (id .eq. ind%nhy_flux_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'NHy Flux'
          surface_flux_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! external C Flux
        if (id .eq. ind%ext_C_flux_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'external C Flux'
          surface_flux_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! external P Flux
        if (id .eq. ind%ext_P_flux_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'external P Flux'
          surface_flux_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! external Si Flux
        if (id .eq. ind%ext_Si_flux_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'external Si Flux'
          surface_flux_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! atm pressure
        if (id .eq. ind%atm_pressure_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'Atmospheric Pressure'
          surface_flux_forcings(id)%metadata%field_units   = 'atmospheres'
        end if

        ! xco2
        if (id .eq. ind%xco2_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'xco2'
          surface_flux_forcings(id)%metadata%field_units   = 'ppmv'
        end if

        ! xco2_alt_co2
        if (id .eq. ind%xco2_alt_co2_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'xco2_alt_co2'
          surface_flux_forcings(id)%metadata%field_units   = 'ppmv'
        end if

        ! d13c
        if (id .eq. ind%d13c_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'd13c'
          surface_flux_forcings(id)%metadata%field_units   = 'permil'
        end if

        ! d14c
        if (id .eq. ind%d14c_id) then
          found = .true.
          surface_flux_forcings(id)%metadata%varname       = 'd14c'
          surface_flux_forcings(id)%metadata%field_units   = 'permil'
        end if

        if (.not.found) then
          write(log_message, "(A,I0,A)") "Index number ", id, &
               " is not associated with a forcing field!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if

        ! All surface forcing fields are rank 0; if that changes, make this
        ! call from inside each "if (id .eq. *)" block
        call surface_flux_forcings(id)%set_rank(num_elements, 0, marbl_status_log)

      end do

    end associate

    ! FIXME #26: do we have any forcing fields that are required to be set?
    !            If so, check to make sure those indices are not zero here.

  end subroutine marbl_init_surface_flux_forcing_fields

  !*****************************************************************************

  subroutine marbl_init_interior_tendency_forcing_fields(&
       num_elements, &
       interior_tendency_forcing_indices, &
       tracer_metadata, &
       num_PAR_subcols, &
       num_levels, &
       interior_tendency_forcings, &
       marbl_status_log)

    !  Initialize the interior forcing_fields datatype with information from the
    !  namelist read
    !
    use marbl_interface_private_types, only : marbl_interior_tendency_forcing_indexing_type

    integer,                                    intent(in)    :: num_elements
    type(marbl_interior_tendency_forcing_indexing_type), intent(in)    :: interior_tendency_forcing_indices
    type(marbl_tracer_metadata_type),           intent(in)    :: tracer_metadata(:)
    integer,                                    intent(in)    :: num_PAR_subcols
    integer,                                    intent(in)    :: num_levels
    type(marbl_forcing_fields_type),            intent(out)   :: interior_tendency_forcings(:)
    type(marbl_log_type),                       intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_interior_tendency_forcing_fields'
    character(len=char_len)     :: log_message

    ! NAG didn't like associating to tracer_metadata(:)%*
    character(len=char_len) :: tracer_name
    character(len=char_len) :: tracer_units
    integer                 :: id, n
    logical                 :: found
    !-----------------------------------------------------------------------

    associate(ind => interior_tendency_forcing_indices)

      interior_tendency_forcings(:)%metadata%varname = ''

      ! Surface fluxes that influence interior forcing
      do id=1,size(interior_tendency_forcings)
        found = .false.
        ! Dust Flux
        if (id .eq. ind%dustflux_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'Dust Flux'
          interior_tendency_forcings(id)%metadata%field_units = 'g/cm^2/s'
          call interior_tendency_forcings(id)%set_rank(num_elements, 0, marbl_status_log)
        end if

        ! PAR Column Fraction and Shortwave Radiation
        if (id .eq. ind%PAR_col_frac_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'PAR Column Fraction'
          interior_tendency_forcings(id)%metadata%field_units = 'unitless'
          call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, dim1 = num_PAR_subcols)
        end if

        if (id .eq. ind%surf_shortwave_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'Surface Shortwave'
          interior_tendency_forcings(id)%metadata%field_units = 'W/m^2'
          call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, dim1 = num_PAR_subcols)
        end if


        ! Temperature
        if (id .eq. ind%potemp_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'Potential Temperature'
          interior_tendency_forcings(id)%metadata%field_units = 'degC'
          call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, dim1 = num_levels)
        end if

        ! Salinity
        if (id .eq. ind%salinity_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'Salinity'
          interior_tendency_forcings(id)%metadata%field_units = 'psu'
          call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, dim1 = num_levels)
        end if

        ! Pressure
        if (id .eq. ind%pressure_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'Pressure'
          interior_tendency_forcings(id)%metadata%field_units = 'bars'
          call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, dim1 = num_levels)
        end if

        ! Iron Sediment Flux
        if (id .eq. ind%fesedflux_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'Iron Sediment Flux'
          interior_tendency_forcings(id)%metadata%field_units = 'nmol/cm^2/s'
          call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, dim1 = num_levels)
        end if

        ! O2 Consumption Scale Factor
        if (id .eq. ind%o2_consumption_scalef_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'O2 Consumption Scale Factor'
          interior_tendency_forcings(id)%metadata%field_units = '1'
          call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, dim1 = num_levels)
        end if

        ! Particulate Remin Scale Factor
        if (id .eq. ind%p_remin_scalef_id) then
          found = .true.
          interior_tendency_forcings(id)%metadata%varname     = 'Particulate Remin Scale Factor'
          interior_tendency_forcings(id)%metadata%field_units = '1'
          call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, dim1 = num_levels)
        end if

        ! Interior Tracer Restoring
        do n=1,size(ind%tracer_restore_id)
          if (id .eq. ind%tracer_restore_id(n)) then
            tracer_name = tracer_metadata(n)%short_name
            tracer_units = tracer_metadata(n)%units
            found = .true.
            write(interior_tendency_forcings(id)%metadata%varname,"(A,1X,A)")            &
                  trim(tracer_name), 'Restoring Field'
            interior_tendency_forcings(id)%metadata%field_units = tracer_units
            call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                                dim1 = num_levels)
          end if
          if (id .eq. ind%inv_tau_id(n)) then
            found = .true.
            write(interior_tendency_forcings(id)%metadata%varname,"(A,1X,A)")            &
                  trim(tracer_name), 'Restoring Inverse Timescale'
            interior_tendency_forcings(id)%metadata%field_units = '1/s'
            call interior_tendency_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                                dim1 = num_levels)
          end if
        end do

        ! Check to see if %set_rank() returned an error
        if (marbl_status_log%labort_marbl) then
          write(log_message, "(2A)") trim(interior_tendency_forcings(id)%metadata%varname), &
                                     ' set_rank()'
          call marbl_status_log%log_error_trace(log_message, subname)
          return
        end if

        ! Abort if there was no match between id and the restoring indices
        if (.not.found) then
          write(log_message, "(A,I0,A)") "Index number ", id, &
                                         " is not associated with a forcing field!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if

      end do

    end associate

    ! FIXME #26: do we have any forcing fields that are required to be set?
    !            If so, check to make sure those indices are not zero here.

  end subroutine marbl_init_interior_tendency_forcing_fields

  !*****************************************************************************

end module marbl_init_mod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
