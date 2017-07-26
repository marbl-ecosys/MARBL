module marbl_init_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : char_len

  use marbl_interface_types, only : marbl_tracer_metadata_type
  use marbl_interface_types, only : marbl_forcing_fields_type

  use marbl_internal_types, only : marbl_tracer_index_type

  use marbl_logging, only : marbl_log_type

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt
  use marbl_sizes, only : grazer_prey_cnt

  implicit none
  private

  public :: marbl_init_log_and_timers
  public :: marbl_init_config_vars1
  public :: marbl_init_tracer_metadata
  public :: marbl_init_bury_coeff
  public :: marbl_init_forcing_fields

  private :: marbl_init_non_autotroph_tracer_metadata
  private :: marbl_init_non_autotroph_tracers_metadata
  private :: marbl_init_zooplankton_tracer_metadata
  private :: marbl_init_autotroph_tracer_metadata
  private :: marbl_init_surface_forcing_fields
  private :: marbl_init_interior_forcing_fields

contains

  !***********************************************************************

  subroutine marbl_init_log_and_timers(marbl_timers, timer_ids, marbl_status_log)

    use marbl_internal_types  , only : marbl_internal_timers_type
    use marbl_internal_types  , only : marbl_timer_indexing_type

    type(marbl_internal_timers_type), intent(inout) :: marbl_timers
    type(marbl_timer_indexing_type),  intent(inout) :: timer_ids
    type(marbl_log_type),             intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_log_and_timers'

    ! Construct status log
    call marbl_status_log%construct()
    call marbl_status_log%log_noerror('', subname)

    ! Set up timers
    call marbl_timers%setup(timer_ids, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("setup_timers()", subname)
      return
    end if

  end subroutine marbl_init_log_and_timers

  !***********************************************************************

  subroutine marbl_init_config_vars1(marbl_configuration, marbl_status_log, gcm_nl_buffer)

    use marbl_config_mod, only : marbl_config_and_parms_type
    use marbl_config_mod, only : marbl_config_set_defaults
    use marbl_config_mod, only : marbl_config_read_namelist
    use marbl_config_mod, only : marbl_define_config_vars

    type(marbl_config_and_parms_type), intent(inout) :: marbl_configuration
    type(marbl_log_type),              intent(inout) :: marbl_status_log
    character(len=*), optional,        intent(in)    :: gcm_nl_buffer(:)

    ! local variables
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_config_vars1'
    character(len=char_len) :: log_message

    !---------------------------------------------------------------------------
    ! set default values for configuration
    !---------------------------------------------------------------------------

    call marbl_config_set_defaults()

    !---------------------------------------------------------------------------
    ! read configuration from namelist (if present)
    !---------------------------------------------------------------------------

    if (present(gcm_nl_buffer)) then
      call marbl_config_read_namelist(gcm_nl_buffer, marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace('marbl_config_read_namelist', subname)
        return
      end if
    else
      write(log_message, "(2A)") '** No namelists were provided to config, ', &
           'use put() and get() to change configuration variables'
      call marbl_status_log%log_noerror(log_message, subname)
    end if

    !---------------------------------------------------------------------------
    ! construct configuration_type
    !---------------------------------------------------------------------------

    call marbl_define_config_vars(marbl_configuration, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("marbl_define_config_vars()", subname)
      return
    end if

  end subroutine marbl_init_config_vars1

  !***********************************************************************

  subroutine marbl_init_tracer_metadata(marbl_tracer_metadata,                &
             marbl_tracer_indices, marbl_status_log)

    !  Set tracer and forcing metadata

    use marbl_config_mod, only : lecovars_full_depth_tavg

    implicit none

    type (marbl_tracer_metadata_type), intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type(marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices
    type(marbl_log_type)             , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_tracer_metadata'

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

  subroutine marbl_init_bury_coeff(marbl_particulate_share, marbl_status_log)

    use marbl_logging, only : marbl_log_type
    use marbl_config_mod, only : init_bury_coeff_opt
    use marbl_config_mod, only : ladjust_bury_coeff
    use marbl_parms, only : parm_init_POC_bury_coeff
    use marbl_parms, only : parm_init_POP_bury_coeff
    use marbl_parms, only : parm_init_bSi_bury_coeff
    use marbl_internal_types, only : marbl_particulate_share_type

    type(marbl_particulate_share_type), intent(inout) :: marbl_particulate_share
    type(marbl_log_type)              , intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_bury_coeff'

    !---------------------------------------------------------------------------

    ! if ladjust_bury_coeff is true, then bury coefficients are set at runtime
    ! so they do not need to be initialized here

    if (.not. ladjust_bury_coeff) then
       if (init_bury_coeff_opt == 'nml') then
          marbl_particulate_share%POC_bury_coeff = parm_init_POC_bury_coeff
          marbl_particulate_share%POP_bury_coeff = parm_init_POP_bury_coeff
          marbl_particulate_share%bSi_bury_coeff = parm_init_bSi_bury_coeff
       else
          call marbl_status_log%log_error("ladjust_bury_coeff=.false., init_bury_coeff_opt='restfile' not implemented", subname)
          return
       end if
    end if

  end subroutine marbl_init_bury_coeff

  !***********************************************************************

  subroutine marbl_init_forcing_fields(domain, &
                                       tracer_metadata, &
                                       surface_forcing_ind, &
                                       surface_forcing_share, &
                                       surface_forcing_internal, &
                                       surface_input_forcings, &
                                       interior_forcing_ind, &
                                       interior_input_forcings, &
                                       marbl_status_log)

    use marbl_interface_types, only : marbl_domain_type
    use marbl_internal_types, only : marbl_surface_forcing_indexing_type
    use marbl_internal_types  , only : marbl_surface_forcing_share_type
    use marbl_internal_types  , only : marbl_surface_forcing_internal_type
    use marbl_internal_types, only : marbl_interior_forcing_indexing_type
    use marbl_config_mod, only : ciso_on
    use marbl_config_mod, only : lflux_gas_o2
    use marbl_config_mod, only : lflux_gas_co2
    use marbl_config_mod, only : ladjust_bury_coeff
    use marbl_parms, only : tracer_restore_vars

    type(marbl_domain_type),                      intent(in)    :: domain
    type(marbl_tracer_metadata_type),             intent(in)    :: tracer_metadata(:)
    type(marbl_surface_forcing_indexing_type),    intent(inout) :: surface_forcing_ind
    type(marbl_surface_forcing_share_type),       intent(inout) :: surface_forcing_share
    type(marbl_surface_forcing_internal_type),    intent(inout) :: surface_forcing_internal
    type(marbl_forcing_fields_type), allocatable, intent(inout) :: surface_input_forcings(:)
    type(marbl_interior_forcing_indexing_type),   intent(inout) :: interior_forcing_ind
    type(marbl_forcing_fields_type), allocatable, intent(inout) :: interior_input_forcings(:)
    type(marbl_log_type),                         intent(inout) :: marbl_status_log

    ! Local variables
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_forcing_fields'
    integer :: num_surface_forcing_fields
    integer :: num_interior_forcing_fields

    associate(&
         num_surface_elements  => domain%num_elements_surface_forcing,   &
         num_interior_elements => domain%num_elements_interior_forcing,  &
         num_PAR_subcols       => domain%num_PAR_subcols,                &
         num_levels            => domain%km                              &
         )

      ! Construct indices for surface and interior forcing
      call surface_forcing_ind%construct(ciso_on,                             &
                                         lflux_gas_o2,                        &
                                         lflux_gas_co2,                       &
                                         ladjust_bury_coeff,                  &
                                         num_surface_forcing_fields)
      call interior_forcing_ind%construct(tracer_metadata%short_name,         &
                                          tracer_restore_vars,                &
                                          num_interior_forcing_fields,        &
                                          marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace("interior_forcing_ind%construct", subname)
        return
      end if

      ! Construct share / internal types for surface forcing
      call surface_forcing_share%construct(num_surface_elements)
      call surface_forcing_internal%construct(num_surface_elements)

      ! Initialize surface forcing fields
      allocate(surface_input_forcings(num_surface_forcing_fields))
      call marbl_init_surface_forcing_fields(                                &
           num_elements            = num_surface_elements,                   &
           surface_forcing_indices = surface_forcing_ind,                    &
           surface_forcings        = surface_input_forcings,                 &
           marbl_status_log        = marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace("marbl_init_surface_forcing_fields()", subname)
        return
      end if

      ! Initialize interior forcing fields
      allocate(interior_input_forcings(num_interior_forcing_fields))
      call marbl_init_interior_forcing_fields(                                &
           num_elements             = num_interior_elements,                  &
           interior_forcing_indices = interior_forcing_ind,                   &
           tracer_metadata          = tracer_metadata,                        &
           num_PAR_subcols          = num_PAR_subcols,                        &
           num_levels               = num_levels,                             &
           interior_forcings        = interior_input_forcings,                &
           marbl_status_log         = marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace("marbl_init_interior_forcing_fields()", subname)
        return
      end if

    end associate

  end subroutine marbl_init_forcing_fields

  !***********************************************************************

  subroutine marbl_init_non_autotroph_tracer_metadata(short_name, long_name, &
                                                      marbl_tracer_metadata)

    !-----------------------------------------------------------------------
    !  initialize non-autotroph tracer_d values and accumulate
    !  non_living_biomass_ecosys_tracer_cnt
    !-----------------------------------------------------------------------

    implicit none

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

    implicit none

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

    use marbl_config_mod, only : zooplankton_config

    implicit none

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)             ! descriptors for each tracer
    type (marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n, zoo_ind            ! zooplankton functional group index
    !-----------------------------------------------------------------------

    do zoo_ind = 1, zooplankton_cnt
       n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
       marbl_tracer_metadata(n)%short_name = trim(zooplankton_config(zoo_ind)%sname) // 'C'
       marbl_tracer_metadata(n)%long_name  = trim(zooplankton_config(zoo_ind)%lname) // ' Carbon'
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

    use marbl_config_mod, only : autotrophs_config

    implicit none

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type    (marbl_tracer_index_type) , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n, auto_ind
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
       marbl_tracer_metadata(n)%short_name = trim(autotrophs_config(auto_ind)%sname) // 'Chl'
       marbl_tracer_metadata(n)%long_name  = trim(autotrophs_config(auto_ind)%lname) // ' Chlorophyll'
       marbl_tracer_metadata(n)%units      = 'mg/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mg/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mg/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
       marbl_tracer_metadata(n)%short_name = trim(autotrophs_config(auto_ind)%sname) // 'C'
       marbl_tracer_metadata(n)%long_name  = trim(autotrophs_config(auto_ind)%lname) // ' Carbon'
       marbl_tracer_metadata(n)%units      = 'mmol/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%P_ind
       if (n.gt.0) then
          marbl_tracer_metadata(n)%short_name = trim(autotrophs_config(auto_ind)%sname) // 'P'
          marbl_tracer_metadata(n)%long_name  = trim(autotrophs_config(auto_ind)%lname) // ' Phosphorus'
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
       marbl_tracer_metadata(n)%short_name = trim(autotrophs_config(auto_ind)%sname) // 'Fe'
       marbl_tracer_metadata(n)%long_name  = trim(autotrophs_config(auto_ind)%lname) // ' Iron'
       marbl_tracer_metadata(n)%units      = 'mmol/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
       if (n .gt. 0) then
          marbl_tracer_metadata(n)%short_name = trim(autotrophs_config(auto_ind)%sname) // 'Si'
          marbl_tracer_metadata(n)%long_name  = trim(autotrophs_config(auto_ind)%lname) // ' Silicon'
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n .gt. 0) then
          marbl_tracer_metadata(n)%short_name = trim(autotrophs_config(auto_ind)%sname) // 'CaCO3'
          marbl_tracer_metadata(n)%long_name  = trim(autotrophs_config(auto_ind)%lname) // ' CaCO3'
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif
    end do

  end subroutine marbl_init_autotroph_tracer_metadata

  !***********************************************************************

  subroutine marbl_init_surface_forcing_fields(num_elements, surface_forcing_indices, &
                                        surface_forcings, marbl_status_log)

    !  Initialize the surface forcing_fields datatype with information from the
    !  namelist read
    !

    use marbl_internal_types, only : marbl_surface_forcing_indexing_type

    implicit none

    integer,                                   intent(in)    :: num_elements
    type(marbl_surface_forcing_indexing_type), intent(in)    :: surface_forcing_indices
    type(marbl_forcing_fields_type),           intent(inout) :: surface_forcings(:)
    type(marbl_log_type),                      intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_surface_forcing_fields'
    character(len=char_len)     :: log_message

    integer :: id
    logical :: found
    !-----------------------------------------------------------------------

    associate(ind => surface_forcing_indices)

      surface_forcings(:)%metadata%varname = ''
      do id=1,size(surface_forcings)
        found = .false.

        ! Square of 10m wind
        if (id .eq. ind%u10_sqr_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'u10_sqr'
          surface_forcings(id)%metadata%field_units   = 'cm^2/s^2'
        end if

        ! Sea-surface salinity
        if (id .eq. ind%sss_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'sss'
          surface_forcings(id)%metadata%field_units   = 'unknown units'
        end if

        ! Sea-surface temperature
        if (id .eq. ind%sst_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'sst'
          surface_forcings(id)%metadata%field_units   = 'degrees C'
        end if

        ! Ice Fraction
        if (id .eq. ind%ifrac_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'Ice Fraction'
          surface_forcings(id)%metadata%field_units   = 'unitless'
        end if

        ! Dust Flux
        if (id .eq. ind%dust_flux_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'Dust Flux'
          surface_forcings(id)%metadata%field_units   = 'g/cm^2/s'
        end if

        ! Iron Flux
        if (id .eq. ind%iron_flux_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'Iron Flux'
          surface_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! NOx Flux
        if (id .eq. ind%nox_flux_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'NOx Flux'
          surface_forcings(id)%metadata%field_units   = 'unknown units'
        end if

        ! NHy Flux
        if (id .eq. ind%nhy_flux_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'NHy Flux'
          surface_forcings(id)%metadata%field_units   = 'unknown units'
        end if

        ! external C Flux
        if (id .eq. ind%ext_C_flux_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'external C Flux'
          surface_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! external P Flux
        if (id .eq. ind%ext_P_flux_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'external P Flux'
          surface_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! external Si Flux
        if (id .eq. ind%ext_Si_flux_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'external Si Flux'
          surface_forcings(id)%metadata%field_units   = 'nmol/cm^2/s'
        end if

        ! atm pressure
        if (id .eq. ind%atm_pressure_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'Atmospheric Pressure'
          surface_forcings(id)%metadata%field_units   = 'unknown units'
        end if

        ! xco2
        if (id .eq. ind%xco2_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'xco2'
          surface_forcings(id)%metadata%field_units   = 'unknown units'
        end if

        ! xco2_alt_co2
        if (id .eq. ind%xco2_alt_co2_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'xco2_alt_co2'
          surface_forcings(id)%metadata%field_units   = 'unknown units'
        end if

        ! d13c
        if (id .eq. ind%d13c_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'd13c'
          surface_forcings(id)%metadata%field_units   = 'unknown units'
        end if

        ! d14c
        if (id .eq. ind%d14c_id) then
          found = .true.
          surface_forcings(id)%metadata%varname       = 'd14c'
          surface_forcings(id)%metadata%field_units   = 'unknown units'
        end if

        if (.not.found) then
          write(log_message, "(A,I0,A)") "Index number ", id, &
               " is not associated with a forcing field!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if

        ! All surface forcing fields are rank 0; if that changes, make this
        ! call from inside each "if (id .eq. *)" block
        call surface_forcings(id)%set_rank(num_elements, 0, marbl_status_log)

      end do

    end associate

    ! FIXME #26: do we have any forcing fields that are required to be set?
    !            If so, check to make sure those indices are not zero here.

  end subroutine marbl_init_surface_forcing_fields

  !*****************************************************************************

  subroutine marbl_init_interior_forcing_fields(&
       num_elements, &
       interior_forcing_indices, &
       tracer_metadata, &
       num_PAR_subcols, &
       num_levels, &
       interior_forcings, &
       marbl_status_log)

    !  Initialize the interior forcing_fields datatype with information from the
    !  namelist read
    !
    use marbl_internal_types, only : marbl_interior_forcing_indexing_type

    implicit none

    integer,                                    intent(in)    :: num_elements
    type(marbl_interior_forcing_indexing_type), intent(in)    :: interior_forcing_indices
    type(marbl_tracer_metadata_type),           intent(in)    :: tracer_metadata(:)
    integer,                                    intent(in)    :: num_PAR_subcols
    integer,                                    intent(in)    :: num_levels
    type(marbl_forcing_fields_type),            intent(inout) :: interior_forcings(:)
    type(marbl_log_type),                       intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_init_mod:marbl_init_interior_forcing_fields'
    character(len=char_len)     :: log_message

    ! NAG didn't like associating to tracer_metadata(:)%*
    character(len=char_len) :: tracer_name
    character(len=char_len) :: tracer_units
    integer                 :: id, n
    logical                 :: found
    !-----------------------------------------------------------------------

    associate(ind => interior_forcing_indices)

      interior_forcings(:)%metadata%varname = ''

      ! Surface fluxes that influence interior forcing
      do id=1,size(interior_forcings)
        found = .false.
        ! Dust Flux
        if (id .eq. ind%dustflux_id) then
          found = .true.
          interior_forcings(id)%metadata%varname     = 'Dust Flux'
          interior_forcings(id)%metadata%field_units = 'need_units'
          call interior_forcings(id)%set_rank(num_elements, 0, marbl_status_log)
        end if

        ! PAR Column Fraction and Shortwave Radiation
        if (id .eq. ind%PAR_col_frac_id) then
          found = .true.
          interior_forcings(id)%metadata%varname     = 'PAR Column Fraction'
          interior_forcings(id)%metadata%field_units = 'unitless'
          call interior_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                              dim1 = num_PAR_subcols)
        end if

        if (id .eq. ind%surf_shortwave_id) then
          found = .true.
          interior_forcings(id)%metadata%varname     = 'Surface Shortwave'
          interior_forcings(id)%metadata%field_units = 'need_units' ! W/m^2?
          call interior_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                              dim1 = num_PAR_subcols)
        end if


        ! Temperature
        if (id .eq. ind%temperature_id) then
          found = .true.
          interior_forcings(id)%metadata%varname     = 'Temperature'
          interior_forcings(id)%metadata%field_units = 'Degrees C'
          call interior_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                              dim1 = num_levels)
        end if

        ! Salinity
        if (id .eq. ind%salinity_id) then
          found = .true.
          interior_forcings(id)%metadata%varname     = 'Salinity'
          interior_forcings(id)%metadata%field_units = 'need_units'
          call interior_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                              dim1 = num_levels)
        end if

        ! Pressure
        if (id .eq. ind%pressure_id) then
          found = .true.
          interior_forcings(id)%metadata%varname     = 'Pressure'
          interior_forcings(id)%metadata%field_units = 'need_units'
          call interior_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                              dim1 = num_levels)
        end if

        ! Iron Sediment Flux
        if (id .eq. ind%fesedflux_id) then
          found = .true.
          interior_forcings(id)%metadata%varname     = 'Iron Sediment Flux'
          interior_forcings(id)%metadata%field_units = 'need_units'
          call interior_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                              dim1 = num_levels)
        end if

        ! Interior Tracer Restoring
        do n=1,size(ind%tracer_restore_id)
          if (id .eq. ind%tracer_restore_id(n)) then
            tracer_name = tracer_metadata(n)%short_name
            tracer_units = tracer_metadata(n)%units
            found = .true.
            write(interior_forcings(id)%metadata%varname,"(A,1X,A)")            &
                  trim(tracer_name), 'Restoring Field'
            interior_forcings(id)%metadata%field_units = tracer_units
            call interior_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                                dim1 = num_levels)
          end if
          if (id .eq. ind%inv_tau_id(n)) then
            found = .true.
            write(interior_forcings(id)%metadata%varname,"(A,1X,A)")            &
                  trim(tracer_name), 'Restoring Inverse Timescale'
            interior_forcings(id)%metadata%field_units = '1/s'
            call interior_forcings(id)%set_rank(num_elements, 1, marbl_status_log, &
                                                dim1 = num_levels)
          end if
        end do

        ! Check to see if %set_rank() returned an error
        if (marbl_status_log%labort_marbl) then
          write(log_message, "(2A)") trim(interior_forcings(id)%metadata%varname), &
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

  end subroutine marbl_init_interior_forcing_fields

  !*****************************************************************************

end module marbl_init_mod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
