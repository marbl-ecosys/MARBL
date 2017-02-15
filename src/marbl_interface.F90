!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_interface

  !-----------------------------------------------------------------------
  ! marbl interface
  !
  ! This module contains the public interface to marbl. This is the
  ! only public API and the only interface guaranteed to be documented
  ! and semi-stable. All other marbl rountines are private and may
  ! change at any time.
  !
  ! The following terminology is used in this module:
  !
  ! driver - refers to the code that is calling marbl routines. This
  !          can be anything from a full GCM to a simplified single
  !          column test system
  !-----------------------------------------------------------------------

  use marbl_kinds_mod       , only : r8, log_kind, int_kind, log_kind, char_len
  use marbl_logging         , only : marbl_log_type

  use marbl_sizes           , only : marbl_total_tracer_cnt
  use marbl_sizes           , only : autotroph_cnt
  use marbl_sizes           , only : zooplankton_cnt
  use marbl_sizes           , only : num_surface_forcing_fields
  use marbl_sizes           , only : num_interior_forcing_fields

  use marbl_interface_types , only : marbl_domain_type
  use marbl_interface_types , only : marbl_tracer_metadata_type
  use marbl_interface_types , only : marbl_surface_forcing_output_type
  use marbl_interface_types , only : marbl_diagnostics_type
  use marbl_interface_types , only : marbl_forcing_fields_type
  use marbl_interface_types , only : marbl_saved_state_type
  use marbl_interface_types , only : marbl_timers_type
  use marbl_interface_types , only : marbl_running_mean_0d_type

  use marbl_internal_types  , only : marbl_surface_forcing_indexing_type
  use marbl_internal_types  , only : marbl_surface_saved_state_indexing_type
  use marbl_internal_types  , only : marbl_interior_forcing_indexing_type
  use marbl_internal_types  , only : marbl_interior_saved_state_indexing_type
  use marbl_internal_types  , only : marbl_PAR_type
  use marbl_internal_types  , only : marbl_interior_share_type
  use marbl_internal_types  , only : marbl_autotroph_share_type
  use marbl_internal_types  , only : marbl_zooplankton_share_type
  use marbl_internal_types  , only : marbl_particulate_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_internal_type
  use marbl_internal_types  , only : marbl_tracer_index_type
  use marbl_internal_types  , only : marbl_internal_timers_type
  use marbl_internal_types  , only : marbl_timer_indexing_type


  use marbl_config_mod, only : marbl_config_and_parms_type
  use marbl_config_mod, only : ciso_on
  use marbl_config_mod, only : lvariable_PtoC

  implicit none

  private

  !-----------------------------------------------------------------------------
  !
  ! The following data structures are part of the public API that the
  ! driver will read/write to interact with marbl.
  !
  !-----------------------------------------------------------------------------

  type, public :: marbl_interface_class

     ! public data - general
     type(marbl_domain_type)                   , public               :: domain
     type(marbl_tracer_metadata_type)          , public, allocatable  :: tracer_metadata(:)
     type(marbl_tracer_index_type)             , public               :: tracer_indices
     type(marbl_log_type)                      , public               :: StatusLog
     type(marbl_config_and_parms_type)         , public               :: configuration
     type(marbl_config_and_parms_type)         , public               :: parameters

     type(marbl_saved_state_type)              , public               :: surface_saved_state             ! input/output
     type(marbl_saved_state_type)              , public               :: interior_saved_state             ! input/output
     type(marbl_surface_saved_state_indexing_type), public            :: surf_state_ind
     type(marbl_interior_saved_state_indexing_type), public           :: interior_state_ind
     type(marbl_timers_type)                   , public               :: timer_summary

     ! public data - interior forcing
     real (r8)                                 , public, allocatable  :: column_tracers(:,:)     ! input  *
     real (r8)                                 , public, allocatable  :: column_dtracers(:,:)    ! output *
     type(marbl_interior_forcing_indexing_type), public               :: interior_forcing_ind         !
     type(marbl_forcing_fields_type)           , public, allocatable  :: interior_input_forcings(:)
     type(marbl_diagnostics_type)              , public               :: interior_forcing_diags  ! output

     ! public data surface forcing
     real (r8)                                 , public, allocatable  :: surface_vals(:,:)           ! input  *
     type(marbl_surface_forcing_indexing_type) , public               :: surface_forcing_ind         !
     type(marbl_forcing_fields_type)           , public, allocatable  :: surface_input_forcings(:) ! input  *
     real (r8)                                 , public, allocatable  :: surface_tracer_fluxes(:,:)  ! output *
     type(marbl_surface_forcing_output_type)   , public               :: surface_forcing_output      ! output
     type(marbl_diagnostics_type)              , public               :: surface_forcing_diags       ! output

     ! public data - global averages
     real (r8)                                 , public, allocatable  :: glo_avg_fields_interior(:)   ! output (nfields)
     real (r8)                                 , public, allocatable  :: glo_avg_averages_interior(:) ! input (nfields)
     real (r8)                                 , public, allocatable  :: glo_avg_fields_surface(:,:)  ! output (num_elements,nfields)
     real (r8)                                 , public, allocatable  :: glo_avg_averages_surface(:)  ! input (nfields)

     ! FIXME
     ! for now, running means are being computed in the driver
     ! they will eventually be moved from the interface to inside MARBL
     real (r8)                                 , public, allocatable  :: glo_scalar_interior(:)
     real (r8)                                 , public, allocatable  :: glo_scalar_surface(:)

     type(marbl_running_mean_0d_type)          , public, allocatable  :: glo_avg_rmean_interior(:)
     type(marbl_running_mean_0d_type)          , public, allocatable  :: glo_avg_rmean_surface(:)
     type(marbl_running_mean_0d_type)          , public, allocatable  :: glo_scalar_rmean_interior(:)
     type(marbl_running_mean_0d_type)          , public, allocatable  :: glo_scalar_rmean_surface(:)

     ! private data
     type(marbl_PAR_type)                      , private              :: PAR
     type(marbl_particulate_share_type)        , private              :: particulate_share
     type(marbl_interior_share_type)           , private, allocatable :: interior_share(:)
     type(marbl_zooplankton_share_type)        , private, allocatable :: zooplankton_share(:,:)
     type(marbl_autotroph_share_type)          , private, allocatable :: autotroph_share(:,:)
     type(marbl_surface_forcing_share_type)    , private              :: surface_forcing_share
     type(marbl_surface_forcing_internal_type) , private              :: surface_forcing_internal
     logical                                   , private              :: lallow_glo_ops
     type(marbl_internal_timers_type)          , private              :: timers
     type(marbl_timer_indexing_type)           , private              :: timer_ids

   contains

     procedure, public  :: config
     procedure, public  :: init
     procedure, public  :: complete_config_and_init
     procedure, public  :: reset_timers
     procedure, public  :: extract_timing
     procedure, private :: glo_vars_init
     procedure, public  :: get_tracer_index
     procedure, public  :: set_interior_forcing
     procedure, public  :: set_surface_forcing
     procedure, public  :: set_global_scalars
     procedure, public  :: shutdown

  end type marbl_interface_class

  private :: config
  private :: init
  private :: complete_config_and_init
  private :: reset_timers
  private :: extract_timing
  private :: glo_vars_init
  private :: set_interior_forcing
  private :: set_surface_forcing
  private :: shutdown

  !***********************************************************************

contains

  !***********************************************************************

  subroutine config(this,                 &
       lgcm_has_global_ops,               &
       gcm_nl_buffer)

    use marbl_namelist_mod, only : marbl_nl_cnt
    use marbl_namelist_mod, only : marbl_nl_buffer_size
    use marbl_config_mod  , only : marbl_config_set_defaults
    use marbl_config_mod  , only : marbl_config_read_namelist
    use marbl_config_mod  , only : marbl_define_config_vars

    class(marbl_interface_class)   , intent(inout)        :: this
    character(marbl_nl_buffer_size), optional, intent(in) :: gcm_nl_buffer(:)
    logical,                         optional, intent(in) :: lgcm_has_global_ops

    character(*), parameter :: subname = 'marbl_interface:config'
    character(len=char_len) :: log_message

    !--------------------------------------------------------------------
    ! initialize status log
    !--------------------------------------------------------------------

    call this%StatusLog%construct()
    call this%StatusLog%log_noerror('', subname)

    !-----------------------------------------------------------------------
    !  Set up timers
    !-----------------------------------------------------------------------

    call this%timers%setup(this%timer_ids, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("setup_timers()", subname)
      return
    end if

    ! Start initialization timer
    call this%timers%start(this%timer_ids%init_timer_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%start()", subname)
      return
    end if

    !-----------------------------------------------------------------------
    !  Check to see if global ops are allowed
    !-----------------------------------------------------------------------

    if (present(lgcm_has_global_ops)) then
      this%lallow_glo_ops = lgcm_has_global_ops
    else
      this%lallow_glo_ops = .false.
    end if

    !---------------------------------------------------------------------------
    ! set default values for configuration
    !---------------------------------------------------------------------------

    call marbl_config_set_defaults()

    !---------------------------------------------------------------------------
    ! read configuration from namelist (if present)
    !---------------------------------------------------------------------------

    if (present(gcm_nl_buffer)) then
      call marbl_config_read_namelist(gcm_nl_buffer, this%StatusLog)
      if (this%StatusLog%labort_marbl) then
        call this%StatusLog%log_error_trace('marbl_config_read_namelist', subname)
        return
      end if
    else
      write(log_message, "(2A)") '** No namelists were provided to config, ', &
           'use put() and get() to change configuration variables'
      call this%StatusLog%log_noerror(log_message, subname)
      call this%StatusLog%log_noerror('', subname)
    end if

    !---------------------------------------------------------------------------
    ! construct configuration_type
    !---------------------------------------------------------------------------

    call marbl_define_config_vars(this%configuration, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_define_config_vars()", subname)
      return
    end if

    call this%timers%stop(this%timer_ids%init_timer_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("this%timers%stop()", subname)
      return
    end if

  end subroutine config

  !***********************************************************************

  subroutine init(this,                   &
       gcm_num_levels,                    &
       gcm_num_PAR_subcols,               &
       gcm_num_elements_interior_forcing, &
       gcm_num_elements_surface_forcing,  &
       gcm_dz,                            &
       gcm_zw,                            &
       gcm_zt,                            &
       gcm_nl_buffer,                     &
       marbl_tracer_cnt)

    use marbl_namelist_mod    , only : marbl_nl_cnt
    use marbl_namelist_mod    , only : marbl_nl_buffer_size
    use marbl_ciso_mod        , only : marbl_ciso_init_tracer_metadata
    use marbl_mod             , only : marbl_init_tracer_metadata
    use marbl_mod             , only : marbl_tracer_index_consistency_check
    use marbl_diagnostics_mod , only : marbl_diagnostics_init
    use marbl_config_mod      , only : ladjust_bury_coeff
    use marbl_config_mod      , only : autotrophs_config
    use marbl_config_mod      , only : zooplankton_config
    use marbl_config_mod      , only : set_derived_config
    use marbl_parms           , only : marbl_parms_set_defaults
    use marbl_parms           , only : marbl_parms_read_namelist
    use marbl_parms           , only : marbl_define_parameters
    use marbl_saved_state_mod , only : marbl_saved_state_init

    implicit none

    class     (marbl_interface_class)      , intent(inout) :: this
    integer   (int_kind)                   , intent(in)    :: gcm_num_levels
    integer   (int_kind)                   , intent(in)    :: gcm_num_PAR_subcols
    integer   (int_kind)                   , intent(in)    :: gcm_num_elements_surface_forcing
    integer   (int_kind)                   , intent(in)    :: gcm_num_elements_interior_forcing
    real      (r8)                         , intent(in)    :: gcm_dz(gcm_num_levels) ! thickness of layer k
    real      (r8)                         , intent(in)    :: gcm_zw(gcm_num_levels) ! thickness of layer k
    real      (r8)                         , intent(in)    :: gcm_zt(gcm_num_levels) ! thickness of layer k
    character(marbl_nl_buffer_size), optional, intent(in)  :: gcm_nl_buffer(:)
    integer   (int_kind), optional         , intent(out)   :: marbl_tracer_cnt

    character(*), parameter :: subname = 'marbl_interface:init'
    character(len=char_len) :: log_message
    integer :: i
    !--------------------------------------------------------------------

    call this%timers%start(this%timer_ids%init_timer_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("this%timers%start()", subname)
      return
    end if

    associate(&
         num_levels            => gcm_num_levels,                              &
         num_PAR_subcols       => gcm_num_PAR_subcols,                         &
         num_surface_elements  => gcm_num_elements_surface_forcing,            &
         num_interior_elements => gcm_num_elements_interior_forcing            &
         )

    !-----------------------------------------------------------------------
    !  Lock and log this%configuration
    !-----------------------------------------------------------------------

    call this%configuration%finalize_vars(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('configuration%finalize_vars', subname)
      return
    end if

    call set_derived_config(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('set_derived_config', subname)
      return
    end if

    !-----------------------------------------------------------------------
    !  Abort if GCM doesn't support global ops but configuration requires them
    !-----------------------------------------------------------------------

    if (ladjust_bury_coeff .and. (.not.this%lallow_glo_ops)) then
      write(log_message,'(2A)') 'Can not run with ladjust_bury_coeff = ',     &
             '.true. unless GCM can perform global operations'
      call this%StatusLog%log_error(log_message, subname)
      return
    end if

    !-----------------------------------------------------------------------
    !  Set up tracer indices
    !-----------------------------------------------------------------------

    call this%tracer_indices%construct(ciso_on, lvariable_PtoC, autotrophs_config, &
         zooplankton_config)
    if (present(marbl_tracer_cnt)) &
      marbl_tracer_cnt = marbl_total_tracer_cnt

    !--------------------------------------------------------------------
    ! call constructors and allocate memory
    !--------------------------------------------------------------------

    call this%PAR%construct(num_levels, num_PAR_subcols)

    call this%particulate_share%construct(num_levels)

    allocate (this%interior_share(num_levels))

    allocate (this%zooplankton_share(zooplankton_cnt, num_levels))

    allocate (this%autotroph_share(autotroph_cnt, num_levels))

    call this%domain%construct(                                 &
         num_levels                    = num_levels,            &
         num_PAR_subcols               = num_PAR_subcols,       &
         num_elements_surface_forcing  = num_surface_elements,  &
         num_elements_interior_forcing = num_interior_elements, &
         dz                            = gcm_dz,                &
         zw                            = gcm_zw,                &
         zt                            = gcm_zt)

    allocate(this%surface_vals(num_surface_elements, marbl_total_tracer_cnt))

    allocate(this%surface_tracer_fluxes(num_surface_elements, marbl_total_tracer_cnt))

    allocate(this%column_tracers(marbl_total_tracer_cnt, num_levels))

    allocate(this%column_dtracers(marbl_total_tracer_cnt, num_levels))

    !--------------------------------------------------------------------
    ! set up saved state variables
    !--------------------------------------------------------------------

    call marbl_saved_state_init(this%surface_saved_state,                     &
                                this%interior_saved_state,                    &
                                this%surf_state_ind,                          &
                                this%interior_state_ind,                      &
                                num_levels,                                   &
                                num_surface_elements,                         &
                                num_interior_elements,                        &
                                this%StatusLog)

    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_saved_state_init()", subname)
      return
    end if

    !--------------------------------------------------------------------
    ! Initialize public data / general tracer metadata
    ! And then update tracer input info based on namelist
    !--------------------------------------------------------------------

    allocate(this%tracer_metadata(marbl_total_tracer_cnt))

    call marbl_init_tracer_metadata( &
         this%tracer_metadata,       &
         this%tracer_indices,        &
         this%StatusLog)

    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_tracer_metadata()", subname)
      return
    end if

    if (ciso_on) then
       call marbl_ciso_init_tracer_metadata(this%tracer_metadata,             &
            this%tracer_indices)
    end if

    !--------------------------------------------------------------------
    ! Report what tracers are being used, abort if count is not correct
    !--------------------------------------------------------------------

    call this%StatusLog%log_noerror('--------------------', subname)
    call this%StatusLog%log_noerror('MARBL Tracer indices', subname)
    call this%StatusLog%log_noerror('--------------------', subname)
    call this%StatusLog%log_noerror('', subname)
    do i=1,marbl_total_tracer_cnt
      write(log_message, "(I3,2A)") i, '. ', &
                                 trim(this%tracer_metadata(i)%short_name)
      call this%StatusLog%log_noerror(log_message, subname)
    end do
    call this%StatusLog%log_noerror('', subname)

    call marbl_tracer_index_consistency_check(this%tracer_indices, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('marbl_tracer_index_consistency_check', &
           subname)
      return
    end if

    !--------------------------------------------------------------------
    ! Initialize marbl diagnostics
    !--------------------------------------------------------------------

    call marbl_diagnostics_init(                                              &
         marbl_domain                 = this%domain,                          &
         marbl_tracer_metadata        = this%tracer_metadata,                 &
         marbl_tracer_indices         = this%tracer_indices,                  &
         marbl_interior_forcing_diags = this%interior_forcing_diags,          &
         marbl_surface_forcing_diags  = this%surface_forcing_diags,           &
         marbl_status_log             = this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_diagnostics_init()", subname)
      return
    end if

    !---------------------------------------------------------------------------
    ! set default values for parameters
    !---------------------------------------------------------------------------

    call marbl_parms_set_defaults(num_levels)

    !---------------------------------------------------------------------------
    ! read parameters from namelist (if present)
    !---------------------------------------------------------------------------

    if (present(gcm_nl_buffer)) then
      call marbl_parms_read_namelist(gcm_nl_buffer, this%StatusLog)
      if (this%StatusLog%labort_marbl) then
        call this%StatusLog%log_error_trace('marbl_parms_read_namelist', subname)
        return
      end if
    else
      write(log_message, "(2A)") '** No namelists were provided to init, ',   &
           'use put() and get() to change parameters'
      call this%StatusLog%log_noerror(log_message, subname)
      call this%StatusLog%log_noerror('', subname)
    end if

    end associate

    !---------------------------------------------------------------------------
    ! construct parameters_type
    !---------------------------------------------------------------------------

    call marbl_define_parameters(this%parameters, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_define_parameters()", subname)
      return
    end if

    call this%timers%stop(this%timer_ids%init_timer_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("this%timers%stop()", subname)
      return
    end if

  end subroutine init

  !***********************************************************************

  subroutine complete_config_and_init(this)

    use marbl_parms,       only : set_derived_parms
    use marbl_parms,       only : tracer_restore_vars
    use marbl_mod,         only : marbl_init_bury_coeff
    use marbl_mod,         only : marbl_init_surface_forcing_fields
    use marbl_mod,         only : marbl_init_interior_forcing_fields
    use marbl_config_mod,  only : lflux_gas_o2
    use marbl_config_mod,  only : lflux_gas_co2
    use marbl_config_mod,  only : ladjust_bury_coeff

    class(marbl_interface_class), intent(inout) :: this

    character(*), parameter :: subname = 'marbl_interface:complete_config_and_init'
    character(len=char_len) :: log_message
    integer :: i

    call this%timers%start(this%timer_ids%init_timer_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("this%timers%start()", subname)
      return
    end if

    !-----------------------------------------------------------------------
    !  Lock and log this%parameters
    !-----------------------------------------------------------------------

    call this%parameters%finalize_vars(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('parmeters%finalize_list', &
           subname)
      return
    end if

    call set_derived_parms(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('set_derived_parms', subname)
      return
    end if

    !-----------------------------------------------------------------------
    !  Initialize bury coefficient
    !-----------------------------------------------------------------------

    call marbl_init_bury_coeff(this%particulate_share, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('marbl_init_bury_coeff', subname)
      return
    end if

    associate(&
         num_surface_elements  => this%domain%num_elements_surface_forcing,   &
         num_interior_elements => this%domain%num_elements_interior_forcing,  &
         num_PAR_subcols       => this%domain%num_PAR_subcols,                &
         num_levels            => this%domain%km                              &
         )

    !-----------------------------------------------------------------------
    !  Initialize surface and interior forcing (including tracer restoring)
    !-----------------------------------------------------------------------

    call this%surface_forcing_ind%construct(ciso_on, lflux_gas_o2, lflux_gas_co2, ladjust_bury_coeff)
    call this%interior_forcing_ind%construct(this%tracer_metadata%short_name, &
                                   tracer_restore_vars, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("itnerior_forcing_ind%construct",   &
                                          subname)
      return
    end if

    call this%surface_forcing_share%construct(num_surface_elements)
    call this%surface_forcing_internal%construct(num_surface_elements)

    allocate(this%surface_input_forcings(num_surface_forcing_fields))
    call marbl_init_surface_forcing_fields(                                   &
         num_elements            = num_surface_elements,                      &
         surface_forcing_indices = this%surface_forcing_ind,                  &
         surface_forcings        = this%surface_input_forcings,               &
         marbl_status_log        = this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_surface_forcing_fields()", subname)
      return
    end if

    allocate(this%interior_input_forcings(num_interior_forcing_fields))
    call marbl_init_interior_forcing_fields(                                  &
         num_elements             = num_interior_elements,                    &
         interior_forcing_indices = this%interior_forcing_ind,                &
         tracer_metadata          = this%tracer_metadata,                     &
         num_PAR_subcols          = this%domain%num_PAR_subcols,              &
         num_levels               = this%domain%km,                           &
         interior_forcings        = this%interior_input_forcings,             &
         marbl_status_log         = this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_interior_forcing_fields()", &
                                          subname)
      return
    end if

    end associate

    !--------------------------------------------------------------------
    ! Report what forcings are required from the driver
    !--------------------------------------------------------------------

    call this%StatusLog%log_noerror('-----------------------------', subname)
    call this%StatusLog%log_noerror('MARBL-Required Forcing Fields', subname)
    call this%StatusLog%log_noerror('-----------------------------', subname)
    call this%StatusLog%log_noerror('', subname)
    call this%StatusLog%log_noerror('Surface:', subname)
    do i=1,num_surface_forcing_fields
      write(log_message, "(2A)") '* ', trim(this%surface_input_forcings(i)%metadata%varname)
      call this%StatusLog%log_noerror(log_message, subname)
    end do

    call this%StatusLog%log_noerror('', subname)
    call this%StatusLog%log_noerror('Interior:', subname)
    do i=1,num_interior_forcing_fields
      write(log_message, "(2A)") '* ', trim(this%interior_input_forcings(i)%metadata%varname)
      call this%StatusLog%log_noerror(log_message, subname)
    end do
    call this%StatusLog%log_noerror('', subname)

    ! Set up running mean variables (dependent on parms namelist)
    call this%glo_vars_init()

    ! End of initialization
    call this%timers%stop(this%timer_ids%init_timer_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("this%timers%stop()", subname)
      return
    end if

  end subroutine complete_config_and_init

  !***********************************************************************

  subroutine reset_timers(this)

    class (marbl_interface_class), intent(inout) :: this

    character(*), parameter :: subname = 'marbl_interface:reset_timers'

    call this%timers%reset(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('reset_timers', subname)
      return
    end if

  end subroutine reset_timers

  !***********************************************************************

  subroutine extract_timing(this)

    class (marbl_interface_class), intent(inout) :: this

    character(*), parameter :: subname = 'marbl_interface:extract_timing'

    call this%timers%extract(this%timer_summary, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('extract_timer_data', subname)
      return
    end if

  end subroutine extract_timing

  !***********************************************************************

  subroutine glo_vars_init(this)

    use marbl_mod, only : marbl_set_glo_vars_cnt
    use marbl_mod, only : marbl_set_rmean_init_vals

    class (marbl_interface_class), intent(inout) :: this

    integer (int_kind) :: glo_avg_field_cnt_interior
    integer (int_kind) :: glo_avg_field_cnt_surface
    integer (int_kind) :: glo_scalar_cnt_interior
    integer (int_kind) :: glo_scalar_cnt_surface

    associate(num_surface_elements => this%domain%num_elements_surface_forcing)

    call marbl_set_glo_vars_cnt(glo_avg_field_cnt_interior, &
                                glo_avg_field_cnt_surface,  &
                                glo_scalar_cnt_interior,    &
                                glo_scalar_cnt_surface)

    allocate(this%glo_avg_fields_interior(glo_avg_field_cnt_interior))
    allocate(this%glo_avg_averages_interior(glo_avg_field_cnt_interior))

    allocate(this%glo_avg_fields_surface(num_surface_elements, glo_avg_field_cnt_surface))
    allocate(this%glo_avg_averages_surface(glo_avg_field_cnt_surface))

    allocate(this%glo_scalar_interior(glo_scalar_cnt_interior))

    allocate(this%glo_scalar_surface(glo_scalar_cnt_surface))

    allocate(this%glo_avg_rmean_interior(glo_avg_field_cnt_interior))
    allocate(this%glo_avg_rmean_surface(glo_avg_field_cnt_surface))
    allocate(this%glo_scalar_rmean_interior(glo_scalar_cnt_interior))
    allocate(this%glo_scalar_rmean_surface(glo_scalar_cnt_surface))

    call marbl_set_rmean_init_vals(this%glo_avg_rmean_interior,    &
                                   this%glo_avg_rmean_surface,     &
                                   this%glo_scalar_rmean_interior, &
                                   this%glo_scalar_rmean_surface)

    end associate

  end subroutine glo_vars_init

  !***********************************************************************

  subroutine set_interior_forcing(this)

    use marbl_mod, only : marbl_set_interior_forcing

    class(marbl_interface_class), intent(inout) :: this
    character(*), parameter :: subname = 'marbl_interface:set_interior_forcing'

    call this%timers%start(this%timer_ids%interior_forcing_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%start()", subname)
      return
    end if

    call marbl_set_interior_forcing(                                          &
         domain                   = this%domain,                              &
         interior_forcings        = this%interior_input_forcings,             &
         saved_state              = this%interior_saved_state,                &
         saved_state_ind          = this%interior_state_ind,                  &
         tracers                  = this%column_tracers,                      &
         surface_forcing_indices  = this%surface_forcing_ind,                 &
         interior_forcing_indices = this%interior_forcing_ind,                &
         dtracers                 = this%column_dtracers,                     &
         marbl_tracer_indices     = this%tracer_indices,                      &
         marbl_timers             = this%timers,                              &
         marbl_timer_indices      = this%timer_ids,                           &
         PAR                      = this%PAR,                                 &
         marbl_interior_share     = this%interior_share,                      &
         marbl_zooplankton_share  = this%zooplankton_share,                   &
         marbl_autotroph_share    = this%autotroph_share,                     &
         marbl_particulate_share  = this%particulate_share,                   &
         interior_forcing_diags   = this%interior_forcing_diags,              &
         glo_avg_fields_interior  = this%glo_avg_fields_interior,             &
         marbl_status_log         = this%StatusLog)

    if (this%StatusLog%labort_marbl) then
       call this%StatusLog%log_error_trace("marbl_set_interior_forcing()", subname)
       return
    end if

    call this%timers%stop(this%timer_ids%interior_forcing_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%stop()", subname)
      return
    end if

  end subroutine set_interior_forcing

  !***********************************************************************

  subroutine set_surface_forcing(this)

    use marbl_mod      , only : marbl_set_surface_forcing

    implicit none

    class(marbl_interface_class), intent(inout) :: this

    character(*), parameter :: subname = 'marbl_interface:set_surface_forcing'

    call this%timers%start(this%timer_ids%surface_forcing_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%start()", subname)
      return
    end if

    call marbl_set_surface_forcing(                                           &
         num_elements             = this%domain%num_elements_surface_forcing, &
         surface_forcing_ind      = this%surface_forcing_ind,                 &
         surface_input_forcings   = this%surface_input_forcings,              &
         surface_vals             = this%surface_vals,                        &
         surface_tracer_fluxes    = this%surface_tracer_fluxes,               &
         marbl_tracer_indices     = this%tracer_indices,                      &
         saved_state              = this%surface_saved_state,                 &
         saved_state_ind          = this%surf_state_ind,                      &
         surface_forcing_output   = this%surface_forcing_output,              &
         surface_forcing_internal = this%surface_forcing_internal,            &
         surface_forcing_share    = this%surface_forcing_share,               &
         surface_forcing_diags    = this%surface_forcing_diags,               &
         glo_avg_fields_surface   = this%glo_avg_fields_surface,              &
         marbl_status_log         = this%StatusLog)
    if (this%StatusLog%labort_marbl) then
       call this%StatusLog%log_error_trace("marbl_set_surface_forcing()", subname)
       return
    end if


    call this%timers%stop(this%timer_ids%surface_forcing_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%stop()", subname)
      return
    end if

  end subroutine set_surface_forcing

  !***********************************************************************

  subroutine set_global_scalars(this, field_source)

    use marbl_mod, only : marbl_set_global_scalars_interior

    implicit none

    class(marbl_interface_class), intent(inout) :: this
    character (*)               , intent(in)    :: field_source ! 'interior' or 'surface'

    if (field_source == 'interior') then
       call marbl_set_global_scalars_interior(                          &
            marbl_particulate_share   = this%particulate_share,         &
            glo_avg_rmean_interior    = this%glo_avg_rmean_interior,    &
            glo_avg_rmean_surface     = this%glo_avg_rmean_surface,     &
            glo_scalar_rmean_interior = this%glo_scalar_rmean_interior, &
            glo_scalar_rmean_surface  = this%glo_scalar_rmean_surface,  &
            glo_scalar_interior       = this%glo_scalar_interior)
    end if

  end subroutine set_global_scalars

  !***********************************************************************

  subroutine shutdown(this)

    implicit none

    class(marbl_interface_class), intent(inout) :: this

    character(*), parameter :: subname = 'marbl_interface:shutdown'

    ! free dynamically allocated memory, etc

    call this%timers%shutdown(this%timer_ids, this%timer_summary, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('shutdown_timers', subname)
      return
    end if

  end subroutine shutdown

  !***********************************************************************

  function get_tracer_index(this, tracer_name)

    class(marbl_interface_class), intent(inout) :: this
    character(*),                 intent(in)    :: tracer_name
    integer :: get_tracer_index

    integer :: n

    get_tracer_index = 0
    do n=1,marbl_total_tracer_cnt
      if (trim(tracer_name).eq.trim(this%tracer_metadata(n)%short_name) .or.  &
          trim(tracer_name).eq.trim(this%tracer_metadata(n)%long_name)) then
        get_tracer_index = n
        exit
      end if
    end do

  end function get_tracer_index

  !*****************************************************************************

end module marbl_interface
