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

  use marbl_kinds_mod, only : r8, log_kind, int_kind, log_kind, char_len

  use marbl_settings_mod, only : zooplankton_cnt
  use marbl_settings_mod, only : marbl_settings_type

  use marbl_logging, only : marbl_log_type

  use marbl_interface_public_types, only : marbl_domain_type
  use marbl_interface_public_types, only : marbl_tracer_metadata_type
  use marbl_interface_public_types, only : marbl_surface_flux_output_type
  use marbl_interface_public_types, only : marbl_diagnostics_type
  use marbl_interface_public_types, only : marbl_forcing_fields_type
  use marbl_interface_public_types, only : marbl_saved_state_type
  use marbl_interface_public_types, only : marbl_timers_type
  use marbl_interface_public_types, only : marbl_running_mean_0d_type

  use marbl_interface_private_types, only : marbl_surface_flux_forcing_indexing_type
  use marbl_interface_private_types, only : marbl_surface_flux_saved_state_indexing_type
  use marbl_interface_private_types, only : marbl_interior_tendency_forcing_indexing_type
  use marbl_interface_private_types, only : marbl_interior_tendency_saved_state_indexing_type
  use marbl_interface_private_types, only : marbl_PAR_type
  use marbl_interface_private_types, only : autotroph_derived_terms_type
  use marbl_interface_private_types, only : autotroph_local_type
  use marbl_interface_private_types, only : zooplankton_derived_terms_type
  use marbl_interface_private_types, only : zooplankton_local_type
  use marbl_interface_private_types, only : zooplankton_share_type
  use marbl_interface_private_types, only : marbl_particulate_share_type
  use marbl_interface_private_types, only : marbl_interior_tendency_share_type
  use marbl_interface_private_types, only : dissolved_organic_matter_type
  use marbl_interface_private_types, only : carbonate_type
  use marbl_interface_private_types, only : marbl_surface_flux_share_type
  use marbl_interface_private_types, only : marbl_surface_flux_internal_type
  use marbl_interface_private_types, only : marbl_tracer_index_type
  use marbl_interface_private_types, only : marbl_internal_timers_type
  use marbl_interface_private_types, only : marbl_timer_indexing_type

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
     type(marbl_domain_type)                        , public  :: domain
     type(marbl_tracer_metadata_type)  , allocatable, public  :: tracer_metadata(:)
     ! Pointer so that destructor doesn't need to reset all inds to 0
     ! (that happens automatically when new tracer indexing type is allocated)
     type(marbl_tracer_index_type)     , pointer    , public  :: tracer_indices => NULL()
     type(marbl_log_type)                           , public  :: StatusLog

     type(marbl_saved_state_type)              , public               :: surface_flux_saved_state             ! input/output
     type(marbl_saved_state_type)              , public               :: interior_tendency_saved_state        ! input/output
     type(marbl_surface_flux_saved_state_indexing_type), public       :: surf_state_ind
     type(marbl_interior_tendency_saved_state_indexing_type), public  :: interior_state_ind
     type(marbl_timers_type)                   , public               :: timer_summary

     ! public data related to computing interior tendencies
     real (r8), allocatable                             , public  :: tracers(:,:)                  ! input
     type(marbl_forcing_fields_type), allocatable       , public  :: interior_tendency_forcings(:) ! input
     real (r8), allocatable                             , public  :: interior_tendencies(:,:)      ! output
     type(marbl_interior_tendency_forcing_indexing_type), public  :: interior_tendency_forcing_ind ! FIXME #311: should be private
     type(marbl_diagnostics_type)                       , public  :: interior_tendency_diags       ! output

     ! public data related to computing surface fluxes
     real (r8)                                      , public, allocatable  :: tracers_at_surface(:,:)     ! input
     type(marbl_forcing_fields_type)                , public, allocatable  :: surface_flux_forcings(:)    ! input
     type(marbl_surface_flux_forcing_indexing_type) , public               :: surface_flux_forcing_ind    ! FIXME #311: should be private
     real (r8)                                      , public, allocatable  :: surface_fluxes(:,:)         ! output
     type(marbl_surface_flux_output_type)           , public               :: surface_flux_output         ! output
     type(marbl_diagnostics_type)                   , public               :: surface_flux_diags          ! output

     ! public data - global averages
     real (r8)                                 , public, allocatable  :: glo_avg_fields_interior_tendency(:)   ! output (nfields)
     real (r8)                                 , public, allocatable  :: glo_avg_averages_interior_tendency(:) ! input (nfields)
     real (r8)                                 , public, allocatable  :: glo_avg_fields_surface_flux(:,:)      ! output (num_elements,nfields)
     real (r8)                                 , public, allocatable  :: glo_avg_averages_surface_flux(:)      ! input (nfields)

     ! FIXME #77: for now, running means are being computed in the driver
     !            they will eventually be moved from the interface to inside MARBL
     real (r8)                                 , public, allocatable  :: glo_scalar_interior_tendency(:)
     real (r8)                                 , public, allocatable  :: glo_scalar_surface_flux(:)

     type(marbl_running_mean_0d_type)          , public, allocatable  :: glo_avg_rmean_interior_tendency(:)
     type(marbl_running_mean_0d_type)          , public, allocatable  :: glo_avg_rmean_surface_flux(:)
     type(marbl_running_mean_0d_type)          , public, allocatable  :: glo_scalar_rmean_interior_tendency(:)
     type(marbl_running_mean_0d_type)          , public, allocatable  :: glo_scalar_rmean_surface_flux(:)

     ! private data
     type(marbl_PAR_type),                     private :: PAR
     type(autotroph_derived_terms_type),       private :: autotroph_derived_terms
     type(autotroph_local_type),               private :: autotroph_local
     type(zooplankton_derived_terms_type),     private :: zooplankton_derived_terms
     type(zooplankton_local_type),             private :: zooplankton_local
     type(zooplankton_share_type),             private :: zooplankton_share
     type(marbl_particulate_share_type),       private :: particulate_share
     type(marbl_interior_tendency_share_type), private :: interior_tendency_share
     type(dissolved_organic_matter_type),      private :: dissolved_organic_matter
     type(carbonate_type),                     private :: carbonate
     type(marbl_surface_flux_share_type),      private :: surface_flux_share
     type(marbl_surface_flux_internal_type),   private :: surface_flux_internal
     logical,                                  private :: lallow_glo_ops
     type(marbl_internal_timers_type),         private :: timers
     type(marbl_timer_indexing_type),          private :: timer_ids
     type(marbl_settings_type),                private :: settings

   contains

     procedure, public  :: init
     procedure, public  :: reset_timers
     procedure, public  :: extract_timing
     procedure, private :: glo_vars_init
     procedure, public  :: get_tracer_index
     procedure, public  :: interior_tendency_compute
     procedure, public  :: surface_flux_compute
     procedure, public  :: set_global_scalars
     procedure, public  :: shutdown
     generic            :: inquire_settings_metadata => inquire_settings_metadata_by_name, &
                                                        inquire_settings_metadata_by_id
     generic            :: put_setting => put_real,            &
                                          put_integer,         &
                                          put_logical,         &
                                          put_string,          & ! This routine checks to see if string is actually an array
                                          put_input_file_line, & ! This line converts string "var = val" to proper put()
                                          put_all_string
     generic            :: get_setting => get_real,    &
                                          get_integer, &
                                          get_logical, &
                                          get_string
     procedure, public  :: get_settings_var_cnt
     procedure, private :: inquire_settings_metadata_by_name
     procedure, private :: inquire_settings_metadata_by_id
     procedure, private :: put_real
     procedure, private :: put_integer
     procedure, private :: put_logical
     procedure, private :: put_string
     procedure, private :: put_input_file_line
     procedure, private :: put_all_string
     procedure, private :: get_real
     procedure, private :: get_integer
     procedure, private :: get_logical
     procedure, private :: get_string

  end type marbl_interface_class

  private :: init
  private :: reset_timers
  private :: extract_timing
  private :: glo_vars_init
  private :: interior_tendency_compute
  private :: surface_flux_compute
  private :: shutdown

  !***********************************************************************

contains

  !***********************************************************************

  subroutine init(this,                   &
       gcm_num_levels,                    &
       gcm_num_PAR_subcols,               &
       gcm_num_elements_surface_flux,     &
       gcm_delta_z,                       &
       gcm_zw,                            &
       gcm_zt,                            &
       lgcm_has_global_ops)

    use marbl_init_mod, only : marbl_init_log_and_timers
    use marbl_init_mod, only : marbl_init_parameters_pre_tracers
    use marbl_init_mod, only : marbl_init_tracers
    use marbl_init_mod, only : marbl_init_parameters_post_tracers
    use marbl_init_mod, only : marbl_init_bury_coeff
    use marbl_init_mod, only : marbl_init_forcing_fields
    use marbl_settings_mod, only : marbl_settings_set_all_derived
    use marbl_settings_mod, only : marbl_settings_consistency_check
    use marbl_settings_mod, only : autotroph_cnt
    use marbl_settings_mod, only : ciso_on
    use marbl_diagnostics_mod, only : marbl_diagnostics_init
    use marbl_saved_state_mod, only : marbl_saved_state_init

    class(marbl_interface_class), intent(inout) :: this
    integer(int_kind),            intent(in)    :: gcm_num_levels
    integer(int_kind),            intent(in)    :: gcm_num_PAR_subcols
    integer(int_kind),            intent(in)    :: gcm_num_elements_surface_flux
    real(r8),                     intent(in)    :: gcm_delta_z(gcm_num_levels) ! thickness of layer k
    real(r8),                     intent(in)    :: gcm_zw(gcm_num_levels) ! thickness of layer k
    real(r8),                     intent(in)    :: gcm_zt(gcm_num_levels) ! thickness of layer k
    logical,           optional,  intent(in)    :: lgcm_has_global_ops

    character(len=*), parameter :: subname = 'marbl_interface:init'
    integer, parameter :: num_elements_interior_tendency = 1 ! FIXME #66: get this value from interface, let it vary

    !--------------------------------------------------------------------
    ! initialize status log and timers
    !--------------------------------------------------------------------

    call marbl_init_log_and_timers(this%timers, this%timer_ids, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_log_and_timers", subname)
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
    ! Initialize parameters that do not depend on tracer count or PFT categories
    !---------------------------------------------------------------------------

    call marbl_init_parameters_pre_tracers(this%settings, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_parameters_pre_tracers", subname)
      return
    end if

    associate(&
         num_levels            => gcm_num_levels,                              &
         num_PAR_subcols       => gcm_num_PAR_subcols,                         &
         num_elements_surface_flux  => gcm_num_elements_surface_flux           &
         )

    !-----------------------------------------------------------------------
    !  Set up domain type
    !-----------------------------------------------------------------------

    call this%domain%construct(                                           &
         num_levels                     = num_levels,                     &
         num_PAR_subcols                = num_PAR_subcols,                &
         num_elements_surface_flux      = num_elements_surface_flux,      &
         num_elements_interior_tendency = num_elements_interior_tendency, &
         delta_z                        = gcm_delta_z,                    &
         zw                             = gcm_zw,                         &
         zt                             = gcm_zt)

    !--------------------------------------------------------------------
    ! call constructors and allocate memory
    !--------------------------------------------------------------------

    call this%PAR%construct(num_levels, num_PAR_subcols)
    call this%dissolved_organic_matter%construct(num_levels)
    call this%carbonate%construct(num_levels)
    call this%particulate_share%construct(num_levels)
    call this%autotroph_derived_terms%construct(autotroph_cnt, num_levels)
    call this%autotroph_local%construct(ciso_on, autotroph_cnt, num_levels)
    call this%zooplankton_derived_terms%construct(zooplankton_cnt, num_levels)
    call this%zooplankton_local%construct(zooplankton_cnt, num_levels)
    if (ciso_on) then
      call this%zooplankton_share%construct(num_levels)
      call this%interior_tendency_share%construct(num_levels)
    end if

    !-----------------------------------------------------------------------
    !  Set up tracers
    !-----------------------------------------------------------------------

    call marbl_init_tracers(num_levels, num_elements_surface_flux, &
                            this%tracer_indices, this%tracers_at_surface, this%surface_fluxes, &
                            this%tracers, this%interior_tendencies, this%tracer_metadata, &
                            this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_tracers", subname)
      return
    end if

    !--------------------------------------------------------------------
    ! set up saved state variables
    !--------------------------------------------------------------------

    call marbl_saved_state_init(this%surface_flux_saved_state,                &
                                this%interior_tendency_saved_state,           &
                                this%surf_state_ind,                          &
                                this%interior_state_ind,                      &
                                num_levels,                                   &
                                num_elements_surface_flux,                    &
                                num_elements_interior_tendency,               &
                                this%StatusLog)

    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_saved_state_init()", subname)
      return
    end if

    !--------------------------------------------------------------------
    ! Initialize marbl diagnostics
    !--------------------------------------------------------------------

    call marbl_diagnostics_init(                                              &
         marbl_domain                  = this%domain,                         &
         marbl_tracer_metadata         = this%tracer_metadata,                &
         marbl_tracer_indices          = this%tracer_indices,                 &
         marbl_interior_tendency_diags = this%interior_tendency_diags,        &
         marbl_surface_flux_diags      = this%surface_flux_diags,             &
         marbl_status_log              = this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_diagnostics_init()", subname)
      return
    end if

    !---------------------------------------------------------------------------
    ! Initialize parameters that depend on tracer count
    !---------------------------------------------------------------------------
    call marbl_init_parameters_post_tracers(this%settings, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_parameters_post_tracers", subname)
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

    end associate

    !-----------------------------------------------------------------------
    !  Initialize surface and interior forcing (including tracer restoring)
    !-----------------------------------------------------------------------

    call marbl_init_forcing_fields(this%domain, &
                                   this%tracer_metadata, &
                                   this%surface_flux_forcing_ind, &
                                   this%surface_flux_forcings, &
                                   this%interior_tendency_forcing_ind, &
                                   this%interior_tendency_forcings, &
                                   this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_forcing_fields", subname)
      return
    end if

    ! Surface forcing constructors
    if (ciso_on) call this%surface_flux_share%construct(this%domain%num_elements_surface_flux)
    call this%surface_flux_internal%construct(this%domain%num_elements_surface_flux)

    ! Set up running mean variables (dependent on parms namelist)
    call this%glo_vars_init()

    !  Lock and log configuration variables and parameters
    call this%settings%finalize_vars(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('parmeters%finalize_vars', subname)
      return
    end if

    ! Set variables that depend on previously-set values
    ! (Typically unit conversion or string -> int for easy comparison)
    call marbl_settings_set_all_derived(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('marbl_settings_set_all_derived', subname)
      return
    end if

    call marbl_settings_consistency_check(this%lallow_glo_ops, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('marbl_settings_consistency_check', subname)
      return
    end if

    ! End of initialization
    call this%timers%stop(this%timer_ids%init_timer_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("this%timers%stop()", subname)
      return
    end if

  end subroutine init

  !***********************************************************************

  subroutine put_real(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    real(r8),                      intent(in)    :: val

    character(len=*), parameter :: subname = 'marbl_interface:put_real'

    call this%settings%put(varname, this%StatusLog, rval=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%put()', subname)
      return
    end if

  end subroutine put_real

  !***********************************************************************

  subroutine put_integer(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    integer(int_kind),             intent(in)    :: val

    character(len=*), parameter :: subname = 'marbl_interface:put_integer'

    call this%settings%put(varname, this%StatusLog, ival=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%put()', subname)
      return
    end if

  end subroutine put_integer

  !***********************************************************************

  subroutine put_logical(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    logical,                       intent(in)    :: val

    character(len=*), parameter :: subname = 'marbl_interface:put_logical'

    call this%settings%put(varname, this%StatusLog, lval=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%put()', subname)
      return
    end if

  end subroutine put_logical

  !***********************************************************************

  subroutine put_string(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    character(len=*),              intent(in)    :: val

    character(len=*), parameter :: subname = 'marbl_interface:put_string'

    call this%settings%put(varname, this%StatusLog, sval=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%put()', subname)
      return
    end if

  end subroutine put_string

  !***********************************************************************

  subroutine put_all_string(this, varname, datatype, val)
    ! This interface to put_setting() is called from put_input_file_line()

    use marbl_settings_mod, only : marbl_settings_string_to_var

    class (marbl_interface_class),  intent(inout) :: this
    character(len=*),               intent(in)    :: varname, datatype, val

    character(len=*), parameter :: subname = 'marbl_interface:put_all_string'
    character(len=char_len)     :: log_message

    real(r8)                :: rval
    integer(int_kind)       :: ival
    logical(log_kind)       :: lval

    select case (trim(datatype))
      case ('logical')
        call marbl_settings_string_to_var(val, this%StatusLog, lval = lval)
        if (this%StatusLog%labort_marbl) then
          call this%StatusLog%log_error_trace('marbl_settings_string_to_var', subname)
          return
        end if
        call this%settings%put(trim(varname), this%StatusLog, lval=lval)
      case ('real')
        call marbl_settings_string_to_var(val, this%StatusLog, rval = rval)
        if (this%StatusLog%labort_marbl) then
          call this%StatusLog%log_error_trace('marbl_settings_string_to_var', subname)
          return
        end if
        call this%settings%put(trim(varname), this%StatusLog, rval=rval)
      case ('integer')
        call marbl_settings_string_to_var(val, this%StatusLog, ival = ival)
        if (this%StatusLog%labort_marbl) then
          call this%StatusLog%log_error_trace('marbl_settings_string_to_var', subname)
          return
        end if
        call this%settings%put(trim(varname), this%StatusLog, ival=ival)
      case ('string')
        call this%settings%put(trim(varname), this%StatusLog, sval=val)
      case('unknown')
        ! Is val an array?
        call this%settings%put(trim(varname), this%StatusLog, uval=val)
      case DEFAULT
        call this%StatusLog%construct()
        write(log_message,"(2A)") trim(datatype), " is not a recognized type"
        call this%StatusLog%log_error(log_message, subname)
    end select
    if (this%StatusLog%labort_marbl) then
      write(log_message, "(3A)") "string_to_var(", trim(val), ")"
      call this%StatusLog%log_error_trace(log_message, subname)
      return
    end if

  end subroutine put_all_string

  !***********************************************************************

  subroutine put_input_file_line(this, line, pgi_bugfix_var)

    ! This subroutine takes a single line from MARBL's default input file format,
    ! determines the variable name and whether the value is a scalar or an array,
    ! and then calls put_setting (once for a scalar, per-element for an array).
    !
    ! The put_setting() call forces the datatype to be "unknown", so when the
    ! put list is traversed by add_var the datatype is assumed to match new_entry
    ! (and error checking verifies that it is an appropriate value)

    use marbl_utils_mod, only : marbl_utils_str_to_substrs

    class(marbl_interface_class), intent(inout) :: this
    character(len=*),             intent(in)    :: line
    ! For some reason PGI doesn't like this particular interface to put_setting()
    ! --- Error from building stand-alone driver ---
    ! PGF90-S-0155-cannot access PRIVATE type bound procedure
    ! put_input_file_line$tbp (/NO_BACKUP/codes/marbl/tests/driver_src/marbl.F90: 239)
    !
    ! But adding another variable to the interface makes it okay
    logical, optional,            intent(in)    :: pgi_bugfix_var(0)

    character(len=char_len), dimension(:), allocatable :: value, line_loc_arr
    character(len=char_len) :: varname, var_loc, line_loc
    integer(int_kind)       :: n, char_ind

    line_loc = ''
    ! The included PGI bugfix variable triggers a warning from gfortran unless it's used
    if (present(pgi_bugfix_var)) line_loc=''
    ! Strip out comments (denoted by '!'); line_loc_arr(1) is the line to be processed
    call marbl_utils_str_to_substrs(line, '!', line_loc_arr)
    line_loc = line_loc_arr(1)

    ! Return without processing if
    ! (a) line is empty / only contains spaces
    if (len_trim(line_loc) .eq. 0) return
    ! (b) first non-space character is '&' (can use namelist files)
    if (line_loc(1:1) .eq. '&') return
    ! (c) line contains only '/' (can use namelist files)
    if (trim(line_loc) .eq. '/') return

    ! Everything up to first '=' is varname
    varname = ''
    do char_ind = 1, len_trim(line_loc)
      if (line_loc(char_ind:char_ind) .eq. '=') then
        line_loc(char_ind:char_ind) = ' '
        exit
      end if
      varname(char_ind:char_ind) = line_loc(char_ind:char_ind)
      line_loc(char_ind:char_ind) = ' '
    end do

    ! Everything to the right of the first '=' is the variable value, which might be an array
    call marbl_utils_str_to_substrs(line_loc, ',', value)
    var_loc = varname
    do n=1, size(value)
      if (size(value) .gt. 1) write(var_loc, "(2A,I0,A)") trim(varname), '(', n, ')'
      call this%put_setting(var_loc, "unknown", value(n))
    end do
    deallocate(value)

  end subroutine put_input_file_line

  !***********************************************************************

  subroutine get_real(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    real(r8),                      intent(out)   :: val

    character(len=*), parameter :: subname = 'marbl_interface:get_real'

    call this%settings%get(varname, this%StatusLog, rval=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%get()', subname)
      return
    end if

  end subroutine get_real

  !***********************************************************************

  subroutine get_integer(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    integer(int_kind),             intent(out)   :: val

    character(len=*), parameter :: subname = 'marbl_interface:get_integer'

    call this%settings%get(varname, this%StatusLog, ival=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%get()', subname)
      return
    end if

  end subroutine get_integer

  !***********************************************************************

  subroutine get_logical(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    logical,                       intent(out)   :: val

    character(len=*), parameter :: subname = 'marbl_interface:get_logical'

    call this%settings%get(varname, this%StatusLog, lval=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%get()', subname)
      return
    end if

  end subroutine get_logical

  !***********************************************************************

  subroutine get_string(this, varname, val, linput_file_format)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    character(len=*),              intent(out)   :: val
    logical, optional,             intent(in)    :: linput_file_format

    character(len=*), parameter :: subname = 'marbl_interface:get_string'
    character(len=char_len) :: log_message

    logical :: linput_file_format_loc
    character(len=char_len) :: datatype
    real(r8)                :: rval
    integer                 :: ival
    logical                 :: lval
    character(len=char_len) :: sval

    val = ''
    if (present(linput_file_format)) then
      linput_file_format_loc = linput_file_format
    else
      linput_file_format_loc = .false.
    end if

    if (linput_file_format_loc) then
      ! Determine datatype
      call this%inquire_settings_metadata(varname, datatype=datatype)
      if (this%StatusLog%labort_marbl) then
        call this%StatusLog%log_error_trace('inquire_settings_metadata', subname)
        return
      end if
      select case (trim(datatype))
        case ('real')
          call this%settings%get(varname, this%StatusLog, rval=rval)
          write(val, "(A,' = ', E25.17)") trim(varname), rval
        case ('integer')
          call this%settings%get(varname, this%StatusLog, ival=ival)
          write(val, "(A, ' = ', I0)") trim(varname), ival
        case ('logical')
          call this%settings%get(varname, this%StatusLog, lval=lval)
          write(val, "(A, ' = ', L1)") trim(varname), lval
        case ('string')
          call this%settings%get(varname, this%StatusLog, sval=sval)
          write(val, "(A, ' = ', 3A)") trim(varname), "'", trim(sval), "'"
        case DEFAULT
          write(log_message, "(3A)") "Unknown datatype '", trim(datatype), &
                                     "' returned from inquire_settings_metadata()"
          call this%StatusLog%log_error(log_message, subname)
          return
      end select
    else
      call this%settings%get(varname, this%StatusLog, sval=val)
    end if ! linput_file_format_loc

    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%get()', subname)
    end if

  end subroutine get_string

  !***********************************************************************

  function get_settings_var_cnt(this) result(cnt)

    class (marbl_interface_class), intent(in) :: this
    integer :: cnt

    cnt = this%settings%get_cnt()

  end function get_settings_var_cnt

  !***********************************************************************

  subroutine inquire_settings_metadata_by_name(this, varname, id, lname, units, datatype)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    integer(int_kind), optional,   intent(out)   :: id
    character(len=*),  optional,   intent(out)   :: lname, units, datatype

    character(len=*), parameter :: subname = 'marbl_interface:inquire_settings_metadata_by_name'
    integer :: id_loc

    id_loc = this%settings%inquire_id(varname, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%inquire_id', subname)
      return
    end if
    if (present(id)) id = id_loc
    if (any((/present(lname), present(units), present(datatype)/))) then
      call this%settings%inquire_metadata(id_loc,                  &
                                          lname    = lname,        &
                                          units    = units,        &
                                          datatype = datatype)
    end if

  end subroutine inquire_settings_metadata_by_name

  !***********************************************************************

  subroutine inquire_settings_metadata_by_id(this, id, sname, lname, units, datatype)

    class (marbl_interface_class), intent(inout) :: this
    integer(int_kind),             intent(in)    :: id
    character(len=*), optional,    intent(out)   :: sname, lname, units
    character(len=*), optional,    intent(out)   :: datatype

    call this%settings%inquire_metadata(id,                  &
                                        sname    = sname,    &
                                        lname    = lname,    &
                                        units    = units,    &
                                        datatype = datatype)

  end subroutine inquire_settings_metadata_by_id

  !***********************************************************************

  subroutine reset_timers(this)

    class (marbl_interface_class), intent(inout) :: this

    character(len=*), parameter :: subname = 'marbl_interface:reset_timers'

    call this%timers%reset(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('reset_timers', subname)
      return
    end if

  end subroutine reset_timers

  !***********************************************************************

  subroutine extract_timing(this)

    class (marbl_interface_class), intent(inout) :: this

    character(len=*), parameter :: subname = 'marbl_interface:extract_timing'

    call this%timers%extract(this%timer_summary, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('extract_timer_data', subname)
      return
    end if

  end subroutine extract_timing

  !***********************************************************************

  subroutine glo_vars_init(this)

    use marbl_glo_avg_mod, only : marbl_glo_avg_var_cnts_compute
    use marbl_glo_avg_mod, only : marbl_glo_avg_init_rmean_vals

    class (marbl_interface_class), intent(inout) :: this

    integer (int_kind) :: glo_avg_field_cnt_interior_tendency
    integer (int_kind) :: glo_avg_field_cnt_surface_flux
    integer (int_kind) :: glo_scalar_cnt_interior_tendency
    integer (int_kind) :: glo_scalar_cnt_surface_flux

    associate(num_elements_surface_flux => this%domain%num_elements_surface_flux)

    call marbl_glo_avg_var_cnts_compute(glo_avg_field_cnt_interior_tendency, &
                                        glo_avg_field_cnt_surface_flux,      &
                                        glo_scalar_cnt_interior_tendency,    &
                                        glo_scalar_cnt_surface_flux)

    allocate(this%glo_avg_fields_interior_tendency(glo_avg_field_cnt_interior_tendency))
    allocate(this%glo_avg_averages_interior_tendency(glo_avg_field_cnt_interior_tendency))

    allocate(this%glo_avg_fields_surface_flux(num_elements_surface_flux, glo_avg_field_cnt_surface_flux))
    allocate(this%glo_avg_averages_surface_flux(glo_avg_field_cnt_surface_flux))

    allocate(this%glo_scalar_interior_tendency(glo_scalar_cnt_interior_tendency))

    allocate(this%glo_scalar_surface_flux(glo_scalar_cnt_surface_flux))

    allocate(this%glo_avg_rmean_interior_tendency(glo_avg_field_cnt_interior_tendency))
    allocate(this%glo_avg_rmean_surface_flux(glo_avg_field_cnt_surface_flux))
    allocate(this%glo_scalar_rmean_interior_tendency(glo_scalar_cnt_interior_tendency))
    allocate(this%glo_scalar_rmean_surface_flux(glo_scalar_cnt_surface_flux))

    call marbl_glo_avg_init_rmean_vals(this%glo_avg_rmean_interior_tendency,    &
                                       this%glo_avg_rmean_surface_flux,         &
                                       this%glo_scalar_rmean_interior_tendency, &
                                       this%glo_scalar_rmean_surface_flux)

    end associate

  end subroutine glo_vars_init

  !***********************************************************************

  subroutine interior_tendency_compute(this)

    use marbl_interior_tendency_mod, only : marbl_interior_tendency_compute

    class(marbl_interface_class), intent(inout) :: this

    character(len=*), parameter :: subname = 'marbl_interface:interior_tendency_compute'

    call this%timers%start(this%timer_ids%interior_tendency_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%start()", subname)
      return
    end if

    call marbl_interior_tendency_compute(                                           &
         domain                            = this%domain,                           &
         interior_tendency_forcings        = this%interior_tendency_forcings,       &
         tracers                           = this%tracers,                          &
         surface_flux_forcing_indices      = this%surface_flux_forcing_ind,         &
         interior_tendency_forcing_indices = this%interior_tendency_forcing_ind,    &
         saved_state_ind                   = this%interior_state_ind,               &
         marbl_tracer_indices              = this%tracer_indices,                   &
         marbl_timer_indices               = this%timer_ids,                        &
         PAR                               = this%PAR,                              &
         dissolved_organic_matter          = this%dissolved_organic_matter,         &
         carbonate                         = this%carbonate,                        &
         autotroph_derived_terms           = this%autotroph_derived_terms,          &
         autotroph_local                   = this%autotroph_local,                  &
         zooplankton_derived_terms         = this%zooplankton_derived_terms,        &
         zooplankton_local                 = this%zooplankton_local,                &
         zooplankton_share                 = this%zooplankton_share,                &
         saved_state                       = this%interior_tendency_saved_state,    &
         marbl_timers                      = this%timers,                           &
         interior_tendency_share           = this%interior_tendency_share,          &
         marbl_particulate_share           = this%particulate_share,                &
         interior_tendency_diags           = this%interior_tendency_diags,          &
         interior_tendencies               = this%interior_tendencies,              &
         glo_avg_fields_interior_tendency  = this%glo_avg_fields_interior_tendency, &
         marbl_status_log                  = this%StatusLog)

    if (this%StatusLog%labort_marbl) then
       call this%StatusLog%log_error_trace("marbl_interior_tendency_compute()", subname)
       return
    end if

    call this%timers%stop(this%timer_ids%interior_tendency_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%stop()", subname)
      return
    end if

  end subroutine interior_tendency_compute

  !***********************************************************************

  subroutine surface_flux_compute(this)

    use marbl_surface_flux_mod, only : marbl_surface_flux_compute

    class(marbl_interface_class), intent(inout) :: this

    character(len=*), parameter :: subname = 'marbl_interface:surface_flux_compute'

    call this%timers%start(this%timer_ids%surface_flux_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%start()", subname)
      return
    end if

    call marbl_surface_flux_compute(                                          &
         num_elements             = this%domain%num_elements_surface_flux,    &
         surface_flux_forcing_ind = this%surface_flux_forcing_ind,            &
         surface_flux_forcings    = this%surface_flux_forcings,               &
         tracers_at_surface       = this%tracers_at_surface,                  &
         surface_fluxes           = this%surface_fluxes,                      &
         marbl_tracer_indices     = this%tracer_indices,                      &
         saved_state              = this%surface_flux_saved_state,            &
         saved_state_ind          = this%surf_state_ind,                      &
         surface_flux_output      = this%surface_flux_output,                 &
         surface_flux_internal    = this%surface_flux_internal,               &
         surface_flux_share       = this%surface_flux_share,                  &
         surface_flux_diags       = this%surface_flux_diags,                  &
         glo_avg_fields_surface_flux = this%glo_avg_fields_surface_flux,      &
         marbl_status_log         = this%StatusLog)
    if (this%StatusLog%labort_marbl) then
       call this%StatusLog%log_error_trace("marbl_surface_flux_compute()", subname)
       return
    end if


    call this%timers%stop(this%timer_ids%surface_flux_id, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("timers%stop()", subname)
      return
    end if

  end subroutine surface_flux_compute

  !***********************************************************************

  subroutine set_global_scalars(this, field_source)

    use marbl_interior_tendency_mod, only : marbl_interior_tendency_adjust_bury_coeff

    class(marbl_interface_class), intent(inout) :: this
    character(len=*),             intent(in)    :: field_source ! 'interior_tendency' or 'surface_flux`'

    if (field_source == 'interior_tendency') then
       call marbl_interior_tendency_adjust_bury_coeff(                                    &
            marbl_particulate_share            = this%particulate_share,                  &
            glo_avg_rmean_interior_tendency    = this%glo_avg_rmean_interior_tendency,    &
            glo_avg_rmean_surface_flux         = this%glo_avg_rmean_surface_flux,         &
            glo_scalar_rmean_interior_tendency = this%glo_scalar_rmean_interior_tendency, &
            glo_scalar_interior_tendency       = this%glo_scalar_interior_tendency)
    end if

  end subroutine set_global_scalars

  !***********************************************************************

  subroutine shutdown(this)

    use marbl_settings_mod, only : ciso_on
    use marbl_settings_mod, only : max_grazer_prey_cnt
    use marbl_settings_mod, only : autotroph_settings
    use marbl_settings_mod, only : zooplankton_settings
    use marbl_settings_mod, only : grazing_relationship_settings
    use marbl_settings_mod, only : tracer_restore_vars
    use marbl_diagnostics_mod, only : marbl_interior_tendency_diag_ind

    class(marbl_interface_class), intent(inout) :: this

    character(len=*), parameter :: subname = 'marbl_interface:shutdown'
    integer(int_kind) :: m,n

    if (allocated(this%glo_avg_fields_interior_tendency)) then
      deallocate(this%glo_avg_fields_interior_tendency)
      deallocate(this%glo_avg_averages_interior_tendency)
      deallocate(this%glo_avg_fields_surface_flux)
      deallocate(this%glo_avg_averages_surface_flux)
      deallocate(this%glo_scalar_interior_tendency)
      deallocate(this%glo_scalar_surface_flux)
      deallocate(this%glo_avg_rmean_interior_tendency)
      deallocate(this%glo_avg_rmean_surface_flux)
      deallocate(this%glo_scalar_rmean_interior_tendency)
      deallocate(this%glo_scalar_rmean_surface_flux)
    end if

    ! free dynamically allocated memory, etc
    ! FIXME #69: this is not ideal for threaded runs
    if (allocated(autotroph_settings)) then
      deallocate(autotroph_settings)
      deallocate(zooplankton_settings)
      do m=1,max_grazer_prey_cnt
        do n=1,zooplankton_cnt
          deallocate(grazing_relationship_settings(m,n)%auto_ind)
          deallocate(grazing_relationship_settings(m,n)%zoo_ind)
        end do
      end do
      deallocate(grazing_relationship_settings)
    end if
    call marbl_interior_tendency_diag_ind%destruct()

    if (allocated(this%interior_tendency_forcings)) then
      deallocate(this%interior_tendency_forcings)
      deallocate(this%surface_flux_forcings)
    end if
    call this%surface_flux_internal%destruct()
    if (ciso_on) call this%surface_flux_share%destruct()
    if (allocated(this%tracers_at_surface)) then
      deallocate(this%tracers_at_surface)
      deallocate(this%surface_fluxes)
      deallocate(this%tracers)
      deallocate(this%interior_tendencies)
      deallocate(this%tracer_metadata)
      deallocate(tracer_restore_vars)
    end if
    call this%tracer_indices%destruct()
    deallocate(this%tracer_indices)
    call this%settings%destruct()
    call this%particulate_share%destruct()
    call this%PAR%destruct()
    call this%dissolved_organic_matter%destruct()
    call this%carbonate%destruct()
    call this%autotroph_derived_terms%destruct()
    call this%autotroph_local%destruct()
    call this%zooplankton_derived_terms%destruct()
    call this%zooplankton_local%destruct()
    if (ciso_on) then
      call this%zooplankton_share%destruct()
      call this%interior_tendency_share%destruct()
    end if
    call this%domain%destruct()

    call this%timers%shutdown(this%timer_ids, this%timer_summary, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('shutdown_timers', subname)
      return
    end if

  end subroutine shutdown

  !***********************************************************************

  function get_tracer_index(this, tracer_name)

    class(marbl_interface_class), intent(inout) :: this
    character(len=*),             intent(in)    :: tracer_name
    integer :: get_tracer_index

    integer :: n

    get_tracer_index = 0
    do n=1,this%tracer_indices%total_cnt
      if (trim(tracer_name).eq.trim(this%tracer_metadata(n)%short_name) .or.  &
          trim(tracer_name).eq.trim(this%tracer_metadata(n)%long_name)) then
        get_tracer_index = n
        exit
      end if
    end do

  end function get_tracer_index

  !*****************************************************************************

end module marbl_interface
