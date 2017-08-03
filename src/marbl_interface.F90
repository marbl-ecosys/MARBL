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

  use marbl_sizes           , only : autotroph_cnt
  use marbl_sizes           , only : zooplankton_cnt

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
  use marbl_internal_types  , only : marbl_particulate_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_internal_type
  use marbl_internal_types  , only : marbl_tracer_index_type
  use marbl_internal_types  , only : marbl_internal_timers_type
  use marbl_internal_types  , only : marbl_timer_indexing_type

  use marbl_config_mod, only : marbl_settings_type

  use ieee_arithmetic, only : ieee_is_nan

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
     type(marbl_domain_type)           , public               :: domain
     type(marbl_tracer_metadata_type)  , public, allocatable  :: tracer_metadata(:)
     type(marbl_tracer_index_type)     , public               :: tracer_indices
     type(marbl_log_type)              , public               :: StatusLog

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
     type(marbl_surface_forcing_share_type)    , private              :: surface_forcing_share
     type(marbl_surface_forcing_internal_type) , private              :: surface_forcing_internal
     logical                                   , private              :: lallow_glo_ops
     type(marbl_internal_timers_type)          , private              :: timers
     type(marbl_timer_indexing_type)           , private              :: timer_ids
     type(marbl_settings_type)                 , private              :: settings

   contains

     procedure, public  :: init
     procedure, public  :: reset_timers
     procedure, public  :: extract_timing
     procedure, private :: glo_vars_init
     procedure, public  :: get_tracer_index
     procedure, public  :: set_interior_forcing
     procedure, public  :: set_surface_forcing
     procedure, public  :: set_global_scalars
     procedure, public  :: shutdown
     procedure, public  :: inquire_settings_metadata
     generic            :: put_setting => put_real,    &
                                          put_integer, &
                                          put_logical, &
                                          put_string,  &
                                          put_all_string
     generic            :: get_setting => get_real,    &
                                          get_integer, &
                                          get_logical, &
                                          get_string
     procedure, public  :: get_settings_var_cnt
     procedure, public  :: parse_inputfile_line
     procedure, private :: put_real
     procedure, private :: put_integer
     procedure, private :: put_logical
     procedure, private :: put_string
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
  private :: set_interior_forcing
  private :: set_surface_forcing
  private :: shutdown

  !***********************************************************************

contains

  !***********************************************************************

  subroutine init(this,                   &
       gcm_num_levels,                    &
       gcm_num_PAR_subcols,               &
       gcm_num_elements_surface_forcing,  &
       gcm_delta_z,                       &
       gcm_zw,                            &
       gcm_zt,                            &
       lgcm_has_global_ops,               &
       marbl_tracer_cnt)

    use marbl_init_mod, only : marbl_init_log_and_timers
    use marbl_init_mod, only : marbl_init_config_vars
    use marbl_init_mod, only : marbl_init_tracers
    use marbl_init_mod, only : marbl_init_parameters
    use marbl_init_mod, only : marbl_init_bury_coeff
    use marbl_init_mod, only : marbl_init_forcing_fields


    use marbl_diagnostics_mod, only : marbl_diagnostics_init

    use marbl_saved_state_mod, only : marbl_saved_state_init

    class(marbl_interface_class), intent(inout) :: this
    integer(int_kind),            intent(in)    :: gcm_num_levels
    integer(int_kind),            intent(in)    :: gcm_num_PAR_subcols
    integer(int_kind),            intent(in)    :: gcm_num_elements_surface_forcing
    real(r8),                     intent(in)    :: gcm_delta_z(gcm_num_levels) ! thickness of layer k
    real(r8),                     intent(in)    :: gcm_zw(gcm_num_levels) ! thickness of layer k
    real(r8),                     intent(in)    :: gcm_zt(gcm_num_levels) ! thickness of layer k
    logical,           optional,  intent(in)    :: lgcm_has_global_ops
    integer(int_kind), optional,  intent(out)   :: marbl_tracer_cnt

    character(len=*), parameter :: subname = 'marbl_interface:init'
    integer, parameter :: num_interior_elements = 1 ! FIXME #66: get this value from interface, let it vary

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
    ! Initialize configuration variables in settings
    !---------------------------------------------------------------------------

    call marbl_init_config_vars(this%lallow_glo_ops, this%settings, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_config_vars", subname)
      return
    end if

    associate(&
         num_levels            => gcm_num_levels,                              &
         num_PAR_subcols       => gcm_num_PAR_subcols,                         &
         num_surface_elements  => gcm_num_elements_surface_forcing             &
         )

    !-----------------------------------------------------------------------
    !  Set up domain type
    !-----------------------------------------------------------------------

    call this%domain%construct(                                 &
         num_levels                    = num_levels,            &
         num_PAR_subcols               = num_PAR_subcols,       &
         num_elements_surface_forcing  = num_surface_elements,  &
         num_elements_interior_forcing = num_interior_elements, &
         delta_z                       = gcm_delta_z,           &
         zw                            = gcm_zw,                &
         zt                            = gcm_zt)

    !--------------------------------------------------------------------
    ! call constructors and allocate memory
    !--------------------------------------------------------------------

    call this%PAR%construct(num_levels, num_PAR_subcols)

    call this%particulate_share%construct(num_levels)

    !-----------------------------------------------------------------------
    !  Set up tracers
    !-----------------------------------------------------------------------

    call marbl_init_tracers(num_levels, num_surface_elements, &
                            this%tracer_indices, this%surface_vals, this%surface_tracer_fluxes, &
                            this%column_tracers, this%column_dtracers, this%tracer_metadata,    &
                            this%StatusLog, marbl_tracer_cnt)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_tracers", subname)
      return
    end if

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
    ! Initialize parameters in settings
    !---------------------------------------------------------------------------
    call marbl_init_parameters(num_levels, this%settings, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_parameters", subname)
      return
    end if

    end associate

    !-----------------------------------------------------------------------
    !  Initialize bury coefficient
    !-----------------------------------------------------------------------

    call marbl_init_bury_coeff(this%particulate_share, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('marbl_init_bury_coeff', subname)
      return
    end if

    !-----------------------------------------------------------------------
    !  Initialize surface and interior forcing (including tracer restoring)
    !-----------------------------------------------------------------------

    call marbl_init_forcing_fields(this%domain, &
                                   this%tracer_metadata, &
                                   this%surface_forcing_ind, &
                                   this%surface_forcing_share, &
                                   this%surface_forcing_internal, &
                                   this%surface_input_forcings, &
                                   this%interior_forcing_ind, &
                                   this%interior_input_forcings, &
                                   this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_forcing_fields", subname)
      return
    end if

    ! Set up running mean variables (dependent on parms namelist)
    call this%glo_vars_init()

    !  Lock and log configuration variables and parameters
    call this%settings%finalize_vars(this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('parmeters%finalize_vars', subname)
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
    character(len=char_len) :: log_message

    call this%StatusLog%construct()
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
    character(len=char_len) :: log_message

    call this%StatusLog%construct()
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
    character(len=char_len) :: log_message

    call this%StatusLog%construct()
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
    character(len=char_len) :: log_message

    call this%StatusLog%construct()
    call this%settings%put(varname, this%StatusLog, sval=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%put()', subname)
      return
    end if

  end subroutine put_string

  !***********************************************************************

  subroutine put_all_string(this, varname, datatype, val)
    ! This interface to put_setting() complements parse_inputfile_line()

    class (marbl_interface_class),  intent(inout) :: this
    character(len=*), dimension(:), intent(in)    :: varname, datatype, val

    character(len=*), parameter :: subname = 'marbl_interface:put_all_string'
    character(len=char_len)     :: log_message
    real(r8)          :: rval
    integer(int_kind) :: ival
    logical(log_kind) :: lval
    integer(int_kind) :: n, ioerr

    call this%StatusLog%construct()
    do n=1,size(datatype)
      if (len_trim(datatype(n)) .eq. 0) exit
      select case (trim(datatype(n)))
        case ('real')
          read(val(n), *, iostat=ioerr) rval
          if ((ioerr.eq.0) .and. (.not.ieee_is_nan(rval))) then
            call this%put_setting(trim(varname(n)), rval)
          else
            write(log_message, "(2A)") trim(val(n)), ' is not a valid real value'
            call this%StatusLog%log_error(log_message, subname)
          end if
        case ('integer')
          read(val(n), *, iostat=ioerr) ival
          if (ioerr.eq.0) then
            call this%put_setting(trim(varname(n)), ival)
          else
            write(log_message, "(2A)") trim(val(n)), ' is not a valid integer value'
            call this%StatusLog%log_error(log_message, subname)
          end if
        case ('string')
          call this%put_setting(trim(varname(n)), trim(val(n)))
        case ('logical')
          select case (trim(val(n)))
            case ('.true.')
              call this%put_setting(trim(varname(n)), .true.)
            case ('.false.')
              call this%put_setting(trim(varname(n)), .false.)
            case DEFAULT
              write(log_message, "(2A)") trim(val(n)), ' is not a valid logical value'
              ! Put something in case we need to construct StatusLog
              call this%put_setting('MARBL ABORT', .true.)
              call this%StatusLog%log_error(log_message, subname)
          end select
        case DEFAULT
          write(log_message,"(2A)") trim(datatype(n)), " is not a recognized type"
          ! Put something in case we need to construct StatusLog
          call this%put_setting('MARBL ABORT', .true.)
          call this%StatusLog%log_error(log_message, subname)
      end select
    end do
  end subroutine put_all_string

  !***********************************************************************

  subroutine get_real(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    real(r8),                      intent(out)   :: val

    character(len=*), parameter :: subname = 'marbl_interface:get_real'
    character(len=char_len) :: log_message

    call this%StatusLog%construct()
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
    character(len=char_len) :: log_message

    call this%StatusLog%construct()
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
    character(len=char_len) :: log_message

    call this%StatusLog%construct()
    call this%settings%get(varname, this%StatusLog, lval=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%get()', subname)
      return
    end if

  end subroutine get_logical

  !***********************************************************************

  subroutine get_string(this, varname, val)

    class (marbl_interface_class), intent(inout) :: this
    character(len=*),              intent(in)    :: varname
    character(len=*),              intent(out)   :: val

    character(len=*), parameter :: subname = 'marbl_interface:get_string'
    character(len=char_len) :: log_message

    call this%StatusLog%construct()
    call this%settings%get(varname, this%StatusLog, sval=val)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%get()', subname)
      return
    end if

  end subroutine get_string

  !***********************************************************************

  function get_settings_var_cnt(this) result(cnt)

    class (marbl_interface_class), intent(in) :: this
    integer :: cnt

    cnt = this%settings%get_cnt()

  end function get_settings_var_cnt

  !***********************************************************************

  subroutine inquire_settings_metadata(this, id, sname, lname, units, group, datatype)

    class (marbl_interface_class), intent(inout) :: this
    integer(int_kind),          intent(in)    :: id
    character(len=*), optional, intent(out)   :: sname, lname, units
    character(len=*), optional, intent(out)   :: group, datatype

    character(len=*), parameter :: subname = 'marbl_interface:inquire_settings_metadata'

    call this%settings%inquire_metadata(id, this%StatusLog, sname, lname, units, &
                                        group, datatype)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace('settings%inquire_metadata', subname)
      return
    end if

  end subroutine inquire_settings_metadata
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

    character(len=*), parameter :: subname = 'marbl_interface:set_interior_forcing'

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

    character(len=*), parameter :: subname = 'marbl_interface:set_surface_forcing'

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
    character(len=*),             intent(in)    :: field_source ! 'interior' or 'surface'

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

    character(len=*), parameter :: subname = 'marbl_interface:shutdown'

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

  !***********************************************************************

  subroutine parse_inputfile_line(this, line, varname, datatype, value)

    ! This subroutine takes a single line from MARBL's default inputfile format
    ! and returns the variable name, type of variable, and the value (all as
    ! strings).
    !
    ! This is useful because the standard fortran parser treats commas as
    ! delimiters, but some variable names have commas in them because they are
    ! 2D arrays.

    class(marbl_interface_class), intent(in)  :: this
    character(len=*),             intent(in)  :: line
    character(len=*),             intent(out) :: varname, datatype, value

    character(len=char_len) :: line_loc
    integer(int_kind) :: char_ind

    ! Variables used to determine datatype
    integer(int_kind) :: ioerr
    real(r8)          :: rval
    integer(int_kind) :: ival

    varname = ''
    datatype = ''
    value = ''
    line_loc = adjustl(line)

    ! Strip out comments (denoted by '!')
    do char_ind = 1, len_trim(line_loc)
      if (line_loc(char_ind:char_ind) .eq. '!') exit
    end do
    if (char_ind .le. len_trim(line_loc)) &
      line_loc(char_ind:len_trim(line_loc)) = ' '

    ! Return without processing if
    ! (a) line is empty / only contains spaces
    if (len_trim(line_loc) .eq. 0) return
    ! (b) first non-space character is '&' (can use namelist files)
    if (line_loc(1:1) .eq. '&') return
    ! (c) line contains only '/' (can use namelist files)
    if (trim(line_loc) .eq. '/') return

    ! Everything up to first '=' is varname
    do char_ind = 1, len_trim(line_loc)
      if (line_loc(char_ind:char_ind) .eq. '=') then
        line_loc(char_ind:char_ind) = ' '
        exit
      end if
      varname(char_ind:char_ind) = line_loc(char_ind:char_ind)
      line_loc(char_ind:char_ind) = ' '
    end do
    ! Everything else is value
    value = adjustl(line_loc)

    ! ------------------
    ! Determine datatype
    ! ------------------

    ! (1) check for logical
    if ((trim(value).eq.'.true.').or.(trim(value).eq.'.false.')) &
      datatype = "logical"

    ! (2) check for integer
    if (len_trim(datatype).eq.0) then
      read(value, *, iostat=ioerr) ival
      if (ioerr.eq.0) datatype="integer"
    end if

    ! (3) check for real
    if (len_trim(datatype).eq.0) then
      read(value, *, iostat=ioerr) rval
      if ((ioerr.eq.0) .and. (.not.ieee_is_nan(rval))) datatype="real"
    end if

    ! (4) everything else is string
    if (len_trim(datatype).eq.0) then
      datatype="string"
      ! Strip leading / trailing quotes from string values
      char_ind = 1
      if ((value(char_ind:char_ind) .eq. "'") .or. (value(char_ind:char_ind) .eq. '"')) &
        value(char_ind:char_ind) = ' '
      char_ind = len_trim(value)
      if ((value(char_ind:char_ind) .eq. "'") .or. (value(char_ind:char_ind) .eq. '"')) &
        value(char_ind:char_ind) = ' '
      value = adjustl(value)
    end if

    ! TODO: error checking

  end subroutine parse_inputfile_line

  !*****************************************************************************

end module marbl_interface
