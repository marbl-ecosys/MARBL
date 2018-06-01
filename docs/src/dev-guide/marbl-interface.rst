.. _marbl-interface:

===============
MARBL interface
===============

:Release: |release|
:Date: June 6, 2018
:Editors: MARBL team


The MARBL interface class

.. code-block:: fortran

  type, public :: marbl_interface_class

     ! public data - general
     type(marbl_domain_type)                        , public  :: domain
     type(marbl_tracer_metadata_type)  , allocatable, public  :: tracer_metadata(:)
     ! Pointer so that destructor doesn't need to reset all inds to 0
     ! (that happens automatically when new tracer indexing type is allocated)
     type(marbl_tracer_index_type)     , pointer    , public  :: tracer_indices => NULL()
     type(marbl_log_type)                           , public  :: StatusLog

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

     ! FIXME #77: for now, running means are being computed in the driver
     !            they will eventually be moved from the interface to inside MARBL
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
