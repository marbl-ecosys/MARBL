MARBL interface
***************

:Release: |release|
:Date: |today|
:Editors: MARBL team


The MARBL inteface class

.. code-block:: fortran
		
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

     ! public data - interior forcing
     real (r8)                                 , public, allocatable  :: column_tracers(:,:)     ! input  *
     real (r8)                                 , public, allocatable  :: column_dtracers(:,:)    ! output *
     type(marbl_interior_forcing_indexing_type), public               :: interior_forcing_ind         !
     type(marbl_forcing_fields_type)           , public, allocatable  :: interior_input_forcings(:)
     type(marbl_diagnostics_type)              , public               :: interior_forcing_diags  ! output
     type(marbl_diagnostics_type)              , public               :: interior_restore_diags  ! output

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

   contains

     procedure, public  :: config
     procedure, public  :: init
     procedure, public  :: complete_config_and_init
     procedure, private :: glo_vars_init
     procedure, public  :: get_tracer_index
     procedure, public  :: set_interior_forcing
     procedure, public  :: set_surface_forcing
     procedure, public  :: set_global_scalars
     procedure, public  :: shutdown

  end type marbl_interface_class

   
