! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
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

  use marbl_sizes           , only : ecosys_base_tracer_cnt
  use marbl_sizes           , only : marbl_total_tracer_cnt
  use marbl_sizes           , only : autotroph_cnt
  use marbl_sizes           , only : zooplankton_cnt
  use marbl_sizes           , only : num_surface_forcing_fields

  use marbl_interface_types , only : marbl_domain_type
  use marbl_interface_types , only : marbl_tracer_metadata_type
  use marbl_interface_types , only : marbl_tracer_read_type
  use marbl_interface_types , only : marbl_interior_forcing_input_type
  use marbl_interface_types , only : marbl_surface_forcing_output_type
  use marbl_interface_types , only : marbl_diagnostics_type
  use marbl_interface_types , only : marbl_surface_forcing_indexing_type
  use marbl_interface_types , only : marbl_forcing_fields_type
  use marbl_interface_types , only : marbl_saved_state_type

  use marbl_internal_types  , only : marbl_PAR_type
  use marbl_internal_types  , only : marbl_interior_share_type
  use marbl_internal_types  , only : marbl_autotroph_share_type
  use marbl_internal_types  , only : marbl_zooplankton_share_type
  use marbl_internal_types  , only : marbl_particulate_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_internal_type
  use marbl_internal_types  , only : marbl_tracer_index_type

  use marbl_restore_mod     , only : marbl_restore_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  !
  ! The following data structures are part of the public API that the
  ! driver will read/write to interact with marbl.
  !
  !-----------------------------------------------------------------------------
  
  ! note that column_restore is currently nutrients restoring (mmol ./m^3/sec)

  type, public :: marbl_interface_class

     ! public data - general
     type(marbl_domain_type)                   , public               :: domain
     type(marbl_tracer_metadata_type)          , public, allocatable  :: tracer_metadata(:)
     type(marbl_tracer_read_type)              , public, allocatable  :: tracer_read(:)
     type(marbl_tracer_index_type)             , public               :: tracer_indices
     type(marbl_log_type)                      , public               :: StatusLog
     type(marbl_saved_state_type)              , public               :: saved_state             ! input/output

     ! public data - interior forcing
     real (r8)                                 , public, allocatable  :: column_tracers(:,:)     ! input  *
     real (r8)                                 , public, allocatable  :: column_dtracers(:,:)    ! output *
     real (r8)                                 , public, allocatable  :: column_restore(:,:)     ! input  * 
     ! FIXME #25: update marbl_interior_forcing_input_type
     type(marbl_interior_forcing_input_type)   , public               :: interior_forcing_input
     type(marbl_diagnostics_type)              , public               :: interior_forcing_diags  ! output
     type(marbl_diagnostics_type)              , public               :: interior_restore_diags  ! output
     type(marbl_restore_type)                  , public               :: restoring

     ! public data surface forcing
     real (r8)                                 , public, allocatable  :: surface_vals(:,:)           ! input  *
     real (r8)                                 , public, allocatable  :: surface_input_forcings(:,:) ! input  *
     real (r8)                                 , public, allocatable  :: surface_tracer_fluxes(:,:)  ! output *
     type(marbl_surface_forcing_indexing_type) , public               :: surface_forcing_ind         ! 
     type(marbl_forcing_fields_type)           , public               :: surface_forcing_fields
     type(marbl_surface_forcing_output_type)   , public               :: surface_forcing_output      ! output
     type(marbl_diagnostics_type)              , public               :: surface_forcing_diags       ! output


     ! private data 
     logical (log_kind)                        , private              :: ciso_on
     type(marbl_PAR_type)                      , private              :: PAR
     type(marbl_particulate_share_type)        , private              :: particulate_share
     type(marbl_interior_share_type)           , private, allocatable :: interior_share(:)
     type(marbl_zooplankton_share_type)        , private, allocatable :: zooplankton_share(:,:)
     type(marbl_autotroph_share_type)          , private, allocatable :: autotroph_share(:,:)
     type(marbl_surface_forcing_share_type)    , private              :: surface_forcing_share
     type(marbl_surface_forcing_internal_type) , private              :: surface_forcing_internal

   contains

     procedure, public :: init             
     procedure, public :: get_tracer_index
     procedure, public :: set_interior_forcing     
     procedure, public :: set_surface_forcing
     procedure, public :: shutdown         

  end type marbl_interface_class
  
  private :: init
  private :: set_interior_forcing
  private :: set_surface_forcing
  private :: shutdown

  !***********************************************************************

contains

  !***********************************************************************
  
  subroutine init(this,                   &
       gcm_nl_buffer,                     &
       gcm_ciso_on,                       &
       gcm_tracer_cnt,                    &
       gcm_num_levels,                    &
       gcm_num_PAR_subcols,               &
       gcm_num_elements_interior_forcing, &
       gcm_num_elements_surface_forcing,  &
       gcm_dz,                            &
       gcm_zw,                            &
       gcm_zt)

    use marbl_namelist_mod    , only : marbl_nl_cnt
    use marbl_namelist_mod    , only : marbl_nl_buffer_size
    use marbl_ciso_mod        , only : marbl_ciso_init_nml
    use marbl_ciso_mod        , only : marbl_ciso_init_tracer_metadata
    use marbl_mod             , only : marbl_init_nml
    use marbl_mod             , only : marbl_init_surface_forcing_fields
    use marbl_mod             , only : marbl_init_tracer_metadata
    use marbl_mod             , only : marbl_update_tracer_file_metadata
    use marbl_diagnostics_mod , only : marbl_diagnostics_init
    use marbl_parms           , only : autotrophs
    use marbl_parms           , only : zooplankton
    use marbl_share_mod       , only : tracer_init_ext
    use marbl_share_mod       , only : ciso_tracer_init_ext
    
    implicit none

    class     (marbl_interface_class)      , intent(inout) :: this
    logical   (log_kind)                   , intent(in)    :: gcm_ciso_on
    character (marbl_nl_buffer_size)       , intent(in)    :: gcm_nl_buffer(marbl_nl_cnt)
    integer   (int_kind)                   , intent(in)    :: gcm_tracer_cnt
    integer   (int_kind)                   , intent(in)    :: gcm_num_levels
    integer   (int_kind)                   , intent(in)    :: gcm_num_PAR_subcols
    integer   (int_kind)                   , intent(in)    :: gcm_num_elements_surface_forcing
    integer   (int_kind)                   , intent(in)    :: gcm_num_elements_interior_forcing
    real      (r8)                         , intent(in)    :: gcm_dz(gcm_num_levels) ! thickness of layer k
    real      (r8)                         , intent(in)    :: gcm_zw(gcm_num_levels) ! thickness of layer k
    real      (r8)                         , intent(in)    :: gcm_zt(gcm_num_levels) ! thickness of layer k

    character(*), parameter :: subname = 'marbl_interface:marbl_init'
    integer :: i
    !--------------------------------------------------------------------

    associate(&
         num_levels           => gcm_num_levels,                               &
         num_PAR_subcols      => gcm_num_PAR_subcols,                          &
         num_surface_elements => gcm_num_elements_surface_forcing,             &
         num_interior_forcing => gcm_num_elements_interior_forcing,            &
         ecosys_base_ind_beg  => this%tracer_indices%ecosys_base_ind_beg,      &
         ecosys_base_ind_end  => this%tracer_indices%ecosys_base_ind_end       &
         )

    !--------------------------------------------------------------------
    ! initialize ciso_on and status log
    !--------------------------------------------------------------------

    this%ciso_on = gcm_ciso_on
    call this%StatusLog%construct()

    !--------------------------------------------------------------------
    ! initialize marbl namelists
    !--------------------------------------------------------------------

    call marbl_init_nml(gcm_nl_buffer, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_nml()", subname)
      return
    end if

    if (gcm_ciso_on) then
       call marbl_ciso_init_nml(gcm_nl_buffer, this%StatusLog)
       if (this%StatusLog%labort_marbl) then
          call this%StatusLog%log_error_trace("marbl_ciso_init_nml()", subname)
          return
       end if
    end if

    !--------------------------------------------------------------------
    ! call constructors and allocate memory
    !--------------------------------------------------------------------

    call this%tracer_indices%construct(gcm_ciso_on, autotrophs, zooplankton,  &
         gcm_tracer_cnt, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("tracer_indices%construct", subname)
      return
    end if

    call this%PAR%construct(num_levels, num_PAR_subcols)

    call this%particulate_share%construct(num_levels)

    call this%surface_forcing_share%construct(num_surface_elements)

    call this%surface_forcing_internal%construct(num_surface_elements)

    allocate (this%interior_share(num_levels))

    allocate (this%zooplankton_share(zooplankton_cnt, num_levels))

    allocate (this%autotroph_share(autotroph_cnt, num_levels))

    call this%domain%construct(                                &
         num_levels                    = num_levels,           &
         num_PAR_subcols               = num_PAR_subcols,      &
         num_elements_surface_forcing  = num_surface_elements,  &
         num_elements_interior_forcing = num_interior_forcing, &
         dz                            = gcm_dz,               &
         zw                            = gcm_zw,               &
         zt                            = gcm_zt)

         
    call this%interior_forcing_input%construct(num_levels, num_PAR_subcols)

    call this%saved_state%construct(num_surface_elements, num_levels)

    allocate(this%surface_vals(num_surface_elements, marbl_total_tracer_cnt))

    allocate(this%surface_tracer_fluxes(num_surface_elements, marbl_total_tracer_cnt))

    allocate(this%column_tracers(marbl_total_tracer_cnt, num_levels))

    allocate(this%column_dtracers(marbl_total_tracer_cnt, num_levels))

    allocate(this%column_restore(marbl_total_tracer_cnt, gcm_num_levels))

    !--------------------------------------------------------------------
    ! Initialize public data / general tracer metadata
    ! And then update tracer input info based on namelist
    !--------------------------------------------------------------------

    allocate(this%tracer_metadata(marbl_total_tracer_cnt))
    allocate(this%tracer_read(marbl_total_tracer_cnt))

    call marbl_init_tracer_metadata( &
         this%tracer_metadata,       &
         this%tracer_read,           &
         this%tracer_indices,        &
         this%StatusLog)

    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_tracer_metadata()", subname)
      return
    end if

    if (this%ciso_on) then
       call marbl_ciso_init_tracer_metadata(this%tracer_metadata,          &
            this%tracer_read, this%tracer_indices)
    end if

    call marbl_update_tracer_file_metadata(this%tracer_indices, this%tracer_read, &
         tracer_init_ext, this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_update_tracer_file_metadata()", subname)
      return
    end if
    if (this%ciso_on) then
      call marbl_update_tracer_file_metadata(this%tracer_indices, this%tracer_read, &
           ciso_tracer_init_ext, this%StatusLog)
      if (this%StatusLog%labort_marbl) then
        call this%StatusLog%log_error_trace("marbl_update_tracer_file_metadata() [ciso]", subname)
        return
      end if
    end if

    !--------------------------------------------------------------------
    ! initialize marbl surface forcing fields
    !--------------------------------------------------------------------

    call marbl_init_surface_forcing_fields(                         &
         ciso_on                      = this%ciso_on,               &
         num_elements                 = num_surface_elements,       &
         num_surface_forcing_fields   = num_surface_forcing_fields, &  
         surface_forcing_indices      = this%surface_forcing_ind,   &
         surface_forcing_fields       = this%surface_forcing_fields,          &
         marbl_status_log             = this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("marbl_init_surface_forcing_fields()", subname)
      return
    end if

    allocate(this%surface_input_forcings(num_surface_elements, num_surface_forcing_fields))

    !--------------------------------------------------------------------
    ! Initialize tracer restoring
    !--------------------------------------------------------------------

    call this%restoring%init(                                                   &
         nl_buffer       = gcm_nl_buffer,                                       &
         domain          = this%domain,                                         &
         tracer_metadata = this%tracer_metadata(ecosys_base_ind_beg:ecosys_base_ind_end), &
         status_log      = this%StatusLog)
    if (this%StatusLog%labort_marbl) then
      call this%StatusLog%log_error_trace("this%restoring%init()", subname)
      return
    end if

    !--------------------------------------------------------------------
    ! Initialize marbl diagnostics
    !--------------------------------------------------------------------

    call marbl_diagnostics_init(                                              &
         ciso_on                      = this%ciso_on,                         &
         marbl_domain                 = this%domain,                          &
         marbl_tracer_metadata        = this%tracer_metadata,                 &
         marbl_tracer_indices         = this%tracer_indices,                  &
         marbl_interior_forcing_diags = this%interior_forcing_diags,          &
         marbl_interior_restore_diags = this%interior_restore_diags,          &
         marbl_surface_forcing_diags  = this%surface_forcing_diags,           &
         marbl_status_log             = this%StatusLog)

    end associate

  end subroutine init

  !***********************************************************************
  
  subroutine set_interior_forcing(this)

    use marbl_mod, only : marbl_set_interior_forcing
    
    implicit none

    class(marbl_interface_class), intent(inout) :: this
    character(*), parameter :: subname = 'marbl_interface:set_interior_forcing'

    call this%restoring%restore_tracers( &
         this%column_tracers,            &
         this%domain%km,                 &
         marbl_total_tracer_cnt,         &
         this%column_restore)

    call marbl_set_interior_forcing(   &
         ciso_on                 = this%ciso_on,                 &
         domain                  = this%domain,                  &
         interior_forcing_input  = this%interior_forcing_input,  &
         saved_state             = this%saved_state,             &
         interior_restore        = this%column_restore,          &
         tracers                 = this%column_tracers,          &
         dtracers                = this%column_dtracers,         &
         marbl_tracer_indices    = this%tracer_indices,          &
         marbl_PAR               = this%PAR,                     &
         marbl_interior_share    = this%interior_share,          &
         marbl_zooplankton_share = this%zooplankton_share,       &
         marbl_autotroph_share   = this%autotroph_share,         &
         marbl_particulate_share = this%particulate_share,       &
         interior_forcing_diags  = this%interior_forcing_diags,  &
         interior_restore_diags  = this%interior_restore_diags,  &
         marbl_status_log        = this%StatusLog)

    if (this%StatusLog%labort_marbl) then
       call this%StatusLog%log_error_trace("marbl_set_interior_forcing()", subname)
       return
    end if
    
  end subroutine set_interior_forcing
  
  !***********************************************************************
  
  subroutine set_surface_forcing(this)

    use marbl_mod      , only : marbl_set_surface_forcing
    
    implicit none

    class(marbl_interface_class), intent(inout) :: this

    call marbl_set_surface_forcing(                                           &
         ciso_on                  = this%ciso_on,                             &
         num_elements             = this%domain%num_elements_surface_forcing, &
         surface_forcing_ind      = this%surface_forcing_ind,                 &
         surface_input_forcings   = this%surface_input_forcings,              &
         surface_vals             = this%surface_vals,                        &
         surface_tracer_fluxes    = this%surface_tracer_fluxes,               &
         marbl_tracer_indices     = this%tracer_indices,                      &
         saved_state              = this%saved_state,                         &
         surface_forcing_output   = this%surface_forcing_output,              &
         surface_forcing_internal = this%surface_forcing_internal,            &
         surface_forcing_share    = this%surface_forcing_share,               &
         surface_forcing_diags    = this%surface_forcing_diags,               &
         marbl_status_log         = this%statuslog)

  end subroutine set_surface_forcing
  
  !***********************************************************************
  
  subroutine shutdown(this)

    implicit none

    class(marbl_interface_class), intent(inout) :: this

    ! free dynamically allocated memory, etc
    
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
