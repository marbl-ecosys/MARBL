module marbl_interface_types
  ! module for definitions of types that are shared between marbl interior and the driver.

  use marbl_kinds_mod           , only : r8, log_kind, int_kind, char_len
  use marbl_constants_mod       , only : c0, c1
  use marbl_interface_constants , only : marbl_str_length
  use marbl_logging             , only : marbl_log_type
  use marbl_logging             , only : error_msg
  use marbl_logging             , only : status_msg
  use marbl_parms               , only : autotrophs, zooplankton
  use marbl_parms               , only : autotroph_cnt, zooplankton_cnt

  implicit none

  private

  !****************************************************************************

  type, public :: marbl_surface_forcing_indexing_type
     integer(int_kind) :: surface_mask_id     = 0
     integer(int_kind) :: u10_sqr_id          = 0
     integer(int_kind) :: ifrac_id            = 0
     integer(int_kind) :: sst_id              = 0
     integer(int_kind) :: sss_id              = 0
     integer(int_kind) :: atm_pressure_id     = 0
     integer(int_kind) :: xco2_id             = 0
     integer(int_kind) :: xco2_alt_co2_id     = 0
     integer(int_kind) :: xkw_id              = 0
     integer(int_kind) :: dust_flux_id        = 0
     integer(int_kind) :: iron_flux_id        = 0
     integer(int_kind) :: nox_flux_id         = 0
     integer(int_kind) :: nhy_flux_id         = 0
     integer(int_kind) :: din_riv_flux_id     = 0
     integer(int_kind) :: dip_riv_flux_id     = 0
     integer(int_kind) :: don_riv_flux_id     = 0
     integer(int_kind) :: dop_riv_flux_id     = 0
     integer(int_kind) :: dsi_riv_flux_id     = 0
     integer(int_kind) :: dfe_riv_flux_id     = 0
     integer(int_kind) :: dic_riv_flux_id     = 0
     integer(int_kind) :: alk_riv_flux_id     = 0
     integer(int_kind) :: doc_riv_flux_id     = 0
     integer(int_kind) :: d13c_id             = 0
     integer(int_kind) :: d14c_id             = 0
     integer(int_kind) :: d14c_glo_avg_id     = 0
  end type marbl_surface_forcing_indexing_type

  !****************************************************************************

  ! NOTE: when adding a new surface forcing output field (a field that the GCM
  !       may need to pass to flux coupler), remember to add a new index for it
  !       as well.
  type, public :: marbl_surface_forcing_output_indexing_type
    integer(int_kind) :: flux_o2_id = 0
    integer(int_kind) :: flux_co2_id = 0
    integer(int_kind) :: totalChl_id = 0
  end type marbl_surface_forcing_output_indexing_type

  type(marbl_surface_forcing_output_indexing_type), public :: sfo_ind

  !*****************************************************************************

  type, public :: marbl_saved_state_type
     real (r8), allocatable :: ph_prev_col(:)          ! (km)
     real (r8), allocatable :: ph_prev_alt_co2_col(:)  ! (km)
     real (r8), allocatable :: ph_prev_surf(:)         ! (num_elements)
     real (r8), allocatable :: ph_prev_alt_co2_surf(:) ! (num_elements)
   contains
     procedure, public :: construct => marbl_saved_state_constructor
  end type marbl_saved_state_type

  !*****************************************************************************

  type, public :: marbl_domain_type
     integer(int_kind)     :: num_PAR_subcols               ! number of PAR subcols
     integer(int_kind)     :: num_elements_surface_forcing  ! number of surface forcing columns
     integer(int_kind)     :: num_elements_interior_forcing ! number of interior forcing columns
     integer(int_kind)     :: km                            ! number of vertical grid cells
     integer(int_kind)     :: kmt                           ! index of ocean floor
     real(r8), allocatable :: zt(:)                         ! (km) vert dist from sfc to midpoint of layer
     real(r8), allocatable :: zw(:)                         ! (km) vert dist from sfc to bottom of layer
     real(r8), allocatable :: delta_z(:)                    ! (km) delta z - different values for partial bottom cells
     real(r8), allocatable :: dz(:)                         ! (km) delta z - same values for partial bottom cells
   contains
     procedure, public :: construct => marbl_domain_constructor
  end type marbl_domain_type

  !*****************************************************************************

  type, public :: marbl_tracer_metadata_type
     character(char_len) :: short_name
     character(char_len) :: long_name
     character(char_len) :: units
     character(char_len) :: tend_units
     character(char_len) :: flux_units
     logical             :: lfull_depth_tavg
     real(r8)            :: scale_factor
     character(char_len) :: tracer_module_name
  end type marbl_tracer_metadata_type

  !*****************************************************************************

  type, public :: marbl_tracer_read_type
     character(char_len) :: mod_varname
     character(char_len) :: filename
     character(char_len) :: file_varname
     character(char_len) :: file_fmt
     real(r8)            :: scale_factor
     real(r8)            :: default_val
  end type marbl_tracer_read_type

  !*****************************************************************************

  ! FIXME #25: update marbl_interior_forcing_input_type
  type, public :: marbl_interior_forcing_input_type
     real(r8), allocatable :: temperature(:)    ! (km)
     real(r8), allocatable :: salinity(:)       ! (km)
     real(r8), allocatable :: pressure(:)       ! (km)
     real(r8), allocatable :: fesedflux(:)      ! (km)
     real(r8), allocatable :: PAR_col_frac(:)   ! column fraction occupied by each sub-column
     real(r8), allocatable :: surf_shortwave(:) ! surface shortwave for each sub-column (W/m^2)
     real(r8)              :: dust_flux           
   contains
     procedure, public :: construct => marbl_interior_forcing_input_constructor
  end type marbl_interior_forcing_input_type

  !*****************************************************************************

  type, private :: marbl_single_diagnostic_type
     ! marbl_single_diagnostic : 
     ! a private type, this contains both the metadata
     ! and the actual diagnostic data for a single
     ! diagnostic quantity. Data must be accessed via
     ! the marbl_diagnostics_type data structure.
     character (len=char_len)                    :: long_name
     character (len=char_len)                    :: short_name
     character (len=char_len)                    :: units
     character (len=char_len)                    :: vertical_grid ! 'none', 'layer_avg', 'layer_iface'
     logical   (log_kind)                        :: compute_now
     logical   (log_kind)                        :: ltruncated_vertical_extent
     real      (r8), allocatable, dimension(:)   :: field_2d
     real      (r8), allocatable, dimension(:,:) :: field_3d

   contains
     procedure :: initialize  => marbl_single_diag_init
  end type marbl_single_diagnostic_type

  !*****************************************************************************

  type, public :: marbl_diagnostics_type
     ! marbl_diagnostics : 
     ! used to pass diagnostic information from marbl back to
     ! the driver.
     integer :: diag_cnt
     integer :: num_elements
     integer :: num_levels
     type(marbl_single_diagnostic_type), dimension(:), allocatable :: diags

   contains
     procedure, public :: construct      => marbl_diagnostics_constructor
     procedure, public :: set_to_zero    => marbl_diagnostics_set_to_zero
     procedure, public :: add_diagnostic => marbl_diagnostics_add
     procedure, public :: deconstruct    => marbl_diagnostics_deconstructor
  end type marbl_diagnostics_type

  !*****************************************************************************

  type, public :: marbl_single_sfo_type
     ! marbl_single_sfo :
     ! a private type, this contains both the metadata
     ! and the actual data for a single surface forcing
     ! field that needs to be passed to the GCM / flux
     ! coupler. Data must be accesed via the
     ! marbl_surface_forcing_output_type data structure.
     character (len=char_len)            :: long_name
     character (len=char_len)            :: short_name
     character (len=char_len)            :: units
     real(r8), allocatable, dimension(:) :: forcing_field
   contains
     procedure :: construct  => marbl_single_sfo_constructor
  end type marbl_single_sfo_type
  !*****************************************************************************

  type, public :: marbl_surface_forcing_output_type
     integer :: sfo_cnt
     integer :: num_elements
     type(marbl_single_sfo_type), dimension(:), pointer :: sfo => NULL()
   contains
     procedure, public :: add_sfo => marbl_sfo_add
  end type marbl_surface_forcing_output_type

  !*****************************************************************************

  type, public :: marbl_forcing_monthly_every_ts_type
     type      (marbl_tracer_read_type) :: input
     logical   (log_kind)               :: has_data
     character (char_len)               :: interp_type       ! = 'linear'
     character (char_len)               :: data_type         ! = 'monthly-calendar'
     character (char_len)               :: interp_freq       ! = 'every-timestep'
     character (char_len)               :: filename          ! = 'not-used-for-monthly'
     character (char_len)               :: data_label        ! = 'not-used-for-monthly'
     real      (r8), pointer            :: data(:,:,:,:,:)
     real      (r8)                     :: data_time(12)     ! times where DATA is given
     real      (r8)                     :: data_renorm(20)   ! not used for monthly
     real      (r8)                     :: data_inc          ! not used for monthly data
     real      (r8)                     :: data_next         ! time used for the next value of forcing data needed
     real      (r8)                     :: data_update       ! time when new forcing value needs to be added 
     real      (r8)                     :: interp_inc        ! not used for 'every-timestep' interp
     real      (r8)                     :: interp_next       ! not used for 'every-timestep' interp
     real      (r8)                     :: interp_last       ! not used for 'every-timestep' interp
     integer   (int_kind)               :: data_time_min_loc ! index of third dimension of data_time
                                                             ! containing minimum forcing time
  end type marbl_forcing_monthly_every_ts_type

  !*****************************************************************************

  type, private :: marbl_forcing_constant_type
     real(kind=r8) :: field_constant           ! constant value for field_source
   contains
      procedure :: initialize  => marbl_forcing_constant_init
  end type marbl_forcing_constant_type


  !*****************************************************************************

  type, private :: marbl_forcing_driver_type
     character(char_len) :: marbl_driver_varname
   contains
     procedure :: initialize  => marbl_forcing_driver_init
  end type marbl_forcing_driver_type

  !*****************************************************************************

  ! MNL -- made public because I need it for ecosys_restore; could just
  ! keep ecosys_single_restoring_field_type in this module instead
  ! (or just put filename and file_varname in the restoring_field_type)
  type, public :: marbl_forcing_file_type
     character (char_len)      :: filename
     character (char_len)      :: file_varname
     character (char_len)      :: temporal      ! temporarily to support current I/O routines
     integer   (kind=int_kind) :: year_first
     integer   (kind=int_kind) :: year_last
     integer   (kind=int_kind) :: year_align
     integer   (kind=int_kind) :: date
     integer   (kind=int_kind) :: time
   contains
     procedure :: initialize  => marbl_forcing_file_init
  end type marbl_forcing_file_type

  type, public :: marbl_forcing_monthly_calendar_type
     type (marbl_forcing_monthly_every_ts_type), pointer :: marbl_forcing_calendar_name
   contains
     procedure :: initialize  => marbl_forcing_monthly_calendar_init
  end type marbl_forcing_monthly_calendar_type

  !*****************************************************************************

  type, private :: marbl_single_forcing_field_type
     ! single_forcing_field_type (contains the above 4 type definitions)
     character(char_len)                        :: marbl_varname
     character(char_len)                        :: field_units      ! field data units, not the file (driver must do unit conversion)
     character(char_len)                        :: field_source     ! "file", "driver", "POP monthly calendar", "constant", "none"
     character(char_len)                        :: temporal_interp  ! information on interpolation scheme used to populate field data
     real(kind=r8)                              :: unit_conv_factor ! unit conversion factor, incorporates scale_factor
     type (marbl_forcing_constant_type)         :: field_constant_info
     type (marbl_forcing_driver_type)           :: field_driver_info
     type (marbl_forcing_file_type)             :: field_file_info
     type (marbl_forcing_monthly_calendar_type) :: field_monthly_calendar_info
   contains
     procedure :: initialize  => marbl_single_forcing_field_init
  end type marbl_single_forcing_field_type

  !*****************************************************************************

  type, public :: marbl_forcing_fields_type
     integer(kind=int_kind) :: num_elements
     integer(kind=int_kind) :: forcing_field_cnt
     type(marbl_single_forcing_field_type), dimension(:), allocatable :: forcing_fields
   contains
     procedure, public :: construct         => marbl_forcing_fields_constructor
     procedure, public :: add_forcing_field => marbl_forcing_fields_add
     procedure, public :: deconstruct       => marbl_forcing_fields_deconstructor
  end type marbl_forcing_fields_type

  !*****************************************************************************

  type, private :: marbl_living_tracer_index_type
     integer (KIND=int_kind) :: Chl_ind     = 0  ! tracer indices for Chl content
     integer (KIND=int_kind) :: C_ind       = 0  ! tracer indices for C content
     integer (KIND=int_kind) :: Fe_ind      = 0  ! tracer indices for Fe content
     integer (KIND=int_kind) :: Si_ind      = 0  ! tracer indices for Si  content
     integer (KIND=int_kind) :: CaCO3_ind   = 0  ! tracer indices for CaCO3 content
     integer (KIND=int_kind) :: C13_ind     = 0  ! tracer indices for 13C content
     integer (KIND=int_kind) :: C14_ind     = 0  ! tracer indices for 14C content
     integer (KIND=int_kind) :: Ca13CO3_ind = 0  ! tracer indices for 13CaCO3 content
     integer (KIND=int_kind) :: Ca14CO3_ind = 0  ! tracer indices for 14CaCO3 content
  end type marbl_living_tracer_index_type

  !*****************************************************************************

  type, public :: marbl_tracer_index_type
    ! General tracers
    integer (int_kind) :: po4_ind         = 0 ! dissolved inorganic phosphate
    integer (int_kind) :: no3_ind         = 0 ! dissolved inorganic nitrate
    integer (int_kind) :: sio3_ind        = 0 ! dissolved inorganic silicate
    integer (int_kind) :: nh4_ind         = 0 ! dissolved ammonia
    integer (int_kind) :: fe_ind          = 0 ! dissolved inorganic iron
    integer (int_kind) :: o2_ind          = 0 ! dissolved oxygen
    integer (int_kind) :: dic_ind         = 0 ! dissolved inorganic carbon
    integer (int_kind) :: dic_alt_co2_ind = 0 ! dissolved inorganic carbon with alternative CO2
    integer (int_kind) :: alk_ind         = 0 ! alkalinity
    integer (int_kind) :: doc_ind         = 0 ! dissolved organic carbon
    integer (int_kind) :: don_ind         = 0 ! dissolved organic nitrogen
    integer (int_kind) :: dop_ind         = 0 ! dissolved organic phosphorus
    integer (int_kind) :: dopr_ind        = 0 ! refractory DOP
    integer (int_kind) :: donr_ind        = 0 ! refractory DON
    integer (int_kind) :: docr_ind        = 0 ! refractory DOC

    ! CISO tracers
    integer (int_kind) :: di13c_ind       = 0 ! dissolved inorganic carbon 13
    integer (int_kind) :: do13c_ind       = 0 ! dissolved organic carbon 13
    integer (int_kind) :: di14c_ind       = 0 ! dissolved inorganic carbon 14
    integer (int_kind) :: do14c_ind       = 0 ! dissolved organic carbon 14

    ! Living tracers
    type(marbl_living_tracer_index_type), dimension(autotroph_cnt)   :: auto_inds
    type(marbl_living_tracer_index_type), dimension(zooplankton_cnt) :: zoo_inds
    ! For CISO, don't want individual C13 and C14 tracers for each zooplankton
    ! Instead we collect them into one tracer for each isotope, regardless of
    ! zooplankton_cnt
    integer (int_kind) :: zoo13C_ind      = 0 ! zooplankton carbon 13
    integer (int_kind) :: zoo14C_ind      = 0 ! zooplankton carbon 14

  contains
    procedure, public :: construct => marbl_tracer_index_constructor
  end type marbl_tracer_index_type

  !*****************************************************************************

contains

  !*****************************************************************************

  subroutine marbl_domain_constructor(this, &
       num_levels, num_PAR_subcols, &
       num_elements_surface_forcing, num_elements_interior_forcing, &
       dz, zw, zt)

    class(marbl_domain_type), intent(inout) :: this
    integer (int_kind) , intent(in) :: num_levels
    integer (int_kind) , intent(in) :: num_PAR_subcols
    integer (int_kind) , intent(in) :: num_elements_surface_forcing
    integer (int_kind) , intent(in) :: num_elements_interior_forcing
    real (r8)          , intent(in) :: dz(num_levels) 
    real (r8)          , intent(in) :: zw(num_levels) 
    real (r8)          , intent(in) :: zt(num_levels) 

    integer :: k

    ! FIXME #24: remove dz from data type, use delta_z for all vertical depths
    allocate(this%dz(num_levels))
    allocate(this%delta_z(num_levels))
    allocate(this%zw(num_levels))
    allocate(this%zt(num_levels))

    this%km = num_levels
    this%num_PAR_subcols = num_PAR_subcols
    this%num_elements_surface_forcing = num_elements_surface_forcing
    this%num_elements_interior_forcing = num_elements_interior_forcing

    do k = 1, num_levels
       this%delta_z(k) = dz(k)
       this%dz(k)      = dz(k)
       this%zw(k)      = zw(k)
       this%zt(k)      = zt(k)
    end do

  end subroutine marbl_domain_constructor
  
  !*****************************************************************************

  subroutine marbl_saved_state_constructor(this, num_elements, num_levels)

    class(marbl_saved_state_type), intent(inout) :: this
    integer (int_kind) , intent(in) :: num_elements
    integer (int_kind) , intent(in) :: num_levels

    allocate(this%ph_prev_surf         (num_elements))
    allocate(this%ph_prev_alt_co2_surf (num_elements))
    allocate(this%ph_prev_col          (num_levels))
    allocate(this%ph_prev_alt_co2_col  (num_levels))

  end subroutine marbl_saved_state_constructor

  !*****************************************************************************

  subroutine marbl_interior_forcing_input_constructor(this, num_levels, num_PAR_subcols)

    class(marbl_interior_forcing_input_type) , intent(inout) :: this
    integer , intent(in)    :: num_levels
    integer , intent(in)    :: num_PAR_subcols

    allocate(this%temperature      (num_levels))
    allocate(this%salinity         (num_levels))
    allocate(this%pressure         (num_levels))
    allocate(this%fesedflux        (num_levels))
    allocate(this%PAR_col_frac     (num_PAR_subcols))
    allocate(this%surf_shortwave   (num_PAR_subcols))

  end subroutine marbl_interior_forcing_input_constructor

  !*****************************************************************************

  subroutine marbl_single_diag_init(this, lname, sname, units, vgrid,         &
             truncate, num_elements, num_levels)

    class(marbl_single_diagnostic_type) , intent(inout) :: this
    character(len=char_len) , intent(in)    :: lname, sname, units, vgrid
    logical                 , intent(in)    :: truncate
    integer                 , intent(in)    :: num_elements
    integer                 , intent(in)    :: num_levels

    ! Allocate column memory for 3D vars or num_elements memory for 2D vars
    select case (trim(vgrid))
      case ('layer_avg')
        allocate(this%field_3d(num_levels, num_elements))
      case ('layer_iface')
        allocate(this%field_3d(num_levels+1, num_elements))
      case ('none')
        allocate(this%field_2d(num_elements))
      case DEFAULT
        ! FIXME #23: use marbl_log_type to trap this error and then return!
        print*, "ERROR: ", trim(vgrid), " is not a valid vertical grid for MARBL"
    end select

    this%compute_now = .true.
    this%long_name = trim(lname)
    this%short_name = trim(sname)
    this%units = trim(units)
    this%vertical_grid = trim(vgrid)
    this%ltruncated_vertical_extent = truncate

  end subroutine marbl_single_diag_init

  !*****************************************************************************

  subroutine marbl_single_sfo_constructor(this, num_elements, field_name, id, &
                                          marbl_status_log)
    class(marbl_single_sfo_type), intent(inout) :: this
    character(len=*),             intent(in)    :: field_name
    integer(int_kind),            intent(in)    :: num_elements
    integer(int_kind),            intent(in)    :: id
    type(marbl_log_type),         intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname =                                  &
      'marbl_interface_types:marbl_single_sfo_constructor'

    select case (trim(field_name))
      case("flux_o2")
        this%long_name  = "Oxygen Flux"
        this%short_name = "flux_o2"
        this%units      = "nmol/cm^2/s"
        sfo_ind%flux_o2_id = id
      case("flux_co2")
        this%long_name  = "Carbon Dioxide Flux"
        this%short_name = "flux_co2"
        this%units      = "nmol/cm^2/s"
        sfo_ind%flux_co2_id = id
      case("totalChl")
        this%long_name  = "Total Chlorophyll Concentration"
        this%short_name = "totalChl"
        this%units      = "mg/m^3"
        sfo_ind%totalChl_id = id
      case DEFAULT
        write(error_msg, "(2A)") trim(field_name),                            &
                                 " is not a valid surface forcing field name"
        call marbl_status_log%log_error(error_msg, subname)
        return
    end select
    write(status_msg, "(3A)") "Adding ", trim(field_name), " to surface forcing outputs"
    call marbl_status_log%log_noerror(status_msg, subname)

    allocate(this%forcing_field(num_elements))
    this%forcing_field = c0

  end subroutine marbl_single_sfo_constructor

  !*****************************************************************************

  subroutine marbl_sfo_add(this, num_elements, field_name, sfo_id,            &
                           marbl_status_log)

  ! MARBL uses pointers to create an extensible allocatable array. The surface
  ! forcing output fields (part of the intent(out) of this routine) are stored
  ! in this%sfo(:). To allow the size of this%sfo to grow, the process for
  ! adding a new field is:
  !
  ! 1) allocate new_sfo to be size N (one element larger than this%sfo)
  ! 2) copy this%sfo into first N-1 elements of new_sfo
  ! 3) newest surface forcing output (field_name) is Nth element of new_sfo
  ! 4) deallocate / nullify this%sfo
  ! 5) point this%sfo => new_sfo
  !
  ! If the number of possible surface forcing output fields grows, this workflow
  ! may need to be replaced with something that is not O(N^2).

    class(marbl_surface_forcing_output_type), intent(inout) :: this
    character(len=*),     intent(in)    :: field_name
    integer(int_kind),    intent(in)    :: num_elements
    type(marbl_log_type), intent(inout) :: marbl_status_log
    integer(int_kind),    intent(out)   :: sfo_id

    type(marbl_single_sfo_type), dimension(:), pointer :: new_sfo
    integer :: n, old_size
    character(len=*), parameter :: subname = 'marbl_interface_types:marbl_sfo_add'

    if (associated(this%sfo)) then
      old_size = size(this%sfo)
    else
      old_size = 0
    end if
    sfo_id = old_size+1

    ! 1) allocate new_sfo to be size N (one element larger than this%sfo)
    allocate(new_sfo(sfo_id))

    ! 2) copy this%sfo into first N-1 elements of new_sfo
    do n=1,old_size
      new_sfo(n)%long_name  = this%sfo(n)%long_name
      new_sfo(n)%short_name = this%sfo(n)%short_name
      new_sfo(n)%units      = this%sfo(n)%units
      allocate(new_sfo(n)%forcing_field(num_elements))
      new_sfo(n)%forcing_field = this%sfo(n)%forcing_field
      deallocate(this%sfo(n)%forcing_field)
    end do

    ! 3) newest surface forcing output (field_name) is Nth element of new_sfo
    call new_sfo(sfo_id)%construct(num_elements, field_name, sfo_id,          &
                                   marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      error_msg = "error code returned from new_sfo%construct"
      call marbl_status_log%log_error(error_msg, subname)
      return
    end if

    ! 4) deallocate / nullify this%sfo
    if (old_size.gt.0) then
      deallocate(this%sfo)
      nullify(this%sfo)
    end if

    ! 5) point this%sfo => new_sfo
    this%sfo=>new_sfo

  end subroutine marbl_sfo_add

  !*****************************************************************************

  subroutine marbl_diagnostics_constructor(this, num_diags, num_elements, num_levels)

    class(marbl_diagnostics_type), intent(inout) :: this
    integer (int_kind),            intent(in)    :: num_diags
    integer (int_kind),            intent(in)    :: num_elements
    integer (int_kind),            intent(in)    :: num_levels

    allocate(this%diags(num_diags))
    this%diag_cnt = 0
    this%num_elements = num_elements
    this%num_levels = num_levels

  end subroutine marbl_diagnostics_constructor

  !*****************************************************************************

  subroutine marbl_diagnostics_set_to_zero(this)

    class(marbl_diagnostics_type), intent(inout) :: this

    integer (int_kind) :: n

    do n=1,this%diag_cnt
      if (allocated(this%diags(n)%field_2d)) then
        this%diags(n)%field_2d(:) = c0
      elseif (allocated(this%diags(n)%field_3d)) then
        this%diags(n)%field_3d(:, :) = c0
      else
        ! TODO abort abort abort
        write(*,*) "ERROR: neither field_2d nor field_3d are allocated"
        write(*,*) "Diag short name = ", trim(this%diags(n)%short_name)
        write(*,*) "Diag long name = ", trim(this%diags(n)%long_name)
      end if
    end do

  end subroutine marbl_diagnostics_set_to_zero

  !*****************************************************************************

  subroutine marbl_diagnostics_add(this, lname, sname, units, vgrid,          &
             truncate, id)

    class(marbl_diagnostics_type) , intent(inout) :: this
    character(len=char_len)       , intent(in)    :: lname, sname, units, vgrid
    logical (int_kind)            , intent(in)    :: truncate
    integer (int_kind)            , intent(out)   :: id

    this%diag_cnt = this%diag_cnt + 1
    id = this%diag_cnt
    if (id.gt.size(this%diags)) then
      ! FIXME #23: use marbl_log_type to trap this error and then return!
      print*, "ERROR: increase max number of diagnostics!"
    end if
    call this%diags(id)%initialize(lname, sname, units, vgrid, truncate,      &
         this%num_elements, this%num_levels)

  end subroutine marbl_diagnostics_add

  !*****************************************************************************

  subroutine marbl_diagnostics_deconstructor(this)

    class(marbl_diagnostics_type), intent(inout) :: this

    integer :: n

    do n=1,size(this%diags)
      if (allocated(this%diags(n)%field_2d)) then
        deallocate(this%diags(n)%field_2d)
      end if
      if (allocated(this%diags(n)%field_3d)) then
        deallocate(this%diags(n)%field_3d)
      end if
    end do
    deallocate(this%diags)

  end subroutine marbl_diagnostics_deconstructor

  !*****************************************************************************

  subroutine marbl_forcing_constant_init(this, field_constant)

    class(marbl_forcing_constant_type), intent(inout) :: this
    real(kind=r8),                      intent(in)    :: field_constant

    this%field_constant = field_constant

  end subroutine marbl_forcing_constant_init

  !*****************************************************************************

  subroutine marbl_forcing_driver_init(this, marbl_driver_varname)

    class(marbl_forcing_driver_type), intent(inout) :: this
    character(char_len),              intent(in)    :: marbl_driver_varname

    this%marbl_driver_varname = marbl_driver_varname

  end subroutine marbl_forcing_driver_init

  !*****************************************************************************

  subroutine marbl_forcing_file_init(this, filename, file_varname, temporal, year_first, &
                                     year_last, year_align, date, time)


    class(marbl_forcing_file_type),   intent(inout) :: this
    character(char_len),              intent(in)    :: filename
    character(char_len),              intent(in)    :: file_varname
    character(char_len),    optional, intent(in)    :: temporal
    integer(kind=int_kind), optional, intent(in)    :: year_first
    integer(kind=int_kind), optional, intent(in)    :: year_last
    integer(kind=int_kind), optional, intent(in)    :: year_align
    integer(kind=int_kind), optional, intent(in)    :: date
    integer(kind=int_kind), optional, intent(in)    :: time

    this%filename     = filename
    this%file_varname = file_varname
    if (present(temporal  )) this%temporal   = temporal
    if (present(year_first)) this%year_first = year_first
    if (present(year_last )) this%year_last  = year_last
    if (present(year_align)) this%year_align = year_align
    if (present(date      )) this%date       = date
    if (present(time      )) this%time       = time

  end subroutine marbl_forcing_file_init

  !*****************************************************************************

  subroutine marbl_forcing_monthly_calendar_init(this, marbl_forcing_calendar_name)

    class(marbl_forcing_monthly_calendar_type)        , intent(inout) :: this
    type (marbl_forcing_monthly_every_ts_type), target, intent(in)    :: marbl_forcing_calendar_name

    this%marbl_forcing_calendar_name => marbl_forcing_calendar_name

  end subroutine marbl_forcing_monthly_calendar_init

  !*****************************************************************************

  subroutine marbl_single_forcing_field_init(this, &
       num_elements,                               &
       field_source,                               &
       marbl_varname,                              &
       field_units,                                &
       unit_conv_factor, temporal_interp,          &
       field_constant,                             &
       marbl_driver_varname,                       &
       filename,                                   &
       file_varname,                               &
       temporal,                                   &
       year_first, year_last, year_align,          &
       date,                                       &
       time,                                       &
       marbl_forcing_calendar_name)

    class(marbl_single_forcing_field_type), intent(inout) :: this
    integer (kind=int_kind),                intent(in)    :: num_elements
    character (char_len),                   intent(in)    :: field_source  ! must  have valid_field_source value)
    character (char_len),                   intent(in)    :: marbl_varname ! required
    character (char_len),                   intent(in)    :: field_units
    real(kind=r8),           optional,      intent(in)    :: unit_conv_factor
    character (char_len),    optional,      intent(in)    :: temporal_interp
    real(kind=r8),           optional,      intent(in)    :: field_constant
    character (char_len),    optional,      intent(in)    :: marbl_driver_varname
    character (char_len),    optional,      intent(in)    :: filename
    character (char_len),    optional,      intent(in)    :: file_varname
    character (char_len),    optional,      intent(in)    :: temporal
    integer (kind=int_kind), optional,      intent(in)    :: year_first
    integer (kind=int_kind), optional,      intent(in)    :: year_last
    integer (kind=int_kind), optional,      intent(in)    :: year_align
    integer (kind=int_kind), optional,      intent(in)    :: date
    integer (kind=int_kind), optional,      intent(in)    :: time
    type (marbl_forcing_monthly_every_ts_type), optional, target, intent(in) :: marbl_forcing_calendar_name

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=char_len), dimension(6) :: valid_field_sources
    integer (kind=int_kind) :: n
    logical (log_kind)      :: has_valid_source
    logical (log_kind)      :: has_valid_inputs
    !-----------------------------------------------------------------------

    valid_field_sources(1) = "constant"
    valid_field_sources(2) = "driver"
    valid_field_sources(3) = "file"
    valid_field_sources(4) = "marbl"
    valid_field_sources(5) = "POP monthly calendar"
    valid_field_sources(6) = "none"

    ! check for valid source
    has_valid_source = .false.
    do n = 1,size(valid_field_sources)
       if (trim(field_source) .eq. trim(valid_field_sources(n))) has_valid_source = .true.
    enddo
    if (.not. has_valid_source) then
       ! FIXME #23: use marbl_log_type to trap this error and then return!
       write(*,*) "ERROR: ", trim(field_source), "is not a valid field source for MARBL"
    endif

    ! required variables for all forcing field sources
    this%field_source  = trim(field_source)
    this%marbl_varname = marbl_varname
    this%field_units   = field_units

    ! optional variables
    this%unit_conv_factor = c1
    if (present(unit_conv_factor)) this%unit_conv_factor = unit_conv_factor

    this%temporal_interp  = ''
    if (present(temporal_interp )) this%temporal_interp  = temporal_interp


    ! optional variables for forcing field type

    ! each forcing type has its own requirements - if we check here, then the
    ! separate type inits can have fewer optional arguments

    has_valid_inputs = .true.

    select case (trim(field_source))

    case('constant')
       if (.not.present(field_constant)) has_valid_inputs = .false.
       if (has_valid_inputs) then
          write(*,*) "Adding constant forcing_field_type for ", this%marbl_varname 
          call marbl_forcing_constant_init(this%field_constant_info, field_constant)
       else
          ! FIXME #23: use marbl_log_type to trap this error and then return!
          write(*,*) "ERROR: Call to MARBL does not have the correct optional arguments for ", trim(field_source)
       endif

    case('driver')
       if (.not.present(marbl_driver_varname)) has_valid_inputs = .false.
       if (has_valid_inputs) then
          write(*,*) "Adding driver forcing_field_type for ", this%marbl_varname 
          call this%field_driver_info%initialize(marbl_driver_varname)
       else
          ! FIXME #23: use marbl_log_type to trap this error and then return!
          write(*,*) "ERROR: Call to MARBL does not have the correct optional arguments for ", trim(field_source)
       endif

    case('file') 
       if (.not.present(filename))     has_valid_inputs = .false.
       if (.not.present(file_varname)) has_valid_inputs = .false.
       if (has_valid_inputs) then
          write(*,*) "Adding file forcing_field_type for ", this%marbl_varname 
          call this%field_file_info%initialize(&
               filename, file_varname, &
               temporal=temporal, year_first=year_first,   &
               year_last=year_last, year_align=year_align, &
               date=date, time=time)
       else
          ! FIXME #23: use marbl_log_type to trap this error and then return!
          write(*,*) "ERROR: Call to MARBL does not have the correct optional arguments for ", trim(field_source)
       endif

    case('POP monthly calendar') 
       if (.not.present(marbl_forcing_calendar_name)) has_valid_inputs = .false.
       if (has_valid_inputs) then
          write(*,*) "Adding calendar forcing_field_type for ", this%marbl_varname 
          call this%field_monthly_calendar_info%initialize(marbl_forcing_calendar_name)
       else
          ! FIXME #23: use marbl_log_type to trap this error and then return!
          write(*,*) "ERROR: Call to MARBL does not have the correct optional arguments for ", trim(field_source)
       endif

    end select

   end subroutine marbl_single_forcing_field_init

  !*****************************************************************************

  subroutine marbl_forcing_fields_constructor(this, num_elements, num_forcing_fields)

    class(marbl_forcing_fields_type), intent(inout) :: this
    integer (int_kind),               intent(in)    :: num_elements
    integer (int_kind),               intent(in)    :: num_forcing_fields

    allocate(this%forcing_fields(num_forcing_fields))
    this%forcing_field_cnt = 0
    this%num_elements      = num_elements
    !TODO: initialize forcing fields to null?

  end subroutine marbl_forcing_fields_constructor

  !*****************************************************************************

  subroutine marbl_forcing_fields_add(this, &
       field_source,                        &
       marbl_varname,                       &
       field_units,                         &
       unit_conv_factor,                    &
       temporal_interp,                     &
       field_constant,                      &
       marbl_driver_varname,                &
       filename, file_varname,              &
       temporal,                            &
       year_first, year_last, year_align,   &
       date, time,                          &
       marbl_forcing_calendar_name,         &
       id)

    class(marbl_forcing_fields_type) , intent(inout) :: this
    character (char_len)             , intent(in)    :: field_source
    character (char_len)             , intent(in)    :: marbl_varname
    character (char_len)             , intent(in)    :: field_units
    real(kind=r8),           optional, intent(in)    :: unit_conv_factor
    character (char_len),    optional, intent(in)    :: temporal_interp
    real(kind=r8),           optional, intent(in)    :: field_constant
    character (char_len),    optional, intent(in)    :: marbl_driver_varname
    character (char_len),    optional, intent(in)    :: filename
    character (char_len),    optional, intent(in)    :: file_varname
    character (char_len),    optional, intent(in)    :: temporal
    integer (kind=int_kind), optional, intent(in)    :: year_first
    integer (kind=int_kind), optional, intent(in)    :: year_last
    integer (kind=int_kind), optional, intent(in)    :: year_align
    integer (kind=int_kind), optional, intent(in)    :: date
    integer (kind=int_kind), optional, intent(in)    :: time
    integer (kind=int_kind)          , intent(out)   :: id
    type (marbl_forcing_monthly_every_ts_type), optional, target, intent(in) :: marbl_forcing_calendar_name

    integer (kind=int_kind) :: num_elem

    ! Note - the following sets the indices into the marble interface type surface_input_forcings(:,indices)

    this%forcing_field_cnt = this%forcing_field_cnt + 1
    id = this%forcing_field_cnt
    if (id .gt. size(this%forcing_fields)) then
      ! FIXME #23: use marbl_log_type to trap this error and then return!
      print*, "ERROR: increase max number of forcing fields!"
    end if
    num_elem = this%num_elements

    call this%forcing_fields(id)%initialize(             &
         num_elem, field_source, marbl_varname,          &
         field_units, unit_conv_factor=unit_conv_factor, &
         temporal_interp=temporal_interp,                &
         field_constant=field_constant,                  &
         marbl_driver_varname=marbl_driver_varname,      &
         filename=filename, file_varname=file_varname,   &
         temporal=temporal, year_first=year_first,       &
         year_last=year_last, year_align=year_align,     &
         date=date, time=time,                           &
         marbl_forcing_calendar_name=marbl_forcing_calendar_name)

  end subroutine marbl_forcing_fields_add

  !*****************************************************************************

  subroutine marbl_forcing_fields_deconstructor(this)

    class(marbl_forcing_fields_type), intent(inout) :: this

    integer (kind=int_kind) :: n

    deallocate(this%forcing_fields)

  end subroutine marbl_forcing_fields_deconstructor

  !*****************************************************************************

  subroutine marbl_tracer_index_constructor(this, ciso_on, gcm_tracer_cnt,    &
             marbl_status_log)

    ! This subroutine sets the tracer indices for the non-autotroph tracers. To
    ! know where to start the indexing for the autotroph tracers, it increments
    ! tracer_cnt by 1 for each tracer that is included. Note that this gives an
    ! accurate count whether the carbon isotope tracers are included or not.

    use marbl_sizes,   only : ecosys_used_tracer_cnt
    use marbl_sizes,   only : ecosys_ind_beg
    use marbl_sizes,   only : ecosys_ind_end
    use marbl_sizes,   only : ecosys_ciso_ind_beg
    use marbl_sizes,   only : ecosys_ciso_ind_end

    class(marbl_tracer_index_type), intent(inout) :: this
    integer,                        intent(in)    :: gcm_tracer_cnt
    logical,                        intent(in)    :: ciso_on
    type(marbl_log_type),           intent(inout) :: marbl_status_log

    integer :: n
    character(*), parameter :: subname='marbl_parms:marbl_tracer_index_constructor'

    associate(tracer_cnt      => ecosys_used_tracer_cnt)

      tracer_cnt = 0

      ! General ecosys tracers
      ecosys_ind_beg = tracer_cnt + 1

      tracer_cnt  = tracer_cnt + 1
      this%po4_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%no3_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%sio3_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%nh4_ind  = tracer_cnt

      tracer_cnt  = tracer_cnt + 1
      this%fe_ind = tracer_cnt

      tracer_cnt  = tracer_cnt + 1
      this%o2_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%dic_ind = tracer_cnt

      tracer_cnt           = tracer_cnt + 1
      this%dic_alt_co2_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%alk_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%doc_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%don_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%dop_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%dopr_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%donr_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%docr_ind = tracer_cnt

      write (status_msg, "(A)") '----- zooplankton tracer indices -----'
      call marbl_status_log%log_noerror(status_msg, subname)

      do n=1,zooplankton_cnt
        tracer_cnt    = tracer_cnt + 1
        this%zoo_inds(n)%C_ind = tracer_cnt

        write (status_msg, "(3A,I0)") 'C_ind('     , trim(zooplankton(n)%sname), &
              ') = ', this%zoo_inds(n)%C_ind
        call marbl_status_log%log_noerror(status_msg, subname)

      end do

      write (status_msg, "(A)") '----- autotroph tracer indices -----'
      call marbl_status_log%log_noerror(status_msg, subname)

      do n=1,autotroph_cnt
        tracer_cnt    = tracer_cnt + 1
        this%auto_inds(n)%Chl_ind = tracer_cnt

        tracer_cnt    = tracer_cnt + 1
        this%auto_inds(n)%C_ind = tracer_cnt

        tracer_cnt    = tracer_cnt + 1
        this%auto_inds(n)%Fe_ind = tracer_cnt

        if (autotrophs(n)%kSiO3.gt.c0) then
          tracer_cnt    = tracer_cnt + 1
          this%auto_inds(n)%Si_ind = tracer_cnt
        end if

        if (autotrophs(n)%imp_calcifier.or.autotrophs(n)%exp_calcifier) then
          tracer_cnt    = tracer_cnt + 1
          this%auto_inds(n)%CaCO3_ind = tracer_cnt
        end if

        write (status_msg, "(3A,I0)") 'Chl_ind('     , trim(autotrophs(n)%sname), &
              ') = ', this%auto_inds(n)%Chl_ind
        call marbl_status_log%log_noerror(status_msg, subname)

        write (status_msg, "(3A,I0)") 'C_ind('     , trim(autotrophs(n)%sname), &
              ') = ', this%auto_inds(n)%C_ind
        call marbl_status_log%log_noerror(status_msg, subname)

        write (status_msg, "(3A,I0)") 'Fe_ind('     , trim(autotrophs(n)%sname), &
              ') = ', this%auto_inds(n)%Fe_ind
        call marbl_status_log%log_noerror(status_msg, subname)

        write (status_msg, "(3A,I0)") 'Si_ind('     , trim(autotrophs(n)%sname), &
              ') = ', this%auto_inds(n)%Si_ind
        call marbl_status_log%log_noerror(status_msg, subname)

        write (status_msg, "(3A,I0)") 'CaCO3_ind('     , trim(autotrophs(n)%sname), &
              ') = ', this%auto_inds(n)%CaCO3_ind
        call marbl_status_log%log_noerror(status_msg, subname)

      end do
      ecosys_ind_end = tracer_cnt

      if (ciso_on) then
        ! Next tracer is start of the CISO tracers
        ecosys_ciso_ind_beg = tracer_cnt + 1

        tracer_cnt     = tracer_cnt + 1
        this%di13c_ind = tracer_cnt

        tracer_cnt     = tracer_cnt + 1
        this%do13c_ind = tracer_cnt

        tracer_cnt     = tracer_cnt + 1
        this%di14c_ind = tracer_cnt

        tracer_cnt     = tracer_cnt + 1
        this%do14c_ind = tracer_cnt

        tracer_cnt      = tracer_cnt + 1
        this%zoo13C_ind = tracer_cnt

        tracer_cnt      = tracer_cnt + 1
        this%zoo14C_ind = tracer_cnt

        write (status_msg, "(A)") '----- autotroph tracer indices (CISO) -----'
        call marbl_status_log%log_noerror(status_msg, subname)

        do n=1,autotroph_cnt
          tracer_cnt    = tracer_cnt + 1
          this%auto_inds(n)%C13_ind = tracer_cnt

          tracer_cnt    = tracer_cnt + 1
          this%auto_inds(n)%C14_ind = tracer_cnt

          if (autotrophs(n)%imp_calcifier.or.autotrophs(n)%exp_calcifier) then
            tracer_cnt    = tracer_cnt + 1
            this%auto_inds(n)%Ca13CO3_ind = tracer_cnt

            tracer_cnt    = tracer_cnt + 1
            this%auto_inds(n)%Ca14CO3_ind = tracer_cnt
          end if

          write (status_msg, "(3A,I0)") 'C13_ind('     , trim(autotrophs(n)%sname), &
                ') = ', this%auto_inds(n)%C13_ind
          call marbl_status_log%log_noerror(status_msg, subname)
          write (status_msg, "(3A,I0)") 'C14_ind('     , trim(autotrophs(n)%sname), &
                ') = ', this%auto_inds(n)%C14_ind
          call marbl_status_log%log_noerror(status_msg, subname)
          write (status_msg, "(3A,I0)") 'Ca13CO3_ind(' , trim(autotrophs(n)%sname), &
                ') = ', this%auto_inds(n)%Ca13CO3_ind
          call marbl_status_log%log_noerror(status_msg, subname)
          write (status_msg, "(3A,I0)") 'Ca14CO3_ind(' , trim(autotrophs(n)%sname), &
                ') = ', this%auto_inds(n)%Ca14CO3_ind
          call marbl_status_log%log_noerror(status_msg, subname)
        end do

        ecosys_ciso_ind_end = tracer_cnt

      end if

    if (tracer_cnt.ne.gcm_tracer_cnt) then
      write(error_msg,"(A,I0,A,I0)") "MARBL has defined ", tracer_cnt,        &
                            " tracers, but GCM is expecting ", gcm_tracer_cnt
      call marbl_status_log%log_error(error_msg, subname)
      return
    else
      write(status_msg, "(A,I0,A)") "MARBL has defined ", tracer_cnt, " tracers."
      call marbl_status_log%log_noerror(status_msg, subname)
      write(status_msg, "(A, I0,A,I0)") "General tracers: ", ecosys_ind_beg,  &
                                        " to ", ecosys_ind_end
      call marbl_status_log%log_noerror(status_msg, subname)
      write(status_msg, "(A, I0,A,I0)") "CISO tracers: ", ecosys_ciso_ind_beg, &
                                        " to ", ecosys_ciso_ind_end
      call marbl_status_log%log_noerror(status_msg, subname)
    end if
    end associate

  end subroutine marbl_tracer_index_constructor

  !*****************************************************************************

end module marbl_interface_types
