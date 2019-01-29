module marbl_interface_public_types
  ! module for definitions of types that are shared between marbl interior and the driver.

  use marbl_kinds_mod           , only : r8, log_kind, int_kind, char_len
  use marbl_constants_mod       , only : c0
  use marbl_logging             , only : marbl_log_type

  implicit none

  private

  !****************************************************************************

  ! NOTE: when adding a new surface flux output field (a field that the GCM
  !       may need to pass to flux coupler), remember to add a new index for it
  !       as well.
  type, public :: marbl_surface_flux_output_indexing_type
    integer(int_kind) :: flux_o2_id = 0
    integer(int_kind) :: flux_co2_id = 0
    integer(int_kind) :: flux_nhx_id = 0
    integer(int_kind) :: totalChl_id = 0
  end type marbl_surface_flux_output_indexing_type

  type(marbl_surface_flux_output_indexing_type), public :: sfo_ind

  !*****************************************************************************

  type, public :: marbl_single_saved_state_type
    integer                 :: rank
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: vertical_grid ! 'none', 'layer_avg', 'layer_iface'
    real(r8), allocatable, dimension(:)   :: field_2d  ! num_elements
    real(r8), allocatable, dimension(:,:) :: field_3d  ! num_levels, num_elements
  contains
    procedure :: construct => marbl_single_saved_state_construct
  end type marbl_single_saved_state_type

  !*****************************************************************************

  type, public :: marbl_saved_state_type
    integer :: saved_state_cnt
    integer :: num_elements
    integer :: num_levels
    type(marbl_single_saved_state_type), dimension(:), pointer :: state => NULL()
   contains
     procedure, public :: construct => marbl_saved_state_constructor
     procedure, public :: add_state => marbl_saved_state_add
  end type marbl_saved_state_type

  !*****************************************************************************

  type, public :: marbl_domain_type
     integer(int_kind)     :: num_PAR_subcols                ! number of PAR subcols
     integer(int_kind)     :: num_elements_surface_flux      ! number of columns computed in surface_flux_compute
     integer(int_kind)     :: num_elements_interior_tendency ! number of interior forcing columns
     integer(int_kind)     :: km                             ! number of vertical grid cells
     integer(int_kind)     :: kmt                            ! index of ocean floor
     real(r8), allocatable :: zt(:)                          ! (km) vert dist from sfc to midpoint of layer
     real(r8), allocatable :: zw(:)                          ! (km) vert dist from sfc to bottom of layer
     real(r8), allocatable :: delta_z(:)                     ! (km) delta z - different values for partial bottom cells
   contains
     procedure, public :: construct => marbl_domain_constructor
     procedure, public :: destruct => marbl_domain_destructor
  end type marbl_domain_type

  !*****************************************************************************

  type, public :: marbl_tracer_metadata_type
     character(len=char_len) :: short_name
     character(len=char_len) :: long_name
     character(len=char_len) :: units
     character(len=char_len) :: tend_units
     character(len=char_len) :: flux_units
     logical                 :: lfull_depth_tavg
     character(len=char_len) :: tracer_module_name
  end type marbl_tracer_metadata_type

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
     integer   (int_kind)                        :: ref_depth ! depth that diagnostic nominally resides at
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
     integer :: num_elements
     integer :: num_levels
     type(marbl_single_diagnostic_type), dimension(:), pointer :: diags

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
     ! and the actual data for a single field computed
     ! in surface_flux_compute() that needs to be passed
     ! to the GCM / flux coupler. Data must be accessed
     ! via the marbl_surface_flux_output_type data structure.
     character (len=char_len)            :: long_name
     character (len=char_len)            :: short_name
     character (len=char_len)            :: units
     real(r8), allocatable, dimension(:) :: forcing_field
   contains
     procedure :: construct  => marbl_single_sfo_constructor
  end type marbl_single_sfo_type
  !*****************************************************************************

  type, public :: marbl_surface_flux_output_type
     integer :: sfo_cnt
     integer :: num_elements
     type(marbl_single_sfo_type), dimension(:), pointer :: sfo => NULL()
   contains
     procedure, public :: add_sfo => marbl_sfo_add
  end type marbl_surface_flux_output_type

  !*****************************************************************************

  type :: marbl_forcing_fields_metadata_type
     ! Contains variable names and units for required forcing fields as well as
     ! dimensional information; actual forcing data is in array of
     ! marbl_forcing_fields_type
     character(len=char_len) :: varname
     character(len=char_len) :: field_units
     integer                 :: rank            ! 0d or 1d
     integer,  allocatable   :: extent(:)       ! length = rank
  end type marbl_forcing_fields_metadata_type

  !*****************************************************************************

  type, public :: marbl_forcing_fields_type
     type(marbl_forcing_fields_metadata_type) :: metadata
     ! use pointers instead of allocatable because restoring needs to point
     ! into part of the field_1d array
     ! Dimension in name (0d, 1d) refers to per-column dimension
     real(r8), pointer :: field_0d(:)   => NULL()  ! num_elements
     real(r8), pointer :: field_1d(:,:) => NULL()  ! num_elements x extent(1)
   contains
     procedure, public :: set_rank => marbl_forcing_fields_set_rank
  end type marbl_forcing_fields_type

  !*****************************************************************************

  type, public :: marbl_timers_type
    integer :: num_timers
    character(len=char_len), allocatable :: names(:)
    real(r8),                allocatable :: cumulative_runtimes(:)
    logical,                 allocatable :: is_threaded(:)
  contains
    procedure, public :: construct => marbl_timers_constructor
    procedure, public :: deconstruct => marbl_timers_deconstructor
  end type marbl_timers_type

  !*****************************************************************************

  ! FIXME : move to marbl_internal_types.F90 when running means are moved to MARBL

  type, public :: marbl_running_mean_0d_type
     character(len=char_len) :: sname
     real(kind=r8)           :: timescale
     real(kind=r8)           :: rmean

     ! FIXME : perhaps the following get removed from the type when running means are moved to MARBL
     logical(log_kind)       :: linit_by_val
     real(kind=r8)           :: init_val
  end type marbl_running_mean_0d_type

  !*****************************************************************************

contains

  !*****************************************************************************

  subroutine marbl_domain_constructor(this, &
       num_levels, num_PAR_subcols, &
       num_elements_surface_flux, num_elements_interior_tendency, &
       delta_z, zw, zt)

    class(marbl_domain_type), intent(out) :: this
    integer (int_kind),       intent(in)  :: num_levels
    integer (int_kind),       intent(in)  :: num_PAR_subcols
    integer (int_kind),       intent(in)  :: num_elements_surface_flux
    integer (int_kind),       intent(in)  :: num_elements_interior_tendency
    real (r8),                intent(in)  :: delta_z(num_levels)
    real (r8),                intent(in)  :: zw(num_levels)
    real (r8),                intent(in)  :: zt(num_levels)

    integer :: k

    allocate(this%delta_z(num_levels))
    allocate(this%zw(num_levels))
    allocate(this%zt(num_levels))

    this%km = num_levels
    this%num_PAR_subcols = num_PAR_subcols
    this%num_elements_surface_flux = num_elements_surface_flux
    this%num_elements_interior_tendency = num_elements_interior_tendency

    do k = 1, num_levels
       this%delta_z(k) = delta_z(k)
       this%zw(k)      = zw(k)
       this%zt(k)      = zt(k)
    end do

  end subroutine marbl_domain_constructor

  !*****************************************************************************

  subroutine marbl_domain_destructor(this)

    class(marbl_domain_type), intent(inout) :: this

    if (allocated(this%delta_z)) then
      deallocate(this%delta_z)
      deallocate(this%zw)
      deallocate(this%zt)
    end if

  end subroutine marbl_domain_destructor

  !*****************************************************************************

  subroutine marbl_single_saved_state_construct(this, lname, sname, units,    &
             vgrid, rank, num_elements, num_levels, marbl_status_log)

    class(marbl_single_saved_state_type), intent(inout) :: this
    type(marbl_log_type),                 intent(inout) :: marbl_status_log

    character(len=*), intent(in) :: lname
    character(len=*), intent(in) :: sname
    character(len=*), intent(in) :: units
    character(len=*), intent(in) :: vgrid
    integer,          intent(in) :: rank
    integer,          intent(in) :: num_elements
    integer,          intent(in) :: num_levels

    character(len=*), parameter :: subname =                                  &
                  'marbl_interface_public_types:marbl_single_saved_state_construct'
    character(len=char_len)     :: log_message

    select case (rank)
      case (3)
        select case (trim(vgrid))
          case ('layer_avg')
            allocate(this%field_3d(num_levels, num_elements))
          case ('layer_iface')
            allocate(this%field_3d(num_levels+1, num_elements))
          case DEFAULT
            write(log_message,"(2A)") trim(vgrid),                            &
                  " is not a valid vertical grid for 3D saved state"
            call marbl_status_log%log_error(log_message, subname)
        end select
      case (2)
        if (trim(vgrid).eq.'none') then
          allocate(this%field_2d(num_elements))
        else
          write(log_message,"(2A)") trim(vgrid),                              &
                " is not a valid vertical grid for 2D saved state"
            call marbl_status_log%log_error(log_message, subname)
        end if
      case DEFAULT
        write(log_message,"(I0, 2A)") rank, " is not a valid rank for saved", &
              " state"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select

    this%long_name     = trim(lname)
    this%short_name    = trim(sname)
    this%units         = trim(units)
    this%vertical_grid = trim(vgrid)
    this%rank          = rank

  end subroutine marbl_single_saved_state_construct

  !*****************************************************************************

  subroutine marbl_saved_state_constructor(this, num_elements, num_levels)

    class(marbl_saved_state_type), intent(out) :: this
    integer (int_kind),            intent(in)  :: num_elements
    integer (int_kind),            intent(in)  :: num_levels

    this%saved_state_cnt = 0
    this%num_elements    = num_elements
    this%num_levels      = num_levels
    allocate(this%state(this%saved_state_cnt))

  end subroutine marbl_saved_state_constructor

  !*****************************************************************************

  subroutine marbl_saved_state_add(this, lname, sname, units, vgrid, rank,    &
             id, marbl_status_log)

    class(marbl_saved_state_type), intent(inout) :: this
    type(marbl_log_type),          intent(inout) :: marbl_status_log

    character(len=*),  intent(in)  :: lname
    character(len=*),  intent(in)  :: sname
    character(len=*),  intent(in)  :: units
    character(len=*),  intent(in)  :: vgrid
    integer(int_kind), intent(in)  :: rank
    integer(int_kind), intent(out) :: id

    character(len=*), parameter :: subname = 'marbl_interface_public_types:marbl_saved_state_add'
    character(len=char_len)     :: log_message

    type(marbl_single_saved_state_type), dimension(:), pointer :: new_state
    integer :: old_size,n, nlev

    if (.not.associated(this%state)) then
      write(log_message, "(A)") 'Saved state constructor must be run'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if
    old_size = size(this%state)
    id = old_size + 1

    ! 1) allocate new_state to be size N (one element larger than this%state)
    allocate(new_state(id))

    ! 2) copy this%state into first N-1 elements of new_state
    do n=1,old_size
      new_state(n)%rank          = this%state(n)%rank
      new_state(n)%long_name     = this%state(n)%long_name
      new_state(n)%short_name    = this%state(n)%short_name
      new_state(n)%units         = this%state(n)%units
      new_state(n)%vertical_grid = this%state(n)%vertical_grid
      select case (new_state(n)%rank)
        case (2)
          allocate(new_state(n)%field_2d(this%num_elements))
          new_state(n)%field_2d = this%state(n)%field_2d
          deallocate(this%state(n)%field_2d)
        case (3)
          nlev = size(this%state(n)%field_3d, dim=1)
          allocate(new_state(n)%field_3d(nlev, this%num_elements))
          new_state(n)%field_3d = this%state(n)%field_3d
          deallocate(this%state(n)%field_3d)
      end select
    end do

    ! 3) add newest saved state variable
    call new_state(id)%construct(lname, sname, units, vgrid, rank,            &
              this%num_elements, this%num_levels, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('this%state%initialize', subname)
      return
    end if

    ! 4) deallocate / nullify this%state
    deallocate(this%state)
    nullify(this%state)

    ! 5) point this%state => new_state and update saved_state_cnt
    this%state => new_state
    this%saved_state_cnt = id

  end subroutine marbl_saved_state_add

  !*****************************************************************************

  subroutine marbl_single_diag_init(this, lname, sname, units, vgrid,         &
             truncate, num_elements, num_levels, marbl_status_log, ref_depth)

    class(marbl_single_diagnostic_type) , intent(inout) :: this
    character(len=char_len) , intent(in)    :: lname, sname, units, vgrid
    logical                 , intent(in)    :: truncate
    integer                 , intent(in)    :: num_elements
    integer                 , intent(in)    :: num_levels
    type(marbl_log_type)    , intent(inout) :: marbl_status_log
    integer,       optional , intent(in)    :: ref_depth

    character(len=*), parameter :: subname = 'marbl_interface_public_types:marbl_single_diag_init'
    character(len=char_len)     :: log_message

    ! Allocate column memory for 3D vars or num_elements memory for 2D vars
    select case (trim(vgrid))
      case ('layer_avg')
        allocate(this%field_3d(num_levels, num_elements))
      case ('layer_iface')
        allocate(this%field_3d(num_levels+1, num_elements))
      case ('none')
        allocate(this%field_2d(num_elements))
      case DEFAULT
        write(log_message,"(2A)") trim(vgrid),                                  &
                                " is not a valid vertical grid for MARBL"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select

    this%compute_now = .true.
    this%long_name = trim(lname)
    this%short_name = trim(sname)
    this%units = trim(units)
    this%vertical_grid = trim(vgrid)
    this%ltruncated_vertical_extent = truncate

    if (present(ref_depth)) then
      if (.not. allocated(this%field_2d)) then
        write(log_message,"(A)") "ref_depth can only be provided for 2D vars"
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
      this%ref_depth = ref_depth
    else
      this%ref_depth = 0
    end if

  end subroutine marbl_single_diag_init

  !*****************************************************************************

  subroutine marbl_single_sfo_constructor(this, num_elements, field_name, id, &
                                          marbl_status_log)

    class(marbl_single_sfo_type), intent(out)   :: this
    character(len=*),             intent(in)    :: field_name
    integer(int_kind),            intent(in)    :: num_elements
    integer(int_kind),            intent(in)    :: id
    type(marbl_log_type),         intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_interface_public_types:marbl_single_sfo_constructor'
    character(len=char_len)     :: log_message

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
      case("flux_nhx")
        this%long_name  = "NHx Surface Emissions"
        this%short_name = "flux_nhx"
        this%units      = "nmol/cm^2/s"
        sfo_ind%flux_nhx_id = id
      case("totalChl")
        this%long_name  = "Total Chlorophyll Concentration"
        this%short_name = "totalChl"
        this%units      = "mg/m^3"
        sfo_ind%totalChl_id = id
      case DEFAULT
        write(log_message, "(2A)") trim(field_name), " is not a valid surface flux output field name"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select
    write(log_message, "(3A)") "Adding ", trim(field_name), " to surface flux outputs"
    call marbl_status_log%log_noerror(log_message, subname)

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
  ! 3) newest surface flux output (field_name) is Nth element of new_sfo
  ! 4) deallocate / nullify this%sfo
  ! 5) point this%sfo => new_sfo
  !
  ! If the number of possible surface flux output fields grows, this workflow
  ! may need to be replaced with something that is not O(N^2).

    class(marbl_surface_flux_output_type), intent(inout) :: this
    character(len=*),     intent(in)    :: field_name
    integer(int_kind),    intent(in)    :: num_elements
    type(marbl_log_type), intent(inout) :: marbl_status_log
    integer(int_kind),    intent(out)   :: sfo_id

    character(len=*), parameter :: subname = 'marbl_interface_public_types:marbl_sfo_add'

    type(marbl_single_sfo_type), dimension(:), pointer :: new_sfo
    integer :: n, old_size

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

    ! 3) newest surface flux output (field_name) is Nth element of new_sfo
    call new_sfo(sfo_id)%construct(num_elements, field_name, sfo_id,          &
                                   marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('new_sfo%construct()', subname)
      return
    end if

    ! 4) deallocate / nullify this%sfo
    if (old_size .gt. 0) then
      deallocate(this%sfo)
      nullify(this%sfo)
    end if

    ! 5) point this%sfo => new_sfo
    this%sfo=>new_sfo

  end subroutine marbl_sfo_add

  !*****************************************************************************

  subroutine marbl_diagnostics_constructor(this, num_elements, num_levels)

    class(marbl_diagnostics_type), intent(out) :: this
    integer (int_kind),            intent(in)  :: num_elements
    integer (int_kind),            intent(in)  :: num_levels

    allocate(this%diags(0))
    this%num_elements = num_elements
    this%num_levels = num_levels

  end subroutine marbl_diagnostics_constructor

  !*****************************************************************************

  subroutine marbl_diagnostics_set_to_zero(this, marbl_status_log)

    class(marbl_diagnostics_type), intent(inout) :: this
    type(marbl_log_type),          intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_interface_public_types:marbl_diagnostics_set_to_zero'
    character(len=char_len)     :: log_message

    integer (int_kind) :: n

    do n=1,size(this%diags)
      if (allocated(this%diags(n)%field_2d)) then
        this%diags(n)%field_2d(:) = c0
      elseif (allocated(this%diags(n)%field_3d)) then
        this%diags(n)%field_3d(:, :) = c0
      else
        log_message = "neither field_2d nor field_3d are allocated"
        call marbl_status_log%log_error(log_message, subname)
        write(log_message,"(2A)") "Diag short name = ", trim(this%diags(n)%short_name)
        call marbl_status_log%log_error(log_message, subname)
        write(log_message,"(2A)") "Diag long name = ", trim(this%diags(n)%long_name)
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    end do

  end subroutine marbl_diagnostics_set_to_zero

  !*****************************************************************************

  subroutine marbl_diagnostics_add(this, lname, sname, units, vgrid,          &
             truncate, id, marbl_status_log, ref_depth)

    class(marbl_diagnostics_type) , intent(inout) :: this
    character(len=char_len)       , intent(in)    :: lname, sname, units, vgrid
    logical (int_kind)            , intent(in)    :: truncate
    integer (int_kind)            , intent(out)   :: id
    type(marbl_log_type)          , intent(inout) :: marbl_status_log
    integer,             optional , intent(in)    :: ref_depth

    character(len=*), parameter :: subname = 'marbl_interface_public_types:marbl_diagnostics_add'
    character(len=char_len)     :: log_message

    type(marbl_single_diagnostic_type), dimension(:), pointer :: new_diags
    integer :: n, old_size

    if (.not.associated(this%diags)) then
      write(log_message, "(A)") "Diagnostics constructor must be run"
      call marbl_status_log%log_error(log_message, subname)
    end if

    old_size = size(this%diags)
    id = old_size+1

    ! 1) allocate new_diags to be size N (1 larger than this%diags)
    allocate(new_diags(id))

    ! 2) copy this%diags into first N-1 elements of new_diags
    do n=1, old_size
      associate(old_diag => this%diags(n))
        new_diags(n)%long_name                  = old_diag%long_name
        new_diags(n)%short_name                 = old_diag%short_name
        new_diags(n)%units                      = old_diag%units
        new_diags(n)%vertical_grid              = old_diag%vertical_grid
        new_diags(n)%compute_now                = old_diag%compute_now
        new_diags(n)%ltruncated_vertical_extent = old_diag%ltruncated_vertical_extent
        new_diags(n)%ref_depth                  = old_diag%ref_depth
        if (allocated(old_diag%field_2d)) then
          allocate(new_diags(n)%field_2d(this%num_elements))
          new_diags(n)%field_2d = old_diag%field_2d
          deallocate(old_diag%field_2d)
        end if
        if (allocated(old_diag%field_3d)) then
          allocate(new_diags(n)%field_3d(size(old_diag%field_3d, dim=1), this%num_elements))
          new_diags(n)%field_3d = old_diag%field_3d
          deallocate(old_diag%field_3d)
        end if
      end associate
    end do

    ! 3) Add newest diagnostic
    call new_diags(id)%initialize(lname, sname, units, vgrid, truncate,      &
         this%num_elements, this%num_levels, marbl_status_log, ref_depth=ref_depth)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('this%diags%initialize()', subname)
      return
    end if

    ! 4) Deallocate / nullify this%diags
    deallocate(this%diags)
    nullify(this%diags)

    ! 5) Point this%diags => new_diags
    this%diags => new_diags

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

  subroutine marbl_forcing_fields_set_rank(this, num_elements, rank,          &
                                           marbl_status_log, dim1)

    class(marbl_forcing_fields_type), intent(inout) :: this
    integer,                          intent(in)    :: num_elements
    integer,                          intent(in)    :: rank
    type(marbl_log_type),             intent(inout) :: marbl_status_log
    integer, optional,                intent(in)    :: dim1

    character(len=*), parameter :: subname = 'marbl_interface_public_types:marbl_forcing_fields_set_rank'
    character(len=char_len)     :: log_message

    this%metadata%rank = rank
    select case (rank)
      case (0)
        allocate(this%field_0d(num_elements))
      case (1)
        if (.not.present(dim1)) then
          call marbl_status_log%log_error('dim1 is required when rank=1', subname)
          return
        end if
        allocate(this%metadata%extent(1))
        this%metadata%extent(1) = dim1
        allocate(this%field_1d(num_elements, this%metadata%extent(1)))
      case DEFAULT
        write(log_message,"(I0,A)") rank, ' is not a valid rank for a forcing field'
        call marbl_status_log%log_error(log_message, subname)
        return
    end select

  end subroutine marbl_forcing_fields_set_rank

  !*****************************************************************************

  subroutine marbl_timers_constructor(this, num_timers)

    class(marbl_timers_type), intent(out) :: this
    integer,                  intent(in)  :: num_timers

    this%num_timers = num_timers
    allocate(this%names(num_timers))
    allocate(this%is_threaded(num_timers))
    allocate(this%cumulative_runtimes(num_timers))

    if (num_timers .gt. 0) then
      this%names = ''
      this%is_threaded = .false.
      this%cumulative_runtimes = c0
    end if

  end subroutine marbl_timers_constructor

  !*****************************************************************************

  subroutine marbl_timers_deconstructor(this)

    class(marbl_timers_type), intent(inout) :: this

    if (allocated(this%names)) &
      deallocate(this%names)
    if (allocated(this%is_threaded)) &
      deallocate(this%is_threaded)
    if (allocated(this%cumulative_runtimes)) &
      deallocate(this%cumulative_runtimes)

  end subroutine marbl_timers_deconstructor

  !*****************************************************************************

end module marbl_interface_public_types
