module marbl_interface_types
  ! module for definitions of types that are shared between marbl interior and the driver.

  use marbl_kinds_mod           , only : r8, log_kind, int_kind, char_len
  use marbl_constants_mod       , only : c0, c1
  use marbl_interface_constants , only : marbl_str_length
  use marbl_logging             , only : marbl_log_type

  implicit none

  private

  !****************************************************************************

  ! NOTE: when adding a new surface forcing output field (a field that the GCM
  !       may need to pass to flux coupler), remember to add a new index for it
  !       as well.
  type, public :: marbl_surface_forcing_output_indexing_type
    integer(int_kind) :: flux_o2_id = 0
    integer(int_kind) :: flux_co2_id = 0
    integer(int_kind) :: flux_nhx_id = 0
    integer(int_kind) :: totalChl_id = 0
  end type marbl_surface_forcing_output_indexing_type

  type(marbl_surface_forcing_output_indexing_type), public :: sfo_ind

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
!     real (r8), allocatable :: ph_prev_col(:)          ! (km)
!     real (r8), allocatable :: ph_prev_alt_co2_col(:)  ! (km)
!     real (r8), allocatable :: ph_prev_surf(:)         ! (num_elements)
!     real (r8), allocatable :: ph_prev_alt_co2_surf(:) ! (num_elements)
   contains
     procedure, public :: construct => marbl_saved_state_constructor
     procedure, public :: add_state => marbl_saved_state_add
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
     character(char_len) :: tracer_module_name
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

  type :: marbl_forcing_fields_metadata_type
     ! Contains variable names and units for required forcing fields as well as
     ! dimensional information; actual forcing data is in array of
     ! marbl_forcing_fields_type
     character(char_len)   :: varname
     character(char_len)   :: field_units
     integer               :: rank            ! 0d or 1d
     integer,  allocatable :: extent(:)       ! length = rank
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
    character(char_len), allocatable :: names(:)
    real(r8), allocatable :: cumulative_runtimes(:)
  end type marbl_timers_type

  !*****************************************************************************

  ! FIXME : move to marbl_internal_types.F90 when running means are moved to MARBL

  type, public :: marbl_running_mean_0d_type
     character(char_len) :: sname
     real(kind=r8)       :: timescale
     real(kind=r8)       :: rmean

     ! FIXME : perhaps the following get removed from the type when running means are moved to MARBL
     logical(log_kind)   :: linit_by_val
     real(kind=r8)       :: init_val
  end type marbl_running_mean_0d_type

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

  subroutine marbl_single_saved_state_construct(this, lname, sname, units,    &
             vgrid, rank, num_elements, num_levels, marbl_status_log)

    class(marbl_single_saved_state_type), intent(inout) :: this
    type(marbl_log_type),                 intent(inout) :: marbl_status_log

    character(*), intent(in) :: lname
    character(*), intent(in) :: sname
    character(*), intent(in) :: units
    character(*), intent(in) :: vgrid
    integer,      intent(in) :: rank
    integer,      intent(in) :: num_elements
    integer,      intent(in) :: num_levels

    character(*), parameter :: subname =                                      &
                  'marbl_interface_types:marbl_single_saved_state_construct'
    character(char_len)     :: log_message

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

    class(marbl_saved_state_type), intent(inout) :: this
    integer (int_kind) , intent(in) :: num_elements
    integer (int_kind) , intent(in) :: num_levels

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

    character(*),      intent(in)  :: lname
    character(*),      intent(in)  :: sname
    character(*),      intent(in)  :: units
    character(*),      intent(in)  :: vgrid
    integer(int_kind), intent(in)  :: rank
    integer(int_kind), intent(out) :: id

    character(*), parameter :: subname = 'marbl_interface_types:marbl_saved_state_add'
    character(len=char_len) :: log_message
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
             truncate, num_elements, num_levels, marbl_status_log)

    class(marbl_single_diagnostic_type) , intent(inout) :: this
    character(len=char_len) , intent(in)    :: lname, sname, units, vgrid
    logical                 , intent(in)    :: truncate
    integer                 , intent(in)    :: num_elements
    integer                 , intent(in)    :: num_levels
    type(marbl_log_type)    , intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_interface_types:marbl_single_diag_init'
    character(len=char_len) :: log_message

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

  end subroutine marbl_single_diag_init

  !*****************************************************************************

  subroutine marbl_single_sfo_constructor(this, num_elements, field_name, id, &
                                          marbl_status_log)
    class(marbl_single_sfo_type), intent(inout) :: this
    character(len=*),             intent(in)    :: field_name
    integer(int_kind),            intent(in)    :: num_elements
    integer(int_kind),            intent(in)    :: id
    type(marbl_log_type),         intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_interface_types:marbl_single_sfo_constructor'
    character(len=char_len) :: log_message

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
        write(log_message, "(2A)") trim(field_name),                            &
                                 " is not a valid surface forcing field name"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select
    write(log_message, "(3A)") "Adding ", trim(field_name), " to surface forcing outputs"
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
    character(*), parameter :: subname = 'marbl_interface_types:marbl_sfo_add'

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
      call marbl_status_log%log_error_trace('new_sfo%construct()', subname)
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

  subroutine marbl_diagnostics_set_to_zero(this, marbl_status_log)

    class(marbl_diagnostics_type), intent(inout) :: this
    type(marbl_log_type),          intent(inout) :: marbl_status_log

    integer (int_kind) :: n
    character(*), parameter :: subname = 'marbl_interface_types:marbl_diagnostics_set_to_zero'
    character(len=char_len) :: log_message

    do n=1,this%diag_cnt
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
             truncate, id, marbl_status_log)

    class(marbl_diagnostics_type) , intent(inout) :: this
    character(len=char_len)       , intent(in)    :: lname, sname, units, vgrid
    logical (int_kind)            , intent(in)    :: truncate
    integer (int_kind)            , intent(out)   :: id
    type(marbl_log_type)          , intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_interface_types:marbl_diagnostics_add'
    character(len=char_len) :: log_message

    this%diag_cnt = this%diag_cnt + 1
    id = this%diag_cnt
    if (id.gt.size(this%diags)) then
      log_message = "not enough memory allocated for this number of diagnostics!"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if
    call this%diags(id)%initialize(lname, sname, units, vgrid, truncate,      &
         this%num_elements, this%num_levels, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('this%diags%initialize()', subname)
      return
    end if

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

    character(*), parameter :: subname = 'marbl_interface_types:marbl_forcing_fields_set_rank'
    character(len=char_len) :: log_message

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

end module marbl_interface_types
