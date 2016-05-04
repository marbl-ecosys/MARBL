module marbl_restore_mod
  !
  ! Module to generalize restoring any non-autotroph tracer
  !

  use marbl_kinds_mod      , only : r8, log_kind, int_kind
  use marbl_constants_mod  , only : p5, c0, c2, c1000
  use marbl_interface_types, only : marbl_forcing_file_type
  use marbl_interface_types, only : marbl_domain_type
  use marbl_sizes          , only : marbl_total_tracer_cnt

  implicit none

  private

  type :: marbl_single_restoring_field_type
    real(kind=r8), dimension(:), allocatable :: inv_tau ! 1/time_scale (s^-1)
    real(kind=r8), dimension(:), allocatable :: climatology ! Field we restore to
    type(marbl_forcing_file_type) :: file_metadata ! file containing climatology
  end type marbl_single_restoring_field_type

  type, public :: marbl_restore_type
    logical :: lrestore_any
    type(marbl_single_restoring_field_type), allocatable, dimension(:) :: tracer_restore
  contains
     procedure, public :: init
     procedure, public :: restore_tracers
  end type marbl_restore_type

  real (r8), parameter, private :: default_rest_time_inv_surf = c0
  real (r8), parameter, private :: default_rest_time_inv_deep = c0
  real (r8), parameter, private :: default_rest_z0 = c1000
  real (r8), parameter, private :: default_rest_z1 = c2 * c1000

contains

!*****************************************************************************

subroutine init(this, nl_buffer, domain, tracer_metadata, status_log)

  ! initialize marbl_restore instance to default values, then read
  ! namelist and setup tracers that need to be restored

  use marbl_kinds_mod   , only : char_len, int_kind, i4, log_kind
  use marbl_logging     , only : marbl_log_type
  use marbl_interface_types, only : marbl_tracer_metadata_type
  use marbl_namelist_mod, only : marbl_nl_cnt
  use marbl_namelist_mod, only : marbl_nl_buffer_size
  use marbl_namelist_mod, only : marbl_namelist

  implicit none

  !-----------------------------------------------------------------------
  !  input / output variables
  !-----------------------------------------------------------------------

  class(marbl_restore_type), intent(inout) :: this
  type(marbl_log_type),      intent(inout) :: status_log

  !-----------------------------------------------------------------------
  !  input variables
  !-----------------------------------------------------------------------

  character(marbl_nl_buffer_size), dimension(marbl_nl_cnt), intent(in) :: nl_buffer
  type(marbl_domain_type),                                  intent(in) :: domain
  type(marbl_tracer_metadata_type), dimension(:),           intent(in) :: tracer_metadata

  !-----------------------------------------------------------------------
  !  local variables
  !-----------------------------------------------------------------------

  character(len=char_len), allocatable, dimension(:) :: restore_short_names, &
                                                        restore_filenames,   &
                                                        restore_file_varnames
  real(r8) :: rest_time_inv_surf, rest_time_inv_deep, rest_z0, rest_z1
  real(r8), dimension(:), allocatable :: inv_tau
  integer(int_kind) :: nml_error, k, n, t
  character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
  character(*), parameter :: subname = 'marbl_restore_mod:init'
  character(len=char_len) :: log_message

  !-----------------------------------------------------------------------

  namelist /ecosys_restore_nml/       &
       restore_short_names,           &
       restore_filenames,             &
       restore_file_varnames,         &
       rest_time_inv_surf,            &
       rest_time_inv_deep,            &
       rest_z0, rest_z1

  this%lrestore_any = .false.
  allocate(this%tracer_restore(marbl_total_tracer_cnt))
  allocate(restore_short_names(marbl_total_tracer_cnt))
  allocate(restore_filenames(marbl_total_tracer_cnt))
  allocate(restore_file_varnames(marbl_total_tracer_cnt))
  allocate(inv_tau(domain%km))

  ! initialize namelist variables to default values
  restore_short_names = ''
  restore_filenames = ''
  restore_file_varnames = ''

  rest_time_inv_surf = default_rest_time_inv_surf
  rest_time_inv_deep = default_rest_time_inv_deep
  rest_z0 = default_rest_z0
  rest_z1 = default_rest_z1

  tmp_nl_buffer = marbl_namelist(nl_buffer, 'ecosys_restore_nml')
  read(tmp_nl_buffer, nml=ecosys_restore_nml, iostat=nml_error)
  if (nml_error /= 0) then
     log_message = "Error reading ecosys_restore_nml"
     call status_log%log_error(log_message, subname)
     return
  else
    ! FIXME #16: this is printing contents of pop_in, not the entire ecosys_restore_nml
    call status_log%log_namelist('ecosys_restore_nml', tmp_nl_buffer, subname)
  end if

  ! Set up inverse tau based on namelist values
  if (rest_z1 == rest_z0) then
    inv_tau(:) = rest_time_inv_surf + p5 * (rest_time_inv_deep - rest_time_inv_surf)
  else
    do k = 1, size(domain%zt)
      if (domain%zt(k) < rest_z0) then
        inv_tau(k) = rest_time_inv_surf
      else if (domain%zt(k) > rest_z1) then
        inv_tau(k) = rest_time_inv_deep
      else
        inv_tau(k) = rest_time_inv_surf +                             &
                     (domain%zt(k) - rest_z0) / (rest_z1 - rest_z0) * &
                     (rest_time_inv_deep - rest_time_inv_surf)
      endif
    end do
  endif

  ! FIXME #35: assert(len(restore_short_names) == len(restore_filenames))
  write(log_message, "(A)") "Found restore variables : "
  call status_log%log_noerror(log_message, subname)
  do t = 1, size(restore_short_names)
     if (len(trim(restore_short_names(t))) > 0) then
        this%lrestore_any = .true.
        write(log_message, "(6A)") trim(restore_short_names(t)), " --> ", &
             trim(restore_filenames(t)), " [ ", trim(restore_file_varnames(t)), " ]"
        call status_log%log_noerror(log_message, subname)
        do n=1,size(tracer_metadata)
          if (trim(restore_short_names(t)).eq.trim(tracer_metadata(n)%short_name)) exit
        end do
        if (n.le.size(tracer_metadata)) then
          write(log_message, "(3A,I0)") "Index for ", trim(restore_short_names(t)), &
                                       " is ", n
          call status_log%log_noerror(log_message, subname)
          allocate(this%tracer_restore(n)%inv_tau(domain%km))
          allocate(this%tracer_restore(n)%climatology(domain%km))
          this%tracer_restore(n)%inv_tau(:) = inv_tau
          this%tracer_restore(n)%climatology(:) = c0
          this%tracer_restore(n)%file_metadata%filename = trim(restore_filenames(t))
          this%tracer_restore(n)%file_metadata%file_varname = trim(restore_file_varnames(t))
        else
          write(log_message, "(2A)") "Can not find tracer named ", trim(restore_short_names(t))
          call status_log%log_error(log_message, subname)
        end if
     end if
  end do

end subroutine Init

!*****************************************************************************

subroutine restore_tracers(this, interior_tracers, km, nt, interior_restore)
  !
  !  restore a variable if required
  !
  use marbl_kinds_mod       , only : r8, int_kind, log_kind
  use marbl_constants_mod   , only : c0

  implicit none

  !-----------------------------------------------------------------------
  !  input variables
  !-----------------------------------------------------------------------

  class(marbl_restore_type),        intent(inout) :: this
  integer,                          intent(in)    :: km, nt
  real(kind=r8), dimension(nt, km), intent(in)    :: interior_tracers

  !-----------------------------------------------------------------------
  !  output variables
  !-----------------------------------------------------------------------

  real(kind=r8), dimension(nt, km), intent(out) :: interior_restore

  !-----------------------------------------------------------------------
  !  local variables
  !-----------------------------------------------------------------------
  integer(int_kind) :: n
  !-----------------------------------------------------------------------

  if (this%lrestore_any) then
    do n=1,nt
      associate(single_restore => this%tracer_restore(n))
        if (allocated(single_restore%climatology)) then
          interior_restore(n,:) = (single_restore%climatology(:)-interior_tracers(n,:))*&
                                  single_restore%inv_tau(:)
        else
          interior_restore(n,:) = c0
        end if
      end associate
    end do
  else
    interior_restore(:,:) = c0
  end if

end subroutine restore_tracers

!*****************************************************************************

end module marbl_restore_mod
