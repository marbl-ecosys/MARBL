module marbl_restore_mod
  !
  ! Module to generalize restoring any non-autotroph tracer
  !

  use marbl_kinds_mod      , only : r8, log_kind, int_kind
  use marbl_constants_mod  , only : p5, c0, c2, c1000
  use marbl_interface_types, only : marbl_domain_type
  use marbl_sizes          , only : marbl_total_tracer_cnt

  implicit none

  private

  type :: marbl_single_restoring_field_type
    real(kind=r8), dimension(:), allocatable :: inv_tau ! 1/time_scale (s^-1)
    real(kind=r8), dimension(:), allocatable :: climatology ! Field we restore to
  end type marbl_single_restoring_field_type

  type, public :: marbl_restore_type
    logical :: lrestore_any
    type(marbl_single_restoring_field_type), allocatable, dimension(:) :: tracer_restore
  contains
     procedure, public :: init
     procedure, public :: restore_tracers
  end type marbl_restore_type

contains

!*****************************************************************************

subroutine init(this, domain, tracer_metadata, status_log)

  ! initialize marbl_restore instance to default values, then read
  ! namelist and setup tracers that need to be restored

  use marbl_kinds_mod   , only : char_len, int_kind, i4, log_kind
  use marbl_logging     , only : marbl_log_type
  use marbl_interface_types, only : marbl_tracer_metadata_type
  use marbl_parms       , only : tracer_restore_vars
  use marbl_parms       , only : rest_time_inv_surf
  use marbl_parms       , only : rest_time_inv_deep
  use marbl_parms       , only : rest_z0
  use marbl_parms       , only : rest_z1
  use marbl_parms       , only : inv_tau

  implicit none

  !-----------------------------------------------------------------------
  !  input / output variables
  !-----------------------------------------------------------------------

  class(marbl_restore_type), intent(inout) :: this
  type(marbl_log_type),      intent(inout) :: status_log

  !-----------------------------------------------------------------------
  !  input variables
  !-----------------------------------------------------------------------

  type(marbl_domain_type),                                  intent(in) :: domain
  type(marbl_tracer_metadata_type), dimension(:),           intent(in) :: tracer_metadata

  !-----------------------------------------------------------------------
  !  local variables
  !-----------------------------------------------------------------------

  integer(int_kind) :: k, n, t
  character(*), parameter :: subname = 'marbl_restore_mod:init'
  character(len=char_len) :: log_message

  !-----------------------------------------------------------------------

  this%lrestore_any = .false.
  allocate(this%tracer_restore(marbl_total_tracer_cnt))

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

  ! FIXME #35: assert(len(tracer_restore_vars) == len(restore_filenames))
  do t = 1, size(tracer_restore_vars)
     if (len(trim(tracer_restore_vars(t))) > 0) then
        if (.not.this%lrestore_any) then
           ! first time we encounter variable to restore, add header to status
           ! log (and update lrestore_any)
           call status_log%log_noerror('', subname)
           write(log_message, "(A)") "Found restore variables : "
           call status_log%log_noerror(log_message, subname)
           this%lrestore_any = .true.
        end if
        do n=1,size(tracer_metadata)
          if (trim(tracer_restore_vars(t)).eq.trim(tracer_metadata(n)%short_name)) exit
        end do
        if (n.le.size(tracer_metadata)) then
          write(log_message, "(3A,I0)") "Index for ", trim(tracer_restore_vars(t)), &
                                       " is ", n
          call status_log%log_noerror(log_message, subname)
          allocate(this%tracer_restore(n)%inv_tau(domain%km))
          allocate(this%tracer_restore(n)%climatology(domain%km))
          this%tracer_restore(n)%inv_tau(:) = inv_tau
          this%tracer_restore(n)%climatology(:) = c0
        else
          write(log_message, "(2A)") "Can not find tracer named ", trim(tracer_restore_vars(t))
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
