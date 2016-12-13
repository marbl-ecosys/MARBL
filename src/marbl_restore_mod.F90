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
  save

  type :: marbl_single_restoring_field_type
    ! Both the inverse timescale and the values to restore towards are provided
    ! to MARBL by the GCM (using the interior forcing data type; in that data
    ! type the fields are stored as 1 x km arrays (1 => num_elements)
    real(kind=r8), dimension(:,:), pointer :: inv_tau ! 1/time_scale (s^-1)
    real(kind=r8), dimension(:,:), pointer :: data
    integer                                :: restore_var_ind = 0
  end type marbl_single_restoring_field_type

  type, public :: marbl_restore_type
    type(marbl_single_restoring_field_type), allocatable, dimension(:) :: tracer_restore
  contains
     procedure, public :: init
     procedure, public :: restore_tracers
  end type marbl_restore_type

  integer, public :: tracer_restore_cnt

contains

!*****************************************************************************

subroutine init(this, domain, tracer_metadata, interior_forcings,             &
                restoring_inds, inv_tau_inds, marbl_status_log)

  ! initialize marbl_restore instance to default values, then read
  ! namelist and setup tracers that need to be restored

  use marbl_kinds_mod   , only : char_len, int_kind, i4, log_kind
  use marbl_logging     , only : marbl_log_type
  use marbl_interface_types, only : marbl_tracer_metadata_type
  use marbl_interface_types, only : marbl_forcing_fields_type
  use marbl_parms       , only : tracer_restore_vars

  implicit none

  !-----------------------------------------------------------------------
  !  input / output variables
  !-----------------------------------------------------------------------

  class(marbl_restore_type), intent(inout) :: this
  type(marbl_log_type),      intent(inout) :: marbl_status_log

  !-----------------------------------------------------------------------
  !  input variables
  !-----------------------------------------------------------------------

  type(marbl_domain_type),          intent(in) :: domain
  type(marbl_tracer_metadata_type), intent(in) :: tracer_metadata(:)
  type(marbl_forcing_fields_type),  intent(in) :: interior_forcings(:)
  integer,                          intent(in) :: restoring_inds(:)
  integer,                          intent(in) :: inv_tau_inds(:)

  !-----------------------------------------------------------------------
  !  local variables
  !-----------------------------------------------------------------------

  integer(int_kind) :: k, n, m
  character(*), parameter :: subname = 'marbl_restore_mod:init'
  character(len=char_len) :: log_message

  !-----------------------------------------------------------------------

  ! Process tracer_restore_vars to determine number of tracers to restore
  tracer_restore_cnt = count((len_trim(tracer_restore_vars).gt.0))
  allocate(this%tracer_restore(tracer_restore_cnt))

  ! Ensure there are no duplicate tracers listed and no empty strings in
  ! first tracer_restore_cnt elements of tracer_restore_vars
  do n=1,tracer_restore_cnt
    if (len_trim(tracer_restore_vars(n)).eq.0) then
      log_message = "Empty string appears in middle of tracer_restore_vars!"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if
    if (n.lt.marbl_total_tracer_cnt) then
      if (any(tracer_restore_vars(n).eq.tracer_restore_vars(n+1:))) then
        write(log_message,"(A,1X,A)") trim(tracer_restore_vars(n)),           &
                              "appears in tracer_restore_vars more than once"
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    end if
  end do

  if (tracer_restore_cnt.gt.0) then
    call marbl_status_log%log_noerror('', subname)
    log_message = "Restoring the following tracers to data:"
    call marbl_status_log%log_noerror(log_message, subname)
  end if
  do m=1, tracer_restore_cnt
    nullify(this%tracer_restore(m)%data)
    nullify(this%tracer_restore(m)%inv_tau)
    do n=1,size(tracer_metadata)
      if (trim(tracer_restore_vars(m)).eq.trim(tracer_metadata(n)%short_name)) exit
    end do
    if (n.le.size(tracer_metadata)) then
      this%tracer_restore(m)%inv_tau => interior_forcings(inv_tau_inds(n))%field_1d
      this%tracer_restore(m)%data    => interior_forcings(restoring_inds(n))%field_1d
      this%tracer_restore(m)%restore_var_ind = n
      write(log_message, "(2A,I0,A)") trim(tracer_metadata(n)%short_name),    &
                                      " (tracer index: ", n, ')'
      call marbl_status_log%log_noerror(log_message, subname)
    else
      write(log_message, "(2A)") "Can not find tracer named ",                &
            trim(tracer_restore_vars(m))
      call marbl_status_log%log_error(log_message, subname)
      return
    end if
  end do
  if (tracer_restore_cnt.gt.0) call marbl_status_log%log_noerror('', subname)

end subroutine Init

!*****************************************************************************

subroutine restore_tracers(this, interior_tracers, km, interior_restore)
  !
  !  restore a variable if required
  !
  use marbl_kinds_mod       , only : r8, int_kind, log_kind
  use marbl_constants_mod   , only : c0

  implicit none

  !-----------------------------------------------------------------------
  !  input variables
  !-----------------------------------------------------------------------

  class(marbl_restore_type),     intent(inout) :: this
  integer,                       intent(in)    :: km
  real(kind=r8), dimension(:,:), intent(in)    :: interior_tracers

  !-----------------------------------------------------------------------
  !  output variables
  !-----------------------------------------------------------------------

  real(kind=r8), dimension(marbl_total_tracer_cnt, km), intent(out) :: interior_restore

  !-----------------------------------------------------------------------
  !  local variables
  !-----------------------------------------------------------------------
  integer(int_kind) :: m, n
  !-----------------------------------------------------------------------

  interior_restore = c0

  do m=1,tracer_restore_cnt
    n = this%tracer_restore(m)%restore_var_ind
    associate(single_restore => this%tracer_restore(m))
    interior_restore(n,:) = (single_restore%data(1,:) - interior_tracers(n,:)) * &
                            single_restore%inv_tau(1,:)
    end associate
  end do

end subroutine restore_tracers

!*****************************************************************************

end module marbl_restore_mod
