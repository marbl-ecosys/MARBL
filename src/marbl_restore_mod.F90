module marbl_restore_mod
  !
  ! Module to generalize restoring any non-autotroph tracer
  !

  use marbl_kinds_mod      , only : r8, int_kind, char_len
  use marbl_constants_mod  , only : p5, c0, c2, c1000
  use marbl_interface_types, only : marbl_domain_type
  use marbl_sizes          , only : marbl_total_tracer_cnt
  use marbl_sizes          , only : tracer_restore_cnt

  implicit none
  public
  save

contains

!*****************************************************************************

subroutine marbl_restore_compute_interior_restore(interior_tracers, km,       &
                                                  interior_forcings,          &
                                                  interior_forcing_ind,       &
                                                  interior_restore)
  !
  !  restore a variable if required
  !
  use marbl_kinds_mod      , only : r8, int_kind
  use marbl_constants_mod  , only : c0
  use marbl_interface_types, only : marbl_forcing_fields_type
  use marbl_internal_types , only : marbl_interior_forcing_indexing_type

  !-----------------------------------------------------------------------
  !  input variables
  !-----------------------------------------------------------------------

  real(kind=r8), dimension(:,:),               intent(in) :: interior_tracers
  integer,                                     intent(in) :: km
  type(marbl_forcing_fields_type),             intent(in) :: interior_forcings(:)
  type(marbl_interior_forcing_indexing_type),  intent(in) :: interior_forcing_ind

  !-----------------------------------------------------------------------
  !  output variables
  !-----------------------------------------------------------------------

  real(kind=r8), dimension(marbl_total_tracer_cnt, km), intent(out) :: interior_restore

  !-----------------------------------------------------------------------
  !  local variables
  !-----------------------------------------------------------------------
  integer(int_kind), pointer :: restoring_inds(:)
  integer(int_kind), pointer :: inv_tau_inds(:)
  integer(int_kind) :: m, n
  !-----------------------------------------------------------------------

  interior_restore = c0
  restoring_inds => interior_forcing_ind%tracer_restore_id
  inv_tau_inds   => interior_forcing_ind%inv_tau_id

  do m=1,tracer_restore_cnt
    n = interior_forcing_ind%tracer_id(m)
    associate(restore_field => interior_forcings(restoring_inds(n))%field_1d, &
              inv_tau       =>  interior_forcings(inv_tau_inds(n))%field_1d)
      interior_restore(n,:) = (restore_field(1,:) - interior_tracers(n,:)) * inv_tau(1,:)
    end associate
  end do

end subroutine marbl_restore_compute_interior_restore

!*****************************************************************************

end module marbl_restore_mod
