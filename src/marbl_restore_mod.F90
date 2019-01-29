module marbl_restore_mod
  !
  ! Module to generalize restoring any non-autotroph tracer
  !

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_constants_mod, only : p5
  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c2
  use marbl_constants_mod, only : c1000

  use marbl_interface_public_types, only : marbl_domain_type

  implicit none
  public
  save

contains

!*****************************************************************************

subroutine marbl_restore_compute_interior_restore(interior_tracers,              &
                                                  interior_tendency_forcings,    &
                                                  interior_tendency_forcing_ind, &
                                                  interior_restore)
  !
  !  restore a variable if required
  !
  use marbl_interface_public_types, only : marbl_forcing_fields_type
  use marbl_interface_private_types, only : marbl_interior_tendency_forcing_indexing_type

  !-----------------------------------------------------------------------
  !  input variables
  !-----------------------------------------------------------------------

  real(kind=r8), dimension(:,:),                        intent(in) :: interior_tracers
  type(marbl_forcing_fields_type),                      intent(in) :: interior_tendency_forcings(:)
  type(marbl_interior_tendency_forcing_indexing_type),  intent(in) :: interior_tendency_forcing_ind

  !-----------------------------------------------------------------------
  !  output variables
  !-----------------------------------------------------------------------

  real(kind=r8), dimension(:, :), intent(out) :: interior_restore

  !-----------------------------------------------------------------------
  !  local variables
  !-----------------------------------------------------------------------
  integer(int_kind), pointer :: restoring_inds(:)
  integer(int_kind), pointer :: inv_tau_inds(:)
  integer(int_kind) :: m, n
  !-----------------------------------------------------------------------

  interior_restore = c0
  restoring_inds => interior_tendency_forcing_ind%tracer_restore_id
  inv_tau_inds   => interior_tendency_forcing_ind%inv_tau_id

  do m=1,size(interior_tendency_forcing_ind%tracer_id)
    n = interior_tendency_forcing_ind%tracer_id(m)
    associate(restore_field => interior_tendency_forcings(restoring_inds(n))%field_1d, &
              inv_tau       => interior_tendency_forcings(inv_tau_inds(n))%field_1d)
      interior_restore(n,:) = (restore_field(1,:) - interior_tracers(n,:)) * inv_tau(1,:)
    end associate
  end do

end subroutine marbl_restore_compute_interior_restore

!*****************************************************************************

end module marbl_restore_mod
