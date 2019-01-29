module marbl_diagnostics_share_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind

  use marbl_constants_mod, only : c0

  use marbl_interface_private_types, only : marbl_surface_flux_diagnostics_indexing_type
  use marbl_interface_private_types, only : marbl_interior_tendency_diagnostics_indexing_type

  implicit none
  private

  !-----------------------------------------------------------------------
  !  Indices for diagnostic values written to tavg files
  !-----------------------------------------------------------------------

  type(marbl_surface_flux_diagnostics_indexing_type)      :: marbl_surface_flux_diag_ind
  type(marbl_interior_tendency_diagnostics_indexing_type) :: marbl_interior_tendency_diag_ind

  public :: marbl_surface_flux_diag_ind
  public :: marbl_interior_tendency_diag_ind
  public :: marbl_diagnostics_share_compute_vertical_integrals

  !***********************************************************************

contains

  !*****************************************************************************

  subroutine marbl_diagnostics_share_compute_vertical_integrals(integrand, delta_z, kmt, &
       full_depth_integral, near_surface_integral, integrated_terms, shallow_depth)

    real(kind=r8) , intent(in)             :: integrand(:)
    real(kind=r8) , intent(in)             :: delta_z(:)
    integer       , intent(in)             :: kmt
    ! For some vertical integral diagnostics, we need to add additional terms
    ! that have already been integrated, so they are separated from the
    ! integrand
    real(kind=r8) , intent(in)  , optional :: integrated_terms(:)
    real(kind=r8) , intent(in)  , optional :: shallow_depth
    real(kind=r8) , intent(out) , optional :: full_depth_integral
    real(kind=r8) , intent(out) , optional :: near_surface_integral

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: k
    real(kind=r8)      :: integrated_terms_used(size(integrand))
    real(kind=r8)      :: zw
    real(kind=r8)      :: ztop
    real(kind=r8)      :: shallow_depth_used
    !-----------------------------------------------------------------------

    if (present(integrated_terms)) then
       integrated_terms_used = integrated_terms
    else
       integrated_terms_used = c0
    end if

    if (present(shallow_depth)) then
      shallow_depth_used = shallow_depth
    else
      shallow_depth_used = 100.0e2_r8
    end if

    if (present(full_depth_integral)) then
       full_depth_integral = sum(delta_z(1:kmt)*integrand(1:kmt) + integrated_terms_used(1:kmt))
    end if

    if (present(near_surface_integral)) then
       ! initialize integral to zero
       near_surface_integral = c0
       ztop = c0
       zw = c0
       do k=1,kmt
          zw = zw + delta_z(k)
          near_surface_integral = near_surface_integral +                     &
                              min(shallow_depth_used-ztop,delta_z(k))*integrand(k)
          if (zw.le.shallow_depth_used) then
             near_surface_integral = near_surface_integral + integrated_terms_used(k)
          else
             exit
          end if
          ztop = zw
       end do
    end if

  end subroutine marbl_diagnostics_share_compute_vertical_integrals

  !***********************************************************************

end module marbl_diagnostics_share_mod