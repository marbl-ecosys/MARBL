module marbl_interior_share_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_interface_private_types, only : column_sinking_particle_type

  implicit none
  private

  public :: marbl_interior_share_update_sinking_particle_from_level_above

contains

  !***********************************************************************

  subroutine marbl_interior_share_update_sinking_particle_from_level_above(k, sinking_particle)

    integer (int_kind), intent(in) :: k
    type(column_sinking_particle_type), intent(inout) :: sinking_particle

    ! NOTE(bja, 201504) level k influx is equal to the level k-1 outflux.
    sinking_particle%sflux_in(k)  = sinking_particle%sflux_out(k-1)
    sinking_particle%hflux_in(k)  = sinking_particle%hflux_out(k-1)

  end subroutine marbl_interior_share_update_sinking_particle_from_level_above

  !***********************************************************************

end module marbl_interior_share_mod