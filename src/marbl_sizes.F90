module marbl_sizes

  use marbl_kinds_mod, only : int_kind

  implicit none

  public

  !-----------------------------------------------------------------------------
  ! number of ecosystem tracers
  !-----------------------------------------------------------------------------

  integer(int_kind), parameter :: ecosys_base_tracer_cnt = ECOSYS_BASE_NT
  integer(int_kind), parameter :: ciso_tracer_cnt = 16
  integer(int_kind)            :: marbl_total_tracer_cnt = 0
  integer(int_kind)            :: tracer_restore_cnt = 0


  !-----------------------------------------------------------------------------
  ! number of ecosystem constituents and grazing interactions
  !-----------------------------------------------------------------------------

  integer (KIND=int_kind), parameter :: zooplankton_cnt = ZOOPLANKTON_CNT
  integer (KIND=int_kind), parameter :: autotroph_cnt   = AUTOTROPH_CNT
  integer (KIND=int_kind), parameter :: grazer_prey_cnt = GRAZER_PREY_CNT

  integer (KIND=int_kind), parameter :: max_prey_class_size = 9

  !-----------------------------------------------------------------------------
  ! number of forcing fields
  !-----------------------------------------------------------------------------

  integer :: num_surface_forcing_fields
  integer :: num_interior_forcing_fields

end module marbl_sizes
