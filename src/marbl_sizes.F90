module marbl_sizes

  use marbl_kinds_mod, only : int_kind

  implicit none

  public

  !-----------------------------------------------------------------------------
  ! number of ecosystem constituents and grazing interactions
  ! Stored here and not in marbl_parms to avoid circular dependency
  ! between marbl_parms and marbl_internal_types
  !-----------------------------------------------------------------------------

  integer (KIND=int_kind), target :: zooplankton_cnt
  integer (KIND=int_kind), target :: autotroph_cnt
  integer (KIND=int_kind), target :: max_grazer_prey_cnt

end module marbl_sizes
