module marbl_timing_mod

  implicit none
  private
  save

#if USE_GPTL
#include "gptl.inc"
#endif

  public :: marbl_timing_init

Contains

  subroutine marbl_timing_init()

    print*, "Entering timing init!"

  end subroutine marbl_timing_init

end module marbl_timing_mod
