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

#if USE_GPTL
    print*, "GPTL Timers"
#else
#if HAVE_MPI
    print*, "MPI_Wtime() timers"
#else
    print*, "cpu_time() timers"
#endif
#endif

  end subroutine marbl_timing_init

end module marbl_timing_mod
