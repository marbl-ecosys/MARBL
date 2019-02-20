module marbl_mpi_mod

  ! This module contains wrappers for all MPI calls so that marbl.F90
  ! does not need to have #ifdef macros scattered throughout

  implicit none
  public

#define MPI 1
#if MARBL_TIMING_OPT == MPI
#define MARBL_WITH_MPI
#endif

#ifdef MARBL_WITH_MPI
  include 'mpif.h'
  logical, parameter :: mpi_on = .true.
#else
  logical, parameter :: mpi_on = .false.
#endif

  integer :: my_task
  integer :: num_tasks
  logical :: marbl_mpi_return_error = .true.

  interface marbl_mpi_send
    module procedure marbl_mpi_send_dbl
  end interface marbl_mpi_send

  interface marbl_mpi_recv
    module procedure marbl_mpi_recv_dbl
  end interface marbl_mpi_recv

  interface marbl_mpi_bcast
    module procedure marbl_mpi_bcast_str
    module procedure marbl_mpi_bcast_logical
    module procedure marbl_mpi_bcast_integer
  end interface marbl_mpi_bcast

  !****************************************************************************

contains

  !****************************************************************************

  subroutine marbl_mpi_init()

#ifdef MARBL_WITH_MPI
    integer :: ierr

    call MPI_Init(ierr)
    call check_mpi_err_code(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_task, ierr)
    call check_mpi_err_code(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_tasks, ierr)
    call check_mpi_err_code(ierr)
#else
    my_task = 0
    num_tasks = 1
#endif

  end subroutine marbl_mpi_init

  !****************************************************************************

  subroutine marbl_mpi_finalize()

#ifdef MARBL_WITH_MPI
    integer :: ierr

    call MPI_Finalize(ierr)
    call check_mpi_err_code(ierr)
#endif

  end subroutine marbl_mpi_finalize

  !****************************************************************************

  subroutine marbl_mpi_send_dbl(dbl_var, dest)

    use marbl_kinds_mod, only : r8

    real(r8), intent(in) :: dbl_var
    integer,  intent(in)  :: dest

    integer :: ierr

#ifdef MARBL_WITH_MPI
    call MPI_Send(dbl_var, 1, MPI_DOUBLE_PRECISION, dest, 2017, MPI_COMM_WORLD, ierr)
    call check_mpi_err_code(ierr)
#else
    ierr = dest + floor(dbl_var)
#endif

  end subroutine marbl_mpi_send_dbl

  !****************************************************************************

  subroutine marbl_mpi_recv_dbl(dbl_var, source)

    use marbl_kinds_mod, only : r8

    real(r8), intent(out) :: dbl_var
    integer,  intent(in)  :: source

    integer :: ierr

#ifdef MARBL_WITH_MPI
    integer :: status(MPI_STATUS_SIZE)

    call MPI_Recv(dbl_var, 1, MPI_DOUBLE_PRECISION, source, 2017, MPI_COMM_WORLD, status, ierr)
    call check_mpi_err_code(ierr)
#else
    dbl_var = 0._r8
    ierr = source
#endif

  end subroutine marbl_mpi_recv_dbl

  !****************************************************************************

  subroutine marbl_mpi_bcast_str(str_to_bcast, root)

    character(len=*), intent(inout) :: str_to_bcast
    integer,          intent(in)    :: root

    integer :: ierr

#ifdef MARBL_WITH_MPI
    call MPI_Bcast(str_to_bcast, len(str_to_bcast), MPI_CHARACTER, root, MPI_COMM_WORLD, ierr)
    call check_mpi_err_code(ierr)
#else
    ! Avoid an empty subroutine when no MPI
    ierr = root + len(str_to_bcast)
#endif

  end subroutine marbl_mpi_bcast_str

  !****************************************************************************

  subroutine marbl_mpi_bcast_logical(logical_to_bcast, root)

    logical, intent(inout) :: logical_to_bcast
    integer, intent(in)    :: root

    integer :: ierr

#ifdef MARBL_WITH_MPI
    call MPI_Bcast(logical_to_bcast, 1, MPI_LOGICAL, root, MPI_COMM_WORLD, ierr)
#else
    ! Avoid an empty subroutine when no MPI
    if (logical_to_bcast) ierr = root
#endif

  end subroutine marbl_mpi_bcast_logical

  !****************************************************************************

  subroutine marbl_mpi_bcast_integer(int_to_bcast, root)

    integer, intent(inout) :: int_to_bcast
    integer, intent(in)    :: root

    integer :: ierr

#ifdef MARBL_WITH_MPI
    call MPI_Bcast(int_to_bcast, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
    call check_mpi_err_code(ierr)
#else
    ! Avoid an empty subroutine when no MPI
    ierr = root + int_to_bcast
#endif

  end subroutine marbl_mpi_bcast_integer

  !****************************************************************************

  subroutine marbl_mpi_abort()

#ifdef MARBL_WITH_MPI
    call marbl_mpi_barrier()
    call marbl_mpi_finalize()
#endif
    if (marbl_mpi_return_error) stop 1
    stop

  !****************************************************************************

  end subroutine marbl_mpi_abort

  !****************************************************************************

#ifdef MARBL_WITH_MPI

  subroutine marbl_mpi_barrier()

    integer :: ierr

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    call check_mpi_err_code(ierr)

  end subroutine marbl_mpi_barrier

  !****************************************************************************

  subroutine check_mpi_err_code(ierr)

    integer, intent(in) :: ierr

    if (ierr /= MPI_SUCCESS) then
      call marbl_mpi_abort()
    end if

  end subroutine check_mpi_err_code

#endif

!****************************************************************************

end module marbl_mpi_mod
