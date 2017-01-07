module marbl_mpi_mod

  ! This module contains wrappers for all MPI calls so that marbl.F90
  ! does not need to have #if HAVE_MPI macros scattered throughout

  Implicit None
  public

#if HAVE_MPI
  include 'mpif.h'
  logical, parameter :: mpi_on = .true.
#else
  logical, parameter :: mpi_on = .false.
#endif

  integer :: my_task
  integer :: num_tasks

  interface marbl_mpi_send
    module procedure marbl_mpi_send_dbl
  end interface marbl_mpi_send

  interface marbl_mpi_recv
    module procedure marbl_mpi_recv_dbl
  end interface marbl_mpi_recv

  interface marbl_mpi_bcast
    module procedure marbl_mpi_bcast_str
  end interface marbl_mpi_bcast

  !****************************************************************************

contains

  !****************************************************************************

  subroutine marbl_mpi_init()

#ifdef HAVE_MPI
    integer :: ierr

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_task, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_tasks, ierr)
#else
    my_task = 0
    num_tasks = 1
#endif

  end subroutine marbl_mpi_init

  !****************************************************************************

  subroutine marbl_mpi_finalize()

#ifdef HAVE_MPI
    integer :: ierr

    call MPI_Finalize(ierr)
#endif

  end subroutine marbl_mpi_finalize

  !****************************************************************************

  subroutine marbl_mpi_send_dbl(dbl_var, receiver)

    use marbl_kinds_mod, only : r8

    real(r8), intent(out) :: dbl_var
    integer,  intent(in)  :: receiver

#if HAVE_MPI
    integer :: ierr

    call MPI_Send(dbl_var, 1, MPI_DOUBLE_PRECISION, 0, 2017, MPI_COMM_WORLD, ierr)
#endif

  end subroutine marbl_mpi_send_dbl

  !****************************************************************************

  subroutine marbl_mpi_recv_dbl(dbl_var, sender)

    use marbl_kinds_mod, only : r8

    real(r8), intent(out) :: dbl_var
    integer,  intent(in)  :: sender

#if HAVE_MPI
    integer :: status(MPI_STATUS_SIZE)
    integer :: ierr

    call MPI_Recv(dbl_var, 1, MPI_DOUBLE_PRECISION, sender, 2017,             &
                  MPI_COMM_WORLD, status, ierr)
#endif

  end subroutine marbl_mpi_recv_dbl

  !****************************************************************************

  subroutine marbl_mpi_bcast_str(str_to_bcast, root_task)

    character(len=*), intent(inout) :: str_to_bcast
    integer,          intent(in)    :: root_task

    integer :: ierr

#if HAVE_MPI
    call MPI_Bcast(str_to_bcast, len(str_to_bcast), MPI_CHARACTER, root_task, &
                   MPI_COMM_WORLD, ierr)
#else
    ! Avoid an empty subroutien when no MPI
    ierr = root_task + len(str_to_bcast)
#endif

  end subroutine marbl_mpi_bcast_str

  !****************************************************************************

  subroutine marbl_mpi_abort()

#if HAVE_MPI
    integer :: ierr

    call MPI_Abort(MPI_COMM_WORLD, ierr)
#else
    stop 1
#endif

  !****************************************************************************

  end subroutine marbl_mpi_abort

  !****************************************************************************

end module marbl_mpi_mod
