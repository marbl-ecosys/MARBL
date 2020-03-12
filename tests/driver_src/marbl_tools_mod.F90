module marbl_tools_mod

  use marbl_kinds_mod, only : r8, char_len
  use marbl_interface, only : marbl_interface_class
  use marbl_logging,   only : marbl_log_type
  use marbl_mpi_mod,   only : my_task
  use marbl_mpi_mod,   only : mpi_on
  use marbl_mpi_mod,   only : num_tasks

  implicit none
  private
  save

  public :: marbl_tools_summarize_timers
  public :: marbl_tools_distribute_cols

contains

  !****************************************************************************

  subroutine marbl_tools_summarize_timers(marbl_instances, driver_status_log, header_text)
    ! TODO: differentiate between multiple MPI tasks and multiple instances
    !       * The goal is to get reasonable timing info when running a test
    !         that has multiple instances divided across multiple MPI tasks

    use marbl_mpi_mod, only : marbl_mpi_send
    use marbl_mpi_mod, only : marbl_mpi_recv

    type(marbl_interface_class), intent(in)   :: marbl_instances(:)
    type(marbl_log_type),       intent(inout) :: driver_status_log
    character(len=*), optional, intent(in)    :: header_text

    character(len=*), parameter :: subname = 'marbl_tools_mod:marbl_tools_summarize_timers'
    character(len=char_len) :: log_message

    real(r8) :: min_runtime, ind_runtime, max_runtime, tot_runtime
    real(r8), dimension(:), allocatable :: per_timer_tot_runtime
    character(len=15) :: int_to_str
    integer :: m, i, n, num_timers

100 format(A, ': ', F11.3, ' seconds',A)

    num_timers = marbl_instances(1)%timer_summary%num_timers
    allocate(per_timer_tot_runtime(num_timers))
    per_timer_tot_runtime = marbl_instances(1)%timer_summary%cumulative_runtimes
    do n=2, size(marbl_instances)
      per_timer_tot_runtime = per_timer_tot_runtime + marbl_instances(n)%timer_summary%cumulative_runtimes
    end do

    associate(timer_names => marbl_instances(1)%timer_summary%names)
      if (present(header_text)) then
        call driver_status_log%log_header(header_text, subname)
      else
        call driver_status_log%log_header('Timer summary', subname)
      end if
      write(log_message, "(A, I0, A)") 'There are ', num_timers, ' timers being returned'
      call driver_status_log%log_noerror(log_message, subname)
      call driver_status_log%log_noerror('----', subname)
      do i=1, num_timers
        ind_runtime = per_timer_tot_runtime(i)
        if (mpi_on) then
          min_runtime = ind_runtime
          max_runtime = ind_runtime
          tot_runtime = ind_runtime
          if (my_task.eq.0) then
            write(log_message, 100) trim(timer_names(i)), ind_runtime,       &
                                    ' (Task 0)'
            call driver_status_log%log_noerror(log_message, subname)
            do m=1, num_tasks-1
              call marbl_mpi_recv(ind_runtime, m)
              min_runtime = min(min_runtime, ind_runtime)
              max_runtime = max(max_runtime, ind_runtime)
              tot_runtime = tot_runtime + ind_runtime
              write(int_to_str, "(' (Task ',I0,')')") m
              write(log_message, 100) trim(timer_names(i)), ind_runtime,     &
                                      trim(int_to_str)
              call driver_status_log%log_noerror(log_message, subname)
            end do
          else ! not task 0
            call marbl_mpi_send(ind_runtime, 0)
          end if

          if (my_task.eq.0) then
            write(log_message, 100) trim(timer_names(i)), tot_runtime/real(num_tasks,r8), ' (avg)'
            call driver_status_log%log_noerror(log_message, subname)
            write(log_message, 100) trim(timer_names(i)), min_runtime, ' (min)'
            call driver_status_log%log_noerror(log_message, subname)
            write(log_message, 100) trim(timer_names(i)), max_runtime, ' (max)'
            call driver_status_log%log_noerror(log_message, subname)
          end if
        else ! no MPI
          write(log_message, 100) trim(timer_names(i)), ind_runtime, ''
          call driver_status_log%log_noerror(log_message, subname)
        end if
      end do
    end associate

  end subroutine marbl_tools_summarize_timers

  !*****************************************************************************

  subroutine marbl_tools_distribute_cols(num_cols, num_insts, col_start, col_cnt, driver_status_log)

    integer,              intent(in)    :: num_cols
    integer,              intent(in)    :: num_insts
    integer, allocatable, intent(inout) :: col_start(:)
    integer, allocatable, intent(inout) :: col_cnt(:)
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_tools_mod:marbl_tools_distribute_cols'
    character(len=char_len) :: log_message
    integer :: n, cols_remaining

    ! 1. allocate memory for col_start and col_cnt
    allocate(col_start(num_insts), col_cnt(num_insts))

    ! 2. Determine which columns each instance owns
    !    Note that we use 0-base indexing for
    !    col_start and 1-base indexing for col_id_loc
    !    (col_id_loc in [1, col_cnt(n)]), so
    !        col_id = col_start(n) + col_id_loc
    cols_remaining = num_cols
    do n=1, num_insts
      if (n.eq.1) then
        col_start(n) = 0
      else
        col_start(n) = col_start(n-1) + col_cnt(n-1)
      end if
      col_cnt(n) = cols_remaining / (num_insts-n+1) ! remaining cols / remaining insts
      cols_remaining = cols_remaining - col_cnt(n)
    end do

    !  3. Log decomposition
    do n=1, num_insts
      write(log_message, "(A, I0, A, I0, A, I0)") "Instance ", n, " has ", col_cnt(n),        &
                                                  " columns, beginning with ", col_start(n)+1
      call driver_status_log%log_noerror(log_message, subname)
    end do

  end subroutine marbl_tools_distribute_cols

  !****************************************************************************

end module marbl_tools_mod