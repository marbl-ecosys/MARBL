.. _coding-conventions:

===========================
Coding conventions in MARBL
===========================

MARBL is written in Fortran.
A few constructs commonly used in MARBL may be unfamiliar to scientific programmers.

------------------------------------
Object-oriented programming features
------------------------------------

Object-oriented programming constructs permit the definition of classes that both contain data and methods which can perform operations on that data.

.. _ref-OO-examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example of an Object-oriented Class
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The class used to time blocks of code inside MARBL, ``marbl_internal_timers_type``, is object-oriented:

.. block from marbl_timing_mod
.. code-block:: fortran

  !*****************************************************************************
  ! Internal timer types

  type :: marbl_single_timer_type
    character(len=char_len) :: name
    logical                 :: is_running
    logical                 :: is_threaded
    real(r8)                :: cur_start
    real(r8)                :: cumulative_runtime
  contains
    procedure :: init => init_single_timer
  end type marbl_single_timer_type

  type, public :: marbl_internal_timers_type
    integer :: num_timers
    type(marbl_single_timer_type), allocatable :: individual_timers(:)
  contains
    procedure :: add => add_new_timer
    procedure :: start => start_timer
    procedure :: stop => stop_timer
    procedure :: extract => extract_timer_data
    procedure :: setup => setup_timers
    procedure :: reset => reset_timers
    procedure :: shutdown => shutdown_timers
  end type marbl_internal_timers_type

This type includes several `methods`, such as the ``start()`` routine, referenced to the subroutine ``start_timer``.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How to Call an Object-oriented Subroutine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A subroutine inside a class is referenced just like any other member, via the ``%`` character.
For example, MARBL times the call to the subroutine ``marbl_compute_carbonate_chemistry()`` from
the subroutine ``marbl_interior_tendency_compute()`` (part of ``marbl_interior_tendency_mod.F90``).
The timer calls look like this:

.. block from marbl_interior_tendency_mod
.. code-block:: fortran

  subroutine marbl_interior_tendency_compute( &
    .
    .
    .
    type(marbl_internal_timers_type),                        intent(inout) :: marbl_timers
    .
    .
    .
    call marbl_timers%start(marbl_timer_indices%carbonate_chem_id,            &
                            marbl_status_log)
    call compute_carbonate_chemistry(domain, temperature, pressure,           &
         salinity, tracer_local(:, :), marbl_tracer_indices, carbonate,       &
         ph_prev_col(:), ph_prev_alt_co2_col(:), marbl_status_log)
    call marbl_timers%stop(marbl_timer_indices%carbonate_chem_id,             &
                            marbl_status_log)


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example of a Subroutine Inside an Object-oriented Class
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The subroutine header looks like this:

.. block from marbl_timing_mod
.. code-block:: fortran

  subroutine start_timer(self, id, marbl_status_log)

    class(marbl_internal_timers_type), intent(inout) :: self
    integer,                           intent(in)    :: id
    type(marbl_log_type),              intent(inout) :: marbl_status_log

One key thing to note here is the use of ``self``, the first argument to the subroutine.
In this case, ``self`` stands for the particular instance of an object of type ``marbl_internal_timers_type``.
The subroutine is actually part of this object, which lets it access members of the class without explicitly passing them through the interface.
So this subroutine can change members of ``self%individual_timers(:)`` (e.g. ``individual_timers(:)%cur_start``).

-------------------
Associate construct
-------------------

The ``associate`` construct allows complex variables or expression to be denoted by a simple name or "alias."
The association between the name and the underlying variable is terminated at the end of the associate block.

If we look closer at the ``start_timer`` routine, we see an example:

.. block from marbl_timing_mod
.. code-block:: fortran

    associate(timer => self%individual_timers(id))
      if (timer%is_running) then
        log_message = 'Timer has already been started!'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if

      timer%is_running = .true.
      .
      .
      .
      timer%cur_start = get_time()
     end associate

In this case, ``timer`` replaces all instances of the more complicated expression ``self%individual_timers(id)``.
The assocation is terminated at ``end associate``.
