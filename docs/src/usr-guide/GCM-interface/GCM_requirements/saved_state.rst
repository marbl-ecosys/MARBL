.. _saved_state:

==============================================
How Many Saved State Fields Need to be Stored?
==============================================

MARBL does not yet have a stand-alone test to provide a list of requested saved state fields, but ``marbl_saved_state_init()`` shows that there are four saved state fields - two for the surface and two for the interior.
The two surface fields are ``surface pH`` and ``surface pH (alternate CO2)``:

.. block comes from marbl_saved_state_mod
.. code-block:: fortran

  call surface_state%construct(num_elements_surface_flux, num_levels)

  lname = 'surface pH'
  sname = 'PH_SURF'
  units = 'pH'
  vgrid = 'none'
  rank  = 2
  call surface_state%add_state(lname, sname, units, vgrid, rank,            &
       surf_ind%ph_surf, marbl_status_log)
  if (marbl_status_log%labort_marbl) then
    call marbl_status_log%log_error_trace("add_state(PH_SURF)", subname)
    return
  end if

  lname = 'surface pH (alternate CO2)'
  sname = 'PH_SURF_ALT_CO2'
  units = 'pH'
  vgrid = 'none'
  rank  = 2
  call surface_state%add_state(lname, sname, units, vgrid, rank,            &
       surf_ind%ph_alt_co2_surf, marbl_status_log)
  if (marbl_status_log%labort_marbl) then
    call marbl_status_log%log_error_trace("add_state(PH_SURF_ALT_CO2)", subname)
    return
  end if

The two interior state fields are ``3D pH`` and ``3D pH (alternate CO2)``

.. block comes from marbl_saved_state_mod
.. code-block:: fortran

  call interior_state%construct(num_elements_interior_tendency, num_levels)

  lname = '3D pH'
  sname = 'PH_3D'
  units = 'pH'
  vgrid = 'layer_avg'
  rank  = 3
  call interior_state%add_state(lname, sname, units, vgrid, rank,           &
       interior_ind%ph_col, marbl_status_log)
  if (marbl_status_log%labort_marbl) then
    call marbl_status_log%log_error_trace("add_state(PH_3D)", subname)
    return
  end if

  lname = '3D pH (alternate CO2)'
  sname = 'PH_3D_ALT_CO2'
  units = 'pH'
  vgrid = 'layer_avg'
  rank  = 3
  call interior_state%add_state(lname, sname, units, vgrid, rank,           &
       interior_ind%ph_alt_co2_col, marbl_status_log)
  if (marbl_status_log%labort_marbl) then
    call marbl_status_log%log_error_trace("add_state(PH_3D_ALT_CO2)", subname)
    return
  end if

The pH computation is an iterative solver, and it has proven useful to use the pH at timestep ``t`` as an initial value when solving for time ``t+1``.

----------------------------
Saved State in the Interface
----------------------------

MARBL splits the saved state fields between those needed for computing surface fluxes and those needed for computing interior tendencies, so on the interface we have

.. block comes from marbl_interface
.. code-block:: fortran

  type, public :: marbl_interface_class

    .
    .
    .
    type(marbl_saved_state_type)              , public               :: surface_flux_saved_state             ! input/output
    type(marbl_saved_state_type)              , public               :: interior_tendency_saved_state        ! input/output
    .
    .
    .
  end type marbl_interface_class

The ``marbl_saved_state_type`` is a wrapper for ``marbl_single_saved_state_type``:

.. block comes from marbl_interface_public_types
.. code-block:: fortran

  type, public :: marbl_single_saved_state_type
    integer                 :: rank
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: vertical_grid ! 'none', 'layer_avg', 'layer_iface'
    real(r8), allocatable, dimension(:)   :: field_2d  ! num_elements
    real(r8), allocatable, dimension(:,:) :: field_3d  ! num_levels, num_elements
  contains
    procedure :: construct => marbl_single_saved_state_construct
  end type marbl_single_saved_state_type

  !*****************************************************************************

  type, public :: marbl_saved_state_type
    integer :: saved_state_cnt
    integer :: num_elements
    integer :: num_levels
    type(marbl_single_saved_state_type), dimension(:), pointer :: state => NULL()
   contains
     procedure, public :: construct => marbl_saved_state_constructor
     procedure, public :: add_state => marbl_saved_state_add
  end type marbl_saved_state_type

-----------------------
What Should the GCM Do?
-----------------------

After ``marbl_instance%surface_flux_compute()`` returns, the GCM needs to process ``marbl_instance%surface_flux_saved_state``.
That means looping through each element in the ``marbl_instance%surface_flux_saved_state%state(:)`` array, checking ``state(n)%rank``,
and then storing either ``state(n)%field_2d`` or ``state(n)%field_3d`` in a global array.
Before calling ``surface_flux_compute()`` in the next time step, these saved values should be copied back into ``marbl_intance%surface_flux_saved_state``.

Similar actions must be taken with ``marbl_instance%interior_tendency_saved_state`` before / after calls to ``marbl_instance%interior_tendency_compute()``.

