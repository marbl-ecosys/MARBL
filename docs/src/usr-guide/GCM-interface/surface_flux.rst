.. _surface_flux:

.. _ref-compute-surface-fluxes:

======================
Compute Surface Fluxes
======================

``surface_flux_compute()`` computes surface fluxes (and related diagnostics) over a 1D array of grid cells.
The array is assumed to be length ``num_elements_surface_flux``, per :ref:`ref-init-interface`.

------------------------------
Example from Stand-Alone MARBL
------------------------------

The GCM needs to make sure the MARBL instance has all the data it needs to compute surface fluxes correctly.
The following blocks of code can all be found in ``tests/driver_src/marbl_call_compute_subroutines_drv.F90``:

.. _ref-global-scalars-surface-flux:

~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Set global scalars
~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, there are no MARBL configurations requiring :ref:`global scalars <global-scalars>` to compute the surface fluxes.
To prepare for future updates where that is no longer the case, it is recommended that the GCM calls

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

  ! 5. Call surface_flux_compute() (all columns simultaneously)
  !    (a) call set_global_scalars() for consistent setting of time-varying scalars
  !        [surface_flux computation doesn't currently have any time-varying scalars]
  call marbl_instances(n)%set_global_scalars('surface_flux')

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Copy data into MARBL
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MARBL needs surface tracer values, surface flux forcings, and saved state values.
These are copied to ``marbl_instances(n)%tracers_at_surface``, ``marbl_instances(n)%surface_flux_forcings``, and
``marbl_instances(n)%surface_flux_saved_state%state``, respectively.

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

  do col_id_loc = 1, col_cnt(n)
    col_id = col_start(n)+col_id_loc

    !  (b) copy surface tracer values into marbl_instances(n)%tracers_at_surface
    marbl_instances(n)%tracers_at_surface(col_id_loc, :) = tracer_initial_vals(:, 1, col_id)

    !  (c) copy surface flux forcings into marbl_instances(n)%surface_flux_forcings
    do m=1, size(marbl_instances(n)%surface_flux_forcings)
      if (associated(marbl_instances(n)%surface_flux_forcings(m)%field_0d)) then
        marbl_instances(n)%surface_flux_forcings(m)%field_0d(col_id_loc) = surface_flux_forcings(m)%field_0d(col_id)
      else
        marbl_instances(n)%surface_flux_forcings(m)%field_1d(col_id_loc,:) = surface_flux_forcings(m)%field_1d(col_id,:)
      end if
    end do
  end do

  !    (d) populate marbl_instances(n)%surface_flux_saved_state (with 0s)
  do m=1, size(marbl_instances(n)%surface_flux_saved_state%state)
    marbl_instances(n)%surface_flux_saved_state%state(m)%field_2d(:) = 0._r8
  end do

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 3. Call ``surface_flux_compute()``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since all the data is available on the class object, the call to the routine does not require any arguments:

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

  !    (e) call surface_flux_compute()
  call marbl_instances(n)%surface_flux_compute()

.. _ref-after-surface-flux-call:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 4. Copy values MARBL will need later into a local buffer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After computing the surface fluxes, MARBL returns several different fields to the GCM.
Each GCM will handle these fields in its own way, but it is important to know where to find them.
In the descriptions below, ``num_elements_surface_flux`` is the number of grid cells MARBL computes surface fluxes for simultaneously.

#. The surface fluxes themselves, which are needed in the source term of the advection solver

   * ``marbl_instance%surface_fluxes(:,:)`` is a ``num_elements_surface_flux`` by ``marbl_tracer_cnt`` array

#. Surface forcing fields, which may be requested by the GCM

#. Saved state, which the GCM should store and then provide to MARBL on the next time step

   * ``marbl_instance%surface_flux_saved_state`` is ``marbl_saved_state_type``
   * ``marbl_instance%surface_flux_saved_state%state(:)`` is array of ``marbl_single_saved_state_type`` containing data GCM should store for next time step

#. Values that need a global operation performed (per :ref:`above <ref-global-scalars-surface-flux>`, there are not yet any of these)

   * ``marbl_instance%glo_avg_fields_surface_flux(:,:)`` is a ``num_elements_surface_flux`` by ``glo_avg_field_cnt_surface_flux`` array
   * GCM should store fields in global array and compute an average prior to calling ``interior_tendency_compute()``
   * Recommended to do global average as soon as all surface fluxes have been computed

#. Diagnostics for the GCM to provide to the user

   * ``marbl_instance%surface_flux_diags`` is ``marbl_diagnostics_type``
   * ``marbl_instance%surface_flux_diags%diags(:)`` is array of ``marbl_single_diagnostic_type`` containing data GCM should add to diagnostic output

The stand-alone driver does not hold on to saved state (there is no time stepping involved).
It also does not request any surface forcing fields or compute global averages.
After the call to ``surface_flux_compute()``, the standalone driver copies diags into the buffer and stores the surface fluxes
(which are also written to the netCDF output file).

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

  !    (f) write to diagnostic buffers
  !        Note: passing col_start and col_cnt => surface flux diagnostic buffer
  call marbl_io_copy_into_diag_buffer(col_start(n), col_cnt(n), marbl_instances(n))
  surface_fluxes((col_start(n)+1):(col_start(n)+col_cnt(n)),:) = marbl_instances(n)%surface_fluxes(:,:)

A more complete example can be found in :ref:`how POP handles MARBL output <ref-surface_flux_in_POP>`.
