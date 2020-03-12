.. _interior_tend:

.. _ref-compute-interior-tendencies:

==================================
Compute Interior Tracer Tendencies
==================================

``interior_tendency_compute()`` computes interior tracer tendencies (and related diagnostics) for a single column.
(Recall that ``num_elements_interior_tendency = 1``, per :ref:`ref-init-interface`.)

------------------------------
Example from Stand-Alone MARBL
------------------------------

The GCM needs to make sure the MARBL instance has all the data it needs to compute surface fluxes correctly.
The following blocks of code can all be found in ``tests/driver_src/marbl_call_compute_subroutines_drv.F90``:

.. _ref-global-scalars-interior-tend:

~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Set global scalars
~~~~~~~~~~~~~~~~~~~~~~~~~~

If MARBL is configured with ``ladjust_bury_coeff = .true.`` then it will request running means of global averages of a few fields.

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

  !  (a) call set_global_scalars() for consistent setting of time-varying scalars
  !      [necessary when running ladjust_bury_coeff, since GCM is responsible
  !       for computing running means of values needed to compute burial coefficients]
  call marbl_instances(n)%set_global_scalars('interior_tendency')

Note that at this point, the GCM is responsible for both the global averaging and keeping the running means; in the future running means will be computed in MARBL (and requested as part of saved state).
At present there is not an example of this behavior in the stand-alone driver.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Copy data into MARBL
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Interior tracer tendencies are computed for a single column in MARBL.
For each column, MARBL needs to know the following:

#. Domain information (level depths, layer thicknesses, number of active levels, etc)
#. Tracer values (for each level and each tracer)
#. Interior forcing data
#. Saved state

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

  !  (b) set domain information
  !      In this example, the vertical grid is the same from column to column and
  !      therefore set during initialization. The columns vary in depth, so
  !      the index of the bottom layer must be updated for each column.
  marbl_instances(n)%domain%kmt = active_level_cnt(col_id)

  !  (c) copy tracer values into marbl_instances(n)%tracers
  marbl_instances(n)%tracers = tracer_initial_vals(:,:,col_id)

  !  (d) copy interior tendency forcings into marbl_instances(n)%interior_tendency_forcings
  do m=1, size(marbl_instances(n)%interior_tendency_forcings)
    if (associated(marbl_instances(n)%interior_tendency_forcings(m)%field_0d)) then
      marbl_instances(n)%interior_tendency_forcings(m)%field_0d(1) = &
           interior_tendency_forcings(m)%field_0d(col_id)
    else
      marbl_instances(n)%interior_tendency_forcings(m)%field_1d(1,:) = &
           interior_tendency_forcings(m)%field_1d(col_id,:)
    end if
  end do

  !  (e) populate marbl_instances(n)%interior_tendency_saved_state (with 0s)
  do m=1, size(marbl_instances(n)%interior_tendency_saved_state%state)
    if (allocated(marbl_instances(n)%interior_tendency_saved_state%state(m)%field_2d)) then
      marbl_instances(n)%interior_tendency_saved_state%state(m)%field_2d(:) = 0._r8
    else
      marbl_instances(n)%interior_tendency_saved_state%state(m)%field_3d(:,1) = 0._r8
    end if
  end do

.. _ref-GCM-update-interior-tend:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 3. Call ``interior_tendency_compute()``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since all the data is available on the class object, the call to the routine does not require any arguments:

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

  !  (f) call interior_tendency_compute()
  call marbl_instances(n)%interior_tendency_compute()

.. _ref-after-interior-tend-call:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 4. Copy values MARBL will need later into a local buffer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After computing the tracer tendencies, MARBL returns several fields to the GCM.
Each GCM will handle these fields in its own way, but it is important to know where to find them.
MARBL returns all these fields on a per-column basis.
Some variables may have a ``num_elements_interior_tendency`` dimension, but that dimension is hard-coded to be 1.
POP checks to ensure that MARBL does set any values to ``NaN``, other GCMs may or may not want to do so as well.

#. The interior tendencies themselves, which are needed in the source term of the advection solver

   * ``marbl_instance%interior_tendencies(:,:)`` is a ``marbl_tracer_cnt`` by ``num_levels`` array

#. Saved state, which the GCM should store and then provide to MARBL on the next time step

   * ``marbl_instance%interior_tendency_saved_state`` is ``marbl_saved_state_type``
   * ``marbl_instance%interior_tendency_saved_state%state(:)`` is array of ``marbl_single_saved_state_type`` containing data GCM should store for next time step

#. Values that need a global operation performed

   * ``marbl_instance%glo_avg_fields_interior_tendency(:)`` is an array of length ``glo_avg_field_cnt_interior_tendency``
   * The global average should be computed prior to the next ``interior_tendency_compute()`` call

#. Diagnostics for the GCM to provide to the user

   * ``marbl_instance%interior_tendency_diags`` is ``marbl_diagnostics_type``
   * ``marbl_instance%interior_tendency_diags%diags(:)`` is array of ``marbl_single_diagnostic_type`` containing data GCM should add to diagnostic output

The stand-alone driver does not hold on to saved state (there is no time stepping involved) or compute global averages.
After the call to ``interior_tendency_compute()``, the standalone driver copies diags into the buffer and stores the tendencies
(which are also written to the netCDF output file).

.. block comes from marbl_call_compute_subroutines_drv.F90
.. code-block:: fortran

  !  (g) write to diagnostic buffer
  !        Note: passing just col_id => interior tendency diagnostic buffer
  call marbl_io_copy_into_diag_buffer(col_id, marbl_instances(n))
  interior_tendencies(:,:,col_id) = marbl_instances(n)%interior_tendencies(:,:)

A more complete example can be found in :ref:`how POP handles MARBL output <ref-interior_tend_in_POP>`.
