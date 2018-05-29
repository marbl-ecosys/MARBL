.. _interior_tend:

==================================
Compute Interior Tracer Tendencies
==================================

``set_interior_forcing()`` computes interior tracer tendencies (and related diagnostics) for a single column.
(Recall that ``num_interior_forcing_elements = 1``, per :ref:`ref-init-interface`.)
The stand-alone test suite does not yet call this routine, so examples come from the POP driver.
The call to the routine is straightforward:

.. code-block:: fortran

       call marbl_instances(iblock)%set_interior_forcing()

As with :ref:`ref-compute-surface-fluxes`, the details are in the surrounding calls.

------------------------------------------------------
What MARBL needs prior to calling set_interior_forcing
------------------------------------------------------

The GCM needs to make sure the MARBL instance has all the data it needs to compute interior tendencies correctly.
Specifically it needs to to the following.

~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Set global scalars
~~~~~~~~~~~~~~~~~~~~~~~~~~

If MARBL is configured with ``ladjust_bury_coeff = .true.`` then it will request running means of global averages of a few fields.

.. code-block:: fortran

       call marbl_instances(iblock)%set_global_scalars('interior')

Note that at this point, MARBL is responsible for both the global averaging and keeping the running means; in the future running means will be computed in MARBL (and requested as part of saved state).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Copy data into MARBL
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Interior tracer tendencies are computed for a single column in MARBL.
For each column, MARBL needs to know the following:

#. domain information (including ``kmt``, the index of the level containing the ocean bottom)
#. Interior forcing data
#. Tracer values (for each level and each tracer)
#. Saved state

.. code-block:: fortran

  ! --- set marbl_domain kmt and if partial bottom cells then also delta_z ---

  marbl_instances(bid)%domain%kmt = KMT(i, c, bid)
  if (partial_bottom_cells) then
     marbl_instances(bid)%domain%delta_z(:) = DZT(i, c, :, bid)
  end if

  ! --- set forcing fields ---

  do n = 1, size(interior_forcing_fields)
    if (interior_forcing_fields(n)%rank == 2) then
      marbl_instances(bid)%interior_input_forcings(n)%field_0d(1) = &
           interior_forcing_fields(n)%field_0d(i,c,bid)
    else
      marbl_instances(bid)%interior_input_forcings(n)%field_1d(1,:) = &
           interior_forcing_fields(n)%field_1d(i,c,:,bid)
    end if
  end do

  ! --- set column tracers, averaging 2 time levels into 1 ---

  do n = 1, ecosys_tracer_cnt
     marbl_instances(bid)%column_tracers(n, :) = p5*(tracer_module_old(i, c, :, n) + tracer_module_cur(i, c, :, n))
  end do

  ! --- copy data from slab to column for marbl_saved_state ---
  do n=1,size(saved_state_interior)
    marbl_instances(bid)%interior_saved_state%state(n)%field_3d(:,1) = &
      saved_state_interior(n)%field_3d(:,i,c,bid)
  end do

--------------------------------------
What the GCM needs after MARBL returns
--------------------------------------

MARBL returns tracer tendencies on a per-column basis, and that needs to be stored in the GCM.
Additionally, saved state needs to be saved so it is available in the next time step and any fields that are globally averaged also need to be stored.

.. code-block:: fortran

             do n=1,size(saved_state_interior)
               saved_state_interior(n)%field_3d(:,i,c,bid) =               &
                 marbl_instances(bid)%interior_saved_state%state(n)%field_3d(:,1)
             end do

             do n = 1, ecosys_tracer_cnt
                dtracer_module(i, c, :, n) = marbl_instances(bid)%column_dtracers(n, :)
             end do

             ! copy values to be used in computing requested global averages
             ! arrays have zero extent if none are requested
             glo_avg_fields_interior(i, c, bid, :) = marbl_instances(bid)%glo_avg_fields_interior(:)
