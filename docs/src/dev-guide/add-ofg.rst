.. _add-ofg:

============================
Adding an Output for the GCM
============================

In addition to surface fluxes and interior tendencies, MARBL can provide :ref:`additional output <additional_output>` as well.
These might be fluxes that feed back to the atmosphere, or tracer sums used for other physical properties.
Adding a new output is a straightforward process.

-------------------------------------------------
Step 1. Add to MARBL output for GCM indexing type
-------------------------------------------------

MARBL tracks the internal index of each output for the GCM in the ``output_for_GCM_type`` components of the interface
(``surface_flux_output`` and ``interior_tendency_output``).
There is a module variable in ``marbl_interface_public_types.F90`` of type ``marbl_output_for_GCM_indexing_type``:

.. code block from marbl_interface_public_types
.. code-block:: fortran

  type, public :: marbl_output_for_GCM_indexing_type
    integer(int_kind) :: flux_o2_id = 0
    integer(int_kind) :: flux_co2_id = 0
    integer(int_kind) :: flux_nhx_id = 0
    integer(int_kind) :: total_surfChl_id = 0
    integer(int_kind) :: total_Chl_id = 0
  end type marbl_output_for_GCM_indexing_type
  type(marbl_output_for_GCM_indexing_type), target, public :: ofg_ind

``ofg_ind`` is a target because there is a registry of all defined outputs for the GCM that use pointers to
make sure the correct index is updated.
Any new outputs for the GCM must have an index added to this indexing type.

---------------------------------------------------
Step 2. Add to MARBL output for GCM output registry
---------------------------------------------------

MARBL tracks what outputs are available via the ``marbl_output_for_GCM_registry_type``.
When a GCM calls ``marbl_instance%add_output_for_GCM()``,
MARBL checks the registry to see if it can provide the output.

.. code block from marbl_interface_public_types
.. code-block:: fortran

  type, public :: marbl_output_for_GCM_linked_list_type
    character(len=char_len)    :: short_name
    character(len=char_len)    :: long_name
    character(len=char_len)    :: units
    character(len=char_len)    :: field_source
    character(len=char_len)    :: err_message
    integer(int_kind), pointer :: id
    type(marbl_output_for_GCM_linked_list_type), pointer :: next => NULL()
  end type marbl_output_for_GCM_linked_list_type

  !*****************************************************************************

  type, public :: marbl_output_for_GCM_registry_type
    type(marbl_output_for_GCM_linked_list_type), pointer :: registered_outputs
  contains
    procedure, public  :: create_registry
    procedure, private :: add_registry_entry
  end type marbl_output_for_GCM_registry_type

``create_registry()`` is called during MARBL initialization,
and it makes repeated use of the ``add_registry_entry()`` subroutine.
To add a new output to the registry, copy an existing block and modify appropriately:

.. code block from marbl_interface_public_types
.. code-block:: fortran

    ! Surface Chlorophyll
    err_message = ""
    if (.not. base_bio_on) &
      err_message = "requires base biotic tracers"
    call this%add_registry_entry(short_name = "total_surfChl", &
                                 long_name = "Total Surface Chlorophyll Concentration", &
                                 units = "mg/m^3", &
                                 field_source = "surface_flux", &
                                 id = ofg_ind%total_surfChl_id, &
                                 err_message = err_message)

    ! Full Depth Chlorophyll
    err_message = ""
    if (.not. base_bio_on) &
      err_message = "requires base biotic tracers"
    call this%add_registry_entry(short_name = "total_Chl", &
                                 long_name = "Total Chlorophyll Concentration", &
                                 units = "mg/m^3", &
                                 field_source = "interior_tendency", &
                                 id = ofg_ind%total_Chl_id, &
                                 err_message = err_message)

* ``field_source`` will tell the GCM whether to look for updated values of this output
  after calls to ``surface_flux_compute()`` or ``interior_tendency_compute()``.
* If ``err_message`` is an empty string, then this output is available for the GCM.
  If MARBL is configured in a way such that this output will not be computed,
  ``err_message`` should give a concise explanation of why it is unavailable.
  MARBL will return the error ``{short_name} {err_message}`` -- for example,
  if ``base_bio_on = .false.`` and a GCM requests ``total_Chl`` then MARBL will abort with the message
  ``total_Chl requires base biotic tracers``.

------------------------------------------------
Step 3. Copy Output into ``output_for_GCM_type``
------------------------------------------------

If ``field_source = "surface_flux"`` you need something like this inside ``surface_flux_compute()``:

.. code block from marbl_surface_flux_mod
.. code-block:: fortran

    !-----------------------------------------------------------------------
    !  Compute surface chlorophyll (if requested by GCM)
    !-----------------------------------------------------------------------

    if (ofg_ind%total_surfChl_id.ne.0) then
      totalChl_loc = c0
      do auto_ind = 1,autotroph_cnt
        totalChl_loc = totalChl_loc +                                         &
          max(c0, tracers_at_surface(:,marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind))
      end do
      surface_flux_output%outputs_for_GCM(ofg_ind%total_surfChl_id)%forcing_field_0d(:) = totalChl_loc
    end if

Otherwise, ``field_source = "interior_tendency"`` and you need something like this inside ``interior_tendency_compute()``:

.. code block from marbl_interior_tendency_mod
.. code-block:: fortran

    !-----------------------------------------------------------------------
    !  Compute Chlorophyll (if requested by GCM)
    !-----------------------------------------------------------------------

    if (ofg_ind%total_Chl_id.ne.0) then
      interior_tendency_output%outputs_for_GCM(ofg_ind%total_Chl_id)%forcing_field_1d(1,:) = c0
      do auto_ind = 1,autotroph_cnt
        interior_tendency_output%outputs_for_GCM(ofg_ind%total_Chl_id)%forcing_field_1d(1,:) = &
            interior_tendency_output%outputs_for_GCM(ofg_ind%total_Chl_id)%forcing_field_1d(1,:) &
            + tracer_local(marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind,:)
      end do
    end if

Note that, in both examples, the output is only computed and stored if the index is positive.
If the GCM did not request this output,
memory will not be allocated to store it and MARBL won't spend time computing it.