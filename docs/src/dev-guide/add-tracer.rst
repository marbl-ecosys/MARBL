.. _add-tracer:

===============
Adding a Tracer
===============

The steps needed to add a new tracer depend greatly on what the tracer is, so this page will not use a single tracer as an example.
Also, a significant portion of the code shown in these examples will be cleaned up prior to the MARBL 1.0.0 release (sorry!).

------------------
MARBL Code Changes
------------------

This is an eight step process.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Add to MARBL tracer index type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As mentioned in :ref:`ref-add-diag`, the ``indexing_type`` is a common structure in MARBL.
Due to the many ways to introduce tracers (different modules, living tracers, etc), the tracer indexing type is a little more complex than others.

.. code-block:: fortran

  type, public :: marbl_tracer_index_type
    ! Book-keeping (tracer count and index ranges)
    integer (int_kind) :: total_cnt = 0
    type (marbl_tracer_count_type) :: ecosys_base
    type (marbl_tracer_count_type) :: ciso

    ! General tracers
    integer (int_kind) :: po4_ind         = 0 ! dissolved inorganic phosphate
    .
    .
    .
    ! CISO tracers
    integer (int_kind) :: di13c_ind       = 0 ! dissolved inorganic carbon 13
    .
    .
    .
    ! Living tracers
    type(marbl_living_tracer_index_type), allocatable :: auto_inds(:)
    .
    .
    .
  contains
    procedure, public :: add_tracer_index
    procedure, public :: construct => tracer_index_constructor
    procedure, public :: destruct => tracer_index_destructor
  end type marbl_tracer_index_type

For this example, assume we are adding a single tracer in the base ecosys module (regretfully referred to as "general tracers" in most comments).

.. note::
  This data type does not conform to MARBL naming conventions and will be renamed ``marbl_tracer_indexing_type`` in a future update.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Update ``tracer_index_constructor``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are adding a tracer that is only active in certain configurations, you would include an if statement around the following code.
At this point in time, all the base ecosystem tracers are present in all configurations, so there is no such restriction.
For example, here we set in index for the refractory DOC tracer:

.. code-block:: fortran

  subroutine tracer_index_constructor(this, ciso_on, lvariable_PtoC, autotrophs, &
             zooplankton, marbl_status_log)
  .
  .
  .
      ! General ecosys tracers
  .
  .
  .
      call this%add_tracer_index('docr', 'ecosys_base', this%docr_ind, marbl_status_log)
  .
  .
  .
  end subroutine tracer_index_constructor

.. note::
  There is an `issue ticket <https://github.com/marbl-ecosys/MARBL/issues/124>`_ to refer to objects as ``self`` instead of ``this``.
  :ref:`ref-OO-examples` has it right.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 3. Set tracer metadata
~~~~~~~~~~~~~~~~~~~~~~~~~~~

MARBL provides the following metadata to describe each tracer:

.. code-block:: fortran

  type, public :: marbl_tracer_metadata_type
     character(len=char_len) :: short_name
     character(len=char_len) :: long_name
     character(len=char_len) :: units
     character(len=char_len) :: tend_units
     character(len=char_len) :: flux_units
     logical                 :: lfull_depth_tavg
     character(len=char_len) :: tracer_module_name
  end type marbl_tracer_metadata_type

There are a few different subroutines in ``marbl_init_mod.F90`` to define the metadata for different classes of tracers.
(Metadata for carbon isotope tracers is handled in ``marbl_ciso_mod::marbl_ciso_init_tracer_metadata``.)

.. code-block:: fortran

  private :: marbl_init_non_autotroph_tracer_metadata
  private :: marbl_init_non_autotroph_tracers_metadata
  private :: marbl_init_zooplankton_tracer_metadata
  private :: marbl_init_autotroph_tracer_metadata

The last three subroutines above are called from ``marbl_init_tracer_metadata()``, and ``marbl_init_non_autotroph_tracer_metadata()`` is called from ``marbl_init_non_autotroph_tracers_metadata()``
Prior to those calls, ``marbl_init_tracer_metadata()`` sets two attributes in the metadata type:

.. code-block:: fortran

    marbl_tracer_metadata(:)%lfull_depth_tavg   = .true.
    marbl_tracer_metadata(:)%tracer_module_name = 'ecosys'

Metadata for all base ecosystem non-living tracers is set in ``marbl_init_non_autotroph_tracers_metadata()``.
For example, here is where the dissolved inorganic phosphate index is set:

.. code-block:: fortran

  subroutine marbl_init_non_autotroph_tracers_metadata(marbl_tracer_metadata, &
             marbl_tracer_indices)
    .
    .
    .
    call marbl_init_non_autotroph_tracer_metadata('PO4', 'Dissolved Inorganic Phosphate', &
               marbl_tracer_metadata(marbl_tracer_indices%po4_ind))

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 4. Compute surface flux for new tracer (if necessary)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Not all tracers return a surface flux, so this may not be necessary for your tracer.
For this example, we will follow the oxygen tracer.
Surface fluxes are computed in ``marbl_surface_flux_mod::marbl_surface_flux_compute``:

.. code-block:: fortran

  subroutine marbl_surface_flux_compute( &
  .
  .
  .
    associate(                                                                                      &
    .
    .
    .
         o2_ind            => marbl_tracer_indices%o2_ind,                                      &
         .
         .
         .
         )

    !-----------------------------------------------------------------------
    !  fluxes initially set to 0
    !-----------------------------------------------------------------------

    surface_fluxes(:, :) = c0
    .
    .
    .
    !-----------------------------------------------------------------------
    !  compute CO2 flux, computing disequilibrium one row at a time
    !-----------------------------------------------------------------------

    if (lflux_gas_o2 .or. lflux_gas_co2) then
       .
       .
       .
       if (lflux_gas_o2) then
       .
       .
       .
       pv_o2(:) = xkw_ice(:) * sqrt(660.0_r8 / schmidt_o2(:))
       o2sat(:) = ap_used(:) * o2sat_1atm(:)
       flux_o2_loc(:) = pv_o2(:) * (o2sat(:) - surface_vals(:, o2_ind))
       surface_fluxes(:, o2_ind) = surface_fluxes(:, o2_ind) + flux_o2_loc(:)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 5. Compute tracer tendency
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The tracer tendencies are computed in a two step process - MARBL computes the tracer tendency terms from a variety of processes and then combines the terms in the end.
Given the modular nature of MARBL, the tendencies from each process are computed in their own routine.
This is done in ``marbl_interior_tendency_mod::interior_tendency_compute``:

.. code-block:: fortran

  subroutine marbl_interior_tendency_compute( &
  .
  .
  .
    call marbl_compute_PAR(domain, interior_forcings, interior_forcing_indices, &
                           autotroph_cnt, totalChl_local, PAR)

    do k = 1, km
    .
    .
    .
       call marbl_compute_autotroph_uptake(autotroph_cnt, autotrophs,  &
            tracer_local(:, k), marbl_tracer_indices,                  &
            autotroph_secondary_species(:, k))
    .
    .
    .
       call marbl_compute_denitrif(tracer_local(o2_ind, k), tracer_local(no3_ind, k), &
            dissolved_organic_matter(k)%DOC_remin, &
            dissolved_organic_matter(k)%DOCr_remin, &
            POC%remin(k), other_remin(k), sed_denitrif(k), denitrif(k))

       call marbl_compute_dtracer_local (autotroph_cnt, zooplankton_cnt,      &
            autotrophs, zooplankton,           &
            autotroph_secondary_species(:, k), &
            zooplankton_secondary_species(:, k), &
            dissolved_organic_matter(k), &
            nitrif(k), denitrif(k), sed_denitrif(k), &
            Fe_scavenge(k), Lig_prod(k), Lig_loss(k), &
            P_iron%remin(k), POC%remin(k), POP%remin(k), &
            P_SiO2%remin(k), P_CaCO3%remin(k), P_CaCO3_ALT_CO2%remin(k), &
            other_remin(k), PON_remin(k), &
            interior_restore(:, k), &
            tracer_local(o2_ind, k), &
            o2_production(k), o2_consumption(k), &
            dtracers(:, k), marbl_tracer_indices )
    .
    .
    .
    end do

The tendencies are combined in ``marbl_compute_dtracer_local`` while subroutines like ``marbl_compute_PAR``, ``marbl_compute_autotroph_uptake``, and ``marbl_compute_denitrif`` are the per-process computations.
So you will need to update ``marbl_compute_dtracer_local`` to compute the tracer tendency for your new tracer correctly:

.. code-block:: fortran

  subroutine marbl_compute_dtracer_local (auto_cnt, zoo_cnt, autotrophs,       &
             zooplankton, autotroph_secondary_species,                  &
             zooplankton_secondary_species, dissolved_organic_matter,           &
             nitrif, denitrif, sed_denitrif, Fe_scavenge, Lig_prod, Lig_loss,   &
             P_iron_remin, POC_remin, POP_remin, P_SiO2_remin, P_CaCO3_remin,   &
             P_CaCO3_ALT_CO2_remin, other_remin, PON_remin, interior_restore,   &
             O2_loc, o2_production, o2_consumption, dtracers, marbl_tracer_indices)
  .
  .
  .
    associate(                                                            &
    .
    .
    .
         o2_ind            => marbl_tracer_indices%o2_ind,          &
         .
         .
         .
    )
    .
    .
    .
    o2_consumption = (O2_loc - parm_o2_min) / parm_o2_min_delta
    o2_consumption = min(max(o2_consumption, c0), c1)
    o2_consumption = o2_consumption * ( (POC_remin * (c1 - POCremin_refract) + DOC_remin &
         + DOCr_remin - (sed_denitrif * denitrif_C_N) - other_remin + sum(zoo_loss_dic(:)) &
         + sum(zoo_graze_dic(:)) + sum(auto_loss_dic(:)) + sum(auto_graze_dic(:)) ) &
         / parm_Remin_D_C_O2 + (c2 * nitrif))

    dtracers(o2_ind) = o2_production - o2_consumption

.. note::
  The ``k`` loop in the example may be removed in favor of doing per-process computations on an entire column at once.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 6. Add any necessary diagnostics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, MARBL's diagnostics include the interior restoring tendency for each tracer.
Otherwise, it is assumed that the GCM will provide tracer diagnostics itself.
MARBL does compute the vertical integral of the conservative terms in the source-sink computation of many tracers.
If your tracer affects these integrals, you should update the appropriate subroutine in ``marbl_diagnostics_mod.F90``:

.. code-block:: fortran

  private :: store_diagnostics_carbon_fluxes
  private :: store_diagnostics_nitrogen_fluxes
  private :: store_diagnostics_phosphorus_fluxes
  private :: store_diagnostics_silicon_fluxes
  private :: store_diagnostics_iron_fluxes

If you want to provide a specific diagnostic related to your tracer, see :ref:`add-diagnostic`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 7. Update the settings YAML files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``defaults/settings_*.yaml`` files also contain a list of all defined tracers.
On the ``development`` branch, make changes to ``defaults/settings_latest.yaml``.
Release branches may only offer specific versions of this file, such as ``defaults/settings_cesm2.1.yaml``.
The block of code defining the tracers looks like this:

.. code-block:: yaml

  # ABOUT THIS FILE
  # ---------------
  .
  .
  .
  # Tracer count
  _tracer_list :
     # Non-living tracers
     PO4 :
        long_name : Dissolved Inorganic Phosphate
        units : mmol/m^3
     NO3 :
        long_name : Dissolved Inorganic Nitrate
        units : mmol/m^3
  .
  .
  .

This list needed because some parameters (such as ``tracer_restore_vars(:)``) depend on the tracer count.
Additionally, it makes it easy for GCMs to see a list of all tracers being returned by MARBL to help configure diagnostic output.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 8. Convert the YAML file to JSON
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We prefer editing YAML files to editing JSON files because they are much easier to maintain (and allow user comments).
Unfortunately, python does not include a YAML parser in the default distributions.
Rather than require all users to install ``pyYAML``, we require that of MARBL developers and then ask them to convert the YAML files to JSON.
The ``MARBL_tools/yaml_to_json.py`` script is provided to do just that:

.. code-block:: none

  $ cd MARBL_tools
  $ ./yaml_to_json.py

There is not a tracer-specific python script to run, but the ``MARBL_settings_class`` has ``get_tracer_names()`` and ``get_tracer_cnt()`` routines.

----------------
GCM Code Changes
----------------

The GCM will need to provide initial conditions for this new tracer, and may also need to output additional tracer-specific diagnostics.
The MARBL guide is not able to offer guidance on how to do that, as it will vary from GCM to GCM.
