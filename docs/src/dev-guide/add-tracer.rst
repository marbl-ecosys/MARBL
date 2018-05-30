.. _add-tracer:

===============
Adding a Tracer
===============

The steps needed to add a new tracer depend greatly on what the tracer is, so this page will not use a single tracer as an example.
Also, a significant portion of the code shown in these examples will be cleaned up prior to the MARBL 1.0.0 release (sorry!).

This is a 7 step process.

------------------
MARBL Code Changes
------------------

The bulk of the changes are made in MARBL, but the GCM will need to provide initial conditions for the new tracer.
In practice, this likely requires creating a new initial conditions file for all the tracers, and then updating the MARBL driver in the GCM to provide the proper values for the new tracer.
This page focuses on the changes to MARBL.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Update the Tracer Count
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The tracer count is currently a build-time parameter in ``marbl_sizes.F90``

.. code-block:: fortran

  !-----------------------------------------------------------------------------
  ! number of ecosystem tracers
  !-----------------------------------------------------------------------------

  integer(int_kind), parameter :: ecosys_base_tracer_cnt = ECOSYS_NT
  integer(int_kind), parameter :: ciso_tracer_cnt = 14
  integer(int_kind)            :: marbl_total_tracer_cnt = 0
  integer(int_kind)            :: tracer_restore_cnt = 0

If you are adding a tracer to the base tracer module, ``ECOSYS_NT`` is set in the ``Makefile`` (both ``src/`` and ``tests/driver_src``). If you are adding a carbon isotope tracer, you can change ``ciso_tracer_cnt`` in-line.

.. note::
  We are in the process of updating MARBL's build system and the end result will be that ``ECOSYS_NT`` and ``CISO_NT`` will both be computed based on the number of non-living tracers, autotrophs, zooplankton, and grazers.
  Adding a per-autotroph tracer is out of the scope of this document, but in the release the steps on this page will assume you are adding a non-living tracer to the base module.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Add to MARBL tracer index type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As mentioned in :ref:`ref-add-diag`, the ``indexing_type`` is a common structure in MARBL.
Due to the many ways to introduce tracers (different modules, living tracers, etc), the tracer indexing type is a little more complex than others.

.. code-block:: fortran

  type, public :: marbl_tracer_index_type
    ! Index ranges
    integer (int_kind) :: ecosys_base_ind_beg
    integer (int_kind) :: ecosys_base_ind_end
    integer (int_kind) :: ciso_ind_beg
    integer (int_kind) :: ciso_ind_end

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
    type(marbl_living_tracer_index_type), dimension(autotroph_cnt)   :: auto_inds
    .
    .
    .
  contains
    procedure, public :: construct => tracer_index_constructor
  end type marbl_tracer_index_type

For this example, assume we are adding a single tracer in the base ecosys module (regretfully referred to as "general tracers" in most comments).

.. note::
  This data type does not conform to MARBL naming conventions and will be renamed ``marbl_tracer_indexing_type`` in a future update.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 3. Update ``tracer_index_constructor``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are adding a tracer that is only active in certain configurations, you would include an if statement around the following code.
At this point in time, all the base ecosystem tracers are present in all configurations, so there is no such restriction.
For example, here we set in index for the refractory DOC tracer:

.. code-block:: fortran

  subroutine tracer_index_constructor(this, ciso_on, autotrophs_config,       &
             zooplankton_config)
  .
  .
  .
      ! General ecosys tracers
  .
  .
  .
      tracer_cnt    = tracer_cnt + 1
      this%docr_ind = tracer_cnt
  .
  .
  .
  end subroutine tracer_index_constructor

.. note::
  There is an `issue ticket <https://github.com/marbl-ecosys/MARBL/issues/124>`_ to refer to objects as ``self`` instead of ``this``.
  :ref:`ref-OO-examples` has it right.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 4. Set tracer metadata
~~~~~~~~~~~~~~~~~~~~~~~~~~~

MARBL provides the following metadata to describe each tracer:

.. code-block:: fortran

  type, public :: marbl_tracer_metadata_type
     character(char_len) :: short_name
     character(char_len) :: long_name
     character(char_len) :: units
     character(char_len) :: tend_units
     character(char_len) :: flux_units
     logical             :: lfull_depth_tavg
     character(char_len) :: tracer_module_name
  end type marbl_tracer_metadata_type

There are a few different subroutines in ``marbl_mod.F90`` to define the metadata for different classes of tracers.
(Metadata for carbon isotope tracers is handled in ``marbl_ciso_mod::marbl_ciso_init_tracer_metadata``.)

.. code-block:: fortran

  private :: marbl_init_non_autotroph_tracer_metadata
  private :: marbl_init_zooplankton_tracer_metadata
  private :: marbl_init_autotroph_tracer_metadata

The three subroutines above are called from ``marbl_init_tracer_metadata()``.
Prior to those calls, two attributes in the metadata type are set.

.. code-block:: fortran

    marbl_tracer_metadata(:)%lfull_depth_tavg   = .true.
    marbl_tracer_metadata(:)%tracer_module_name = 'ecosys'

Metadata for all base ecosystem non-living tracers is set in ``marbl_init_non_autotroph_tracer_metadata()``.
For example, here is where the dissolved inorganic phosphate index is set:

.. code-block:: fortran

  subroutine marbl_init_non_autotroph_tracer_metadata(marbl_tracer_metadata, &
             marbl_tracer_indices, non_living_biomass_ecosys_tracer_cnt)
  .
  .
  .
    associate(&
         po4_ind           => marbl_tracer_indices%po4_ind,         &
    .
    .
    .
             )
    .
    .
    .
    marbl_tracer_metadata(po4_ind)%short_name='PO4'
    marbl_tracer_metadata(po4_ind)%long_name='Dissolved Inorganic Phosphate'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1
    .
    .
    .
    do n = 1, non_living_biomass_ecosys_tracer_cnt
       if (n == alk_ind) then
          marbl_tracer_metadata(n)%units      = 'meq/m^3'
          marbl_tracer_metadata(n)%tend_units = 'meq/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'meq/m^3 cm/s'
       else
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif
    end do

    end associate

  end subroutine marbl_init_non_autotroph_tracer_metadata

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 5. Compute surface flux for new tracer (if necessary)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Not all tracers return a surface flux, so this may not be necessary for your tracer.
For this example, we will follow the oxygen tracer.
Surface fluxes are computed in ``marbl_mod::marbl_set_surface_forcing``:

.. code-block:: fortran

  subroutine marbl_set_surface_forcing( &
  .
  .
  .
    associate(                                                                                      &
    .
    .
    .
         stf                  => surface_tracer_fluxes(:,:),                                        &
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

    stf(:, :) = c0
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
          where (surface_mask(:) /= c0)
             pv_o2(:) = xkw_ice(:) * sqrt(660.0_r8 / schmidt_o2(:))
             o2sat(:) = ap_used(:) * o2sat_1atm(:)
             flux_o2_loc(:) = pv_o2(:) * (o2sat(:) - surface_vals(:, o2_ind))
             stf(:, o2_ind) = stf(:, o2_ind) + flux_o2_loc(:)

.. note::
  This subroutine will be renamed ``marbl_compute_surface_fluxes`` to better reflect what the code is doing.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 6. Compute tracer tendency
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The tracer tendencies are computed in a two step process - MARBL computes the tracer tendency terms from a variety of processes and then combines the terms in the end.
Given the modular nature of MARBL, the tendencies from each process are computed in their own routine.
This is done in ``marbl_mod::set_interior_forcing``:

.. code-block:: fortran

  subroutine marbl_set_interior_forcing( &
  .
  .
  .
    call marbl_compute_PAR(domain, interior_forcings, interior_forcing_indices, &
                           autotroph_cnt, autotroph_local, PAR)
    .
    .
    .
    do k = 1, km
    .
    .
    .
       call marbl_compute_autotroph_uptake(autotroph_cnt, autotrophs_config,  &
            autotrophs, tracer_local(:, k), marbl_tracer_indices,             &
            autotroph_secondary_species(:, k))
    .
    .
    .
       call marbl_compute_denitrif(tracer_local(o2_ind, k), tracer_local(no3_ind, k), &
            dissolved_organic_matter(k)%DOC_remin, &
            dissolved_organic_matter(k)%DOCr_remin, &
            POC%remin(k), other_remin(k), sed_denitrif(k), denitrif(k))
    .
    .
    .
       call marbl_compute_dtracer_local (autotroph_cnt, zooplankton_cnt,      &
            autotrophs_config, autotrophs, zooplankton, &
            autotroph_secondary_species(:, k), &
            zooplankton_secondary_species(:, k), &
            dissolved_organic_matter(k), &
            nitrif(k), denitrif(k), sed_denitrif(k), &
            Fe_scavenge(k) , Fe_scavenge_rate(k), &
            P_iron%remin(k), POC%remin(k), &
            P_SiO2%remin(k), P_CaCO3%remin(k), other_remin(k), &
            PON_remin(k), POP_remin(k), &
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

  subroutine marbl_compute_dtracer_local (auto_cnt, zoo_cnt, auto_config,       &
             auto_meta, zoo_meta, autotroph_secondary_species,                  &
             zooplankton_secondary_species, dissolved_organic_matter,           &
             nitrif, denitrif, sed_denitrif, Fe_scavenge, Fe_scavenge_rate,     &
             P_iron_remin, POC_remin, P_SiO2_remin, P_CaCO3_remin, other_remin, &
             PON_remin, POP_remin, interior_restore, O2_loc, o2_production,     &
             o2_consumption, dtracers, marbl_tracer_indices)
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
  #. This subroutine will be renamed ``marbl_compute_interior_tendencies`` to better reflect what the code is doing.
  #. The ``k`` loop in the example may be removed in favor of doing per-process computations on an entire column at once.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 7. Add any necessary diagnostics
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

