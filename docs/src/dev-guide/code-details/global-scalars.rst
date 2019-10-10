.. _global-scalars:

==============
Global Scalars
==============

In some configurations, MARBL needs to know the value of some globally-averaged scalars.
Currently, the only example of such is setting ``ladjust_bury_coeff = .true.`` to allow MARBL to recompute various burial coefficients.
These burial coefficients are used to :ref:`ref-compute-interior-tendencies`.

.. note::

  In these configurations, the GCM must explicitly tell MARBL it can perform global operations :ref:`during initialization <ref-init-interface>` otherwise MARBL will abort.

---------------------------
Subroutine on the Interface
---------------------------

This is the subroutine called prior to calling :ref:`surface_flux_compute() <ref-global-scalars-surface-flux>` or :ref:`interior_tendency_compute() <ref-global-scalars-interior-tend>`.
If ``field_source == 'surface_flux'`` then the subroutine returns without doing anything.

.. block comes from marbl_interface
.. code-block:: fortran

  subroutine set_global_scalars(this, field_source)

    use marbl_interior_tendency_mod, only : marbl_interior_tendency_adjust_bury_coeff

    class(marbl_interface_class), intent(inout) :: this
    character(len=*),             intent(in)    :: field_source ! 'interior_tendency' or 'surface_flux`'

    if (field_source == 'interior_tendency') then
       call marbl_interior_tendency_adjust_bury_coeff(                                    &
            marbl_particulate_share            = this%particulate_share,                  &
            glo_avg_rmean_interior_tendency    = this%glo_avg_rmean_interior_tendency,    &
            glo_avg_rmean_surface_flux         = this%glo_avg_rmean_surface_flux,         &
            glo_scalar_rmean_interior_tendency = this%glo_scalar_rmean_interior_tendency, &
            glo_scalar_interior_tendency       = this%glo_scalar_interior_tendency)
    end if

  end subroutine set_global_scalars

------------------------------------
Global scalars for interior tendency
------------------------------------

If ``field_source == 'interior_tendency'`` then ``marbl_interior_tendency_adjust_bury_coeff()`` is called.
If ``ladjust_bury_coeff == .false.`` then this subroutine returns immediately.
For runs where MARBL is adjusting the burial coefficients, though, values in ``marbl_instance%particulate_share`` as well as some global averages of running means are updated.
Note that there is an assumption that the ``marbl_glo_avg_mod`` values have been :ref:`updated by the GCM <ref-GCM-update-interior-tend>`.

.. block comes from marbl_interior_tendency_mod
.. code-block:: fortran

  subroutine marbl_interior_tendency_adjust_bury_coeff(marbl_particulate_share, &
       glo_avg_rmean_interior_tendency, glo_avg_rmean_surface_flux, &
       glo_scalar_rmean_interior_tendency, glo_scalar_interior_tendency)

    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_CaCO3_bury
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_POC_bury
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_POP_bury
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_bSi_bury
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff
    use marbl_glo_avg_mod, only : glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_C_input
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_P_input
    use marbl_glo_avg_mod, only : glo_avg_field_ind_surface_flux_Si_input
    use marbl_glo_avg_mod, only : glo_scalar_ind_interior_tendency_POC_bury_coeff
    use marbl_glo_avg_mod, only : glo_scalar_ind_interior_tendency_POP_bury_coeff
    use marbl_glo_avg_mod, only : glo_scalar_ind_interior_tendency_bSi_bury_coeff

    type (marbl_particulate_share_type), intent(inout) :: marbl_particulate_share
    type (marbl_running_mean_0d_type)  , intent(in)    :: glo_avg_rmean_interior_tendency(:)
    type (marbl_running_mean_0d_type)  , intent(in)    :: glo_avg_rmean_surface_flux(:)
    type (marbl_running_mean_0d_type)  , intent(in)    :: glo_scalar_rmean_interior_tendency(:)
    real (r8)                          , intent(inout) :: glo_scalar_interior_tendency(:)

    !-----------------------------------------------------------------------

    if (.not. ladjust_bury_coeff) return

    associate( &
         POC_bury_coeff => marbl_particulate_share%POC_bury_coeff, &
         POP_bury_coeff => marbl_particulate_share%POP_bury_coeff, &
         bSi_bury_coeff => marbl_particulate_share%bSi_bury_coeff, &

         rmean_CaCO3_bury_avg => glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_CaCO3_bury)%rmean, &
         rmean_POC_bury_avg   => glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_POC_bury)%rmean, &
         rmean_POP_bury_avg   => glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_POP_bury)%rmean, &
         rmean_bSi_bury_avg   => glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_bSi_bury)%rmean, &

         rmean_POC_bury_deriv_avg => &
              glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff)%rmean, &
         rmean_POP_bury_deriv_avg => &
              glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff)%rmean, &
         rmean_bSi_bury_deriv_avg => &
              glo_avg_rmean_interior_tendency(glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff)%rmean, &

         rmean_C_input_avg  => glo_avg_rmean_surface_flux(glo_avg_field_ind_surface_flux_C_input)%rmean, &
         rmean_P_input_avg  => glo_avg_rmean_surface_flux(glo_avg_field_ind_surface_flux_P_input)%rmean, &
         rmean_Si_input_avg => glo_avg_rmean_surface_flux(glo_avg_field_ind_surface_flux_Si_input)%rmean, &

         rmean_POC_bury_coeff => &
              glo_scalar_rmean_interior_tendency(glo_scalar_ind_interior_tendency_POC_bury_coeff)%rmean, &
         rmean_POP_bury_coeff => &
              glo_scalar_rmean_interior_tendency(glo_scalar_ind_interior_tendency_POP_bury_coeff)%rmean, &
         rmean_bSi_bury_coeff => &
              glo_scalar_rmean_interior_tendency(glo_scalar_ind_interior_tendency_bSi_bury_coeff)%rmean &
    )

      ! Newton's method for POC_bury(coeff) + CaCO3_bury - C_input = 0
      POC_bury_coeff = rmean_POC_bury_coeff &
                     - (rmean_POC_bury_avg + rmean_CaCO3_bury_avg - rmean_C_input_avg) &
                     / rmean_POC_bury_deriv_avg

      ! Newton's method for POP_bury(coeff) - P_input = 0
      POP_bury_coeff = rmean_POP_bury_coeff &
                     - (rmean_POP_bury_avg - rmean_P_input_avg) / rmean_POP_bury_deriv_avg

      ! Newton's method for bSi_bury(coeff) - Si_input = 0
      bSi_bury_coeff = rmean_bSi_bury_coeff &
                     - (rmean_bSi_bury_avg - rmean_Si_input_avg) / rmean_bSi_bury_deriv_avg

      ! copy computed bury coefficients into output argument
      glo_scalar_interior_tendency(glo_scalar_ind_interior_tendency_POC_bury_coeff) = POC_bury_coeff
      glo_scalar_interior_tendency(glo_scalar_ind_interior_tendency_POP_bury_coeff) = POP_bury_coeff
      glo_scalar_interior_tendency(glo_scalar_ind_interior_tendency_bSi_bury_coeff) = bSi_bury_coeff

    end associate

  end subroutine marbl_interior_tendency_adjust_bury_coeff


