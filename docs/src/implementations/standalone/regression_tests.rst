.. _regression_tests:

=========================
Regression Tests in MARBL
=========================

There are eight subdirectories in ``tests/regression_tests``, though only one is truely a regression test.
The other seven subdirectories can be better thought of as examples of specific MARBL functionality,
and indeed may be moved from the ``tests/`` directory to an ``examples/`` directory in a future update.

-----------------------------
``call_compute_subroutines/``
-----------------------------

``call_compute_subroutines/`` is the only regression test provided.
It computes surface fluxes and interior tendencies based on forcing from five columns of the standard POP configuration.
Initial conditions are forcing fields for these columns can be found in
``tests/input_files/initial_conditions/call_compute_subroutines.20190718.nc``.

All the default diagnostic values are output to a netCDF file
The test can be run with one, two, or five MARBL instances, and regardless of instance count the results should
be bit-for-bit identical.
Additionally, there is a baseline provided in ``tests/input_files/baselines/call_compute_subroutines.history.nc``.

The ``MARBL_tools/netcdf_comparison.py`` script can be used to compare netCDF output between instance counts
(use ``--strict exact`` to check for bit-for-bit match) or between a test run and the baseline
(us ``--strict loose`` to account for round-off level changes that may creep in due to hardware differences).
The ``--strict loose`` option sets bounds on the allowable differences between the new run and the baseline --
by default, absolute differences under 10 :sup:`-16` are allowed in variables within 10 :sup:`-16` of 0,
and relative differences under 10 :sup:`-11` are allowed in all other variables.
These different boundaries are necessary because round-off level diffences in variables that are numerically close
to 0 may have relative errors that are O(1).

--------------
Other Examples
--------------

There are five subdirectories that provide information on how MARBL is configured, and three are documented elsewhere on
on this site:

#. ``gen_settings_file/`` generates a :ref:`settings file <marbl_settings.gen>` for a later MARBL run.
#. ``requested_forcings/`` lists the :ref:`forcing fields MARBL needs <forcing_fields>` in a given configuration.
#. ``requested_tracers/`` lists the :ref:`tracer tendencies computed <tracer_state>` in a given configuration.

The stand-alone driver can also report what tracers are being restored
(without :ref:`looking at the rest of the forcing fields <restoring_as_forcing>`)
as well as what diagnostics are being returned to the GCM.

~~~~~~~~~~~~~~~~~~~~~~~~
``requested_restoring/``
~~~~~~~~~~~~~~~~~~~~~~~~

Running ``./requested_restoring.py`` returns a list of tracers to be restored.
By default, it relies on the settings file ``tests/input_files/settings/marbl_with_restore.settings``,
which sets a few elements of ``tracer_restore_vars``.

.. block comes from output of requested_restoring.py
.. code-block:: none

  ----------------------------
  Requested tracers to restore
  ----------------------------

  1. SiO3
  2. NO3
  3. PO4
  4. ALK
  5. ALK_ALT_CO2

If no tracers are to be restored (as per ``./requested_restoring.py -s None``) the output should be

.. block comes from output of requested_restoring.py -s None
.. code-block:: none

  ----------------------------
  Requested tracers to restore
  ----------------------------

  No tracers to restore!

The details are found in ``$MARBL/tests/driver_src/marbl.F90``:

.. block comes from tests/driver_src/marbl.F90
.. code-block:: fortran

  ! Log tracers requested for restoring
  call driver_status_log%log_header('Requested tracers to restore', subname)
  cnt = 0
  do n=1,size(marbl_instances(1)%interior_tendency_forcings)
    varname = marbl_instances(1)%interior_tendency_forcings(n)%metadata%varname
    if (index(varname, 'Restoring Field').gt.0) then
      cnt = cnt + 1
      varname = varname(1:scan(varname,' ')-1)
      write(log_message, "(I0, 2A)") cnt, '. ', trim(varname)
  call driver_status_log%log_noerror(log_message, subname)
    end if
  end do
  if (cnt.eq.0) then
    call driver_status_log%log_noerror('No tracers to restore!', subname)
  end if

The driver looks at metadata for ``interior_tendency_forcings(:)`` and tracks forcings containing
``'Restoring Field'``.

~~~~~~~~~~~~~~~~~~~~
``requested_diags/``
~~~~~~~~~~~~~~~~~~~~

Running ``./requested_diags.py`` returns a list of diagnostics returned from MARBL.
The list is split between surface flux diagnostics and interior tendency diagnostics.

.. Toggle switch from https://stackoverflow.com/a/60394068
.. raw:: html

  <details>
  <summary><a><i>Show / hide output</i></a></summary>

.. block comes from output of requested_diags.py
.. code-block:: none

  ------------------------
  Surface flux diagnostics
  ------------------------

  1. ECOSYS_IFRAC: Ice Fraction for ecosys fluxes (units: fraction)
  2. ECOSYS_XKW: XKW for ecosys fluxes (units: cm/s)
  3. ECOSYS_ATM_PRESS: Atmospheric Pressure for ecosys fluxes (units: atmospheres)
  4. PV_O2: PV_O2 (units: cm/s)
  5. SCHMIDT_O2: O2 Schmidt Number (units: 1)
  6. O2SAT: O2 Saturation (units: mmol/m^3)
  7. CO2STAR: CO2 Star (units: mmol/m^3)
  8. DCO2STAR: D CO2 Star (units: mmol/m^3)
  9. pCO2SURF: surface pCO2 (units: ppmv)
  10. DpCO2: D pCO2 (units: ppmv)
  11. PV_CO2: CO2 Piston Velocity (units: cm/s)
  12. SCHMIDT_CO2: CO2 Schmidt Number (units: 1)
  13. FG_CO2: DIC Surface Gas Flux (units: mmol/m^3 cm/s)
  14. PH: Surface pH (units: 1)
  15. ATM_CO2: Atmospheric CO2 (units: ppmv)
  16. CO2STAR_ALT_CO2: CO2 Star, Alternative CO2 (units: mmol/m^3)
  17. DCO2STAR_ALT_CO2: D CO2 Star, Alternative CO2 (units: mmol/m^3)
  18. pCO2SURF_ALT_CO2: surface pCO2, Alternative CO2 (units: ppmv)
  19. DpCO2_ALT_CO2: D pCO2, Alternative CO2 (units: ppmv)
  20. FG_ALT_CO2: DIC Surface Gas Flux, Alternative CO2 (units: mmol/m^3 cm/s)
  21. PH_ALT_CO2: Surface pH, Alternative CO2 (units: 1)
  22. ATM_ALT_CO2: Atmospheric Alternative CO2 (units: ppmv)
  23. IRON_FLUX: Atmospheric Iron Flux (units: mmol/m^2/s)
  24. DUST_FLUX: Dust Flux (units: g/cm^2/s)
  25. NOx_FLUX: Flux of NOx from Atmosphere (units: nmol/cm^2/s)
  26. NHy_FLUX: Flux of NHy from Atmosphere (units: nmol/cm^2/s)
  27. NHx_SURFACE_EMIS: Emission of NHx to Atmosphere (units: nmol/cm^2/s)

  -----------------------------
  Interior tendency diagnostics
  -----------------------------

  1. zsatcalc: Calcite Saturation Depth (units: cm)
  2. zsatarag: Aragonite Saturation Depth (units: cm)
  3. O2_ZMIN: Vertical Minimum of O2 (units: mmol/m^3)
  4. O2_ZMIN_DEPTH: Depth of Vertical Minimum of O2 (units: cm)
  5. photoC_TOT_zint: Total C Fixation Vertical Integral (units: mmol/m^3 cm/s)
  6. photoC_TOT_zint_100m: Total C Fixation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  7. photoC_NO3_TOT_zint: Total C Fixation from NO3 Vertical Integral (units: mmol/m^3 cm/s)
  8. photoC_NO3_TOT_zint_100m: Total C Fixation from NO3 Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  9. DOC_prod_zint: Vertical Integral of DOC Production (units: mmol/m^3 cm/s)
  10. DOC_prod_zint_100m: Vertical Integral of DOC Production, 0-100m (units: mmol/m^3 cm/s)
  11. DOC_remin_zint: Vertical Integral of DOC Remineralization (units: mmol/m^3 cm/s)
  12. DOC_remin_zint_100m: Vertical Integral of DOC Remineralization, 0-100m (units: mmol/m^3 cm/s)
  13. DOCr_remin_zint: Vertical Integral of DOCr Remineralization (units: mmol/m^3 cm/s)
  14. DOCr_remin_zint_100m: Vertical Integral of DOCr Remineralization, 0-100m (units: mmol/m^3 cm/s)
  15. Jint_Ctot: Vertical Integral of Conservative Subterms of Source Sink Term for Ctot (units: mmol/m^3 cm/s)
  16. Jint_Ntot: Vertical Integral of Conservative Subterms of Source Sink Term for Ntot (units: mmol/m^3 cm/s)
  17. Jint_Ptot: Vertical Integral of Conservative Subterms of Source Sink Term for Ptot (units: mmol/m^3 cm/s)
  18. Jint_Sitot: Vertical Integral of Conservative Subterms of Source Sink Term for Sitot (units: mmol/m^3 cm/s)
  19. Jint_Fetot: Vertical Integral of Conservative Subterms of Source Sink Term for Fetot (units: mmol/m^3 cm/s)
  20. calcToFloor: CaCO3 Flux Hitting Sea Floor (units: nmol/cm^2/s)
  21. calcToSed: CaCO3 Flux to Sediments (units: nmol/cm^2/s)
  22. calcToSed_ALT_CO2: CaCO3 Flux to Sediments, Alternative CO2 (units: nmol/cm^2/s)
  23. pocToFloor: POC Flux Hitting Sea Floor (units: nmol/cm^2/s)
  24. pocToSed: POC Flux to Sediments (units: nmol/cm^2/s)
  25. ponToSed: nitrogen burial Flux to Sediments (units: nmol/cm^2/s)
  26. SedDenitrif: nitrogen loss in Sediments (units: nmol/cm^2/s)
  27. OtherRemin: non-oxic,non-dentr remin in Sediments (units: nmol/cm^2/s)
  28. popToSed: phosphorus Flux to Sediments (units: nmol/cm^2/s)
  29. bsiToSed: biogenic Si Flux to Sediments (units: nmol/cm^2/s)
  30. dustToSed: dust Flux to Sediments (units: g/cm^2/s)
  31. pfeToSed: pFe Flux to Sediments (units: nmol/cm^2/s)
  32. sp_N_lim_surf: Small Phyto N Limitation, Surface (units: 1)
  33. sp_N_lim_Cweight_avg_100m: Small Phyto N Limitation, carbon biomass weighted average over 0-100m (units: 1)
  34. sp_P_lim_surf: Small Phyto P Limitation, Surface (units: 1)
  35. sp_P_lim_Cweight_avg_100m: Small Phyto P Limitation, carbon biomass weighted average over 0-100m (units: 1)
  36. sp_Fe_lim_surf: Small Phyto Fe Limitation, Surface (units: 1)
  37. sp_Fe_lim_Cweight_avg_100m: Small Phyto Fe Limitation, carbon biomass weighted average over 0-100m (units: 1)
  38. sp_light_lim_surf: Small Phyto Light Limitation, Surface (units: 1)
  39. sp_light_lim_Cweight_avg_100m: Small Phyto Light Limitation, carbon biomass weighted average over 0-100m (units: 1)
  40. photoC_sp_zint: Small Phyto C Fixation Vertical Integral (units: mmol/m^3 cm/s)
  41. photoC_sp_zint_100m: Small Phyto C Fixation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  42. photoC_NO3_sp_zint: Small Phyto C Fixation from NO3 Vertical Integral (units: mmol/m^3 cm/s)
  43. sp_CaCO3_form_zint: Small Phyto CaCO3 Formation Vertical Integral (units: mmol/m^3 cm/s)
  44. sp_CaCO3_form_zint_100m: Small Phyto CaCO3 Formation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  45. graze_sp_zint: Small Phyto Grazing Vertical Integral (units: mmol/m^3 cm/s)
  46. graze_sp_zint_100m: Small Phyto Grazing Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  47. graze_sp_poc_zint: Small Phyto Grazing to POC Vertical Integral (units: mmol/m^3 cm/s)
  48. graze_sp_poc_zint_100m: Small Phyto Grazing to POC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  49. graze_sp_doc_zint: Small Phyto Grazing to DOC Vertical Integral (units: mmol/m^3 cm/s)
  50. graze_sp_doc_zint_100m: Small Phyto Grazing to DOC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  51. graze_sp_zoo_zint: Small Phyto Grazing to ZOO Vertical Integral (units: mmol/m^3 cm/s)
  52. graze_sp_zoo_zint_100m: Small Phyto Grazing to ZOO Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  53. sp_loss_zint: Small Phyto Loss Vertical Integral (units: mmol/m^3 cm/s)
  54. sp_loss_zint_100m: Small Phyto Loss Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  55. sp_loss_poc_zint: Small Phyto Loss to POC Vertical Integral (units: mmol/m^3 cm/s)
  56. sp_loss_poc_zint_100m: Small Phyto Loss to POC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  57. sp_loss_doc_zint: Small Phyto Loss to DOC Vertical Integral (units: mmol/m^3 cm/s)
  58. sp_loss_doc_zint_100m: Small Phyto Loss to DOC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  59. sp_agg_zint: Small Phyto Aggregation Vertical Integral (units: mmol/m^3 cm/s)
  60. sp_agg_zint_100m: Small Phyto Aggregation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  61. diat_N_lim_surf: Diatom N Limitation, Surface (units: 1)
  62. diat_N_lim_Cweight_avg_100m: Diatom N Limitation, carbon biomass weighted average over 0-100m (units: 1)
  63. diat_P_lim_surf: Diatom P Limitation, Surface (units: 1)
  64. diat_P_lim_Cweight_avg_100m: Diatom P Limitation, carbon biomass weighted average over 0-100m (units: 1)
  65. diat_Fe_lim_surf: Diatom Fe Limitation, Surface (units: 1)
  66. diat_Fe_lim_Cweight_avg_100m: Diatom Fe Limitation, carbon biomass weighted average over 0-100m (units: 1)
  67. diat_SiO3_lim_surf: Diatom SiO3 Limitation, Surface (units: 1)
  68. diat_SiO3_lim_Cweight_avg_100m: Diatom SiO3 Limitation, carbon biomass weighted average over 0-100m (units: 1)
  69. diat_light_lim_surf: Diatom Light Limitation, Surface (units: 1)
  70. diat_light_lim_Cweight_avg_100m: Diatom Light Limitation, carbon biomass weighted average over 0-100m (units: 1)
  71. photoC_diat_zint: Diatom C Fixation Vertical Integral (units: mmol/m^3 cm/s)
  72. photoC_diat_zint_100m: Diatom C Fixation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  73. photoC_NO3_diat_zint: Diatom C Fixation from NO3 Vertical Integral (units: mmol/m^3 cm/s)
  74. graze_diat_zint: Diatom Grazing Vertical Integral (units: mmol/m^3 cm/s)
  75. graze_diat_zint_100m: Diatom Grazing Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  76. graze_diat_poc_zint: Diatom Grazing to POC Vertical Integral (units: mmol/m^3 cm/s)
  77. graze_diat_poc_zint_100m: Diatom Grazing to POC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  78. graze_diat_doc_zint: Diatom Grazing to DOC Vertical Integral (units: mmol/m^3 cm/s)
  79. graze_diat_doc_zint_100m: Diatom Grazing to DOC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  80. graze_diat_zoo_zint: Diatom Grazing to ZOO Vertical Integral (units: mmol/m^3 cm/s)
  81. graze_diat_zoo_zint_100m: Diatom Grazing to ZOO Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  82. diat_loss_zint: Diatom Loss Vertical Integral (units: mmol/m^3 cm/s)
  83. diat_loss_zint_100m: Diatom Loss Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  84. diat_loss_poc_zint: Diatom Loss to POC Vertical Integral (units: mmol/m^3 cm/s)
  85. diat_loss_poc_zint_100m: Diatom Loss to POC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  86. diat_loss_doc_zint: Diatom Loss to DOC Vertical Integral (units: mmol/m^3 cm/s)
  87. diat_loss_doc_zint_100m: Diatom Loss to DOC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  88. diat_agg_zint: Diatom Aggregation Vertical Integral (units: mmol/m^3 cm/s)
  89. diat_agg_zint_100m: Diatom Aggregation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  90. diaz_N_lim_surf: Diazotroph N Limitation, Surface (units: 1)
  91. diaz_N_lim_Cweight_avg_100m: Diazotroph N Limitation, carbon biomass weighted average over 0-100m (units: 1)
  92. diaz_P_lim_surf: Diazotroph P Limitation, Surface (units: 1)
  93. diaz_P_lim_Cweight_avg_100m: Diazotroph P Limitation, carbon biomass weighted average over 0-100m (units: 1)
  94. diaz_Fe_lim_surf: Diazotroph Fe Limitation, Surface (units: 1)
  95. diaz_Fe_lim_Cweight_avg_100m: Diazotroph Fe Limitation, carbon biomass weighted average over 0-100m (units: 1)
  96. diaz_light_lim_surf: Diazotroph Light Limitation, Surface (units: 1)
  97. diaz_light_lim_Cweight_avg_100m: Diazotroph Light Limitation, carbon biomass weighted average over 0-100m (units: 1)
  98. photoC_diaz_zint: Diazotroph C Fixation Vertical Integral (units: mmol/m^3 cm/s)
  99. photoC_diaz_zint_100m: Diazotroph C Fixation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  100. photoC_NO3_diaz_zint: Diazotroph C Fixation from NO3 Vertical Integral (units: mmol/m^3 cm/s)
  101. graze_diaz_zint: Diazotroph Grazing Vertical Integral (units: mmol/m^3 cm/s)
  102. graze_diaz_zint_100m: Diazotroph Grazing Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  103. graze_diaz_poc_zint: Diazotroph Grazing to POC Vertical Integral (units: mmol/m^3 cm/s)
  104. graze_diaz_poc_zint_100m: Diazotroph Grazing to POC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  105. graze_diaz_doc_zint: Diazotroph Grazing to DOC Vertical Integral (units: mmol/m^3 cm/s)
  106. graze_diaz_doc_zint_100m: Diazotroph Grazing to DOC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  107. graze_diaz_zoo_zint: Diazotroph Grazing to ZOO Vertical Integral (units: mmol/m^3 cm/s)
  108. graze_diaz_zoo_zint_100m: Diazotroph Grazing to ZOO Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  109. diaz_loss_zint: Diazotroph Loss Vertical Integral (units: mmol/m^3 cm/s)
  110. diaz_loss_zint_100m: Diazotroph Loss Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  111. diaz_loss_poc_zint: Diazotroph Loss to POC Vertical Integral (units: mmol/m^3 cm/s)
  112. diaz_loss_poc_zint_100m: Diazotroph Loss to POC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  113. diaz_loss_doc_zint: Diazotroph Loss to DOC Vertical Integral (units: mmol/m^3 cm/s)
  114. diaz_loss_doc_zint_100m: Diazotroph Loss to DOC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  115. diaz_agg_zint: Diazotroph Aggregation Vertical Integral (units: mmol/m^3 cm/s)
  116. diaz_agg_zint_100m: Diazotroph Aggregation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  117. CaCO3_form_zint: Total CaCO3 Formation Vertical Integral (units: mmol/m^3 cm/s)
  118. CaCO3_form_zint_100m: Total CaCO3 Formation Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  119. zoo_loss_zint: Zooplankton Loss Vertical Integral (units: mmol/m^3 cm/s)
  120. zoo_loss_zint_100m: Zooplankton Loss Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  121. zoo_loss_poc_zint: Zooplankton Loss to POC Vertical Integral (units: mmol/m^3 cm/s)
  122. zoo_loss_poc_zint_100m: Zooplankton Loss to POC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  123. zoo_loss_doc_zint: Zooplankton Loss to DOC Vertical Integral (units: mmol/m^3 cm/s)
  124. zoo_loss_doc_zint_100m: Zooplankton Loss to DOC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  125. graze_zoo_zint: Zooplankton Grazing Vertical Integral (units: mmol/m^3 cm/s)
  126. graze_zoo_zint_100m: Zooplankton Grazing Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  127. graze_zoo_poc_zint: Zooplankton Grazing to POC Vertical Integral (units: mmol/m^3 cm/s)
  128. graze_zoo_poc_zint_100m: Zooplankton Grazing to POC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  129. graze_zoo_doc_zint: Zooplankton Grazing to DOC Vertical Integral (units: mmol/m^3 cm/s)
  130. graze_zoo_doc_zint_100m: Zooplankton Grazing to DOC Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  131. graze_zoo_zoo_zint: Zooplankton Grazing to ZOO Vertical Integral (units: mmol/m^3 cm/s)
  132. graze_zoo_zoo_zint_100m: Zooplankton Grazing to ZOO Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  133. x_graze_zoo_zint: Zooplankton Grazing Gain Vertical Integral (units: mmol/m^3 cm/s)
  134. x_graze_zoo_zint_100m: Zooplankton Grazing Gain Vertical Integral, 0-100m (units: mmol/m^3 cm/s)
  135. insitu_temp: in situ temperature (units: degC)
  136. CO3: Carbonate Ion Concentration (units: mmol/m^3)
  137. HCO3: Bicarbonate Ion Concentration (units: mmol/m^3)
  138. H2CO3: Carbonic Acid Concentration (units: mmol/m^3)
  139. pH_3D: pH (units: 1)
  140. CO3_ALT_CO2: Carbonate Ion Concentration, Alternative CO2 (units: mmol/m^3)
  141. HCO3_ALT_CO2: Bicarbonate Ion Concentration, Alternative CO2 (units: mmol/m^3)
  142. H2CO3_ALT_CO2: Carbonic Acid Concentration, Alternative CO2 (units: mmol/m^3)
  143. pH_3D_ALT_CO2: pH, Alternative CO2 (units: 1)
  144. co3_sat_calc: CO3 concentration at calcite saturation (units: mmol/m^3)
  145. co3_sat_arag: CO3 concentration at aragonite saturation (units: mmol/m^3)
  146. NITRIF: Nitrification (units: mmol/m^3/s)
  147. DENITRIF: Denitrification (units: mmol/m^3/s)
  148. O2_PRODUCTION: O2 Production (units: mmol/m^3/s)
  149. O2_CONSUMPTION: O2 Consumption (units: mmol/m^3/s)
  150. AOU: Apparent O2 Utilization (units: mmol/m^3)
  151. PAR_avg: PAR Average over Model Cell (units: W/m^2)
  152. graze_auto_TOT: Total Autotroph Grazing (units: mmol/m^3/s)
  153. photoC_TOT: Total C Fixation (units: mmol/m^3/s)
  154. photoC_NO3_TOT: Total C Fixation from NO3 (units: mmol/m^3/s)
  155. DOC_prod: DOC Production (units: mmol/m^3/s)
  156. DOC_remin: DOC Remineralization (units: mmol/m^3/s)
  157. DOCr_remin: DOCr Remineralization (units: mmol/m^3/s)
  158. DON_prod: DON Production (units: mmol/m^3/s)
  159. DON_remin: DON Remineralization (units: mmol/m^3/s)
  160. DONr_remin: DONr Remineralization (units: mmol/m^3/s)
  161. DOP_prod: DOP Production (units: mmol/m^3/s)
  162. DOP_remin: DOP Remineralization (units: mmol/m^3/s)
  163. DOPr_remin: DOPr Remineralization (units: mmol/m^3/s)
  164. DOP_loss_P_bal: DOP loss, due to P budget balancing (units: mmol/m^3/s)
  165. Fe_scavenge: Iron Scavenging (units: mmol/m^3/s)
  166. Fe_scavenge_rate: Iron Scavenging Rate (units: 1/y)
  167. Lig_prod: Production of Fe-binding Ligand (units: mmol/m^3/s)
  168. Lig_loss: Loss of Fe-binding Ligand (units: mmol/m^3/s)
  169. Lig_scavenge: Loss of Fe-binding Ligand from Scavenging (units: mmol/m^3/s)
  170. Fefree: Fe not bound to Ligand (units: mmol/m^3)
  171. Lig_photochem: Loss of Fe-binding Ligand from UV radiation (units: mmol/m^3/s)
  172. Lig_deg: Loss of Fe-binding Ligand from Bacterial Degradation (units: mmol/m^3/s)
  173. FESEDFLUX: Iron Sediment Flux (units: nmol/cm^2/s)
  174. POC_FLUX_100m: POC Flux at 100m (units: mmol/m^3 cm/s)
  175. POP_FLUX_100m: POP Flux at 100m (units: mmol/m^3 cm/s)
  176. CaCO3_FLUX_100m: CaCO3 Flux at 100m (units: mmol/m^3 cm/s)
  177. SiO2_FLUX_100m: SiO2 Flux at 100m (units: mmol/m^3 cm/s)
  178. P_iron_FLUX_100m: P_iron Flux at 100m (units: mmol/m^3 cm/s)
  179. POC_PROD_zint: Vertical Integral of POC Production (units: mmol/m^3 cm/s)
  180. POC_PROD_zint_100m: Vertical Integral of POC Production, 0-100m (units: mmol/m^3 cm/s)
  181. POC_REMIN_DOCr_zint: Vertical Integral of POC Remineralization routed to DOCr (units: mmol/m^3 cm/s)
  182. POC_REMIN_DOCr_zint_100m: Vertical Integral of POC Remineralization routed to DOCr, 0-100m (units: mmol/m^3 cm/s)
  183. POC_REMIN_DIC_zint: Vertical Integral of POC Remineralization routed to DIC (units: mmol/m^3 cm/s)
  184. POC_REMIN_DIC_zint_100m: Vertical Integral of POC Remineralization routed to DIC, 0-100m (units: mmol/m^3 cm/s)
  185. CaCO3_PROD_zint: Vertical Integral of CaCO3 Production (units: mmol/m^3 cm/s)
  186. CaCO3_PROD_zint_100m: Vertical Integral of CaCO3 Production, 0-100m (units: mmol/m^3 cm/s)
  187. CaCO3_REMIN_zint: Vertical Integral of CaCO3 Remineralization (units: mmol/m^3 cm/s)
  188. CaCO3_REMIN_zint_100m: Vertical Integral of CaCO3 Remineralization, 0-100m (units: mmol/m^3 cm/s)
  189. POC_FLUX_IN: POC Flux into Cell (units: mmol/m^3 cm/s)
  190. POC_sFLUX_IN: POC sFlux into Cell (units: mmol/m^3 cm/s)
  191. POC_hFLUX_IN: POC hFlux into Cell (units: mmol/m^3 cm/s)
  192. POC_PROD: POC Production (units: mmol/m^3/s)
  193. POC_REMIN_DOCr: POC Remineralization routed to DOCr (units: mmol/m^3/s)
  194. POC_REMIN_DIC: POC Remineralization routed to DIC (units: mmol/m^3/s)
  195. POP_FLUX_IN: POP Flux into Cell (units: mmol/m^3 cm/s)
  196. POP_PROD: POP Production (units: mmol/m^3/s)
  197. POP_REMIN_DOPr: POP Remineralization routed to DOPr (units: mmol/m^3/s)
  198. POP_REMIN_PO4: POP Remineralization routed to PO4 (units: mmol/m^3/s)
  199. PON_REMIN_DONr: PON Remineralization routed to DONr (units: mmol/m^3/s)
  200. PON_REMIN_NH4: PON Remineralization routed to NH4 (units: mmol/m^3/s)
  201. CaCO3_FLUX_IN: CaCO3 Flux into Cell (units: mmol/m^3 cm/s)
  202. CaCO3_PROD: CaCO3 Production (units: mmol/m^3/s)
  203. CaCO3_REMIN: CaCO3 Remineralization (units: mmol/m^3/s)
  204. CaCO3_ALT_CO2_FLUX_IN: CaCO3 Flux into Cell, Alternative CO2 (units: mmol/m^3 cm/s)
  205. CaCO3_ALT_CO2_PROD: CaCO3 Production, Alternative CO2 (units: mmol/m^3/s)
  206. CaCO3_ALT_CO2_REMIN: CaCO3 Remineralization, Alternative CO2 (units: mmol/m^3/s)
  207. SiO2_FLUX_IN: SiO2 Flux into Cell (units: mmol/m^3 cm/s)
  208. SiO2_PROD: SiO2 Production (units: mmol/m^3/s)
  209. SiO2_REMIN: SiO2 Remineralization (units: mmol/m^3/s)
  210. dust_FLUX_IN: Dust Flux into Cell (units: g/cm^2/s)
  211. dust_REMIN: Dust Remineralization (units: g/cm^3/s)
  212. P_iron_FLUX_IN: P_iron Flux into Cell (units: mmol/m^3 cm/s)
  213. P_iron_PROD: P_iron Production (units: mmol/m^3/s)
  214. P_iron_REMIN: P_iron Remineralization (units: mmol/m^3/s)
  215. sp_Qp: Small Phyto P:C ratio (units: 1)
  216. photoC_sp: Small Phyto C Fixation (units: mmol/m^3/s)
  217. photoC_NO3_sp: Small Phyto C Fixation from NO3 (units: mmol/m^3/s)
  218. photoFe_sp: Small Phyto Fe Uptake (units: mmol/m^3/s)
  219. photoNO3_sp: Small Phyto NO3 Uptake (units: mmol/m^3/s)
  220. photoNH4_sp: Small Phyto NH4 Uptake (units: mmol/m^3/s)
  221. DOP_sp_uptake: Small Phyto DOP Uptake (units: mmol/m^3/s)
  222. PO4_sp_uptake: Small Phyto PO4 Uptake (units: mmol/m^3/s)
  223. graze_sp: Small Phyto Grazing (units: mmol/m^3/s)
  224. graze_sp_poc: Small Phyto Grazing to POC (units: mmol/m^3/s)
  225. graze_sp_doc: Small Phyto Grazing to DOC (units: mmol/m^3/s)
  226. graze_sp_zootot: Small Phyto Grazing to ZOO TOT (units: mmol/m^3/s)
  227. graze_sp_zoo: Small Phyto Grazing to Zooplankton (units: mmol/m^3/s)
  228. sp_loss: Small Phyto Loss (units: mmol/m^3/s)
  229. sp_loss_poc: Small Phyto Loss to POC (units: mmol/m^3/s)
  230. sp_loss_doc: Small Phyto Loss to DOC (units: mmol/m^3/s)
  231. sp_agg: Small Phyto Aggregation (units: mmol/m^3/s)
  232. sp_CaCO3_form: Small Phyto CaCO3 Formation (units: mmol/m^3/s)
  233. diat_Qp: Diatom P:C ratio (units: 1)
  234. photoC_diat: Diatom C Fixation (units: mmol/m^3/s)
  235. photoC_NO3_diat: Diatom C Fixation from NO3 (units: mmol/m^3/s)
  236. photoFe_diat: Diatom Fe Uptake (units: mmol/m^3/s)
  237. photoNO3_diat: Diatom NO3 Uptake (units: mmol/m^3/s)
  238. photoNH4_diat: Diatom NH4 Uptake (units: mmol/m^3/s)
  239. DOP_diat_uptake: Diatom DOP Uptake (units: mmol/m^3/s)
  240. PO4_diat_uptake: Diatom PO4 Uptake (units: mmol/m^3/s)
  241. graze_diat: Diatom Grazing (units: mmol/m^3/s)
  242. graze_diat_poc: Diatom Grazing to POC (units: mmol/m^3/s)
  243. graze_diat_doc: Diatom Grazing to DOC (units: mmol/m^3/s)
  244. graze_diat_zootot: Diatom Grazing to ZOO TOT (units: mmol/m^3/s)
  245. graze_diat_zoo: Diatom Grazing to Zooplankton (units: mmol/m^3/s)
  246. diat_loss: Diatom Loss (units: mmol/m^3/s)
  247. diat_loss_poc: Diatom Loss to POC (units: mmol/m^3/s)
  248. diat_loss_doc: Diatom Loss to DOC (units: mmol/m^3/s)
  249. diat_agg: Diatom Aggregation (units: mmol/m^3/s)
  250. diat_bSi_form: Diatom Si Uptake (units: mmol/m^3/s)
  251. diaz_Qp: Diazotroph P:C ratio (units: 1)
  252. photoC_diaz: Diazotroph C Fixation (units: mmol/m^3/s)
  253. photoC_NO3_diaz: Diazotroph C Fixation from NO3 (units: mmol/m^3/s)
  254. photoFe_diaz: Diazotroph Fe Uptake (units: mmol/m^3/s)
  255. photoNO3_diaz: Diazotroph NO3 Uptake (units: mmol/m^3/s)
  256. photoNH4_diaz: Diazotroph NH4 Uptake (units: mmol/m^3/s)
  257. DOP_diaz_uptake: Diazotroph DOP Uptake (units: mmol/m^3/s)
  258. PO4_diaz_uptake: Diazotroph PO4 Uptake (units: mmol/m^3/s)
  259. graze_diaz: Diazotroph Grazing (units: mmol/m^3/s)
  260. graze_diaz_poc: Diazotroph Grazing to POC (units: mmol/m^3/s)
  261. graze_diaz_doc: Diazotroph Grazing to DOC (units: mmol/m^3/s)
  262. graze_diaz_zootot: Diazotroph Grazing to ZOO TOT (units: mmol/m^3/s)
  263. graze_diaz_zoo: Diazotroph Grazing to Zooplankton (units: mmol/m^3/s)
  264. diaz_loss: Diazotroph Loss (units: mmol/m^3/s)
  265. diaz_loss_poc: Diazotroph Loss to POC (units: mmol/m^3/s)
  266. diaz_loss_doc: Diazotroph Loss to DOC (units: mmol/m^3/s)
  267. diaz_agg: Diazotroph Aggregation (units: mmol/m^3/s)
  268. diaz_Nfix: Diazotroph N Fixation (units: mmol/m^3/s)
  269. bSi_form: Total Si Uptake (units: mmol/m^3/s)
  270. CaCO3_form: Total CaCO3 Formation (units: mmol/m^3/s)
  271. Nfix: Total N Fixation (units: mmol/m^3/s)
  272. zoo_loss: Zooplankton Loss (units: mmol/m^3/s)
  273. zoo_loss_poc: Zooplankton Loss to POC (units: mmol/m^3/s)
  274. zoo_loss_doc: Zooplankton Loss to DOC (units: mmol/m^3/s)
  275. graze_zoo: Zooplankton grazing loss (units: mmol/m^3/s)
  276. graze_zoo_poc: Zooplankton grazing loss to POC (units: mmol/m^3/s)
  277. graze_zoo_doc: Zooplankton grazing loss to DOC (units: mmol/m^3/s)
  278. graze_zoo_zootot: Zooplankton grazing loss to ZOO TOT (units: mmol/m^3/s)
  279. graze_zoo_zoo: Zooplankton grazing loss to Zooplankton (units: mmol/m^3/s)
  280. x_graze_zoo: Zooplankton grazing gain (units: mmol/m^3/s)
  281. PO4_RESTORE_TEND: Dissolved Inorganic Phosphate Restoring Tendency (units: mmol/m^3/s)
  282. NO3_RESTORE_TEND: Dissolved Inorganic Nitrate Restoring Tendency (units: mmol/m^3/s)
  283. SiO3_RESTORE_TEND: Dissolved Inorganic Silicate Restoring Tendency (units: mmol/m^3/s)
  284. NH4_RESTORE_TEND: Dissolved Ammonia Restoring Tendency (units: mmol/m^3/s)
  285. Fe_RESTORE_TEND: Dissolved Inorganic Iron Restoring Tendency (units: mmol/m^3/s)
  286. Lig_RESTORE_TEND: Iron Binding Ligand Restoring Tendency (units: mmol/m^3/s)
  287. O2_RESTORE_TEND: Dissolved Oxygen Restoring Tendency (units: mmol/m^3/s)
  288. DIC_RESTORE_TEND: Dissolved Inorganic Carbon Restoring Tendency (units: mmol/m^3/s)
  289. DIC_ALT_CO2_RESTORE_TEND: Dissolved Inorganic Carbon, Alternative CO2 Restoring Tendency (units: mmol/m^3/s)
  290. ALK_RESTORE_TEND: Alkalinity Restoring Tendency (units: meq/m^3/s)
  291. ALK_ALT_CO2_RESTORE_TEND: Alkalinity, Alternative CO2 Restoring Tendency (units: meq/m^3/s)
  292. DOC_RESTORE_TEND: Dissolved Organic Carbon Restoring Tendency (units: mmol/m^3/s)
  293. DON_RESTORE_TEND: Dissolved Organic Nitrogen Restoring Tendency (units: mmol/m^3/s)
  294. DOP_RESTORE_TEND: Dissolved Organic Phosphorus Restoring Tendency (units: mmol/m^3/s)
  295. DOPr_RESTORE_TEND: Refractory DOP Restoring Tendency (units: mmol/m^3/s)
  296. DONr_RESTORE_TEND: Refractory DON Restoring Tendency (units: mmol/m^3/s)
  297. DOCr_RESTORE_TEND: Refractory DOC Restoring Tendency (units: mmol/m^3/s)
  298. zooC_RESTORE_TEND: Zooplankton Carbon Restoring Tendency (units: mmol/m^3/s)
  299. spChl_RESTORE_TEND: Small Phyto Chlorophyll Restoring Tendency (units: mg/m^3/s)
  300. spC_RESTORE_TEND: Small Phyto Carbon Restoring Tendency (units: mmol/m^3/s)
  301. spP_RESTORE_TEND: Small Phyto Phosphorus Restoring Tendency (units: mmol/m^3/s)
  302. spFe_RESTORE_TEND: Small Phyto Iron Restoring Tendency (units: mmol/m^3/s)
  303. spCaCO3_RESTORE_TEND: Small Phyto CaCO3 Restoring Tendency (units: mmol/m^3/s)
  304. diatChl_RESTORE_TEND: Diatom Chlorophyll Restoring Tendency (units: mg/m^3/s)
  305. diatC_RESTORE_TEND: Diatom Carbon Restoring Tendency (units: mmol/m^3/s)
  306. diatP_RESTORE_TEND: Diatom Phosphorus Restoring Tendency (units: mmol/m^3/s)
  307. diatFe_RESTORE_TEND: Diatom Iron Restoring Tendency (units: mmol/m^3/s)
  308. diatSi_RESTORE_TEND: Diatom Silicon Restoring Tendency (units: mmol/m^3/s)
  309. diazChl_RESTORE_TEND: Diazotroph Chlorophyll Restoring Tendency (units: mg/m^3/s)
  310. diazC_RESTORE_TEND: Diazotroph Carbon Restoring Tendency (units: mmol/m^3/s)
  311. diazP_RESTORE_TEND: Diazotroph Phosphorus Restoring Tendency (units: mmol/m^3/s)
  312. diazFe_RESTORE_TEND: Diazotroph Iron Restoring Tendency (units: mmol/m^3/s)

.. raw:: html

  </details>
  </p>

The details are found in ``$MARBL/tests/driver_src/marbl.F90``:

.. block comes from tests/driver_src/marbl.F90
.. code-block:: fortran

  ! Log surface flux diagnostics passed back to driver
  associate(diags => marbl_instances(1)%surface_flux_diags%diags)
    call driver_status_log%log_header('Surface flux diagnostics', subname)
    do n=1, size(diags)
      write(log_message, "(I0,7A)") n, '. ', trim(diags(n)%short_name), ': ', trim(diags(n)%long_name), &
                                    ' (units: ', trim(diags(n)%units),')'
      call driver_status_log%log_noerror(log_message, subname)
    end do
  end associate
  ! Log interior tendency diagnostics passed back to driver
  associate(diags => marbl_instances(1)%interior_tendency_diags%diags)
    call driver_status_log%log_header('Interior tendency diagnostics', subname)
    do n=1, size(diags)
      write(log_message, "(I0,7A)") n, '. ', trim(diags(n)%short_name), ': ', trim(diags(n)%long_name), &
                                    ' (units: ', trim(diags(n)%units),')'
      call driver_status_log%log_noerror(log_message, subname)
    end do
  end associate

The ``marbl_interface_class`` contains two objects (``surface_flux_diags`` and ``interior_tendency_diags``).
Both are of type ``marbl_diagnostics_type``, and contain an array named ``diags``.
The length of this array is equal to the number of surface flux or interior tendency diagnostics.
This ``marbl_single_diagnostic_type`` contains the data and metadata for each diagnostic:

.. block comes from marbl_interface_public_types
.. code-block:: fortran

  type, private :: marbl_single_diagnostic_type
     ! marbl_single_diagnostic :
     ! a private type, this contains both the metadata
     ! and the actual diagnostic data for a single
     ! diagnostic quantity. Data must be accessed via
     ! the marbl_diagnostics_type data structure.
     character (len=char_len)                    :: long_name
     character (len=char_len)                    :: short_name
     character (len=char_len)                    :: units
     character (len=char_len)                    :: vertical_grid ! 'none', 'layer_avg', 'layer_iface'
     logical   (log_kind)                        :: compute_now
     logical   (log_kind)                        :: ltruncated_vertical_extent
     integer   (int_kind)                        :: ref_depth ! depth that diagnostic nominally resides at
     real      (r8), allocatable, dimension(:)   :: field_2d
     real      (r8), allocatable, dimension(:,:) :: field_3d

   contains
     procedure :: initialize  => marbl_single_diag_init
  end type marbl_single_diagnostic_type

  !*****************************************************************************

  type, public :: marbl_diagnostics_type
     ! marbl_diagnostics :
     ! used to pass diagnostic information from marbl back to
     ! the driver.
     integer :: num_elements
     integer :: num_levels
     type(marbl_single_diagnostic_type), dimension(:), pointer :: diags

   contains
     procedure, public :: construct      => marbl_diagnostics_constructor
     procedure, public :: set_to_zero    => marbl_diagnostics_set_to_zero
     procedure, public :: add_diagnostic => marbl_diagnostics_add
     procedure, public :: deconstruct    => marbl_diagnostics_deconstructor
  end type marbl_diagnostics_type

The short name of the diagnostic is the recommended name for the variable if writing a netCDF output file.
The long name is a more descriptive name and, as the example output shows, the units are also included in the metadata.
