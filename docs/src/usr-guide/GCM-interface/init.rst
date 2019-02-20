.. _init:

====
Init
====

The init stage is where MARBL is configured, parameters are set, and memory is allocated.
If the GCM wants to specify non-default parameter values, that needs to be done with ``put_setting()`` statements before calling ``init()``.
There are three different interfaces that can be used; all are equivalent, but different GCMs may find it easiest to call different interfaces.

#. Two arguments: a string containing the variable name, and then the variable value (in proper datatype)

   .. block comes from made-up example
   .. code-block:: fortran

     call marbl_instance%put_setting('ciso_on', .true.)

#. One argument: a string containing a line from a MARBL input file (TODO: input file link!) of the format ``varname = value``

   .. block comes from made-up example
   .. code-block:: fortran

     call marbl_instance%put_setting('ciso_on = .true.')

#. Three arguments: strings containing the variable name, the datatype, and the value

   .. block comes from made-up example
   .. code-block:: fortran

     call marbl_instance%put_setting('ciso_on', 'logical', '.true.')

``put_setting()`` calls that do not correspond to defined MARBL parameters will result in an error during ``init()``.
There is no check in the ``put_setting()`` call itself because allowable parameters may depend on other parameter values.
For example, ``autotrophs(3)%sname`` is defined if ``autotroph_cnt >= 3`` but not if ``autotroph_cnt < 3``.

.. _ref-init-interface:

The ``init()`` interface
------------------------

.. block comes from marbl_interface
.. code-block:: fortran

  subroutine init(this,                   &
       gcm_num_levels,                    &
       gcm_num_PAR_subcols,               &
       gcm_num_elements_surface_flux,     &
       gcm_delta_z,                       &
       gcm_zw,                            &
       gcm_zt,                            &
       lgcm_has_global_ops)

    .
    .
    .

    class(marbl_interface_class), intent(inout) :: this
    integer(int_kind),            intent(in)    :: gcm_num_levels
    integer(int_kind),            intent(in)    :: gcm_num_PAR_subcols
    integer(int_kind),            intent(in)    :: gcm_num_elements_surface_flux
    real(r8),                     intent(in)    :: gcm_delta_z(gcm_num_levels) ! thickness of layer k
    real(r8),                     intent(in)    :: gcm_zw(gcm_num_levels) ! thickness of layer k
    real(r8),                     intent(in)    :: gcm_zt(gcm_num_levels) ! thickness of layer k
    logical,           optional,  intent(in)    :: lgcm_has_global_ops

Note the optional argument:
``lgcm_has_global_ops`` is a way for the GCM to inform MARBL that it can perform global operations such as finding global averages of values.
There are some MARBL configurations that require this, and MARBL will abort unless the GCM verifies it can provide these values.

MARBL does not have an explicit interface to tell a GCM how many tracers are being computed.
Instead, use ``size(marbl_instance%tracer_metadata)`` (called after ``marbl_instance%init()``).

MARBL can compute surface fluxes across multiple columns simultaneously, with the number of columns supported set by ``gcm_num_elements_surface_flux``.
There is not a corresponding ``gcm_num_elements_interior_tendency`` yet, because currently MARBL computes tracer tendencies one column at a time.

Example from Stand-Alone MARBL
------------------------------

The stand-alone MARBL driver / test suite use input files (``*.input`` in the test directory) that are processed in the following manner:

.. block comes from driver_src/marbl.F90
.. code-block:: fortran

  input_line = ''
  do while(ioerr .eq. 0)
    ! (i) broadcast input_line and call put_setting(); abort if error
    !     calling with empty input_line on first entry to loop is okay, and
    !     this ensures we don't call put_setting with a garbage line if
    !     ioerr is non-zero
    call marbl_mpi_bcast(input_line, 0)
    call marbl_instance%put_setting(input_line)
    if (marbl_instance%StatusLog%labort_marbl) then
      call marbl_instance%StatusLog%log_error_trace("put_setting(input_line)", subname)
      call print_marbl_log(marbl_instance%StatusLog)
    end if

    ! (ii) master task reads next line in input file
    if (my_task .eq. 0) read(97,"(A)", iostat=ioerr) input_line

    ! (iii) broadcast input file line to all tasks (along with iostat)
    call marbl_mpi_bcast(ioerr, 0)
  end do

  if (.not.is_iostat_end(ioerr)) then
    if (my_task .eq. 0) then
      write(*,"(A,I0)") "ioerr = ", ioerr
      write(*,"(2A)") "ERROR encountered when reading MARBL input file ", trim(input_file)
    end if
    call marbl_mpi_abort()
  end if

``init()`` is then called from the individual test, storing the tracer count in the local variable ``nt``:

.. block comes from marbl_init_drv
.. code-block:: fortran

  ! Call marbl%init
  call marbl_instance%init(gcm_num_levels = km,               &
                           gcm_num_PAR_subcols = 1,           &
                           gcm_num_elements_surface_flux = 1, &
                           gcm_delta_z = delta_z,             &
                           gcm_zw = zw,                       &
                           gcm_zt = zt)
  if (marbl_instance%StatusLog%labort_marbl) then
    call marbl_instance%StatusLog%log_error_trace('marbl%init', subname)
    return
  end if

Default Parameter Values
------------------------

Below are the default parameter values (real variables provided to double precision).
This specific page was been generated by running the ``gen_input_file`` regression test with no input file.
The test writes this output to ``marbl_input.gen``.
Note that the order the variables are listed in comes from the order the variables are defined in MARBL, but the order of ``put_setting()`` calls does not matter.

.. block comes from tests/regression_tests/gen_input_file/marbl_input.gen
.. code-block:: none

  PFT_defaults = 'CESM2'
  ciso_on = F
  lsource_sink = T
  lecovars_full_depth_tavg = F
  ciso_lsource_sink = T
  ciso_lecovars_full_depth_tavg = F
  lflux_gas_o2 = T
  lflux_gas_co2 = T
  lcompute_nhx_surface_emis = T
  lvariable_PtoC = T
  ladjust_bury_coeff = F
  lo2_consumption_scalef = F
  lp_remin_scalef = F
  init_bury_coeff_opt = 'settings_file'
  particulate_flux_ref_depth = 100
  Jint_Ctot_thres_molpm2pyr =   0.10000000000000001E-08
  gQsi_0 =   0.13700000000000001E+00
  gQsi_max =   0.82199999999999995E+00
  gQsi_min =   0.45699999999999998E-01
  gQ_Fe_kFe_thres =   0.10000000000000000E+02
  gQ_Si_kSi_thres =   0.60000000000000000E+01
  parm_Fe_bioavail =   0.10000000000000000E+01
  parm_o2_min =   0.50000000000000000E+01
  parm_o2_min_delta =   0.50000000000000000E+01
  parm_kappa_nitrif_per_day =   0.59999999999999998E-01
  parm_nitrif_par_lim =   0.10000000000000000E+01
  parm_labile_ratio =   0.93999999999999995E+00
  parm_init_POC_bury_coeff =   0.25400000000000000E+01
  parm_init_POP_bury_coeff =   0.35999999999999999E+00
  parm_init_bSi_bury_coeff =   0.15300000000000000E+01
  parm_Fe_scavenge_rate0 =   0.22000000000000000E+02
  parm_Lig_scavenge_rate0 =   0.14999999999999999E-01
  parm_FeLig_scavenge_rate0 =   0.12000000000000000E+01
  parm_Lig_degrade_rate0 =   0.93999999999999994E-04
  parm_Fe_desorption_rate0 =   0.99999999999999995E-06
  parm_f_prod_sp_CaCO3 =   0.70000000000000007E-01
  parm_POC_diss =   0.10000000000000000E+05
  parm_SiO2_diss =   0.65000000000000000E+05
  parm_SiO2_gamma =   0.00000000000000000E+00
  parm_hPOC_SiO2_ratio =   0.10000000000000000E-01
  parm_CaCO3_diss =   0.50000000000000000E+05
  parm_CaCO3_gamma =   0.20000000000000000E-01
  parm_hPOC_CaCO3_ratio =   0.10000000000000000E-01
  parm_hPOC_dust_ratio =   0.10000000000000000E-01
  o2_sf_o2_range_hi =   0.45000000000000000E+02
  o2_sf_o2_range_lo =   0.50000000000000000E+01
  o2_sf_val_lo_o2 =   0.26000000000000001E+01
  parm_sed_denitrif_coeff =   0.10000000000000000E+01
  bury_coeff_rmean_timescale_years =   0.10000000000000000E+02
  parm_scalelen_z(1) =   0.10000000000000000E+05
  parm_scalelen_z(2) =   0.25000000000000000E+05
  parm_scalelen_z(3) =   0.50000000000000000E+05
  parm_scalelen_z(4) =   0.10000000000000000E+06
  parm_scalelen_vals(1) =   0.10000000000000000E+01
  parm_scalelen_vals(2) =   0.36000000000000001E+01
  parm_scalelen_vals(3) =   0.47000000000000002E+01
  parm_scalelen_vals(4) =   0.47999999999999998E+01
  caco3_bury_thres_opt = 'omega_calc'
  caco3_bury_thres_depth =   0.30000000000000000E+06
  caco3_bury_thres_omega_calc =   0.89000000000000001E+00
  PON_bury_coeff =   0.50000000000000000E+00
  POM_bury_frac_max =   0.80000000000000004E+00
  bSi_bury_frac_max =   0.10000000000000000E+01
  ciso_fract_factors = 'Laws'
  autotroph_cnt = 3
  zooplankton_cnt = 1
  max_grazer_prey_cnt = 3
  autotroph_settings(1)%sname = 'sp'
  autotroph_settings(1)%lname = 'Small Phyto'
  autotroph_settings(1)%Nfixer = F
  autotroph_settings(1)%imp_calcifier = T
  autotroph_settings(1)%exp_calcifier = F
  autotroph_settings(1)%silicifier = F
  autotroph_settings(1)%kFe =   0.30000000000000001E-04
  autotroph_settings(1)%kPO4 =   0.10000000000000000E-01
  autotroph_settings(1)%kDOP =   0.29999999999999999E+00
  autotroph_settings(1)%kNO3 =   0.25000000000000000E+00
  autotroph_settings(1)%kNH4 =   0.10000000000000000E-01
  autotroph_settings(1)%kSiO3 =   0.00000000000000000E+00
  autotroph_settings(1)%Qp_fixed =   0.85470085470085479E-02
  autotroph_settings(1)%gQfe_0 =   0.30000000000000001E-04
  autotroph_settings(1)%gQfe_min =   0.25000000000000002E-05
  autotroph_settings(1)%alphaPi_per_day =   0.39000000000000001E+00
  autotroph_settings(1)%PCref_per_day =   0.50000000000000000E+01
  autotroph_settings(1)%thetaN_max =   0.25000000000000000E+01
  autotroph_settings(1)%loss_thres =   0.10000000000000000E-01
  autotroph_settings(1)%loss_thres2 =   0.00000000000000000E+00
  autotroph_settings(1)%temp_thres =  -0.10000000000000000E+02
  autotroph_settings(1)%mort_per_day =   0.10000000000000001E+00
  autotroph_settings(1)%mort2_per_day =   0.10000000000000000E-01
  autotroph_settings(1)%agg_rate_max =   0.50000000000000000E+00
  autotroph_settings(1)%agg_rate_min =   0.10000000000000000E-01
  autotroph_settings(1)%loss_poc =   0.00000000000000000E+00
  autotroph_settings(2)%sname = 'diat'
  autotroph_settings(2)%lname = 'Diatom'
  autotroph_settings(2)%Nfixer = F
  autotroph_settings(2)%imp_calcifier = F
  autotroph_settings(2)%exp_calcifier = F
  autotroph_settings(2)%silicifier = T
  autotroph_settings(2)%kFe =   0.69999999999999994E-04
  autotroph_settings(2)%kPO4 =   0.50000000000000003E-01
  autotroph_settings(2)%kDOP =   0.50000000000000000E+00
  autotroph_settings(2)%kNO3 =   0.50000000000000000E+00
  autotroph_settings(2)%kNH4 =   0.50000000000000003E-01
  autotroph_settings(2)%kSiO3 =   0.69999999999999996E+00
  autotroph_settings(2)%Qp_fixed =   0.85470085470085479E-02
  autotroph_settings(2)%gQfe_0 =   0.30000000000000001E-04
  autotroph_settings(2)%gQfe_min =   0.25000000000000002E-05
  autotroph_settings(2)%alphaPi_per_day =   0.28000000000000003E+00
  autotroph_settings(2)%PCref_per_day =   0.50000000000000000E+01
  autotroph_settings(2)%thetaN_max =   0.40000000000000000E+01
  autotroph_settings(2)%loss_thres =   0.20000000000000000E-01
  autotroph_settings(2)%loss_thres2 =   0.00000000000000000E+00
  autotroph_settings(2)%temp_thres =  -0.10000000000000000E+02
  autotroph_settings(2)%mort_per_day =   0.10000000000000001E+00
  autotroph_settings(2)%mort2_per_day =   0.10000000000000000E-01
  autotroph_settings(2)%agg_rate_max =   0.50000000000000000E+00
  autotroph_settings(2)%agg_rate_min =   0.20000000000000000E-01
  autotroph_settings(2)%loss_poc =   0.00000000000000000E+00
  autotroph_settings(3)%sname = 'diaz'
  autotroph_settings(3)%lname = 'Diazotroph'
  autotroph_settings(3)%Nfixer = T
  autotroph_settings(3)%imp_calcifier = F
  autotroph_settings(3)%exp_calcifier = F
  autotroph_settings(3)%silicifier = F
  autotroph_settings(3)%kFe =   0.45000000000000003E-04
  autotroph_settings(3)%kPO4 =   0.14999999999999999E-01
  autotroph_settings(3)%kDOP =   0.74999999999999997E-01
  autotroph_settings(3)%kNO3 =   0.20000000000000000E+01
  autotroph_settings(3)%kNH4 =   0.20000000000000001E+00
  autotroph_settings(3)%kSiO3 =   0.00000000000000000E+00
  autotroph_settings(3)%Qp_fixed =   0.27350427350427355E-02
  autotroph_settings(3)%gQfe_0 =   0.60000000000000002E-04
  autotroph_settings(3)%gQfe_min =   0.25000000000000002E-05
  autotroph_settings(3)%alphaPi_per_day =   0.39000000000000001E+00
  autotroph_settings(3)%PCref_per_day =   0.25000000000000000E+01
  autotroph_settings(3)%thetaN_max =   0.25000000000000000E+01
  autotroph_settings(3)%loss_thres =   0.20000000000000000E-01
  autotroph_settings(3)%loss_thres2 =   0.10000000000000000E-02
  autotroph_settings(3)%temp_thres =   0.15000000000000000E+02
  autotroph_settings(3)%mort_per_day =   0.10000000000000001E+00
  autotroph_settings(3)%mort2_per_day =   0.10000000000000000E-01
  autotroph_settings(3)%agg_rate_max =   0.50000000000000000E+00
  autotroph_settings(3)%agg_rate_min =   0.10000000000000000E-01
  autotroph_settings(3)%loss_poc =   0.00000000000000000E+00
  zooplankton_settings(1)%sname = 'zoo'
  zooplankton_settings(1)%lname = 'Zooplankton'
  zooplankton_settings(1)%z_mort_0_per_day =   0.10000000000000001E+00
  zooplankton_settings(1)%loss_thres =   0.74999999999999997E-01
  zooplankton_settings(1)%z_mort2_0_per_day =   0.40000000000000002E+00
  grazing_relationship_settings(1,1)%sname = 'grz_sp_zoo'
  grazing_relationship_settings(1,1)%lname = 'Grazing of sp by zoo'
  grazing_relationship_settings(1,1)%auto_ind_cnt = 1
  grazing_relationship_settings(1,1)%zoo_ind_cnt = 0
  grazing_relationship_settings(1,1)%grazing_function = 1
  grazing_relationship_settings(1,1)%z_umax_0_per_day =   0.32999999999999998E+01
  grazing_relationship_settings(1,1)%z_grz =   0.12000000000000000E+01
  grazing_relationship_settings(1,1)%graze_zoo =   0.29999999999999999E+00
  grazing_relationship_settings(1,1)%graze_poc =   0.00000000000000000E+00
  grazing_relationship_settings(1,1)%graze_doc =   0.59999999999999998E-01
  grazing_relationship_settings(1,1)%f_zoo_detr =   0.12000000000000000E+00
  grazing_relationship_settings(1,1)%auto_ind(1) = 1
  grazing_relationship_settings(2,1)%sname = 'grz_diat_zoo'
  grazing_relationship_settings(2,1)%lname = 'Grazing of diat by zoo'
  grazing_relationship_settings(2,1)%auto_ind_cnt = 1
  grazing_relationship_settings(2,1)%zoo_ind_cnt = 0
  grazing_relationship_settings(2,1)%grazing_function = 1
  grazing_relationship_settings(2,1)%z_umax_0_per_day =   0.31499999999999999E+01
  grazing_relationship_settings(2,1)%z_grz =   0.12000000000000000E+01
  grazing_relationship_settings(2,1)%graze_zoo =   0.25000000000000000E+00
  grazing_relationship_settings(2,1)%graze_poc =   0.39000000000000001E+00
  grazing_relationship_settings(2,1)%graze_doc =   0.59999999999999998E-01
  grazing_relationship_settings(2,1)%f_zoo_detr =   0.23999999999999999E+00
  grazing_relationship_settings(2,1)%auto_ind(1) = 2
  grazing_relationship_settings(3,1)%sname = 'grz_diaz_zoo'
  grazing_relationship_settings(3,1)%lname = 'Grazing of diaz by zoo'
  grazing_relationship_settings(3,1)%auto_ind_cnt = 1
  grazing_relationship_settings(3,1)%zoo_ind_cnt = 0
  grazing_relationship_settings(3,1)%grazing_function = 1
  grazing_relationship_settings(3,1)%z_umax_0_per_day =   0.32999999999999998E+01
  grazing_relationship_settings(3,1)%z_grz =   0.12000000000000000E+01
  grazing_relationship_settings(3,1)%graze_zoo =   0.29999999999999999E+00
  grazing_relationship_settings(3,1)%graze_poc =   0.10000000000000001E+00
  grazing_relationship_settings(3,1)%graze_doc =   0.59999999999999998E-01
  grazing_relationship_settings(3,1)%f_zoo_detr =   0.12000000000000000E+00
  grazing_relationship_settings(3,1)%auto_ind(1) = 3
  tracer_restore_vars(1) = ''
  tracer_restore_vars(2) = ''
  tracer_restore_vars(3) = ''
  tracer_restore_vars(4) = ''
  tracer_restore_vars(5) = ''
  tracer_restore_vars(6) = ''
  tracer_restore_vars(7) = ''
  tracer_restore_vars(8) = ''
  tracer_restore_vars(9) = ''
  tracer_restore_vars(10) = ''
  tracer_restore_vars(11) = ''
  tracer_restore_vars(12) = ''
  tracer_restore_vars(13) = ''
  tracer_restore_vars(14) = ''
  tracer_restore_vars(15) = ''
  tracer_restore_vars(16) = ''
  tracer_restore_vars(17) = ''
  tracer_restore_vars(18) = ''
  tracer_restore_vars(19) = ''
  tracer_restore_vars(20) = ''
  tracer_restore_vars(21) = ''
  tracer_restore_vars(22) = ''
  tracer_restore_vars(23) = ''
  tracer_restore_vars(24) = ''
  tracer_restore_vars(25) = ''
  tracer_restore_vars(26) = ''
  tracer_restore_vars(27) = ''
  tracer_restore_vars(28) = ''
  tracer_restore_vars(29) = ''
  tracer_restore_vars(30) = ''
  tracer_restore_vars(31) = ''
  tracer_restore_vars(32) = ''

A python tool to generate input settings files is also provided: ``MARBL_tools/MARBL_generate_settings_file.py``.
This script creates ``marbl.input``, and organizes the output better than the Fortran test:

.. block comes from MARBL_tools/marbl.input
.. code-block:: none

  ! config PFTs
  PFT_defaults = "CESM2"
  autotroph_cnt = 3
  max_grazer_prey_cnt = 3
  zooplankton_cnt = 1

  ! config flags
  ciso_lecovars_full_depth_tavg = .false.
  ciso_lsource_sink = .true.
  ciso_on = .false.
  ladjust_bury_coeff = .false.
  lcompute_nhx_surface_emis = .true.
  lecovars_full_depth_tavg = .false.
  lflux_gas_co2 = .true.
  lflux_gas_o2 = .true.
  lo2_consumption_scalef = .false.
  lp_remin_scalef = .false.
  lsource_sink = .true.
  lvariable_PtoC = .true.

  ! config strings
  init_bury_coeff_opt = "settings_file"

  ! general parameters
  Jint_Ctot_thres_molpm2pyr = 1e-09
  bury_coeff_rmean_timescale_years = 10
  caco3_bury_thres_depth =   3.0000000000000000e+05
  caco3_bury_thres_omega_calc = 0.89
  caco3_bury_thres_opt = "omega_calc"
  ciso_fract_factors = "Laws"
  gQ_Fe_kFe_thres = 10.0
  gQ_Si_kSi_thres = 6.0
  gQsi_0 = 0.137
  gQsi_max = 0.822
  gQsi_min = 0.0457
  o2_sf_o2_range_hi = 45.0
  o2_sf_o2_range_lo = 5.0
  o2_sf_val_lo_o2 = 2.6
  parm_CaCO3_gamma = 0.02
  parm_Fe_bioavail = 1.0
  parm_Fe_desorption_rate0 =   9.9999999999999995e-07
  parm_Lig_degrade_rate0 = 9.4e-05
  parm_SiO2_gamma = 0.0
  parm_f_prod_sp_CaCO3 = 0.07
  parm_hPOC_CaCO3_ratio = 0.01
  parm_hPOC_SiO2_ratio = 0.01
  parm_hPOC_dust_ratio = 0.01
  parm_labile_ratio = 0.94
  parm_o2_min = 5.0
  parm_o2_min_delta = 5.0
  parm_sed_denitrif_coeff = 1
  particulate_flux_ref_depth = 100

  ! general parameters (bury coeffs)
  POM_bury_frac_max = 0.8
  PON_bury_coeff = 0.5
  bSi_bury_frac_max = 1.0
  parm_init_POC_bury_coeff = 2.54
  parm_init_POP_bury_coeff = 0.36
  parm_init_bSi_bury_coeff = 1.53

  ! general parameters (dissolution)
  parm_CaCO3_diss =   5.0000000000000000e+04
  parm_POC_diss =   1.0000000000000000e+04
  parm_SiO2_diss =   6.5000000000000000e+04

  ! general parameters (nitrification)
  parm_kappa_nitrif_per_day = 0.06
  parm_nitrif_par_lim = 1.0

  ! general parameters (scavenging)
  parm_FeLig_scavenge_rate0 = 1.2
  parm_Fe_scavenge_rate0 = 22.0
  parm_Lig_scavenge_rate0 = 0.015

  ! Scale lengths
  parm_scalelen_vals(1) = 1
  parm_scalelen_vals(2) = 3.6
  parm_scalelen_vals(3) = 4.7
  parm_scalelen_vals(4) = 4.8
  parm_scalelen_z(1) =   1.0000000000000000e+04
  parm_scalelen_z(2) =   2.5000000000000000e+04
  parm_scalelen_z(3) =   5.0000000000000000e+04
  parm_scalelen_z(4) =   1.0000000000000000e+05

  ! autotrophs
  autotroph_settings(1)%Nfixer = .false.
  autotroph_settings(1)%PCref_per_day = 5
  autotroph_settings(1)%Qp_fixed =   8.5470085470085479e-03
  autotroph_settings(1)%agg_rate_max = 0.5
  autotroph_settings(1)%agg_rate_min = 0.01
  autotroph_settings(1)%alphaPI_per_day = 0.39
  autotroph_settings(1)%exp_calcifier = .false.
  autotroph_settings(1)%gQfe_0 =   3.0000000000000001e-05
  autotroph_settings(1)%gQfe_min = 2.5e-06
  autotroph_settings(1)%imp_calcifier = .true.
  autotroph_settings(1)%kDOP = 0.3
  autotroph_settings(1)%kFe = 3e-05
  autotroph_settings(1)%kNH4 = 0.01
  autotroph_settings(1)%kNO3 = 0.25
  autotroph_settings(1)%kPO4 = 0.01
  autotroph_settings(1)%kSiO3 = 0
  autotroph_settings(1)%lname = "Small Phyto"
  autotroph_settings(1)%loss_poc = 0
  autotroph_settings(1)%loss_thres = 0.01
  autotroph_settings(1)%loss_thres2 = 0
  autotroph_settings(1)%mort2_per_day = 0.01
  autotroph_settings(1)%mort_per_day = 0.1
  autotroph_settings(1)%silicifier = .false.
  autotroph_settings(1)%sname = "sp"
  autotroph_settings(1)%temp_thres = -10
  autotroph_settings(1)%thetaN_max = 2.5
  autotroph_settings(2)%Nfixer = .false.
  autotroph_settings(2)%PCref_per_day = 5
  autotroph_settings(2)%Qp_fixed =   8.5470085470085479e-03
  autotroph_settings(2)%agg_rate_max = 0.5
  autotroph_settings(2)%agg_rate_min = 0.02
  autotroph_settings(2)%alphaPI_per_day = 0.28
  autotroph_settings(2)%exp_calcifier = .false.
  autotroph_settings(2)%gQfe_0 =   3.0000000000000001e-05
  autotroph_settings(2)%gQfe_min = 2.5e-06
  autotroph_settings(2)%imp_calcifier = .false.
  autotroph_settings(2)%kDOP = 0.5
  autotroph_settings(2)%kFe = 7e-05
  autotroph_settings(2)%kNH4 = 0.05
  autotroph_settings(2)%kNO3 = 0.5
  autotroph_settings(2)%kPO4 = 0.05
  autotroph_settings(2)%kSiO3 = 0.7
  autotroph_settings(2)%lname = "Diatom"
  autotroph_settings(2)%loss_poc = 0
  autotroph_settings(2)%loss_thres = 0.02
  autotroph_settings(2)%loss_thres2 = 0
  autotroph_settings(2)%mort2_per_day = 0.01
  autotroph_settings(2)%mort_per_day = 0.1
  autotroph_settings(2)%silicifier = .true.
  autotroph_settings(2)%sname = "diat"
  autotroph_settings(2)%temp_thres = -10
  autotroph_settings(2)%thetaN_max = 4
  autotroph_settings(3)%Nfixer = .true.
  autotroph_settings(3)%PCref_per_day = 2.5
  autotroph_settings(3)%Qp_fixed =   2.7350427350427355e-03
  autotroph_settings(3)%agg_rate_max = 0.5
  autotroph_settings(3)%agg_rate_min = 0.01
  autotroph_settings(3)%alphaPI_per_day = 0.39
  autotroph_settings(3)%exp_calcifier = .false.
  autotroph_settings(3)%gQfe_0 =   6.0000000000000002e-05
  autotroph_settings(3)%gQfe_min = 2.5e-06
  autotroph_settings(3)%imp_calcifier = .false.
  autotroph_settings(3)%kDOP = 0.075
  autotroph_settings(3)%kFe = 4.5e-05
  autotroph_settings(3)%kNH4 = 0.2
  autotroph_settings(3)%kNO3 = 2
  autotroph_settings(3)%kPO4 = 0.015
  autotroph_settings(3)%kSiO3 = 0
  autotroph_settings(3)%lname = "Diazotroph"
  autotroph_settings(3)%loss_poc = 0
  autotroph_settings(3)%loss_thres = 0.02
  autotroph_settings(3)%loss_thres2 = 0.001
  autotroph_settings(3)%mort2_per_day = 0.01
  autotroph_settings(3)%mort_per_day = 0.1
  autotroph_settings(3)%silicifier = .false.
  autotroph_settings(3)%sname = "diaz"
  autotroph_settings(3)%temp_thres = 15
  autotroph_settings(3)%thetaN_max = 2.5

  ! zooplankton
  zooplankton_settings(1)%lname = "Zooplankton"
  zooplankton_settings(1)%loss_thres = 0.075
  zooplankton_settings(1)%sname = "zoo"
  zooplankton_settings(1)%z_mort2_0_per_day = 0.4
  zooplankton_settings(1)%z_mort_0_per_day = 0.1

  ! grazing
  grazing_relationship_settings(1,1)%auto_ind(1) = 1
  grazing_relationship_settings(1,1)%auto_ind_cnt = 1
  grazing_relationship_settings(1,1)%f_zoo_detr = 0.12
  grazing_relationship_settings(1,1)%graze_doc = 0.06
  grazing_relationship_settings(1,1)%graze_poc = 0
  grazing_relationship_settings(1,1)%graze_zoo = 0.3
  grazing_relationship_settings(1,1)%grazing_function = 1
  grazing_relationship_settings(1,1)%lname = "Grazing of sp by zoo"
  grazing_relationship_settings(1,1)%sname = "grz_sp_zoo"
  grazing_relationship_settings(1,1)%z_grz = 1.2
  grazing_relationship_settings(1,1)%z_umax_0_per_day = 3.3
  grazing_relationship_settings(1,1)%zoo_ind_cnt = 0
  grazing_relationship_settings(2,1)%auto_ind(1) = 2
  grazing_relationship_settings(2,1)%auto_ind_cnt = 1
  grazing_relationship_settings(2,1)%f_zoo_detr = 0.24
  grazing_relationship_settings(2,1)%graze_doc = 0.06
  grazing_relationship_settings(2,1)%graze_poc = 0.39
  grazing_relationship_settings(2,1)%graze_zoo = 0.25
  grazing_relationship_settings(2,1)%grazing_function = 1
  grazing_relationship_settings(2,1)%lname = "Grazing of diat by zoo"
  grazing_relationship_settings(2,1)%sname = "grz_diat_zoo"
  grazing_relationship_settings(2,1)%z_grz = 1.2
  grazing_relationship_settings(2,1)%z_umax_0_per_day = 3.15
  grazing_relationship_settings(2,1)%zoo_ind_cnt = 0
  grazing_relationship_settings(3,1)%auto_ind(1) = 3
  grazing_relationship_settings(3,1)%auto_ind_cnt = 1
  grazing_relationship_settings(3,1)%f_zoo_detr = 0.12
  grazing_relationship_settings(3,1)%graze_doc = 0.06
  grazing_relationship_settings(3,1)%graze_poc = 0.1
  grazing_relationship_settings(3,1)%graze_zoo = 0.3
  grazing_relationship_settings(3,1)%grazing_function = 1
  grazing_relationship_settings(3,1)%lname = "Grazing of diaz by zoo"
  grazing_relationship_settings(3,1)%sname = "grz_diaz_zoo"
  grazing_relationship_settings(3,1)%z_grz = 1.2
  grazing_relationship_settings(3,1)%z_umax_0_per_day = 3.3
  grazing_relationship_settings(3,1)%zoo_ind_cnt = 0

  ! tracer restoring
  tracer_restore_vars(1) = ""
  tracer_restore_vars(2) = ""
  tracer_restore_vars(3) = ""
  tracer_restore_vars(4) = ""
  tracer_restore_vars(5) = ""
  tracer_restore_vars(6) = ""
  tracer_restore_vars(7) = ""
  tracer_restore_vars(8) = ""
  tracer_restore_vars(9) = ""
  tracer_restore_vars(10) = ""
  tracer_restore_vars(11) = ""
  tracer_restore_vars(12) = ""
  tracer_restore_vars(13) = ""
  tracer_restore_vars(14) = ""
  tracer_restore_vars(15) = ""
  tracer_restore_vars(16) = ""
  tracer_restore_vars(17) = ""
  tracer_restore_vars(18) = ""
  tracer_restore_vars(19) = ""
  tracer_restore_vars(20) = ""
  tracer_restore_vars(21) = ""
  tracer_restore_vars(22) = ""
  tracer_restore_vars(23) = ""
  tracer_restore_vars(24) = ""
  tracer_restore_vars(25) = ""
  tracer_restore_vars(26) = ""
  tracer_restore_vars(27) = ""
  tracer_restore_vars(28) = ""
  tracer_restore_vars(29) = ""
  tracer_restore_vars(30) = ""
  tracer_restore_vars(31) = ""
  tracer_restore_vars(32) = ""
