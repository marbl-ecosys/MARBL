module marbl_glo_avg_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8
  use marbl_constants_mod, only : spd

  implicit none
  public

  integer (int_kind) :: glo_avg_field_ind_interior_tendency_CaCO3_bury = 0
  integer (int_kind) :: glo_avg_field_ind_interior_tendency_POC_bury = 0
  integer (int_kind) :: glo_avg_field_ind_interior_tendency_POP_bury = 0
  integer (int_kind) :: glo_avg_field_ind_interior_tendency_bSi_bury = 0
  integer (int_kind) :: glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff = 0
  integer (int_kind) :: glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff = 0
  integer (int_kind) :: glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff = 0

  integer (int_kind) :: glo_avg_field_ind_surface_flux_C_input = 0
  integer (int_kind) :: glo_avg_field_ind_surface_flux_P_input = 0
  integer (int_kind) :: glo_avg_field_ind_surface_flux_Si_input = 0

  integer (int_kind) :: glo_scalar_ind_interior_tendency_POC_bury_coeff = 0
  integer (int_kind) :: glo_scalar_ind_interior_tendency_POP_bury_coeff = 0
  integer (int_kind) :: glo_scalar_ind_interior_tendency_bSi_bury_coeff = 0

contains

  !*****************************************************************************

  subroutine marbl_glo_avg_var_cnts_compute( &
       glo_avg_field_cnt_interior_tendency,  &
       glo_avg_field_cnt_surface_flux,       &
       glo_scalar_cnt_interior_tendency,     &
       glo_scalar_cnt_surface_flux)

    use marbl_settings_mod, only : ladjust_bury_coeff

    integer (int_kind), intent(out) :: glo_avg_field_cnt_interior_tendency
    integer (int_kind), intent(out) :: glo_avg_field_cnt_surface_flux
    integer (int_kind), intent(out) :: glo_scalar_cnt_interior_tendency
    integer (int_kind), intent(out) :: glo_scalar_cnt_surface_flux

    glo_avg_field_cnt_interior_tendency = 0
    glo_avg_field_cnt_surface_flux      = 0
    glo_scalar_cnt_interior_tendency    = 0
    glo_scalar_cnt_surface_flux         = 0

    if (ladjust_bury_coeff) then
       glo_avg_field_cnt_interior_tendency = glo_avg_field_cnt_interior_tendency + 1
       glo_avg_field_ind_interior_tendency_CaCO3_bury = glo_avg_field_cnt_interior_tendency

       glo_avg_field_cnt_interior_tendency = glo_avg_field_cnt_interior_tendency + 1
       glo_avg_field_ind_interior_tendency_POC_bury = glo_avg_field_cnt_interior_tendency

       glo_avg_field_cnt_interior_tendency = glo_avg_field_cnt_interior_tendency + 1
       glo_avg_field_ind_interior_tendency_POP_bury = glo_avg_field_cnt_interior_tendency

       glo_avg_field_cnt_interior_tendency = glo_avg_field_cnt_interior_tendency + 1
       glo_avg_field_ind_interior_tendency_bSi_bury = glo_avg_field_cnt_interior_tendency

       glo_avg_field_cnt_interior_tendency = glo_avg_field_cnt_interior_tendency + 1
       glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff = glo_avg_field_cnt_interior_tendency

       glo_avg_field_cnt_interior_tendency = glo_avg_field_cnt_interior_tendency + 1
       glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff = glo_avg_field_cnt_interior_tendency

       glo_avg_field_cnt_interior_tendency = glo_avg_field_cnt_interior_tendency + 1
       glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff = glo_avg_field_cnt_interior_tendency


       glo_avg_field_cnt_surface_flux = glo_avg_field_cnt_surface_flux + 1
       glo_avg_field_ind_surface_flux_C_input = glo_avg_field_cnt_surface_flux

       glo_avg_field_cnt_surface_flux = glo_avg_field_cnt_surface_flux + 1
       glo_avg_field_ind_surface_flux_P_input = glo_avg_field_cnt_surface_flux

       glo_avg_field_cnt_surface_flux = glo_avg_field_cnt_surface_flux + 1
       glo_avg_field_ind_surface_flux_Si_input = glo_avg_field_cnt_surface_flux


       glo_scalar_cnt_interior_tendency = glo_scalar_cnt_interior_tendency + 1
       glo_scalar_ind_interior_tendency_POC_bury_coeff = glo_scalar_cnt_interior_tendency

       glo_scalar_cnt_interior_tendency = glo_scalar_cnt_interior_tendency + 1
       glo_scalar_ind_interior_tendency_POP_bury_coeff = glo_scalar_cnt_interior_tendency

       glo_scalar_cnt_interior_tendency = glo_scalar_cnt_interior_tendency + 1
       glo_scalar_ind_interior_tendency_bSi_bury_coeff = glo_scalar_cnt_interior_tendency
    end if

  end subroutine marbl_glo_avg_var_cnts_compute

  !***********************************************************************

  subroutine marbl_glo_avg_init_rmean_vals( &
       glo_avg_rmean_interior,              &
       glo_avg_rmean_surface,               &
       glo_scalar_rmean_interior,           &
       glo_scalar_rmean_surface)

    use marbl_interface_public_types, only : marbl_running_mean_0d_type
    use marbl_settings_mod, only : parm_init_POC_bury_coeff
    use marbl_settings_mod, only : parm_init_POP_bury_coeff
    use marbl_settings_mod, only : parm_init_bSi_bury_coeff
    use marbl_settings_mod, only : bury_coeff_rmean_timescale_years
    use marbl_settings_mod, only : ladjust_bury_coeff
    use marbl_settings_mod, only : init_bury_coeff_opt

    type(marbl_running_mean_0d_type), intent(out) :: glo_avg_rmean_interior(:)
    type(marbl_running_mean_0d_type), intent(out) :: glo_avg_rmean_surface(:)
    type(marbl_running_mean_0d_type), intent(out) :: glo_scalar_rmean_interior(:)
    type(marbl_running_mean_0d_type), intent(out) :: glo_scalar_rmean_surface(:)

    !-----------------------------------------------------------------------

    integer (int_kind) :: glo_ind
    real (r8)          :: bury_coeff_rmean_timescale_sec

    real (r8) :: rmean_CaCO3_bury_integral
    real (r8) :: rmean_C_input_integral
    real (r8) :: rmean_P_input_integral
    real (r8) :: rmean_Si_input_integral

    real (r8) :: rmean_POC_bury_integral
    real (r8) :: rmean_d_POC_bury_d_bury_coeff_integral

    real (r8) :: rmean_POP_bury_integral
    real (r8) :: rmean_d_POP_bury_d_bury_coeff_integral

    real (r8) :: rmean_bSi_bury_integral
    real (r8) :: rmean_d_bSi_bury_d_bury_coeff_integral

    !-----------------------------------------------------------------------

    if (ladjust_bury_coeff) then
       ! FIXME : change 365*spd to spy
       bury_coeff_rmean_timescale_sec = bury_coeff_rmean_timescale_years * 365.0_r8 * spd

       glo_avg_rmean_interior(:)%timescale       = bury_coeff_rmean_timescale_sec
       glo_avg_rmean_interior(:)%linit_by_val    = (init_bury_coeff_opt == 'settings_file')

       glo_avg_rmean_surface(:)%timescale        = bury_coeff_rmean_timescale_sec
       glo_avg_rmean_surface(:)%linit_by_val     = (init_bury_coeff_opt == 'settings_file')

       glo_scalar_rmean_interior(:)%timescale    = bury_coeff_rmean_timescale_sec
       glo_scalar_rmean_interior(:)%linit_by_val = (init_bury_coeff_opt == 'settings_file')

       glo_scalar_rmean_surface(:)%timescale     = bury_coeff_rmean_timescale_sec
       glo_scalar_rmean_surface(:)%linit_by_val  = (init_bury_coeff_opt == 'settings_file')


       ! these initial values are only used if linit_by_val is .true.
       ! (i.e., if init_bury_coeff_opt == 'settings_file')
       ! However, for simpler code we always set them

!      rmean_ALK_nonN_input_integral = 1.62e-4_r8 ! GNEWS2000 value on gx1v6 grid [neq/cm^2/s]
       rmean_CaCO3_bury_integral     = 1.62e-4_r8 ! matches rmean_ALK_nonN_input_integral
       rmean_C_input_integral        = 2.69e-4_r8 ! GNEWS2000 value on gx1v6 grid [nmol C/cm^2/s]
       rmean_P_input_integral        = 9.66e-7_r8 ! GNEWS2000 value on gx1v6 grid [nmol P/cm^2/s]
       rmean_Si_input_integral       = 4.10e-5_r8 ! GNEWS2000 value on gx1v6 grid [nmol Si/cm^2/s]

       rmean_POC_bury_integral = (rmean_C_input_integral - rmean_CaCO3_bury_integral)
       rmean_d_POC_bury_d_bury_coeff_integral = rmean_POC_bury_integral / parm_init_POC_bury_coeff

       rmean_POP_bury_integral = rmean_P_input_integral
       rmean_d_POP_bury_d_bury_coeff_integral = rmean_POP_bury_integral / parm_init_POP_bury_coeff

       rmean_bSi_bury_integral = rmean_Si_input_integral
       rmean_d_bSi_bury_d_bury_coeff_integral = rmean_bSi_bury_integral / parm_init_bSi_bury_coeff


       glo_ind = glo_avg_field_ind_interior_tendency_CaCO3_bury
       glo_avg_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_avg_CaCO3_bury'
       glo_avg_rmean_interior(glo_ind)%init_val = rmean_CaCO3_bury_integral

       glo_ind = glo_avg_field_ind_interior_tendency_POC_bury
       glo_avg_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_avg_POC_bury'
       glo_avg_rmean_interior(glo_ind)%init_val = rmean_POC_bury_integral

       glo_ind = glo_avg_field_ind_interior_tendency_POP_bury
       glo_avg_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_avg_POP_bury'
       glo_avg_rmean_interior(glo_ind)%init_val = rmean_POP_bury_integral

       glo_ind = glo_avg_field_ind_interior_tendency_bSi_bury
       glo_avg_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_avg_bSi_bury'
       glo_avg_rmean_interior(glo_ind)%init_val = rmean_bSi_bury_integral

       glo_ind = glo_avg_field_ind_interior_tendency_d_POC_bury_d_bury_coeff
       glo_avg_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_avg_d_POC_bury_d_bury_coeff'
       glo_avg_rmean_interior(glo_ind)%init_val = rmean_d_POC_bury_d_bury_coeff_integral

       glo_ind = glo_avg_field_ind_interior_tendency_d_POP_bury_d_bury_coeff
       glo_avg_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_avg_d_POP_bury_d_bury_coeff'
       glo_avg_rmean_interior(glo_ind)%init_val = rmean_d_POP_bury_d_bury_coeff_integral

       glo_ind = glo_avg_field_ind_interior_tendency_d_bSi_bury_d_bury_coeff
       glo_avg_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_avg_d_bSi_bury_d_bury_coeff'
       glo_avg_rmean_interior(glo_ind)%init_val = rmean_d_bSi_bury_d_bury_coeff_integral


       glo_ind = glo_avg_field_ind_surface_flux_C_input
       glo_avg_rmean_surface(glo_ind)%sname    = 'MARBL_rmean_glo_avg_C_input'
       glo_avg_rmean_surface(glo_ind)%init_val = rmean_C_input_integral

       glo_ind = glo_avg_field_ind_surface_flux_P_input
       glo_avg_rmean_surface(glo_ind)%sname    = 'MARBL_rmean_glo_avg_P_input'
       glo_avg_rmean_surface(glo_ind)%init_val = rmean_P_input_integral

       glo_ind = glo_avg_field_ind_surface_flux_Si_input
       glo_avg_rmean_surface(glo_ind)%sname    = 'MARBL_rmean_glo_avg_Si_input'
       glo_avg_rmean_surface(glo_ind)%init_val = rmean_Si_input_integral


       glo_ind = glo_scalar_ind_interior_tendency_POC_bury_coeff
       glo_scalar_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_scalar_POC_bury_coeff'
       glo_scalar_rmean_interior(glo_ind)%init_val = parm_init_POC_bury_coeff

       glo_ind = glo_scalar_ind_interior_tendency_POP_bury_coeff
       glo_scalar_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_scalar_POP_bury_coeff'
       glo_scalar_rmean_interior(glo_ind)%init_val = parm_init_POP_bury_coeff

       glo_ind = glo_scalar_ind_interior_tendency_bSi_bury_coeff
       glo_scalar_rmean_interior(glo_ind)%sname    = 'MARBL_rmean_glo_scalar_bSi_bury_coeff'
       glo_scalar_rmean_interior(glo_ind)%init_val = parm_init_bSi_bury_coeff
    end if

  end subroutine marbl_glo_avg_init_rmean_vals

  !*****************************************************************************

end module marbl_glo_avg_mod