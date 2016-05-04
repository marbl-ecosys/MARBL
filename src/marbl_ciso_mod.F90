! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_ciso_mod

  !-----------------------------------------------------------------------
  !  Carbon 13 module and biotic 14C module
  !  13C code is based on code form G. Xavier, ETH, 2010, which
  !  was written for pop1 (CCSM3)
  !  This code needs the ecosystem model to run, as it uses several
  !  variables computed there. Data is shared using marbl_share_mod.F90
  !  This module adds 7 carbon pools for 13C and another 7 for 14C
  !-----------------------------------------------------------------------

  use marbl_kinds_mod       , only : r8
  use marbl_kinds_mod       , only : int_kind
  use marbl_kinds_mod       , only : log_kind
  use marbl_kinds_mod       , only : char_len

  use marbl_parms           , only : c0
  use marbl_parms           , only : c1
  use marbl_parms           , only : c2
  use marbl_parms           , only : c1000
  use marbl_parms           , only : mpercm
  use marbl_parms           , only : autotrophs  
  use marbl_parms           , only : zooplankton 
  use marbl_parms           , only : grazing     

  use marbl_sizes           , only : autotroph_cnt
  use marbl_sizes           , only : zooplankton_cnt
  use marbl_sizes           , only : grazer_prey_cnt

  use marbl_logging         , only : marbl_log_type

  use marbl_interface_types , only : marbl_tracer_metadata_type
  use marbl_interface_types , only : marbl_diagnostics_type
  use marbl_interface_types , only : marbl_domain_type
  use marbl_interface_types , only : marbl_interior_forcing_input_type

  use marbl_internal_types  , only : autotroph_type
  use marbl_internal_types  , only : column_sinking_particle_type
  use marbl_internal_types  , only : marbl_interior_share_type
  use marbl_internal_types  , only : marbl_zooplankton_share_type
  use marbl_internal_types  , only : marbl_autotroph_share_type
  use marbl_internal_types  , only : marbl_particulate_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_share_type
  use marbl_internal_types , only : marbl_tracer_index_type
  
  implicit none
  private

  !-----------------------------------------------------------------------
  !  public/private declarations
  !-----------------------------------------------------------------------

  public  :: marbl_ciso_init_nml
  public  :: marbl_ciso_init_tracer_metadata
  public  :: marbl_ciso_set_interior_forcing
  public  :: marbl_ciso_set_surface_forcing

  private :: setup_cell_attributes
  private :: setup_local_column_tracers
  private :: setup_local_autotrophs
  private :: fract_keller_morel
  private :: init_particulate_terms
  private :: compute_particulate_terms

  type, private :: autotroph_local_type
     real (r8) :: C13     ! local copy of model autotroph C13
     real (r8) :: C14     ! local copy of model autotroph C14 
     real (r8) :: Ca13CO3 ! local copy of model autotroph Ca13CO3
     real (r8) :: Ca14CO3 ! local copy of model autotroph Ca14CO3
  end type autotroph_local_type

  ! Using scaled isotopic carbon pools, so Rstd =1
  real(r8), parameter :: R13C_std = 1.0_r8 ! actual 13C/12C PDB standard ratio (Craig, 1957) = 1123.72e-5_r8
  real(r8), parameter :: R14C_std = 1.0_r8 ! actual 14C/12C NOSAMS standard ratio = 11.76e-13_r8

  ! Module variables
  real(r8) :: pi

  !-----------------------------------------------------------------------
  !  scalar constants for 14C decay calculation
  !-----------------------------------------------------------------------

   real (r8), parameter :: c14_halflife_years = 5730.0_r8 !C14 half file
   real (r8) :: c14_lambda_inv_sec           ! Decay variable in seconds

  !***********************************************************************

contains

  !*****************************************************************************

  subroutine marbl_ciso_init_nml(nl_buffer, marbl_status_log)

    use marbl_namelist_mod        , only : marbl_nl_in_size
    use marbl_namelist_mod        , only : marbl_nl_cnt
    use marbl_namelist_mod        , only : marbl_nl_buffer_size
    use marbl_namelist_mod        , only : marbl_nl_split_string
    use marbl_namelist_mod        , only : marbl_namelist
    use marbl_share_mod           , only : ciso_tracer_cnt
    use marbl_share_mod           , only : ciso_init_ecosys_option
    use marbl_share_mod           , only : ciso_init_ecosys_init_file
    use marbl_share_mod           , only : ciso_init_ecosys_init_file_fmt
    use marbl_share_mod           , only : ciso_tracer_init_ext
    use marbl_share_mod           , only : ciso_lsource_sink
    use marbl_share_mod           , only : ciso_atm_d13c_opt
    use marbl_share_mod           , only : ciso_atm_d13c_const
    use marbl_share_mod           , only : ciso_atm_d13c_filename
    use marbl_share_mod           , only : ciso_atm_d14c_opt
    use marbl_share_mod           , only : ciso_atm_d14c_const
    use marbl_share_mod           , only : ciso_atm_d14c_filename
    use marbl_share_mod           , only : ciso_fract_factors
    use marbl_share_mod           , only : ciso_atm_model_year
    use marbl_share_mod           , only : ciso_atm_data_year
    use marbl_share_mod           , only : ciso_lecovars_full_depth_tavg 
    use marbl_share_mod           , only : marbl_freq_opt_never  
    use marbl_share_mod           , only : marbl_freq_opt_nmonth 
    use marbl_share_mod           , only : marbl_freq_opt_nyear  

    implicit none

    character(marbl_nl_buffer_size) , intent(in)    :: nl_buffer(marbl_nl_cnt)
    type(marbl_log_type)            , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_ciso_mod:marbl_ciso_init_nml'
    character(len=char_len) :: log_message

    integer (int_kind) ::               &
         n,                             & ! index
         nml_error                        ! namelist i/o error flag

    character(len=marbl_nl_buffer_size) :: &
         tmp_nl_buffer

    ! ecosys_ciso_nml namelist
    namelist /ecosys_ciso_nml/ &
         ciso_init_ecosys_option, ciso_init_ecosys_init_file, &
         ciso_init_ecosys_init_file_fmt, ciso_tracer_init_ext, &
         ciso_lsource_sink, &
         ciso_lecovars_full_depth_tavg, &
         ciso_atm_d13c_opt, ciso_atm_d13c_const, ciso_atm_d13c_filename, &
         ciso_atm_d14c_opt, ciso_atm_d14c_const, ciso_atm_d14c_filename, &
         ciso_fract_factors, ciso_atm_model_year, ciso_atm_data_year
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  default namelist settings
    !-----------------------------------------------------------------------

    ciso_init_ecosys_option                 = 'unknown'
    ciso_init_ecosys_init_file              = 'unknown'
    ciso_init_ecosys_init_file_fmt          = 'bin'
    do n = 1,ciso_tracer_cnt
       ciso_tracer_init_ext(n)%mod_varname  = 'unknown'
       ciso_tracer_init_ext(n)%filename     = 'unknown'
       ciso_tracer_init_ext(n)%file_varname = 'unknown'
       ciso_tracer_init_ext(n)%scale_factor = c1
       ciso_tracer_init_ext(n)%default_val  = c0
       ciso_tracer_init_ext(n)%file_fmt     = 'bin'
    end do

    ciso_lsource_sink                       = .true.

    ciso_atm_d13c_opt                       = 'const'
    ciso_atm_d13c_const                     = -6.379_r8
    ciso_atm_d13c_filename                  = 'unknown'

    ciso_atm_d14c_opt                       = 'const'
    ciso_atm_d14c_const                     = 0.0_r8
    ciso_atm_d14c_filename(1)               = 'unknown'
    ciso_atm_d14c_filename(2)               = 'unknown'
    ciso_atm_d14c_filename(3)               = 'unknown'

    ciso_fract_factors                      = 'Rau'

    ciso_lecovars_full_depth_tavg           = .false.

    ciso_atm_model_year                     = 1
    ciso_atm_data_year                      = 1

    !-----------------------------------------------------------------------
    ! read the namelist buffer on every processor
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'ecosys_ciso_nml')
    read(tmp_nl_buffer, nml=ecosys_ciso_nml, iostat=nml_error)
    if (nml_error /= 0) then
       call marbl_status_log%log_error("error reading &ecosys_ciso_nml", subname)
       return
    else
       ! FIXME #16: this is printing contents of pop_in, not the entire ecosys_ciso_nml
       call marbl_status_log%log_namelist('ecosys_ciso_nml', tmp_nl_buffer, subname)
    end if

  end subroutine marbl_ciso_init_nml

  !*****************************************************************************
  
  subroutine marbl_ciso_init_tracer_metadata(marbl_tracer_metadata,           &
                                             marbl_tracer_read,               &
                                             marbl_tracer_indices)

    !  Set tracer and forcing metadata

    use marbl_interface_types, only : marbl_tracer_read_type
    use marbl_share_mod      , only : ciso_lecovars_full_depth_tavg
    use marbl_share_mod      , only : ciso_init_ecosys_init_file
    use marbl_share_mod      , only : ciso_init_ecosys_init_file_fmt

    implicit none

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type (marbl_tracer_read_type)     , intent(inout) :: marbl_tracer_read(:)
    type(marbl_tracer_index_type)     , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_ciso_mod:marbl_ciso_init_tracer_metadata'
    character(len=char_len) :: log_message

    integer (int_kind) :: n                             ! tracer index
    integer (int_kind) :: auto_ind                      ! autotroph functional group index
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  initialize non-autotroph metadata values
    !-----------------------------------------------------------------------

    associate(di13c_ind         => marbl_tracer_indices%di13c_ind,            &
              do13c_ind         => marbl_tracer_indices%do13c_ind,            &
              zoo13c_ind        => marbl_tracer_indices%zoo13c_ind,           &
              di14c_ind         => marbl_tracer_indices%di14c_ind,            &
              do14c_ind         => marbl_tracer_indices%do14c_ind,            &
              zoo14c_ind        => marbl_tracer_indices%zoo14c_ind,           &
              ciso_ind_beg      => marbl_tracer_indices%ciso_ind_beg,         &
              ciso_ind_end      => marbl_tracer_indices%ciso_ind_end          &
             )

    ! All CISO tracers share units, tend_units, flux_units, and
    ! tracer_module_name
    do n=ciso_ind_beg,ciso_ind_end
      marbl_tracer_metadata(n)%units      = 'mmol/m^3'
      marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
      marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
      marbl_tracer_metadata(n)%tracer_module_name = 'ciso'
    end do
    marbl_tracer_metadata(di13c_ind)%short_name='DI13C'
    marbl_tracer_metadata(di13c_ind)%long_name='Dissolved Inorganic Carbon-13'

    marbl_tracer_metadata(do13c_ind)%short_name='DO13C'
    marbl_tracer_metadata(do13c_ind)%long_name='Dissolved Organic Carbon-13'

    marbl_tracer_metadata(zoo13C_ind)%short_name='zoo13C'
    marbl_tracer_metadata(zoo13C_ind)%long_name='Zooplankton Carbon-13'

    marbl_tracer_metadata(di14c_ind)%short_name='DI14C'
    marbl_tracer_metadata(di14c_ind)%long_name='Dissolved Inorganic Carbon-14'

    marbl_tracer_metadata(do14c_ind)%short_name='DO14C'
    marbl_tracer_metadata(do14c_ind)%long_name='Dissolved Organic Carbon-14'

    marbl_tracer_metadata(zoo14C_ind)%short_name='zoo14C'
    marbl_tracer_metadata(zoo14C_ind)%long_name='Zooplankton Carbon-14'

    !-----------------------------------------------------------------------
    !  initialize autotroph tracer_d values and tracer indices
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
       marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // '13C'
       marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Carbon-13'

       n = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
       marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // '14C'
       marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Carbon-14'

       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
       if (n.gt.0) then
          marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Ca13CO3'
          marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Ca13CO3'
        end if

       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
       if (n.gt.0) then
          marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Ca14CO3'
          marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Ca14CO3'
       endif
    end do

    !-----------------------------------------------------------------------
    !  set lfull_depth_tavg flag for short-lived ecosystem tracers
    !-----------------------------------------------------------------------

    marbl_tracer_metadata(zoo13C_ind   )%lfull_depth_tavg = ciso_lecovars_full_depth_tavg
    marbl_tracer_metadata(zoo14C_ind   )%lfull_depth_tavg = ciso_lecovars_full_depth_tavg

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg
       endif
    end do

    do n=ciso_ind_beg,ciso_ind_end
      marbl_tracer_read(n)%mod_varname  = marbl_tracer_metadata(n)%short_name
      marbl_tracer_read(n)%filename     = ciso_init_ecosys_init_file
      marbl_tracer_read(n)%file_varname = marbl_tracer_metadata(n)%short_name
      marbl_tracer_read(n)%file_fmt     = ciso_init_ecosys_init_file_fmt
      marbl_tracer_read(n)%scale_factor = c1
      marbl_tracer_read(n)%default_val  = c0
    end do

    end associate

  end subroutine marbl_ciso_init_tracer_metadata

  !***********************************************************************

  subroutine marbl_ciso_set_interior_forcing( &
       marbl_domain,                          &
       marbl_interior_forcing_input,          &
       marbl_interior_share,                  &
       marbl_zooplankton_share,               &
       marbl_autotroph_share,                 &
       marbl_particulate_share,               &
       column_tracer,                         &
       column_dtracer,                        &
       marbl_tracer_indices,                  &
       marbl_interior_diags,                  &
       marbl_status_log)

    !  Compute time derivatives for 13C and 14C state variables.
    !  13C code is based on code from X. Giraud, ETH ZÃ¼rich, 2008, for pop1
    !  Also added biotic 14C

    use marbl_share_mod        , only : ciso_lsource_sink
    use marbl_share_mod        , only : ciso_fract_factors
    use marbl_parms            , only : f_graze_CaCO3_REMIN
    use marbl_parms            , only : R13c_std, R14c_std
    use marbl_parms            , only : spd
    use marbl_diagnostics_mod  , only : store_diagnostics_ciso_interior

    implicit none

    type(marbl_domain_type)                 , intent(in)    :: marbl_domain                               
    type(marbl_interior_forcing_input_type) , intent(in)    :: marbl_interior_forcing_input
    ! FIXME #17: intent is inout due to DIC_Loc
    type(marbl_interior_share_type)         , intent(inout) :: marbl_interior_share(marbl_domain%km)
    type(marbl_zooplankton_share_type)      , intent(in)    :: marbl_zooplankton_share(zooplankton_cnt, marbl_domain%km)
    type(marbl_autotroph_share_type)        , intent(in)    :: marbl_autotroph_share(autotroph_cnt, marbl_domain%km)
    type(marbl_particulate_share_type)      , intent(inout) :: marbl_particulate_share
    real (r8)                               , intent(in)    :: column_tracer(:,:)
    real (r8)                               , intent(inout) :: column_dtracer(:,:)  ! computed source/sink terms (inout because we don't touch non-ciso tracers)
    type(marbl_tracer_index_type)           , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)            , intent(inout) :: marbl_interior_diags
    type(marbl_log_type)                    , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_ciso_mod:marbl_ciso_set_interior forcing'

    logical (log_kind) :: zero_mask

    real (r8) :: &
         marbl_seconds_in_year, & 
         work1,     & ! temporaries
         ztop         ! depth of top of cell

    integer (int_kind) :: &
         n,m,            & ! tracer index
         auto_ind,       & ! autotroph functional group index
         k                 ! index for looping over k levels

    type(autotroph_local_type), dimension(autotroph_cnt, marbl_domain%km) :: &
         autotroph_loc

    real (r8), dimension(autotroph_cnt) :: &
         cell_active_C_uptake,  & ! ratio of active carbon uptake to carbon fixation
         cell_active_C,         & ! ratio of active carbon uptake to carbon fixation
         cell_surf,             & ! surface areas of cells ( m2 )
         cell_carb_cont,        & ! cell carbon content ( mol C cell-1 )
         cell_radius,           & ! cell radius ( um )
         cell_permea,           & ! cell wall permeability to CO2(aq) (m/s)
         cell_eps_fix             ! fractionation effect of carbon fixation

    real(r8), parameter :: &
         eps_carb = -2.0_r8      ! eps_carb = d13C(CaCO3) - d13C(DIC)  Ziveri et al., 2003

    type(column_sinking_particle_type) :: &   
         PO13C,          & ! base units = nmol 13C
         PO14C,          & ! base units = nmol 14C
         P_Ca13CO3,      & ! base units = nmol CaCO3 13C
         P_Ca14CO3         ! base units = nmol CaCO3 14C

    real (r8), dimension (marbl_domain%km) :: &
         DO13C_loc,         & ! local copy of model DO13C
         DI13C_loc,         & ! local copy of model DI13C
         zoo13C_loc,        & ! local copy of model zoo13C
         DO14C_loc,         & ! local copy of model DO14C
         DI14C_loc,         & ! local copy of model DI14C
         zoo14C_loc           ! local copy of model zoo14C

    real (r8), dimension(marbl_domain%km) :: &
         mui_to_co2star_loc     ! local carbon autotroph instanteous growth rate over [CO2*] (m^3 /mol C /s)

    real (r8), dimension(marbl_domain%km) :: &
         R13C_CaCO3_PROD,   & ! 13C/12C in CaCO3 production of small phyto
         R13C_CO2STAR,      & ! 13C/12C in CO2* water
         R13C_DIC,          & ! 13C/12C in total DIC
         R13C_DOC,          & ! 13C/12C in total DOC
         R13C_zooC,         & ! 13C/12C in total zooplankton
         R14C_CaCO3_PROD,   & ! 14C/12C in CaCO3 production of small phyto
         R14C_CO2STAR,      & ! 14C/12C in CO2* water
         R14C_DIC,          & ! 14C/12C in total DIC
         R14C_DOC,          & ! 14C/12C in total DOC
         R14C_zooC            ! 14C/12C in total zooplankton

    real (r8), dimension(autotroph_cnt, marbl_domain%km) :: &
         Ca13CO3_PROD,        & ! prod. of 13C CaCO3 by small phyto (mmol CaCO3/m^3/sec)
         Ca14CO3_PROD,        & ! prod. of 13C CaCO3 by small phyto (mmol CaCO3/m^3/sec)
         eps_autotroph,       & ! Permil fractionation (or discrimination factor) for Carbon autotroph types sp, diat, diaz
         mui_to_co2star,      & ! Carbon autotroph instanteous growth rate over [CO2*] (m^3 /mmol C /s)
         R13C_photoC,         & ! 13C/12C in Carbon autotroph C-fixation (mmol C/m^3/sec)
         R13C_autotroph,      & ! 13C/12C in total small phytoplankton
         photo13C,            & ! Carbon autotroph 13C-fixation (mmol C/m^3/sec)
         photo14C,            & ! Carbon autotroph 14C-fixation (mmol C/m^3/sec)
         R14C_photoC,         & ! 14C/12C in Carbon autotroph C-fixation (mmol C/m^3/sec)
         R14C_autotroph,      & ! 14C/12C in total small phytoplankton
         autotrophCaCO3_d13C, & ! d13C of autotrophCaCO3
         autotrophCaCO3_d14C, & ! d14C of autotrophCaCO3
         autotroph_d13C,      & ! d13C of autotroph C
         autotroph_d14C,      & ! d14C of autotroph C
         R13C_autotrophCaCO3, & ! 13C/12C in total small phytoplankton carbonate
         R14C_autotrophCaCO3    ! 14C/12C in total small phytoplankton carbonate

    real (r8), dimension(marbl_domain%km) :: &
         frac_co3,          & ! carbonate fraction fCO3 = [CO3--]/DIC
         CO2STAR_int,       & ! [CO2*] water (mmol/m^3) in interior domain (not only surface)
         DO13C_prod,        & ! production of 13C DOC (mmol C/m^3/sec)
         DO13C_remin,       & ! remineralization of 13C DOC (mmol C/m^3/sec)
         eps_aq_g,          & ! equilibrium fractionation (CO2_gaseous <-> CO2_aq)
         eps_dic_g,         & ! equilibrium fractionation between total DIC and gaseous CO2
         alpha_aq_g,        & ! eps = ( alpa -1 ) * 1000
         alpha_dic_g,       & ! eps = ( alpa -1 ) * 1000
         delta_C13_Corg,    & ! deltaC13 of Net Primary Production
         delta_C13_CO2STAR, & ! deltaC13 of CO2*
         DO14C_prod,        & ! production of 13C DOC (mmol C/m^3/sec)
         DO14C_remin,       & ! remineralization of 13C DOC (mmol C/m^3/sec)
         alpha_aq_g_14c,    & ! alpha for 14C, with fractionation twice as large as for 13C
         alpha_dic_g_14c,   & ! alpha for 14C, with fractionation twice as large as for 13C
         delta_C14_Corg,    & ! deltaC14 of Net Primary Production
         delta_C14_CO2STAR, & ! deltaC14 of CO2*
         DIC_d13C,          & ! d13C of DIC
         DOC_d13C,          & ! d13C of DOC
         zooC_d13C,         & ! d13C of zooC
         DIC_d14C,          & ! d14C of DIC
         DOC_d14C,          & ! d14C of DOC
         zooC_d14C            ! d14C of zooC
    !-------------------------------------------------------------

    associate(                                                                   &
         column_km          => marbl_domain%km                                 , &
         column_kmt         => marbl_domain%kmt                                , &
         column_delta_z     => marbl_domain%delta_z                            , &
         column_zw          => marbl_domain%zw                                 , &

         temperature        => marbl_interior_forcing_input%temperature        , &

         DIC_loc            => marbl_interior_share%DIC_loc_fields             , & ! INPUT local copy of model DIC                                                       
         DOC_loc            => marbl_interior_share%DOC_loc_fields             , & ! INPUT local copy of model DOC                                                       
         O2_loc             => marbl_interior_share%O2_loc_fields              , & ! INPUT local copy of model O2                                                        
         NO3_loc            => marbl_interior_share%NO3_loc_fields             , & ! INPUT local copy of model NO3                                                       
         CO3                => marbl_interior_share%CO3_fields                 , & ! INPUT carbonate ion                                                                 
         HCO3               => marbl_interior_share%HCO3_fields                , & ! INPUT bicarbonate ion                                                               
         H2CO3              => marbl_interior_share%H2CO3_fields               , & ! INPUT carbonic acid                                                                 
         DOC_remin          => marbl_interior_share%DOC_remin_fields           , & ! INPUT remineralization of 13C DOC (mmol C/m^3/sec)

         autotrophCaCO3_loc => marbl_autotroph_share%autotrophCaCO3_loc_fields , & ! INPUT local copy of model autotroph CaCO3                                           
         autotrophC_loc     => marbl_autotroph_share%autotrophC_loc_fields     , & ! INPUT local copy of model autotroph C
         QCaCO3             => marbl_autotroph_share%QCaCO3_fields             , & ! INPUT small phyto CaCO3/C ratio (mmol CaCO3/mmol C)                                 
         auto_graze         => marbl_autotroph_share%auto_graze_fields         , & ! INPUT autotroph grazing rate (mmol C/m^3/sec)                                       
         auto_graze_zoo     => marbl_autotroph_share%auto_graze_zoo_fields     , & ! INPUT auto_graze routed to zoo (mmol C/m^3/sec)                                     
         auto_graze_poc     => marbl_autotroph_share%auto_graze_poc_fields     , & ! INPUT auto_graze routed to poc (mmol C/m^3/sec)                                     
         auto_graze_doc     => marbl_autotroph_share%auto_graze_doc_fields     , & ! INPUT auto_graze routed to doc (mmol C/m^3/sec)                                     
         auto_graze_dic     => marbl_autotroph_share%auto_graze_dic_fields     , & ! INPUT auto_graze routed to dic (mmol C/m^3/sec)                                     
         auto_loss          => marbl_autotroph_share%auto_loss_fields          , & ! INPUT autotroph non-grazing mort (mmol C/m^3/sec)                                   
         auto_loss_poc      => marbl_autotroph_share%auto_loss_poc_fields      , & ! INPUT auto_loss routed to poc (mmol C/m^3/sec)                                      
         auto_loss_doc      => marbl_autotroph_share%auto_loss_doc_fields      , & ! INPUT auto_loss routed to doc (mmol C/m^3/sec)                                      
         auto_loss_dic      => marbl_autotroph_share%auto_loss_dic_fields      , & ! INPUT auto_loss routed to dic (mmol C/m^3/sec)                                      
         auto_agg           => marbl_autotroph_share%auto_agg_fields           , & ! INPUT autotroph aggregation (mmol C/m^3/sec)                                        
         photoC             => marbl_autotroph_share%photoC_fields             , & ! INPUT C-fixation (mmol C/m^3/sec)                                                   
         CaCO3_PROD         => marbl_autotroph_share%CaCO3_PROD_fields         , & ! INPUT prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)                            
         PCphoto            => marbl_autotroph_share%PCphoto_fields            , & ! INPUT C-specific rate of photosynth. (1/sec)                                        

         zooC_loc           => marbl_zooplankton_share%zooC_loc_fields         , & ! INPUT local copy of model zooC                                                      
         zoo_loss           => marbl_zooplankton_share%zoo_loss_fields         , & ! INPUT mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)            
         zoo_loss_poc       => marbl_zooplankton_share%zoo_loss_poc_fields     , & ! INPUT zoo_loss routed to large detrital pool (mmol C/m^3/sec)                       
         zoo_loss_doc       => marbl_zooplankton_share%zoo_loss_doc_fields     , & ! INPUT zoo_loss routed to doc (mmol C/m^3/sec)                                       
         zoo_loss_dic       => marbl_zooplankton_share%zoo_loss_dic_fields     , & ! INPUT zoo_loss routed to dic (mmol C/m^3/sec)                                       

         POC                => marbl_particulate_share%POC                     , & ! INPUT
         P_CaCO3            => marbl_particulate_share%P_CaCO3                 , & ! INPUT

         di13c_ind          => marbl_tracer_indices%di13c_ind                  , &
         do13c_ind          => marbl_tracer_indices%do13c_ind                  , &
         zoo13c_ind         => marbl_tracer_indices%zoo13c_ind                 , &
         di14c_ind          => marbl_tracer_indices%di14c_ind                  , &
         do14c_ind          => marbl_tracer_indices%do14c_ind                  , &
         zoo14c_ind         => marbl_tracer_indices%zoo14c_ind                   &
         )

    !-----------------------------------------------------------------------
    ! Allocate memory for column_sinking_particle data types 
    !-----------------------------------------------------------------------
    call PO13C%construct(num_levels=column_km)
    call PO14C%construct(num_levels=column_km)
    call P_Ca13CO3%construct(num_levels=column_km)
    call P_Ca14CO3%construct(num_levels=column_km)

    !-----------------------------------------------------------------------
    ! Set module variables
    !-----------------------------------------------------------------------

    ! FIXME #36: move this calculations to the init phase when 
    !            the initialization is brought into this module

    pi  = 4.0_r8 * atan( 1.0_r8 )

    !  Define decay variable for DI14C, using earlier defined half-life of 14C
    marbl_seconds_in_year = 86400._r8 * 365._r8
    c14_lambda_inv_sec = log(c2) / (c14_halflife_years * marbl_seconds_in_year)

    !----------------------------------------------------------------------------------------
    ! Set cell attributes
    !----------------------------------------------------------------------------------------

    call setup_cell_attributes(ciso_fract_factors, &
       cell_active_C_uptake, cell_active_C, cell_surf, cell_carb_cont, &
       cell_radius, cell_permea, cell_eps_fix, marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace("setup_cell_attributes", subname)
       return
    end if

    !-----------------------------------------------------------------------
    !  Create local copies of model column_tracer, treat negative values as zero
    !-----------------------------------------------------------------------
    
    call setup_local_column_tracers(column_km, column_kmt, column_tracer,     &
           marbl_tracer_indices, DI13C_loc, DO13c_loc, zoo13C_loc, DI14C_loc, &
           DO14C_loc, zoo14C_loc)

    !-----------------------------------------------------------------------
    !  Create local copies of model column autotrophs, treat negative values as zero
    !-----------------------------------------------------------------------

    call setup_local_autotrophs(column_km, column_kmt, column_tracer, &
         marbl_tracer_indices, autotroph_loc)

    !-----------------------------------------------------------------------
    !  If any ecosys phyto box is zero, set others to zeros
    !-----------------------------------------------------------------------

    call marbl_autotroph_consistency_check(column_km, autotroph_cnt, autotrophs, &
         marbl_tracer_indices, marbl_autotroph_share, autotroph_loc)

    !-----------------------------------------------------------------------
    !  Initialize Particulate terms for k=1
    !-----------------------------------------------------------------------

    call init_particulate_terms(k=1, POC_ciso=PO13C, P_CaCO3_ciso=P_Ca13CO3)
    call init_particulate_terms(k=1, POC_ciso=PO14C, P_CaCO3_ciso=P_Ca14CO3)

    !-----------------------------------------------------------------------
    !  Set ratios
    !-----------------------------------------------------------------------

    do k = 1, column_km

       if (k > column_kmt) then
          DIC_loc(k) = c0
          DOC_loc(k) = c0
       end if

       !-----------------------------------------------------------------------
       !  set local 13C/12C ratios, assuming ecosystem carries 12C (C=C12+C13+C14)
       !  If any Carbon boxes are zero, set corresponding 13C to zeros.
       !-----------------------------------------------------------------------

       if (DOC_loc(k) > c0) then
          R13C_DOC(k) = DO13C_loc(k) / DOC_loc(k)
          R14C_DOC(k) = DO14C_loc(k) / DOC_loc(k)
       else
          R13C_DOC(k) = c0
          R14C_DOC(k) = c0
       end if

       if (DIC_loc(k) > c0) then
          R13C_DIC(k) = DI13C_loc(k) / DIC_loc(k)
          R14C_DIC(k) = DI14C_loc(k) / DIC_loc(k)
       else
          R13C_DIC(k) = c0
          R14C_DIC(k) = c0
       end if

       work1 = sum(zooC_loc(:,k),dim=1)
       if (work1 > c0) then
          R13C_zooC(k) = zoo13C_loc(k) / work1
          R14C_zooC(k) = zoo14C_loc(k) / work1
       else
          R13C_zooC(k) = c0
          R14C_zooC(k) = c0
       end if

       do auto_ind = 1, autotroph_cnt
          if (autotrophC_loc(auto_ind,k) > c0) then
             R13C_autotroph(auto_ind,k) = autotroph_loc(auto_ind,k)%C13 / autotrophC_loc(auto_ind,k)
             R14C_autotroph(auto_ind,k) = autotroph_loc(auto_ind,k)%C14 / autotrophC_loc(auto_ind,k)
          else
             R13C_autotroph(auto_ind,k) = c0
             R14C_autotroph(auto_ind,k) = c0
          end if

          if (autotrophCaCO3_loc(auto_ind,k) > c0) then
             R13C_autotrophCaCO3(auto_ind,k) = autotroph_loc(auto_ind,k)%Ca13CO3 / autotrophCaCO3_loc(auto_ind,k)
             R14C_autotrophCaCO3(auto_ind,k) = autotroph_loc(auto_ind,k)%Ca14CO3 / autotrophCaCO3_loc(auto_ind,k)
          else
             R13C_autotrophCaCO3(auto_ind,k) = c0
             R14C_autotrophCaCO3(auto_ind,k) = c0
          end if
       end do

       !-----------------------------------------------------------------------
       ! Calculate fraction of CO3
       !-----------------------------------------------------------------------

       if (k > column_kmt) then
          frac_co3(k) = c0
       else
          frac_co3(k) = CO3(k) / DIC_loc(k)
       end if

       !-----------------------------------------------------------------------
       !   discrimination factors of carbone chemistry based on
       !   Zhang et al, 1995, Geochim. et Cosmochim. Acta, 59 (1), 107-114
       !
       !   eps = permil fractionation and alpha is the fractionation factor
       !   with eps =(alpha - 1) *1000
       !
       !   Fractionation is twice as large for 14C compared to 13C
       !-----------------------------------------------------------------------

       eps_aq_g(k)   = 0.0049_r8 * temperature(k) - 1.31_r8
       eps_dic_g(k)  = 0.014_r8  * temperature(k) * frac_co3(k) - 0.105_r8 * temperature(k) + 10.53_r8

       alpha_aq_g(k)  = c1 + eps_aq_g(k)  / c1000
       alpha_dic_g(k) = c1 + eps_dic_g(k) / c1000

       !fractionation is twice as large for 14C compared to 13C
       alpha_aq_g_14c(k)  = c1 + eps_aq_g(k)  * 2.0_r8 / c1000
       alpha_dic_g_14c(k) = c1 + eps_dic_g(k) * 2.0_r8 / c1000

       !-----------------------------------------------------------------------
       !  13C/12C ratios of CO2* (CO2STAR)
       !-----------------------------------------------------------------------

       R13C_CO2STAR(k) = R13C_DIC(k) * alpha_aq_g(k) / alpha_dic_g(k)

       !-----------------------------------------------------------------------
       !  delta_13C of CO2* (CO2STAR)
       !-----------------------------------------------------------------------

       delta_C13_CO2STAR(k) = ( R13C_CO2STAR(k) / R13C_std - c1 ) * c1000

       !-----------------------------------------------------------------------
       !  14C/12C ratios of CO2* (CO2STAR)
       !-----------------------------------------------------------------------

       R14C_CO2STAR(k) = R14C_DIC(k) * alpha_aq_g_14c(k) / alpha_dic_g_14c(k)

       !-----------------------------------------------------------------------
       !  delta_14C of CO2* (CO2STAR)
       !-----------------------------------------------------------------------

       delta_C14_CO2STAR(k) = ( R14C_CO2STAR(k) / R14C_std - c1 ) * c1000

       !-----------------------------------------------------------------------
       !  [CO2STAR]  = [CO2*] = [CO2(aq)] + [H2CO3]
       !  (this is eq 1.1.1 in Zeebe and Wolf-Gladrow, CO2 in seawater:
       !  equilibrium, kinetics, isotopes, Elseview Oceanography Series 65)
       !
       !  DIC= [CO3] + [HCO3] + [CO2*] (eq 1.1.7)
       !
       !  => CO2STAR_int = DIC_loc - HCO3 - CO3 !
       !-----------------------------------------------------------------------

       CO2STAR_int(k) = DIC_loc(k) - HCO3(k) - CO3(k)

       !------------------------------------------------------------------------
       !  Loop over autotrophe types sp, diat, diaz and calculate fractionation
       !  for each type
       !------------------------------------------------------------------------

       do auto_ind = 1, autotroph_cnt

          !------------------------------------------------------------------------
          !   mu(i) / [CO2*]  of small phytoplankton ( m^3 / mmol C /s )
          !-----------------------------------------------------------------------

          if ( CO2STAR_int(k) /= c0 ) then
             mui_to_co2star(auto_ind,k) =  PCphoto(auto_ind,k) / CO2STAR_int(k)
          else
             mui_to_co2star(auto_ind,k) = c0
          end if

          !-----------------------------------------------------------------------
          !  fractionation factors for 13C fixation  against CO2* in
          !  authotrophe types (sp, diaz, diat)
          !-----------------------------------------------------------------------
          select case (ciso_fract_factors)

          case ('Rau')

             !-----------------------------------------------------------------------
             !   Rau et al., 1989 ( see Gruber et al., 1998 )
             !   with restriction between -19 and -31 permil (see Rau et al., 1989)
             !-----------------------------------------------------------------------

             delta_C13_Corg(k) = -0.8_r8 * CO2STAR_int(k) - 12.6_r8

             delta_C13_Corg(k) = min( delta_C13_Corg(k) , -18.0_r8 )
             delta_C13_Corg(k) = max( delta_C13_Corg(k) , -32.0_r8 )

             eps_autotroph(auto_ind,k) = c1000 * (delta_C13_CO2STAR(k) - delta_C13_Corg(k)) &
                  /(c1000 + delta_C13_Corg(k))

          case ('Laws')

             !-----------------------------------------------------------------------
             !   Laws et al, 1995
             !   with restriction between 10 and 26 for size effect (Tagliabue and Bopp, 2008)
             !   convert mui_to_co2star from m3/mmol/s to kg/mumol/d
             !-----------------------------------------------------------------------

             eps_autotroph(auto_ind,k) = (( mui_to_co2star(auto_ind,k) * spd ) - 0.371_r8 ) / (-0.015_r8)

             !--------------------------------------------------------------------------
             ! uncomment the following two lines to restrict eps_sp  between 10 and 26
             !--------------------------------------------------------------------------

             !         eps_autotroph(auto_ind,k) = min( eps_autotroph(auto_ind,k), 26.0_r8 )
             !         eps_autotroph(auto_ind,k) = max( eps_autotroph(auto_ind,k), 10.0_r8 )

          case ('KellerMorel')

             !-----------------------------------------------------------------------
             ! Keller and morel, 1999
             !-----------------------------------------------------------------------

             ! convert mui_to_co2start from m3/mmol/s to m3/mol/s
             mui_to_co2star_loc(k) = mui_to_co2star(auto_ind,k) * c1000

             call fract_keller_morel(             &
                  mui_to_co2star_loc(k),             &
                  cell_active_C_uptake(auto_ind), &
                  cell_surf(auto_ind),            &
                  cell_carb_cont(auto_ind),       &
                  cell_radius(auto_ind),          &
                  cell_permea(auto_ind),          &
                  cell_eps_fix(auto_ind),         &
                  eps_autotroph(auto_ind,k) )

          end select
          !-----------------------------------------------------------------------

          if (eps_autotroph(auto_ind,k) /= -c1000 ) then
             R13C_photoC(auto_ind,k) = R13C_CO2STAR(k) *c1000 / (eps_autotroph(auto_ind,k) + c1000)
             R14C_photoC(auto_ind,k) = R14C_CO2STAR(k) *c1000 / (2.0_r8* eps_autotroph(auto_ind,k) + c1000)
          else
             R13C_photoC(auto_ind,k) = c1
             R14C_photoC(auto_ind,k) = c1
          end if

          !-----------------------------------------------------------------------
          ! Use R13/14C_photoC to determine small phytoplankton, Diatom, and
          ! Diaztroph 13C and 14C fixation
          !-----------------------------------------------------------------------
          
          photo13C(auto_ind,k) = photoC(auto_ind,k) * R13C_photoC(auto_ind,k)
          photo14C(auto_ind,k) = photoC(auto_ind,k) * R14C_photoC(auto_ind,k)
          
          !-----------------------------------------------------------------------
          ! C13 & C14 CaCO3 production
          !-----------------------------------------------------------------------

          if (autotrophs(auto_ind)%imp_calcifier) then
             
             R13C_CaCO3_PROD(k) = R13C_DIC(k) + R13C_std * eps_carb / c1000 
             
             R14C_CaCO3_PROD(k) = R14C_DIC(k) + R14C_std * eps_carb * 2.0_r8 / c1000
             
             Ca13CO3_PROD(auto_ind,k) = CaCO3_PROD(auto_ind,k) * R13C_CaCO3_PROD(k)
             
             Ca14CO3_PROD(auto_ind,k) = CaCO3_PROD(auto_ind,k) * R14C_CaCO3_PROD(k)

          end if
             
       end do ! end loop over auto_ind

       !-----------------------------------------------------------------------
       !  compute terms for DO13C and DO14C
       !-----------------------------------------------------------------------
       
       DO13C_prod(k) = &
            sum(zoo_loss_doc(:,k),dim=1)*R13C_zooC(k) + &
            sum((auto_loss_doc(:,k) + auto_graze_doc(:,k)) * R13C_autotroph(:,k),dim=1)

       DO14C_prod(k) = &
            sum(zoo_loss_doc(:,k),dim=1)*R14C_zooC(k) + &
            sum((auto_loss_doc(:,k) + auto_graze_doc(:,k)) * R14C_autotroph(:,k),dim=1)
       
       DO13C_remin(k) = DOC_remin(k) * R13C_DOC(k)
       DO14C_remin(k) = DOC_remin(k) * R14C_DOC(k)
       
       !-----------------------------------------------------------------------
       !  large detritus 13C and 14C
       !-----------------------------------------------------------------------
       
       PO13C%prod(k) = &
            sum(zoo_loss_poc(:,k),dim=1)*R13C_zooC(k) + &
            sum((auto_graze_poc(:,k) + auto_agg(:,k) + auto_loss_poc(:,k)) * R13C_autotroph(:,k),dim=1)

       PO14C%prod(k) = &
            sum(zoo_loss_poc(:,k),dim=1)*R14C_zooC(k) + &
            sum((auto_graze_poc(:,k) + auto_agg(:,k) + auto_loss_poc(:,k)) * R14C_autotroph(:,k),dim=1)
       
       !-----------------------------------------------------------------------
       !  large detrital Ca13CO3 and Ca14CCO3
       !-----------------------------------------------------------------------
       
       do auto_ind = 1, autotroph_cnt
          if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
             P_Ca13CO3%prod(k) = P_CaCO3%prod(k) * R13C_autotrophCaCO3(auto_ind,k)
             P_Ca14CO3%prod(k) = P_CaCO3%prod(k) * R14C_autotrophCaCO3(auto_ind,k)
          endif
       end do
       
       !-----------------------------------------------------------------------
       !   Calculate oceanic D14C and D13C of carbon pools
       !-----------------------------------------------------------------------

       DIC_d13C(k) =  ( R13C_DIC(k) / R13C_std - c1 ) * c1000
       DIC_d14C(k) =  ( R14C_DIC(k) / R14C_std - c1 ) * c1000

       DOC_d13C(k) =  ( R13C_DOC(k) / R13C_std - c1 ) * c1000
       DOC_d14C(k) =  ( R14C_DOC(k) / R14C_std - c1 ) * c1000

       zooC_d13C(k)=  ( R13C_zooC(k) / R13C_std - c1 ) * c1000
       zooC_d14C(k)=  ( R14C_zooC(k) / R14C_std - c1 ) * c1000

       do auto_ind = 1, autotroph_cnt
          if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
             autotrophCaCO3_d13C(auto_ind,k) =  ( R13C_autotrophCaCO3(auto_ind,k) / R13C_std - c1 ) * c1000
             autotrophCaCO3_d14C(auto_ind,k) =  ( R14C_autotrophCaCO3(auto_ind,k) / R14C_std - c1 ) * c1000
          else
             autotrophCaCO3_d13C(auto_ind,k) =  c0
             autotrophCaCO3_d14C(auto_ind,k) =  c0
          endif

          autotroph_d13C(auto_ind,k) =  ( R13C_autotroph(auto_ind,k) / R13C_std - c1 ) * c1000
          autotroph_d14C(auto_ind,k) =  ( R14C_autotroph(auto_ind,k) / R14C_std - c1 ) * c1000
       end do ! end loop over auto_ind

       !-----------------------------------------------------------------------
       ! Compute carbon isotope particulate terms 
       !-----------------------------------------------------------------------

       call compute_particulate_terms(k, column_km, column_kmt, column_delta_z(k), column_zw(k), &
            O2_loc(k), NO3_loc(k), POC, P_CaCO3, marbl_particulate_share, PO13C, P_Ca13CO3)

       call compute_particulate_terms(k, column_km, column_kmt, column_delta_z(k), column_zw(k), &
            O2_loc(k), NO3_loc(k), POC, P_CaCO3, marbl_particulate_share, PO14C, P_Ca14CO3)

       !-----------------------------------------------------------------------
       ! Update column_dtracer for the 7 carbon pools for each Carbon isotope
       !-----------------------------------------------------------------------

       !-----------------------------------------------------------------------
       !  dtracrs: autotroph Carbon (3 carbon pools), autotroph Ca13CO3 and Ca14CO3
       !-----------------------------------------------------------------------
       
       do auto_ind = 1, autotroph_cnt
          work1 = auto_graze(auto_ind,k) + auto_loss(auto_ind,k) + auto_agg(auto_ind,k)

          n = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
          column_dtracer(n,k) = photo13C(auto_ind,k) - work1 * R13C_autotroph(auto_ind,k)

          n = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
          column_dtracer(n,k) = photo14C(auto_ind,k) - work1 * R14C_autotroph(auto_ind,k) - &
               c14_lambda_inv_sec * autotroph_loc(auto_ind,k)%C14

          n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
          if (n > 0) then
             column_dtracer(n,k) = Ca13CO3_PROD(auto_ind,k) - QCaCO3(auto_ind,k) &
                  * work1 * R13C_autotrophCaCO3(auto_ind,k)
          endif

          n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
          if (n > 0) then
             column_dtracer(n,k) = Ca14CO3_PROD(auto_ind,k) - QCaCO3(auto_ind,k) &
                  * work1 * R14C_autotrophCaCO3(auto_ind,k)      &
                  - c14_lambda_inv_sec * autotroph_loc(auto_ind,k)%Ca14CO3
          endif
       end do

       !-----------------------------------------------------------------------
       !  column_dtracer: zoo 13 and 14 Carbon
       !-----------------------------------------------------------------------

       column_dtracer(zoo13C_ind,k) = &
              sum(auto_graze_zoo(:,k) * R13C_autotroph(:,k),dim=1) &
            - sum(zoo_loss(:,k),dim=1) * R13C_zooC(k)

       column_dtracer(zoo14C_ind,k) = &
              sum(auto_graze_zoo(:,k) * R14C_autotroph(:,k),dim=1) &
            - sum(zoo_loss(:,k),dim=1) * R14C_zooC(k) &
            - c14_lambda_inv_sec * zoo14C_loc(k)

       !-----------------------------------------------------------------------
       !  column_dtracer: dissolved organic Matter 13C and 14C
       !-----------------------------------------------------------------------

       column_dtracer(do13c_ind,k) = DO13C_prod(k) - DO13C_remin(k)

       column_dtracer(do14c_ind,k) = DO14C_prod(k) - DO14C_remin(k) - c14_lambda_inv_sec * DO14C_loc(k)

       !-----------------------------------------------------------------------
       !   column_dtracer: dissolved inorganic Carbon 13 and 14
       !-----------------------------------------------------------------------

       column_dtracer(di13c_ind,k) =                                                       &
            sum( (auto_loss_dic(:,k) + auto_graze_dic(:,k)) * R13C_autotroph(:,k), dim=1 ) &
          - sum(photo13C(:,k),dim=1)                                                       &
          + DO13C_remin(k) + PO13C%remin(k)                                                &
          + sum(zoo_loss_dic(:,k),dim=1) * R13C_zooC(k)                                    &
          + P_Ca13CO3%remin(k)

       column_dtracer(di14c_ind,k) =                                                       &
            sum( (auto_loss_dic(:,k) + auto_graze_dic(:,k)) * R14C_autotroph(:,k), dim=1 ) &
          - sum(photo14C(:,k),dim=1)                                                       &
          + DO14C_remin(k) + PO14C%remin(k)                                                &
          + sum(zoo_loss_dic(:,k),dim=1) * R14C_zooC(k)                                    &
          + P_Ca14CO3%remin(k)                                                             &
          - c14_lambda_inv_sec * DI14C_loc(k)

       do auto_ind = 1, autotroph_cnt
          if (marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind > 0) then
             column_dtracer(di13c_ind,k) = column_dtracer(di13c_ind,k)            &
                  + f_graze_CaCO3_REMIN * auto_graze(auto_ind,k)                  &
                  * QCaCO3(auto_ind,k) * R13C_autotrophCaCO3(auto_ind,k)          &
                  - Ca13CO3_PROD(auto_ind,k)
          endif
          if (marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind > 0) then
             column_dtracer(di14c_ind,k) = column_dtracer(di14c_ind,k)            &
                  + f_graze_CaCO3_REMIN * auto_graze(auto_ind,k)                  &
                  * QCaCO3(auto_ind,k) * R14C_autotrophCaCO3(auto_ind,k)          &
                  - Ca14CO3_PROD(auto_ind,k)
          endif
       end do

       !-----------------------------------------------------------------------
       ! Update particulate terms from prior level for next level
       !-----------------------------------------------------------------------

       if  (k < column_km) then
          call marbl_update_particulate_terms_from_prior_level(k+1, PO13C, P_Ca13CO3)

          call marbl_update_particulate_terms_from_prior_level(k+1, PO14C, P_Ca14CO3)
       endif

    end do ! end of loop over k levels

    end associate

    ! update carbon isotope diagnostics 
    ! FIXME #18: the following arguments need to be group into a derived type

    call store_diagnostics_ciso_interior(&
       marbl_domain,        &
       autotroph_d13C,      &
       autotroph_d14C,      &
       autotrophCaCO3_d13C, &
       autotrophCaCO3_d14C, &
       DIC_d13C,            &
       DIC_d14C,            &
       DOC_d13C,            &
       DOC_d14C,            &
       zooC_d13C,           &
       zooC_d14C,           &
       photo13C,            &
       photo14C,            &
       eps_autotroph,       &
       mui_to_co2star,      &
       Ca13CO3_prod,        &
       Ca14CO3_prod,        &
       DO13C_prod,          &
       DO14C_prod,          &
       DO13C_remin,         &
       DO14C_remin,         &
       eps_aq_g,            &
       eps_dic_g,           &
       PO13C,               &
       PO14C,               &
       P_Ca13CO3,           &
       P_Ca14CO3,           &
       column_dtracer,      &
       marbl_tracer_indices,&
       marbl_interior_diags)

    !-----------------------------------------------------------------------
    ! Deallocate memory for column_sinking_particle data types 
    !-----------------------------------------------------------------------

    call PO13C%destruct()
    call PO14C%destruct()
    call P_Ca13CO3%destruct()
    call P_Ca14CO3%destruct()

  end subroutine marbl_ciso_set_interior_forcing

  !***********************************************************************

  subroutine setup_cell_attributes(ciso_fract_factors, &
       cell_active_C_uptake, cell_active_C, cell_surf, cell_carb_cont, &
       cell_radius, cell_permea, cell_eps_fix, marbl_status_log)

    !----------------------------------------------------------------------------------------
    ! For Keller and Morel, set cell attributes based on autotroph type (from observations)
    !----------------------------------------------------------------------------------------

    implicit none

    character(char_len) , intent(in)  :: ciso_fract_factors                  ! option for which biological fractionation calculation to use
    real (r8)           , intent(out) :: cell_active_C_uptake(autotroph_cnt) ! ratio of active carbon uptake to carbon fixation
    real (r8)           , intent(out) :: cell_active_C(autotroph_cnt)        ! ratio of active carbon uptake to carbon fixation
    real (r8)           , intent(out) :: cell_surf(autotroph_cnt)            ! surface areas of cells ( m2 )
    real (r8)           , intent(out) :: cell_carb_cont(autotroph_cnt)       ! cell carbon content ( mol C cell-1 )
    real (r8)           , intent(out) :: cell_radius(autotroph_cnt)          ! cell radius ( um )
    real (r8)           , intent(out) :: cell_permea(autotroph_cnt)          ! cell wall permeability to CO2(aq) (m/s)
    real (r8)           , intent(out) :: cell_eps_fix(autotroph_cnt)         ! fractionation effect of carbon fixation
    type(marbl_log_type), intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_ciso_mod:setup_cell_attributes'
    character(len=char_len) :: log_message
    integer (int_kind) :: &
         auto_ind           ! autotroph functional group index
    !-----------------------------------------------------------------------

    select case (ciso_fract_factors)
    case ('KellerMorel')
       do auto_ind = 1, autotroph_cnt

          if (autotrophs(auto_ind)%kSiO3 > c0) then
             !----------------------------------------------------------------------------------------
             ! Diatom based on P. tricornumtum ( Keller and morel, 1999; Popp et al., 1998 )
             !----------------------------------------------------------------------------------------
             cell_active_C_uptake(auto_ind) = 2.3_r8       ! ratio of active carbon uptake to carbon fixation
             cell_surf(auto_ind)            = 100.6e-12_r8 ! surface areas of cells ( m2 )
             cell_carb_cont(auto_ind)       = 63.3e-14_r8  ! cell carbon content ( mol C cell-1 )
             cell_radius(auto_ind)          = 14.2_r8      ! cell radius ( um )
             cell_permea(auto_ind)          = 3.3e-5_r8    ! cell wall permeability to CO2(aq) (m/s)
             cell_eps_fix(auto_ind)         = 26.6_r8      ! fractionation effect of carbon fixation

          else if (autotrophs(auto_ind)%Nfixer) then
             !----------------------------------------------------------------------------------------
             ! Diazotroph based on  Standard Phyto of Rau et al., (1996)
             !----------------------------------------------------------------------------------------
             !cell_active_C_uptake(auto_ind) = 0.0_r8        ! ratio of active carbon uptake to carbon fixation
             !cell_surf(auto_ind)            = -99.9_r8      ! surface areas of cells ( m2 ) - not used -
             !cell_carb_cont(auto_ind)       = -99.9_r8      ! cell carbon content ( mol C cell-1 ) - not used -
             !cell_radius(auto_ind)          = 10.0_r8       ! cell radius ( um )
             !cell_permea(auto_ind)          = 10.0e-5_r8    ! cell wall permeability to CO2(aq) (m/s)
             !cell_eps_fix(auto_ind)         = 25.0_r8       ! fractionation effect of carbon fixation
             !----------------------------------------------------------------------------------------
             ! Diazotroph based on Synechococcus sp. ( Keller and morel, 1999; Popp et al., 1998 )
             !----------------------------------------------------------------------------------------
             cell_active_C_uptake(auto_ind) = 7.5_r8        ! ratio of active carbon uptake to carbon fixation
             cell_surf(auto_ind)            = 5.8e-12_r8    ! surface areas of cells ( m2 )
             cell_carb_cont(auto_ind)       = 3e-14_r8      ! cell carbon content ( mol C cell-1 )
             cell_radius(auto_ind)          = 0.68_r8       ! cell radius ( um )
             cell_permea(auto_ind)          = 3.0e-8_r8     ! cell wall permeability to CO2(aq) (m/s)
             cell_eps_fix(auto_ind)         = 30.0_r8       ! fractionation effect of carbon fixation

         !else if (autotrophs(auto_ind)%exp_calcifier) then
             !Currently not set up to separate exp_calcifiers, needs cell_radius value from data
             !----------------------------------------------------------------------------------------
             ! Calcifier based on P. glacialis ( Keller and morel, 1999; Popp et al., 1998 )
             ! Popp et al express cell carbon content in ( pg C cell-1 ), here we convert in (mol C cell-1)
             ! convert pgC to molC : ! Mc = 12 g / mol ! Mc = 12 e12 pg / mol
             !----------------------------------------------------------------------------------------
             !   cell_active_C_uptake(auto_ind) = 0.0_r9       ! ratio of active carbon uptake to carbon fixation
             !   cell_surf(auto_ind)            = 3886.0_r8    ! surface areas of cells ( m2 )
             !   cell_carb_cont(auto_ind)       = 1.68e-10_r8  ! cell carbon content ( mol C cell-1 )
             !   cell_radius(auto_ind)          =              ! cell radius ( um )
             !   cell_permea(auto_ind)          = 1.1e-5_r8    ! cell wall permeability to CO2(aq) (m/s)
             !   cell_eps_fix(auto_ind)         = 23.0_r8      ! fractionation effect of carbon fixation
             
          else if (autotrophs(auto_ind)%Nfixer .and. autotrophs(auto_ind)%kSiO3 > c0) then
              log_message = "ciso: Currently Keller and Morel fractionation does not work for Diatoms-Diazotrophs"
              call marbl_status_log%log_error(log_message, subname)
              return

          else
             !----------------------------------------------------------------------------------------
             ! Small phytoplankton based on E. huxleyi ( Keller and morel, 1999; Popp et al., 1998 )
             ! Popp et al express cell carbon content in ( pg C cell-1 ), here we convert in (mol C cell-1)
             ! convert pgC to molC : ! Mc = 12 g / mol ! Mc = 12 e12 pg / mol
             !----------------------------------------------------------------------------------------
             cell_active_C_uptake(auto_ind) = 2.2_r8      ! ratio of active carbon uptake to carbon fixation
             cell_surf(auto_ind)            = 87.6e-12_r8 ! surface areas of cells ( m2 )
             cell_carb_cont(auto_ind)       = 69.2e-14_r8 ! cell carbon content ( mol C cell-1 )
             cell_radius(auto_ind)          = 2.6_r8      ! cell radius ( um )
             cell_permea(auto_ind)          = 1.8e-5_r8   ! cell wall permeability to CO2(aq) (m/s)
             cell_eps_fix(auto_ind)         = 25.3_r8     ! fractionation effect of carbon fixation

          endif
       end do
    end select

  end subroutine setup_cell_attributes

  !***********************************************************************

  subroutine setup_local_column_tracers(column_km, column_kmt, column_tracer, &
           marbl_tracer_indices, DI13C_loc, DO13c_loc, zoo13C_loc, DI14C_loc, &
           DO14C_loc, zoo14C_loc)

    !-----------------------------------------------------------------------
    !  create local copies of model column_tracer
    !  treat negative values as zero
    !-----------------------------------------------------------------------

    implicit none

    integer(int_kind) , intent(in)  :: column_km
    integer(int_kind) , intent(in)  :: column_kmt
    real (r8)         , intent(in)  :: column_tracer(:,:) ! (marbl_total_tracer_cnt,km) tracer values
    type(marbl_tracer_index_type), intent(in) :: marbl_tracer_indices

    real (r8)         , intent(out) :: DI13C_loc(:)     ! (km) local copy of model DI13C
    real (r8)         , intent(out) :: DO13C_loc(:)     ! (km) local copy of model DO13C
    real (r8)         , intent(out) :: zoo13C_loc(:)    ! (km) local copy of model zoo13C
    real (r8)         , intent(out) :: DI14C_loc(:)     ! (km) local copy of model DI14C
    real (r8)         , intent(out) :: DO14C_loc(:)     ! (km) local copy of model DO14C
    real (r8)         , intent(out) :: zoo14C_loc(:)    ! (km) local copy of model zoo14C
    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer :: k 
    !-----------------------------------------------------------------------

    associate(di13c_ind  => marbl_tracer_indices%di13c_ind,                   &
              do13c_ind  => marbl_tracer_indices%do13c_ind,                   &
              zoo13c_ind => marbl_tracer_indices%zoo13c_ind,                  &
              di14c_ind  => marbl_tracer_indices%di14c_ind,                   &
              do14c_ind  => marbl_tracer_indices%do14c_ind,                   &
              zoo14c_ind => marbl_tracer_indices%zoo14c_ind)
    do k = 1,column_kmt
       DI13C_loc(k)  = max(c0, column_tracer(di13c_ind,k))
       DI14C_loc(k)  = max(c0, column_tracer(di14c_ind,k))

       DO13C_loc(k)  = max(c0, column_tracer(do13c_ind,k))
       DO14C_loc(k)  = max(c0, column_tracer(do14c_ind,k))

       zoo13C_loc(k) = max(c0, column_tracer(zoo13C_ind,k))
       zoo14C_loc(k) = max(c0, column_tracer(zoo14C_ind,k))
    end do

    do k = column_kmt+1, column_km
       DI13C_loc(k)  = c0
       DI14C_loc(k)  = c0

       DO13C_loc(k)  = c0
       DO14C_loc(k)  = c0

       zoo13C_loc(k) = c0
       zoo14C_loc(k) = c0
    end do
    end associate

  end subroutine setup_local_column_tracers

  !***********************************************************************

  subroutine setup_local_autotrophs(column_km, column_kmt, column_tracer, &
             marbl_tracer_indices, autotroph_loc)

    !-----------------------------------------------------------------------
    !  create local copies of model column_tracer, treat negative values as zero
    !-----------------------------------------------------------------------

    implicit none

    integer(int_kind)          , intent(in)  :: column_km
    integer(int_kind)          , intent(in)  :: column_kmt
    real (r8)                  , intent(in)  :: column_tracer(:,:)  ! (marbl_total_tracer_cnt, km) tracer values

    type(marbl_tracer_index_type), intent(in) :: marbl_tracer_indices
    type(autotroph_local_type) , intent(out) :: autotroph_loc(:,:)  ! (autotroph_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: k 
    integer (int_kind) :: auto_ind
    integer (int_kind) :: tracer_ind ! tracer index
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
       do k = 1, column_kmt
          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
          autotroph_loc(auto_ind,k)%C13 = max(c0, column_tracer(tracer_ind,k))

          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
          autotroph_loc(auto_ind,k)%C14 = max(c0, column_tracer(tracer_ind,k))

          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
          if (tracer_ind > 0) then
             autotroph_loc(auto_ind,k)%Ca13CO3 = max(c0, column_tracer(tracer_ind,k))
          else
             autotroph_loc(auto_ind,k)%Ca13CO3 = c0
          end if

          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
          if (tracer_ind > 0) then
             autotroph_loc(auto_ind,k)%Ca14CO3 = max(c0, column_tracer(tracer_ind,k))
          else
             autotroph_loc(auto_ind,k)%Ca14CO3 = c0
          end if
       end do
       do k = column_kmt+1, column_km
          autotroph_loc(auto_ind,k)%C13 = c0
          autotroph_loc(auto_ind,k)%C14 = c0
          autotroph_loc(auto_ind,k)%Ca13CO3 = c0
          autotroph_loc(auto_ind,k)%Ca14CO3 = c0
       end do
    end do

  end subroutine setup_local_autotrophs

  !***********************************************************************

  subroutine fract_keller_morel( &
       mui_to_co2star,           &
       cell_active_C_uptake,     &
       cell_surf,                &
       cell_carb_cont,           &
       cell_radius,              &
       cell_permea,              &
       cell_eps_fix,             &
       eps_p)

    !---------------------------------------------------------------------------
    ! Calculate the carbon isotopic fractionation of phytoplankton photosynthesis : eps_p
    !
    ! COMPUTATION : based on Keller and Morel, 1999
    !
    !   eps_p = d13C(co2(aq)) - d13C(phyto)
    !
    !   eps_p = eps_diff + (cell_active_C_uptake/(cell_active_C_uptake + 1/var)) * delta_d13C
    !         + ( (1 + (cell_active_C_uptake-1)*var )/(1+cell_active_C_uptake*var) )
    !         * ( cell_eps_fix - eps_diff )
    !
    !   delta_d13C = d13C(CO2) - d13C(source)
    !              = 9 per mil
    !
    !   var = mui_to_co2star * cell_carb_cont / ( cell_permea * cell_surf )
    !
    !   mui_to_co2star = mu_i / [CO2*]
    !
    !  --------
    !
    !   Developed by X. Giraud, ETH ZÃ¼rich, 21.07.2008
    !---------------------------------------------------------------------------
    
    implicit none

    real (r8), intent(in) :: &
         mui_to_co2star,        & ! mui_to_co2star = mu_i / [CO2*] (m3 / mol C / s)
         cell_active_C_uptake,  & ! ratio of active carbon uptake to carbon fixation
         cell_eps_fix,          & ! fractionation effect of carbon fixation
         cell_surf,             & ! surface areas of cells ( m2 )
         cell_carb_cont,        & ! cell carbon content ( mol C )
         cell_radius,           & ! cell radius ( um )
         cell_permea              ! cell wall permeability to CO2(aq) ( m /s )

    real (r8), intent(out) :: eps_p    ! = d13C(co2(aq)) - d13C(phyto)

    !-----------------------------------------------------------------------
    !  local variables and parameters
    !-----------------------------------------------------------------------
    real (r8) :: &
         var, theta, eps_up

    real (r8) :: &
         pi, Vol, Qc, Surf, radius_m

    real (r8) :: &
         eps_diff   = 0.7_r8, & ! fractionation by diffusion, O'Leary, 1984
         delta_d13C = -9.0_r8   ! = d13C(CO2) - d13C(source), difference between the
                                ! isotopic compositions of the external CO2 and the
                                ! organic matter pools (Goericke et al. 1994).
                                ! For active HCO3- uptake, the substrate for
                                ! the carbon uptake mechanism has an isotopic
                                ! composition which is around 9 permil higher
                                ! than the external CO2, so D13CO2 -D13C_source
                                ! is -9 permil ((Mook et al. 197)
    !-----------------------------------------------------------------------

    !---------------------------------------------------------------------
    !  cell surface in m^2
    !---------------------------------------------------------------------

    radius_m = cell_radius * 1e-6_r8 ! convert radius from um to m

    if ( cell_surf > c0 ) then
       Surf = cell_surf
    else
       Surf = 4.0_r8 * pi * (radius_m ** 2)
    endif

    !---------------------------------------------------------------------
    !     cellular carbon content ( mol C )
    !     volume in um^3
    !---------------------------------------------------------------------

    if ( cell_carb_cont > c0 ) then
       Qc = cell_carb_cont
    else
       Vol = 4.0_r8 * pi * (cell_radius ** 3) / 3.0_r8
       Qc = 3.154e-14_r8 * (Vol ** (0.758_r8 ))
    endif

    !---------------------------------------------------------------------
    !     final expression of eps_p
    !---------------------------------------------------------------------

    if ( mui_to_co2star /= c0 ) then

       var = mui_to_co2star * Qc / ( cell_permea * Surf )
       
       theta = c1 + ( cell_active_C_uptake - c1 ) * var
       theta = theta / ( c1 + cell_active_C_uptake * var )

       eps_up = eps_diff + ( cell_active_C_uptake /  &
            ( cell_active_C_uptake + c1 / var ) ) * delta_d13C

       eps_p = eps_up + theta * ( cell_eps_fix - eps_diff )

    else

       eps_p = cell_eps_fix

    end if

  end subroutine fract_keller_morel

  !***********************************************************************

  subroutine init_particulate_terms(k, POC_ciso, P_CaCO3_ciso)

    !---------------------------------------------------------------------
    !  Set incoming fluxes (put into outgoing flux for first level usage).
    !  Set dissolution length, production fraction and mass terms.
    !---------------------------------------------------------------------
    
    implicit none

    integer(int_kind)                 , intent(in)    :: k
    type(column_sinking_particle_type), intent(inout) :: POC_ciso     ! base units = nmol C_ciso
    type(column_sinking_particle_type), intent(inout) :: P_CaCO3_ciso ! base units = nmol C_ciso

    !-----------------------------------------------------------------------
    !  parameters
    !-----------------------------------------------------------------------

    POC_ciso%diss      = c0       ! not used
    POC_ciso%gamma     = c0       ! not used
    POC_ciso%mass      = c0       ! not used
    POC_ciso%rho       = c0       ! not used

    P_CaCO3_ciso%diss  = c0       ! not used
    P_CaCO3_ciso%gamma = c0       ! not used
    P_CaCO3_ciso%mass  = c0       ! not used
    P_CaCO3_ciso%rho   = c0       ! not used

    !-----------------------------------------------------------------------
    !  Set incoming fluxes
    !-----------------------------------------------------------------------

    P_CaCO3_ciso%sflux_out(k) = c0
    P_CaCO3_ciso%hflux_out(k) = c0
    P_CaCO3_ciso%sflux_in(k)  = P_CaCO3_ciso%sflux_out(k) 
    P_CaCO3_ciso%hflux_in(k)  = P_CaCO3_ciso%hflux_out(k) 

    !-----------------------------------------------------------------------
    !  Hard POC is QA flux and soft POC is excess POC.
    !-----------------------------------------------------------------------

    POC_ciso%sflux_out(k) = c0
    POC_ciso%hflux_out(k) = c0
    POC_ciso%sflux_in(k)  = POC_ciso%sflux_out(k) 
    POC_ciso%hflux_in(k)  = POC_ciso%hflux_out(k) 

  end subroutine init_particulate_terms

  !***********************************************************************

  subroutine marbl_update_particulate_terms_from_prior_level(k, POC_ciso, P_CaCO3_ciso)

    !-----------------------------------------------------------------------
    ! NOTE: incoming fluxes are outgoing fluxes from previous level
    ! initialize loss to sediments = 0
    ! Assume that k == 1 condition was handled by call to init_particulate_terms()
    !-----------------------------------------------------------------------

    implicit none 

    integer (int_kind)                 , intent(in)    :: k ! vertical model level
    type(column_sinking_particle_type) , intent(inout) :: POC_ciso
    type(column_sinking_particle_type) , intent(inout) :: P_CaCO3_ciso
    !-----------------------------------------------------------------------

    if (k > 1) then
       call marbl_update_sinking_particle_from_prior_level(k, POC_ciso)
       call marbl_update_sinking_particle_from_prior_level(k, P_CaCO3_ciso)
    end if

  end subroutine marbl_update_particulate_terms_from_prior_level

  !***********************************************************************

  subroutine marbl_update_sinking_particle_from_prior_level(k, sinking_particle)

    implicit none 

    integer (int_kind), intent(in) :: k
    type(column_sinking_particle_type), intent(inout) :: sinking_particle
    !-----------------------------------------------------------------------

    ! NOTE: level k influx is equal to the level k-1 outflux.
    sinking_particle%sflux_out(k) = sinking_particle%sflux_out(k-1)
    sinking_particle%hflux_out(k) = sinking_particle%hflux_out(k-1)
    sinking_particle%sflux_in(k)  = sinking_particle%sflux_out(k-1)
    sinking_particle%hflux_in(k)  = sinking_particle%hflux_out(k-1)

  end subroutine marbl_update_sinking_particle_from_prior_level

  !***********************************************************************

  subroutine compute_particulate_terms(k, column_km, column_kmt, column_delta_z, column_zw, &
       O2_loc, NO3_loc, POC, P_CaCO3, marbl_particulate_share, POC_ciso, P_CaCO3_ciso)

    !----------------------------------------------------------------------------------------
    !  Compute outgoing fluxes and remineralization terms for Carbon isotopes.
    !  Assumes that production terms have been set and that fluxes and remineralization
    !  for Carbon 12 has already been computed.
    !
    !  Incoming fluxes are assumed to be the outgoing fluxes from the previous level.
    !  For other comments, see compute_particulate_terms in marbl_mod
    !----------------------------------------------------------------------------------------
    
    use marbl_parms     , only : denitrif_C_N
    use marbl_parms     , only : parm_POMbury
    use marbl_parms     , only : spd

    implicit none

    integer (int_kind)                 , intent(in)    :: k                 ! vertical model level
    integer (int_kind)                 , intent(in)    :: column_km
    integer (int_kind)                 , intent(in)    :: column_kmt
    real (r8)                          , intent(in)    :: column_delta_z
    real (r8)                          , intent(in)    :: column_zw      
    real (r8)                          , intent(in)    :: O2_loc            ! dissolved oxygen used to modify POC%diss, Sed fluxes
    real (r8)                          , intent(in)    :: NO3_loc           ! dissolved nitrate used to modify sed fluxes
    type(column_sinking_particle_type) , intent(in)    :: POC               ! base units = nmol C
    type(column_sinking_particle_type) , intent(in)    :: P_CaCO3           ! base units = nmol CaCO3
    type(marbl_particulate_share_type) , intent(in)    :: marbl_particulate_share

    type(column_sinking_particle_type) , intent(inout) :: POC_ciso          ! base units = nmol particulate organic Carbon isotope
    type(column_sinking_particle_type) , intent(inout) :: P_CaCO3_ciso      ! base units = nmol CaCO3 Carbon isotope

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_ciso_mod:ciso_compute_particulate_terms'
    character(len=char_len) :: log_message

    real (r8) ::              &
         dz_loc,              & ! dz at a particular i,j location
         dzr_loc,             & ! dzr at a particular i,j location
         flux, flux_alt,      & ! temp variables used to update sinking flux
         POC_ciso_PROD_avail, & ! 13C POC production available for excess POC flux
         Rciso_POC_hflux_out, & ! ciso/12C of outgoing flux of hard POC
         Rciso_POC_in,        & ! ciso/12C of total POC ingoing component
         Rciso_CaCO3_in,      & ! ciso/12C of total CaCO3 ingoing component
         sed_denitrif,        & ! sedimentary denitrification (umolN/cm^2/s)
         other_remin            ! sedimentary remin not due to oxic or denitrification
    !-----------------------------------------------------------------------

    associate(                                                                &
         decay_CaCO3       => marbl_particulate_share%decay_CaCO3_fields    , & ! IN
         DECAY_Hard        => marbl_particulate_share%DECAY_Hard_fields     , & ! IN
         decay_POC_E       => marbl_particulate_share%decay_POC_E_fields    , & ! IN
         POC_PROD_avail    => marbl_particulate_share%POC_PROD_avail_fields , & ! IN
         poc_diss          => marbl_particulate_share%poc_diss_fields       , & ! IN
         caco3_diss        => marbl_particulate_share%caco3_diss_fields     , & ! IN
         POC_sflux_out     => marbl_particulate_share%POC_sflux_out_fields  , & ! IN
         POC_hflux_out     => marbl_particulate_share%POC_hflux_out_fields  , & ! IN
         POC_remin         => marbl_particulate_share%POC_remin_fields      , & ! IN
         P_CaCO3_remin     => marbl_particulate_share%P_CaCO3_remin_fields    & ! IN
         )

    !-----------------------------------------------------------------------
    !  initialize loss to sediments = 0 and local copy of percent sed
    !-----------------------------------------------------------------------

    POC_ciso%sed_loss(k)     = c0
    P_CaCO3_ciso%sed_loss(k) = c0
    sed_denitrif             = c0
    other_remin              = c0

    !-----------------------------------------------------------------------
    ! if any incoming carbon flux is zero, set carbon isotope flux to zero
    !-----------------------------------------------------------------------

    if (POC%sflux_in(k) == c0) POC_ciso%sflux_in(k) = c0
    if (POC%hflux_in(k) == c0) POC_ciso%hflux_in(k) = c0

    if (P_CaCO3%sflux_in(k) == c0) P_CaCO3_ciso%sflux_in(k) = c0
    if (P_CaCO3%hflux_in(k) == c0) P_CaCO3_ciso%hflux_in(k) = c0

    dz_loc = column_delta_z

    if (k <= column_kmt) then

       dzr_loc = c1 / dz_loc

       !-----------------------------------------------------------------------
       !  P_CaCO3_ciso sflux and hflux out
       !-----------------------------------------------------------------------
       
       P_CaCO3_ciso%sflux_out(k) = P_CaCO3_ciso%sflux_in(k) * decay_CaCO3(k) + &
            P_CaCO3_ciso%prod(k) * ((c1 - P_CaCO3%gamma) * (c1 - decay_CaCO3(k)) * caco3_diss(k))
       
       P_CaCO3_ciso%hflux_out(k) = P_CaCO3_ciso%hflux_in(k) * DECAY_Hard(k) + &
            P_CaCO3_ciso%prod(k) * (P_CaCO3%gamma * dz_loc)
       
       !-----------------------------------------------------------------
       !   Compute how much 13C POC_PROD is available for deficit
       !   reduction and excess POC flux
       !-----------------------------------------------------------------
       
       if (POC%prod(k) > c0 ) then
          POC_ciso_PROD_avail = POC_PROD_avail(k) * POC_ciso%prod(k) / POC%prod(k)
       else
          POC_ciso_PROD_avail = c0
       endif
       
       !-----------------------------------------------------------------
       !   Compute outgoing isotope POC fluxes of soft POC
       !-----------------------------------------------------------------
       
       POC_ciso%sflux_out(k) = POC_ciso%sflux_in(k) * decay_POC_E(k) + &
                               POC_ciso_PROD_avail * ((c1 - decay_POC_E(k)) * (poc_diss(k)))
       
       !-----------------------------------------------------------------
       !   Compute outgoing isotope POC fluxes of hard POC
       !-----------------------------------------------------------------

       if (POC_ciso%hflux_in(k) == c0 .and. POC_ciso%prod(k) == c0) then
          POC_ciso%hflux_out(k) = c0
       else
          
          Rciso_POC_hflux_out = POC%prod(k) + ( POC%sflux_in(k) - POC_sflux_out(k) + POC%hflux_in(k) ) * dzr_loc
          
          if (Rciso_POC_hflux_out /= c0) then
             Rciso_POC_hflux_out = ( POC_ciso%prod(k) + ( POC_ciso%sflux_in(k) - &
                  POC_ciso%sflux_out(k) + POC_ciso%hflux_in(k) ) * dzr_loc ) / Rciso_POC_hflux_out
          else
             Rciso_POC_hflux_out = c0
          endif
          
          POC_ciso%hflux_out(k) = POC_hflux_out(k) * Rciso_POC_hflux_out
          POC_ciso%hflux_out(k) = max(POC_ciso%hflux_out(k), c0)

       endif

       !-----------------------------------------------------------------------
       !  Compute remineralization terms. It is assumed that there is no
       !  sub-surface dust production.
       !-----------------------------------------------------------------------

       P_CaCO3_ciso%remin(k) = P_CaCO3_ciso%prod(k) + &
            ((P_CaCO3_ciso%sflux_in(k) - P_CaCO3_ciso%sflux_out(k)) + &
             (P_CaCO3_ciso%hflux_in(k) - P_CaCO3_ciso%hflux_out(k))) *  dzr_loc

       POC_ciso%remin(k) = POC_ciso%prod(k)  + &
            ((POC_ciso%sflux_in(k) - POC_ciso%sflux_out(k)) + &
             (POC_ciso%hflux_in(k) - POC_ciso%hflux_out(k))) * dzr_loc

       !-----------------------------------------------------------------
       !   Option to force the 13C/12C ratio of outgoing P_CaCO3 fluxes
       !   to equal the rate of total incoming flux
       !-----------------------------------------------------------------
       !
       !   Rciso_CaCO3_in = P_CaCO3%prod(k) + &
       !       ( P_CaCO3%sflux_in(k) + P_CaCO3%hflux_in(k) ) *  dzr_loc
       !
       !   if ( Rciso_CaCO3_in > c0 ) then
       !       Rciso_CaCO3_in = ( P_CaCO3_ciso%prod(k) + &
       !         ( P_CaCO3_ciso%sflux_in(k) + P_CaCO3_ciso%hflux_in(k) ) * dzr_loc) &
       !          / Rciso_CaCO3_in
       !   else
       !           Rciso_CaCO3_in = c0
       !   endif
       !
       !   P_CaCO3_ciso%sflux_out(k) = P_CaCO3%sflux_out(k) * Rciso_CaCO3_in
       !   P_CaCO3_ciso%hflux_out(k) = P_CaCO3%hflux_out(k) * Rciso_CaCO3_in
       !   P_CaCO3_ciso%remin(k)     = P_CaCO3%remin(k)     * Rciso_CaCO3_in
       !
       !-----------------------------------------------------------------
       !   Option to force the 13C/12C ratio of outgoing POC fluxes
       !   to equal the rate of total incoming flux
       !-----------------------------------------------------------------
       !
       !   Rciso_POC_in = POC%prod(k) + ( POC%sflux_in(k) + POC%hflux_in(k) ) &
       !                 * dzr_loc
       !   if ( Rciso_POC_in > c0 ) then
       !      Rciso_POC_in = ( POC_ciso%prod(k) + ( POC_ciso%sflux_in(k) + &
       !         POC_ciso%hflux_in(k)) * dzr_loc ) / Rciso_POC_in
       !   else
       !      Rciso_POC_in = c0
       !   endif
       !
       !   POC_ciso%sflux_out(k) = POC_sflux_out(k) * Rciso_POC_in
       !   POC_ciso%hflux_out(k) = POC_hflux_out(k) * Rciso_POC_in
       !   POC_ciso%remin(k)     = POC_remin(k)     * Rciso_POC_in
       !
       !-----------------------------------------------------------------

    else

       P_CaCO3_ciso%sflux_out(k) = c0
       P_CaCO3_ciso%hflux_out(k) = c0
       P_CaCO3_ciso%remin(k)     = c0

       POC_ciso%sflux_out(k) = c0
       POC_ciso%hflux_out(k) = c0
       POC_ciso%remin(k)     = c0

    endif

    !-----------------------------------------------------------------------
    !  Bottom Sediments Cell?
    !  If so compute sedimentary burial and denitrification N losses.
    !  Using empirical relations from Bohlen et al., 2012 (doi:10.1029/2011GB004198) for Sed Denitrification
    !  OTHER_REMIN estimates organic matter remineralized in the sediments
    !      by the processes other than oxic remin and denitrification (SO4 and CO2,
    !      etc..)
    !      based on Soetaert et al., 1996, varies between 10% and 50%
    !      0.4_r8 is a coefficient with units mmolC/cm2/yr sinking flux,
    !      OTHER_REMIN is 50% above this high flux value,
    !      In special case where bottom O2 has been depleted to < 1.0 uM,
    !               all sedimentary remin is due to DENITRIFICATION + OTHER_REMIN
    !  POC burial from Dunne et al. 2007 (doi:10.1029/2006GB002907), maximum of 80% burial efficiency imposed
    !  Bsi preservation in sediments = 0.3*sinkBsi - 0.06 mmol/m2/day
    !     Ragueneau et al. 2000 (doi:10.1016/S0921-8181(00)00052-7)
    !  Calcite is preserved in sediments above the lysocline, dissolves below.
    !       Here a constant depth is used for lysocline.
    !-----------------------------------------------------------------------

    if (k == column_kmt) then

       flux = POC_ciso%sflux_out(k) + POC_ciso%hflux_out(k)
       
       if (flux > c0) then
          flux_alt = flux * mpercm * spd ! convert to mmol/m^2/day

          POC_ciso%sed_loss(k) = flux * min(0.8_r8, parm_POMbury &
               * (0.013_r8 + 0.53_r8 * flux_alt*flux_alt / (7.0_r8 + flux_alt)**2))


          sed_denitrif = dzr_loc * flux * (0.06_r8 + 0.19_r8 * 0.99_r8**(O2_loc-NO3_loc))

          flux_alt = flux*1.0e-6_r8*spd*365.0_r8 ! convert to mmol/cm^2/year
          other_remin = dzr_loc &
               * min ( min(0.1_r8 + flux_alt,0.5_r8) * (flux - POC_ciso%sed_loss(k)) , &
                      (flux - POC_ciso%sed_loss(k) - (sed_denitrif*dz_loc*denitrif_C_N)))

          ! if bottom water O2 is depleted, assume all remin is denitrif + other
          if (O2_loc < c1) then
             other_remin = dzr_loc * (flux - POC_ciso%sed_loss(k) - (sed_denitrif * dz_loc * denitrif_C_N))
          endif
       endif

       if (column_zw < 3300.0e2_r8) then
          flux = P_CaCO3_ciso%sflux_out(k) + P_CaCO3_ciso%hflux_out(k)
          P_CaCO3_ciso%sed_loss(k) = flux
       endif

       !----------------------------------------------------------------------------------
       ! Update sinking fluxes and remin fluxes, accounting for sediments.
       ! flux used to hold sinking fluxes before update.
       !----------------------------------------------------------------------------------

       flux = P_CaCO3_ciso%sflux_out(k) + P_CaCO3_ciso%hflux_out(k)
       if (flux > c0) then
          P_CaCO3_ciso%remin(k) = P_CaCO3_ciso%remin(k) + ((flux - P_CaCO3_ciso%sed_loss(k)) * dzr_loc)
       endif

       flux = POC_ciso%sflux_out(k) + POC_ciso%hflux_out(k)
       if (flux > c0) then
          POC_ciso%remin(k) = POC_ciso%remin(k) + ((flux - POC_ciso%sed_loss(k)) * dzr_loc)
       endif

       !-----------------------------------------------------------------------
       ! Set all outgoing fluxes to 0.0
       !-----------------------------------------------------------------------

       P_CaCO3_ciso%sflux_out(k) = c0
       P_CaCO3_ciso%hflux_out(k) = c0

       POC_ciso%sflux_out(k) = c0
       POC_ciso%hflux_out(k) = c0

    endif

    end associate

  end subroutine compute_particulate_terms

  !***********************************************************************

  subroutine marbl_autotroph_consistency_check(column_km, autotroph_cnt,      &
         autotroph_meta, marbl_tracer_indices, autotroph_share, autotroph_loc)

    !-----------------------------------------------------------------------
    !  If any phyto box are zero, set others to zeros.
    !-----------------------------------------------------------------------

    implicit none

    integer(int_kind)                , intent(in)    :: column_km                     ! number of active model layers
    integer(int_kind)                , intent(in)    :: autotroph_cnt                 ! autotroph_cnt
    type(autotroph_type)             , intent(in)    :: autotroph_meta(autotroph_cnt) ! autotroph metadata
    type(marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices
    type(marbl_autotroph_share_type) , intent(in)    :: autotroph_share(autotroph_cnt, column_km)
    type(autotroph_local_type)       , intent(inout) :: autotroph_loc(autotroph_cnt, column_km)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: auto_ind, k
    logical (log_kind) :: zero_mask
    !-----------------------------------------------------------------------

    associate(&
         autotrophC_loc     => autotroph_share%autotrophC_loc_fields     , & ! local copy of model autotroph C  
         autotrophChl_loc   => autotroph_share%autotrophChl_loc_fields   , & ! local copy of model autotroph Chl
         autotrophFe_loc    => autotroph_share%autotrophFe_loc_fields    , & ! local copy of model autotroph Fe 
         autotrophSi_loc    => autotroph_share%autotrophSi_loc_fields      & ! local copy of model autotroph Si 
         )

    do k = 1, column_km
       do auto_ind = 1, autotroph_cnt

          ! Zero_mask=true for any zero in Chl, C, Fe, 
          ! It is false only if all are false
          ! Add si to zero mask... if it is present (ind > 0)

          zero_mask = (autotrophChl_loc(auto_ind,k) == c0 .or. &
                       autotrophC_loc(auto_ind,k)   == c0 .or. &
                       autotrophFe_loc(auto_ind,k)  == c0)

          if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
             zero_mask = (zero_mask .or. autotrophSi_loc(auto_ind,k) == c0)
          end if

          if (zero_mask) then
             autotroph_loc(auto_ind,k)%C13 = c0
             autotroph_loc(auto_ind,k)%C14 = c0

             if (marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind > 0) then
                autotroph_loc(auto_ind,k)%Ca13CO3 = c0
             end if
             if (marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind > 0) then
                autotroph_loc(auto_ind,k)%Ca14CO3 = c0
             end if
          end if

       end do
    end do

    end associate

  end subroutine marbl_autotroph_consistency_check

  !*****************************************************************************

  subroutine marbl_ciso_set_surface_forcing( &
       num_elements        ,                 &
       surface_mask        ,                 &
       sst                 ,                 &
       d13c                ,                 &
       d14c                ,                 &
       d14c_glo_avg        ,                 &
       surface_vals        ,                 &
       stf                 ,                 &
       marbl_tracer_indices,                 &
       marbl_surface_forcing_share ,         &
       marbl_surface_forcing_diags)

    use marbl_parms            , only : R13c_std
    use marbl_parms            , only : R14c_std
    use marbl_parms            , only : p5
    use marbl_diagnostics_mod  , only : store_diagnostics_ciso_surface_forcing

    implicit none

    integer (int_kind)                     , intent(in)    :: num_elements
    real(r8)                               , intent(in)    :: surface_mask(num_elements)
    real(r8)                               , intent(in)    :: sst(num_elements)
    real(r8)                               , intent(in)    :: d13c(num_elements)  ! atm 13co2 value
    real(r8)                               , intent(in)    :: d14c(num_elements)  ! atm 14co2 value
    real(r8)                               , intent(in)    :: d14c_glo_avg(num_elements)
    real(r8)                               , intent(in)    :: surface_vals(:,:)
    type(marbl_surface_forcing_share_type) , intent(in)    :: marbl_surface_forcing_share
    real(r8)                               , intent(inout) :: stf(:, :)
    type(marbl_tracer_index_type)          , intent(in)    :: marbl_tracer_indices
    type(marbl_diagnostics_type)           , intent(inout) :: marbl_surface_forcing_diags

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_ciso_mod:marbl_ciso_set_surface_forcing'
    character(len=char_len) :: log_message

    logical (log_kind), save :: &
         first = .true.  ! Logical for first iteration test

    integer (int_kind) :: &
         n,            & ! loop indices
         auto_ind,     & ! autotroph functional group index
         mcdate,sec,   & ! date vals for shr_strdata_advance
         errorCode       ! errorCode from HaloUpdate call

    real (r8), dimension(num_elements) :: &
         R13C_DIC,                        & ! 13C/12C ratio in DIC
         R14C_DIC,                        & ! 14C/12C ratio in total DIC
         R13C_atm,                        & ! 13C/12C ratio in atmospheric CO2
         R14C_atm,                        & ! 14C/12C ratio in atmospheric CO2
         flux,                            & ! gas flux of CO2 (nmol/cm^2/s)
         flux13,                          & ! gas flux of 13CO2 (nmol/cm^2/s)
         flux14,                          & ! gas flux of 14CO2 (nmol/cm^2/s)
         flux_as,                         & ! air-to-sea gas flux of CO2 (nmol/cm^2/s)
         flux13_as,                       & ! air-to-sea gas flux of 13CO2 (nmol/cm^2/s)
         flux14_as,                       & ! air-to-sea gas flux of 14CO2 (nmol/cm^2/s)
         flux_sa,                         & ! sea-to-air gas flux of CO2 (nmol/cm^2/s)
         flux13_sa,                       & ! sea-to-air gas flux of 13CO2 (nmol/cm^2/s)
         flux14_sa                          ! sea-to-air gas flux of 14CO2 (nmol/cm^2/s)

    real (r8), dimension(num_elements) :: &
         eps_aq_g_surf,                   & ! equilibrium fractionation (CO2_gaseous <-> CO2_aq)
         alpha_aq_g_surf,                 & ! alpha_xxx_g_surf => eps = ( alpa -1 ) * 1000
         eps_dic_g_surf,                  & ! equilibrium fractionation between total DIC and gaseous CO2
         alpha_dic_g_surf,                & ! alpha_xxx_g_surf => eps = ( alpa -1 ) * 1000
         eps_hco3_g_surf,                 & ! equilibrium fractionation between bicarbonate and gaseous CO2
         eps_co3_g_surf,                  & ! equilibrium fractionation between carbonate and gaseous CO2
         frac_co3,                        & ! carbonate fraction fCO3 = [CO3--]/DIC
         alpha_aq_g_surf_14c,             & ! for 14C, with fractionation being twice as large for 14C than for 13C
         alpha_dic_g_surf_14c               ! for 14C, with fractionation being twice as large for 14C than for 13C

    real (r8), dimension(num_elements) :: &
         di13c_riv_flux,                  & ! River input of DI13C
         do13c_riv_flux,                  & ! River input of DO13C
         di14c_riv_flux,                  & ! River input of DI14C
         do14c_riv_flux                     ! River input of DO14C

    ! local parameters for 13C, Zhang et al, 1995, Geochim. et Cosmochim. Acta, 59 (1), 107-114
    real(r8) ::            &
         alpha_k,          & ! eps = ( alpa -1 ) * 1000
         alpha_k_14c         ! for 14C, with fractionation being twice as large for 14C than for 13C

    ! kinetic fraction during gas transfert (per mil) (air-sea CO2 exchange) at 21C, Zhang et al 1995,
    real(r8), parameter :: eps_k = -0.81_r8  ! eps_k = -0.95 at 5C
    !-----------------------------------------------------------------------

    associate(                                                                     &
         pv                  => marbl_surface_forcing_share%pv_surf_fields       , & ! in/out
         dic                 => marbl_surface_forcing_share%dic_surf_fields      , & ! in/out DIC values for solver
         co2star             => marbl_surface_forcing_share%co2star_surf_fields  , & ! in/out CO2STAR from solver
         dco2star            => marbl_surface_forcing_share%dco2star_surf_fields , & ! in/out DCO2STAR from solver
         co3_surf_fields     => marbl_surface_forcing_share%co3_surf_fields      , & ! in/out
         dic_riv_flux_fields => marbl_surface_forcing_share%dic_riv_flux_fields  , & ! in/out
         doc_riv_flux_fields => marbl_surface_forcing_share%doc_riv_flux_fields  , & ! in/out

         di13c_ind          => marbl_tracer_indices%di13c_ind                  , &
         do13c_ind          => marbl_tracer_indices%do13c_ind                  , &
         di14c_ind          => marbl_tracer_indices%di14c_ind                  , &
         do14c_ind          => marbl_tracer_indices%do14c_ind                  , &
              ciso_ind_beg  => marbl_tracer_indices%ciso_ind_beg               , &
              ciso_ind_end  => marbl_tracer_indices%ciso_ind_end                 &
         )

    !-----------------------------------------------------------------------
    !  ciso fluxes initially set to 0
    !-----------------------------------------------------------------------

    stf(:,ciso_ind_beg:ciso_ind_end) = c0

    !-----------------------------------------------------------------------
    !     initialize R13C_atm  and R14C_atm
    !-----------------------------------------------------------------------
    
    R13C_atm(:) = R13C_std * ( c1 + d13c(:) / c1000 )
    R14C_atm(:) = R14C_std * ( c1 + d14c(:) / c1000 )

    !-----------------------------------------------------------------------
    !     compute 13C02 flux, based on CO2 flux calculated in ecosystem model
    !     Zhang et al, 1995, Geochim. et Cosmochim. Acta, 59 (1), 107-114
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !     compute R13C_DIC in surface ocean (assuming that DIC is 12C)
    !-----------------------------------------------------------------------

    where ( dic(:) /= c0 ) 
       R13C_dic(:) = surface_vals(:,di13c_ind) / dic(:)
       R14C_dic(:) = surface_vals(:,di14c_ind) / dic(:)
    elsewhere
       R13C_dic(:) = c0
       R14C_dic(:) = c0
    end where

    !-----------------------------------------------------------------------
    !     individal discrimination factor of each species with respect to
    !     gaseous CO2, temperature dependent, based on Zhang et al. 95
    !-----------------------------------------------------------------------
    eps_aq_g_surf(:)   = 0.0049_r8 * sst(:) - 1.31_r8
    !!         eps_hco3_g_surf(:) = -0.141_r8  * SST(:) + 10.78_r8
    !!         eps_co3_g_surf(:)  = -0.052_r8  * SST(:) + 7.22_r8

    !-----------------------------------------------------------------------
    !     compute the equilibrium discrimination factor between dic and
    !     gaseous CO2
    !-----------------------------------------------------------------------
    !     solution 1 : from individual species.
    !     Not used as Zhang et al. 95
    !     concluded that eps_dic_g_surf can not be calculated from the sum of
    !     the three individual species
    !----------------------------------------------------------------------
    !         eps_dic_g_surf(:,j) = eps_aq_g_surf(:,j) + eps_hco3_g_surf(:,j) &
    !                                + eps_co3_g_surf(:,j)
    !-----------------------------------------------------------------------
    !     solution 2: function of T and carbonate fraction (frac_co3)
    !     Using this one, which is based on the empirical relationship from
    !     the measured e_dic_g_surf of Zhang et al. 1995
    !---------------------------------------------------------------------

    where (surface_mask(:) /= c0) 
       frac_co3(:) = CO3_SURF_fields(:) / dic(:)
    elsewhere
       frac_co3(:) = c0
    end where

    eps_dic_g_surf(:) = 0.014_r8 * sst(:) * frac_co3(:) - 0.105_r8 * sst(:) + 10.53_r8

    !-----------------------------------------------------------------------
    !     compute alpha coefficients from eps :  eps = ( alpha -1 ) * 1000
    !     => alpha = 1 + eps / 1000
    !-----------------------------------------------------------------------

    alpha_k             = c1 + eps_k             / c1000
    alpha_aq_g_surf(:)  = c1 + eps_aq_g_surf(:)  / c1000
    alpha_dic_g_surf(:) = c1 + eps_dic_g_surf(:) / c1000

    ! Fractionation is twice as large for 14C than for 13C, so eps needs to be multiplied by 2 for 14C
    alpha_k_14c             = c1 + eps_k * 2.0_r8            / c1000
    alpha_aq_g_surf_14c(:)  = c1 + eps_aq_g_surf(:) *2.0_r8  / c1000
    alpha_dic_g_surf_14c(:) = c1 + eps_dic_g_surf(:) *2.0_r8 / c1000

    !-----------------------------------------------------------------------
    !     compute 13C flux and C flux
    !-----------------------------------------------------------------------

    flux13(:) = pv(:) * alpha_k * alpha_aq_g_surf(:) * &
         (( co2star(:) + dco2star(:) ) * r13c_atm(:) - co2star(:) * r13c_dic(:) / alpha_dic_g_surf(:))

    flux14(:) = pv(:) * alpha_k_14c * alpha_aq_g_surf_14c(:) * &
         (( co2star(:) + dco2star(:) ) * r14c_atm(:) - co2star(:) * r14c_dic(:) / alpha_dic_g_surf_14c(:))

    flux(:) = pv(:) * dco2star(:)

    !-----------------------------------------------------------------------
    !     compute fluxes in and out
    !-----------------------------------------------------------------------

    flux_as(:)   = pv(:) * ( dco2star(:) + co2star(:) )
    flux_sa(:)   = pv(:) * co2star(:)

    flux13_as(:) = pv(:) * alpha_k     * alpha_aq_g_surf(:)     * ((co2star(:) + dco2star(:)) * r13c_atm(:))
    flux14_as(:) = pv(:) * alpha_k_14c * alpha_aq_g_surf_14c(:) * ((co2star(:) + dco2star(:)) * r14c_atm(:))

    flux13_sa(:) = pv(:) * alpha_k     * alpha_aq_g_surf(:)     * (co2star(:) * r13c_dic(:) / alpha_dic_g_surf(:))
    flux14_sa(:) = pv(:) * alpha_k_14c * alpha_aq_g_surf_14c(:) * (co2star(:) * r14c_dic(:) / alpha_dic_g_surf_14c(:))

    !-----------------------------------------------------------------------
    !     end of 13C computation for gass exchange
    !-----------------------------------------------------------------------

    stf(:,di13c_ind) = stf(:,di13c_ind) + flux13(:)
    stf(:,di14c_ind) = stf(:,di14c_ind) + flux14(:)

    !-----------------------------------------------------------------------
    !     Adding 13C FLux to total DI13C
    !-----------------------------------------------------------------------

    di13c_riv_flux(:) = dic_riv_flux_fields(:) * (-10.0_r8/c1000 +c1) * R13C_std
    di14c_riv_flux(:) = dic_riv_flux_fields(:) * ((D14C_glo_avg(:) - 50.0_r8)/c1000 +c1) * R14C_std

    do13c_riv_flux(:) = doc_riv_flux_fields(:) * (-27.6_r8/c1000 +c1) * R13C_std
    do14c_riv_flux(:) = doc_riv_flux_fields(:) * (-50.0_r8/c1000 +c1) * R14C_std

    stf(:,di13c_ind) = stf(:,di13c_ind) + di13c_riv_flux(:)
    stf(:,do13c_ind) = stf(:,do13c_ind) + do13c_riv_flux(:)

    stf(:,di14c_ind) = stf(:,di14c_ind) + di14c_riv_flux(:)
    stf(:,do14c_ind) = stf(:,do14c_ind) + do14c_riv_flux(:)

    end associate

    ! update carbon isotope diagnostics 
    ! FIXME #18: the following arguments need to be group into a derived type

    call store_diagnostics_ciso_surface_forcing( &
         num_elements,   &
         d13c,           &
         d14c,           &
         d14c_glo_avg,   &
         flux,           &
         flux13,         &
         flux14,         &
         flux_as,        &
         flux13_as,      &
         flux14_as,      &
         flux_sa,        &
         flux13_sa,      &
         flux14_sa,      &
         R13C_dic,       &
         R14C_dic,       &
         R13C_atm,       &
         R14C_atm,       &
         di13c_riv_flux, &
         do13c_riv_flux, &
         di14c_riv_flux, &
         do14c_riv_flux, &
         eps_aq_g_surf,  &
         eps_dic_g_surf, &
         marbl_surface_forcing_diags)

  end subroutine marbl_ciso_set_surface_forcing

end module marbl_ciso_mod
