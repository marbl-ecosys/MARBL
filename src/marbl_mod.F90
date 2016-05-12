! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module marbl_mod

  !  Multispecies ecosystem based on Doney et al. 1996, Moore et al., 2002
  !  Based on POP Global NCAR Nitrogen Ecosystem Model
  !  version 0.0 (June 15th, 1998) from S.C. Doney.
  !  Based on Doney et al., 1996 model.
  !  Climate and Global Dynamics, NCAR
  !  (doney@whoi.edu)
  !
  !  Version 1.0
  !  Multispecies, multiple limiting nutrient version of ecosystem
  !  based on mixed layer model of Moore et al.(2002).  Implemented here with
  !  fixed elemental ratios and including only the diatoms and small
  !  phytoplankton, with a parameterization of calcification,
  !  by Keith Lindsay and Keith Moore, Fall 2001 - Spring 2002.
  !  Calcification parameterization based on Moore et al. 2002.
  !
  !  Version 2.0, January 2003
  !    Adds diazotrophs as a phytoplankton group, (based on Moore et al., 2002a)
  !    Allows for variable fe/C for all phytoplankton groups
  !     Allows for variable si/C for the diatoms
  !     Adds explicit tracers for DON, DOP, DOFe
  !     variable remin length scale for detrital soft POM and bSi f(temperature)
  !     Extensive modifications to iron scavenging parameterization
  !     Addition of a sedimentary dissolved iron source,
  !        (implemented in ballast code as excess remin in bottom cell)
  !        coded by J.K. Moore, (jkmoore@uci.edu)
  !
  !   Version 2.01. March 2003
  !     corrected O2 bug
  !     corrected grazing parameter z_grz bug at depth
  !     dust dissolution at depth releases iron,
  !     increased length scale for dust diss., increased hard fraction dust
  !     no deep ocean reduction in scavenging rates,
  !     increase bSi OC/ballast ratio 0.3 -> 0.35,
  !     corrected bug in diazotroph photoadaptation, and diat and sp adapatation
  !
  !   Version 2.02.
  !     corrected bug in Fe_scavenge (units for dust), May 2003
  !     changed C/N/P ratios to 117/16/1 (Anderson & Sarmiento, 1994)
  !
  !   Version 2.03., July 2003
  !     Remin of DOM no longer temperature dependent,
  !     new iron scavenging parameterization added,
  !     some dissolution of hard fraction of ballast materials added
  !
  !   Version 2.1, September 2003
  !     modfied iron scavenging and dust dissolution at depth
  !
  !   Version 2.11, March 2004
  !     fixed bug in iron scavenging code, replace dust and POC flux_in w/ flux_out
  !
  !   Version 2.12, April 2004 - Final version for GBC paper revision,
  !     (Questions/comments, Keith Moore - jkmoore@uci.edu
  !
  !   References
  !   Doney, S.C., Glover, D.M., Najjar, R.G., 1996. A new coupled, one-dimensional
  !   biological-physical model for the upper ocean: applications to the JGOFS
  !   Bermuda Time-Series Study (BATS) site. Deep-Sea Res. II, 43: 591-624.
  !
  !   Moore, JK, Doney, SC, Kleypas, JA, Glover, DM, Fung, IY, 2002. An intermediate
  !   complexity marine ecosystem model for the global domain. Deep-Sea Res. II, 49:
  !   403-462.
  !
  !   Moore, JK, Doney, SC, Glover, DM, Fung, IY, 2002. Iron cycling and nutrient
  !   limitation patterns in surface waters of the world ocean. Deep-Sea Res. II,
  !   49: 463-507.

  !-----------------------------------------------------------------------
  !  The following are used extensively in this ecosys, so are used at
  !  the module level. The use statements for variables that are only needed
  !  locally are located at the module subprogram level.
  !-----------------------------------------------------------------------

  use marbl_constants_mod, only : T0_Kelvin

  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : char_len

  use marbl_parms, only : c0
  use marbl_parms, only : c1
  use marbl_parms, only : c2
  use marbl_parms, only : c10
  use marbl_parms, only : mpercm
  use marbl_parms, only : blank_fmt
  use marbl_parms, only : delim_fmt
  use marbl_parms, only : ndelim_fmt
  use marbl_parms, only : marbl_params_init, marbl_params_print
  use marbl_parms, only : grz_fnc_michaelis_menten
  use marbl_parms, only : grz_fnc_sigmoidal
  use marbl_parms, only : f_qsw_par
  use marbl_parms, only : parm_Fe_bioavail
  use marbl_parms, only : dust_to_Fe
  use marbl_parms, only : parm_BSIbury
  use marbl_parms, only : parm_POMbury
  use marbl_parms, only : denitrif_C_N
  use marbl_parms, only : parm_Red_Fe_C
  use marbl_parms, only : Q
  use marbl_parms, only : Qp_zoo_pom
  use marbl_parms, only : spd
  use marbl_parms, only : parm_CaCO3_diss
  use marbl_parms, only : parm_POC_diss
  use marbl_parms, only : parm_SiO2_diss
  use marbl_parms, only : parm_scalelen_z
  use marbl_parms, only : parm_scalelen_vals
  use marbl_parms, only : caco3_poc_min
  use marbl_parms, only : CaCO3_sp_thres
  use marbl_parms, only : CaCO3_temp_thres1
  use marbl_parms, only : CaCO3_temp_thres2
  use marbl_parms, only : DOC_reminR_light
  use marbl_parms, only : DON_reminR_light
  use marbl_parms, only : DOP_reminR_light
  use marbl_parms, only : DOC_reminR_dark
  use marbl_parms, only : DON_reminR_dark
  use marbl_parms, only : DOP_reminR_dark
  use marbl_parms, only : DOCr_reminR0
  use marbl_parms, only : DONr_reminR0
  use marbl_parms, only : DOPr_reminR0
  use marbl_parms, only : DOCprod_refract
  use marbl_parms, only : DONprod_refract
  use marbl_parms, only : DOPprod_refract
  use marbl_parms, only : POCremin_refract
  use marbl_parms, only : PONremin_refract
  use marbl_parms, only : POPremin_refract
  use marbl_parms, only : DOCriv_refract
  use marbl_parms, only : DONriv_refract
  use marbl_parms, only : DOPriv_refract
  use marbl_parms, only : f_toDON
  use marbl_parms, only : dps
  use marbl_parms, only : dust_fescav_scale
  use marbl_parms, only : f_graze_CaCO3_REMIN
  use marbl_parms, only : f_graze_si_remin
  use marbl_parms, only : f_graze_sp_poc_lim
  use marbl_parms, only : f_photosp_CaCO3
  use marbl_parms, only : fe_max_scale2
  use marbl_parms, only : Fe_scavenge_thres1
  use marbl_parms, only : parm_f_prod_sp_CaCO3
  use marbl_parms, only : parm_Fe_scavenge_rate0
  use marbl_parms, only : parm_kappa_nitrif
  use marbl_parms, only : parm_labile_ratio
  use marbl_parms, only : parm_nitrif_par_lim
  use marbl_parms, only : parm_o2_min
  use marbl_parms, only : parm_o2_min_delta
  use marbl_parms, only : parm_red_d_c_o2
  use marbl_parms, only : parm_red_d_c_o2_diaz
  use marbl_parms, only : parm_Remin_D_C_O2
  use marbl_parms, only : q_10
  use marbl_parms, only : QCaCO3_max
  use marbl_parms, only : Qfe_zoo
  use marbl_parms, only : r_Nfix_photo
  use marbl_parms, only : spc_poc_fac
  use marbl_parms, only : yps

  use marbl_sizes, only : ecosys_base_tracer_cnt    
  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt
  use marbl_sizes, only : grazer_prey_cnt

  use marbl_parms, only : grazing  
  use marbl_parms, only : autotrophs
  use marbl_parms, only : zooplankton

  use marbl_internal_types  , only : carbonate_type
  use marbl_internal_types  , only : zooplankton_type
  use marbl_internal_types  , only : autotroph_type
  use marbl_internal_types  , only : zooplankton_secondary_species_type
  use marbl_internal_types  , only : autotroph_secondary_species_type
  use marbl_internal_types  , only : dissolved_organic_matter_type
  use marbl_internal_types  , only : column_sinking_particle_type
  use marbl_internal_types  , only : marbl_PAR_type
  use marbl_internal_types  , only : marbl_particulate_share_type
  use marbl_internal_types  , only : marbl_interior_share_type
  use marbl_internal_types  , only : marbl_autotroph_share_type
  use marbl_internal_types  , only : marbl_zooplankton_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_share_type
  use marbl_internal_types  , only : marbl_surface_forcing_internal_type
  use marbl_internal_types , only : marbl_tracer_index_type

  use marbl_interface_types , only : marbl_domain_type
  use marbl_interface_types , only : marbl_tracer_metadata_type
  use marbl_interface_types , only : marbl_tracer_read_type
  use marbl_interface_types , only : marbl_saved_state_type
  use marbl_interface_types , only : marbl_interior_forcing_input_type
  use marbl_interface_types , only : marbl_surface_forcing_indexing_type
  use marbl_interface_types , only : marbl_surface_forcing_output_type
  use marbl_interface_types , only : marbl_forcing_fields_type
  use marbl_interface_types , only : marbl_forcing_monthly_every_ts_type
  use marbl_interface_types , only : marbl_diagnostics_type

  use marbl_diagnostics_mod , only : marbl_diagnostics_set_surface_forcing
  use marbl_diagnostics_mod , only : marbl_diagnostics_set_interior_forcing

  use marbl_logging         , only : marbl_log_type

  implicit none
  private

  !-----------------------------------------------------------------------
  !  public/private member procedure declarations
  !-----------------------------------------------------------------------

  public  :: marbl_init_nml
  public  :: marbl_init_surface_forcing_fields
  public  :: marbl_init_tracer_metadata
  public  :: marbl_update_tracer_file_metadata
  public  :: marbl_set_interior_forcing
  public  :: marbl_set_surface_forcing

  private :: marbl_init_non_autotroph_tracer_metadata
  private :: marbl_init_surface_forcing_metadata
  private :: marbl_init_monthly_surface_forcing_metadata
  private :: marbl_init_particulate_terms
  private :: marbl_init_zooplankton_tracer_metadata         
  private :: marbl_init_autotroph_tracer_metadata           
  private :: marbl_update_particulate_terms_from_prior_level      
  private :: marbl_update_sinking_particle_from_prior_level       
  private :: marbl_setup_local_tracers                            
  private :: marbl_setup_local_zooplankton                        
  private :: marbl_setup_local_autotrophs                         
  private :: marbl_consistency_check_autotrophs                    
  private :: marbl_check_ecosys_tracer_count_consistency          
  private :: marbl_compute_particulate_terms                      
  private :: marbl_compute_autotroph_elemental_ratios             
  private :: marbl_compute_PAR
  private :: marbl_compute_carbonate_chemistry                    
  private :: marbl_compute_function_scaling                       
  private :: marbl_compute_Pprime                                 
  private :: marbl_compute_Zprime

  type, private :: zooplankton_local_type
     real (r8) :: C  ! local copy of model zooplankton C
  end type zooplankton_local_type

  type, private :: autotroph_local_type
     real (r8) :: Chl   ! local copy of model autotroph Chl
     real (r8) :: C     ! local copy of model autotroph C
     real (r8) :: Fe    ! local copy of model autotroph Fe
     real (r8) :: Si    ! local copy of model autotroph Si
     real (r8) :: CaCO3 ! local copy of model autotroph CaCO3
  end type autotroph_local_type

  !-----------------------------------------------------------------------
  ! flags
  !-----------------------------------------------------------------------

  ! control which portion of code are executed, useful for debugging
  logical (log_kind) ::  lsource_sink

  ! should ecosystem vars be written full depth
  logical (log_kind)  :: lecovars_full_depth_tavg 

  !-----------------------------------------------------------------------
  !  bury to sediment options
  !-----------------------------------------------------------------------

  ! option of threshold of caco3 burial ['fixed_depth', 'omega_calc']
  character(char_len) :: caco3_bury_thres_opt    

  ! integer version of caco3_bury_thres_opt
  integer (int_kind)  :: caco3_bury_thres_iopt   

  integer (int_kind), parameter :: caco3_bury_thres_iopt_fixed_depth = 1
  integer (int_kind), parameter :: caco3_bury_thres_iopt_omega_calc  = 2

  ! threshold depth for caco3_bury_thres_opt='fixed_depth'
  real (r8) :: caco3_bury_thres_depth  

  ! PON_sed_loss = PON_bury_coeff * Q * POC_sed_loss
  ! factor is used to avoid overburying PON like POC
  ! is when total C burial is matched to C riverine input
  real (r8) :: PON_bury_coeff 

  ! POP_sed_loss = POP_bury_coeff * Qp_zoo_pom * POC_sed_loss
  ! factor is used to enable forced closure of the P cycle
  ! i.e. POP_sed_loss = P inputs (riverine + atm dep)
  real (r8) :: POP_bury_coeff 

  !-----------------------------------------------------------------------
  ! pH parameters
  !-----------------------------------------------------------------------

  real (r8), parameter :: phlo_surf_init = 7.0_r8 ! low bound for surface ph for no prev soln
  real (r8), parameter :: phhi_surf_init = 9.0_r8 ! high bound for surface ph for no prev soln
  real (r8), parameter :: phlo_3d_init = 6.0_r8   ! low bound for subsurface ph for no prev soln
  real (r8), parameter :: phhi_3d_init = 9.0_r8   ! high bound for subsurface ph for no prev soln
  real (r8), parameter :: del_ph = 0.20_r8        ! delta-ph for prev soln

  !-----------------------------------------------------------------------
  !  input surface forcing
  !-----------------------------------------------------------------------

  ! FIXME #56 : move this option, and corresponding code to driver when
  ! surface forcing source is selected in driver, instead of MARBL
  logical (log_kind) :: liron_flux_derived

  type(marbl_forcing_monthly_every_ts_type), target :: dust_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: iron_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: fice_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: xkw_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: ap_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: nox_flux_monthly_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: nhy_flux_monthly_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: din_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dip_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: don_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dop_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dsi_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dfe_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: dic_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: alk_riv_flux_file_loc
  type(marbl_forcing_monthly_every_ts_type), target :: doc_riv_flux_file_loc

  !*****************************************************************************

contains

  !*****************************************************************************

  subroutine marbl_init_nml(nl_buffer, marbl_status_log)

    !  Initialize ecosys tracer module. This involves setting metadata, reading
    !  the module namelist, setting initial conditions, setting up forcing,
    !  and defining additional tavg variables.
    !
    use marbl_namelist_mod        , only : marbl_nl_cnt
    use marbl_namelist_mod        , only : marbl_nl_buffer_size
    use marbl_namelist_mod        , only : marbl_namelist
    use marbl_share_mod           , only : init_ecosys_option        
    use marbl_share_mod           , only : init_ecosys_init_file
    use marbl_share_mod           , only : init_ecosys_init_file_fmt
    use marbl_share_mod           , only : tracer_init_ext
    use marbl_share_mod           , only : ndep_data_type 
    use marbl_share_mod           , only : ndep_shr_stream_year_first
    use marbl_share_mod           , only : ndep_shr_stream_year_last
    use marbl_share_mod           , only : ndep_shr_stream_year_align
    use marbl_share_mod           , only : ndep_shr_stream_file
    use marbl_share_mod           , only : ndep_shr_stream_scale_factor
    use marbl_share_mod           , only : lflux_gas_co2
    use marbl_share_mod           , only : lflux_gas_o2
    use marbl_share_mod           , only : gas_flux_forcing_iopt_drv
    use marbl_share_mod           , only : gas_flux_forcing_iopt_file
    use marbl_share_mod           , only : gas_flux_forcing_iopt
    use marbl_share_mod           , only : gas_flux_forcing_file
    use marbl_share_mod           , only : atm_co2_const
    use marbl_share_mod           , only : atm_alt_co2_const
    use marbl_share_mod           , only : atm_co2_iopt
    use marbl_share_mod           , only : atm_co2_iopt_const
    use marbl_share_mod           , only : atm_co2_iopt_drv_prog
    use marbl_share_mod           , only : atm_co2_iopt_drv_diag
    use marbl_share_mod           , only : atm_alt_co2_iopt
    use marbl_share_mod           , only : dust_flux_source
    use marbl_share_mod           , only : iron_flux_source
    use marbl_share_mod           , only : iron_frac_in_dust
    use marbl_share_mod           , only : iron_frac_in_bc
    use marbl_share_mod           , only : dust_flux_file        
    use marbl_share_mod           , only : iron_flux_file        
    use marbl_share_mod           , only : fice_file        
    use marbl_share_mod           , only : xkw_file         
    use marbl_share_mod           , only : ap_file          
    use marbl_share_mod           , only : nox_flux_monthly_file 
    use marbl_share_mod           , only : nhy_flux_monthly_file 
    use marbl_share_mod           , only : din_riv_flux_file     
    use marbl_share_mod           , only : dip_riv_flux_file     
    use marbl_share_mod           , only : don_riv_flux_file     
    use marbl_share_mod           , only : dop_riv_flux_file     
    use marbl_share_mod           , only : dsi_riv_flux_file     
    use marbl_share_mod           , only : dfe_riv_flux_file     
    use marbl_share_mod           , only : dic_riv_flux_file     
    use marbl_share_mod           , only : alk_riv_flux_file     
    use marbl_share_mod           , only : doc_riv_flux_file     
    use marbl_share_mod           , only : liron_patch  
    use marbl_share_mod           , only : iron_patch_flux_filename  
    use marbl_share_mod           , only : iron_patch_month  
    use marbl_share_mod           , only : fesedflux_input 
    use marbl_share_mod           , only : marbl_freq_opt_never  
    use marbl_share_mod           , only : marbl_freq_opt_nmonth 
    use marbl_share_mod           , only : marbl_freq_opt_nyear  

    implicit none

    character(marbl_nl_buffer_size), intent(in)  :: nl_buffer(marbl_nl_cnt)
    type(marbl_log_type)           , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    character(*), parameter :: subname = 'marbl_mod:marbl_init_nml'
    character(len=char_len) :: log_message
    character(len=marbl_nl_buffer_size) :: tmp_nl_buffer

    integer (int_kind)           :: n                           ! index for looping over tracers
    character(char_len)          :: gas_flux_forcing_opt        ! option for forcing gas fluxes
    character(char_len)          :: atm_co2_opt                 ! option for atmospheric co2 concentration
    character(char_len)          :: atm_alt_co2_opt             ! option for atmospheric alternative CO2
    type(marbl_tracer_read_type) :: dust_flux_input             ! namelist input for dust_flux
    type(marbl_tracer_read_type) :: iron_flux_input             ! namelist input for iron_flux
    type(marbl_tracer_read_type) :: nox_flux_monthly_input      ! namelist input for nox_flux_monthly
    type(marbl_tracer_read_type) :: nhy_flux_monthly_input      ! namelist input for nhy_flux_monthly
    type(marbl_tracer_read_type) :: din_riv_flux_input          ! namelist input for din_riv_flux
    type(marbl_tracer_read_type) :: dip_riv_flux_input          ! namelist input for dip_riv_flux
    type(marbl_tracer_read_type) :: don_riv_flux_input          ! namelist input for don_riv_flux
    type(marbl_tracer_read_type) :: dop_riv_flux_input          ! namelist input for dop_riv_flux
    type(marbl_tracer_read_type) :: dsi_riv_flux_input          ! namelist input for dsi_riv_flux
    type(marbl_tracer_read_type) :: dfe_riv_flux_input          ! namelist input for dfe_riv_flux
    type(marbl_tracer_read_type) :: dic_riv_flux_input          ! namelist input for dic_riv_flux
    type(marbl_tracer_read_type) :: alk_riv_flux_input          ! namelist input for alk_riv_flux
    type(marbl_tracer_read_type) :: doc_riv_flux_input          ! namelist input for doc_riv_flux
    integer (int_kind)           :: nml_error                   ! namelist i/o error flag
    integer (int_kind)           :: zoo_ind                     ! zooplankton functional group index
    type(marbl_tracer_read_type) :: gas_flux_fice               ! ice fraction for gas fluxes
    type(marbl_tracer_read_type) :: gas_flux_ws                 ! wind speed for gas fluxes
    type(marbl_tracer_read_type) :: gas_flux_ap                 ! atmospheric pressure for gas fluxes
    character(char_len)          :: nutr_rest_file              ! file containing nutrient fields
    character(char_len)          :: nutr_variable_rest_file     ! file containing variable restoring info
    character(char_len)          :: nutr_variable_rest_file_fmt ! format of file containing variable restoring info
    logical (log_kind)           :: lnutr_variable_restore      ! geographically varying nutrient restoring (maltrud)
    logical (log_kind)           :: locmip_k1_k2_bug_fix

    namelist /ecosys_nml/                                                 &
         init_ecosys_option, init_ecosys_init_file, tracer_init_ext,      &
         init_ecosys_init_file_fmt,                                       &
         dust_flux_source, dust_flux_input,                               &
         iron_flux_source, iron_flux_input, fesedflux_input,              &
         iron_frac_in_dust, iron_frac_in_bc,                              &
         ndep_data_type, nox_flux_monthly_input, nhy_flux_monthly_input,  &
         ndep_shr_stream_year_first, ndep_shr_stream_year_last,           &
         ndep_shr_stream_year_align, ndep_shr_stream_file,                &
         ndep_shr_stream_scale_factor,                                    &
         din_riv_flux_input, dip_riv_flux_input, don_riv_flux_input,      &
         dop_riv_flux_input, dsi_riv_flux_input, dfe_riv_flux_input,      &
         dic_riv_flux_input, alk_riv_flux_input, doc_riv_flux_input,      &
         gas_flux_forcing_opt, gas_flux_forcing_file,                     &
         gas_flux_fice, gas_flux_ws, gas_flux_ap, nutr_rest_file,         &
         lsource_sink, lflux_gas_o2, lflux_gas_co2, locmip_k1_k2_bug_fix, &
         lnutr_variable_restore, nutr_variable_rest_file,                 &
         nutr_variable_rest_file_fmt, atm_co2_opt, atm_co2_const,         &
         atm_alt_co2_opt, atm_alt_co2_const,                              &
         liron_patch, iron_patch_flux_filename, iron_patch_month,         &
         caco3_bury_thres_opt, caco3_bury_thres_depth, &
         PON_bury_coeff, &
         lecovars_full_depth_tavg

    !-----------------------------------------------------------------------
    !  default namelist settings
    !-----------------------------------------------------------------------
    init_ecosys_option = 'unknown'
    init_ecosys_init_file = 'unknown'
    init_ecosys_init_file_fmt = 'bin'

    gas_flux_forcing_opt  = 'drv'
    gas_flux_forcing_file = 'unknown'

    gas_flux_fice%filename     = 'unknown'
    gas_flux_fice%file_varname = 'FICE'
    gas_flux_fice%scale_factor = c1
    gas_flux_fice%default_val  = c0
    gas_flux_fice%file_fmt     = 'bin'

    gas_flux_ws%filename     = 'unknown'
    gas_flux_ws%file_varname = 'XKW'
    gas_flux_ws%scale_factor = c1
    gas_flux_ws%default_val  = c0
    gas_flux_ws%file_fmt     = 'bin'

    gas_flux_ap%filename     = 'unknown'
    gas_flux_ap%file_varname = 'P'
    gas_flux_ap%scale_factor = c1
    gas_flux_ap%default_val  = c0
    gas_flux_ap%file_fmt     = 'bin'

    nutr_rest_file = 'unknown'

    !maltrud variable restoring
    lnutr_variable_restore      = .false.
    nutr_variable_rest_file     = 'unknown'
    nutr_variable_rest_file_fmt = 'bin'

    dust_flux_source             = 'monthly-calendar'
    dust_flux_input%filename     = 'unknown'
    dust_flux_input%file_varname = 'dust_flux'
    dust_flux_input%scale_factor = c1
    dust_flux_input%default_val  = c0
    dust_flux_input%file_fmt     = 'bin'

    iron_flux_source             = 'monthly-calendar'
    iron_flux_input%filename     = 'unknown'
    iron_flux_input%file_varname = 'iron_flux'
    iron_flux_input%scale_factor = c1
    iron_flux_input%default_val  = c0
    iron_flux_input%file_fmt     = 'bin'

    iron_frac_in_dust            = 0.035_r8 * 0.01_r8
    iron_frac_in_bc              = 0.06_r8

    fesedflux_input%filename     = 'unknown'
    fesedflux_input%file_varname = 'FESEDFLUXIN'
    fesedflux_input%scale_factor = c1
    fesedflux_input%default_val  = c0
    fesedflux_input%file_fmt     = 'bin'

    ndep_data_type = 'monthly-calendar'

    nox_flux_monthly_input%filename     = 'unknown'
    nox_flux_monthly_input%file_varname = 'nox_flux'
    nox_flux_monthly_input%scale_factor = c1
    nox_flux_monthly_input%default_val  = c0
    nox_flux_monthly_input%file_fmt     = 'bin'

    nhy_flux_monthly_input%filename     = 'unknown'
    nhy_flux_monthly_input%file_varname = 'nhy_flux'
    nhy_flux_monthly_input%scale_factor = c1
    nhy_flux_monthly_input%default_val  = c0
    nhy_flux_monthly_input%file_fmt     = 'bin'

    ndep_shr_stream_year_first = 1
    ndep_shr_stream_year_last  = 1
    ndep_shr_stream_year_align = 1
    ndep_shr_stream_file       = 'unknown'
    ndep_shr_stream_scale_factor = c1

    din_riv_flux_input%filename     = 'unknown'
    din_riv_flux_input%file_varname = 'din_riv_flux'
    din_riv_flux_input%scale_factor = c1
    din_riv_flux_input%default_val  = c0
    din_riv_flux_input%file_fmt     = 'nc'

    dip_riv_flux_input%filename     = 'unknown'
    dip_riv_flux_input%file_varname = 'dip_riv_flux'
    dip_riv_flux_input%scale_factor = c1
    dip_riv_flux_input%default_val  = c0
    dip_riv_flux_input%file_fmt     = 'nc'

    don_riv_flux_input%filename     = 'unknown'
    don_riv_flux_input%file_varname = 'don_riv_flux'
    don_riv_flux_input%scale_factor = c1
    don_riv_flux_input%default_val  = c0
    don_riv_flux_input%file_fmt     = 'nc'

    dop_riv_flux_input%filename     = 'unknown'
    dop_riv_flux_input%file_varname = 'dop_riv_flux'
    dop_riv_flux_input%scale_factor = c1
    dop_riv_flux_input%default_val  = c0
    dop_riv_flux_input%file_fmt     = 'nc'

    dsi_riv_flux_input%filename     = 'unknown'
    dsi_riv_flux_input%file_varname = 'dsi_riv_flux'
    dsi_riv_flux_input%scale_factor = c1
    dsi_riv_flux_input%default_val  = c0
    dsi_riv_flux_input%file_fmt     = 'nc'

    dfe_riv_flux_input%filename     = 'unknown'
    dfe_riv_flux_input%file_varname = 'dfe_riv_flux'
    dfe_riv_flux_input%scale_factor = c1
    dfe_riv_flux_input%default_val  = c0
    dfe_riv_flux_input%file_fmt     = 'nc'

    dic_riv_flux_input%filename     = 'unknown'
    dic_riv_flux_input%file_varname = 'dic_riv_flux'
    dic_riv_flux_input%scale_factor = c1
    dic_riv_flux_input%default_val  = c0
    dic_riv_flux_input%file_fmt     = 'nc'

    alk_riv_flux_input%filename     = 'unknown'
    alk_riv_flux_input%file_varname = 'alk_riv_flux'
    alk_riv_flux_input%scale_factor = c1
    alk_riv_flux_input%default_val  = c0
    alk_riv_flux_input%file_fmt     = 'nc'

    doc_riv_flux_input%filename     = 'unknown'
    doc_riv_flux_input%file_varname = 'doc_riv_flux'
    doc_riv_flux_input%scale_factor = c1
    doc_riv_flux_input%default_val  = c0
    doc_riv_flux_input%file_fmt     = 'nc'

    do n = 1, ecosys_base_tracer_cnt
       tracer_init_ext(n)%mod_varname  = 'unknown'
       tracer_init_ext(n)%filename     = 'unknown'
       tracer_init_ext(n)%file_varname = 'unknown'
       tracer_init_ext(n)%scale_factor = c1
       tracer_init_ext(n)%default_val  = c0
       tracer_init_ext(n)%file_fmt     = 'bin'
    end do

    lsource_sink          = .true.
    lflux_gas_o2          = .true.
    lflux_gas_co2         = .true.
    locmip_k1_k2_bug_fix  = .true.

    liron_patch              = .false.
    iron_patch_flux_filename = 'unknown_iron_patch_filename'
    iron_patch_month         = 1

    atm_co2_opt   = 'const'
    atm_co2_const = 280.0_r8

    atm_alt_co2_opt   = 'const'
    atm_alt_co2_const = 280.0_r8

    caco3_bury_thres_opt = 'omega_calc'
    caco3_bury_thres_depth = 3000.0e2

    PON_bury_coeff = 0.5_r8
    POP_bury_coeff = 1.0_r8

    lecovars_full_depth_tavg = .false.

    !-----------------------------------------------------------------------
    ! read the namelist buffer on every processor
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'ecosys_nml')
    read(tmp_nl_buffer, nml=ecosys_nml, iostat=nml_error)
    if (nml_error /= 0) then
      call marbl_status_log%log_error("error reading &ecosys_nml", subname)
      return
    else
      ! FIXME #16: this is printing contents of pop_in, not the entire ecosys_nml
      call marbl_status_log%log_namelist('ecosys_nml', tmp_nl_buffer, subname)
    end if

    !-----------------------------------------------------------------------
    ! reassign values temporary input values to correct arrays
    !-----------------------------------------------------------------------

    if (trim(gas_flux_forcing_opt) == 'drv') then
       gas_flux_forcing_iopt = gas_flux_forcing_iopt_drv
    else if (trim(gas_flux_forcing_opt) == 'file') then
       gas_flux_forcing_iopt = gas_flux_forcing_iopt_file
    else
       write(log_message, "(2A)") "unknown gas_flux_forcing_opt: ", trim(gas_flux_forcing_opt)
       call marbl_status_log%log_error(log_message, subname)
       return
    endif

    fice_file_loc%input             = gas_flux_fice
    xkw_file_loc%input              = gas_flux_ws
    ap_file_loc%input               = gas_flux_ap
    dust_flux_file_loc%input        = dust_flux_input
    iron_flux_file_loc%input        = iron_flux_input
    nox_flux_monthly_file_loc%input = nox_flux_monthly_input
    nhy_flux_monthly_file_loc%input = nhy_flux_monthly_input
    din_riv_flux_file_loc%input     = din_riv_flux_input
    dip_riv_flux_file_loc%input     = dip_riv_flux_input
    don_riv_flux_file_loc%input     = don_riv_flux_input
    dop_riv_flux_file_loc%input     = dop_riv_flux_input
    dsi_riv_flux_file_loc%input     = dsi_riv_flux_input
    dfe_riv_flux_file_loc%input     = dfe_riv_flux_input
    dic_riv_flux_file_loc%input     = dic_riv_flux_input
    alk_riv_flux_file_loc%input     = alk_riv_flux_input
    doc_riv_flux_file_loc%input     = doc_riv_flux_input

    !-----------------------------------------------------------------------
    !  set variables immediately dependent on namelist variables
    !-----------------------------------------------------------------------

    select case (atm_co2_opt)
    case ('const')
       atm_co2_iopt = atm_co2_iopt_const
    case ('drv_prog')
       atm_co2_iopt = atm_co2_iopt_drv_prog
    case ('drv_diag')
       atm_co2_iopt = atm_co2_iopt_drv_diag
    case default
       write(log_message, "(2A)") "unknown atm_co2_opt: ", trim(atm_co2_opt)
       call marbl_status_log%log_error(log_message, subname)
       return
    end select

    select case (atm_alt_co2_opt)
    case ('const')
       atm_alt_co2_iopt = atm_co2_iopt_const
    case default
       write(log_message, "(2A)") "unknown atm_alt_co2_opt: ", trim(atm_alt_co2_opt)
       call marbl_status_log%log_error(log_message, subname)
       return
    end select

    select case (caco3_bury_thres_opt)
    case ('fixed_depth')
       caco3_bury_thres_iopt = caco3_bury_thres_iopt_fixed_depth
    case ('omega_calc')
       caco3_bury_thres_iopt = caco3_bury_thres_iopt_omega_calc
    case default
       write(log_message, "(2A)") "unknown caco3_bury_thres_opt: ", trim(caco3_bury_thres_opt)
       call marbl_status_log%log_error(log_message, subname)
       return
    end select

    !-----------------------------------------------------------------------
    !  read ecosys_parms_nml namelist
    !-----------------------------------------------------------------------

    ! FIXME #11: eliminate marbl_parms!
    call marbl_params_init(nl_buffer, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('marbl_params_init', subname)
      return
    end if

    dust_flux_file        => dust_flux_file_loc
    iron_flux_file        => iron_flux_file_loc
    fice_file             => fice_file_loc
    xkw_file              => xkw_file_loc
    ap_file               => ap_file_loc
    nox_flux_monthly_file => nox_flux_monthly_file_loc
    nhy_flux_monthly_file => nhy_flux_monthly_file_loc
    din_riv_flux_file     => din_riv_flux_file_loc
    dip_riv_flux_file     => dip_riv_flux_file_loc
    don_riv_flux_file     => don_riv_flux_file_loc
    dop_riv_flux_file     => dop_riv_flux_file_loc
    dsi_riv_flux_file     => dsi_riv_flux_file_loc
    dfe_riv_flux_file     => dfe_riv_flux_file_loc
    dic_riv_flux_file     => dic_riv_flux_file_loc
    alk_riv_flux_file     => alk_riv_flux_file_loc
    doc_riv_flux_file     => doc_riv_flux_file_loc

  end subroutine marbl_init_nml

  !*****************************************************************************

  subroutine marbl_init_surface_forcing_fields(&
       ciso_on, num_elements, num_surface_forcing_fields, &
       surface_forcing_indices, surface_forcing_fields,   &
       marbl_status_log)

    !  Initialize the surface forcing_fields datatype with information from the
    !  namelist read
    !
    use marbl_share_mod, only : gas_flux_forcing_iopt_drv
    use marbl_share_mod, only : gas_flux_forcing_iopt_file
    use marbl_share_mod, only : gas_flux_forcing_iopt
    use marbl_share_mod, only : gas_flux_forcing_file
    use marbl_share_mod, only : dust_flux_source
    use marbl_share_mod, only : dust_flux_file
    use marbl_share_mod, only : iron_flux_source
    use marbl_share_mod, only : iron_flux_file
    use marbl_share_mod, only : fice_file
    use marbl_share_mod, only : xkw_file
    use marbl_share_mod, only : ap_file
    use marbl_share_mod, only : nox_flux_monthly_file
    use marbl_share_mod, only : nhy_flux_monthly_file
    use marbl_share_mod, only : din_riv_flux_file
    use marbl_share_mod, only : dip_riv_flux_file
    use marbl_share_mod, only : don_riv_flux_file
    use marbl_share_mod, only : dop_riv_flux_file
    use marbl_share_mod, only : dsi_riv_flux_file
    use marbl_share_mod, only : dfe_riv_flux_file
    use marbl_share_mod, only : dic_riv_flux_file
    use marbl_share_mod, only : alk_riv_flux_file
    use marbl_share_mod, only : doc_riv_flux_file
    use marbl_share_mod, only : atm_co2_iopt
    use marbl_share_mod, only : atm_co2_iopt_drv_prog
    use marbl_share_mod, only : atm_co2_iopt_drv_diag
    use marbl_share_mod, only : atm_co2_iopt_const
    use marbl_share_mod, only : atm_co2_const
    use marbl_share_mod, only : atm_alt_co2_const
    use marbl_share_mod, only : atm_alt_co2_iopt
    use marbl_share_mod, only : ndep_data_type 
    use marbl_share_mod, only : ndep_shr_stream_year_first
    use marbl_share_mod, only : ndep_shr_stream_year_last
    use marbl_share_mod, only : ndep_shr_stream_year_align
    use marbl_share_mod, only : ndep_shr_stream_file
    use marbl_share_mod, only : ndep_shr_stream_scale_factor
    use marbl_share_mod, only : lflux_gas_co2
    use marbl_share_mod, only : lflux_gas_o2

    implicit none

    logical (kind=log_kind)                   , intent(in)    :: ciso_on
    integer (KIND=int_kind)                   , intent(in)    :: num_elements
    integer (kind=int_kind)                   , intent(out)   :: num_surface_forcing_fields
    type(marbl_surface_forcing_indexing_type) , intent(out)   :: surface_forcing_indices
    type(marbl_forcing_fields_type)           , intent(out)   :: surface_forcing_fields
    type(marbl_log_type)                      , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_mod:marbl_init_surface_forcing_fields'
    character(len=char_len) :: log_message
    character(char_len) :: fsource                  
    character(char_len) :: filename                 
    character(char_len) :: varname                  
    character(char_len) :: file_varname                  
    character(char_len) :: driver_varname               
    character(char_len) :: units                    
    real (kind=r8)      :: constant
    logical             :: count_only ! true => count the diagnostics, false => add the diagnostics
    integer             :: imode      ! imode = 1, count_only is true, otherwise count_only is false
    !-----------------------------------------------------------------------

    associate(                                    &
         ind => surface_forcing_indices,          &
         forcing_fields => surface_forcing_fields &
         )

    ! First count then allocate memory for surface forcing fields

    num_surface_forcing_fields = 0
    do imode = 1,2

       if (imode == 1) then
          count_only = .true.
       else
          count_only = .false.
          call forcing_fields%construct(num_elements, num_surface_forcing_fields)
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'driver'
          varname    = 'surface_mask'
          driver_varname = 'SURFACE_MASK'
          units      = 'unknown' 
          call forcing_fields%add_forcing_field(&
               field_source=fsource, marbl_varname=varname, field_units=units, &
               marbl_driver_varname=varname, id=ind%surface_mask_id,          &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (ciso_on) then
          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'driver' 
             varname    = 'd13c'
             driver_varname = 'D13C'
             units      = 'unknown' 
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units, &
                  marbl_driver_varname=varname, id=ind%d13c_id,               &
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if

          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'driver'
             varname    = 'd14c'
             driver_varname = 'D14C'
             units      = 'unknown' 
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units, &
                  marbl_driver_varname=varname, id=ind%d14c_id,               &
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if

          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'driver'
             varname    = 'd14c_gloavg'
             driver_varname = 'D14C_GLOAVG'
             units      = 'unknown' 
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units,    &
                  marbl_driver_varname=varname, id=ind%d14c_glo_avg_id,       &
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'driver'
          varname    = 'u10_sqr'
          driver_varname = 'U10_SQR'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, marbl_varname=varname, field_units=units, &
               marbl_driver_varname=driver_varname, id=ind%u10_sqr_id,        &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'driver'
          varname    = 'sst'
          driver_varname = 'SST'
          units      = 'Temperature (C)'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, marbl_varname=varname, field_units=units, &
               marbl_driver_varname=driver_varname, id=ind%sst_id,            &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'driver'
          varname    = 'sss'
          driver_varname = 'SSS'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, marbl_varname=varname, field_units=units, &
               marbl_driver_varname=driver_varname, id=ind%sss_id,            &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (lflux_gas_co2) then
          if (atm_co2_iopt == atm_co2_iopt_const) then
             if (count_only) then
                num_surface_forcing_fields = num_surface_forcing_fields + 1
             else
                fsource    = 'constant'
                varname    = 'xco2'
                units      = 'unknown'
                call forcing_fields%add_forcing_field(&
                     field_source=fsource, marbl_varname=varname, field_units=units, &
                     field_constant = atm_co2_const, id=ind%xco2_id,          &
                     marbl_status_log = marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_forcing_field_error(marbl_status_log, varname, subname)
                  return
                end if
             end if
          else if (atm_co2_iopt == atm_co2_iopt_drv_prog .or. atm_co2_iopt == atm_co2_iopt_drv_diag) then 
             if (count_only) then
                num_surface_forcing_fields = num_surface_forcing_fields + 1
             else
                fsource    = 'driver'
                varname    = 'xco2'
                if (atm_co2_iopt == atm_co2_iopt_drv_prog) then
                   driver_varname = 'ATM_CO2_PROG'
                else
                   driver_varname = 'ATM_CO2_DIAG'
                end if
                units      = 'unknown'
                call forcing_fields%add_forcing_field(&
                     field_source=fsource, marbl_varname=varname, field_units=units,       &
                     marbl_driver_varname=driver_varname, id=ind%xco2_id,     &
                     marbl_status_log = marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_forcing_field_error(marbl_status_log, varname, subname)
                  return
                end if
             end if
          end if
       end if

       if (lflux_gas_co2) then
          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'constant'
             varname    = 'xco2_alt_co2'
             constant   = atm_alt_co2_const
             units      = 'unknown'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units, &
                  field_constant=atm_alt_co2_const, id=ind%xco2_alt_co2_id,   &
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if
       end if

       if (gas_flux_forcing_iopt == gas_flux_forcing_iopt_drv) then
          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'driver'
             varname    = 'Ice Fraction'
             driver_varname = 'ICE Fraction'
             units      = 'unknown'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units,    &
                  marbl_driver_varname=driver_varname, id=ind%ifrac_id,       &
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if
       elseif (gas_flux_forcing_iopt == gas_flux_forcing_iopt_file) then
          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'POP monthly calendar'
             varname    = 'Ice Fraction'
             units      = 'unknown'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units, &
                  marbl_forcing_calendar_name=fice_file, id=ind%ifrac_id,     &
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if
       end if

       if (gas_flux_forcing_iopt == gas_flux_forcing_iopt_drv) then
          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'driver'
             varname    = 'Piston Velocity'
             driver_varname = 'XKW_ICE'
             units      = 'unknown'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units,    &
                  marbl_driver_varname=driver_varname, id=ind%xkw_id,         &
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if
       elseif (gas_flux_forcing_iopt == gas_flux_forcing_iopt_file) then
          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'POP monthly calendar'
             varname    = 'Piston Velocity'
             units      = 'unknown'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units,    &
                  marbl_forcing_calendar_name=xkw_file, id=ind%xkw_id,        &
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if
       end if

       if (lflux_gas_o2 .or. lflux_gas_co2) then
          if (gas_flux_forcing_iopt == gas_flux_forcing_iopt_drv) then
             if (count_only) then
                num_surface_forcing_fields = num_surface_forcing_fields + 1
             else
                fsource    = 'driver'
                varname    = 'Atmospheric Pressure'
                driver_varname = 'AP_FILE_INPUT'
                units      = 'unknown'
                call forcing_fields%add_forcing_field(&
                     field_source=fsource, marbl_varname=varname, field_units=units,       &
                     marbl_driver_varname=driver_varname, id=ind%atm_pressure_id, &
                     marbl_status_log = marbl_status_log)
                if (marbl_status_log%labort_marbl) then
                  call log_add_forcing_field_error(marbl_status_log, varname, subname)
                  return
                end if
             end if
          end if
       elseif (gas_flux_forcing_iopt == gas_flux_forcing_iopt_file) then
          if (count_only) then
             num_surface_forcing_fields = num_surface_forcing_fields + 1
          else
             fsource    = 'POP monthly calendar'
             varname    = 'Atmospheric Pressure'
             units      = 'unknown'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units,    &
                  marbl_forcing_calendar_name=ap_file, id=ind%atm_pressure_id,&
                  marbl_status_log = marbl_status_log)
             if (marbl_status_log%labort_marbl) then
               call log_add_forcing_field_error(marbl_status_log, varname, subname)
               return
             end if
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          varname = 'Dust Flux'
          units   = 'g/cm^2/s'
          if (dust_flux_source == 'monthly-calendar') then
             fsource = 'POP monthly calendar'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units, &
                  marbl_forcing_calendar_name=dust_flux_file, id=ind%dust_flux_id,&
                  marbl_status_log = marbl_status_log)
          elseif (dust_flux_source == 'driver') then
             fsource        = 'driver'
             driver_varname = 'DUST_FLUX'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units, &
                  marbl_driver_varname=driver_varname, id=ind%dust_flux_id,       &
                  marbl_status_log = marbl_status_log)
          else
             log_message = 'unknown value for dust_flux_source ' // trim(dust_flux_source)
             call marbl_status_log%log_error(log_message, subname)
             return
          end if
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          varname = 'Iron Flux'
          if (iron_flux_source == 'monthly-calendar') then
             liron_flux_derived = .false.
             fsource            = 'POP monthly calendar'
             units              = 'nmol/cm^2/s'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units, &
                  marbl_forcing_calendar_name=iron_flux_file, id=ind%iron_flux_id,&
                  marbl_status_log = marbl_status_log)
          elseif (iron_flux_source == 'driver-derived') then
             liron_flux_derived = .true.
             fsource            = 'driver'
             driver_varname     = 'BLACK_CARBON_FLUX'
             units              = 'g/cm^2/s'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units,   &
                  marbl_driver_varname=driver_varname, id=ind%black_carbon_flux_id, &
                  marbl_status_log = marbl_status_log)
          else
             log_message = 'unknown value for iron_flux_source ' // trim(iron_flux_source)
             call marbl_status_log%log_error(log_message, subname)
             return
          end if
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          varname    = 'NOx Flux'
          units      = 'unknown'
          if (ndep_data_type == 'shr_stream') then
             fsource    = 'file'
             file_varname = 'NOy_deposition'
             ! stream_index = stream_index + 1 - line in forcing field routine
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname, field_units=units, &
                  unit_conv_factor = ndep_shr_stream_scale_factor,            &
                  file_varname = file_varname,                                &
                  year_first = ndep_shr_stream_year_first,                    &
                  year_last  = ndep_shr_stream_year_last,                     &
                  year_align = ndep_shr_stream_year_align,                    &
                  filename   = ndep_shr_stream_file,                          &
                  id=ind%nox_flux_id,                                         &
                  marbl_status_log = marbl_status_log)
          elseif (ndep_data_type == 'monthly-calendar') then
             fsource    = 'POP monthly calendar'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource, marbl_varname=varname,                &
                  field_units=units,                                          &
                  marbl_forcing_calendar_name=nox_flux_monthly_file,          &
                  id=ind%nox_flux_id,                                         &
                  marbl_status_log = marbl_status_log)
          else
             log_message = 'unknown value for ndep_data_type ' // trim(ndep_data_type)
             call marbl_status_log%log_error(log_message, subname)
             return
          end if
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          varname    = 'NHy Flux'
          units      = 'unknown'
          if (ndep_data_type == 'shr_stream') then
             fsource    = 'file'
             file_varname = 'NHx_deposition'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource,                                       &
                  marbl_varname=varname,                                      &
                  field_units=units,                                          &
                  unit_conv_factor = ndep_shr_stream_scale_factor,            &
                  file_varname = file_varname,                                &
                  year_first = ndep_shr_stream_year_first,                    &
                  year_last  = ndep_shr_stream_year_last,                     &
                  year_align = ndep_shr_stream_year_align,                    &
                  filename   = ndep_shr_stream_file,                          &
                  id=ind%nhy_flux_id,                                         &
                  marbl_status_log = marbl_status_log)
          elseif (ndep_data_type == 'monthly-calendar') then
             fsource    = 'POP monthly calendar'
             call forcing_fields%add_forcing_field(&
                  field_source=fsource,                                       &
                  marbl_varname=varname,                                      &
                  field_units=units,                                          &
                  marbl_forcing_calendar_name=nhy_flux_monthly_file,          &
                  id=ind%nhy_flux_id,                                         &
                  marbl_status_log = marbl_status_log)
          else
             log_message = 'unknown value for ndep_data_type ' // trim(ndep_data_type)
             call marbl_status_log%log_error(log_message, subname)
             return
          end if
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'DIN river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=din_riv_flux_file, &
               id=ind%din_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'DIP river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=dip_riv_flux_file, &
               id=ind%dip_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'DON river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=don_riv_flux_file, &
               id=ind%don_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'DOP river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=dop_riv_flux_file, &
               id=ind%dop_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'DSI river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=dsi_riv_flux_file, &
               id=ind%dsi_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'DFE river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=dfe_riv_flux_file, &
               id=ind%dfe_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'DIC river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=dic_riv_flux_file, &
               id=ind%dic_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'ALK river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=alk_riv_flux_file, &
               id=ind%alk_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

       if (count_only) then
          num_surface_forcing_fields = num_surface_forcing_fields + 1
       else
          fsource    = 'POP monthly calendar'
          varname    = 'DOC river flux'
          units      = 'unknown'
          call forcing_fields%add_forcing_field(&
               field_source=fsource, &
               marbl_varname=varname, &
               field_units=units, &
               marbl_forcing_calendar_name=doc_riv_flux_file, &
               id=ind%doc_riv_flux_id, &
               marbl_status_log = marbl_status_log)
          if (marbl_status_log%labort_marbl) then
            call log_add_forcing_field_error(marbl_status_log, varname, subname)
            return
          end if
       end if

    end do

    if (liron_flux_derived .and. ind%dust_flux_id .eq. 0) then
       log_message = 'liron_flux_derived is .true., but dust_flux_id == 0'
       call marbl_status_log%log_error(log_message, subname)
       return
    endif

    end associate

    ! FIXME #26: do we have any forcing fields that are required to be set?
    !            If so, check to make sure those indices are not zero here.

  end subroutine marbl_init_surface_forcing_fields

  !*****************************************************************************
  
  subroutine marbl_init_tracer_metadata(marbl_tracer_metadata,                &
             marbl_tracer_read, marbl_tracer_indices, marbl_status_log)

    !  Set tracer and forcing metadata

    use marbl_share_mod, only : init_ecosys_init_file
    use marbl_share_mod, only : init_ecosys_init_file_fmt

    implicit none

    type (marbl_tracer_metadata_type), intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type (marbl_tracer_read_type)    , intent(inout) :: marbl_tracer_read(:)
    type(marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices
    type(marbl_log_type)             , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    character(*), parameter :: subname = 'marbl_mod:marbl_init_tracer_metadata'

    integer (int_kind) :: non_living_biomass_ecosys_tracer_cnt ! number of non-autotroph ecosystem tracers
    integer (int_kind) :: n        ! index for looping over tracers
    integer (int_kind) :: zoo_ind  ! zooplankton functional group index
    integer (int_kind) :: auto_ind ! autotroph functional group index

    !-----------------------------------------------------------------------
    ! initialize tracer metatdata
    !-----------------------------------------------------------------------

    ! by default, all tracers are written to tavg as full depth and
    ! have scale_factor equal to one

    marbl_tracer_metadata(:)%lfull_depth_tavg   = .true.
    marbl_tracer_metadata(:)%scale_factor       = c1
    marbl_tracer_metadata(:)%tracer_module_name = 'ecosys'

    call marbl_init_surface_forcing_metadata()

    call marbl_init_non_autotroph_tracer_metadata(marbl_tracer_metadata,      &
         marbl_tracer_indices, non_living_biomass_ecosys_tracer_cnt)

    call marbl_check_ecosys_tracer_count_consistency(non_living_biomass_ecosys_tracer_cnt, marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace(                                 &
            'marbl_check_ecosys_tracer_count_consistency()', subname)
       return
    end if

    call marbl_init_zooplankton_tracer_metadata(marbl_tracer_metadata,        &
         marbl_tracer_indices)

    call marbl_init_autotroph_tracer_metadata(marbl_tracer_metadata,          &
         marbl_tracer_indices)

    !-----------------------------------------------------------------------
    !  set lfull_depth_tavg flag for short-lived ecosystem tracers
    !-----------------------------------------------------------------------

    ! Should be done in marbl_diagnostics, and without the _tavg name
    do zoo_ind = 1, zooplankton_cnt
       n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
    end do

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = lecovars_full_depth_tavg
       endif
    end do

    do n=1,ecosys_base_tracer_cnt
      marbl_tracer_read(n)%mod_varname  = marbl_tracer_metadata(n)%short_name
      marbl_tracer_read(n)%filename     = init_ecosys_init_file
      marbl_tracer_read(n)%file_varname = marbl_tracer_metadata(n)%short_name
      marbl_tracer_read(n)%file_fmt     = init_ecosys_init_file_fmt
      marbl_tracer_read(n)%scale_factor = c1
      marbl_tracer_read(n)%default_val  = c0
    end do

  end subroutine marbl_init_tracer_metadata

  !***********************************************************************

  subroutine marbl_set_interior_forcing( &
       ciso_on,                          &
       domain,                           &
       interior_forcing_input,           &
       saved_state,                      &
       interior_restore,                 &
       tracers,                          &
       dtracers,                         &
       marbl_tracer_indices,             &
       marbl_PAR,                        &
       marbl_interior_share,             &
       marbl_zooplankton_share,          &
       marbl_autotroph_share,            &
       marbl_particulate_share,          &
       interior_forcing_diags,           &
       interior_restore_diags,           &
       marbl_status_log)
    
    !  Compute time derivatives for ecosystem state variables

    use marbl_ciso_mod , only : marbl_ciso_set_interior_forcing
    use marbl_sizes    , only : marbl_total_tracer_cnt

    implicit none 

    logical (log_kind)                          , intent(in)    :: ciso_on   ! flag to turn on carbon isotope calculations
    type    (marbl_domain_type)                 , intent(in)    :: domain                                
    type    (marbl_interior_forcing_input_type) , intent(in)    :: interior_forcing_input
    real    (r8)                                , intent(in)    :: interior_restore(:,:) ! (marbl_total_tracer_cnt, km) local restoring terms for nutrients (mmol ./m^3/sec) 
    real    (r8)                                , intent(in)    :: tracers(:,: )         ! (marbl_total_tracer_cnt, km) tracer values 
    type    (marbl_PAR_type)                    , intent(inout) :: marbl_PAR
    type    (marbl_saved_state_type)            , intent(inout) :: saved_state
    real    (r8)                                , intent(out)   :: dtracers(:,:)          ! (marbl_total_tracer_cnt, km) computed source/sink terms
    type    (marbl_tracer_index_type)           , intent(in)    :: marbl_tracer_indices
    ! FIXME #17: intent is inout due to DIC_Loc
    type    (marbl_interior_share_type)         , intent(inout) :: marbl_interior_share(domain%km)
    type    (marbl_zooplankton_share_type)      , intent(inout) :: marbl_zooplankton_share(zooplankton_cnt, domain%km)
    type    (marbl_autotroph_share_type)        , intent(inout) :: marbl_autotroph_share(autotroph_cnt, domain%km)
    type    (marbl_particulate_share_type)      , intent(inout) :: marbl_particulate_share
    type    (marbl_diagnostics_type)            , intent(inout) :: interior_forcing_diags
    type    (marbl_diagnostics_type)            , intent(inout) :: interior_restore_diags
    type(marbl_log_type)                        , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_mod:marbl_set_interior_forcing'

    integer (int_kind) :: auto_ind  ! autotroph functional group index
    integer (int_kind) :: auto_ind2 ! autotroph functional group index
    integer (int_kind) :: zoo_ind   ! zooplankton functional group index
    integer (int_kind) :: zoo_ind2  ! zooplankton functional group index
    integer (int_kind) :: prey_ind  ! grazee group index
    integer (int_kind) :: pred_ind  ! grazer group index
    integer (int_kind) :: kk        ! index for looping over k levels
    integer (int_kind) :: d         ! diag index index
    integer (int_kind) :: n         ! tracer index
    integer (int_kind) :: k         ! vertical level index

    real (r8) :: O2_production(domain%km)    ! O2 production
    real (r8) :: O2_consumption(domain%km)   ! O2 consumption
    real (r8) :: nitrif(domain%km)           ! nitrification (NH4 -> NO3) (mmol N/m^3/sec)
    real (r8) :: denitrif(domain%km)         ! WC nitrification (NO3 -> N2) (mmol N/m^3/sec)
    real (r8) :: sed_denitrif(domain%km)     ! sedimentary denitrification (nmol N/cm^3/sec)
    real (r8) :: other_remin(domain%km)      ! organic C remin not due oxic or denitrif (nmolC/cm^3/sec)
    real (r8) :: Tfunc(domain%km)
    real (r8) :: Fe_scavenge_rate(domain%km) ! annual scavenging rate of iron as % of ambient
    real (r8) :: Fe_scavenge(domain%km)      ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
    real (r8) :: QA_dust_def(domain%km)
    real (r8) :: zsat_calcite(domain%km)     ! Calcite Saturation Depth
    real (r8) :: zsat_aragonite(domain%km)   ! Aragonite Saturation Depth
    real (r8) :: PON_remin(domain%km)        ! remin of PON
    real (r8) :: PON_sed_loss(domain%km)     ! loss of PON to sediments
    real (r8) :: POP_remin(domain%km)        ! remin of POP
    real (r8) :: POP_sed_loss(domain%km)     ! loss of POP to sediments
    real (r8) :: tracer_local(ecosys_base_tracer_cnt, domain%km)

    type(zooplankton_local_type)             :: zooplankton_local(zooplankton_cnt, domain%km)
    type(autotroph_local_type)               :: autotroph_local(autotroph_cnt, domain%km)
    type(autotroph_secondary_species_type)   :: autotroph_secondary_species(autotroph_cnt, domain%km)
    type(zooplankton_secondary_species_type) :: zooplankton_secondary_species(zooplankton_cnt, domain%km)
    type(dissolved_organic_matter_type)      :: dissolved_organic_matter(domain%km)
    type(carbonate_type)                     :: carbonate(domain%km)

    ! NOTE(bja, 2015-07) vectorization: arrays that are (n, k, c, i)
    ! probably can not be vectorized reasonably over c without memory
    ! copies. If we break up the main k loop, some of the (k, c) loops
    ! can probably be vectorized over k and / or c!
    !-----------------------------------------------------------------------

    ! NOTE(bja, 2015-07) dtracers=0 must come before the "not
    ! lsource_sink check to ensure correct answer when not doing
    ! computations.
    ! NOTE(mvertens, 2015-12) the following includes carbon isotopes if 
    ! ciso_on is true

    dtracers(:, :) = c0

    if (.not. lsource_sink) then
       !-----------------------------------------------------------------------
       !  exit immediately if computations are not to be performed
       !-----------------------------------------------------------------------
       return
    endif

    associate(                                                      &
         km                  => domain%km,                          &
         kmt                 => domain%kmt,                         &
         num_PAR_subcols     => domain%num_PAR_subcols,             & 
         delta_z1            => domain%delta_z(1),                  &           

         POC                 => marbl_particulate_share%POC,        &
         P_CaCO3             => marbl_particulate_share%P_CaCO3,    &
         P_SiO2              => marbl_particulate_share%P_SiO2,     &
         dust                => marbl_particulate_share%dust,       &
         P_iron              => marbl_particulate_share%P_iron,     &

         ph_prev_col         => saved_state%ph_prev_col,            &
         ph_prev_alt_co2_col => saved_state%ph_prev_alt_co2_col,    &

         dust_flux_in        => interior_forcing_input%dust_flux,   &
         temperature         => interior_forcing_input%temperature, &
         fesedflux           => interior_forcing_input%fesedflux,   &

         po4_ind           => marbl_tracer_indices%po4_ind,         &
         no3_ind           => marbl_tracer_indices%no3_ind,         &
         sio3_ind          => marbl_tracer_indices%sio3_ind,        &
         nh4_ind           => marbl_tracer_indices%nh4_ind,         &
         fe_ind            => marbl_tracer_indices%fe_ind,          &
         o2_ind            => marbl_tracer_indices%o2_ind,          &
         dic_ind           => marbl_tracer_indices%dic_ind,         &
         dic_alt_co2_ind   => marbl_tracer_indices%dic_alt_co2_ind, &
         alk_ind           => marbl_tracer_indices%alk_ind,         &
         doc_ind           => marbl_tracer_indices%doc_ind,         &
         don_ind           => marbl_tracer_indices%don_ind,         &
         dop_ind           => marbl_tracer_indices%dop_ind,         &
         dopr_ind          => marbl_tracer_indices%dopr_ind,        &
         donr_ind          => marbl_tracer_indices%donr_ind,        &
         docr_ind          => marbl_tracer_indices%docr_ind,        &

         PAR                 => marbl_PAR                           &
         )

    !-----------------------------------------------------------------------
    !  create local copies of model tracers
    !-----------------------------------------------------------------------

    do k = 1, km
       call marbl_setup_local_tracers(k, kmt, tracers(:, k), tracer_local(:, k))

       call marbl_setup_local_zooplankton(k, kmt, tracers(:, k),              &
            marbl_tracer_indices, zooplankton_local(:, k))

       call marbl_setup_local_autotrophs(k, kmt, tracers(:, k),               &
            marbl_tracer_indices, autotroph_local(:, k))
    enddo

    call marbl_init_particulate_terms(1, &
         POC, P_CaCO3, P_SiO2, dust, P_iron, QA_dust_def(:), dust_flux_in)

    !FIXME #27: new marbl timers need to be implemented to turn
    !           on timers here around this subroutine call
    call marbl_compute_carbonate_chemistry(domain, interior_forcing_input,    &
         tracer_local(:, :), marbl_tracer_indices, carbonate(:),              &
         ph_prev_col(:), ph_prev_alt_co2_col(:), zsat_calcite(:),             &
         zsat_aragonite(:), marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace(&
            'marbl_check_ecosys_tracer_count_consistency()', subname)
       return
    end if

    call marbl_consistency_check_autotrophs(autotroph_cnt, kmt, marbl_tracer_indices, &
         autotroph_local(:,1:kmt))

    call marbl_compute_PAR(domain, interior_forcing_input, autotroph_cnt, autotroph_local, PAR)

    do k = 1, km

       call marbl_compute_autotroph_elemental_ratios( autotroph_cnt,    &
            autotrophs, autotroph_local(:, k), tracer_local(:, k),      &
            marbl_tracer_indices, autotroph_secondary_species(:, k))

       call marbl_compute_function_scaling(temperature(k), Tfunc(k))

       call marbl_compute_Pprime(k, domain, autotroph_cnt, autotrophs, &
            autotroph_local(:, k), temperature(k), autotroph_secondary_species(:, k))

       call marbl_compute_autotroph_uptake(autotroph_cnt, autotrophs,         &
            tracer_local(:, k), marbl_tracer_indices,                         &
            autotroph_secondary_species(:, k))

       call marbl_compute_autotroph_photosynthesis(autotroph_cnt,       &
            num_PAR_subcols, autotrophs, autotroph_local(:, k),         &
            temperature(k), Tfunc(k), PAR%col_frac(:), &
            PAR%avg(k,:), autotroph_secondary_species(:, k))

       call marbl_compute_autotroph_phyto_diatoms (autotroph_cnt, autotrophs, &
            autotroph_local(:, k), marbl_tracer_indices,                      &
            autotroph_secondary_species(:, k))

       call marbl_compute_autotroph_calcification(autotroph_cnt, autotrophs, &
            autotroph_local(:, k),  temperature(k), autotroph_secondary_species(:, k))

       call marbl_compute_autotroph_nfixation(autotroph_cnt, autotrophs, &
            autotroph_secondary_species(:, k))

       call marbl_compute_autotroph_loss(autotroph_cnt, autotrophs, &
            Tfunc(k), autotroph_secondary_species(:, k))

       call marbl_compute_Zprime(k, domain, &
            zooplankton_cnt, zooplankton, zooplankton_local(:, k)%C, &
            Tfunc(k), zooplankton_secondary_species(:, k))

       call marbl_compute_grazing (autotroph_cnt, zooplankton_cnt, grazer_prey_cnt, autotrophs, &
            Tfunc(k), zooplankton_local(:, k), &
            zooplankton_secondary_species(:, k), autotroph_secondary_species(:, k))

       call marbl_compute_routing (autotroph_cnt, zooplankton_cnt, autotrophs, &
            zooplankton_secondary_species(:, k), autotroph_secondary_species(:, k))

       call marbl_compute_dissolved_organic_matter (k, autotroph_cnt, zooplankton_cnt, &
            num_PAR_subcols, autotrophs,                                               &
            zooplankton_secondary_species(:, k), autotroph_secondary_species(:, k),    &
            PAR%col_frac(:), PAR%interface(k-1,:), PAR%avg(k,:),                       &
            delta_z1, tracer_local(:, k), marbl_tracer_indices,                        &
            dissolved_organic_matter(k))

       call marbl_compute_large_detritus(k, autotroph_cnt, zooplankton_cnt, autotrophs, &
            zooplankton_secondary_species(:, k), autotroph_secondary_species(:, k),     &
            tracer_local(fe_ind, k), POC, P_CaCO3, P_SiO2, dust, P_iron,                &
            Fe_scavenge(k), Fe_scavenge_rate(k), marbl_tracer_indices)

       ! FIXME #28: need to pull particulate share out
       !            of compute_particulate_terms!
       call marbl_compute_particulate_terms(k, domain,                        &
            marbl_particulate_share, POC, P_CaCO3, P_SiO2, dust,              &
            P_iron, PON_remin(k), PON_sed_loss(k), POP_remin(k),              &
            POP_sed_loss(k), QA_dust_def(k), temperature(k),                  &
            tracer_local(:, k), carbonate(k), sed_denitrif(k),                &
            other_remin(k), fesedflux(k), ciso_on, marbl_tracer_indices,      &
            marbl_status_log)

       if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace('marbl_compute_particulate_terms()', &
                                                subname)
          return
       end if

       call marbl_compute_nitrif(k, num_PAR_subcols, kmt, &
            PAR%col_frac(:), PAR%interface(k-1,:), PAR%interface(k,:),  &
            PAR%KPARdz(k), tracer_local(nh4_ind, k), nitrif(k))

       call marbl_compute_denitrif(tracer_local(o2_ind, k), tracer_local(no3_ind, k), &
            dissolved_organic_matter(k)%DOC_remin, &
            dissolved_organic_matter(k)%DOCr_remin, &
            POC%remin(k), other_remin(k), sed_denitrif(k), denitrif(k))

       call marbl_compute_dtracer_local (autotroph_cnt, zooplankton_cnt, autotrophs, zooplankton, &
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

       if (ciso_on) then
          ! FIXME #28: need to pull particulate share out
          !            of compute_particulate_terms!
          call marbl_export_interior_shared_variables(tracer_local(:, k),       &
               marbl_tracer_indices, carbonate(k), dissolved_organic_matter(k), &
               QA_dust_def(k), marbl_interior_share(k))

          call marbl_export_zooplankton_shared_variables(zooplankton_cnt, &
               zooplankton_local(:, k), &
               zooplankton_secondary_species(:, k), &
               marbl_zooplankton_share(:, k))

          call marbl_export_autotroph_shared_variables(autotroph_cnt, &
               autotroph_local(:, k), &
               autotroph_secondary_species(:, k), &
               marbl_tracer_indices, &
               marbl_autotroph_share(:, k))
       end if

       if  (k < km) then
          call marbl_update_particulate_terms_from_prior_level(k+1, POC, P_CaCO3, &
               P_SiO2, dust, P_iron, QA_dust_def(:))
       endif

    end do ! k

    ! Compute interior diagnostics
    call marbl_diagnostics_set_interior_forcing(            &
         domain,                                            &
         interior_forcing_input,                            &
         dtracers,                                          &
         marbl_tracer_indices,                              &
         carbonate,                                         &
         autotroph_secondary_species,                       &         
         zooplankton_secondary_species,                     &
         dissolved_organic_matter,                          &
         marbl_particulate_share,                           &
         marbl_PAR,                                         &
         PON_remin, PON_sed_loss,                           &
         POP_remin,  POP_sed_loss,                          &
         sed_denitrif, other_remin, nitrif, denitrif,       &
         tracers(o2_ind, :), o2_production, o2_consumption, &
         fe_scavenge, fe_scavenge_rate,                     &
         interior_forcing_diags, &
         marbl_status_log)
    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace(&
            'marbl_diagnostics_set_interior_foricng()', subname)
       return
    end if

    ! Compute restore diagnostics
    do n = 1, ecosys_base_tracer_cnt
       interior_restore_diags%diags(n)%field_3d(:,1) = interior_restore(n,:)
    end do

    !  Compute time derivatives for ecosystem carbon isotope state variables
    if (ciso_on) then
       call marbl_ciso_set_interior_forcing(                        &
            marbl_domain                 = domain,                  &
            marbl_interior_forcing_input = interior_forcing_input,  &
            marbl_interior_share         = marbl_interior_share,    &
            marbl_zooplankton_share      = marbl_zooplankton_share, &
            marbl_autotroph_share        = marbl_autotroph_share,   &
            marbl_particulate_share      = marbl_particulate_share, &
            column_tracer                = tracers,                 &
            column_dtracer               = dtracers,                &
            marbl_tracer_indices         = marbl_tracer_indices,    &
            marbl_interior_diags         = interior_forcing_diags,  &
            marbl_status_log             = marbl_status_log)

       if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace(&
               'marbl_ciso_set_interior_foricng()', subname)
          return
       end if
    end if

    end associate

  end subroutine marbl_set_interior_forcing

  !***********************************************************************

  subroutine marbl_init_particulate_terms(k, &
       POC, P_CaCO3, P_SiO2, dust, P_iron, QA_dust_def, NET_DUST_IN)

    !  Set incoming fluxes (put into outgoing flux for first level usage).
    !  Set dissolution length, production fraction and mass terms.
    !
    !  The first 6 arguments are intent(inout) in
    !  order to preserve contents on other blocks.

    use marbl_share_mod, only : dust_flux_source
    use marbl_share_mod, only : dust_flux_file        

    integer(int_kind)                  , intent(in)    :: k
    real (r8)                          , intent(in)    :: net_dust_in     ! dust flux
    type(column_sinking_particle_type) , intent(inout) :: POC             ! base units = nmol C
    type(column_sinking_particle_type) , intent(inout) :: P_CaCO3         ! base units = nmol CaCO3
    type(column_sinking_particle_type) , intent(inout) :: P_SiO2          ! base units = nmol SiO2
    type(column_sinking_particle_type) , intent(inout) :: dust            ! base units = g
    type(column_sinking_particle_type) , intent(inout) :: P_iron          ! base units = nmol Fe
    real (r8)                          , intent(inout) :: QA_dust_def(:)  ! incoming deficit in the QA(dust) POC flux (km)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  parameters, from Armstrong et al. 2000
    !
    !  July 2002, length scale for excess POC and bSI modified by temperature
    !  Value given here is at Tref of 30 deg. C, JKM
    !-----------------------------------------------------------------------

    POC%diss      = parm_POC_diss   ! diss. length (cm), modified by TEMP
    POC%gamma     = c0              ! not used
    POC%mass      = 12.01_r8        ! molecular weight of POC
    POC%rho       = c0              ! not used

    P_CaCO3%diss  = parm_CaCO3_diss ! diss. length (cm)
    P_CaCO3%gamma = 0.10_r8         ! prod frac -> hard subclass
    P_CaCO3%mass  = 100.09_r8       ! molecular weight of CaCO
    P_CaCO3%rho   = 0.05_r8 * P_CaCO3%mass / POC%mass ! QA mass ratio for CaCO3

    P_SiO2%diss   = parm_SiO2_diss  ! diss. length (cm), modified by TEMP
    P_SiO2%gamma  = 0.10_r8         ! prod frac -> hard subclass
    P_SiO2%mass   = 60.08_r8        ! molecular weight of SiO2
    P_SiO2%rho    = 0.05_r8 * P_SiO2%mass / POC%mass ! QA mass ratio for SiO2

    dust%diss     = 30000.0_r8      ! diss. length (cm)
    dust%gamma    = 0.99_r8         ! prod frac -> hard subclass
    dust%mass     = 1.0e9_r8        ! base units are already grams
    dust%rho      = 0.05_r8 * dust%mass / POC%mass ! QA mass ratio for dust

    P_iron%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron%mass   = c0              ! not used
    P_iron%rho    = c0              ! not used

    !-----------------------------------------------------------------------
    !  Set incoming fluxes
    !-----------------------------------------------------------------------

    P_CaCO3%sflux_out(k) = c0
    P_CaCO3%hflux_out(k) = c0
    P_CaCO3%sflux_in(k) = P_CaCO3%sflux_out(k)
    P_CaCO3%hflux_in(k) = P_CaCO3%hflux_out(k)

    P_SiO2%sflux_out(k) = c0
    P_SiO2%hflux_out(k) = c0
    P_SiO2%sflux_in(k) = P_SiO2%sflux_out(k)
    P_SiO2%hflux_in(k) = P_SiO2%hflux_out(k)

    ! FIXME #56 : need a better (i.e., extensible and maintainable) conditional here
    if (((dust_flux_source == 'monthly-calendar') .and. dust_flux_file%has_data) .or. &
        (dust_flux_source == 'driver')) then
       dust%sflux_out(k) = (c1 - dust%gamma) * net_dust_in
       dust%hflux_out(k) = dust%gamma * net_dust_in
    else
       dust%sflux_out(k) = c0
       dust%hflux_out(k) = c0
    endif
    dust%sflux_in(k) = dust%sflux_out(k)
    dust%hflux_in(k) = dust%hflux_out(k)

    P_iron%sflux_out(k) = c0
    P_iron%hflux_out(k) = c0
    P_iron%sflux_in(k) = P_iron%sflux_out(k)
    P_iron%hflux_in(k) = P_iron%hflux_out(k)

    !-----------------------------------------------------------------------
    !  Hard POC is QA flux and soft POC is excess POC.
    !-----------------------------------------------------------------------

    POC%sflux_out(k) = c0
    POC%hflux_out(k) = c0
    POC%sflux_in(k) = POC%sflux_out(k)
    POC%hflux_in(k) = POC%hflux_out(k)

    !-----------------------------------------------------------------------
    !  Compute initial QA(dust) POC flux deficit.
    !-----------------------------------------------------------------------

    QA_dust_def(k) = dust%rho * (dust%sflux_out(k) + dust%hflux_out(k))

  end subroutine marbl_init_particulate_terms

  !***********************************************************************

  subroutine marbl_update_particulate_terms_from_prior_level(k, &
       POC, P_CaCO3, P_SiO2, dust, P_iron, QA_dust_def)

    integer (int_kind)                 , intent(in)    :: k ! vertical model level
    type(column_sinking_particle_type) , intent(inout) :: POC, P_CaCO3, P_SiO2, dust, P_iron
    real(r8)                           , intent(inout) :: QA_dust_def(:) !(km)

    ! NOTE(bja, 2015-04) assume that k == 1 condition was handled by
    ! call to init_particulate_terms()
    if (k > 1) then
       !-----------------------------------------------------------------------
       ! NOTE: incoming fluxes are outgoing fluxes from previous level
       !
       ! initialize loss to sediments = 0
       !-----------------------------------------------------------------------
       call marbl_update_sinking_particle_from_prior_level(k, P_CaCO3)

       call marbl_update_sinking_particle_from_prior_level(k, P_SiO2)

       call marbl_update_sinking_particle_from_prior_level(k, dust)

       call marbl_update_sinking_particle_from_prior_level(k, POC)

       call marbl_update_sinking_particle_from_prior_level(k, P_iron)

       QA_dust_def(k) = QA_dust_def(k-1)
    end if

  end subroutine marbl_update_particulate_terms_from_prior_level

  !***********************************************************************

  subroutine marbl_update_sinking_particle_from_prior_level(k, sinking_particle)

    integer (int_kind), intent(in) :: k
    type(column_sinking_particle_type), intent(inout) :: sinking_particle

    ! NOTE(bja, 201504) level k influx is equal to the level k-1 outflux.
    sinking_particle%sflux_out(k) = sinking_particle%sflux_out(k-1)
    sinking_particle%hflux_out(k) = sinking_particle%hflux_out(k-1)
    sinking_particle%sflux_in(k)  = sinking_particle%sflux_out(k-1)
    sinking_particle%hflux_in(k)  = sinking_particle%hflux_out(k-1)

  end subroutine marbl_update_sinking_particle_from_prior_level

  !***********************************************************************

  subroutine marbl_compute_particulate_terms(k, domain,                       &
             marbl_particulate_share, POC, P_CaCO3, P_SiO2, dust, P_iron,     &
             PON_remin, PON_sed_loss, POP_remin, POP_sed_loss, QA_dust_def,   &
             temperature, tracer_local, carbonate, sed_denitrif, other_remin, &
             fesedflux, lexport_shared_vars, marbl_tracer_indices,            &
             marbl_status_log)

    !  Compute outgoing fluxes and remineralization terms. Assumes that
    !  production terms have been set. Incoming fluxes are assumed to be the
    !  outgoing fluxes from the previous level.
    !
    !  It is assumed that there is no production of dust.
    !
    !  Instantaneous remineralization in the bottom cell is implemented by
    !  setting the outgoing flux to zero.
    !
    !  For POC, the hard subclass is the POC flux qualitatively associated
    !  with the ballast flux. The soft subclass is the excess POC flux.
    !
    !  Remineralization for the non-iron particulate pools is computing
    !  by first computing the outgoing flux and then computing the
    !  remineralization from conservation, i.e.
    !     flux_in - flux_out + prod * dz - remin * dz == 0.
    !
    !  For iron, remineralization is first computed from POC remineralization
    !  and then flux_out is computed from conservation. If the resulting
    !  flux_out is negative or should be zero because of the sea floor, the
    !  remineralization is adjusted.
    !  Note: all the sinking iron is in the P_iron%sflux pool, hflux Fe not
    !        explicitly tracked, it is assumed that total iron remin is
    !        proportional to total POC remin.
    !
    !  Based upon Armstrong et al. 2000
    !
    !  July 2002, added temperature effect on remin length scale of
    !  excess POC (all soft POM& Iron) and on SiO2.
    !  new variable passed into ballast, Tfunc, main Temperature function
    !  computed in ecosystem routine.  scaling factor for dissolution
    !  of excess POC, Fe, and Bsi now varies with location (f(temperature)).
    !
    !  Added diffusive iron flux from sediments at depths < 1100m,
    !  based on Johnson et al., 1999, value of 5 umolFe/m2/day,
    !      this value too high, using 2 umolFe/m2/day here
    !
    !  Allow hard fraction of ballast to remin with long length scale 40, 000m
    !     thus ~ 10% of hard ballast remins over 4000m water column.
    !
    !  Sinking dust flux is decreased by assumed instant solubility/dissolution
    !     at ocean surface from the parm_Fe_bioavail.
    !
    !  Modified to allow different Q10 factors for soft POM and bSI remin,
    !  water TEMP is now passed in instead of Tfunc (1/2005, JKM)

    ! !USES:

    use marbl_parms           , only : Tref

    integer (int_kind)                      , intent(in)    :: k                   ! vertical model level
    type(marbl_domain_type)                 , intent(in)    :: domain                              
    real (r8)                               , intent(in)    :: temperature         ! temperature for scaling functions bsi%diss
    real (r8), dimension(ecosys_base_tracer_cnt) , intent(in)    :: tracer_local        ! local copies of model tracer concentrations
    type(carbonate_type)                    , intent(in)    :: carbonate
    logical (log_kind)                      , intent(in)    :: lexport_shared_vars ! flag to save shared_vars or not
    real(r8)                                , intent(in)    :: fesedflux           ! sedimentary Fe input
    real(r8)                                , intent(out)   :: PON_remin           ! remin of PON
    real(r8)                                , intent(out)   :: PON_sed_loss        ! loss of PON to sediments
    type(column_sinking_particle_type)      , intent(inout) :: POC                 ! base units = nmol C
    type(column_sinking_particle_type)      , intent(inout) :: P_CaCO3             ! base units = nmol CaCO3
    type(column_sinking_particle_type)      , intent(inout) :: P_SiO2              ! base units = nmol SiO2
    type(column_sinking_particle_type)      , intent(inout) :: dust                ! base units = g
    type(column_sinking_particle_type)      , intent(inout) :: P_iron              ! base units = nmol Fe
    real (r8)                               , intent(out)   :: POP_remin           ! remin of POP
    real (r8)                               , intent(out)   :: POP_sed_loss        ! loss of POP to sediments
    real (r8)                               , intent(inout) :: QA_dust_def         ! incoming deficit in the QA(dust) POC flux
    real (r8)                               , intent(out)   :: sed_denitrif        ! sedimentary denitrification (umolN/cm^2/s)
    real (r8)                               , intent(out)   :: other_remin         ! sedimentary remin not due to oxic or denitrification
    type(marbl_particulate_share_type)      , intent(inout) :: marbl_particulate_share
    type(marbl_tracer_index_type)           , intent(in)    :: marbl_tracer_indices
    type(marbl_log_type)                    , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real (r8) :: poc_diss, & ! diss. length used (cm)
         sio2_diss, & ! diss. length varies spatially with O2
         caco3_diss, &
         dust_diss

    character(*), parameter :: &
         subname = 'marbl_mod:marbl_compute_particulate_terms'
    character(len=char_len) :: log_message

    real (r8) :: TfuncS  ! temperature scaling from soft POM remin

    real (r8) :: &
         DECAY_Hard,         & ! scaling factor for dissolution of Hard Ballast
         DECAY_HardDust        ! scaling factor for dissolution of Hard dust

    real (r8) :: &
         decay_POC_E,        & ! scaling factor for dissolution of excess POC
         decay_SiO2,         & ! scaling factor for dissolution of SiO2
         decay_CaCO3,        & ! scaling factor for dissolution of CaCO3
         decay_dust,         & ! scaling factor for dissolution of dust
         POC_PROD_avail,     & ! POC production available for excess POC flux
         new_QA_dust_def,    & ! outgoing deficit in the QA(dust) POC flux
         scalelength,        & ! used to scale dissolution length scales as function of depth
         o2_scalefactor,     & ! used to scale dissolution length scales as function of o2
         flux, flux_alt,     & ! temp variables used to update sinking flux
         dz_loc, dzr_loc       ! dz, dzr at a particular i, j location

    real (r8), parameter :: &  ! o2_sf is an abbreviation for o2_scalefactor
         o2_sf_o2_range_hi = 50.0_r8, & ! apply o2_scalefactor for O2_loc less than this
         o2_sf_o2_range_lo =  5.0_r8, & ! o2_scalefactor is constant for O2_loc < this parameter
         o2_sf_val_lo_o2   =  2.5_r8    ! o2_scalefactor for O2_loc < o2_sf_o2_range_lo

    integer (int_kind) :: n     ! loop indices

    logical (log_kind) :: poc_error   ! POC error flag
    !-----------------------------------------------------------------------

    associate(                                                                         &
         column_kmt               => domain%kmt,                                       &
         delta_z                  => domain%delta_z,                                   &
         zw                       => domain%zw,                                        & 
         O2_loc                   => tracer_local(marbl_tracer_indices%o2_ind),        &
         NO3_loc                  => tracer_local(marbl_tracer_indices%no3_ind),       &
         POC_PROD_avail_fields    => marbl_particulate_share%POC_PROD_avail_fields,    & ! IN/OUT
         decay_POC_E_fields       => marbl_particulate_share%decay_POC_E_fields,       & ! IN/OUT
         decay_CaCO3_fields       => marbl_particulate_share%decay_CaCO3_fields,       & ! IN/OUT
         poc_diss_fields          => marbl_particulate_share%poc_diss_fields,          & ! IN/OUT
         caco3_diss_fields        => marbl_particulate_share%caco3_diss_fields,        & ! IN/OUT
         P_CaCO3_sflux_out_fields => marbl_particulate_share%P_CaCO3_sflux_out_fields, & ! IN/OUT
         P_CaCO3_hflux_out_fields => marbl_particulate_share%P_CaCO3_hflux_out_fields, & ! IN/OUT
         POC_sflux_out_fields     => marbl_particulate_share%POC_sflux_out_fields,     & ! IN/OUT
         POC_hflux_out_fields     => marbl_particulate_share%POC_hflux_out_fields,     & ! IN/OUT
         POC_remin_fields         => marbl_particulate_share%POC_remin_fields,         & ! IN/OUT
         P_CaCO3_remin_fields     => marbl_particulate_share%P_CaCO3_remin_fields,     & ! IN/OUT
         DECAY_Hard_fields        => marbl_particulate_share%DECAY_Hard_fields         & ! IN/OUT
         )

    !-----------------------------------------------------------------------
    !  initialize local copy of percent sed
    !-----------------------------------------------------------------------
    sed_denitrif = c0
    other_remin = c0

    !-----------------------------------------------------------------------
    !  compute scalelength and decay factors
    !-----------------------------------------------------------------------

    if (zw(k) < parm_scalelen_z(1)) then
       scalelength = parm_scalelen_vals(1)
    else if (zw(k) >= parm_scalelen_z(size(parm_scalelen_z))) then
       scalelength = parm_scalelen_vals(size(parm_scalelen_z))
    else
       do n = 2, size(parm_scalelen_z)
          if (zw(k) < parm_scalelen_z(n)) then
             scalelength = parm_scalelen_vals(n-1) &
                  + (parm_scalelen_vals(n) - parm_scalelen_vals(n-1)) &
                  * (zw(k) - parm_scalelen_z(n-1))/(parm_scalelen_z(n) - parm_scalelen_z(n-1))
             exit
          endif
       end do
    endif

    DECAY_Hard     = exp(-delta_z(k) / 4.0e6_r8)
    DECAY_HardDust = exp(-delta_z(k) / 1.2e8_r8)

    !----------------------------------------------------------------------
    !   Tref = 30.0 reference temperature (deg. C)
    !-----------------------------------------------------------------------
    TfuncS = 1.5_r8**(((temperature + T0_Kelvin) - (Tref + T0_Kelvin)) / c10)

    poc_error = .false.
    dz_loc = delta_z(k)

    if (k <= column_kmt) then

       dzr_loc    = c1 / dz_loc
       poc_diss   = POC%diss
       sio2_diss  = P_SiO2%diss
       caco3_diss = P_CaCO3%diss
       dust_diss  = dust%diss

       !-----------------------------------------------------------------------
       !  increase POC diss length scale where O2 concentrations are low
       !-----------------------------------------------------------------------

       if (O2_loc < o2_sf_o2_range_hi) then
          o2_scalefactor = c1 + (o2_sf_val_lo_o2 - c1) * &
               min(c1, (o2_sf_o2_range_hi - O2_loc)/(o2_sf_o2_range_hi - o2_sf_o2_range_lo))
          poc_diss   = poc_diss   * o2_scalefactor
          sio2_diss  = sio2_diss  * o2_scalefactor
          caco3_diss = caco3_diss * o2_scalefactor
          dust_diss  = dust_diss  * o2_scalefactor
       endif

       !-----------------------------------------------------------------------
       !  apply scalelength factor to length scales
       !-----------------------------------------------------------------------

       poc_diss = scalelength * poc_diss
       sio2_diss = scalelength * sio2_diss
       caco3_diss = scalelength * caco3_diss
       dust_diss = scalelength * dust_diss

       !-----------------------------------------------------------------------
       !  decay_POC_E and decay_SiO2 set locally, modified by O2
       !-----------------------------------------------------------------------

       decay_POC_E = exp(-dz_loc / poc_diss)
       decay_SiO2  = exp(-dz_loc / sio2_diss)
       decay_CaCO3 = exp(-dz_loc / caco3_diss)
       decay_dust  = exp(-dz_loc / dust_diss)

       !-----------------------------------------------------------------------
       !  Set outgoing fluxes for non-iron pools.
       !  The outoing fluxes for ballast materials are from the
       !  solution of the coresponding continuous ODE across the model
       !  level. The ODE has a constant source term and linear decay.
       !  It is assumed that there is no sub-surface dust production.
       !-----------------------------------------------------------------------

       P_CaCO3%sflux_out(k) = P_CaCO3%sflux_in(k) * decay_CaCO3 + &
            P_CaCO3%prod(k) * ((c1 - P_CaCO3%gamma) * (c1 - decay_CaCO3) &
            * caco3_diss)

       P_CaCO3%hflux_out(k) = P_CaCO3%hflux_in(k) * DECAY_Hard + &
            P_CaCO3%prod(k) * (P_CaCO3%gamma * dz_loc)

       P_SiO2%sflux_out(k) = P_SiO2%sflux_in(k) * decay_SiO2 + &
            P_SiO2%prod(k) * ((c1 - P_SiO2%gamma) * (c1 - decay_SiO2) &
            * sio2_diss)

       P_SiO2%hflux_out(k) = P_SiO2%hflux_in(k) * DECAY_Hard + &
            P_SiO2%prod(k) * (P_SiO2%gamma * dz_loc)

       dust%sflux_out(k) = dust%sflux_in(k) * decay_dust

       dust%hflux_out(k) = dust%hflux_in(k) * DECAY_HardDust

       !-----------------------------------------------------------------------
       !  Compute how much POC_PROD is available for deficit reduction
       !  and excess POC flux after subtracting off fraction of non-dust
       !  ballast production from net POC_PROD.
       !-----------------------------------------------------------------------

       POC_PROD_avail = POC%prod(k) - &
            P_CaCO3%rho * P_CaCO3%prod(k) - &
            P_SiO2%rho * P_SiO2%prod(k)

       !-----------------------------------------------------------------------
       !  Check for POC production bounds violations
       !-----------------------------------------------------------------------

       if (POC_PROD_avail < c0) then
          poc_error = .true.
       endif

       !-----------------------------------------------------------------------
       !  Compute 1st approximation to new QA_dust_def, the QA_dust
       !  deficit leaving the cell. Ignore POC_PROD_avail at this stage.
       !-----------------------------------------------------------------------

       if (QA_dust_def > 0) then
          new_QA_dust_def = QA_dust_def * &
               (dust%sflux_out(k) + dust%hflux_out(k)) / &
               (dust%sflux_in(k) + dust%hflux_in(k))
       else
          new_QA_dust_def = c0
       endif

       !-----------------------------------------------------------------------
       !  Use POC_PROD_avail to reduce new_QA_dust_def.
       !-----------------------------------------------------------------------

       if (new_QA_dust_def > c0) then
          new_QA_dust_def = new_QA_dust_def - POC_PROD_avail * dz_loc
          if (new_QA_dust_def < c0) then
             POC_PROD_avail = -new_QA_dust_def * dzr_loc
             new_QA_dust_def = c0
          else
             POC_PROD_avail = c0
          endif
       endif

       QA_dust_def = new_QA_dust_def

       ! Save certain fields for use by other modules
       if (lexport_shared_vars) then
          POC_PROD_avail_fields(k) = POC_PROD_avail
          decay_POC_E_fields(k)    = decay_POC_E
          decay_CaCO3_fields(k)    = decay_CaCO3
          poc_diss_fields(k)       = poc_diss
          caco3_diss_fields(k)     = caco3_diss
       endif

       !-----------------------------------------------------------------------
       !  Compute outgoing POC fluxes. QA POC flux is computing using
       !  ballast fluxes and new_QA_dust_def. If no QA POC flux came in
       !  and no production occured, then no QA POC flux goes out. This
       !  shortcut is present to avoid roundoff cancellation errors from
       !  the dust%rho * dust_flux_out - QA_dust_def computation.
       !  Any POC_PROD_avail still remaining goes into excess POC flux.
       !-----------------------------------------------------------------------

       if (POC%hflux_in(k) == c0 .and. POC%prod(k) == c0) then
          POC%hflux_out(k) = c0
       else
          POC%hflux_out(k) = P_CaCO3%rho * &
               (P_CaCO3%sflux_out(k) + P_CaCO3%hflux_out(k)) + &
               P_SiO2%rho * &
               (P_SiO2%sflux_out(k) + P_SiO2%hflux_out(k)) + &
               dust%rho * &
               (dust%sflux_out(k) + dust%hflux_out(k)) - &
               new_QA_dust_def
          POC%hflux_out(k) = max(POC%hflux_out(k), c0)
       endif

       POC%sflux_out(k) = POC%sflux_in(k) * decay_POC_E + &
            POC_PROD_avail *((c1 - decay_POC_E) * &
            poc_diss)

       !-----------------------------------------------------------------------
       !  Compute remineralization terms. It is assumed that there is no
       !  sub-surface dust production.
       !-----------------------------------------------------------------------

       P_CaCO3%remin(k) = P_CaCO3%prod(k) + &
            ((P_CaCO3%sflux_in(k) - P_CaCO3%sflux_out(k)) + &
            (P_CaCO3%hflux_in(k) - P_CaCO3%hflux_out(k))) * dzr_loc

       P_SiO2%remin(k) = P_SiO2%prod(k) + &
            ((P_SiO2%sflux_in(k) - P_SiO2%sflux_out(k)) + &
            (P_SiO2%hflux_in(k) - P_SiO2%hflux_out(k))) * dzr_loc

       POC%remin(k) = POC%prod(k) + &
            ((POC%sflux_in(k) - POC%sflux_out(k)) + &
            (POC%hflux_in(k) - POC%hflux_out(k))) * dzr_loc

       PON_remin = Q * POC%remin(k)

       POP_remin = Qp_zoo_pom * POC%remin(k)

       dust%remin(k) = &
            ((dust%sflux_in(k) - dust%sflux_out(k)) + &
            (dust%hflux_in(k) - dust%hflux_out(k))) * dzr_loc

       !-----------------------------------------------------------------------
       !  Compute iron remineralization and flux out.
       !-----------------------------------------------------------------------

       if (POC%sflux_in(k) + POC%hflux_in(k) == c0) then
          P_iron%remin(k) = (POC%remin(k) * parm_Red_Fe_C)
       else
          P_iron%remin(k) = (POC%remin(k) * &
               (P_iron%sflux_in(k) + P_iron%hflux_in(k)) / &
               (POC%sflux_in(k) + POC%hflux_in(k)))
       endif

       ! add term for desorption of iron from sinking particles
       P_iron%remin(k) = P_iron%remin(k) +                &
            (P_iron%sflux_in(k) * 1.5e-5_r8)

       P_iron%sflux_out(k) = P_iron%sflux_in(k) + dz_loc * &
            ((c1 - P_iron%gamma) * P_iron%prod(k) - P_iron%remin(k))

       if (P_iron%sflux_out(k) < c0) then
          P_iron%sflux_out(k) = c0
          P_iron%remin(k) = P_iron%sflux_in(k) * dzr_loc + &
               (c1 - P_iron%gamma) * P_iron%prod(k)
       endif

       !-----------------------------------------------------------------------
       !  Compute iron release from dust remin/dissolution
       !
       !  dust remin gDust = 0.035 / 55.847 * 1.0e9 = 626712.0 nmolFe
       !                      gFe     molFe     nmolFe
       !  Also add in Fe source from sediments if applicable to this cell.
       !-----------------------------------------------------------------------


       P_iron%remin(k) = P_iron%remin(k) &
            + dust%remin(k) * dust_to_Fe &
            + (fesedflux * dzr_loc)

       P_iron%hflux_out(k) = P_iron%hflux_in(k)

    else

       P_CaCO3%sflux_out(k) = c0
       P_CaCO3%hflux_out(k) = c0
       P_CaCO3%remin(k) = c0

       P_SiO2%sflux_out(k) = c0
       P_SiO2%hflux_out(k) = c0
       P_SiO2%remin(k) = c0

       dust%sflux_out(k) = c0
       dust%hflux_out(k) = c0
       dust%remin(k) = c0

       POC%sflux_out(k) = c0
       POC%hflux_out(k) = c0
       POC%remin(k) = c0

       PON_remin = c0

       POP_remin = c0

       P_iron%sflux_out(k) = c0
       P_iron%hflux_out(k) = c0
       P_iron%remin(k) = c0

    endif

    ! Save some fields for use by other modules before setting outgoing fluxes to 0.0 in bottom cell below
    if (lexport_shared_vars) then
       P_CaCO3_sflux_out_fields(k) = P_CaCO3%sflux_out(k)
       P_CaCO3_hflux_out_fields(k) = P_CaCO3%hflux_out(k)
       POC_sflux_out_fields(k)     = POC%sflux_out(k)
       POC_hflux_out_fields(k)     = POC%hflux_out(k)
       POC_remin_fields(k)         = POC%remin(k)
       P_CaCO3_remin_fields(k)     = P_CaCO3%remin(k)
       DECAY_Hard_fields(k)        = DECAY_Hard
    endif

    !-----------------------------------------------------------------------
    !  Bottom Sediments Cell?
    !  If so compute sedimentary burial and denitrification N losses.
    !  Using empirical relations from Bohlen et al., 2012 (doi:10.1029/2011GB004198) for Sed Denitrification
    !  other_remin estimates organic matter remineralized in the sediments
    !      by the processes other than oxic remin and denitrification (SO4 and CO2,
    !      etc..)
    !      based on Soetaert et al., 1996, varies between 10% and 50%
    !      0.4_r8 is a coefficient with units mmolC/cm2/yr sinking flux,
    !      other_remin is 50% above this high flux value,
    !      In special case where bottom O2 has been depleted to < 1.0 uM,
    !               all sedimentary remin is due to DENITRIFICATION + other_remin
    !  POC burial from Dunne et al. 2007 (doi:10.1029/2006GB002907), maximum of 80% burial efficiency imposed
    !  Bsi preservation in sediments based on
    !     Ragueneau et al. 2000 (doi:10.1016/S0921-8181(00)00052-7)
    !  Calcite is preserved in sediments above a threshold depth, 
    !     which is based on caco3_bury_thres_opt.
    !-----------------------------------------------------------------------

    POC%sed_loss(k)     = c0
    P_SiO2%sed_loss(k)  = c0
    P_CaCO3%sed_loss(k) = c0
    P_iron%sed_loss(k)  = c0
    dust%sed_loss(k)    = c0

    PON_sed_loss        = c0

    POP_sed_loss        = c0

    if ((k == column_kmt)) then

       flux = POC%sflux_out(k) + POC%hflux_out(k)

       if (flux > c0) then
          flux_alt = flux*mpercm*spd ! convert to mmol/m^2/day

          POC%sed_loss(k) = flux * min(0.8_r8, parm_POMbury &
               * (0.013_r8 + 0.53_r8 * flux_alt*flux_alt / (7.0_r8 + flux_alt)**2))

          PON_sed_loss = PON_bury_coeff * Q * POC%sed_loss(k)

          POP_sed_loss = POP_bury_coeff * Qp_zoo_pom * POC%sed_loss(k)

          sed_denitrif = dzr_loc * flux &
               * (0.06_r8 + 0.19_r8 * 0.99_r8**(O2_loc-NO3_loc))

          flux_alt = flux*1.0e-6_r8*spd*365.0_r8 ! convert to mmol/cm^2/year
          other_remin = dzr_loc &
               * min(min(0.1_r8 + flux_alt, 0.5_r8) * (flux - POC%sed_loss(k)), &
               (flux - POC%sed_loss(k) - (sed_denitrif*dz_loc*denitrif_C_N)))

          !----------------------------------------------------------------------------------
          !              if bottom water O2 is depleted, assume all remin is denitrif + other
          !----------------------------------------------------------------------------------

          if (O2_loc < c1) then
             other_remin = dzr_loc * &
                  (flux - POC%sed_loss(k) - (sed_denitrif*dz_loc*denitrif_C_N))
          endif

       endif

       flux = P_SiO2%sflux_out(k) + P_SiO2%hflux_out(k)
       flux_alt = flux*mpercm*spd ! convert to mmol/m^2/day
       ! first compute burial efficiency, then compute loss to sediments
       if (flux_alt > c2) then
          P_SiO2%sed_loss(k) = 0.2_r8
       else
          P_SiO2%sed_loss(k) = 0.04_r8
       endif
       P_SiO2%sed_loss(k) = flux * parm_BSIbury * P_SiO2%sed_loss(k)

       if (caco3_bury_thres_iopt == caco3_bury_thres_iopt_fixed_depth) then
          if (zw(k) < caco3_bury_thres_depth) then
             P_CaCO3%sed_loss(k) = P_CaCO3%sflux_out(k) + P_CaCO3%hflux_out(k)
          endif
       else ! caco3_bury_thres_iopt = caco3_bury_thres_iopt_omega_calc
          if (carbonate%CO3 > carbonate%CO3_sat_calcite) then
             P_CaCO3%sed_loss(k) = P_CaCO3%sflux_out(k) + P_CaCO3%hflux_out(k)
          endif
       endif

       !----------------------------------------------------------------------------------
       !  Update sinking fluxes and remin fluxes, accounting for sediments.
       !  flux used to hold sinking fluxes before update.
       !----------------------------------------------------------------------------------

       flux = P_CaCO3%sflux_out(k) + P_CaCO3%hflux_out(k)
       if (flux > c0) then
          P_CaCO3%remin(k) = P_CaCO3%remin(k) &
               + ((flux - P_CaCO3%sed_loss(k)) * dzr_loc)
       endif

       flux = P_SiO2%sflux_out(k) + P_SiO2%hflux_out(k)
       if (flux > c0) then
          P_SiO2%remin(k) = P_SiO2%remin(k) &
               + ((flux - P_SiO2%sed_loss(k)) * dzr_loc)
       endif

       flux = POC%sflux_out(k) + POC%hflux_out(k)
       if (flux > c0) then
          POC%remin(k) = POC%remin(k) &
               + ((flux - POC%sed_loss(k)) * dzr_loc)

          PON_remin = PON_remin &
               + ((Q * flux - PON_sed_loss) * dzr_loc)

          POP_remin = POP_remin &
               + ((Qp_zoo_pom * flux - POP_sed_loss) * dzr_loc)
       endif

       !-----------------------------------------------------------------------
       !   Remove all Piron and dust that hits bottom, sedimentary Fe source
       !        accounted for by fesedflux elsewhere.
       !-----------------------------------------------------------------------

       flux = (P_iron%sflux_out(k) + P_iron%hflux_out(k))
       if (flux > c0) then
          P_iron%sed_loss(k) = flux
       endif

       dust%sed_loss(k) = dust%sflux_out(k) + dust%hflux_out(k)

       !-----------------------------------------------------------------------
       !   Set all outgoing fluxes to 0.0
       !-----------------------------------------------------------------------

       if (k == column_kmt) then
          P_CaCO3%sflux_out(k) = c0
          P_CaCO3%hflux_out(k) = c0

          P_SiO2%sflux_out(k) = c0
          P_SiO2%hflux_out(k) = c0

          dust%sflux_out(k) = c0
          dust%hflux_out(k) = c0

          POC%sflux_out(k) = c0
          POC%hflux_out(k) = c0

          P_iron%sflux_out(k) = c0
          P_iron%hflux_out(k) = c0
       endif

    endif

    if (poc_error) then
      write(log_message, "(A)") "mass ratio of ballast production exceeds POC production"
      call marbl_status_log%log_error(log_message, subname)
    endif

    end associate

  end subroutine marbl_compute_particulate_terms

  !***********************************************************************

  subroutine marbl_set_surface_forcing( &
       ciso_on,                         &
       num_elements,                    &
       surface_forcing_ind,             &
       surface_input_forcings,          &
       surface_vals,                    &
       surface_tracer_fluxes,           &
       marbl_tracer_indices,            &
       saved_state,                     &
       surface_forcing_output,          &
       surface_forcing_internal,        &
       surface_forcing_share,           &
       surface_forcing_diags,           &
       marbl_status_log)

    !  Compute surface forcing fluxes 

    use marbl_interface_types , only : sfo_ind
    use marbl_schmidt_number_mod , only : schmidt_co2_surf  
    use marbl_oxygen             , only : schmidt_o2_surf
    use marbl_co2calc_mod        , only : marbl_co2calc_surf
    use marbl_co2calc_mod        , only : thermodynamic_coefficients_type
    use marbl_oxygen             , only : o2sat_surf
    use marbl_parms              , only : molw_Fe
    use marbl_share_mod          , only : lflux_gas_o2
    use marbl_share_mod          , only : lflux_gas_co2
    use marbl_share_mod          , only : ndep_data_type
    use marbl_share_mod          , only : gas_flux_forcing_iopt_drv
    use marbl_share_mod          , only : gas_flux_forcing_iopt_file
    use marbl_share_mod          , only : gas_flux_forcing_iopt
    use marbl_share_mod          , only : fice_file        
    use marbl_share_mod          , only : xkw_file         
    use marbl_share_mod          , only : ap_file          
    use marbl_share_mod          , only : iron_frac_in_dust
    use marbl_share_mod          , only : iron_frac_in_bc
    use marbl_share_mod          , only : dust_flux_file       
    use marbl_share_mod          , only : iron_flux_file          
    use marbl_share_mod          , only : din_riv_flux_file      
    use marbl_share_mod          , only : dip_riv_flux_file    
    use marbl_share_mod          , only : don_riv_flux_file    
    use marbl_share_mod          , only : dop_riv_flux_file    
    use marbl_share_mod          , only : dsi_riv_flux_file    
    use marbl_share_mod          , only : dfe_riv_flux_file    
    use marbl_share_mod          , only : dic_riv_flux_file    
    use marbl_share_mod          , only : alk_riv_flux_file    
    use marbl_share_mod          , only : doc_riv_flux_file    
    use marbl_sizes              , only : marbl_total_tracer_cnt
    use marbl_ciso_mod           , only : marbl_ciso_set_surface_forcing

    implicit none

    integer (int_kind)                        , intent(in)    :: num_elements
    logical (log_kind)                        , intent(in)    :: ciso_on ! flag to save shared_vars or not
    type(marbl_surface_forcing_indexing_type) , intent(in)    :: surface_forcing_ind         
    real(r8)                                  , intent(in)    :: surface_input_forcings(:,:)
    real (r8)                                 , intent(in)    :: surface_vals(:,:)            
    real (r8)                                 , intent(out)   :: surface_tracer_fluxes(:,:)
    type(marbl_tracer_index_type)             , intent(in)    :: marbl_tracer_indices
    type(marbl_saved_state_type)              , intent(inout) :: saved_state
    type(marbl_surface_forcing_internal_type) , intent(inout) :: surface_forcing_internal
    type(marbl_surface_forcing_output_type)   , intent(inout) :: surface_forcing_output
    type(marbl_surface_forcing_share_type)    , intent(inout) :: surface_forcing_share
    type(marbl_diagnostics_type)              , intent(inout) :: surface_forcing_diags
    type(marbl_log_type)                      , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_mod:marbl_set_surface_forcing'
    integer (int_kind)      :: n                        ! loop indices
    integer (int_kind)      :: auto_ind                 ! autotroph functional group index
    real (r8)               :: phlo(num_elements)       ! lower bound for ph in solver
    real (r8)               :: phhi(num_elements)       ! upper bound for ph in solver
    real (r8)               :: ph_new(num_elements)     ! computed ph from solver
    real (r8)               :: xkw_ice(num_elements)    ! common portion of piston vel., (1-fice)*xkw (cm/s)
    real (r8)               :: o2sat_1atm(num_elements) ! o2 saturation @ 1 atm (mmol/m^3)
    real (r8)               :: totalChl_loc(num_elements)  ! local value of totalChl
    real (r8)               :: flux_co2_loc(num_elements)  ! local value of co2 flux
    real (r8)               :: flux_o2_loc(num_elements)   ! local value of o2 flux
    logical (log_kind)      :: mask(num_elements)
    type(thermodynamic_coefficients_type), dimension(num_elements) :: co3_coeffs
    !-----------------------------------------------------------------------

    associate(                                                                                      &
         ind                  => surface_forcing_ind,                                               &
  
         surface_mask         => surface_input_forcings(:,surface_forcing_ind%surface_mask_id),     &
         ifrac                => surface_input_forcings(:,surface_forcing_ind%ifrac_id),            &
         sst                  => surface_input_forcings(:,surface_forcing_ind%sst_id),              &
         sss                  => surface_input_forcings(:,surface_forcing_ind%sss_id),              &
         xco2                 => surface_input_forcings(:,surface_forcing_ind%xco2_id),             &
         xco2_alt_co2         => surface_input_forcings(:,surface_forcing_ind%xco2_alt_co2_id),     & 
         ap_used              => surface_input_forcings(:,surface_forcing_ind%atm_pressure_id),     &
         xkw                  => surface_input_forcings(:,surface_forcing_ind%xkw_id),              &
         dust_flux_in         => surface_input_forcings(:,surface_forcing_ind%dust_flux_id),        &
         iron_flux_in         => surface_input_forcings(:,surface_forcing_ind%iron_flux_id),        &
         black_carbon_flux_in => surface_input_forcings(:,surface_forcing_ind%black_carbon_flux_id),&
         nox_flux             => surface_input_forcings(:,surface_forcing_ind%nox_flux_id),         &
         nhy_flux             => surface_input_forcings(:,surface_forcing_ind%nhy_flux_id),         &
         din_riv_flux         => surface_input_forcings(:,surface_forcing_ind%din_riv_flux_id),     & 
         dip_riv_flux         => surface_input_forcings(:,surface_forcing_ind%dip_riv_flux_id),     & 
         don_riv_flux         => surface_input_forcings(:,surface_forcing_ind%don_riv_flux_id),     & 
         dop_riv_flux         => surface_input_forcings(:,surface_forcing_ind%dop_riv_flux_id),     & 
         dsi_riv_flux         => surface_input_forcings(:,surface_forcing_ind%dsi_riv_flux_id),     & 
         dfe_riv_flux         => surface_input_forcings(:,surface_forcing_ind%dfe_riv_flux_id),     & 
         dic_riv_flux         => surface_input_forcings(:,surface_forcing_ind%dic_riv_flux_id),     & 
         doc_riv_flux         => surface_input_forcings(:,surface_forcing_ind%doc_riv_flux_id),     & 
         alk_riv_flux         => surface_input_forcings(:,surface_forcing_ind%alk_riv_flux_id),     & 

         iron_flux_in_new     => surface_forcing_internal%iron_flux(:),                             &
         co2star              => surface_forcing_internal%co2star(:),                               & 
         dco2star             => surface_forcing_internal%dco2star(:),                              & 
         pco2surf             => surface_forcing_internal%pco2surf(:),                              & 
         dpco2                => surface_forcing_internal%dpco2(:),                                 & 
         co3                  => surface_forcing_internal%co3(:),                                   & 
         co2star_alt          => surface_forcing_internal%co2star_alt(:),                           & 
         dco2star_alt         => surface_forcing_internal%dco2star_alt(:),                          & 
         pco2surf_alt         => surface_forcing_internal%pco2surf_alt(:),                          & 
         dpco2_alt            => surface_forcing_internal%dpco2_alt(:),                             & 
         schmidt_co2          => surface_forcing_internal%schmidt_co2(:),                           & 
         schmidt_o2           => surface_forcing_internal%schmidt_o2(:),                            & 
         pv_o2                => surface_forcing_internal%pv_o2(:),                                 & 
         pv_co2               => surface_forcing_internal%pv_co2(:),                                & 
         o2sat                => surface_forcing_internal%o2sat(:),                                 & 
         flux_alt_co2         => surface_forcing_internal%flux_alt_co2(:),                          & 

         stf                  => surface_tracer_fluxes(:,:),                                        &

         ph_prev_surf         => saved_state%ph_prev_surf,                                          &
         ph_prev_alt_co2_surf => saved_state%ph_prev_alt_co2_surf,                                  &

         po4_ind           => marbl_tracer_indices%po4_ind,                                     &
         no3_ind           => marbl_tracer_indices%no3_ind,                                     &
         sio3_ind          => marbl_tracer_indices%sio3_ind,                                     &
         nh4_ind           => marbl_tracer_indices%nh4_ind,                                     &
         fe_ind            => marbl_tracer_indices%fe_ind,                                      &
         o2_ind            => marbl_tracer_indices%o2_ind,                                      &
         dic_ind           => marbl_tracer_indices%dic_ind,                                     &
         dic_alt_co2_ind   => marbl_tracer_indices%dic_alt_co2_ind,                             &
         alk_ind           => marbl_tracer_indices%alk_ind,                                     &
         doc_ind           => marbl_tracer_indices%doc_ind,                                     &
         don_ind           => marbl_tracer_indices%don_ind,                                     &
         dop_ind           => marbl_tracer_indices%dop_ind,                                     &
         dopr_ind          => marbl_tracer_indices%dopr_ind,                                    &
         donr_ind          => marbl_tracer_indices%donr_ind,                                    &
         docr_ind          => marbl_tracer_indices%docr_ind ,                                   &

         pv_surf_fields       => surface_forcing_share%pv_surf_fields(:),                           & ! out
         dic_surf_fields      => surface_forcing_share%dic_surf_fields(:),                          & ! out
         co2star_surf_fields  => surface_forcing_share%co2star_surf_fields(:),                      & ! out
         dco2star_surf_fields => surface_forcing_share%dco2star_surf_fields(:),                     & ! out
         co3_surf_fields      => surface_forcing_share%co3_surf_fields(:),                          & ! out
         dic_riv_flux_fields  => surface_forcing_share%dic_riv_flux_fields(:),                      & ! out
         doc_riv_flux_fields  => surface_forcing_share%doc_riv_flux_fields(:)                       & ! out
         )

    !-----------------------------------------------------------------------
    !  fluxes initially set to 0
    !-----------------------------------------------------------------------

    stf(:, :) = c0

    !-----------------------------------------------------------------------
    !  Compute total chlorophyll
    !-----------------------------------------------------------------------

    if (sfo_ind%totalChl_id.ne.0) then
      totalChl_loc = c0
      do auto_ind = 1,size(autotrophs)
        totalChl_loc = totalChl_loc +                                         &
          max(c0, surface_vals(:,marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind))
      end do
      surface_forcing_output%sfo(sfo_ind%totalChl_id)%forcing_field = totalChl_loc
    end if

    !-----------------------------------------------------------------------
    !  calculate gas flux quantities if necessary
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  compute CO2 flux, computing disequilibrium one row at a time
    !-----------------------------------------------------------------------
       
    if (lflux_gas_o2 .or. lflux_gas_co2) then

       !-----------------------------------------------------------------------
       !  Compute XKW_ICE. XKW is zero over land, so XKW_ICE is too.
       !-----------------------------------------------------------------------

       xkw_ice(:) = (c1 - ifrac(:)) * xkw

       !-----------------------------------------------------------------------
       !  compute O2 flux
       !-----------------------------------------------------------------------

       if (lflux_gas_o2) then
          schmidt_o2(:) = schmidt_o2_surf(num_elements, sst, surface_mask)

          o2sat_1atm(:) = o2sat_surf(num_elements, sst, sss, surface_mask)

          where (surface_mask(:) /= c0) 
             pv_o2(:) = xkw_ice(:) * sqrt(660.0_r8 / schmidt_o2(:))
             o2sat(:) = ap_used(:) * o2sat_1atm(:)
             flux_o2_loc(:) = pv_o2(:) * (o2sat(:) - surface_vals(:, o2_ind))
             stf(:, o2_ind) = stf(:, o2_ind) + flux_o2_loc(:)
          elsewhere
             pv_o2(:) = c0
             o2sat(:) = c0
             flux_o2_loc = c0
          end where
          if (sfo_ind%flux_o2_id.ne.0) then
            surface_forcing_output%sfo(sfo_ind%flux_o2_id)%forcing_field = flux_o2_loc
          end if
       else
          schmidt_o2(:) = c0
          pv_o2(:)      = c0
          o2sat(:)      = c0
       endif  ! lflux_gas_o2

       !-----------------------------------------------------------------------
       !  compute CO2 flux, computing disequilibrium 
       !-----------------------------------------------------------------------

       if (lflux_gas_co2) then

          schmidt_co2(:) = schmidt_co2_surf(num_elements, sst, surface_mask)

          where (surface_mask(:) /= c0) 
             pv_co2(:) = xkw_ice(:) * sqrt(660.0_r8 / schmidt_co2(:))
          elsewhere
             pv_co2(:) = c0
          end where

          !-----------------------------------------------------------------------
          !  Set FLUX_CO2
          !-----------------------------------------------------------------------

          where (ph_prev_surf(:) /= c0)
             phlo(:) = ph_prev_surf(:) - del_ph
             phhi(:) = ph_prev_surf(:) + del_ph
          elsewhere
             phlo(:) = phlo_surf_init
             phhi(:) = phhi_surf_init
          end where

          where (surface_mask(:) /= c0) 
             mask(:) = .true.
          elsewhere
             mask(:) = .false.
          end where

          ! Note the following computes a new ph_prev_surf
          call marbl_co2calc_surf(                                         &
               num_elements     = num_elements,                            &
               lcomp_co3_coeffs = .true.,                                  &
               mask       = mask,                                          &
               dic_in     = surface_vals(:,dic_ind),                       & 
               xco2_in    = surface_input_forcings(:,ind%xco2_id),         &
               ta_in      = surface_vals(:,alk_ind),                       & 
               pt_in      = surface_vals(:,po4_ind),                       & 
               sit_in     = surface_vals(:,sio3_ind),                      &
               temp       = surface_input_forcings(:,ind%sst_id),          &
               salt       = surface_input_forcings(:,ind%sss_id),          &
               atmpres    = surface_input_forcings(:,ind%atm_pressure_id), &
               co3_coeffs = co3_coeffs,                                    &
               co3        = co3,                                           &
               co2star    = surface_forcing_internal%co2star,              &
               dco2star   = surface_forcing_internal%dco2star,             &
               pco2surf   = surface_forcing_internal%pco2surf,             &
               dpco2      = surface_forcing_internal%dpco2,                &
               phlo       = phlo,                                          &
               phhi       = phhi,                                          &
               ph         = ph_prev_surf,                                  &
               marbl_status_log = marbl_status_log)

          if (marbl_status_log%labort_marbl) then
             call marbl_status_log%log_error_trace('co2calc_surf()', subname)
             return
          end if

          flux_co2_loc(:) = pv_co2(:) * dco2star(:)
          if (sfo_ind%flux_co2_id.ne.0) then
            surface_forcing_output%sfo(sfo_ind%flux_co2_id)%forcing_field = flux_co2_loc
          end if
 
          !-------------------------------------------------------------------
          !  The following variables need to be shared with other modules,
          !  and are now defined in marbl_share as targets.
          !-------------------------------------------------------------------

          if (ciso_on) then
             pv_surf_fields(:)       = pv_co2(:)
             dic_surf_fields(:)      = surface_vals(:,dic_ind)
             co2star_surf_fields(:)  = co2star(:)
             dco2star_surf_fields(:) = dco2star(:)
             co3_surf_fields(:)      = co3(:)
          endif

          !-----------------------------------------------------------------------
          !  Set flux_alt_co2
          !-----------------------------------------------------------------------

          where (ph_prev_alt_co2_surf(:) /= c0)
             phlo(:) = ph_prev_alt_co2_surf(:) - del_ph
             phhi(:) = ph_prev_alt_co2_surf(:) + del_ph
          elsewhere
             phlo(:) = phlo_surf_init
             phhi(:) = phhi_surf_init
          end where

          ! Note the following computes a new ph_prev_alt_co2
          call marbl_co2calc_surf(                                         &
               num_elements     = num_elements,                            &
               lcomp_co3_coeffs = .false.,                                 &
               mask       = mask,                                          &
               dic_in     = surface_vals(:,dic_alt_co2_ind),               &
               xco2_in    = surface_input_forcings(:,ind%xco2_alt_co2_id), &
               ta_in      = surface_vals(:,alk_ind),                       & 
               pt_in      = surface_vals(:,po4_ind),                       & 
               sit_in     = surface_vals(:,sio3_ind),                      &
               temp       = surface_input_forcings(:,ind%sst_id),          &
               salt       = surface_input_forcings(:,ind%sss_id),          &
               atmpres    = surface_input_forcings(:,ind%atm_pressure_id), &
               co3_coeffs = co3_coeffs,                                    &
               co3        = co3,                                           &
               co2star    = surface_forcing_internal%co2star_alt,          &
               dco2star   = surface_forcing_internal%dco2star_alt,         &
               pco2surf   = surface_forcing_internal%pco2surf_alt,         &
               dpco2      = surface_forcing_internal%dpco2_alt,            &
               phlo       = phlo,                                          &
               phhi       = phhi,                                          &
               ph         = ph_prev_alt_co2_surf,                          &
               marbl_status_log = marbl_status_log)

            if (marbl_status_log%labort_marbl) then
               call marbl_status_log%log_error_trace('co2calc_surf()', subname)
               return
            end if

          flux_alt_co2(:) = pv_co2(:) * dco2star_alt(:)

          !-----------------------------------------------------------------------
          !  set air-sea co2 gas flux named field, converting units from
          !  nmol/cm^2/s (positive down) to kg CO2/m^2/s (positive down)
          !-----------------------------------------------------------------------

          stf(:, dic_ind)         = stf(:, dic_ind)         + flux_co2_loc(:)
          stf(:, dic_alt_co2_ind) = stf(:, dic_alt_co2_ind) + FLUX_ALT_CO2(:)

       else
          schmidt_co2(:) = c0
          pv_co2(:)      = c0
       endif  !  lflux_gas_co2

    endif  ! lflux_gas_o2 .or. lflux_gas_co2

    !-----------------------------------------------------------------------
    !  calculate iron and dust fluxes if necessary
    !-----------------------------------------------------------------------

    if (liron_flux_derived) then
       ! compute iron_flux gFe/cm^2/s, then convert to nmolFe/cm^2/s
       iron_flux_in_new(:) = dust_flux_in(:) * iron_frac_in_dust + black_carbon_flux_in(:) * iron_frac_in_bc
       iron_flux_in_new(:) = (1.0e9_r8 / molw_Fe) * iron_flux_in_new(:)
    else
       iron_flux_in_new(:) = iron_flux_in(:) * parm_Fe_bioavail  ! TODO: this gets moved up and out - a forcing field modify
    endif

    stf(:, fe_ind) = stf(:, fe_ind) + iron_flux_in_new(:)

    !-----------------------------------------------------------------------
    !  Add phosphate and silicate from dust after Krishnamurthy et al. (2010)
    !  factors convert from g/cm2/s to nmol/cm2/s
    !  ( P frac in dust by weight) * ( P solubility) / ( P molecular weight) * (mol->nmol)
    !  (Si frac in dust by weight) * (Si solubility) / (Si molecular weight) * (mol->nmol)
    !-----------------------------------------------------------------------

    stf(:, po4_ind) = stf(:, po4_ind)   + (dust_flux_in * (0.00105_r8 *  0.15_r8 / 30.974_r8 * 1.0e9_r8))

    stf(:, sio3_ind) = stf(:, sio3_ind) + (dust_flux_in * (  0.308_r8 * 0.075_r8 / 28.085_r8 * 1.0e9_r8))

    !-----------------------------------------------------------------------
    !  calculate nox and nhy fluxes if necessary
    !-----------------------------------------------------------------------
       
    if (surface_forcing_ind%nox_flux_id.ne.0) then
       where (surface_mask(:).ne.c0)
         stf(:, no3_ind) = stf(:, no3_ind) + nox_flux(:)
       end where
    endif
       
    if (surface_forcing_ind%nhy_flux_id.ne.0) then
       where (surface_mask(:).ne.c0)
         stf(:, nh4_ind) = stf(:, nh4_ind) + nhy_flux(:)
       end where
    endif
       
    !-----------------------------------------------------------------------
    !  calculate river bgc fluxes if necessary
    !-----------------------------------------------------------------------

    if (din_riv_flux_file%has_data) then
       stf(:, no3_ind) = stf(:, no3_ind) + din_riv_flux(:)
    endif

    if (dip_riv_flux_file%has_data) then
       stf(:, po4_ind) = stf(:, po4_ind) + dip_riv_flux(:)
    endif

    if (don_riv_flux_file%has_data) then
       stf(:, don_ind)  = stf(:, don_ind)  +  don_riv_flux(:) * (c1 - DONriv_refract)
       stf(:, donr_ind) = stf(:, donr_ind) +  don_riv_flux(:) * DONriv_refract
    endif

    if (dop_riv_flux_file%has_data) then
       stf(:, dop_ind)  = stf(:, dop_ind)  + dop_riv_flux(:) * (c1 - DOPriv_refract)
       stf(:, dopr_ind) = stf(:, dopr_ind) + dop_riv_flux(:) * DOPriv_refract
    endif

    if (dsi_riv_flux_file%has_data) then
       stf(:, sio3_ind) = stf(:, sio3_ind) + dsi_riv_flux(:)
    endif

    if (dfe_riv_flux_file%has_data) then
       stf(:, fe_ind) = stf(:, fe_ind) + dfe_riv_flux(:)
    endif

    if (dic_riv_flux_file%has_data) then
       stf(:, dic_ind)         = stf(:, dic_ind)         + dic_riv_flux(:)
       stf(:, dic_alt_co2_ind) = stf(:, dic_alt_co2_ind) + dic_riv_flux(:)
       if (ciso_on) then
          dic_riv_flux_fields = dic_riv_flux(:)
       end if
    endif

    if (alk_riv_flux_file%has_data) then
       stf(:, alk_ind) = stf(:, alk_ind) + alk_riv_flux(:)
    endif

    if (doc_riv_flux_file%has_data) then
       stf(:, doc_ind)  = stf(:, doc_ind)  + doc_riv_flux(:) * (c1 - DOCriv_refract)
       stf(:, docr_ind) = stf(:, docr_ind) + doc_riv_flux(:) * DOCriv_refract

       ! FIXME #29: sending total doc river input to ciso
       !            for now, need to separate doc and docr
       if (ciso_on) then
          doc_riv_flux_fields = doc_riv_flux(:)
       end if
    endif

    !-----------------------------------------------------------------------
    !  Apply NO & NH fluxes to alkalinity
    !-----------------------------------------------------------------------

    stf(:, alk_ind) = stf(:, alk_ind) + stf(:, nh4_ind) - stf(:, no3_ind)

    !-----------------------------------------------------------------------
    ! Set surface forcing diagnostics
    !-----------------------------------------------------------------------

    call marbl_diagnostics_set_surface_forcing(               &
         surface_forcing_ind      = ind,                      &
         surface_input_forcings   = surface_input_forcings,   &
         surface_forcing_internal = surface_forcing_internal, &
         surface_tracer_fluxes    = stf,                      &
         marbl_tracer_indices     = marbl_tracer_indices,     &
         saved_state              = saved_state,              & 
         surface_forcing_output   = surface_forcing_output,   &
         surface_forcing_diags    = surface_forcing_diags)

    !-----------------------------------------------------------------------
    ! Compute carbon isotopes surface fluxes
    !-----------------------------------------------------------------------

    if (ciso_on) then
       call marbl_ciso_set_surface_forcing(                                              &
            num_elements                = num_elements,                                  &
            surface_mask                = surface_input_forcings(:,ind%surface_mask_id), &
            sst                         = surface_input_forcings(:,ind%sst_id),          &
            d13c                        = surface_input_forcings(:,ind%d13c_id),         &
            d14c                        = surface_input_forcings(:,ind%d14c_id),         &
            d14c_glo_avg                = surface_input_forcings(:,ind%d14c_glo_avg_id), &
            surface_vals                = surface_vals,                                  &
            stf                         = surface_tracer_fluxes,                         &
            marbl_tracer_indices        = marbl_tracer_indices,                          &
            marbl_surface_forcing_share = surface_forcing_share,                         &
            marbl_surface_forcing_diags = surface_forcing_diags)
    end if

    end associate

  end subroutine marbl_set_surface_forcing

  !***********************************************************************

  subroutine marbl_init_surface_forcing_metadata()

    !-----------------------------------------------------------------------
    ! initialize surface forcing metadata
    !-----------------------------------------------------------------------

    use marbl_share_mod , only : fice_file        
    use marbl_share_mod , only : xkw_file         
    use marbl_share_mod , only : ap_file          
    use marbl_share_mod , only : dust_flux_file          
    use marbl_share_mod , only : iron_flux_file          
    use marbl_share_mod , only : nox_flux_monthly_file  
    use marbl_share_mod , only : nhy_flux_monthly_file  
    use marbl_share_mod , only : din_riv_flux_file       
    use marbl_share_mod , only : dip_riv_flux_file     
    use marbl_share_mod , only : don_riv_flux_file     
    use marbl_share_mod , only : dop_riv_flux_file     
    use marbl_share_mod , only : dsi_riv_flux_file     
    use marbl_share_mod , only : dfe_riv_flux_file     
    use marbl_share_mod , only : dic_riv_flux_file     
    use marbl_share_mod , only : alk_riv_flux_file     
    use marbl_share_mod , only : doc_riv_flux_file     

    implicit none

    call marbl_init_monthly_surface_forcing_metadata(fice_file)
    call marbl_init_monthly_surface_forcing_metadata(xkw_file)
    call marbl_init_monthly_surface_forcing_metadata(ap_file)
    call marbl_init_monthly_surface_forcing_metadata(dust_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(iron_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(nox_flux_monthly_file)
    call marbl_init_monthly_surface_forcing_metadata(nhy_flux_monthly_file)
    call marbl_init_monthly_surface_forcing_metadata(din_riv_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(dip_riv_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(don_riv_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(dop_riv_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(dsi_riv_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(dfe_riv_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(dic_riv_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(alk_riv_flux_file)
    call marbl_init_monthly_surface_forcing_metadata(doc_riv_flux_file)

  end subroutine marbl_init_surface_forcing_metadata

  !*****************************************************************************

  subroutine marbl_init_monthly_surface_forcing_metadata(var)

    implicit none

    type(marbl_forcing_monthly_every_ts_type), intent(out) :: var

    var%interp_type = 'linear'
    var%data_type   = 'monthly-calendar'
    var%interp_freq = 'every-timestep'
    var%filename    = 'not-used-for-monthly'
    var%data_label  = 'not-used-for-monthly'

  end subroutine marbl_init_monthly_surface_forcing_metadata

  !***********************************************************************

  subroutine marbl_init_non_autotroph_tracer_metadata(marbl_tracer_metadata, &
             marbl_tracer_indices, non_living_biomass_ecosys_tracer_cnt)

    !-----------------------------------------------------------------------
    !  initialize non-autotroph tracer_d values and accumulate
    !  non_living_biomass_ecosys_tracer_cnt
    !-----------------------------------------------------------------------

    implicit none

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)             ! descriptors for each tracer
    type (marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices
    integer (int_kind)                , intent(out)   :: non_living_biomass_ecosys_tracer_cnt ! number of non-autotroph ecosystem tracers

    integer(int_kind) :: n

    associate(&
         po4_ind           => marbl_tracer_indices%po4_ind,         &
         no3_ind           => marbl_tracer_indices%no3_ind,         &
         sio3_ind          => marbl_tracer_indices%sio3_ind,        &
         nh4_ind           => marbl_tracer_indices%nh4_ind,         &
         fe_ind            => marbl_tracer_indices%fe_ind,          &
         o2_ind            => marbl_tracer_indices%o2_ind,          &
         dic_ind           => marbl_tracer_indices%dic_ind,         &
         dic_alt_co2_ind   => marbl_tracer_indices%dic_alt_co2_ind, &
         alk_ind           => marbl_tracer_indices%alk_ind,         &
         doc_ind           => marbl_tracer_indices%doc_ind,         &
         don_ind           => marbl_tracer_indices%don_ind,         &
         dop_ind           => marbl_tracer_indices%dop_ind,         &
         dopr_ind          => marbl_tracer_indices%dopr_ind,        &
         donr_ind          => marbl_tracer_indices%donr_ind,        &
         docr_ind          => marbl_tracer_indices%docr_ind         &
             )

    non_living_biomass_ecosys_tracer_cnt = 0

    marbl_tracer_metadata(po4_ind)%short_name='PO4'
    marbl_tracer_metadata(po4_ind)%long_name='Dissolved Inorganic Phosphate'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(no3_ind)%short_name='NO3'
    marbl_tracer_metadata(no3_ind)%long_name='Dissolved Inorganic Nitrate'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(sio3_ind)%short_name='SiO3'
    marbl_tracer_metadata(sio3_ind)%long_name='Dissolved Inorganic Silicate'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(nh4_ind)%short_name='NH4'
    marbl_tracer_metadata(nh4_ind)%long_name='Dissolved Ammonia'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(fe_ind)%short_name='Fe'
    marbl_tracer_metadata(fe_ind)%long_name='Dissolved Inorganic Iron'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(o2_ind)%short_name='O2'
    marbl_tracer_metadata(o2_ind)%long_name='Dissolved Oxygen'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(dic_ind)%short_name='DIC'
    marbl_tracer_metadata(dic_ind)%long_name='Dissolved Inorganic Carbon'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(dic_alt_co2_ind)%short_name='DIC_ALT_CO2'
    marbl_tracer_metadata(dic_alt_co2_ind)%long_name='Dissolved Inorganic Carbon, Alternative CO2'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(alk_ind)%short_name='ALK'
    marbl_tracer_metadata(alk_ind)%long_name='Alkalinity'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(doc_ind)%short_name='DOC'
    marbl_tracer_metadata(doc_ind)%long_name='Dissolved Organic Carbon'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(don_ind)%short_name='DON'
    marbl_tracer_metadata(don_ind)%long_name='Dissolved Organic Nitrogen'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(dop_ind)%short_name='DOP'
    marbl_tracer_metadata(dop_ind)%long_name='Dissolved Organic Phosphorus'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(dopr_ind)%short_name='DOPr'
    marbl_tracer_metadata(dopr_ind)%long_name='Refractory DOP'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(donr_ind)%short_name='DONr'
    marbl_tracer_metadata(donr_ind)%long_name='Refractory DON'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

    marbl_tracer_metadata(docr_ind)%short_name='DOCr'
    marbl_tracer_metadata(docr_ind)%long_name='Refractory DOC'
    non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

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

  !***********************************************************************

  subroutine marbl_check_ecosys_tracer_count_consistency(non_living_biomass_ecosys_tracer_cnt, marbl_status_log)

    implicit none

    integer (int_kind), intent(in) :: &
         non_living_biomass_ecosys_tracer_cnt ! number of non-autotroph ecosystem tracers
    type(marbl_log_type)           , intent(inout) :: marbl_status_log

    integer (int_kind) :: &
         n,               &
         auto_ind,        & ! autotroph functional group index
         zoo_ind            ! zooplankton functional group index

    character(*), parameter :: subname = 'marbl_mod:check_ecosys_tracer_count_consistency'
    character(len=char_len) :: log_message

    !-----------------------------------------------------------------------
    !  confirm that ecosys_base_tracer_cnt is consistent with autotroph declarations
    !-----------------------------------------------------------------------
    n = non_living_biomass_ecosys_tracer_cnt
    ! Do we really need a loop here, or would simple addition work?!
    do zoo_ind = 1, zooplankton_cnt
       n = n + 1 ! C
    end do

    do auto_ind = 1, autotroph_cnt
       n = n + 3 ! Chl, C, Fe tracers
       if (autotrophs(auto_ind)%kSiO3 > c0) n = n + 1 ! Si tracer
       if (autotrophs(auto_ind)%imp_calcifier .or. &
            autotrophs(auto_ind)%exp_calcifier) n = n + 1 ! CaCO3 tracer
    end do

    if (ecosys_base_tracer_cnt /= n) then
       write(log_message, "(A,I0,A,I0)")                                      &
                                 "ecosys_base_tracer_cnt = ",                 &
                                 ecosys_base_tracer_cnt,                      &
                                "but computed ecosys_base_tracer_cnt = ", n
       call marbl_status_log%log_error(log_message, subname)
        return
    endif

  end subroutine marbl_check_ecosys_tracer_count_consistency

  !***********************************************************************

  subroutine marbl_init_zooplankton_tracer_metadata(marbl_tracer_metadata, &
             marbl_tracer_indices)

    !-----------------------------------------------------------------------
    !  initialize zooplankton tracer_d values and tracer indices
    !-----------------------------------------------------------------------

    implicit none

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)             ! descriptors for each tracer
    type (marbl_tracer_index_type)    , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n, zoo_ind            ! zooplankton functional group index
    !-----------------------------------------------------------------------

    do zoo_ind = 1, zooplankton_cnt
       n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
       marbl_tracer_metadata(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'C'
       marbl_tracer_metadata(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Carbon'
       marbl_tracer_metadata(n)%units      = 'mmol/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
    end do

  end subroutine marbl_init_zooplankton_tracer_metadata

  !***********************************************************************

  subroutine marbl_init_autotroph_tracer_metadata(marbl_tracer_metadata,      &
             marbl_tracer_indices)

    !-----------------------------------------------------------------------
    !  initialize autotroph tracer_d values and tracer indices
    !-----------------------------------------------------------------------

    implicit none

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type    (marbl_tracer_index_type) , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n, auto_ind
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
       marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Chl'
       marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Chlorophyll'
       marbl_tracer_metadata(n)%units      = 'mg/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mg/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mg/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
       marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'C'
       marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Carbon'
       marbl_tracer_metadata(n)%units      = 'mmol/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
       marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe'
       marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron'
       marbl_tracer_metadata(n)%units      = 'mmol/m^3'
       marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
       marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'

       n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
       if (n.gt.0) then
          marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Si'
          marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Silicon'
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n.gt.0) then
          marbl_tracer_metadata(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'CaCO3'
          marbl_tracer_metadata(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' CaCO3'
          marbl_tracer_metadata(n)%units      = 'mmol/m^3'
          marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
          marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
       endif
    end do

  end subroutine marbl_init_autotroph_tracer_metadata
  
  !***********************************************************************

  subroutine marbl_setup_local_tracers(k, column_kmt, tracers, tracer_local)

    !-----------------------------------------------------------------------
    !  create local copies of model tracers
    !  treat negative values as zero,  apply mask to local copies
    !-----------------------------------------------------------------------

    implicit none

    integer(int_kind) , intent(in)  :: k
    integer(int_kind) , intent(in)  :: column_kmt
    real (r8)         , intent(in)  :: tracers(ecosys_base_tracer_cnt)      ! tracer values
    real (r8)         , intent(out) :: tracer_local(ecosys_base_tracer_cnt) ! local copies of model tracer concentrations

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n ! tracer index
    !-----------------------------------------------------------------------

    ! FIXME #30: only need to loop over non-living-biomass-ecosys-tracer-cnt. 

    do n = 1, ecosys_base_tracer_cnt
       if ( k > column_kmt) then
          tracer_local(n) = c0
       else
          tracer_local(n) = max(c0, tracers(n))
       end if
    end do

  end subroutine marbl_setup_local_tracers

  !***********************************************************************

  subroutine marbl_setup_local_zooplankton(k, column_kmt, tracers,            &
             marbl_tracer_indices, zooplankton_local)

    !-----------------------------------------------------------------------
    !  create local copies of model tracers, treat negative values as zero
    !-----------------------------------------------------------------------

    implicit none

    integer (int_kind)           , intent(in)  :: k
    integer(int_kind)            , intent(in)  :: column_kmt
    real (r8)                    , intent(in)  :: tracers(:) ! tracer values
    type(marbl_tracer_index_type), intent(in)  :: marbl_tracer_indices
    type(zooplankton_local_type) , intent(out) :: zooplankton_local(:)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: zoo_ind, n ! tracer index
    !-----------------------------------------------------------------------

    do zoo_ind = 1, zooplankton_cnt
       if (k > column_kmt) then
          zooplankton_local(zoo_ind)%C = c0
       else
          n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
          zooplankton_local(zoo_ind)%C = max(c0, tracers(n))
       end if
    end do

  end subroutine marbl_setup_local_zooplankton

  !***********************************************************************

  subroutine marbl_setup_local_autotrophs(k, column_kmt, tracers,             &
             marbl_tracer_indices, autotroph_local)

    !----------------------------------------------------------------------- 
    !  create local copies of model tracers, treat negative values as zero
    !-----------------------------------------------------------------------

    implicit none

    integer (int_kind)         , intent(in)  :: k
    integer(int_kind)          , intent(in)  :: column_kmt
    real (r8)                  , intent(in)  :: tracers(:)           ! tracer values
    type(marbl_tracer_index_type), intent(in) :: marbl_tracer_indices
    type(autotroph_local_type) , intent(out) :: autotroph_local(:)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: auto_ind, tracer_ind ! tracer index
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
       if (k > column_kmt) then
          autotroph_local(auto_ind)%Chl = c0
          autotroph_local(auto_ind)%C = c0
          autotroph_local(auto_ind)%Fe = c0
          autotroph_local(auto_ind)%Si = c0
          autotroph_local(auto_ind)%CaCO3 = c0
       else
          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
          autotroph_local(auto_ind)%Chl = max(c0, tracers(tracer_ind))
          
          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
          autotroph_local(auto_ind)%C = max(c0, tracers(tracer_ind))
          
          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
          autotroph_local(auto_ind)%Fe = max(c0, tracers(tracer_ind))
          
          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
          if (tracer_ind > 0) then
             autotroph_local(auto_ind)%Si = max(c0, tracers(tracer_ind))
          endif
          
          tracer_ind = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
          if (tracer_ind > 0) then
             autotroph_local(auto_ind)%CaCO3 = max(c0, tracers(tracer_ind))
          endif
       end if
    end do

  end subroutine marbl_setup_local_autotrophs

  !***********************************************************************

  subroutine marbl_consistency_check_autotrophs(auto_cnt, column_kmt,         &
             marbl_tracer_indices, autotroph_local)

    !-----------------------------------------------------------------------
    !  If any phyto box are zero, set others to zeros.
    !-----------------------------------------------------------------------

    implicit none

    integer(int_kind)          , intent(in)    :: auto_cnt   ! autotroph_cnt
    integer(int_kind)          , intent(in)    :: column_kmt ! number of active model layers
    type(marbl_tracer_index_type) , intent(in) :: marbl_tracer_indices
    type(autotroph_local_type) , intent(inout) :: autotroph_local(autotroph_cnt, column_kmt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: auto_ind, k
    logical (log_kind) :: zero_mask
    !-----------------------------------------------------------------------

    do k = 1, column_kmt
       do auto_ind = 1, autotroph_cnt

          zero_mask = (autotroph_local(auto_ind,k)%Chl == c0 .or. &
                       autotroph_local(auto_ind,k)%C   == c0 .or. &
                       autotroph_local(auto_ind,k)%Fe  == c0)

          if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
             zero_mask = zero_mask .or. autotroph_local(auto_ind,k)%Si == c0
          end if
          if (zero_mask) then
             autotroph_local(auto_ind,k)%Chl = c0
             autotroph_local(auto_ind,k)%C = c0
             autotroph_local(auto_ind,k)%Fe = c0
          end if
          if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
             if (zero_mask) then
                autotroph_local(auto_ind,k)%Si = c0
             end if
          end if
          if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
             if (zero_mask) then
                autotroph_local(auto_ind,k)%CaCO3 = c0
             end if
          end if

       end do
    end do

  end subroutine marbl_consistency_check_autotrophs

  !***********************************************************************

  subroutine marbl_compute_autotroph_elemental_ratios(auto_cnt, auto_meta,    &
             autotroph_local, tracer_local, marbl_tracer_indices,             &
             autotroph_secondary_species)

    use marbl_parms     , only : epsC
    use marbl_parms     , only : gQsi_0
    use marbl_parms     , only : gQsi_max
    use marbl_parms     , only : gQsi_min

    implicit none

    integer (int_kind)         , intent(in) :: auto_cnt
    type(autotroph_type)       , intent(in) :: auto_meta(auto_cnt)             ! autotrophs
    type(autotroph_local_type) , intent(in) :: autotroph_local(auto_cnt)
    real (r8)                  , intent(in) :: tracer_local(ecosys_base_tracer_cnt) ! local copies of model tracer concentrations
    type(marbl_tracer_index_type), intent(in) :: marbl_tracer_indices
    type(autotroph_secondary_species_type), intent(inout) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real :: cks      ! constant used in  quota modification
    real :: cksi     ! constant used in Si quota modification
    integer(int_kind) :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                                 &
         Fe_loc     => tracer_local(marbl_tracer_indices%fe_ind),                   &
         SiO3_loc   => tracer_local(marbl_tracer_indices%sio3_ind),                 &
         auto_C     => autotroph_local(:)%C,                   &
         auto_Chl   => autotroph_local(:)%Chl,                 &
         auto_Fe    => autotroph_local(:)%Fe,                  &
         auto_Si    => autotroph_local(:)%Si,                  &
         auto_CaCO3 => autotroph_local(:)%CaCO3,               &
         thetaC     => autotroph_secondary_species(:)%thetaC , & ! local Chl/C ratio (mg Chl/mmol C)
         QCaCO3     => autotroph_secondary_species(:)%QCaCO3 , & ! CaCO3/C ratio (mmol CaCO3/mmol C)
         Qfe        => autotroph_secondary_species(:)%Qfe,     & ! init fe/C ratio (mmolFe/mmolC)
         gQfe       => autotroph_secondary_species(:)%gQfe,    & ! fe/C for growth
         Qsi        => autotroph_secondary_species(:)%Qsi,     & ! initial Si/C ratio (mmol Si/mmol C)
         gQsi       => autotroph_secondary_species(:)%gQsi     & ! diatom Si/C ratio for growth (new biomass)
         )

    !-----------------------------------------------------------------------
    !  set local variables, with incoming ratios
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
       thetaC(auto_ind) = auto_Chl(auto_ind) / (auto_C(auto_ind) + epsC)
       Qfe(auto_ind) = auto_Fe(auto_ind) / (auto_C(auto_ind) + epsC)
       if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
          Qsi(auto_ind) = min(auto_Si(auto_ind) / (auto_C(auto_ind) + epsC), gQsi_max)
       endif
    end do

    !-----------------------------------------------------------------------
    !  DETERMINE NEW ELEMENTAL RATIOS FOR GROWTH (NEW BIOMASS)
    !  Modify these initial ratios under low ambient iron conditions
    !  Modify the initial si/C ratio under low ambient Si conditions
    !-----------------------------------------------------------------------

    cks = 10._r8
    cksi = 5._r8

    do auto_ind = 1, autotroph_cnt
       gQfe(auto_ind) = autotrophs(auto_ind)%gQfe_0
       if (Fe_loc < cks * autotrophs(auto_ind)%kFe) then
          gQfe(auto_ind) = &
               max((gQfe(auto_ind) * Fe_loc / (cks * autotrophs(auto_ind)%kFe)), &
               autotrophs(auto_ind)%gQfe_min)
       end if

       if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
          gQsi(auto_ind) = gQsi_0
          if ((Fe_loc < cksi * autotrophs(auto_ind)%kFe) .and. &
               (Fe_loc > c0) .and. &
               (SiO3_loc > (cksi * autotrophs(auto_ind)%kSiO3))) then
             gQsi(auto_ind) = min((gQsi(auto_ind) * cksi * autotrophs(auto_ind)%kFe / Fe_loc), gQsi_max)
          end if

          if (Fe_loc == c0) then
             gQsi(auto_ind) = gQsi_max
          end if

          if (SiO3_loc < (cksi * autotrophs(auto_ind)%kSiO3)) then
             gQsi(auto_ind) = max((gQsi(auto_ind) * SiO3_loc / (cksi * autotrophs(auto_ind)%kSiO3)), &
                  gQsi_min)
          end if
       endif

       !-----------------------------------------------------------------------
       !  QCaCO3 is the percentage of sp organic matter which is associated
       !  with coccolithophores
       !-----------------------------------------------------------------------

       if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
          QCaCO3(auto_ind) = auto_CaCO3(auto_ind) / (auto_C(auto_ind) + epsC)
          if (QCaCO3(auto_ind) > QCaCO3_max) then
             QCaCO3(auto_ind) = QCaCO3_max
          end if
       end if
    end do
    end associate
  end subroutine marbl_compute_autotroph_elemental_ratios

  !***********************************************************************

  subroutine marbl_compute_PAR(domain, interior_forcing_input, auto_cnt, autotroph_local, PAR)

    !-----------------------------------------------------------------------
    !  compute PAR related quantities
    !  Morel, Maritorena, JGR, Vol 106, No. C4, pp 7163--7180, 2001
    !  0.45   fraction of incoming SW -> PAR (non-dim)
    !-----------------------------------------------------------------------

    ! PAR is intent(inout) because it components, while entirely set here, are allocated elsewhere

    integer(int_kind)                       , intent(in)    :: auto_cnt
    type(marbl_domain_type)                 , intent(in)    :: domain  
    type(marbl_interior_forcing_input_type) , intent(in)    :: interior_forcing_input
    type(autotroph_local_type)              , intent(in)    :: autotroph_local(auto_cnt, domain%km)
    type(marbl_PAR_type)                    , intent(inout) :: PAR

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real (r8) :: WORK1(domain%kmt)
    integer(int_kind) :: k, subcol_ind
    !-----------------------------------------------------------------------

    associate(                                        &
         dkm              => domain%km,               &
         column_kmt       => domain%kmt,              &
         delta_z          => domain%delta_z,          &
         PAR_nsubcols     => domain%num_PAR_subcols   &
         )

    !-----------------------------------------------------------------------
    ! set depth independent quantities, sub-column fractions and PAR at surface
    ! ignore provided shortwave where col_frac == 0
    !-----------------------------------------------------------------------

    PAR%col_frac(:) = interior_forcing_input%PAR_col_frac(:)

    where (PAR%col_frac(:) > c0)
       PAR%interface(0,:) = f_qsw_par * interior_forcing_input%surf_shortwave(:)
    elsewhere
       PAR%interface(0,:) = c0
    endwhere

    !-----------------------------------------------------------------------
    ! avoid further computations, such as computing attenuation coefficient, if there is no light
    ! treat forcing as a single dark value, by setting col_frac(1) to 1
    !-----------------------------------------------------------------------

    if (all(PAR%interface(0,:) == c0)) then
       PAR%col_frac(:)    = c0
       PAR%col_frac(1)    = c1
       PAR%interface(:,:) = c0
       PAR%avg(:,:)       = c0
       PAR%KPARdz(:)      = c0
       return
    end if

    !-----------------------------------------------------------------------
    ! compute attenuation coefficient over column
    !-----------------------------------------------------------------------

    ! FIXME #31: move calculation outside and just pass in this
    !            work array as autotroph_Chl instead of passing
    !            in all of autotroph_local?
    WORK1(:) = max(sum(autotroph_local(:,1:column_kmt)%Chl, dim=1), 0.02_r8)

    do k = 1, column_kmt

       if (WORK1(k) < 0.13224_r8) then
          PAR%KPARdz(k) = 0.000919_r8*(WORK1(k)**0.3536_r8)
       else
          PAR%KPARdz(k) = 0.001131_r8*(WORK1(k)**0.4562_r8)
       end if

       PAR%KPARdz(k) = PAR%KPARdz(k) * delta_z(k)

    enddo

    PAR%KPARdz(column_kmt+1:dkm) = c0

    !-----------------------------------------------------------------------
    ! propagate PAR values through column, only on subcolumns with PAR>0
    ! note that if col_frac is 0, then so is PAR
    !-----------------------------------------------------------------------

    WORK1(:) = exp(-PAR%KPARdz(1:column_kmt))

    do subcol_ind = 1, PAR_nsubcols
       if (PAR%interface(0,subcol_ind) > c0) then

          ! this look will probably not vectorize
          do k = 1, column_kmt
             PAR%interface(k,subcol_ind) = PAR%interface(k-1,subcol_ind) * WORK1(k)
          enddo
          PAR%interface(column_kmt+1:dkm,subcol_ind) = c0

          do k = 1, column_kmt
             PAR%avg(k,subcol_ind) = PAR%interface(k-1,subcol_ind) * (c1 - WORK1(k)) / PAR%KPARdz(k)
          enddo
          PAR%avg(column_kmt+1:dkm,subcol_ind) = c0

       else

          PAR%interface(1:dkm,subcol_ind) = c0
          PAR%avg(1:dkm,subcol_ind) = c0

       endif
   end do

   end associate

   end subroutine marbl_compute_PAR

  !***********************************************************************

  subroutine marbl_compute_carbonate_chemistry(domain, interior_forcing_input, &
       tracer_local, marbl_tracer_indices, carbonate, ph_prev_col,             &
       ph_prev_alt_co2_col, zsat_calcite, zsat_aragonite, marbl_status_log)

    use marbl_co2calc_mod, only : marbl_comp_co3terms         
    use marbl_co2calc_mod, only : marbl_comp_co3_sat_vals     
    use marbl_co2calc_mod, only : thermodynamic_coefficients_type

    type(marbl_domain_type)                 , intent(in)    :: domain
    type(marbl_interior_forcing_input_type) , intent(in)    :: interior_forcing_input
    real (r8)                               , intent(in)    :: tracer_local(ecosys_base_tracer_cnt,domain%km) ! local copies of model tracer concentrations
    type(marbl_tracer_index_type)           , intent(in)    :: marbl_tracer_indices
    type(carbonate_type)                    , intent(out)   :: carbonate(domain%km)
    real(r8)                                , intent(inout) :: ph_prev_col(domain%km)
    real(r8)                                , intent(inout) :: ph_prev_alt_co2_col(domain%km)
    real(r8)                                , intent(inout) :: zsat_calcite(domain%km)                   ! Calcite Saturation Depth
    real(r8)                                , intent(inout) :: zsat_aragonite(domain%km)                 ! Aragonite Saturation Depth
    type(marbl_log_type)                    , intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_mod:marbl_compute_carbonate_chemistry'
    integer :: k
    type(thermodynamic_coefficients_type), dimension(domain%km) :: co3_coeffs
    logical(log_kind) , dimension(domain%km) :: mask
    logical(log_kind) , dimension(domain%km) :: pressure_correct
    real(r8)          , dimension(domain%km) :: ph_lower_bound
    real(r8)          , dimension(domain%km) :: ph_upper_bound
    real(r8)          , dimension(domain%km) :: press_bar ! pressure at level (bars)
    real(r8)          , dimension(domain%km) :: dic_loc
    real(r8)          , dimension(domain%km) :: dic_alt_co2_loc
    real(r8)          , dimension(domain%km) :: alk_loc
    real(r8)          , dimension(domain%km) :: po4_loc
    real(r8)          , dimension(domain%km) :: sio3_loc
    !-----------------------------------------------------------------------

    ! make local copies instead of using associate construct because of gnu fortran bug
    ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=68546

    dic_loc(:)         = tracer_local(marbl_tracer_indices%dic_ind,:)
    dic_alt_co2_loc(:) = tracer_local(marbl_tracer_indices%dic_alt_co2_ind,:)
    alk_loc(:)         = tracer_local(marbl_tracer_indices%alk_ind,:)
    po4_loc(:)         = tracer_local(marbl_tracer_indices%po4_ind,:)
    sio3_loc(:)        = tracer_local(marbl_tracer_indices%sio3_ind,:)

    associate(                                                    &
         dkm               => domain%km,                          &
         column_kmt        => domain%kmt,                         &
         co3               => carbonate(:)%CO3,                   &
         hco3              => carbonate(:)%HCO3,                  &
         h2co3             => carbonate(:)%H2CO3,                 &
         ph                => carbonate(:)%pH,                    &
         co3_sat_calcite   => carbonate(:)%CO3_sat_calcite,       &
         co3_sat_aragonite => carbonate(:)%CO3_sat_aragonite,     &
         co3_alt_co2       => carbonate(:)%CO3_ALT_CO2,           &
         hco3_alt_co2      => carbonate(:)%HCO3_ALT_CO2,          &
         h2co3_alt_co2     => carbonate(:)%H2CO3_ALT_CO2,         &
         ph_alt_co2        => carbonate(:)%pH_ALT_CO2,            &
         temperature       => interior_forcing_input%temperature, &
         press_bar         => interior_forcing_input%pressure,    &
         salinity          => interior_forcing_input%salinity     &
         )

    pressure_correct = .TRUE.
    pressure_correct(1) = .FALSE.
    do k=1,dkm

      mask(k) = (k <= column_kmt)

       ! -------------------
       if (ph_prev_col(k)  /= c0) then
          ph_lower_bound(k) = ph_prev_col(k) - del_ph
          ph_upper_bound(k) = ph_prev_col(k) + del_ph
       else
          ph_lower_bound(k) = phlo_3d_init
          ph_upper_bound(k) = phhi_3d_init
       end if

    enddo

    call marbl_comp_CO3terms(&
         dkm, mask, pressure_correct, .true., co3_coeffs, temperature, &
         salinity, press_bar, dic_loc, alk_loc, po4_loc, sio3_loc, &
         ph_lower_bound, ph_upper_bound, ph, h2co3, hco3, co3,     &
         marbl_status_log)

    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('marbl_comp_CO3terms()', subname)
      return
    end if

    do k=1,dkm

       ph_prev_col(k) = pH(k)
       
       ! -------------------
       if (ph_prev_alt_co2_col(k) /= c0) then
          ph_lower_bound(k) = ph_prev_alt_co2_col(k) - del_ph
          ph_upper_bound(k) = ph_prev_alt_co2_col(k) + del_ph
       else
          ph_lower_bound(k) = phlo_3d_init
          ph_upper_bound(k) = phhi_3d_init
       end if

    enddo

    call marbl_comp_CO3terms(&
         dkm, mask, pressure_correct, .false., co3_coeffs, temperature,    &
         salinity, press_bar, dic_alt_co2_loc, alk_loc, po4_loc, sio3_loc, &
         ph_lower_bound, ph_upper_bound, ph_alt_co2, h2co3_alt_co2,        &
         hco3_alt_co2, co3_alt_co2, marbl_status_log)

    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('marbl_comp_CO3terms()', subname)
      return
    end if
       
    ph_prev_alt_co2_col = ph_alt_co2

    call marbl_comp_co3_sat_vals(&
         dkm, mask, pressure_correct, temperature, salinity, &
         press_bar, co3_sat_calcite, co3_sat_aragonite)
       
    end associate

  end subroutine marbl_compute_carbonate_chemistry

  !***********************************************************************

  subroutine marbl_compute_function_scaling(column_temperature, Tfunc )

    !-----------------------------------------------------------------------
    !  Tref = 30.0 reference temperature (deg. C)
    !  Using q10 formulation with Q10 value of 2.0 (Doney et al., 1996).
    !  growth, mort and grazing rates scaled by Tfunc where they are computed
    !-----------------------------------------------------------------------

    use marbl_parms, only : Q_10
    use marbl_parms, only : Tref
    use marbl_parms, only : c10

    real(r8), intent(in)  :: column_temperature
    real(r8), intent(out) :: Tfunc

    Tfunc = Q_10**(((column_temperature + T0_Kelvin) - (Tref + T0_Kelvin)) / c10)

  end subroutine marbl_compute_function_scaling

  !***********************************************************************

  subroutine marbl_compute_Pprime(k, domain, auto_cnt, auto_meta, &
       autotroph_local, column_temperature, autotroph_secondary_species)

    use marbl_parms           , only : thres_z1_auto
    use marbl_parms           , only : thres_z2_auto

    integer(int_kind)                      , intent(in)  :: k
    type(marbl_domain_type)                , intent(in)  :: domain
    integer(int_kind)                      , intent(in)  :: auto_cnt
    type(autotroph_type)                   , intent(in)  :: auto_meta(auto_cnt)
    type(autotroph_local_type)             , intent(in)  :: autotroph_local(auto_cnt)
    real(r8)                               , intent(in)  :: column_temperature
    type(autotroph_secondary_species_type) , intent(out) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    real(r8) :: f_loss_thres
    real(r8) :: C_loss_thres
    !-----------------------------------------------------------------------

    associate(                                           &
         zt     => domain%zt(:),                         &
         Pprime => autotroph_secondary_species(:)%Pprime & ! output
         )

    !  calculate the loss threshold interpolation factor
    if (zt(k) > thres_z1_auto) then
       if (zt(k) < thres_z2_auto) then
          f_loss_thres = (thres_z2_auto - zt(k))/(thres_z2_auto - thres_z1_auto)
       else
          f_loss_thres = c0
       endif
    else
       f_loss_thres = c1
    endif

    !  Compute Pprime for all autotrophs, used for loss terms
    do auto_ind = 1, auto_cnt
       if (column_temperature < auto_meta(auto_ind)%temp_thres) then
          C_loss_thres = f_loss_thres * auto_meta(auto_ind)%loss_thres2
       else
          C_loss_thres = f_loss_thres * auto_meta(auto_ind)%loss_thres
       end if
       Pprime(auto_ind) = max(autotroph_local(auto_ind)%C - C_loss_thres, c0)
    end do

    end associate

  end subroutine marbl_compute_Pprime

  !***********************************************************************

  subroutine marbl_compute_Zprime(k, domain, &
       zoo_cnt, zoo_meta, zooC, &
       Tfunc, zooplankton_secondary_species)

    use marbl_parms           , only : c1, c0
    use marbl_parms           , only : thres_z1_zoo
    use marbl_parms           , only : thres_z2_zoo

    integer(int_kind)                        , intent(in)    :: k
    type(marbl_domain_type)                  , intent(in)    :: domain
    integer(int_kind)                        , intent(in)    :: zoo_cnt
    type(zooplankton_type)                   , intent(in)    :: zoo_meta(zoo_cnt)
    real(r8)                                 , intent(in)    :: zooC(zoo_cnt)
    real(r8)                                 , intent(in)    :: Tfunc
    type(zooplankton_secondary_species_type) , intent(inout) :: zooplankton_secondary_species(zoo_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: zoo_ind
    real(r8) :: f_loss_thres
    real(r8) :: C_loss_thres
    !-----------------------------------------------------------------------

    associate(                                                  &
         zt       => domain%zt(:),                              & !(km)
         Zprime   => zooplankton_secondary_species(:)%Zprime,   & !(zoo_cnt)
         zoo_loss => zooplankton_secondary_species(:)%zoo_loss  & !(zoo_cnt) output
         )

    !  calculate the loss threshold interpolation factor
    if (zt(k) > thres_z1_zoo) then
       if (zt(k) < thres_z2_zoo) then
          f_loss_thres = (thres_z2_zoo - zt(k))/(thres_z2_zoo - thres_z1_zoo)
       else
          f_loss_thres = c0
       endif
    else
       f_loss_thres = c1
    endif

    do zoo_ind = 1, zoo_cnt
       C_loss_thres = f_loss_thres * zoo_meta(zoo_ind)%loss_thres
       Zprime(zoo_ind) = max(zooC(zoo_ind) - C_loss_thres, c0)

       zoo_loss(zoo_ind) = ( zoo_meta(zoo_ind)%z_mort2_0 * Zprime(zoo_ind)**1.5_r8 + &
            zoo_meta(zoo_ind)%z_mort_0  * Zprime(zoo_ind)) * Tfunc
    end do

    end associate
  end subroutine marbl_compute_Zprime

  !***********************************************************************

  subroutine marbl_compute_autotroph_uptake (auto_cnt, auto_meta, &
       tracer_local, marbl_tracer_indices, autotroph_secondary_species)

    use marbl_parms     , only : c1

    integer(int_kind)                      , intent(in)  :: auto_cnt
    type(autotroph_type)                   , intent(in)  :: auto_meta(auto_cnt)
    real(r8)                               , intent(in)  :: tracer_local(ecosys_base_tracer_cnt)
    type(marbl_tracer_index_type)          , intent(in)  :: marbl_tracer_indices
    type(autotroph_secondary_species_type) , intent(out) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !  Get relative nutrient uptake rates for autotrophs,
    !  min. relative uptake rate modifies C fixation in the manner
    !  that the min. cell quota does in GD98.
    !-----------------------------------------------------------------------

    do auto_ind = 1, auto_cnt

       associate(                                                             &
                 DOP_loc => tracer_local(marbl_tracer_indices%dop_ind),       &
                 NO3_loc => tracer_local(marbl_tracer_indices%no3_ind),       &
                 NH4_loc => tracer_local(marbl_tracer_indices%nh4_ind),       &
                 PO4_loc => tracer_local(marbl_tracer_indices%po4_ind),       &
                 Fe_loc   => tracer_local(marbl_tracer_indices%fe_ind),       &
                 SiO3_loc => tracer_local(marbl_tracer_indices%sio3_ind),     &
                 ! OUTPUTS
                 VNO3  => autotroph_secondary_species(auto_ind)%VNO3,         &
                 VNH4  => autotroph_secondary_species(auto_ind)%VNH4,         &
                 VNtot => autotroph_secondary_species(auto_ind)%VNtot,        &
                 VFe   => autotroph_secondary_species(auto_ind)%VFe,          &
                 f_nut => autotroph_secondary_species(auto_ind)%f_nut,        &
                 VDOP  => autotroph_secondary_species(auto_ind)%VDOP,         &
                 VPO4  => autotroph_secondary_species(auto_ind)%VPO4,         &
                 VPtot => autotroph_secondary_species(auto_ind)%VPtot,        &
                 VSiO3 => autotroph_secondary_species(auto_ind)%VSiO3,        &
                 ! AUTO_META
                 kNO3   => auto_meta(auto_ind)%kNO3,                          &
                 kNH4   => auto_meta(auto_ind)%kNH4,                          &
                 kFe    => auto_meta(auto_ind)%kFe,                           &
                 kPO4   => auto_meta(auto_ind)%kPO4,                          &
                 kDOP   => auto_meta(auto_ind)%kDOP,                          &
                 kSiO3  => auto_meta(auto_ind)%kSiO3,                         &
                 Nfixer => auto_meta(auto_ind)%Nfixer                         &
                )

       VNO3 = (NO3_loc / kNO3) / (c1 + (NO3_loc / kNO3) + (NH4_loc / kNH4))
       VNH4 = (NH4_loc / kNH4) / (c1 + (NO3_loc / kNO3) + (NH4_loc / kNH4))
       VNtot = VNO3 + VNH4
       if (Nfixer) then
          VNtot = c1
       end if

       VFe = Fe_loc / (Fe_loc + kFe)

       VPO4 = (PO4_loc / kPO4) / (c1 + (PO4_loc / kPO4) + (DOP_loc / kDOP))
       VDOP = (DOP_loc / kDOP) / (c1 + (PO4_loc / kPO4) + (DOP_loc / kDOP))
       VPtot = VPO4 + VDOP

       if (kSiO3 > c0) then
          VSiO3 = SiO3_loc / (SiO3_loc + kSiO3)
       endif

       f_nut = min(VNtot, VFe)
       f_nut = min(f_nut, VPO4)
       if (kSiO3 > c0) then
          f_nut = min(f_nut, VSiO3)
       endif

    end associate

    end do

  end subroutine marbl_compute_autotroph_uptake

  !***********************************************************************

  subroutine marbl_compute_autotroph_photosynthesis (auto_cnt, PAR_nsubcols, &
       auto_meta, autotroph_loc, temperature, Tfunc, PAR_col_frac, PAR_avg,  &
       autotroph_secondary_species)

    !-----------------------------------------------------------------------
    !     get photosynth. rate, phyto C biomass change, photoadapt
    !-----------------------------------------------------------------------

    use marbl_parms     , only : c0, c1
    use marbl_parms     , only : epsTinv

    integer(int_kind)                      , intent(in)    :: auto_cnt
    integer(int_kind)                      , intent(in)    :: PAR_nsubcols
    type(autotroph_type)                   , intent(in)    :: auto_meta(auto_cnt)
    type(autotroph_local_type)             , intent(in)    :: autotroph_loc(auto_cnt)
    real(r8)                               , intent(in)    :: temperature
    real(r8)                               , intent(in)    :: Tfunc
    real(r8)                               , intent(in)    :: PAR_col_frac(PAR_nsubcols)
    real(r8)                               , intent(in)    :: PAR_avg(PAR_nsubcols)
    type(autotroph_secondary_species_type) , intent(inout) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind, subcol_ind
    real(r8) :: PCmax            ! max value of PCphoto at temperature TEMP (1/sec)
    real(r8) :: light_lim_subcol ! light_lim for a sub-column
    real(r8) :: PCphoto_subcol   ! PCphoto for a sub-column
    real(r8) :: pChl_subcol      ! Chl synth. regulation term (mg Chl/mmol N)
    real(r8) :: photoacc_subcol  ! photoacc for a sub-column
    !-----------------------------------------------------------------------

    do auto_ind = 1, auto_cnt

       associate(                                                         &
            ! local Chl/C ratio (mg Chl / mmol C)
            thetaC    => autotroph_secondary_species(auto_ind)%thetaC,    &
            f_nut     => autotroph_secondary_species(auto_ind)%f_nut,     &
            VNtot     => autotroph_secondary_species(auto_ind)%VNtot,     &
            light_lim => autotroph_secondary_species(auto_ind)%light_lim, &
            PCPhoto   => autotroph_secondary_species(auto_ind)%PCPhoto,   &
            photoC    => autotroph_secondary_species(auto_ind)%photoC,    &
            photoacc  => autotroph_secondary_species(auto_ind)%photoacc,  &
            
            PCref     => auto_meta(auto_ind)%PCref,                       &
            alphaPI   => auto_meta(auto_ind)%alphaPI                      &
            )

       PCmax = PCref * f_nut * Tfunc
       if (temperature < autotrophs(auto_ind)%temp_thres) then
          PCmax = c0
       end if

       if (thetaC > c0) then
          light_lim = c0
          PCphoto   = c0
          photoacc  = c0

          do subcol_ind = 1, PAR_nsubcols
             if (PAR_avg(subcol_ind) > c0) then
                light_lim_subcol = (c1 - exp((-c1 * alphaPI * thetaC * PAR_avg(subcol_ind)) / (PCmax + epsTinv)))

                PCphoto_subcol = PCmax * light_lim_subcol

                ! GD 98 Chl. synth. term
                pChl_subcol = autotrophs(auto_ind)%thetaN_max * PCphoto_subcol / &
                     (autotrophs(auto_ind)%alphaPI * thetaC * PAR_avg(subcol_ind))
                photoacc_subcol = (pChl_subcol * PCphoto_subcol * Q / thetaC) * autotroph_loc(auto_ind)%Chl

                light_lim = light_lim + PAR_col_frac(subcol_ind) * light_lim_subcol
                PCphoto   = PCphoto   + PAR_col_frac(subcol_ind) * PCphoto_subcol
                photoacc  = photoacc  + PAR_col_frac(subcol_ind) * photoacc_subcol
             end if
          end do

          photoC = PCphoto * autotroph_loc(auto_ind)%C
       else
          light_lim = c0
          PCphoto   = c0
          photoacc  = c0
          photoC    = c0
       endif

       end associate

    end do

  end subroutine marbl_compute_autotroph_photosynthesis

  !***********************************************************************

  subroutine marbl_compute_autotroph_phyto_diatoms (auto_cnt, auto_meta, &
       autotroph_loc, marbl_tracer_indices, autotroph_secondary_species)

    !-----------------------------------------------------------------------
    !  Get nutrient uptakes by small phyto based on calculated C fixation
    !-----------------------------------------------------------------------

    use marbl_parms     , only : c0
    use marbl_parms     , only : Q

    integer(int_kind)                      , intent(in)    :: auto_cnt
    type(autotroph_type)                   , intent(in)    :: auto_meta(auto_cnt)
    type(autotroph_local_type)             , intent(in)    :: autotroph_loc(auto_cnt)
    type(marbl_tracer_index_type)          , intent(in)    :: marbl_tracer_indices
    type(autotroph_secondary_species_type) , intent(inout) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                               &
         gQfe     => autotroph_secondary_species(:)%gQfe,    & ! fe/C for growth
         gQsi     => autotroph_secondary_species(:)%gQsi,    & ! diatom Si/C ratio for growth (new biomass)
         VNO3     => autotroph_secondary_species(:)%VNO3,    & ! input
         VNH4     => autotroph_secondary_species(:)%VNH4,    & ! input
         VNtot    => autotroph_secondary_species(:)%VNtot,   & ! input
         VPO4     => autotroph_secondary_species(:)%VPO4,    & ! input
         VDOP     => autotroph_secondary_species(:)%VDOP,    & ! input
         VPtot    => autotroph_secondary_species(:)%VPtot,   & ! input
         photoC   => autotroph_secondary_species(:)%photoC,  & ! input
         NO3_V    => autotroph_secondary_species(:)%NO3_V,   & ! output
         NH4_V    => autotroph_secondary_species(:)%NH4_V,   & ! output
         PO4_V    => autotroph_secondary_species(:)%PO4_V,   & ! output
         DOP_V    => autotroph_secondary_species(:)%DOP_V,   & ! output
         photoFe  => autotroph_secondary_species(:)%photoFe, & ! output
         photoSi  => autotroph_secondary_species(:)%photoSi  & ! output
         )

    do auto_ind = 1, auto_cnt

       if (VNtot(auto_ind) > c0) then
          NO3_V(auto_ind) = (VNO3(auto_ind) / VNtot(auto_ind)) * photoC(auto_ind) * Q
          NH4_V(auto_ind) = (VNH4(auto_ind) / VNtot(auto_ind)) * photoC(auto_ind) * Q
       else
          NO3_V(auto_ind) = c0
          NH4_V(auto_ind) = c0
       end if

       if (VPtot(auto_ind) > c0) then
          PO4_V(auto_ind) = (VPO4(auto_ind) / VPtot(auto_ind)) * photoC(auto_ind) * auto_meta(auto_ind)%Qp
          DOP_V(auto_ind) = (VDOP(auto_ind) / VPtot(auto_ind)) * photoC(auto_ind) * auto_meta(auto_ind)%Qp
       else
          PO4_V(auto_ind) = c0
          DOP_V(auto_ind) = c0
       end if

       !-----------------------------------------------------------------------
       !  Get nutrient uptake by diatoms based on C fixation
       !-----------------------------------------------------------------------

       photoFe(auto_ind) = photoC(auto_ind) * gQfe(auto_ind)

       if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
          photoSi(auto_ind) = photoC(auto_ind) * gQsi(auto_ind)
       endif

    end do

    end associate

  end subroutine marbl_compute_autotroph_phyto_diatoms

  !***********************************************************************

  subroutine marbl_compute_autotroph_calcification (auto_cnt, auto_meta, &
       autotroph_loc, temperature, autotroph_secondary_species)

    !-----------------------------------------------------------------------
    !  CaCO3 Production, parameterized as function of small phyto production
    !  decrease CaCO3 as function of nutrient limitation decrease CaCO3 prod
    !  at low temperatures increase CaCO3 prod under bloom conditions
    !  maximum calcification rate is 40% of primary production
    !-----------------------------------------------------------------------

    use marbl_parms     , only : parm_f_prod_sp_CaCO3
    use marbl_parms     , only : CaCO3_sp_thres
    use marbl_parms     , only : CaCO3_temp_thres1
    use marbl_parms     , only : CaCO3_temp_thres2
    use marbl_parms     , only : f_photosp_CaCO3

    integer(int_kind)                      , intent(in)    :: auto_cnt
    type(autotroph_type)                   , intent(in)    :: auto_meta(auto_cnt)
    type(autotroph_local_type)             , intent(in)    :: autotroph_loc(auto_cnt)
    real(r8)                               , intent(in)    :: temperature
    type(autotroph_secondary_species_type) , intent(inout) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                                       &
         f_nut      => autotroph_secondary_species(:)%f_nut,     & ! input
         photoC     => autotroph_secondary_species(:)%photoC,    & ! input
         CaCO3_PROD => autotroph_secondary_species(:)%CaCO3_PROD & ! output
         )

    do auto_ind = 1, auto_cnt
       if (auto_meta(auto_ind)%imp_calcifier) then
          CaCO3_PROD(auto_ind) = parm_f_prod_sp_CaCO3 * photoC(auto_ind)
          CaCO3_PROD(auto_ind) = CaCO3_PROD(auto_ind) * f_nut(auto_ind) * f_nut(auto_ind)

          if (temperature < CaCO3_temp_thres1)  then
             CaCO3_PROD(auto_ind) = CaCO3_PROD(auto_ind) * max((temperature - CaCO3_temp_thres2), c0) / &
                  (CaCO3_temp_thres1-CaCO3_temp_thres2)
          end if

          if (autotroph_loc(auto_ind)%C > CaCO3_sp_thres) then
             CaCO3_PROD(auto_ind) = min((CaCO3_PROD(auto_ind) * autotroph_loc(auto_ind)%C / CaCO3_sp_thres), &
                  (f_photosp_CaCO3 * photoC(auto_ind)))
          end if
       end if
    end do

    end associate
  end subroutine marbl_compute_autotroph_calcification

  !***********************************************************************

  subroutine marbl_compute_autotroph_nfixation (auto_cnt, auto_meta, &
       autotroph_secondary_species)

    !-----------------------------------------------------------------------
    !  Get N fixation by diazotrophs based on C fixation,
    !  Diazotrophs fix more than they need then 20% is excreted
    !-----------------------------------------------------------------------

    use marbl_parms     , only : Q
    use marbl_parms     , only : r_Nfix_photo

    integer(int_kind)                          , intent(in)  :: auto_cnt
    type(autotroph_type)                       , intent(in)  :: auto_meta(auto_cnt)
    type(autotroph_secondary_species_type) , intent(out) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    real(r8) :: work1
    !-----------------------------------------------------------------------

    associate(                                                   &
         photoC   => autotroph_secondary_species(:)%photoC,  & ! input
         NO3_V    => autotroph_secondary_species(:)%NO3_V ,  & ! input
         NH4_V    => autotroph_secondary_species(:)%NH4_V ,  & ! input
         Nfix     => autotroph_secondary_species(:)%Nfix  ,  & ! output total Nitrogen fixation (mmol N/m^3/sec)
         Nexcrete => autotroph_secondary_species(:)%Nexcrete & ! output fixed N excretion
         )

    do auto_ind = 1, autotroph_cnt
       if (auto_meta(auto_ind)%Nfixer) then
          work1 = photoC(auto_ind) * Q
          Nfix(auto_ind) = (work1 * r_Nfix_photo) - NO3_V(auto_ind) - NH4_V(auto_ind)
          Nexcrete(auto_ind) = Nfix(auto_ind) + NO3_V(auto_ind) + NH4_V(auto_ind) - work1
       endif
    end do

    end associate
  end subroutine marbl_compute_autotroph_nfixation

  !***********************************************************************

  subroutine marbl_compute_autotroph_loss (auto_cnt, auto_meta, &
       Tfunc, autotroph_secondary_species)

    !-----------------------------------------------------------------------
    ! Compute autotroph-loss, autotroph aggregation loss and routine of
    ! loss terms
    !-----------------------------------------------------------------------

    use marbl_parms, only : dps

    integer(int_kind)                          , intent(in)    :: auto_cnt
    type(autotroph_type)                       , intent(in)    :: auto_meta(auto_cnt)
    real(r8)                                   , intent(in)    :: Tfunc
    type(autotroph_secondary_species_type) , intent(inout) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                                               &
         QCaCO3        => autotroph_secondary_species(:)%QCaCO3        , & ! input
         Pprime        => autotroph_secondary_species(:)%Pprime        , & ! input
         auto_loss     => autotroph_secondary_species(:)%auto_loss     , & ! output
         auto_loss_poc => autotroph_secondary_species(:)%auto_loss_poc , & ! output
         auto_loss_dic => autotroph_secondary_species(:)%auto_loss_dic , & ! output
         auto_loss_doc => autotroph_secondary_species(:)%auto_loss_doc , & ! output
         auto_agg      => autotroph_secondary_species(:)%auto_agg        & ! output
         )

    do auto_ind = 1, autotroph_cnt
       !-----------------------------------------------------------------------
       !  get autotroph loss (in C units)
       !  autotroph agg loss
       !-----------------------------------------------------------------------

       auto_loss(auto_ind) = auto_meta(auto_ind)%mort * Pprime(auto_ind) * Tfunc

       auto_agg(auto_ind) = min((auto_meta(auto_ind)%agg_rate_max * dps) * Pprime(auto_ind), &
            auto_meta(auto_ind)%mort2 * Pprime(auto_ind)**1.75_r8)
       auto_agg(auto_ind) = max((auto_meta(auto_ind)%agg_rate_min * dps) * Pprime(auto_ind), auto_agg(auto_ind))

       !-----------------------------------------------------------------------
       !  routing of loss terms
       !  all aggregation goes to POC
       !  min.%C routed from sp_loss = 0.59 * QCaCO3, or P_CaCO3%rho
       !-----------------------------------------------------------------------

       if (auto_meta(auto_ind)%imp_calcifier) then
          auto_loss_poc(auto_ind) = QCaCO3(auto_ind) * auto_loss(auto_ind)
       else
          auto_loss_poc(auto_ind) = auto_meta(auto_ind)%loss_poc * auto_loss(auto_ind)
       endif
       auto_loss_doc(auto_ind) = (c1 - parm_labile_ratio) * (auto_loss(auto_ind) - auto_loss_poc(auto_ind))
       auto_loss_dic(auto_ind) = parm_labile_ratio * (auto_loss(auto_ind) - auto_loss_poc(auto_ind))
    end do  ! auto_ind = 1, autotroph_cnt

    end associate
  end subroutine marbl_compute_autotroph_loss

  !***********************************************************************

  subroutine marbl_compute_grazing (auto_cnt, zoo_cnt, grazer_prey_cnt, auto_meta, &
       Tfunc, zooplankton_loc, &
       zooplankton_secondary_species, autotroph_secondary_species)

    !-----------------------------------------------------------------------
    !  CALCULATE GRAZING
    !
    !  Autotroph prey
    !  routing of grazing terms
    !  all aggregation goes to POC
    !  currently assumes that 33% of grazed caco3 is remineralized
    !  if autotrophs(sp_ind)%graze_zoo ever changes, coefficients on routing grazed sp must change!
    !  min.%C routed to POC from grazing for ballast requirements = 0.4 * Qcaco3
    !  NOTE: if autotrophs(diat_ind)%graze_zoo is changed, coeff.s for poc, doc and dic must change!
    !-----------------------------------------------------------------------

    use marbl_parms     , only : epsC
    use marbl_parms     , only : epsTinv
    use marbl_parms     , only : grz_fnc_michaelis_menten
    use marbl_parms     , only : grz_fnc_sigmoidal
    use marbl_parms     , only : c0

    integer(int_kind)                        , intent(in)    :: auto_cnt
    integer(int_kind)                        , intent(in)    :: zoo_cnt
    integer(int_kind)                        , intent(in)    :: grazer_prey_cnt
    type(autotroph_type)                     , intent(in)    :: auto_meta(auto_cnt)
    real(r8)                                 , intent(in)    :: Tfunc
    type(zooplankton_local_type)             , intent(in)    :: zooplankton_loc(zoo_cnt)
    type(zooplankton_secondary_species_type) , intent(inout) :: zooplankton_secondary_species(zoo_cnt)
    type(autotroph_secondary_species_type)   , intent(inout) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind, auto_ind2
    integer  :: zoo_ind, zoo_ind2
    integer  :: pred_ind
    integer  :: prey_ind
    real(r8) :: work1, work2, work3, work4
    real(r8) :: graze_rate
    !-----------------------------------------------------------------------

    associate(                                                              &
         Pprime         => autotroph_secondary_species(:)%Pprime          , & ! input
         QCaCO3         => autotroph_secondary_species(:)%QCaCO3          , & ! input
         Zprime         => zooplankton_secondary_species(:)%Zprime        , & ! input
         auto_graze     => autotroph_secondary_species(:)%auto_graze      , & ! output
         auto_graze_poc => autotroph_secondary_species(:)%auto_graze_poc  , & ! output
         auto_graze_dic => autotroph_secondary_species(:)%auto_graze_dic  , & ! output
         auto_graze_doc => autotroph_secondary_species(:)%auto_graze_doc  , & ! output
         auto_graze_zoo => autotroph_secondary_species(:)%auto_graze_zoo  , & ! output
         zoo_graze      => zooplankton_secondary_species(:)%zoo_graze     , & ! output
         zoo_graze_poc  => zooplankton_secondary_species(:)%zoo_graze_poc , & ! output
         zoo_graze_dic  => zooplankton_secondary_species(:)%zoo_graze_dic , & ! output
         zoo_graze_doc  => zooplankton_secondary_species(:)%zoo_graze_doc , & ! output
         zoo_graze_zoo  => zooplankton_secondary_species(:)%zoo_graze_zoo , & ! output
         x_graze_zoo    => zooplankton_secondary_species(:)%x_graze_zoo   , & ! output
         f_zoo_detr     => zooplankton_secondary_species(:)%f_zoo_detr      & ! output
         )

    auto_graze(:)     = c0 ! total grazing losses from autotroph pool at auto_ind
    auto_graze_zoo(:) = c0 ! autotroph grazing losses routed to zooplankton at auto_ind
    auto_graze_poc(:) = c0 ! autotroph grazing losses routed to poc
    auto_graze_doc(:) = c0 ! autotroph grazing losses routed to doc
    auto_graze_dic(:) = c0 ! autotroph grazing losses routed to dic (computed by residual)

    zoo_graze(:)     = c0 ! total grazing losses from zooplankton pool at zoo_ind
    zoo_graze_zoo(:) = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
    zoo_graze_poc(:) = c0 ! zooplankton grazing losses routed to poc
    zoo_graze_doc(:) = c0 ! zooplankton grazing losses routed to doc
    zoo_graze_dic(:) = c0 ! zooplankton grazing losses routed to dic (computed by residual)

    x_graze_zoo(:)   = c0 ! grazing gains by zooplankton at zoo_ind

    do pred_ind = 1, zoo_cnt

       work3 = c0
       work4 = c0

       do prey_ind = 1, grazer_prey_cnt

          !-----------------------------------------------------------------------
          !  compute sum of carbon in the grazee class, both autotrophs and zoop
          !-----------------------------------------------------------------------
          work1 = c0 ! biomass in prey class prey_ind
          do auto_ind2 = 1, grazing(prey_ind, pred_ind)%auto_ind_cnt
             auto_ind = grazing(prey_ind, pred_ind)%auto_ind(auto_ind2)
             work1 = work1 + Pprime(auto_ind)
          end do

          do zoo_ind2 = 1, grazing(prey_ind, pred_ind)%zoo_ind_cnt
             zoo_ind = grazing(prey_ind, pred_ind)%zoo_ind(zoo_ind2)
             work1 = work1 + Zprime(zoo_ind)
          end do

          ! compute grazing rate
          select case (grazing(prey_ind, pred_ind)%grazing_function)

          case (grz_fnc_michaelis_menten)

             if (work1 > c0) then
                graze_rate = grazing(prey_ind, pred_ind)%z_umax_0 * Tfunc * zooplankton_loc(pred_ind)%C &
                     * ( work1 / (work1 + grazing(prey_ind, pred_ind)%z_grz) )
             else
                graze_rate = c0
             end if

          case (grz_fnc_sigmoidal)

             if (work1 > c0) then
                graze_rate = grazing(prey_ind, pred_ind)%z_umax_0 * Tfunc * zooplankton_loc(pred_ind)%C &
                     * ( work1**2 / (work1**2 + grazing(prey_ind, pred_ind)%z_grz**2) )
             else
                graze_rate = c0
             end if

          end select

          !-----------------------------------------------------------------------
          !  autotroph prey
          !-----------------------------------------------------------------------

          do auto_ind2 = 1, grazing(prey_ind, pred_ind)%auto_ind_cnt
             auto_ind = grazing(prey_ind, pred_ind)%auto_ind(auto_ind2)

             ! scale by biomass from autotroph pool
             if (work1 > c0) then
                work2 = (Pprime(auto_ind) / work1) * graze_rate ! total grazing loss from auto_ind
             else
                work2 = c0
             end if
             auto_graze(auto_ind) = auto_graze(auto_ind) + work2

             ! routed to zooplankton
             auto_graze_zoo(auto_ind) = auto_graze_zoo(auto_ind) + grazing(prey_ind, pred_ind)%graze_zoo * work2
             x_graze_zoo(pred_ind)    = x_graze_zoo(pred_ind)    + grazing(prey_ind, pred_ind)%graze_zoo * work2

             ! routed to POC
             if (auto_meta(auto_ind)%imp_calcifier) then
                auto_graze_poc(auto_ind) = auto_graze_poc(auto_ind) &
                     + work2 * max((caco3_poc_min * QCaCO3(auto_ind)),  &
                     min(spc_poc_fac * (Pprime(auto_ind)+0.5_r8)**1.5_r8,    &
                     f_graze_sp_poc_lim))
             else
                auto_graze_poc(auto_ind) = auto_graze_poc(auto_ind) + grazing(prey_ind, pred_ind)%graze_poc * work2
             endif

             ! routed to DOC
             auto_graze_doc(auto_ind) = auto_graze_doc(auto_ind) + grazing(prey_ind, pred_ind)%graze_doc * work2

             !  get fractional factor for routing of zoo losses, based on food supply
             work3 = work3 + grazing(prey_ind, pred_ind)%f_zoo_detr * (work2 + epsC * epsTinv)
             work4 = work4 + (work2 + epsC * epsTinv)

          end do

          !-----------------------------------------------------------------------
          !  Zooplankton prey
          !-----------------------------------------------------------------------
          do zoo_ind2 = 1, grazing(prey_ind, pred_ind)%zoo_ind_cnt
             zoo_ind = grazing(prey_ind, pred_ind)%zoo_ind(zoo_ind2)

             ! scale by biomass from zooplankton pool
             if (work1 > c0) then
                work2 = (Zprime(zoo_ind) / work1) * graze_rate
             else
                work2 = c0
             end if

             ! grazing loss from zooplankton prey pool
             zoo_graze(zoo_ind) = zoo_graze(zoo_ind) + work2

             ! routed to zooplankton
             zoo_graze_zoo(zoo_ind) = zoo_graze_zoo(zoo_ind) + grazing(prey_ind, pred_ind)%graze_zoo * work2
             x_graze_zoo(pred_ind) = x_graze_zoo(pred_ind)   + grazing(prey_ind, pred_ind)%graze_zoo * work2

             ! routed to POC/DOC
             zoo_graze_poc(zoo_ind) = zoo_graze_poc(zoo_ind) + grazing(prey_ind, pred_ind)%graze_poc * work2
             zoo_graze_doc(zoo_ind) = zoo_graze_doc(zoo_ind) + grazing(prey_ind, pred_ind)%graze_doc * work2

             !  get fractional factor for routing of zoo losses, based on food supply
             work3 = work3 + grazing(prey_ind, pred_ind)%f_zoo_detr * (work2 + epsC * epsTinv)
             work4 = work4 + (work2 + epsC * epsTinv)

          end do
       end do

       f_zoo_detr(pred_ind) = work3 / work4
    end do

    end associate

  end subroutine marbl_compute_grazing

  !***********************************************************************

  subroutine marbl_compute_routing (auto_cnt, zoo_cnt,  auto_meta, &
       zooplankton_secondary_species, autotroph_secondary_species)

    use marbl_parms     , only : c1
    use marbl_parms     , only : Qp_zoo_pom
    use marbl_parms     , only : parm_labile_ratio

    integer(int_kind)                        , intent(in)    :: auto_cnt
    integer(int_kind)                        , intent(in)    :: zoo_cnt
    type(autotroph_type)                     , intent(in)    :: auto_meta(auto_cnt)
    type(zooplankton_secondary_species_type) , intent(inout) :: zooplankton_secondary_species(zoo_cnt)
    type(autotroph_secondary_species_type)   , intent(inout) :: autotroph_secondary_species(auto_cnt)

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind, zoo_ind
    real(r8) :: remaining_P      ! used in routing P from autotrophs w/ Qp different from Qp_zoo_pom
    !-----------------------------------------------------------------------

    associate(                                                               &
         auto_graze      => autotroph_secondary_species(:)%auto_graze      , & ! input
         auto_graze_zoo  => autotroph_secondary_species(:)%auto_graze_zoo  , & ! input
         auto_graze_poc  => autotroph_secondary_species(:)%auto_graze_poc  , & ! input
         auto_graze_doc  => autotroph_secondary_species(:)%auto_graze_doc  , & ! input
         auto_loss       => autotroph_secondary_species(:)%auto_loss       , & ! input
         auto_loss_poc   => autotroph_secondary_species(:)%auto_loss_poc   , & ! input
         auto_agg        => autotroph_secondary_species(:)%auto_agg        , & ! input

         zoo_graze       => zooplankton_secondary_species(:)%zoo_graze     , & ! input
         zoo_graze_poc   => zooplankton_secondary_species(:)%zoo_graze_poc , & ! input
         zoo_graze_doc   => zooplankton_secondary_species(:)%zoo_graze_doc , & ! input
         zoo_graze_zoo   => zooplankton_secondary_species(:)%zoo_graze_zoo , & ! input
         zoo_loss        => zooplankton_secondary_species(:)%zoo_loss      , & ! input
         f_zoo_detr      => zooplankton_secondary_species(:)%f_zoo_detr    , & ! input

         auto_graze_dic  => autotroph_secondary_species(:)%auto_graze_dic  , & ! output
         remaining_P_dop => autotroph_secondary_species(:)%remaining_P_dop , & ! output
         remaining_P_dip => autotroph_secondary_species(:)%remaining_P_dip , & ! output

         zoo_graze_dic   => zooplankton_secondary_species(:)%zoo_graze_dic , & ! output
         zoo_loss_poc    => zooplankton_secondary_species(:)%zoo_loss_poc  , & ! output
         zoo_loss_doc    => zooplankton_secondary_species(:)%zoo_loss_doc  , & ! output
         zoo_loss_dic    => zooplankton_secondary_species(:)%zoo_loss_dic    & ! output
         )

    !-----------------------------------------------------------------------
    ! compute routing to dic of grazed material
    ! call this and the one below compute_routing
    !-----------------------------------------------------------------------
    do auto_ind = 1, auto_cnt
       auto_graze_dic(auto_ind) = auto_graze(auto_ind) &
            - (auto_graze_zoo(auto_ind) + auto_graze_poc(auto_ind) + auto_graze_doc(auto_ind))
    end do
    do zoo_ind = 1, zoo_cnt
       zoo_graze_dic(zoo_ind) = zoo_graze(zoo_ind)  &
            - (zoo_graze_zoo(zoo_ind) + zoo_graze_poc(zoo_ind) + zoo_graze_doc(zoo_ind))
    end do

    !-----------------------------------------------------------------------
    ! compute zooplankton loss routing
    ! call this compute_routing_zooplankton_loss
    !-----------------------------------------------------------------------
    do zoo_ind = 1, zoo_cnt
       zoo_loss_poc(zoo_ind) = f_zoo_detr(zoo_ind) * zoo_loss(zoo_ind)
       zoo_loss_doc(zoo_ind) = (c1 - parm_labile_ratio) * (c1 - f_zoo_detr(zoo_ind)) * zoo_loss(zoo_ind)
       zoo_loss_dic(zoo_ind) = parm_labile_ratio * (c1 - f_zoo_detr(zoo_ind)) * zoo_loss(zoo_ind)
    end do

    !-----------------------------------------------------------------------
    ! P from some autotrophs w/ Qp different from Qp_zoo_pom must be routed differently than other
    ! elements to ensure that sinking detritus and zooplankton pools get their fixed P/C ratios.
    ! The remaining P is split evenly between DOP and PO4.
    !-----------------------------------------------------------------------
    do auto_ind = 1, auto_cnt
       if (auto_meta(auto_ind)%Qp /= Qp_zoo_pom) then
          remaining_P = ((auto_graze(auto_ind) + auto_loss(auto_ind) + auto_agg(auto_ind)) * auto_meta(auto_ind)%Qp) &
               - ((auto_graze_zoo(auto_ind)) * Qp_zoo_pom) &
               - ((auto_graze_poc(auto_ind) + auto_loss_poc(auto_ind) + auto_agg(auto_ind)) * Qp_zoo_pom)
          remaining_P_dop(auto_ind) = (c1 - parm_labile_ratio) * remaining_P
          remaining_P_dip(auto_ind) = parm_labile_ratio * remaining_P
       endif
    end do

    end associate

  end subroutine marbl_compute_routing

  !***********************************************************************

  subroutine marbl_compute_dissolved_organic_matter (k, auto_cnt, zoo_cnt,    &
             PAR_nsubcols, auto_meta, zooplankton_secondary_species,          &
             autotroph_secondary_species, PAR_col_frac, PAR_in, PAR_avg,      &
             dz1, tracer_local, marbl_tracer_indices, dissolved_organic_matter)

    use marbl_parms     , only : Qfe_zoo
    use marbl_parms     , only : Qp_zoo_pom
    use marbl_parms     , only : Q

    use marbl_parms     , only : DOC_reminR_light
    use marbl_parms     , only : DON_reminR_light
    use marbl_parms     , only : DOP_reminR_light
    use marbl_parms     , only : DOC_reminR_dark
    use marbl_parms     , only : DON_reminR_dark
    use marbl_parms     , only : DOP_reminR_dark

    use marbl_parms     , only : DOCr_reminR0
    use marbl_parms     , only : DONr_reminR0
    use marbl_parms     , only : DOPr_reminR0
    use marbl_parms     , only : DOMr_reminR_photo

    integer(int_kind)                       , intent(in)  :: k
    integer                                 , intent(in)  :: auto_cnt
    integer                                 , intent(in)  :: zoo_cnt
    integer(int_kind)                       , intent(in)  :: PAR_nsubcols
    type(autotroph_type)                    , intent(in)  :: auto_meta(auto_cnt)
    type(zooplankton_secondary_species_type), intent(in)  :: zooplankton_secondary_species(zoo_cnt)
    type(autotroph_secondary_species_type)  , intent(in)  :: autotroph_secondary_species(auto_cnt)
    real(r8)                                , intent(in)  :: PAR_col_frac(PAR_nsubcols)
    real(r8)                                , intent(in)  :: PAR_in(PAR_nsubcols)
    real(r8)                                , intent(in)  :: PAR_avg(PAR_nsubcols)
    real(r8)                                , intent(in)  :: dz1
    real(r8)                                , intent(in)  :: tracer_local(ecosys_base_tracer_cnt)
    type(marbl_tracer_index_type)           , intent(in)  :: marbl_tracer_indices
    type(dissolved_organic_matter_type)     , intent(out) :: dissolved_organic_matter

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind, subcol_ind
    real(r8) :: work
    real(r8) :: DOC_reminR        ! remineralization rate (1/sec)
    real(r8) :: DON_reminR        ! remineralization rate (1/sec)
    real(r8) :: DOP_reminR        ! remineralization rate (1/sec)
    real(r8) :: DOCr_reminR       ! remineralization rate (1/sec)
    real(r8) :: DONr_reminR       ! remineralization rate (1/sec)
    real(r8) :: DOPr_reminR       ! remineralization rate (1/sec)
    !-----------------------------------------------------------------------

    associate(                                                               &
         DOC_loc         => tracer_local(marbl_tracer_indices%doc_ind)     , &
         DON_loc         => tracer_local(marbl_tracer_indices%don_ind)     , &
         DOP_loc         => tracer_local(marbl_tracer_indices%dop_ind)     , &
         DONr_loc        => tracer_local(marbl_tracer_indices%donr_ind)    , &
         DOPr_loc        => tracer_local(marbl_tracer_indices%dopr_ind)    , &
         DOCr_loc        => tracer_local(marbl_tracer_indices%docr_ind)    , &
         Qfe             => autotroph_secondary_species(:)%Qfe             , & ! input
         remaining_P_dop => autotroph_secondary_species(:)%remaining_P_dop , & ! input
         auto_loss_doc   => autotroph_secondary_species(:)%auto_loss_doc   , & ! input
         auto_graze_doc  => autotroph_secondary_species(:)%auto_graze_doc  , & ! input
         zoo_loss_doc    => zooplankton_secondary_species(:)%zoo_loss_doc  , & ! input
         zoo_graze_doc   => zooplankton_secondary_species(:)%zoo_graze_doc , & ! input
         DOC_prod        => dissolved_organic_matter%DOC_prod              , & ! output production of DOC (mmol C/m^3/sec)
         DOC_remin       => dissolved_organic_matter%DOC_remin             , & ! output remineralization of DOC (mmol C/m^3/sec)
         DOCr_remin      => dissolved_organic_matter%DOCr_remin            , & ! output remineralization of DOCr
         DON_prod        => dissolved_organic_matter%DON_prod              , & ! output production of DON
         DON_remin       => dissolved_organic_matter%DON_remin             , & ! output remineralization of DON
         DONr_remin      => dissolved_organic_matter%DONr_remin            , & ! output remineralization of DONr
         DOP_prod        => dissolved_organic_matter%DOP_prod              , & ! output production of DOP
         DOP_remin       => dissolved_organic_matter%DOP_remin             , & ! output remineralization of DOP
         DOPr_remin      => dissolved_organic_matter%DOPr_remin              & ! output remineralization of DOPr
         )

    !-----------------------------------------------------------------------
      !  compute terms for DOM
      !-----------------------------------------------------------------------

      DOC_prod = sum(zoo_loss_doc(:)) + sum(auto_loss_doc(:)) + sum(auto_graze_doc(:)) + sum(zoo_graze_doc(:))
      DON_prod = Q * DOC_prod * f_toDON
      DOP_prod = Qp_zoo_pom * ( sum(zoo_loss_doc(:)) + sum(zoo_graze_doc(:)) )
      do auto_ind = 1, auto_cnt
         if (auto_meta(auto_ind)%Qp == Qp_zoo_pom) then
            DOP_prod = DOP_prod + auto_meta(auto_ind)%Qp * (auto_loss_doc(auto_ind) + auto_graze_doc(auto_ind))
         else
            DOP_prod = DOP_prod + remaining_P_dop(auto_ind)
         endif
      end do

      !-----------------------------------------------------------------------
      !  Different remin rates in light and dark for semi-labile pools
      !-----------------------------------------------------------------------

      DOC_reminR = c0
      DON_reminR = c0
      DOP_reminR = c0

      do subcol_ind = 1, PAR_nsubcols
         if (PAR_col_frac(subcol_ind) > c0) then
            if (PAR_avg(subcol_ind) > 1.0_r8) then
               DOC_reminR = DOC_reminR + PAR_col_frac(subcol_ind) * DOC_reminR_light
               DON_reminR = DON_reminR + PAR_col_frac(subcol_ind) * DON_reminR_light
               DOP_reminR = DOP_reminR + PAR_col_frac(subcol_ind) * DOP_reminR_light
            else
               DOC_reminR = DOC_reminR + PAR_col_frac(subcol_ind) * DOC_reminR_dark
               DON_reminR = DON_reminR + PAR_col_frac(subcol_ind) * DON_reminR_dark
               DOP_reminR = DOP_reminR + PAR_col_frac(subcol_ind) * DOP_reminR_dark
            endif
         endif
      end do

      !-----------------------------------------------------------------------
      !  Refractory remin increased in top layer from photodegradation due to UV
      !-----------------------------------------------------------------------

      DOCr_reminR = DOCr_reminR0
      DONr_reminR = DONr_reminR0
      DOPr_reminR = DOPr_reminR0

      if (k == 1) then
         do subcol_ind = 1, PAR_nsubcols
            if ((PAR_col_frac(subcol_ind) > c0) .and. (PAR_in(subcol_ind) > 1.0_r8)) then
               work = PAR_col_frac(subcol_ind) * (log(PAR_in(subcol_ind))*0.4373_r8) * (10.0e2/dz1)
               DOCr_reminR = DOCr_reminR + work * DOMr_reminR_photo
               DONr_reminR = DONr_reminR + work * DOMr_reminR_photo
               DOPr_reminR = DOPr_reminR + work * DOMr_reminR_photo
            endif
         end do
      endif

      DOC_remin  = DOC_loc  * DOC_reminR
      DON_remin  = DON_loc  * DON_reminR
      DOP_remin  = DOP_loc  * DOP_reminR
      DOCr_remin = DOCr_loc * DOCr_reminR
      DONr_remin = DONr_loc * DONr_reminR
      DOPr_remin = DOPr_loc * DOPr_reminR

    end associate

  end subroutine marbl_compute_dissolved_organic_matter

  !***********************************************************************

  subroutine marbl_compute_large_detritus(k, auto_cnt, zoo_cnt, auto_meta, &
       zooplankton_secondary_species, autotroph_secondary_species, Fe_loc, &
       POC, P_CaCO3, P_SiO2, dust, P_iron, Fe_scavenge, Fe_scavenge_rate,  &
       marbl_tracer_indices)

    use marbl_parms     , only : f_graze_CaCO3_remin
    use marbl_parms     , only : f_graze_si_remin
    use marbl_parms     , only : Qfe_zoo
    use marbl_parms     , only : parm_Fe_scavenge_rate0
    use marbl_parms     , only : dust_fescav_scale
    use marbl_parms     , only : Fe_scavenge_thres1
    use marbl_parms     , only : fe_max_scale2
    use marbl_parms     , only : yps

    ! Note (mvertens, 2016-02), all the column_sinking_partiles must be intent(inout)
    ! rather than intent(out), since if they were intent(out) they would be automatically 
    ! deallocated on entry in this routine (this is not required behavior - but is
    ! standard)

    integer                                  , intent(in)    :: k
    integer                                  , intent(in)    :: auto_cnt
    integer                                  , intent(in)    :: zoo_cnt
    type(autotroph_type)                     , intent(in)    :: auto_meta(auto_cnt)
    type(zooplankton_secondary_species_type) , intent(in)    :: zooplankton_secondary_species(zoo_cnt)
    type(autotroph_secondary_species_type)   , intent(in)    :: autotroph_secondary_species(auto_cnt)
    real(r8)                                 , intent(in)    :: Fe_loc
    type(column_sinking_particle_type)       , intent(inout) :: POC
    type(column_sinking_particle_type)       , intent(inout) :: P_CaCO3
    type(column_sinking_particle_type)       , intent(inout) :: P_SiO2
    type(column_sinking_particle_type)       , intent(inout) :: dust
    type(column_sinking_particle_type)       , intent(inout) :: P_iron
    real(r8)                                 , intent(out)   :: Fe_scavenge
    real(r8)                                 , intent(out)   :: Fe_scavenge_rate
    type(marbl_tracer_index_type)            , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real(r8) :: work
    integer :: auto_ind
    !-----------------------------------------------------------------------

    associate(                                                             &
         QCaCO3         => autotroph_secondary_species(:)%QCaCO3         , & ! input
         Qsi            => autotroph_secondary_species(:)%Qsi            , & ! input
         Qfe            => autotroph_secondary_species(:)%Qfe            , & ! input
         auto_graze     => autotroph_secondary_species(:)%auto_graze     , & ! input
         auto_graze_poc => autotroph_secondary_species(:)%auto_graze_poc , & ! input
         auto_agg       => autotroph_secondary_species(:)%auto_agg       , & ! input
         auto_loss      => autotroph_secondary_species(:)%auto_loss      , & ! input
         auto_loss_poc  => autotroph_secondary_species(:)%auto_loss_poc  , & ! input
         zoo_loss_poc   => zooplankton_secondary_species(:)%zoo_loss_poc , & ! input
         zoo_graze_poc  => zooplankton_secondary_species(:)%zoo_graze_poc  & ! input
         )

    !-----------------------------------------------------------------------
    !  large detritus C
    !-----------------------------------------------------------------------

    POC%prod(k) = sum(zoo_loss_poc(:)) + sum(auto_graze_poc(:)) + sum(zoo_graze_poc(:)) &
         + sum(auto_agg(:)) + sum(auto_loss_poc(:))

    !-----------------------------------------------------------------------
    !  large detrital CaCO3
    !  33% of CaCO3 is remin when phyto are grazed
    !-----------------------------------------------------------------------

    do auto_ind = 1, auto_cnt
       if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
          P_CaCO3%prod(k) = ((c1 - f_graze_CaCO3_remin) * auto_graze(auto_ind) + &
               auto_loss(auto_ind) + auto_agg(auto_ind)) * QCaCO3(auto_ind)
       endif
    end do

    !-----------------------------------------------------------------------
    !  large detritus SiO2
    !  grazed diatom SiO2, 60% is remineralized
    !-----------------------------------------------------------------------

    do auto_ind = 1, auto_cnt
       if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
          P_SiO2%prod(k) = Qsi(auto_ind) * ((c1 - f_graze_si_remin) * auto_graze(auto_ind) + auto_agg(auto_ind) &
               + auto_meta(auto_ind)%loss_poc * auto_loss(auto_ind))
       endif
    end do

    !-----------------------------------------------------------------------
    ! Dust
    !-----------------------------------------------------------------------
    dust%prod(k) = c0

    !-----------------------------------------------------------------------
    !  Compute iron scavenging :
    !  1) compute in terms of loss per year per unit iron (%/year/fe)
    !  2) scale by sinking mass flux (POMx4 + Dust + bSi + CaCO3)
    !  3) increase scavenging at higher iron concentrations (>0.8nM), reduce low (< 0.3nM)
    !-----------------------------------------------------------------------

    Fe_scavenge_rate = parm_Fe_scavenge_rate0

    Fe_scavenge_rate = Fe_scavenge_rate * &
         ((POC%sflux_out(k)     + POC%hflux_out(k)    ) * 4.0_r8*12.01_r8 + &
          (P_CaCO3%sflux_out(k) + P_CaCO3%hflux_out(k)) * P_CaCO3%mass + &
          (P_SiO2%sflux_out(k)  + P_SiO2%hflux_out(k) ) * P_SiO2%mass + &
          (dust%sflux_out(k)    + dust%hflux_out(k)   ) * dust_fescav_scale)

    if (Fe_loc < 0.3e-3_r8) then
       Fe_scavenge_rate = Fe_scavenge_rate * (Fe_loc / 0.3e-3_r8)
    end if

    if (Fe_loc > Fe_scavenge_thres1) then
       Fe_scavenge_rate = Fe_scavenge_rate + (Fe_loc - Fe_scavenge_thres1) * fe_max_scale2
    end if

    Fe_scavenge = yps * Fe_loc * Fe_scavenge_rate

    P_iron%prod(k) = (sum(zoo_loss_poc(:)) + sum(zoo_graze_poc(:))) * Qfe_zoo + Fe_scavenge

    do auto_ind = 1, autotroph_cnt
       P_iron%prod(k) = P_iron%prod(k) + Qfe(auto_ind) * &
            (auto_agg(auto_ind) + auto_graze_poc(auto_ind) + auto_loss_poc(auto_ind))
    end do

    end associate
  end subroutine marbl_compute_large_detritus

  !***********************************************************************

  subroutine marbl_compute_nitrif(k, PAR_nsubcols, column_kmt,                &
             PAR_col_frac, PAR_in, PAR_out, KPARdz, NH4_loc, nitrif)

    !-----------------------------------------------------------------------
    !  nitrate & ammonium
    !  nitrification in low light
    !  use exponential decay of PAR across model level to compute taper factor
    !-----------------------------------------------------------------------

    use marbl_parms, only : parm_nitrif_par_lim
    use marbl_parms, only : parm_kappa_nitrif

    integer(int_kind) , intent(in)  :: k
    integer(int_kind) , intent(in)  :: PAR_nsubcols
    integer(int_kind) , intent(in)  :: column_kmt
    real(r8)          , intent(in)  :: PAR_col_frac(PAR_nsubcols)
    real(r8)          , intent(in)  :: PAR_in(PAR_nsubcols)
    real(r8)          , intent(in)  :: PAR_out(PAR_nsubcols)
    real(r8)          , intent(in)  :: kPARdz
    real(r8)          , intent(in)  :: NH4_loc
    real(r8)          , intent(out) :: nitrif

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer(int_kind) :: subcol_ind
    real(r8) :: nitrif_subcol
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    ! skip computations for non-active layers or NH4 is 0
    !-----------------------------------------------------------------------

    nitrif = c0

    if ((k > column_kmt) .or. (NH4_loc == c0)) return

    do subcol_ind = 1, PAR_nsubcols
       if (PAR_col_frac(subcol_ind) > c0) then
          if (PAR_out(subcol_ind) < parm_nitrif_par_lim) then
             nitrif_subcol = parm_kappa_nitrif * NH4_loc
             if (PAR_in(subcol_ind) > parm_nitrif_par_lim) then
                nitrif_subcol = nitrif_subcol * &
                   log(PAR_out(subcol_ind) / parm_nitrif_par_lim) / (-KPARdz)
             end if
             nitrif = nitrif + PAR_col_frac(subcol_ind) * nitrif_subcol
          end if
       end if
    end do

  end subroutine marbl_compute_nitrif

  !***********************************************************************

  subroutine marbl_compute_denitrif(O2_loc, NO3_loc, DOC_remin, DOCr_remin,   &
             POC_remin, other_remin, sed_denitrif, denitrif)

    !-----------------------------------------------------------------------
    !  Compute denitrification under low O2 conditions
    !-----------------------------------------------------------------------

    real(r8) , intent(in)    :: O2_loc
    real(r8) , intent(in)    :: NO3_loc
    real(r8) , intent(in)    :: DOC_remin
    real(r8) , intent(in)    :: DOCr_remin
    real(r8) , intent(in)    :: POC_remin
    real(r8) , intent(in)    :: OTHER_REMIN
    real(r8) , intent(inout) :: SED_DENITRIF
    real(r8) , intent(out)   :: denitrif

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real(r8) :: work
    !-----------------------------------------------------------------------

    work = ((parm_o2_min + parm_o2_min_delta) - O2_loc) / parm_o2_min_delta
    work = min(max(work, c0), c1)
    denitrif = work * ((DOC_remin + DOCr_remin + POC_remin * (c1 - POCremin_refract) &
         - other_remin) / denitrif_C_N  - sed_denitrif)

    ! scale down denitrif if computed rate would consume all NO3 in 10 days
    if (NO3_loc < ((c10*spd)*(denitrif+sed_denitrif))) then
       work = NO3_loc / ((c10*spd)*(denitrif+sed_denitrif))
       denitrif = denitrif * work
       sed_denitrif = sed_denitrif * work
    endif

  end subroutine marbl_compute_denitrif

  !***********************************************************************

  subroutine marbl_compute_dtracer_local (auto_cnt, zoo_cnt, auto_meta, zoo_meta, &
       autotroph_secondary_species, &
       zooplankton_secondary_species, &
       dissolved_organic_matter, &
       nitrif, denitrif, sed_denitrif, &
       Fe_scavenge, Fe_scavenge_rate, &
       P_iron_remin, POC_remin, &
       P_SiO2_remin, P_CaCO3_remin, other_remin, PON_remin, POP_remin, &
       interior_restore, &
       O2_loc, o2_production, o2_consumption, &
       dtracers, marbl_tracer_indices)

    integer                                  , intent(in)  :: auto_cnt
    integer                                  , intent(in)  :: zoo_cnt
    type(autotroph_type)                     , intent(in)  :: auto_meta(auto_cnt)
    type(zooplankton_type)                   , intent(in)  :: zoo_meta(zoo_cnt)
    type(zooplankton_secondary_species_type) , intent(in)  :: zooplankton_secondary_species(zoo_cnt)
    type(autotroph_secondary_species_type)   , intent(in)  :: autotroph_secondary_species(auto_cnt)
    type(dissolved_organic_matter_type)      , intent(in)  :: dissolved_organic_matter
    real(r8)                                 , intent(in)  :: nitrif
    real(r8)                                 , intent(in)  :: denitrif
    real(r8)                                 , intent(in)  :: sed_denitrif
    real(r8)                                 , intent(in)  :: Fe_scavenge
    real(r8)                                 , intent(in)  :: Fe_scavenge_rate
    real(r8)                                 , intent(in)  :: P_iron_remin
    real(r8)                                 , intent(in)  :: POC_remin
    real(r8)                                 , intent(in)  :: P_SiO2_remin
    real(r8)                                 , intent(in)  :: P_CaCO3_remin
    real(r8)                                 , intent(in)  :: other_remin
    real(r8)                                 , intent(in)  :: PON_remin
    real(r8)                                 , intent(in)  :: POP_remin
    real(r8)                                 , intent(in)  :: interior_restore(ecosys_base_tracer_cnt)
    real(r8)                                 , intent(in)  :: O2_loc
    real(r8)                                 , intent(out) :: o2_production
    real(r8)                                 , intent(out) :: o2_consumption
    real(r8)                                 , intent(out) :: dtracers(ecosys_base_tracer_cnt)
    type(marbl_tracer_index_type)            , intent(in)  :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer  :: auto_ind, zoo_ind, n
    real(r8) :: auto_sum
    !-----------------------------------------------------------------------

    associate(                                                            &
         thetaC          => autotroph_secondary_species(:)%thetaC          , & ! local Chl/C ratio (mg Chl/mmol C)
         QCaCO3          => autotroph_secondary_species(:)%QCaCO3          , & ! CaCO3/C ratio (mmol CaCO3/mmol C)
         Qfe             => autotroph_secondary_species(:)%Qfe             , & ! init fe/C ratio (mmolFe/mmolC)
         Qsi             => autotroph_secondary_species(:)%Qsi             , & ! initial Si/C ratio (mmol Si/mmol C)
         NO3_V           => autotroph_secondary_species(:)%NO3_V           , & ! nitrate uptake (mmol NO3/m^3/sec)
         NH4_V           => autotroph_secondary_species(:)%NH4_V           , & ! ammonium uptake (mmol NH4/m^3/sec)
         PO4_V           => autotroph_secondary_species(:)%PO4_V           , & ! PO4 uptake (mmol PO4/m^3/sec)
         DOP_V           => autotroph_secondary_species(:)%DOP_V           , & ! DOP uptake (mmol DOP/m^3/sec)
         photoC          => autotroph_secondary_species(:)%photoC          , & ! C-fixation (mmol C/m^3/sec)
         photoFe         => autotroph_secondary_species(:)%photoFe         , & ! iron uptake
         photoSi         => autotroph_secondary_species(:)%photoSi         , & ! silicon uptake (mmol Si/m^3/sec)
         photoacc        => autotroph_secondary_species(:)%photoacc        , & ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
         auto_loss       => autotroph_secondary_species(:)%auto_loss       , & ! autotroph non-grazing mort (mmol C/m^3/sec)
         auto_loss_dic   => autotroph_secondary_species(:)%auto_loss_dic   , & ! auto_loss routed to dic (mmol C/m^3/sec)
         auto_loss_doc   => autotroph_secondary_species(:)%auto_loss_doc   , & ! auto_loss routed to doc (mmol C/m^3/sec)
         auto_agg        => autotroph_secondary_species(:)%auto_agg        , & ! autotroph aggregation (mmol C/m^3/sec)
         auto_graze      => autotroph_secondary_species(:)%auto_graze      , & ! autotroph grazing rate (mmol C/m^3/sec)
         auto_graze_zoo  => autotroph_secondary_species(:)%auto_graze_zoo  , & ! auto_graze routed to zoo (mmol C/m^3/sec)
         auto_graze_dic  => autotroph_secondary_species(:)%auto_graze_dic  , & ! auto_graze routed to dic (mmol C/m^3/sec)
         auto_graze_doc  => autotroph_secondary_species(:)%auto_graze_doc  , & ! auto_graze routed to doc (mmol C/m^3/sec)
         CaCO3_PROD      => autotroph_secondary_species(:)%CaCO3_PROD      , & ! prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
         Nfix            => autotroph_secondary_species(:)%Nfix            , & ! total Nitrogen fixation (mmol N/m^3/sec)
         Nexcrete        => autotroph_secondary_species(:)%Nexcrete        , & ! fixed N excretion
         remaining_P_dip => autotroph_secondary_species(:)%remaining_P_dip , & ! remaining_P from mort routed to remin

         f_zoo_detr      => zooplankton_secondary_species(:)%f_zoo_detr    , & ! frac of zoo losses into large detrital pool (non-dim)
         x_graze_zoo     => zooplankton_secondary_species(:)%x_graze_zoo   , & ! {auto, zoo}_graze routed to zoo (mmol C/m^3/sec)
         zoo_graze       => zooplankton_secondary_species(:)%zoo_graze     , & ! zooplankton losses due to grazing (mmol C/m^3/sec)
         zoo_graze_zoo   => zooplankton_secondary_species(:)%zoo_graze_zoo , & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
         zoo_graze_dic   => zooplankton_secondary_species(:)%zoo_graze_dic , & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
         zoo_graze_doc   => zooplankton_secondary_species(:)%zoo_graze_doc , & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
         zoo_loss        => zooplankton_secondary_species(:)%zoo_loss      , & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
         zoo_loss_dic    => zooplankton_secondary_species(:)%zoo_loss_dic  , & ! zoo_loss routed to dic (mmol C/m^3/sec)
         zoo_loss_doc    => zooplankton_secondary_species(:)%zoo_loss_doc  , & ! zoo_loss routed to doc (mmol C/m^3/sec)

         DOC_prod        => dissolved_organic_matter%DOC_prod        , & ! production of DOC (mmol C/m^3/sec)
         DOC_remin       => dissolved_organic_matter%DOC_remin       , & ! remineralization of DOC (mmol C/m^3/sec)
         DOCr_remin      => dissolved_organic_matter%DOCr_remin      , & ! remineralization of DOCr
         DON_prod        => dissolved_organic_matter%DON_prod        , & ! production of DON
         DON_remin       => dissolved_organic_matter%DON_remin       , & ! remineralization of DON
         DONr_remin      => dissolved_organic_matter%DONr_remin      , & ! remineralization of DONr
         DOP_prod        => dissolved_organic_matter%DOP_prod        , & ! production of DOP
         DOP_remin       => dissolved_organic_matter%DOP_remin       , & ! remineralization of DOP
         DOPr_remin      => dissolved_organic_matter%DOPr_remin      , & ! remineralization of DOPr

         po4_ind           => marbl_tracer_indices%po4_ind,         &
         no3_ind           => marbl_tracer_indices%no3_ind,         &
         sio3_ind          => marbl_tracer_indices%sio3_ind,        &
         nh4_ind           => marbl_tracer_indices%nh4_ind,         &
         fe_ind            => marbl_tracer_indices%fe_ind,          &
         o2_ind            => marbl_tracer_indices%o2_ind,          &
         dic_ind           => marbl_tracer_indices%dic_ind,         &
         dic_alt_co2_ind   => marbl_tracer_indices%dic_alt_co2_ind, &
         alk_ind           => marbl_tracer_indices%alk_ind,         &
         doc_ind           => marbl_tracer_indices%doc_ind,         &
         don_ind           => marbl_tracer_indices%don_ind,         &
         dop_ind           => marbl_tracer_indices%dop_ind,         &
         dopr_ind          => marbl_tracer_indices%dopr_ind,        &
         donr_ind          => marbl_tracer_indices%donr_ind,        &
         docr_ind          => marbl_tracer_indices%docr_ind         &
         )

    !-----------------------------------------------------------------------
    !  nitrate & ammonium
    !-----------------------------------------------------------------------

    dtracers(no3_ind) = interior_restore(no3_ind) + nitrif - denitrif - sed_denitrif - sum(NO3_V(:))

    dtracers(nh4_ind) = -sum(NH4_V(:)) - nitrif + DON_remin + DONr_remin  &
         + Q * (sum(zoo_loss_dic(:)) + sum(zoo_graze_dic(:)) + sum(auto_loss_dic(:)) + sum(auto_graze_dic(:)) &
                + DOC_prod*(c1 - f_toDON)) &
         + PON_remin * (c1 - PONremin_refract)

    do auto_ind = 1, auto_cnt
       if (auto_meta(auto_ind)%Nfixer) then
          dtracers(nh4_ind) = dtracers(nh4_ind) + Nexcrete(auto_ind)
       end if
    end do

    !-----------------------------------------------------------------------
    !  dissolved iron
    !-----------------------------------------------------------------------

    dtracers(fe_ind) = P_iron_remin - sum(photofe(:)) - Fe_scavenge &
       + Qfe_zoo * ( sum(zoo_loss_dic(:)) + sum(zoo_loss_doc(:)) + sum(zoo_graze_dic(:)) + sum(zoo_graze_doc(:)) )

    do auto_ind = 1, autotroph_cnt
       dtracers(fe_ind) = dtracers(fe_ind) &
            + (Qfe(auto_ind) * (auto_loss_dic(auto_ind) + auto_graze_dic(auto_ind))) &
            + auto_graze_zoo(auto_ind) * (Qfe(auto_ind) - Qfe_zoo) &
            + (Qfe(auto_ind) * (auto_loss_doc(auto_ind) + auto_graze_doc(auto_ind)))
    end do

    !-----------------------------------------------------------------------
    !  dissolved SiO3
    !-----------------------------------------------------------------------

    dtracers(sio3_ind) = interior_restore(sio3_ind) + P_SiO2_remin

    do auto_ind = 1, auto_cnt
       if (marbl_tracer_indices%auto_inds(auto_ind)%Si_ind > 0) then
          dtracers(sio3_ind) = dtracers(sio3_ind) &
               - photoSi(auto_ind) + Qsi(auto_ind) * (f_graze_si_remin * auto_graze(auto_ind) &
               + (c1 - auto_meta(auto_ind)%loss_poc) * auto_loss(auto_ind))
       endif
    end do

    !-----------------------------------------------------------------------
    !  phosphate
    !-----------------------------------------------------------------------

    dtracers(po4_ind) = interior_restore(po4_ind) + DOP_remin + DOPr_remin - sum(PO4_V(:)) &
         + (c1 - POPremin_refract) * POP_remin + Qp_zoo_pom * ( sum(zoo_loss_dic(:)) + sum(zoo_graze_dic(:)) )

    do auto_ind = 1, autotroph_cnt
       if (auto_meta(auto_ind)%Qp == Qp_zoo_pom) then
          dtracers(po4_ind) = dtracers(po4_ind) &
               + auto_meta(auto_ind)%Qp * (auto_loss_dic(auto_ind) + auto_graze_dic(auto_ind))
       else
          dtracers(po4_ind) = dtracers(po4_ind) &
               + remaining_P_dip(auto_ind)
       endif
    end do

    !-----------------------------------------------------------------------
    !  zoo Carbon
    !-----------------------------------------------------------------------
    do zoo_ind = 1, zoo_cnt
       n = marbl_tracer_indices%zoo_inds(zoo_ind)%C_ind
       dtracers(n) = x_graze_zoo(zoo_ind) - zoo_graze(zoo_ind) - zoo_loss(zoo_ind)
    end do

    !-----------------------------------------------------------------------
    !  autotroph Carbon
    !  autotroph Chlorophyll
    !  autotroph Fe
    !  autotroph Si
    !  autotroph CaCO3
    !-----------------------------------------------------------------------

    do auto_ind = 1, auto_cnt
       auto_sum = auto_graze(auto_ind) + auto_loss(auto_ind) + auto_agg(auto_ind)

       n = marbl_tracer_indices%auto_inds(auto_ind)%C_ind
       dtracers(n) = photoC(auto_ind) - auto_sum

       n = marbl_tracer_indices%auto_inds(auto_ind)%Chl_ind
       dtracers(n) = photoacc(auto_ind) - thetaC(auto_ind) * auto_sum

       n = marbl_tracer_indices%auto_inds(auto_ind)%Fe_ind
       dtracers(n) =  photoFe(auto_ind) - Qfe(auto_ind) * auto_sum

       n = marbl_tracer_indices%auto_inds(auto_ind)%Si_ind
       if (n > 0) then
          dtracers(n) =  photoSi(auto_ind) - Qsi(auto_ind) * auto_sum
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind
       if (n > 0) then
          dtracers(n) = CaCO3_PROD(auto_ind) - QCaCO3(auto_ind) * auto_sum
       endif
    end do


    !-----------------------------------------------------------------------
    !  dissolved organic Matter
    !  from sinking remin small fraction to refractory pool
    !-----------------------------------------------------------------------

    dtracers(doc_ind) = DOC_prod * (c1 - DOCprod_refract) - DOC_remin

    dtracers(docr_ind) = DOC_prod * DOCprod_refract - DOCr_remin + (POC_remin * POCremin_refract)

    dtracers(don_ind) = (DON_prod * (c1 - DONprod_refract)) - DON_remin

    dtracers(donr_ind) = (DON_prod * DONprod_refract) - DONr_remin + (PON_remin * PONremin_refract)

    dtracers(dop_ind) = (DOP_prod * (c1 - DOPprod_refract)) - DOP_remin - sum(DOP_V(:))

    dtracers(dopr_ind) = (DOP_prod * DOPprod_refract) - DOPr_remin + (POP_remin * POPremin_refract)

    !-----------------------------------------------------------------------
    !  dissolved inorganic Carbon
    !-----------------------------------------------------------------------

    dtracers(dic_ind) = &
         sum(auto_loss_dic(:)) + sum(auto_graze_dic(:)) - sum(photoC(:)) &
            + DOC_remin + POC_remin * (c1 - POCremin_refract) + sum(zoo_loss_dic(:)) &
            + sum(zoo_graze_dic(:)) + P_CaCO3_remin + DOCr_remin

    do auto_ind = 1, auto_cnt
       if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
          dtracers(dic_ind) = dtracers(dic_ind) &
               + f_graze_CaCO3_REMIN * auto_graze(auto_ind) * QCaCO3(auto_ind) - CaCO3_PROD(auto_ind)
       end if
    end do

    dtracers(dic_alt_co2_ind) = dtracers(dic_ind)


    !-----------------------------------------------------------------------
    !  alkalinity
    !-----------------------------------------------------------------------

    dtracers(alk_ind) = -dtracers(no3_ind) + dtracers(nh4_ind) + c2 * P_CaCO3_remin

    do auto_ind = 1, auto_cnt
       if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
          dtracers(alk_ind) = dtracers(alk_ind) &
               + c2 * (f_graze_CaCO3_REMIN * auto_graze(auto_ind) * QCaCO3(auto_ind) - CaCO3_PROD(auto_ind))
       end if
    end do

    !-----------------------------------------------------------------------
    !  oxygen
    !-----------------------------------------------------------------------

    o2_production = c0
    do auto_ind = 1, auto_cnt
       if (.not. auto_meta(auto_ind)%Nfixer) then
          if (photoC(auto_ind) > c0) then
             o2_production = o2_production + photoC(auto_ind) * &
                  ((NO3_V(auto_ind) / (NO3_V(auto_ind) + NH4_V(auto_ind))) / parm_Red_D_C_O2 + &
                   (NH4_V(auto_ind) / (NO3_V(auto_ind) + NH4_V(auto_ind))) / parm_Remin_D_C_O2)
          end if
       else
          if (photoC(auto_ind) > c0) then
             o2_production = o2_production + photoC(auto_ind) * &
                  ((NO3_V(auto_ind) / (NO3_V(auto_ind) + NH4_V(auto_ind) + Nfix(auto_ind))) / parm_Red_D_C_O2 + &
                   (NH4_V(auto_ind) / (NO3_V(auto_ind) + NH4_V(auto_ind) + Nfix(auto_ind))) / parm_Remin_D_C_O2 + &
                   (Nfix(auto_ind)  / (NO3_V(auto_ind) + NH4_V(auto_ind) + Nfix(auto_ind))) / parm_Red_D_C_O2_diaz)
          end if
       endif
    end do

    o2_consumption = (O2_loc - parm_o2_min) / parm_o2_min_delta
    o2_consumption = min(max(o2_consumption, c0), c1)
    o2_consumption = o2_consumption * ( (POC_remin * (c1 - POCremin_refract) + DOC_remin &
         + DOCr_remin - (sed_denitrif * denitrif_C_N) - other_remin + sum(zoo_loss_dic(:)) &
         + sum(zoo_graze_dic(:)) + sum(auto_loss_dic(:)) + sum(auto_graze_dic(:)) ) &
         / parm_Remin_D_C_O2 + (c2 * nitrif))

    dtracers(o2_ind) = o2_production - o2_consumption

    end associate
  end subroutine marbl_compute_dtracer_local

  !-----------------------------------------------------------------------

  subroutine marbl_export_interior_shared_variables (&
       tracer_local, &
       marbl_tracer_indices, &
       carbonate, &
       dissolved_organic_matter, &
       QA_dust_def, &
       marbl_interior_share)

    real(r8)                            , intent(in)    :: tracer_local(ecosys_base_tracer_cnt)
    type(marbl_tracer_index_type)       , intent(in)    :: marbl_tracer_indices
    type(carbonate_type)                , intent(in)    :: carbonate
    type(dissolved_organic_matter_type) , intent(in)    :: dissolved_organic_matter
    real(r8)                            , intent(in)    :: QA_dust_def
    type(marbl_interior_share_type)     , intent(inout) :: marbl_interior_share

    associate( &
         share => marbl_interior_share &
         )

    share%QA_dust_def    = QA_dust_def
    share%DIC_loc_fields = tracer_local(marbl_tracer_indices%DIC_ind)
    share%DOC_loc_fields = tracer_local(marbl_tracer_indices%DOC_ind)
    share%O2_loc_fields  = tracer_local(marbl_tracer_indices%O2_ind)
    share%NO3_loc_fields = tracer_local(marbl_tracer_indices%NO3_ind)


    share%CO3_fields   = carbonate%CO3
    share%HCO3_fields  = carbonate%HCO3
    share%H2CO3_fields = carbonate%H2CO3

    share%DOC_remin_fields = dissolved_organic_matter%DOC_remin

    end associate
  end subroutine marbl_export_interior_shared_variables

  !-----------------------------------------------------------------------

  subroutine marbl_export_zooplankton_shared_variables (&
       zoo_cnt, &
       zooplankton_local, &
       zooplankton_secondary_species, &
       marbl_zooplankton_share)

    integer(int_kind)                        , intent(in)    :: zoo_cnt
    type(zooplankton_local_type)             , intent(in)    :: zooplankton_local(zoo_cnt)
    type(zooplankton_secondary_species_type) , intent(in)    :: zooplankton_secondary_species(zoo_cnt)
    type(marbl_zooplankton_share_type)       , intent(inout) :: marbl_zooplankton_share(zoo_cnt)

    integer(int_kind) :: n

    associate( &
         share => marbl_zooplankton_share(:) &
         )

    do n = 1, zoo_cnt
       share(n)%zooC_loc_fields     = zooplankton_local(n)%C
       share(n)%zoo_loss_fields     = zooplankton_secondary_species(n)%zoo_loss
       share(n)%zoo_loss_poc_fields = zooplankton_secondary_species(n)%zoo_loss_poc
       share(n)%zoo_loss_doc_fields = zooplankton_secondary_species(n)%zoo_loss_doc
       share(n)%zoo_loss_dic_fields = zooplankton_secondary_species(n)%zoo_loss_dic
    end do

    end associate
  end subroutine marbl_export_zooplankton_shared_variables


  !***********************************************************************

  subroutine marbl_export_autotroph_shared_variables (&
       auto_cnt, &
       autotroph_local, &
       autotroph_secondary_species, &
       marbl_tracer_indices, &
       marbl_autotroph_share)

    integer(int_kind)                      , intent(in)    :: auto_cnt
    type(autotroph_local_type)             , intent(in)    :: autotroph_local(auto_cnt)
    type(autotroph_secondary_species_type) , intent(in)    :: autotroph_secondary_species(auto_cnt)
    type(marbl_tracer_index_type)          , intent(in)    :: marbl_tracer_indices
    type(marbl_autotroph_share_type)       , intent(inout) :: marbl_autotroph_share(auto_cnt)

    integer(int_kind) :: n

    associate( &
         share => marbl_autotroph_share(:) &
         )

    do n = 1, auto_cnt
       share(n)%autotrophChl_loc_fields = autotroph_local(n)%Chl
       share(n)%autotrophC_loc_fields = autotroph_local(n)%C
       share(n)%autotrophFe_loc_fields = autotroph_local(n)%Fe

       if (marbl_tracer_indices%auto_inds(n)%Si_ind > 0) then
          share(n)%autotrophSi_loc_fields = autotroph_local(n)%Si
       else
          share(n)%autotrophSi_loc_fields = c0
       end if

       if (marbl_tracer_indices%auto_inds(n)%CaCO3_ind > 0) then
          share(n)%autotrophCaCO3_loc_fields = autotroph_local(n)%CaCO3
       else
          share(n)%autotrophCaCO3_loc_fields = c0
       end if

       share(n)%QCaCO3_fields         = autotroph_secondary_species(n)%QCaCO3
       share(n)%auto_graze_fields     = autotroph_secondary_species(n)%auto_graze
       share(n)%auto_graze_zoo_fields = autotroph_secondary_species(n)%auto_graze_zoo
       share(n)%auto_graze_poc_fields = autotroph_secondary_species(n)%auto_graze_poc
       share(n)%auto_graze_doc_fields = autotroph_secondary_species(n)%auto_graze_doc
       share(n)%auto_graze_dic_fields = autotroph_secondary_species(n)%auto_graze_dic
       share(n)%auto_loss_fields      = autotroph_secondary_species(n)%auto_loss
       share(n)%auto_loss_poc_fields  = autotroph_secondary_species(n)%auto_loss_poc
       share(n)%auto_loss_doc_fields  = autotroph_secondary_species(n)%auto_loss_doc
       share(n)%auto_loss_dic_fields  = autotroph_secondary_species(n)%auto_loss_dic
       share(n)%auto_agg_fields       = autotroph_secondary_species(n)%auto_agg
       share(n)%photoC_fields         = autotroph_secondary_species(n)%photoC
       share(n)%CaCO3_PROD_fields     = autotroph_secondary_species(n)%CaCO3_PROD
       share(n)%PCphoto_fields        = autotroph_secondary_species(n)%PCphoto
    end do
    end associate

  end subroutine marbl_export_autotroph_shared_variables

  !***********************************************************************

  subroutine marbl_update_tracer_file_metadata(marbl_tracer_indices,          &
             marbl_tracer_read, tracer_ext, marbl_status_log)

    ! MARBL is responsible for telling the  GCM driver where to read tracer
    ! initial conditions from, and this information comes from the
    ! marbl_tracer_read_type data structure (part of the marbl_interface_class)
    ! By default, the ecosys tracers are expected to be in init_ecosys_init_file
    ! and the CISO tracers are in ciso_init_ecosys_init_file [which is a
    ! terrible variable name], but individual tracers can be read from other
    ! files via the tracer_init_ext and ciso_tracer_init_ext namelist variables.
    ! This routine parses the tracer_init_ext arrays and updates the file
    ! metadata of any specified tracers.

    type(marbl_tracer_index_type), intent(in)    :: marbl_tracer_indices
    type(marbl_tracer_read_type),  intent(inout) :: marbl_tracer_read(:)
    type(marbl_tracer_read_type),  intent(in)    :: tracer_ext(:)
    type(marbl_log_type),          intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_mod:marbl_update_tracer_file_metadata'
    character(len=char_len) :: log_message
    integer :: n, ind, tracer_ind

    do n=1,size(tracer_ext)
      if (trim(tracer_ext(n)%mod_varname).ne.'unknown') then
        ! (1) For each element of tracer_ext(:), determine the tracer index
        !     of the tracer being updated (ignore 'unknown')
        tracer_ind = 0
        do ind=1,size(marbl_tracer_read)
          if (trim(tracer_ext(n)%mod_varname).eq.                              &
              trim(marbl_tracer_read(ind)%mod_varname)) then
            tracer_ind = ind
            exit
          end if
        end do

        ! (1b) Return with an error if no tracer matches
        if (tracer_ind.eq.0) then
          write(log_message,"(A,1X,A)") 'No tracer defined with name',        &
                                        trim(tracer_ext(n)%mod_varname)
          call marbl_status_log%log_error(log_message, subname)
          return
        end if

        ! (2) Given a match, update any fields provided via tracer_ext
        associate(&
                  tracer_read       => marbl_tracer_read(ind),                &
                  namelist_metadata => tracer_ext(n)                          &
                 )
          if (namelist_metadata%filename.ne.'unknown') &
            tracer_read%filename = namelist_metadata%filename
          if (namelist_metadata%file_varname.ne.'unknown') &
            tracer_read%file_varname = namelist_metadata%file_varname
          if (namelist_metadata%scale_factor.ne.c1) &
            tracer_read%scale_factor = namelist_metadata%scale_factor
          if (namelist_metadata%default_val.ne.c0) &
            tracer_read%default_val = namelist_metadata%default_val
        end associate
      end if
    end do

  end subroutine marbl_update_tracer_file_metadata

  !***********************************************************************

  subroutine log_add_forcing_field_error(marbl_status_log, varname, subname)

    type(marbl_log_type), intent(inout) :: marbl_status_log
    character(len=*),     intent(in)    :: varname
    character(len=*),     intent(in)    :: subname
    character(len=char_len) :: routine_name

    write(routine_name,"(3A)") "forcing_fields%add(", trim(varname), ")"
    call marbl_status_log%log_error_trace(routine_name, subname)

  end subroutine log_add_forcing_field_error

end module marbl_mod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
