module marbl_ciso_interior_tendency_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : c2
  use marbl_constants_mod, only : c1000
  use marbl_constants_mod, only : mpercm

  use marbl_settings_mod, only : autotroph_cnt
  use marbl_settings_mod, only : autotroph_settings
  use marbl_settings_mod, only : ciso_on

  use marbl_logging, only : marbl_log_type

  use marbl_interface_public_types, only : marbl_diagnostics_type
  use marbl_interface_public_types, only : marbl_domain_type

  use marbl_interface_private_types, only : autotroph_local_type
  use marbl_interface_private_types, only : column_sinking_particle_type
  use marbl_interface_private_types, only : marbl_interior_tendency_share_type
  use marbl_interface_private_types, only : zooplankton_share_type
  use marbl_interface_private_types, only : marbl_particulate_share_type
  use marbl_interface_private_types, only : marbl_tracer_index_type
  use marbl_interface_private_types, only : autotroph_derived_terms_type

  implicit none
  private

  public :: marbl_ciso_interior_tendency_compute
  public :: marbl_ciso_interior_tendency_autotroph_zero_consistency_enforce

  !-----------------------------------------------------------------------
  !  scalar constants for 14C decay calculation
  !-----------------------------------------------------------------------

   real (r8), parameter :: c14_halflife_years = 5730.0_r8 !C14 half file
   real (r8) :: c14_lambda_inv_sec           ! Decay variable in seconds

contains

  !***********************************************************************

  subroutine marbl_ciso_interior_tendency_compute( &
       marbl_domain,                               &
       interior_tendency_share,                    &
       zooplankton_share,                          &
       marbl_particulate_share,                    &
       tracer_local,                               &
       autotroph_local,                            &
       autotroph_derived_terms,                    &
       temperature,                                &
       marbl_tracer_indices,                       &
       interior_tendencies,                        &
       marbl_interior_diags,                       &
       marbl_status_log)

    !  Compute time derivatives for 13C and 14C state variables.
    !  13C code is based on code from X. Giraud, ETH ZÃ¼rich, 2008, for pop1
    !  Also added biotic 14C

    use marbl_settings_mod, only : ciso_fract_factors
    use marbl_settings_mod, only : f_graze_CaCO3_REMIN
    use marbl_constants_mod, only : R13C_std
    use marbl_constants_mod, only : R14C_std
    use marbl_constants_mod, only : spd
    use marbl_constants_mod, only : spy
    use marbl_ciso_diagnostics_mod, only : store_diagnostics_ciso_interior

    type(marbl_domain_type),                  intent(in)    :: marbl_domain
    type(marbl_interior_tendency_share_type), intent(in)    :: interior_tendency_share
    type(zooplankton_share_type),             intent(in)    :: zooplankton_share
    type(marbl_particulate_share_type),       intent(in)    :: marbl_particulate_share
    real (r8),                                intent(in)    :: tracer_local(:,:)
    type(autotroph_local_type),               intent(in)    :: autotroph_local
    type(autotroph_derived_terms_type),       intent(in)    :: autotroph_derived_terms
    real (r8),                                intent(in)    :: temperature(:)
    type(marbl_tracer_index_type),            intent(in)    :: marbl_tracer_indices
    real (r8),                                intent(inout) :: interior_tendencies(:,:)  ! computed source/sink terms (inout because we don't touch non-ciso tracers)
    type(marbl_diagnostics_type),             intent(inout) :: marbl_interior_diags
    type(marbl_log_type),                     intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_ciso_interior_tendency_mod:marbl_ciso_interior_tendency_compute'

    real (r8) :: work1      ! temporaries

    integer (int_kind) :: &
         n,              & ! tracer index
         auto_ind,       & ! autotroph functional group index
         k                 ! index for looping over k levels

    real (r8), dimension(autotroph_cnt) :: &
         cell_active_C_uptake,  & ! ratio of active carbon uptake to carbon fixation
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

    real (r8), dimension(marbl_domain%km) :: &
         mui_to_co2star_loc     ! local carbon autotroph instanteous growth rate over [CO2*] (m^3 /mol C /s)

    real (r8), dimension(marbl_domain%km) :: &
         R13C_CaCO3_form,   & ! 13C/12C in CaCO3 production of small phyto
         R13C_CO2STAR,      & ! 13C/12C in CO2* water
         R13C_DIC,          & ! 13C/12C in total DIC
         R13C_DOCtot,       & ! 13C/12C in total DOCtot
         R13C_zoototC,      & ! 13C/12C in total zooplankton
         R14C_CaCO3_form,   & ! 14C/12C in CaCO3 production of small phyto
         R14C_CO2STAR,      & ! 14C/12C in CO2* water
         R14C_DIC,          & ! 14C/12C in total DIC
         R14C_DOCtot,       & ! 14C/12C in total DOCtot
         R14C_zoototC         ! 14C/12C in total zooplankton

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
         DO13Ctot_prod,     & ! production of 13C DOCtot (mmol C/m^3/sec)
         DO13Ctot_remin,    & ! remineralization of 13C DOCtot (mmol C/m^3/sec)
         eps_aq_g,          & ! equilibrium fractionation (CO2_gaseous <-> CO2_aq)
         eps_dic_g,         & ! equilibrium fractionation between total DIC and gaseous CO2
         alpha_aq_g,        & ! eps = ( alpa -1 ) * 1000
         alpha_dic_g,       & ! eps = ( alpa -1 ) * 1000
         delta_C13_Corg,    & ! deltaC13 of Net Primary Production
         delta_C13_CO2STAR, & ! deltaC13 of CO2*
         DO14Ctot_prod,     & ! production of 13C DOCtot (mmol C/m^3/sec)
         DO14Ctot_remin,    & ! remineralization of 13C DOCtot (mmol C/m^3/sec)
         alpha_aq_g_14c,    & ! alpha for 14C, with fractionation twice as large as for 13C
         alpha_dic_g_14c,   & ! alpha for 14C, with fractionation twice as large as for 13C
         delta_C14_CO2STAR, & ! deltaC14 of CO2*
         DIC_d13C,          & ! d13C of DIC
         DOCtot_d13C,       & ! d13C of DOCtot
         zoototC_d13C,      & ! d13C of zoototC
         DIC_d14C,          & ! d14C of DIC
         DOCtot_d14C,       & ! d14C of DOCtot
         zoototC_d14C,      & ! d14C of zoototC
         decay_14Ctot         ! 14C decay loss term

    !-------------------------------------------------------------

    ! Return immediately if not running with carbon isotope tracer module
    if (.not. ciso_on) return

    associate(                                   &
         column_km          => marbl_domain%km,  &
         column_kmt         => marbl_domain%kmt, &

         CO3                => interior_tendency_share%CO3_fields,           & ! INPUT carbonate ion
         HCO3               => interior_tendency_share%HCO3_fields,          & ! INPUT bicarbonate ion
         H2CO3              => interior_tendency_share%H2CO3_fields,         & ! INPUT carbonic acid
         DOCtot_remin       => interior_tendency_share%DOCtot_remin_fields,  & ! INPUT remineralization of DOCtot (mmol C/m^3/sec)
         DOCtot_loc         => interior_tendency_share%DOCtot_loc_fields,    & ! INPUT local copy of model DOCtot

         DO13Ctot_loc       => tracer_local(marbl_tracer_indices%DO13Ctot_ind,:),  & ! local copy of model DO14Ctot
         DO14Ctot_loc       => tracer_local(marbl_tracer_indices%DO14Ctot_ind,:),  & ! local copy of model DO14Ctot
         DIC_loc            => tracer_local(marbl_tracer_indices%DIC_ind,:),       & ! INPUT local copy of model DIC
         DI13C_loc          => tracer_local(marbl_tracer_indices%DI13C_ind,:),     & ! local copy of model DI13C
         DI14C_loc          => tracer_local(marbl_tracer_indices%DI14C_ind,:),     & ! local copy of model DI14C
         zootot13C_loc      => tracer_local(marbl_tracer_indices%zootot13C_ind,:), & ! local copy of model zootot13C
         zootot14C_loc      => tracer_local(marbl_tracer_indices%zootot14C_ind,:), & ! local copy of model zootot14C

         QCaCO3             => autotroph_derived_terms%QCaCO3,         & ! INPUT small phyto CaCO3/C ratio (mmol CaCO3/mmol C)
         auto_graze         => autotroph_derived_terms%auto_graze,     & ! INPUT autotroph grazing rate (mmol C/m^3/sec)
         auto_graze_zoo     => autotroph_derived_terms%auto_graze_zoo, & ! INPUT auto_graze routed to zoo (mmol C/m^3/sec)
         auto_graze_poc     => autotroph_derived_terms%auto_graze_poc, & ! INPUT auto_graze routed to poc (mmol C/m^3/sec)
         auto_graze_doc     => autotroph_derived_terms%auto_graze_doc, & ! INPUT auto_graze routed to doc (mmol C/m^3/sec)
         auto_graze_dic     => autotroph_derived_terms%auto_graze_dic, & ! INPUT auto_graze routed to dic (mmol C/m^3/sec)
         auto_loss          => autotroph_derived_terms%auto_loss,      & ! INPUT autotroph non-grazing mort (mmol C/m^3/sec)
         auto_loss_poc      => autotroph_derived_terms%auto_loss_poc,  & ! INPUT auto_loss routed to poc (mmol C/m^3/sec)
         auto_loss_doc      => autotroph_derived_terms%auto_loss_doc,  & ! INPUT auto_loss routed to doc (mmol C/m^3/sec)
         auto_loss_dic      => autotroph_derived_terms%auto_loss_dic,  & ! INPUT auto_loss routed to dic (mmol C/m^3/sec)
         auto_agg           => autotroph_derived_terms%auto_agg,       & ! INPUT autotroph aggregation (mmol C/m^3/sec)
         photoC             => autotroph_derived_terms%photoC,         & ! INPUT C-fixation (mmol C/m^3/sec)
         CaCO3_form         => autotroph_derived_terms%CaCO3_form,     & ! INPUT prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
         PCphoto            => autotroph_derived_terms%PCphoto,        & ! INPUT C-specific rate of photosynth. (1/sec)

         zoototC_loc        => zooplankton_share%zoototC_loc_fields(:),      & ! INPUT local copy of model zoototC
         zootot_loss        => zooplankton_share%zootot_loss_fields(:),      & ! INPUT mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
         zootot_loss_poc    => zooplankton_share%zootot_loss_poc_fields(:),  & ! INPUT zootot_loss routed to large detrital pool (mmol C/m^3/sec)
         zootot_loss_doc    => zooplankton_share%zootot_loss_doc_fields(:),  & ! INPUT zootot_loss routed to doc (mmol C/m^3/sec)
         zootot_loss_dic    => zooplankton_share%zootot_loss_dic_fields(:),  & ! INPUT zootot_loss routed to dic (mmol C/m^3/sec)
         zootot_graze       => zooplankton_share%zootot_graze_fields(:),     & ! INPUT zooplankton losses due to grazing (mmol C/m^3/sec)
         zootot_graze_zoo   => zooplankton_share%zootot_graze_zoo_fields(:), & ! INPUT grazing of zooplankton routed to zoo (mmol C/m^3/sec)
         zootot_graze_poc   => zooplankton_share%zootot_graze_poc_fields(:), & ! INPUT grazing of zooplankton routed to poc (mmol C/m^3/sec)
         zootot_graze_doc   => zooplankton_share%zootot_graze_doc_fields(:), & ! INPUT grazing of zooplankton routed to doc (mmol C/m^3/sec)
         zootot_graze_dic   => zooplankton_share%zootot_graze_dic_fields(:), & ! INPUT grazing of zooplankton routed to dic (mmol C/m^3/sec)

         POC                => marbl_particulate_share%POC,     & ! INPUT
         P_CaCO3            => marbl_particulate_share%P_CaCO3, & ! INPUT

         di13c_ind          => marbl_tracer_indices%di13c_ind,     &
         do13ctot_ind       => marbl_tracer_indices%do13ctot_ind,  &
         zootot13C_ind      => marbl_tracer_indices%zootot13C_ind, &
         di14c_ind          => marbl_tracer_indices%di14c_ind,     &
         do14ctot_ind       => marbl_tracer_indices%do14ctot_ind,  &
         zootot14C_ind      => marbl_tracer_indices%zootot14C_ind  &
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

    !  Define decay variable for DI14C, using earlier defined half-life of 14C
    c14_lambda_inv_sec = log(c2) / (c14_halflife_years * spy)

    !----------------------------------------------------------------------------------------
    ! Set cell attributes
    !----------------------------------------------------------------------------------------

    call setup_cell_attributes(ciso_fract_factors, &
       cell_active_C_uptake, cell_surf, cell_carb_cont, &
       cell_radius, cell_permea, cell_eps_fix, marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace("setup_cell_attributes", subname)
       return
    end if

    !-----------------------------------------------------------------------
    !  Initialize Particulate terms for k=1
    !-----------------------------------------------------------------------

    call set_surface_particulate_terms(POC_ciso=PO13C, P_CaCO3_ciso=P_Ca13CO3)
    call set_surface_particulate_terms(POC_ciso=PO14C, P_CaCO3_ciso=P_Ca14CO3)

    !-----------------------------------------------------------------------
    !  Set ratios
    !-----------------------------------------------------------------------

    do k = 1, column_km

       !-----------------------------------------------------------------------
       !  set local 13C/12C ratios, assuming ecosystem carries 12C (C=C12+C13+C14)
       !  If any Carbon boxes are zero, set corresponding 13C to zeros.
       !  Calculate fraction of CO3
       !-----------------------------------------------------------------------

       if (DOCtot_loc(k) > c0) then
          R13C_DOCtot(k) = DO13Ctot_loc(k) / DOCtot_loc(k)
          R14C_DOCtot(k) = DO14Ctot_loc(k) / DOCtot_loc(k)
       else
          R13C_DOCtot(k) = c0
          R14C_DOCtot(k) = c0
       end if

       if (DIC_loc(k) > c0) then
          R13C_DIC(k) = DI13C_loc(k) / DIC_loc(k)
          R14C_DIC(k) = DI14C_loc(k) / DIC_loc(k)
          frac_co3(k) = CO3(k) / DIC_loc(k)
       else
          R13C_DIC(k) = c0
          R14C_DIC(k) = c0
          frac_co3(k) = c0
       end if

       if (zoototC_loc(k) > c0) then
          R13C_zoototC(k) = zootot13C_loc(k) / zoototC_loc(k)
          R14C_zoototC(k) = zootot14C_loc(k) / zoototC_loc(k)
       else
          R13C_zoototC(k) = c0
          R14C_zoototC(k) = c0
       end if

       do auto_ind = 1, autotroph_cnt
          if (autotroph_local%C(auto_ind,k) > c0) then
             R13C_autotroph(auto_ind,k) = autotroph_local%C13(auto_ind,k) / autotroph_local%C(auto_ind,k)
             R14C_autotroph(auto_ind,k) = autotroph_local%C14(auto_ind,k) / autotroph_local%C(auto_ind,k)
          else
             R13C_autotroph(auto_ind,k) = c0
             R14C_autotroph(auto_ind,k) = c0
          end if

          if (marbl_tracer_indices%auto_inds(auto_ind)%CaCO3_ind > 0) then
             if (autotroph_local%CaCO3(auto_ind,k) > c0) then
                R13C_autotrophCaCO3(auto_ind,k) = autotroph_local%Ca13CO3(auto_ind,k) / autotroph_local%CaCO3(auto_ind,k)
                R14C_autotrophCaCO3(auto_ind,k) = autotroph_local%Ca14CO3(auto_ind,k) / autotroph_local%CaCO3(auto_ind,k)
             else
                R13C_autotrophCaCO3(auto_ind,k) = c0
                R14C_autotrophCaCO3(auto_ind,k) = c0
             end if
          end if
       end do

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

          if (autotroph_settings(auto_ind)%imp_calcifier) then

             R13C_CaCO3_form(k) = R13C_DIC(k) + R13C_std * eps_carb / c1000
             R14C_CaCO3_form(k) = R14C_DIC(k) + R14C_std * eps_carb * 2.0_r8 / c1000

             Ca13CO3_PROD(auto_ind,k) = CaCO3_form(auto_ind,k) * R13C_CaCO3_form(k)
             Ca14CO3_PROD(auto_ind,k) = CaCO3_form(auto_ind,k) * R14C_CaCO3_form(k)

          end if

       end do ! end loop over auto_ind

       !-----------------------------------------------------------------------
       !  compute terms for DO13Ctot and DO14Ctot
       !-----------------------------------------------------------------------

       DO13Ctot_prod(k) = &
            (zootot_loss_doc(k) + zootot_graze_doc(k))*R13C_zoototC(k) + &
            sum((auto_loss_doc(:,k) + auto_graze_doc(:,k)) * R13C_autotroph(:,k),dim=1)

       DO14Ctot_prod(k) = &
            (zootot_loss_doc(k) + zootot_graze_doc(k))*R14C_zoototC(k) + &
            sum((auto_loss_doc(:,k) + auto_graze_doc(:,k)) * R14C_autotroph(:,k),dim=1)

       DO13Ctot_remin(k) = DOCtot_remin(k) * R13C_DOCtot(k)
       DO14Ctot_remin(k) = DOCtot_remin(k) * R14C_DOCtot(k)

       !-----------------------------------------------------------------------
       !  large detritus 13C and 14C
       !-----------------------------------------------------------------------

       PO13C%prod(k) = &
            (zootot_loss_poc(k) + zootot_graze_poc(k))*R13C_zoototC(k) + &
            sum((auto_graze_poc(:,k) + auto_agg(:,k) + auto_loss_poc(:,k)) * R13C_autotroph(:,k),dim=1)

       PO14C%prod(k) = &
            (zootot_loss_poc(k) + zootot_graze_poc(k))*R14C_zoototC(k) + &
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

       DOCtot_d13C(k) =  ( R13C_DOCtot(k) / R13C_std - c1 ) * c1000
       DOCtot_d14C(k) =  ( R14C_DOCtot(k) / R14C_std - c1 ) * c1000

       zoototC_d13C(k)=  ( R13C_zoototC(k) / R13C_std - c1 ) * c1000
       zoototC_d14C(k)=  ( R14C_zoototC(k) / R14C_std - c1 ) * c1000

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

       call compute_particulate_terms(k, marbl_domain, tracer_local(:,k), marbl_tracer_indices, &
            interior_tendency_share, marbl_particulate_share, PO13C, P_Ca13CO3)

       call compute_particulate_terms(k, marbl_domain, tracer_local(:,k), marbl_tracer_indices, &
            interior_tendency_share, marbl_particulate_share, PO14C, P_Ca14CO3)

       !-----------------------------------------------------------------------
       ! Update interior_tendencies for the 7 carbon pools for each Carbon isotope
       !-----------------------------------------------------------------------

       decay_14Ctot(k) = c0

       !-----------------------------------------------------------------------
       !  dtracrs: autotroph Carbon (3 carbon pools), autotroph Ca13CO3 and Ca14CO3
       !-----------------------------------------------------------------------

       do auto_ind = 1, autotroph_cnt
          work1 = auto_graze(auto_ind,k) + auto_loss(auto_ind,k) + auto_agg(auto_ind,k)

          n = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
          interior_tendencies(n,k) = photo13C(auto_ind,k) - work1 * R13C_autotroph(auto_ind,k)

          n = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
          interior_tendencies(n,k) = photo14C(auto_ind,k) - work1 * R14C_autotroph(auto_ind,k) - &
               c14_lambda_inv_sec * autotroph_local%C14(auto_ind,k)

          decay_14Ctot(k) = decay_14Ctot(k) + c14_lambda_inv_sec * autotroph_local%C14(auto_ind,k)

          n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
          if (n > 0) then
             interior_tendencies(n,k) = Ca13CO3_PROD(auto_ind,k) - QCaCO3(auto_ind,k) &
                  * work1 * R13C_autotrophCaCO3(auto_ind,k)
          endif

          n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
          if (n > 0) then
             interior_tendencies(n,k) = Ca14CO3_PROD(auto_ind,k) - QCaCO3(auto_ind,k) &
                  * work1 * R14C_autotrophCaCO3(auto_ind,k)      &
                  - c14_lambda_inv_sec * autotroph_local%Ca14CO3(auto_ind,k)

             decay_14Ctot(k) = decay_14Ctot(k) + c14_lambda_inv_sec * autotroph_local%Ca14CO3(auto_ind,k)
          endif
       end do

       !-----------------------------------------------------------------------
       !  interior_tendencies: zoo 13 and 14 Carbon
       !-----------------------------------------------------------------------

       interior_tendencies(zootot13C_ind,k) = &
              sum(auto_graze_zoo(:,k) * R13C_autotroph(:,k),dim=1) &
            + (zootot_graze_zoo(k) - zootot_graze(k) - zootot_loss(k)) &
            * R13C_zoototC(k)

       interior_tendencies(zootot14C_ind,k) = &
              sum(auto_graze_zoo(:,k) * R14C_autotroph(:,k),dim=1) &
            + (zootot_graze_zoo(k) - zootot_graze(k) - zootot_loss(k)) &
            * R14C_zoototC(k) - c14_lambda_inv_sec * zootot14C_loc(k)

       decay_14Ctot(k) = decay_14Ctot(k) + c14_lambda_inv_sec * zootot14C_loc(k)

       !-----------------------------------------------------------------------
       !  interior_tendencies: dissolved organic Matter 13C and 14C
       !-----------------------------------------------------------------------

       interior_tendencies(do13ctot_ind,k) = DO13Ctot_prod(k) - DO13Ctot_remin(k)

       interior_tendencies(do14ctot_ind,k) = DO14Ctot_prod(k) - DO14Ctot_remin(k) - c14_lambda_inv_sec * DO14Ctot_loc(k)

       decay_14Ctot(k) = decay_14Ctot(k) + c14_lambda_inv_sec * DO14Ctot_loc(k)

       !-----------------------------------------------------------------------
       !   interior_tendencies: dissolved inorganic Carbon 13 and 14
       !-----------------------------------------------------------------------

       interior_tendencies(di13c_ind,k) = &
            sum((auto_loss_dic(:,k) + auto_graze_dic(:,k))*R13C_autotroph(:,k),dim=1) &
          - sum(photo13C(:,k),dim=1) &
          + DO13Ctot_remin(k) + PO13C%remin(k) &
          + (zootot_loss_dic(k) + zootot_graze_dic(k)) * R13C_zoototC(k) &
          + P_Ca13CO3%remin(k)

       interior_tendencies(di14c_ind,k) = &
            sum((auto_loss_dic(:,k) + auto_graze_dic(:,k))*R14C_autotroph(:,k),dim=1) &
          - sum(photo14C(:,k),dim=1) &
          + DO14Ctot_remin(k) + PO14C%remin(k) &
          + (zootot_loss_dic(k) + zootot_graze_dic(k)) * R14C_zoototC(k) &
          + P_Ca14CO3%remin(k) &
          - c14_lambda_inv_sec * DI14C_loc(k)

       decay_14Ctot(k) = decay_14Ctot(k) + c14_lambda_inv_sec * DI14C_loc(k)

       do auto_ind = 1, autotroph_cnt
          if (marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind > 0) then
             interior_tendencies(di13c_ind,k) = interior_tendencies(di13c_ind,k)  &
                  + f_graze_CaCO3_REMIN * auto_graze(auto_ind,k)                  &
                  * QCaCO3(auto_ind,k) * R13C_autotrophCaCO3(auto_ind,k)          &
                  - Ca13CO3_PROD(auto_ind,k)
          endif
          if (marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind > 0) then
             interior_tendencies(di14c_ind,k) = interior_tendencies(di14c_ind,k)  &
                  + f_graze_CaCO3_REMIN * auto_graze(auto_ind,k)                  &
                  * QCaCO3(auto_ind,k) * R14C_autotrophCaCO3(auto_ind,k)          &
                  - Ca14CO3_PROD(auto_ind,k)
          endif
       end do

       !-----------------------------------------------------------------------
       ! Update particulate terms from prior level for next level
       !-----------------------------------------------------------------------

       if  (k < column_km) then
          call update_particulate_terms_from_prior_level(k+1, PO13C, P_Ca13CO3)

          call update_particulate_terms_from_prior_level(k+1, PO14C, P_Ca14CO3)
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
       photo13C,            &
       photo14C,            &
       eps_autotroph,       &
       mui_to_co2star,      &
       Ca13CO3_prod,        &
       Ca14CO3_prod,        &
       DIC_d13C,            &
       DIC_d14C,            &
       DOCtot_d13C,         &
       DOCtot_d14C,         &
       zoototC_d13C,        &
       zoototC_d14C,        &
       DO13Ctot_prod,       &
       DO14Ctot_prod,       &
       DO13Ctot_remin,      &
       DO14Ctot_remin,      &
       eps_aq_g,            &
       eps_dic_g,           &
       decay_14Ctot,        &
       PO13C,               &
       PO14C,               &
       P_Ca13CO3,           &
       P_Ca14CO3,           &
       interior_tendencies, &
       marbl_tracer_indices,&
       marbl_interior_diags,&
       marbl_status_log)

    if (marbl_status_log%labort_marbl) then
       call marbl_status_log%log_error_trace("store_diagnostics_ciso_interior", subname)
       return
    end if

    !-----------------------------------------------------------------------
    ! Deallocate memory for column_sinking_particle data types
    !-----------------------------------------------------------------------

    call PO13C%destruct()
    call PO14C%destruct()
    call P_Ca13CO3%destruct()
    call P_Ca14CO3%destruct()

  end subroutine marbl_ciso_interior_tendency_compute

  !***********************************************************************

  subroutine marbl_ciso_interior_tendency_autotroph_zero_consistency_enforce(auto_ind, column_kmt, zero_mask, &
             autotroph_tracer_indices, autotroph_local)

    use marbl_interface_private_types, only : marbl_living_tracer_index_type

    integer,                              intent(in)    :: auto_ind
    integer,                              intent(in)    :: column_kmt
    logical,                              intent(in)    :: zero_mask(column_kmt)
    type(marbl_living_tracer_index_type), intent(in)    :: autotroph_tracer_indices
    type(autotroph_local_type),           intent(inout) :: autotroph_local

    if (.not. ciso_on) return

    where (zero_mask)
      autotroph_local%C13(auto_ind,1:column_kmt) = c0
      autotroph_local%C14(auto_ind,1:column_kmt) = c0
    end where

    if (autotroph_tracer_indices%Ca13CO3_ind > 0) then
      where (zero_mask)
        autotroph_local%Ca13CO3(auto_ind,1:column_kmt) = c0
      end where
    end if

    if (autotroph_tracer_indices%Ca14CO3_ind > 0) then
      where (zero_mask)
        autotroph_local%Ca14CO3(auto_ind,1:column_kmt) = c0
      end where
    end if

  end subroutine marbl_ciso_interior_tendency_autotroph_zero_consistency_enforce

  !***********************************************************************

  subroutine setup_cell_attributes(ciso_fract_factors, &
       cell_active_C_uptake, cell_surf, cell_carb_cont, &
       cell_radius, cell_permea, cell_eps_fix, marbl_status_log)

    !----------------------------------------------------------------------------------------
    ! For Keller and Morel, set cell attributes based on autotroph type (from observations)
    !----------------------------------------------------------------------------------------

    character(len=char_len), intent(in)  :: ciso_fract_factors                  ! option for which biological fractionation calculation to use
    real (r8),               intent(out) :: cell_active_C_uptake(autotroph_cnt) ! ratio of active carbon uptake to carbon fixation
    real (r8),               intent(out) :: cell_surf(autotroph_cnt)            ! surface areas of cells ( m2 )
    real (r8),               intent(out) :: cell_carb_cont(autotroph_cnt)       ! cell carbon content ( mol C cell-1 )
    real (r8),               intent(out) :: cell_radius(autotroph_cnt)          ! cell radius ( um )
    real (r8),               intent(out) :: cell_permea(autotroph_cnt)          ! cell wall permeability to CO2(aq) (m/s)
    real (r8),               intent(out) :: cell_eps_fix(autotroph_cnt)         ! fractionation effect of carbon fixation
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    character(len=*), parameter :: subname = 'marbl_ciso_interior_tendency_mod:setup_cell_attributes'
    character(len=char_len)     :: log_message

    integer(int_kind) :: auto_ind           ! autotroph functional group index
    !-----------------------------------------------------------------------

    select case (ciso_fract_factors)
    case ('KellerMorel')
       do auto_ind = 1, autotroph_cnt

          if (autotroph_settings(auto_ind)%silicifier) then
             !----------------------------------------------------------------------------------------
             ! Diatom based on P. tricornumtum ( Keller and morel, 1999; Popp et al., 1998 )
             !----------------------------------------------------------------------------------------
             cell_active_C_uptake(auto_ind) = 2.3_r8       ! ratio of active carbon uptake to carbon fixation
             cell_surf(auto_ind)            = 100.6e-12_r8 ! surface areas of cells ( m2 )
             cell_carb_cont(auto_ind)       = 63.3e-14_r8  ! cell carbon content ( mol C cell-1 )
             cell_radius(auto_ind)          = 14.2_r8      ! cell radius ( um )
             cell_permea(auto_ind)          = 3.3e-5_r8    ! cell wall permeability to CO2(aq) (m/s)
             cell_eps_fix(auto_ind)         = 26.6_r8      ! fractionation effect of carbon fixation

          else if (autotroph_settings(auto_ind)%Nfixer) then
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

         !else if (autotroph_settings(auto_ind)%exp_calcifier) then
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

          else if (autotroph_settings(auto_ind)%Nfixer .and. &
                   autotroph_settings(auto_ind)%silicifier) then
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

    use marbl_constants_mod, only : pi, c4, c3

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

    real (r8) :: Vol, Qc, Surf, radius_m

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
       Surf = c4 * pi * (radius_m ** 2)
    endif

    !---------------------------------------------------------------------
    !     cellular carbon content ( mol C )
    !     volume in um^3
    !---------------------------------------------------------------------

    if ( cell_carb_cont > c0 ) then
       Qc = cell_carb_cont
    else
       Vol = c4 * pi * (cell_radius ** 3) / c3
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

  subroutine set_surface_particulate_terms(POC_ciso, P_CaCO3_ciso)

    !---------------------------------------------------------------------
    !  Set incoming fluxes (put into outgoing flux for first level usage).
    !  Set dissolution length, production fraction and mass terms.
    !---------------------------------------------------------------------

    type(column_sinking_particle_type), intent(inout) :: POC_ciso     ! base units = nmol C_ciso
    type(column_sinking_particle_type), intent(inout) :: P_CaCO3_ciso ! base units = nmol C_ciso

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    integer(int_kind) :: ksurf

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

    ksurf = 1

    P_CaCO3_ciso%sflux_in(ksurf) = c0
    P_CaCO3_ciso%hflux_in(ksurf) = c0

    !-----------------------------------------------------------------------
    !  Hard POC is QA flux and soft POC is excess POC.
    !-----------------------------------------------------------------------

    POC_ciso%sflux_in(ksurf) = c0
    POC_ciso%hflux_in(ksurf) = c0

  end subroutine set_surface_particulate_terms

  !***********************************************************************

  subroutine update_particulate_terms_from_prior_level(k, POC_ciso, P_CaCO3_ciso)

    !-----------------------------------------------------------------------
    ! NOTE: incoming fluxes are outgoing fluxes from previous level
    ! initialize loss to sediments = 0
    ! Assume that k == ksurf condition was handled by call to set_surface_particulate_terms()
    !-----------------------------------------------------------------------

    use marbl_interior_tendency_share_mod, only : marbl_interior_tendency_share_update_particle_flux_from_above

    integer (int_kind)                 , intent(in)    :: k ! vertical model level
    type(column_sinking_particle_type) , intent(inout) :: POC_ciso
    type(column_sinking_particle_type) , intent(inout) :: P_CaCO3_ciso
    !-----------------------------------------------------------------------

    if (k > 1) then
       call marbl_interior_tendency_share_update_particle_flux_from_above(k, POC_ciso)
       call marbl_interior_tendency_share_update_particle_flux_from_above(k, P_CaCO3_ciso)
    end if

  end subroutine update_particulate_terms_from_prior_level

  !***********************************************************************

  subroutine compute_particulate_terms(k, domain, tracer_local, marbl_tracer_indices, &
             interior_tendency_share, marbl_particulate_share, POC_ciso, P_CaCO3_ciso)

    !----------------------------------------------------------------------------------------
    !  Compute outgoing fluxes and remineralization terms for Carbon isotopes.
    !  Assumes that production terms have been set and that fluxes and remineralization
    !  for Carbon 12 has already been computed.
    !
    !  Incoming fluxes are assumed to be the outgoing fluxes from the previous level.
    !  For other comments, see compute_particulate_terms in marbl_mod
    !----------------------------------------------------------------------------------------

    use marbl_constants_mod, only : spd
    use marbl_settings_mod , only : denitrif_C_N
    use marbl_settings_mod , only : caco3_bury_thres_iopt
    use marbl_settings_mod , only : caco3_bury_thres_iopt_fixed_depth
    use marbl_settings_mod , only : caco3_bury_thres_depth
    use marbl_settings_mod , only : caco3_bury_thres_omega_calc

    integer (int_kind),                       intent(in)    :: k                 ! vertical model level
    type(marbl_domain_type),                  intent(in)    :: domain
    real(r8),                                 intent(in)    :: tracer_local(:)
    type(marbl_tracer_index_type),            intent(in)    :: marbl_tracer_indices
    type(marbl_interior_tendency_share_type), intent(in)    :: interior_tendency_share
    type(marbl_particulate_share_type),       intent(in)    :: marbl_particulate_share
    type(column_sinking_particle_type),       intent(inout) :: POC_ciso          ! base units = nmol particulate organic Carbon isotope
    type(column_sinking_particle_type),       intent(inout) :: P_CaCO3_ciso      ! base units = nmol CaCO3 Carbon isotope

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    real (r8) ::              &
         dz_loc,              & ! dz at a particular i,j location
         dzr_loc,             & ! dzr at a particular i,j location
         flux_alt,           & ! flux to floor in alternative units, to match particular parameterizations
         POC_ciso_PROD_avail, & ! 13C POC production available for excess POC flux
         Rciso_POC_hflux_out, & ! ciso/12C of outgoing flux of hard POC
         sed_denitrif,        & ! sedimentary denitrification (umolN/cm^2/s)
         other_remin            ! sedimentary remin not due to oxic or denitrification
    !-----------------------------------------------------------------------

    associate(                                                                &
         column_km         => domain%km                                     , & ! IN
         column_kmt        => domain%kmt                                    , & ! IN
         column_delta_z    => domain%delta_z(k)                             , & ! IN
         column_zw         => domain%zw(k)                                  , & ! IN
         O2_loc            => tracer_local(marbl_tracer_indices%O2_ind)     , & ! IN
         NO3_loc           => tracer_local(marbl_tracer_indices%NO3_ind)    , & ! IN
         CO3               => interior_tendency_share%CO3_fields(k)         , & ! IN
         CO3_sat_calcite   => interior_tendency_share%CO3_sat_calcite(k)    , & ! IN
         decay_CaCO3       => marbl_particulate_share%decay_CaCO3_fields    , & ! IN
         DECAY_Hard        => marbl_particulate_share%DECAY_Hard_fields     , & ! IN
         decay_POC_E       => marbl_particulate_share%decay_POC_E_fields    , & ! IN
         POC               => marbl_particulate_share%POC                   , & ! IN
         P_CaCO3           => marbl_particulate_share%P_CaCO3               , & ! IN
         POC_PROD_avail    => marbl_particulate_share%POC_PROD_avail_fields , & ! IN
         poc_diss          => marbl_particulate_share%poc_diss_fields       , & ! IN
         caco3_diss        => marbl_particulate_share%caco3_diss_fields     , & ! IN
         POC_remin         => marbl_particulate_share%POC_remin_fields      , & ! IN
         POC_bury_coeff    => marbl_particulate_share%POC_bury_coeff          & ! IN
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

          Rciso_POC_hflux_out = POC%prod(k) + ( POC%sflux_in(k) - POC%sflux_out(k) + POC%hflux_in(k) ) * dzr_loc

          if (Rciso_POC_hflux_out /= c0) then
             Rciso_POC_hflux_out = ( POC_ciso%prod(k) + ( POC_ciso%sflux_in(k) - &
                  POC_ciso%sflux_out(k) + POC_ciso%hflux_in(k) ) * dzr_loc ) / Rciso_POC_hflux_out
          else
             Rciso_POC_hflux_out = c0
          endif

          POC_ciso%hflux_out(k) = POC%hflux_out(k) * Rciso_POC_hflux_out
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

       POC_ciso%to_floor = POC_ciso%sflux_out(k) + POC_ciso%hflux_out(k)

       if (POC_ciso%to_floor > c0) then
          flux_alt = POC_ciso%to_floor * mpercm * spd ! convert to mmol/m^2/day

          POC_ciso%sed_loss(k) = POC_ciso%to_floor * min(0.8_r8, POC_bury_coeff &
               * (0.013_r8 + 0.53_r8 * flux_alt*flux_alt / (7.0_r8 + flux_alt)**2))


          sed_denitrif = dzr_loc * POC_ciso%to_floor * (0.06_r8 + 0.19_r8 * 0.99_r8**(O2_loc-NO3_loc))

          flux_alt = POC_ciso%to_floor*1.0e-6_r8*spd*365.0_r8 ! convert to mmol/cm^2/year
          other_remin = dzr_loc &
               * min ( min(0.1_r8 + flux_alt,0.5_r8) * (POC_ciso%to_floor - POC_ciso%sed_loss(k)) , &
                      (POC_ciso%to_floor - POC_ciso%sed_loss(k) - (sed_denitrif*dz_loc*denitrif_C_N)))

          ! if bottom water O2 is depleted, assume all remin is denitrif + other
          if (O2_loc < c1) then
             other_remin = dzr_loc * (POC_ciso%to_floor - POC_ciso%sed_loss(k) - (sed_denitrif * dz_loc * denitrif_C_N))
          endif
       endif

       P_CaCO3_ciso%to_floor = P_CaCO3_ciso%sflux_out(k) + P_CaCO3_ciso%hflux_out(k)

       if (caco3_bury_thres_iopt == caco3_bury_thres_iopt_fixed_depth) then
          if (column_zw < caco3_bury_thres_depth) then
            P_CaCO3_ciso%sed_loss(k) = P_CaCO3_ciso%to_floor
         endif
       else ! caco3_bury_thres_iopt = caco3_bury_thres_iopt_omega_calc
         if (CO3 > caco3_bury_thres_omega_calc * CO3_sat_calcite) then
            P_CaCO3_ciso%sed_loss(k) = P_CaCO3_ciso%to_floor
         endif
       end if

       !----------------------------------------------------------------------------------
       ! Update sinking fluxes and remin fluxes, accounting for sediments.
       ! flux used to hold sinking fluxes before update.
       !----------------------------------------------------------------------------------

       if (P_CaCO3_ciso%to_floor > c0) then
          P_CaCO3_ciso%remin(k) = P_CaCO3_ciso%remin(k) + ((P_CaCO3_ciso%to_floor - P_CaCO3_ciso%sed_loss(k)) * dzr_loc)
       endif

       if (POC_ciso%to_floor > c0) then
          POC_ciso%remin(k) = POC_ciso%remin(k) + ((POC_ciso%to_floor - POC_ciso%sed_loss(k)) * dzr_loc)
       endif

    endif

    end associate

  end subroutine compute_particulate_terms

  !***********************************************************************

end module marbl_ciso_interior_tendency_mod
