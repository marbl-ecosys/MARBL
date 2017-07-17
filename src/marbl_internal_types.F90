module marbl_internal_types

  ! module definitions of types that are internal to marbl

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt
  use marbl_sizes, only : max_prey_class_size

  use marbl_logging, only : marbl_log_type

  use marbl_timing_mod, only : marbl_internal_timers_type
  use marbl_timing_mod, only : marbl_timer_indexing_type

  implicit none

  private

  !****************************************************************************
  ! derived types for zooplankton

  type, public :: zooplankton_config_type
     character(len=char_len)     :: sname
     character(len=char_len)     :: lname
  end type zooplankton_config_type

  type, public :: zooplankton_parms_type
     real    (KIND=r8)       :: z_mort_0_per_day   ! zoo linear mort rate (1/day)
     real    (KIND=r8)       :: z_mort_0           ! zoo linear mort rate (1/sec) (derived from z_mort_0_per_day)
     real    (KIND=r8)       :: z_mort2_0_per_day  ! zoo quad mort rate (1/day/((mmol C/m3))
     real    (KIND=r8)       :: z_mort2_0          ! zoo quad mort rate (1/sec/((mmol C/m3)) (derived from z_mort2_0_per_day)
     real    (KIND=r8)       :: loss_thres         ! zoo conc. where losses go to zero
  end type zooplankton_parms_type

  !****************************************************************************
  ! derived types for autotrophs

  type, public :: autotroph_config_type
     character(len=char_len)     :: sname
     character(len=char_len)     :: lname
     logical (KIND=log_kind)     :: Nfixer                             ! flag set to true if this autotroph fixes N2
     logical (KIND=log_kind)     :: imp_calcifier                      ! flag set to true if this autotroph implicitly handles calcification
     logical (KIND=log_kind)     :: exp_calcifier                      ! flag set to true if this autotroph explicitly handles calcification
     logical (KIND=log_kind)     :: silicifier                         ! flag set to true if this autotroph is a silicifier
  end type autotroph_config_type

  type, public :: autotroph_parms_type
     real    (KIND=r8)       :: kFe, kPO4, kDOP, kNO3, kNH4, kSiO3 ! nutrient uptake half-sat constants
     real    (KIND=r8)       :: Qp_fixed                           ! P/C ratio for fixed P/C ratios
     real    (KIND=r8)       :: gQfe_0, gQfe_min                   ! initial and minimum Fe/C ratio for growth
     real    (KIND=r8)       :: alphaPI_per_day                    ! init slope of P_I curve (GD98) (mmol C m^2/(mg Chl W day))
     real    (KIND=r8)       :: alphaPI                            ! init slope of P_I curve (GD98) (mmol C m^2/(mg Chl W sec))
                                                                   !    (derived from alphaPI_per_day)
     real    (KIND=r8)       :: PCref_per_day                      ! max C-spec. grth rate at tref (1/day)
     real    (KIND=r8)       :: PCref                              ! max C-spec. grth rate at tref (1/sec) (derived from PCref_per_day)
     real    (KIND=r8)       :: thetaN_max                         ! max thetaN (Chl/N) (mg Chl/mmol N)
     real    (KIND=r8)       :: loss_thres, loss_thres2            ! conc. where losses go to zero
     real    (KIND=r8)       :: temp_thres                         ! Temp. where concentration threshold and photosynth. rate drops
     real    (KIND=r8)       :: mort_per_day, mort2_per_day        ! linear and quadratic mortality rates (1/day), (1/day/((mmol C/m3))
     real    (KIND=r8)       :: mort, mort2                        ! linear and quadratic mortality rates (1/sec), (1/sec/((mmol C/m3))
                                                                   !    (derived from mort_per_day and mort2_per_day)
     real    (KIND=r8)       :: agg_rate_max, agg_rate_min         ! max and min agg. rate (1/d)
     real    (KIND=r8)       :: loss_poc                           ! routing of loss term
  end type autotroph_parms_type

  !****************************************************************************
  ! derived types for grazing

  type, public :: grazing_config_type
    character(len=char_len) :: sname
    character(len=char_len) :: lname
    integer (KIND=int_kind) :: auto_ind_cnt     ! number of autotrophs in prey-clase auto_ind
    integer (KIND=int_kind) :: zoo_ind_cnt      ! number of zooplankton in prey-clase zoo_ind
  end type grazing_config_type

  type, public :: grazing_parms_type
    integer (KIND=int_kind) :: grazing_function ! functional form of grazing parameterization
    real    (KIND=r8)       :: z_umax_0_per_day ! max zoo growth rate at tref (1/day)
    real    (KIND=r8)       :: z_umax_0         ! max zoo growth rate at tref (1/sec) (derived from z_umax_0_per_day)
    real    (KIND=r8)       :: z_grz            ! grazing coef. (mmol C/m^3)^2
    real    (KIND=r8)       :: graze_zoo        ! routing of grazed term, remainder goes to dic
    real    (KIND=r8)       :: graze_poc        ! routing of grazed term, remainder goes to dic
    real    (KIND=r8)       :: graze_doc        ! routing of grazed term, remainder goes to dic
    real    (KIND=r8)       :: f_zoo_detr       ! fraction of zoo losses to detrital
    ! FIXME #78: we want auto_ind and zoo_ind to be allocatable, but gfortran
    !            does not support having derived types with allocatable components
    !            in a namelist (at least as of 5.2.0). Eventually these two
    !            components should be made allocatable and then we can
    !             (1) remove max_prey_class_size from marbl_sizes
    !             (2) uncomment the constructor for this class
    integer (KIND=int_kind), dimension(max_prey_class_size) :: auto_ind
    integer (KIND=int_kind), dimension(max_prey_class_size) :: zoo_ind
!  contains
!    procedure, public :: construct => grazing_parms_constructor
  end type grazing_parms_type

  !****************************************************************************

  ! derived type for PAR computation
  type, public :: marbl_PAR_type
     real(r8), allocatable :: col_frac(:)    ! column fraction occupied by each sub-column, dimension is (PAR_nsubcols)
     real(r8), allocatable :: interface(:,:) ! PAR at layer interfaces, dimensions are (0:km,PAR_nsubcols)
     real(r8), allocatable :: avg(:,:)       ! PAR averaged over layer, dimensions are (km,PAR_nsubcols)
     real(r8), allocatable :: KPARdz(:)      ! PAR adsorption coefficient times dz (cm), dimension is (km)
   contains
     procedure, public :: construct => marbl_PAR_constructor
     procedure, public :: destruct => marbl_PAR_destructor
  end type marbl_PAR_type

  !****************************************************************************

  ! derived type for implicit handling of sinking particulate matter
  type, public :: column_sinking_particle_type
     real(r8) :: diss                       ! dissolution length for soft subclass
     real(r8) :: gamma                      ! fraction of production -> hard subclass
     real(r8) :: mass                       ! mass of 1e9 base units in g
     real(r8) :: rho                        ! QA mass ratio of POC to this particle class
     real(r8), allocatable  :: sflux_in (:) ! incoming flux of soft subclass (base units/cm^2/sec)
     real(r8), allocatable  :: hflux_in (:) ! incoming flux of hard subclass (base units/cm^2/sec)
     real(r8), allocatable  :: prod     (:) ! production term (base units/cm^3/sec)
     real(r8), allocatable  :: sflux_out(:) ! outgoing flux of soft subclass (base units/cm^2/sec)
     real(r8), allocatable  :: hflux_out(:) ! outgoing flux of hard subclass (base units/cm^2/sec)
     real(r8), allocatable  :: sed_loss (:) ! loss to sediments (base units/cm^s/sec)
     real(r8), allocatable  :: remin    (:) ! remineralization term (base units/cm^3/sec)
   contains
     procedure, public :: construct => column_sinking_particle_constructor
     procedure, public :: destruct => column_sinking_particle_destructor
  end type column_sinking_particle_type

  !****************************************************************************

  type, public :: marbl_surface_forcing_internal_type
     real (r8), allocatable, dimension(:)   :: piston_velocity
     real (r8), allocatable, dimension(:)   :: flux_co2
     real (r8), allocatable, dimension(:)   :: flux_alt_co2 ! tracer flux alternative CO2 (nmol/cm^2/s)
     real (r8), allocatable, dimension(:)   :: co2star
     real (r8), allocatable, dimension(:)   :: dco2star
     real (r8), allocatable, dimension(:)   :: pco2surf
     real (r8), allocatable, dimension(:)   :: dpco2
     real (r8), allocatable, dimension(:)   :: co3
     real (r8), allocatable, dimension(:)   :: co2star_alt
     real (r8), allocatable, dimension(:)   :: dco2star_alt
     real (r8), allocatable, dimension(:)   :: pco2surf_alt
     real (r8), allocatable, dimension(:)   :: dpco2_alt
     real (r8), allocatable, dimension(:)   :: schmidt_co2  ! Schmidt number
     real (r8), allocatable, dimension(:)   :: schmidt_o2   ! Schmidt number
     real (r8), allocatable, dimension(:)   :: pv_o2        ! piston velocity (cm/s)
     real (r8), allocatable, dimension(:)   :: pv_co2       ! piston velocity (cm/s)
     real (r8), allocatable, dimension(:)   :: o2sat        ! used O2 saturation (mmol/m^3)
     real (r8), allocatable, dimension(:)   :: nhx_surface_emis
   contains
     procedure, public :: construct => marbl_surface_forcing_internal_constructor
  end type marbl_surface_forcing_internal_type

  !****************************************************************************
  !
  ! Shared data type definitions
  !
  !****************************************************************************

  type, public :: marbl_interior_share_type
     real(r8) :: QA_dust_def      ! incoming deficit in the QA(dust) POC flux
     real(r8) :: DIC_loc_fields   ! local copy of model DIC
     real(r8) :: DOC_loc_fields   ! local copy of model DOC
     real(r8) :: O2_loc_fields    ! local copy of model O2
     real(r8) :: NO3_loc_fields   ! local copy of model NO3
     real(r8) :: CO3_fields
     real(r8) :: HCO3_fields      ! bicarbonate ion
     real(r8) :: H2CO3_fields     ! carbonic acid
     real(r8) :: DOC_remin_fields ! remineralization of 13C DOC (mmol C/m^3/sec)
  end type marbl_interior_share_type

  !***********************************************************************

  type, public :: marbl_zooplankton_share_type
     real(r8) :: zooC_loc_fields     ! local copy of model zooC
     real(r8) :: zoo_loss_fields     ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
     real(r8) :: zoo_loss_poc_fields ! zoo_loss routed to large detrital (mmol C/m^3/sec)
     real(r8) :: zoo_loss_doc_fields ! zoo_loss routed to doc (mmol C/m^3/sec)
     real(r8) :: zoo_loss_dic_fields ! zoo_loss routed to dic (mmol C/m^3/sec)
  end type marbl_zooplankton_share_type

  !***********************************************************************

  type, public :: marbl_autotroph_share_type
     real(r8) :: autotrophChl_loc_fields   ! local copy of model autotroph Chl
     real(r8) :: autotrophC_loc_fields     ! local copy of model autotroph C
     real(r8) :: autotrophFe_loc_fields    ! local copy of model autotroph Fe
     real(r8) :: autotrophSi_loc_fields    ! local copy of model autotroph Si
     real(r8) :: autotrophCaCO3_loc_fields ! local copy of model autotroph CaCO3
     real(r8) :: QCaCO3_fields             ! small phyto CaCO3/C ratio (mmol CaCO3/mmol C)
     real(r8) :: auto_graze_fields         ! autotroph grazing rate (mmol C/m^3/sec)
     real(r8) :: auto_graze_zoo_fields     ! auto_graze routed to zoo (mmol C/m^3/sec)
     real(r8) :: auto_graze_poc_fields     ! auto_graze routed to poc (mmol C/m^3/sec)
     real(r8) :: auto_graze_doc_fields     ! auto_graze routed to doc (mmol C/m^3/sec)
     real(r8) :: auto_graze_dic_fields     ! auto_graze routed to dic (mmol C/m^3/sec)
     real(r8) :: auto_loss_fields          ! autotroph non-grazing mort (mmol C/m^3/sec)
     real(r8) :: auto_loss_poc_fields      ! auto_loss routed to poc (mmol C/m^3/sec)
     real(r8) :: auto_loss_doc_fields      ! auto_loss routed to doc (mmol C/m^3/sec)
     real(r8) :: auto_loss_dic_fields      ! auto_loss routed to dic (mmol C/m^3/sec)
     real(r8) :: auto_agg_fields           ! autotroph aggregation (mmol C/m^3/sec)
     real(r8) :: photoC_fields             ! C-fixation (mmol C/m^3/sec)
     real(r8) :: CaCO3_form_fields         ! calcification of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
     real(r8) :: PCphoto_fields            ! C-specific rate of photosynth. (1/sec)
  end type marbl_autotroph_share_type

  !***********************************************************************

  type, public :: marbl_particulate_share_type
     type(column_sinking_particle_type) :: POC              ! base units = nmol C
     type(column_sinking_particle_type) :: POP              ! base units = nmol P
     type(column_sinking_particle_type) :: P_CaCO3          ! base units = nmol CaCO3
     type(column_sinking_particle_type) :: P_CaCO3_ALT_CO2  ! base units = nmol CaCO3
     type(column_sinking_particle_type) :: P_SiO2           ! base units = nmol SiO2
     type(column_sinking_particle_type) :: dust             ! base units = g
     type(column_sinking_particle_type) :: P_iron           ! base units = nmol Fe

     real(r8), allocatable :: decay_CaCO3_fields       (:) ! scaling factor for dissolution of CaCO3
     real(r8), allocatable :: decay_POC_E_fields       (:) ! scaling factor for dissolution of excess POC
     real(r8), allocatable :: decay_Hard_fields        (:) ! scaling factor for dissolution of Hard Ballast
     real(r8), allocatable :: poc_diss_fields          (:) ! diss. length used (cm)
     real(r8), allocatable :: caco3_diss_fields        (:) ! caco3 diss. length used (cm)
     real(r8), allocatable :: P_CaCO3_sflux_out_fields (:) ! P_CaCO3 sflux_out from ecosys before getting set to zero for k=KMT
     real(r8), allocatable :: P_CaCO3_hflux_out_fields (:) ! P_CaCO3_hflux_out from ecosys before getting set to zero for k=KMT
     real(r8), allocatable :: P_CaCO3_remin_fields     (:) ! P_CaCO3 remin from ecosys before it gets modified for k=KMT
     real(r8), allocatable :: POC_sflux_out_fields     (:) ! POC_sflux_out from ecosys before getting set to zero for k=KMT
     real(r8), allocatable :: POC_hflux_out_fields     (:) ! POC_hflux_out from ecosys before getting set to zero for k=KMT
     real(r8), allocatable :: POC_remin_fields         (:) ! POC remin from ecosys before it gets modified for k=KMT
     real(r8), allocatable :: POC_prod_avail_fields    (:) ! POC production available for excess POC flux

     real(r8)              :: POC_bury_coeff
     real(r8)              :: POP_bury_coeff
     real(r8)              :: bSi_bury_coeff
   contains
     procedure, public :: construct => marbl_particulate_share_constructor
     procedure, public :: destruct  => marbl_particulate_share_destructor
  end type marbl_particulate_share_type

  !*****************************************************************************

  type, public :: carbonate_type
     real (r8) :: CO3           ! carbonate ion
     real (r8) :: HCO3          ! bicarbonate ion
     real (r8) :: H2CO3         ! carbonic acid
     real (r8) :: pH
     real (r8) :: CO3_sat_calcite
     real (r8) :: CO3_sat_aragonite
     real (r8) :: CO3_ALT_CO2   ! carbonate ion, alternative CO2
     real (r8) :: HCO3_ALT_CO2  ! bicarbonate ion, alternative CO2
     real (r8) :: H2CO3_ALT_CO2 ! carbonic acid, alternative CO2
     real (r8) :: pH_ALT_CO2
  end type carbonate_type

  !*****************************************************************************

  type, public :: autotroph_secondary_species_type
     real (r8) :: thetaC          ! current Chl/C ratio (mg Chl/mmol C)
     real (r8) :: QCaCO3          ! current CaCO3/C ratio (mmol CaCO3/mmol C)
     real (r8) :: Qp              ! current P/C ratio (mmol P/mmol C)
     real (r8) :: gQp             ! P/C for growth
     real (r8) :: Qfe             ! current Fe/C ratio (mmol Fe/mmol C)
     real (r8) :: gQfe            ! fe/C for growth
     real (r8) :: Qsi             ! current Si/C ratio (mmol Si/mmol C)
     real (r8) :: gQsi            ! diatom Si/C ratio for growth (new biomass)
     real (r8) :: VNO3            ! NH4 uptake rate (non-dim)
     real (r8) :: VNH4            ! NO3 uptake rate (non-dim)
     real (r8) :: VNtot           ! total N uptake rate (non-dim)
     real (r8) :: NO3_V           ! nitrate uptake (mmol NO3/m^3/sec)
     real (r8) :: NH4_V           ! ammonium uptake (mmol NH4/m^3/sec)
     real (r8) :: PO4_V           ! PO4 uptake (mmol PO4/m^3/sec)
     real (r8) :: DOP_V           ! DOP uptake (mmol DOP/m^3/sec)
     real (r8) :: VPO4            ! C-specific PO4 uptake (non-dim)
     real (r8) :: VDOP            ! C-specific DOP uptake rate (non-dim)
     real (r8) :: VPtot           ! total P uptake rate (non-dim)
     real (r8) :: f_nut           ! nut limitation factor, modifies C fixation (non-dim)
     real (r8) :: VFe             ! C-specific Fe uptake (non-dim)
     real (r8) :: VSiO3           ! C-specific SiO3 uptake (non-dim)
     real (r8) :: light_lim       ! light limitation factor
     real (r8) :: PCphoto         ! C-specific rate of photosynth. (1/sec)
     real (r8) :: photoC          ! C-fixation (mmol C/m^3/sec)
     real (r8) :: photoFe         ! iron uptake
     real (r8) :: photoSi         ! silicon uptake (mmol Si/m^3/sec)
     real (r8) :: photoacc        ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
     real (r8) :: auto_loss       ! autotroph non-grazing mort (mmol C/m^3/sec)
     real (r8) :: auto_loss_poc   ! auto_loss routed to poc (mmol C/m^3/sec)
     real (r8) :: auto_loss_doc   ! auto_loss routed to doc (mmol C/m^3/sec)
     real (r8) :: auto_loss_dic   ! auto_loss routed to dic (mmol C/m^3/sec)
     real (r8) :: auto_agg        ! autotroph aggregation (mmol C/m^3/sec)
     real (r8) :: auto_graze      ! autotroph grazing rate (mmol C/m^3/sec)
     real (r8) :: auto_graze_zoo  ! auto_graze routed to zoo (mmol C/m^3/sec)
     real (r8) :: auto_graze_poc  ! auto_graze routed to poc (mmol C/m^3/sec)
     real (r8) :: auto_graze_doc  ! auto_graze routed to doc (mmol C/m^3/sec)
     real (r8) :: auto_graze_dic  ! auto_graze routed to dic (mmol C/m^3/sec)
     real (r8) :: Pprime          ! used to limit autotroph mort at low biomass (mmol C/m^3)
     real (r8) :: CaCO3_form      ! calcification of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
     real (r8) :: Nfix            ! total Nitrogen fixation (mmol N/m^3/sec)
     real (r8) :: Nexcrete        ! fixed N excretion
     real (r8) :: remaining_P_dop ! remaining_P from grazing routed to DOP pool
     real (r8) :: remaining_P_pop ! remaining_P from grazing routed to POP pool
     real (r8) :: remaining_P_dip ! remaining_P from grazing routed to remin
  end type autotroph_secondary_species_type

  !*****************************************************************************

  type, public :: zooplankton_secondary_species_type
     real (r8):: f_zoo_detr       ! frac of zoo losses into large detrital pool (non-dim)
     real (r8):: x_graze_zoo      ! {auto, zoo}_graze routed to zoo (mmol C/m^3/sec)
     real (r8):: zoo_graze        ! zooplankton losses due to grazing (mmol C/m^3/sec)
     real (r8):: zoo_graze_zoo    ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
     real (r8):: zoo_graze_poc    ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
     real (r8):: zoo_graze_doc    ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
     real (r8):: zoo_graze_dic    ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
     real (r8):: zoo_loss         ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
     real (r8):: zoo_loss_poc     ! zoo_loss routed to poc (mmol C/m^3/sec)
     real (r8):: zoo_loss_doc     ! zoo_loss routed to doc (mmol C/m^3/sec)
     real (r8):: zoo_loss_dic     ! zoo_loss routed to dic (mmol C/m^3/sec)
     real (r8):: Zprime           ! used to limit zoo mort at low biomass (mmol C/m^3)
  end type zooplankton_secondary_species_type

  !*****************************************************************************

  type, public :: dissolved_organic_matter_type
     real (r8) :: DOC_prod         ! production of DOC (mmol C/m^3/sec)
     real (r8) :: DOC_remin        ! remineralization of DOC (mmol C/m^3/sec)
     real (r8) :: DOCr_remin       ! remineralization of DOCr
     real (r8) :: DON_prod         ! production of DON
     real (r8) :: DON_remin        ! remineralization of DON
     real (r8) :: DONr_remin       ! remineralization of DONr
     real (r8) :: DOP_prod         ! production of DOP
     real (r8) :: DOP_remin        ! remineralization of DOP
     real (r8) :: DOPr_remin       ! remineralization of DOPr
  end type dissolved_organic_matter_type

  !***********************************************************************

  type, public :: marbl_surface_forcing_share_type
     real(r8), allocatable :: PV_SURF_fields       (:) ! piston velocity (cm/s)
     real(r8), allocatable :: DIC_SURF_fields      (:) ! surface values of DIC for solver
     real(r8), allocatable :: CO2STAR_SURF_fields  (:) ! CO2STAR from solver
     real(r8), allocatable :: DCO2STAR_SURF_fields (:) ! DCO2STAR from solver
     real(r8), allocatable :: CO3_SURF_fields      (:) ! Surface carbonate ion
   contains
     procedure, public :: construct => marbl_surface_forcing_share_constructor
     procedure, public :: destruct => marbl_surface_forcing_share_destructor
  end type marbl_surface_forcing_share_type

  !*****************************************************************************

  type, private :: marbl_living_tracer_index_type
     integer (KIND=int_kind) :: Chl_ind     = 0  ! tracer indices for Chl content
     integer (KIND=int_kind) :: C_ind       = 0  ! tracer indices for C content
     integer (KIND=int_kind) :: P_ind       = 0  ! tracer indices for P content
     integer (KIND=int_kind) :: Fe_ind      = 0  ! tracer indices for Fe content
     integer (KIND=int_kind) :: Si_ind      = 0  ! tracer indices for Si  content
     integer (KIND=int_kind) :: CaCO3_ind   = 0  ! tracer indices for CaCO3 content
     integer (KIND=int_kind) :: C13_ind     = 0  ! tracer indices for 13C content
     integer (KIND=int_kind) :: C14_ind     = 0  ! tracer indices for 14C content
     integer (KIND=int_kind) :: Ca13CO3_ind = 0  ! tracer indices for 13CaCO3 content
     integer (KIND=int_kind) :: Ca14CO3_ind = 0  ! tracer indices for 14CaCO3 content
  end type marbl_living_tracer_index_type

  !*****************************************************************************

  type, private :: marbl_tracer_count_type
    ! Total count
    integer(int_kind) :: cnt
    ! Index ranges
    integer(int_kind) :: ind_beg
    integer(int_kind) :: ind_end
  contains
    procedure, public :: set_tracer_cnt
  end type marbl_tracer_count_type

  !*****************************************************************************

  type, public :: marbl_tracer_index_type
    ! Book-keeping (tracer count and index ranges)
    integer (int_kind) :: marbl_total_tracer_cnt
    type (marbl_tracer_count_type) :: ecosys_base
    type (marbl_tracer_count_type) :: ciso

    ! General tracers
    integer (int_kind) :: po4_ind         = 0 ! dissolved inorganic phosphate
    integer (int_kind) :: no3_ind         = 0 ! dissolved inorganic nitrate
    integer (int_kind) :: sio3_ind        = 0 ! dissolved inorganic silicate
    integer (int_kind) :: nh4_ind         = 0 ! dissolved ammonia
    integer (int_kind) :: fe_ind          = 0 ! dissolved inorganic iron
    integer (int_kind) :: lig_ind         = 0 ! Fe-binding Ligand
    integer (int_kind) :: o2_ind          = 0 ! dissolved oxygen
    integer (int_kind) :: dic_ind         = 0 ! dissolved inorganic carbon
    integer (int_kind) :: dic_alt_co2_ind = 0 ! dissolved inorganic carbon with alternative CO2
    integer (int_kind) :: alk_ind         = 0 ! alkalinity
    integer (int_kind) :: alk_alt_co2_ind = 0 ! alkalinity with alternative CO2
    integer (int_kind) :: doc_ind         = 0 ! dissolved organic carbon
    integer (int_kind) :: don_ind         = 0 ! dissolved organic nitrogen
    integer (int_kind) :: dop_ind         = 0 ! dissolved organic phosphorus
    integer (int_kind) :: dopr_ind        = 0 ! refractory DOP
    integer (int_kind) :: donr_ind        = 0 ! refractory DON
    integer (int_kind) :: docr_ind        = 0 ! refractory DOC

    ! CISO tracers
    integer (int_kind) :: di13c_ind       = 0 ! dissolved inorganic carbon 13
    integer (int_kind) :: do13c_ind       = 0 ! dissolved organic carbon 13
    integer (int_kind) :: di14c_ind       = 0 ! dissolved inorganic carbon 14
    integer (int_kind) :: do14c_ind       = 0 ! dissolved organic carbon 14

    ! Living tracers
    type(marbl_living_tracer_index_type), dimension(autotroph_cnt)   :: auto_inds
    type(marbl_living_tracer_index_type), dimension(zooplankton_cnt) :: zoo_inds
    ! For CISO, don't want individual C13 and C14 tracers for each zooplankton
    ! Instead we collect them into one tracer for each isotope, regardless of
    ! zooplankton_cnt
    integer (int_kind) :: zoo13C_ind      = 0 ! zooplankton carbon 13
    integer (int_kind) :: zoo14C_ind      = 0 ! zooplankton carbon 14

  contains
    procedure, public :: construct => tracer_index_constructor
  end type marbl_tracer_index_type

  !****************************************************************************

  type, public :: marbl_surface_forcing_indexing_type
     integer(int_kind) :: u10_sqr_id           = 0
     integer(int_kind) :: ifrac_id             = 0
     integer(int_kind) :: sst_id               = 0
     integer(int_kind) :: sss_id               = 0
     integer(int_kind) :: atm_pressure_id      = 0
     integer(int_kind) :: xco2_id              = 0
     integer(int_kind) :: xco2_alt_co2_id      = 0
     integer(int_kind) :: dust_flux_id         = 0
     integer(int_kind) :: iron_flux_id         = 0
     integer(int_kind) :: nox_flux_id          = 0
     integer(int_kind) :: nhy_flux_id          = 0
     integer(int_kind) :: ext_C_flux_id        = 0
     integer(int_kind) :: ext_P_flux_id        = 0
     integer(int_kind) :: ext_Si_flux_id       = 0
     integer(int_kind) :: d13c_id              = 0
     integer(int_kind) :: d14c_id              = 0
   contains
     procedure, public :: construct => surface_forcing_index_constructor
  end type marbl_surface_forcing_indexing_type

  !****************************************************************************

  type, public :: marbl_interior_forcing_indexing_type
     ! Surface forcing fields that affect interior forcings
     integer(int_kind) :: dustflux_id        = 0
     integer(int_kind) :: PAR_col_frac_id    = 0
     integer(int_kind) :: surf_shortwave_id  = 0

     ! Column fields
     integer(int_kind) :: temperature_id = 0
     integer(int_kind) :: salinity_id    = 0
     integer(int_kind) :: pressure_id    = 0
     integer(int_kind) :: fesedflux_id   = 0

     ! Tracer restoring
     ! * tracer_restore_id is the index in interior forcings that contains the
     !   restoring data
     ! * inv_tau_id is the index in interior forcings that contains the inverse
     !   time scale for restoring
     ! * tracer_id is the tracer index that the restoring is applied to
     integer(int_kind), pointer :: tracer_restore_id(:)
     integer(int_kind), pointer :: inv_tau_id(:)
     integer(int_kind), pointer :: tracer_id(:)
   contains
     procedure, public :: construct => interior_forcing_index_constructor
  end type marbl_interior_forcing_indexing_type

  !*****************************************************************************

  type, public :: marbl_surface_saved_state_indexing_type
    integer :: ph_surf = 0
    integer :: ph_alt_co2_surf = 0
  end type marbl_surface_saved_state_indexing_type

  !*****************************************************************************

  type, public :: marbl_interior_saved_state_indexing_type
    integer :: ph_col = 0
    integer :: ph_alt_co2_col = 0
  end type marbl_interior_saved_state_indexing_type

  !***********************************************************************

  ! marbl interface should use marbl_internal_timers_type and
  ! marbl_timer_indexing_type from here
  public :: marbl_internal_timers_type
  public :: marbl_timer_indexing_type

contains

  !***********************************************************************

  subroutine column_sinking_particle_constructor(this, num_levels)
    class(column_sinking_particle_type), intent(inout) :: this
    integer (int_kind) :: num_levels

    allocate(this%sflux_in (num_levels))
    allocate(this%hflux_in (num_levels))
    allocate(this%prod     (num_levels))
    allocate(this%sflux_out(num_levels))
    allocate(this%hflux_out(num_levels))
    allocate(this%sed_loss (num_levels))
    allocate(this%remin    (num_levels))
  end subroutine column_sinking_particle_constructor

  subroutine column_sinking_particle_destructor(this)
    class(column_sinking_particle_type), intent(inout) :: this
    integer (int_kind) :: num_levels

    deallocate(this%sflux_in)
    deallocate(this%hflux_in)
    deallocate(this%prod)
    deallocate(this%sflux_out)
    deallocate(this%hflux_out)
    deallocate(this%sed_loss)
    deallocate(this%remin)
  end subroutine column_sinking_particle_destructor

  !***********************************************************************

  subroutine marbl_particulate_share_constructor(this, num_levels)
    class(marbl_particulate_share_type), intent(inout) :: this
    integer (int_kind) :: num_levels

    allocate(this%decay_CaCO3_fields               (num_levels))
    allocate(this%decay_POC_E_fields               (num_levels))
    allocate(this%decay_Hard_fields                (num_levels))
    allocate(this%poc_diss_fields                  (num_levels))
    allocate(this%caco3_diss_fields                (num_levels))
    allocate(this%P_CaCO3_sflux_out_fields         (num_levels))
    allocate(this%P_CaCO3_hflux_out_fields         (num_levels))
    allocate(this%P_CaCO3_remin_fields             (num_levels))
    allocate(this%POC_sflux_out_fields             (num_levels))
    allocate(this%POC_hflux_out_fields             (num_levels))
    allocate(this%POC_remin_fields                 (num_levels))
    allocate(this%POC_prod_avail_fields            (num_levels))

    ! Now allocate memory for the column_sinking_particles_type components
    call this%POC%construct             (num_levels)
    call this%POP%construct             (num_levels)
    call this%P_CaCO3%construct         (num_levels)
    call this%P_CaCO3_ALT_CO2%construct (num_levels)
    call this%P_SiO2%construct          (num_levels)
    call this%P_iron%construct          (num_levels)
    call this%dust%construct            (num_levels)
  end subroutine marbl_particulate_share_constructor

  subroutine marbl_particulate_share_destructor(this)
    class(marbl_particulate_share_type), intent(inout) :: this

    deallocate(this%decay_CaCO3_fields)
    deallocate(this%decay_POC_E_fields)
    deallocate(this%decay_Hard_fields)
    deallocate(this%poc_diss_fields)
    deallocate(this%caco3_diss_fields)
    deallocate(this%P_CaCO3_sflux_out_fields)
    deallocate(this%P_CaCO3_hflux_out_fields)
    deallocate(this%P_CaCO3_remin_fields)
    deallocate(this%POC_sflux_out_fields)
    deallocate(this%POC_hflux_out_fields)
    deallocate(this%POC_remin_fields)
    deallocate(this%POC_prod_avail_fields)

     ! Now allocate memory for the column_sinking_particles_type components
     call this%POC%destruct()
     call this%POP%destruct()
     call this%P_CaCO3%destruct()
     call this%P_CaCO3_ALT_CO2%destruct()
     call this%P_SiO2%destruct()
     call this%P_iron%destruct()
     call this%dust%destruct()
   end subroutine marbl_particulate_share_destructor

  !***********************************************************************

   subroutine marbl_surface_forcing_share_constructor(this, num_elements)
     class(marbl_surface_forcing_share_type), intent(inout) :: this
     integer (int_kind) , intent(in) :: num_elements

     allocate(this%PV_SURF_fields       (num_elements)) ! piston velocity (cm/s)
     allocate(this%DIC_SURF_fields      (num_elements)) ! Surface values of DIC for solver
     allocate(this%CO2STAR_SURF_fields  (num_elements)) ! CO2STAR from solver
     allocate(this%DCO2STAR_SURF_fields (num_elements)) ! DCO2STAR from solver
     allocate(this%CO3_SURF_fields      (num_elements)) ! Surface carbonate ion
   end subroutine marbl_surface_forcing_share_constructor

   subroutine marbl_surface_forcing_share_destructor(this, num_elements)
     class(marbl_surface_forcing_share_type), intent(inout) :: this
     integer (int_kind) , intent(in) :: num_elements

     deallocate(this%PV_SURF_fields      ) ! piston velocity (cm/s)
     deallocate(this%DIC_SURF_fields     ) ! Surface values of DIC for solver
     deallocate(this%CO2STAR_SURF_fields ) ! CO2STAR from solver
     deallocate(this%DCO2STAR_SURF_fields) ! DCO2STAR from solver
     deallocate(this%CO3_SURF_fields     ) ! Surface carbonate ion
   end subroutine marbl_surface_forcing_share_destructor

  !*****************************************************************************

  subroutine marbl_PAR_constructor(this, num_levels, num_PAR_subcols)
    class(marbl_PAR_type) , intent(inout) :: this
    integer               , intent(in)    :: num_levels
    integer               , intent(in)    :: num_PAR_subcols

    allocate(this%interface(0:num_levels,num_PAR_subcols))
    allocate(this%avg      (  num_levels,num_PAR_subcols))
    allocate(this%KPARdz   (  num_levels                ))
    allocate(this%col_frac (             num_PAR_subcols))
  end subroutine marbl_PAR_constructor

  subroutine marbl_PAR_destructor(this)
    class(marbl_PAR_type) , intent(inout) :: this

    deallocate(this%interface)
    deallocate(this%avg      )
    deallocate(this%KPARdz   )
    deallocate(this%col_frac )
  end subroutine marbl_PAR_destructor

  !***********************************************************************

  subroutine marbl_surface_forcing_internal_constructor(this, num_elements)
    class(marbl_surface_forcing_internal_type) , intent(inout) :: this
    integer (int_kind)                         , intent(in)    :: num_elements

    allocate(this%piston_velocity (num_elements))
    allocate(this%flux_co2        (num_elements))
    allocate(this%flux_alt_co2    (num_elements))
    allocate(this%co2star         (num_elements))
    allocate(this%dco2star        (num_elements))
    allocate(this%pco2surf        (num_elements))
    allocate(this%dpco2           (num_elements))
    allocate(this%co3             (num_elements))
    allocate(this%co2star_alt     (num_elements))
    allocate(this%dco2star_alt    (num_elements))
    allocate(this%pco2surf_alt    (num_elements))
    allocate(this%dpco2_alt       (num_elements))
    allocate(this%schmidt_co2     (num_elements))
    allocate(this%schmidt_o2      (num_elements))
    allocate(this%pv_o2           (num_elements))
    allocate(this%pv_co2          (num_elements))
    allocate(this%o2sat           (num_elements))
    allocate(this%nhx_surface_emis(num_elements))
  end subroutine marbl_surface_forcing_internal_constructor

  !*****************************************************************************

  subroutine tracer_index_constructor(this, ciso_on, lvariable_PtoC, autotrophs_config, &
             zooplankton_config)

    ! This subroutine sets the tracer indices for the non-autotroph tracers. To
    ! know where to start the indexing for the autotroph tracers, it increments
    ! tracer_cnt by 1 for each tracer that is included. Note that this gives an
    ! accurate count whether the carbon isotope tracers are included or not.

    use marbl_constants_mod, only : c0

    class(marbl_tracer_index_type), intent(inout) :: this
    logical,                        intent(in)    :: ciso_on
    logical,                        intent(in)    :: lvariable_PtoC
    type(autotroph_config_type),    intent(in)    :: autotrophs_config(:)
    type(zooplankton_config_type),  intent(in)    :: zooplankton_config(:)

    integer :: n

    associate(tracer_cnt => this%marbl_total_tracer_cnt)

      tracer_cnt = 0
      this%ciso%ind_beg = 0
      this%ciso%ind_end = 0

      ! General ecosys tracers
      this%ecosys_base%ind_beg = tracer_cnt + 1

      tracer_cnt  = tracer_cnt + 1
      this%po4_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%no3_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%sio3_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%nh4_ind  = tracer_cnt

      tracer_cnt  = tracer_cnt + 1
      this%fe_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%lig_ind = tracer_cnt

      tracer_cnt  = tracer_cnt + 1
      this%o2_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%dic_ind = tracer_cnt

      tracer_cnt           = tracer_cnt + 1
      this%dic_alt_co2_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%alk_ind = tracer_cnt

      tracer_cnt           = tracer_cnt + 1
      this%alk_alt_co2_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%doc_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%don_ind = tracer_cnt

      tracer_cnt   = tracer_cnt + 1
      this%dop_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%dopr_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%donr_ind = tracer_cnt

      tracer_cnt    = tracer_cnt + 1
      this%docr_ind = tracer_cnt

      do n=1,zooplankton_cnt
        tracer_cnt    = tracer_cnt + 1
        this%zoo_inds(n)%C_ind = tracer_cnt
      end do

      do n=1,autotroph_cnt
        tracer_cnt                = tracer_cnt + 1
        this%auto_inds(n)%Chl_ind = tracer_cnt

        tracer_cnt              = tracer_cnt + 1
        this%auto_inds(n)%C_ind = tracer_cnt

        if (lvariable_PtoC) then
          tracer_cnt              = tracer_cnt + 1
          this%auto_inds(n)%P_ind = tracer_cnt
        end if

        tracer_cnt               = tracer_cnt + 1
        this%auto_inds(n)%Fe_ind = tracer_cnt

        if (autotrophs_config(n)%silicifier) then
          tracer_cnt               = tracer_cnt + 1
          this%auto_inds(n)%Si_ind = tracer_cnt
        end if

        if (autotrophs_config(n)%imp_calcifier.or.                            &
            autotrophs_config(n)%exp_calcifier) then
          tracer_cnt                  = tracer_cnt + 1
          this%auto_inds(n)%CaCO3_ind = tracer_cnt
        end if
      end do
      this%ecosys_base%ind_end = tracer_cnt
      call this%ecosys_base%set_tracer_cnt()

      if (ciso_on) then
        ! Next tracer is start of the CISO tracers
        this%ciso%ind_beg = tracer_cnt + 1

        tracer_cnt     = tracer_cnt + 1
        this%di13c_ind = tracer_cnt

        tracer_cnt     = tracer_cnt + 1
        this%do13c_ind = tracer_cnt

        tracer_cnt     = tracer_cnt + 1
        this%di14c_ind = tracer_cnt

        tracer_cnt     = tracer_cnt + 1
        this%do14c_ind = tracer_cnt

        tracer_cnt      = tracer_cnt + 1
        this%zoo13C_ind = tracer_cnt

        tracer_cnt      = tracer_cnt + 1
        this%zoo14C_ind = tracer_cnt

        do n=1,autotroph_cnt
          tracer_cnt                = tracer_cnt + 1
          this%auto_inds(n)%C13_ind = tracer_cnt

          tracer_cnt                = tracer_cnt + 1
          this%auto_inds(n)%C14_ind = tracer_cnt

          if (autotrophs_config(n)%imp_calcifier .or. &
              autotrophs_config(n)%exp_calcifier) then
            tracer_cnt                    = tracer_cnt + 1
            this%auto_inds(n)%Ca13CO3_ind = tracer_cnt

            tracer_cnt                    = tracer_cnt + 1
            this%auto_inds(n)%Ca14CO3_ind = tracer_cnt
          end if

        end do

        this%ciso%ind_end = tracer_cnt
        call this%ciso%set_tracer_cnt()

      else

        this%ciso%cnt = 0
        this%ciso%ind_beg = 0
        this%ciso%ind_end = 0

      end if
    end associate

  end subroutine tracer_index_constructor

  !*****************************************************************************

  subroutine set_tracer_cnt(this)

    class(marbl_tracer_count_type), intent(inout) :: this

    this%cnt = this%ind_end - this%ind_beg + 1

  end subroutine set_tracer_cnt

  !*****************************************************************************

  subroutine surface_forcing_index_constructor(this, ciso_on, lflux_gas_o2,   &
             lflux_gas_co2, ladjust_bury_coeff, num_surface_forcing_fields)

    ! This subroutine sets the surface forcing indexes, which are used to
    ! determine what forcing fields are required from the driver.

    class(marbl_surface_forcing_indexing_type), intent(inout) :: this
    logical,                                    intent(in)    :: ciso_on
    logical,                                    intent(in)    :: lflux_gas_o2
    logical,                                    intent(in)    :: lflux_gas_co2
    logical,                                    intent(in)    :: ladjust_bury_coeff
    integer,                                    intent(out)   :: num_surface_forcing_fields

    associate(forcing_cnt => num_surface_forcing_fields)

      forcing_cnt = 0

      ! -------------------------------
      ! | Always request these fields |
      ! -------------------------------

      ! Square of 10m wind
      forcing_cnt = forcing_cnt + 1
      this%u10_sqr_id = forcing_cnt

      ! Sea-surface salinity
      forcing_cnt = forcing_cnt + 1
      this%sss_id = forcing_cnt

      ! Sea-surface temp
      forcing_cnt = forcing_cnt + 1
      this%sst_id = forcing_cnt

      ! Ice Fraction
      forcing_cnt = forcing_cnt + 1
      this%ifrac_id = forcing_cnt

      ! Dust Flux
      forcing_cnt = forcing_cnt + 1
      this%dust_flux_id = forcing_cnt

      ! Iron Flux
      forcing_cnt = forcing_cnt + 1
      this%iron_flux_id = forcing_cnt

      ! NOx Flux
      forcing_cnt = forcing_cnt + 1
      this%nox_flux_id = forcing_cnt

      ! NHy Flux
      forcing_cnt = forcing_cnt + 1
      this%nhy_flux_id = forcing_cnt

      ! ---------------------------------------------------------
      ! | Request these if bury coefficients are being adjusted |
      ! ---------------------------------------------------------

      if (ladjust_bury_coeff) then
        ! external C Flux
        forcing_cnt = forcing_cnt + 1
        this%ext_C_flux_id = forcing_cnt

        ! external P Flux
        forcing_cnt = forcing_cnt + 1
        this%ext_P_flux_id = forcing_cnt

        ! external Si Flux
        forcing_cnt = forcing_cnt + 1
        this%ext_Si_flux_id = forcing_cnt
      end if

      ! ------------------------------------------
      ! | Request these if gas fluxes are needed |
      ! ------------------------------------------

      if (lflux_gas_o2.or.lflux_gas_co2) then
        ! atm pressure
        forcing_cnt = forcing_cnt + 1
        this%atm_pressure_id = forcing_cnt
      end if

      if (lflux_gas_co2) then
        ! xco2
        forcing_cnt = forcing_cnt + 1
        this%xco2_id = forcing_cnt

        ! xco2_alt_co2
        forcing_cnt = forcing_cnt + 1
        this%xco2_alt_co2_id = forcing_cnt
      end if

      ! -----------------------------------
      ! | Request these fields if ciso_on |
      ! -----------------------------------

      if (ciso_on) then

        ! d13c
        forcing_cnt = forcing_cnt + 1
        this%d13c_id = forcing_cnt

        ! d14c
        forcing_cnt = forcing_cnt + 1
        this%d14c_id = forcing_cnt

      end if

    end associate

  end subroutine surface_forcing_index_constructor

  !*****************************************************************************

  subroutine interior_forcing_index_constructor(this,                         &
                                                tracer_names,                 &
                                                tracer_restore_vars,          &
                                                num_interior_forcing_fields,  &
                                                marbl_status_log)

    ! This subroutine sets the interior forcing indexes, which are used to
    ! determine what forcing fields are required from the driver.

    class(marbl_interior_forcing_indexing_type), intent(inout) :: this
    character(len=char_len), dimension(:),       intent(in)    :: tracer_names
    character(len=char_len), dimension(:),       intent(in)    :: tracer_restore_vars
    integer(int_kind),                           intent(out)   :: num_interior_forcing_fields
    type(marbl_log_type),                        intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_internal_types:interior_forcing_index_constructor'
    character(len=char_len)     :: log_message
    integer :: tracer_restore_cnt
    integer :: m, n

    associate(forcing_cnt => num_interior_forcing_fields, &
              marbl_total_tracer_cnt => size(tracer_names))

      forcing_cnt = 0

      ! -------------------------------
      ! | Always request these fields |
      ! -------------------------------

      ! Dust Flux
      forcing_cnt = forcing_cnt + 1
      this%dustflux_id = forcing_cnt

      ! PAR column fraction
      forcing_cnt = forcing_cnt + 1
      this%PAR_col_frac_id = forcing_cnt

      ! PAR column shortwave
      forcing_cnt = forcing_cnt + 1
      this%surf_shortwave_id = forcing_cnt

      ! Temperature
      forcing_cnt = forcing_cnt + 1
      this%temperature_id = forcing_cnt

      ! Salinity
      forcing_cnt = forcing_cnt + 1
      this%salinity_id = forcing_cnt

      ! Pressure
      forcing_cnt = forcing_cnt + 1
      this%pressure_id = forcing_cnt

      ! Iron Sediment Flux
      forcing_cnt = forcing_cnt + 1
      this%fesedflux_id = forcing_cnt

      ! Tracer restoring
      ! Note that this section
      ! (1) sets tracer_restore_cnt and allocate memory for restoring
      !     arrays
      ! (2) includes consistency check on the tracer_restore_vars array
      ! (3) writes all tracer restore fields to log

      tracer_restore_cnt = count((len_trim(tracer_restore_vars).gt.0))
      allocate(this%tracer_restore_id(marbl_total_tracer_cnt))
      this%tracer_restore_id = 0
      allocate(this%inv_tau_id(marbl_total_tracer_cnt))
      this%inv_tau_id = 0
      allocate(this%tracer_id(tracer_restore_cnt))
      this%tracer_id = 0

      if (tracer_restore_cnt .gt. 0) then
        log_message = "Restoring the following tracers to data:"
        call marbl_status_log%log_noerror(log_message, subname)
      end if

      do m=1,tracer_restore_cnt ! loop over tracer_restore_vars
        ! Check for empty strings in first tracer_restore_cnt elements of
        ! tracer_restore_vars
        if (len_trim(tracer_restore_vars(m)).eq.0) then
          log_message = "Empty string appears in middle of tracer_restore_vars!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if

        ! Check for duplicate tracers in tracer_restore_vars
        if (m .lt. marbl_total_tracer_cnt) then
          if (any(tracer_restore_vars(m).eq.tracer_restore_vars(m+1:))) then
            write(log_message,"(A,1X,A)") trim(tracer_restore_vars(m)),           &
                                  "appears in tracer_restore_vars more than once"
            call marbl_status_log%log_error(log_message, subname)
            return
          end if
        end if

        ! For each element
        do n=1,marbl_total_tracer_cnt ! loop over tracer_names
          if (trim(tracer_restore_vars(m)).eq.trim(tracer_names(n))) then
            forcing_cnt = forcing_cnt + 1
            this%tracer_restore_id(n) = forcing_cnt
            forcing_cnt = forcing_cnt + 1
            this%inv_tau_id(n) = forcing_cnt
            exit
          end if
        end do

        ! Check to make sure match was found
        if (n.le.marbl_total_tracer_cnt) then
          this%tracer_id(m) = n
          write(log_message, "(2A,I0,A)") trim(tracer_names(n)),              &
                                          " (tracer index: ", n, ')'
          call marbl_status_log%log_noerror(log_message, subname)
        else
          write(log_message, "(2A)") "Can not find tracer named ",            &
                trim(tracer_restore_vars(m))
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      end do

      if (tracer_restore_cnt .gt. 0) call marbl_status_log%log_noerror('', subname)

    end associate

  end subroutine interior_forcing_index_constructor

  !*****************************************************************************

#if 0
  ! FIXME #78: commented because auto_ind and zoo_ind are not allocatable yet
  subroutine grazing_parms_constructor(this, grazing_conf, marbl_status_log)

    class(grazing_parms_type), intent(inout) :: this
    type(grazing_config_type), intent(in)    :: grazing_conf
    type(marbl_log_type),      intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_internal_types:grazing_config_constructor'
    character(len=char_len)     :: log_message

    if (allocated(this%auto_ind)) then
      log_message = 'grazing%auto_inds is already allocated!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    if (allocated(this%zoo_ind)) then
      log_message = 'grazing%zoo_inds is already allocated!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    allocate(this%auto_ind(grazing_conf%auto_ind_cnt))
    allocate(this%zoo_ind(grazing_conf%zoo_ind_cnt))

  end subroutine grazing_parms_constructor
#endif

  !*****************************************************************************

end module marbl_internal_types

