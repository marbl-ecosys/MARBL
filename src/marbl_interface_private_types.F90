module marbl_interface_private_types

  ! module definitions of types that are internal to marbl

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len
  use marbl_kinds_mod, only : log_kind


  use marbl_logging, only : marbl_log_type

  use marbl_timing_mod, only : marbl_internal_timers_type
  use marbl_timing_mod, only : marbl_timer_indexing_type

  implicit none

  private

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

  !*****************************************************************************

  type, public :: autotroph_derived_terms_type
    real(r8), allocatable :: thetaC(:,:)          ! current Chl/C ratio (mg Chl/mmol C)
    real(r8), allocatable :: QCaCO3(:,:)          ! current CaCO3/C ratio (mmol CaCO3/mmol C)
    real(r8), allocatable :: Qp(:,:)              ! current P/C ratio (mmol P/mmol C)
    real(r8), allocatable :: gQp(:,:)             ! P/C for growth
    real(r8), allocatable :: Qfe(:,:)             ! current Fe/C ratio (mmol Fe/mmol C)
    real(r8), allocatable :: gQfe(:,:)            ! fe/C for growth
    real(r8), allocatable :: Qsi(:,:)             ! current Si/C ratio (mmol Si/mmol C)
    real(r8), allocatable :: gQsi(:,:)            ! diatom Si/C ratio for growth (new biomass)
    real(r8), allocatable :: VNO3(:,:)            ! NH4 uptake rate (non-dim)
    real(r8), allocatable :: VNH4(:,:)            ! NO3 uptake rate (non-dim)
    real(r8), allocatable :: VNtot(:,:)           ! total N uptake rate (non-dim)
    real(r8), allocatable :: NO3_V(:,:)           ! nitrate uptake (mmol NO3/m^3/sec)
    real(r8), allocatable :: NH4_V(:,:)           ! ammonium uptake (mmol NH4/m^3/sec)
    real(r8), allocatable :: PO4_V(:,:)           ! PO4 uptake (mmol PO4/m^3/sec)
    real(r8), allocatable :: DOP_V(:,:)           ! DOP uptake (mmol DOP/m^3/sec)
    real(r8), allocatable :: VPO4(:,:)            ! C-specific PO4 uptake (non-dim)
    real(r8), allocatable :: VDOP(:,:)            ! C-specific DOP uptake rate (non-dim)
    real(r8), allocatable :: VPtot(:,:)           ! total P uptake rate (non-dim)
    real(r8), allocatable :: f_nut(:,:)           ! nut limitation factor, modifies C fixation (non-dim)
    real(r8), allocatable :: VFe(:,:)             ! C-specific Fe uptake (non-dim)
    real(r8), allocatable :: VSiO3(:,:)           ! C-specific SiO3 uptake (non-dim)
    real(r8), allocatable :: light_lim(:,:)       ! light limitation factor
    real(r8), allocatable :: PCphoto(:,:)         ! C-specific rate of photosynth. (1/sec)
    real(r8), allocatable :: photoC(:,:)          ! C-fixation (mmol C/m^3/sec)
    real(r8), allocatable :: photoFe(:,:)         ! iron uptake
    real(r8), allocatable :: photoSi(:,:)         ! silicon uptake (mmol Si/m^3/sec)
    real(r8), allocatable :: photoacc(:,:)        ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
    real(r8), allocatable :: auto_loss(:,:)       ! autotroph non-grazing mort (mmol C/m^3/sec)
    real(r8), allocatable :: auto_loss_poc(:,:)   ! auto_loss routed to poc (mmol C/m^3/sec)
    real(r8), allocatable :: auto_loss_doc(:,:)   ! auto_loss routed to doc (mmol C/m^3/sec)
    real(r8), allocatable :: auto_loss_dic(:,:)   ! auto_loss routed to dic (mmol C/m^3/sec)
    real(r8), allocatable :: auto_agg(:,:)        ! autotroph aggregation (mmol C/m^3/sec)
    real(r8), allocatable :: auto_graze(:,:)      ! autotroph grazing rate (mmol C/m^3/sec)
    real(r8), allocatable :: auto_graze_zoo(:,:)  ! auto_graze routed to zoo (mmol C/m^3/sec)
    real(r8), allocatable :: auto_graze_poc(:,:)  ! auto_graze routed to poc (mmol C/m^3/sec)
    real(r8), allocatable :: auto_graze_doc(:,:)  ! auto_graze routed to doc (mmol C/m^3/sec)
    real(r8), allocatable :: auto_graze_dic(:,:)  ! auto_graze routed to dic (mmol C/m^3/sec)
    real(r8), allocatable :: Pprime(:,:)          ! used to limit autotroph mort at low biomass (mmol C/m^3)
    real(r8), allocatable :: CaCO3_form(:,:)      ! calcification of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
    real(r8), allocatable :: Nfix(:,:)            ! total Nitrogen fixation (mmol N/m^3/sec)
    real(r8), allocatable :: Nexcrete(:,:)        ! fixed N excretion
    real(r8), allocatable :: remaining_P_dop(:,:) ! remaining_P from grazing routed to DOP pool
    real(r8), allocatable :: remaining_P_pop(:,:) ! remaining_P from grazing routed to POP pool
    real(r8), allocatable :: remaining_P_dip(:,:) ! remaining_P from grazing routed to remin
  contains
    procedure, public :: construct => autotroph_derived_terms_constructor
    procedure, public :: destruct => autotroph_derived_terms_destructor
  end type autotroph_derived_terms_type

  !****************************************************************************

  type, public :: autotroph_local_type
    ! FIXME #316: replace with indices into tracer_local
    real(r8), allocatable :: Chl(:,:)     ! local copy of model autotroph Chl
    real(r8), allocatable :: C(:,:)       ! local copy of model autotroph C
    real(r8), allocatable :: P(:,:)       ! local copy of model autotroph P
    real(r8), allocatable :: Fe(:,:)      ! local copy of model autotroph Fe
    real(r8), allocatable :: Si(:,:)      ! local copy of model autotroph Si
    real(r8), allocatable :: CaCO3(:,:)   ! local copy of model autotroph CaCO3
    real(r8), allocatable :: C13(:,:)     ! local copy of model autotroph C13
    real(r8), allocatable :: C14(:,:)     ! local copy of model autotroph C14
    real(r8), allocatable :: Ca13CO3(:,:) ! local copy of model autotroph Ca13CO3
    real(r8), allocatable :: Ca14CO3(:,:) ! local copy of model autotroph Ca14CO3
  contains
    procedure, public :: construct => autotroph_local_constructor
    procedure, public :: destruct => autotroph_local_destructor
  end type autotroph_local_type

  !*****************************************************************************

  type, public :: zooplankton_derived_terms_type
    real(r8), allocatable :: f_zoo_detr(:,:)       ! frac of zoo losses into large detrital pool (non-dim)
    real(r8), allocatable :: x_graze_zoo(:,:)      ! {auto, zoo}_graze routed to zoo (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_graze(:,:)        ! zooplankton losses due to grazing (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_graze_zoo(:,:)    ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_graze_poc(:,:)    ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_graze_doc(:,:)    ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_graze_dic(:,:)    ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_loss(:,:)         ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_loss_poc(:,:)     ! zoo_loss routed to poc (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_loss_doc(:,:)     ! zoo_loss routed to doc (mmol C/m^3/sec)
    real(r8), allocatable :: zoo_loss_dic(:,:)     ! zoo_loss routed to dic (mmol C/m^3/sec)
    real(r8), allocatable :: Zprime(:,:)           ! used to limit zoo mort at low biomass (mmol C/m^3)
  contains
    procedure, public :: construct => zooplankton_derived_terms_constructor
    procedure, public :: destruct => zooplankton_derived_terms_destructor
  end type zooplankton_derived_terms_type

  !****************************************************************************

  type, public :: zooplankton_local_type
    real(r8), allocatable :: C(:,:)  ! local copy of model zooplankton C
  contains
    procedure, public :: construct => zooplankton_local_constructor
    procedure, public :: destruct => zooplankton_local_destructor
  end type zooplankton_local_type

  !***********************************************************************

  type, public :: zooplankton_share_type
     real(r8), allocatable :: zoototC_loc_fields(:)      ! local copy of model zooC
     real(r8), allocatable :: zootot_loss_fields(:)      ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
     real(r8), allocatable :: zootot_loss_poc_fields(:)  ! zoo_loss routed to large detrital (mmol C/m^3/sec)
     real(r8), allocatable :: zootot_loss_doc_fields(:)  ! zoo_loss routed to doc (mmol C/m^3/sec)
     real(r8), allocatable :: zootot_loss_dic_fields(:)  ! zoo_loss routed to dic (mmol C/m^3/sec)
     real(r8), allocatable :: zootot_graze_fields(:)     ! zooplankton losses due to grazing (mmol C/m^3/sec)
     real(r8), allocatable :: zootot_graze_zoo_fields(:) ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
     real(r8), allocatable :: zootot_graze_poc_fields(:) ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
     real(r8), allocatable :: zootot_graze_doc_fields(:) ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
     real(r8), allocatable :: zootot_graze_dic_fields(:) ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
   contains
     procedure, public :: construct => zooplankton_share_constructor
     procedure, public :: destruct => zooplankton_share_destructor
  end type zooplankton_share_type

  !****************************************************************************

  ! derived type for implicit handling of sinking particulate matter
  type, public :: column_sinking_particle_type
     real(r8) :: diss                       ! dissolution length for soft subclass
     real(r8) :: gamma                      ! fraction of production -> hard subclass
     real(r8) :: mass                       ! mass of 1e9 base units in g
     real(r8) :: rho                        ! QA mass ratio of POC to this particle class
     real(r8) :: to_floor                   ! flux hitting sea floor (base units/cm^s/sec)
     real(r8) :: flux_at_ref_depth          ! flux at particulate_flux_ref_depth (base units/cm^s/sec)
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

  ! This datatype is used to store internal surface flux computations in a way
  ! that makes it easy to pass to the diagnostics
  type, public :: marbl_surface_flux_internal_type
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
     procedure, public :: construct => marbl_surface_flux_internal_constructor
     procedure, public :: destruct => marbl_surface_flux_internal_destructor
  end type marbl_surface_flux_internal_type

  !****************************************************************************
  !
  ! Shared data type definitions
  !
  !****************************************************************************

  type, public :: marbl_interior_tendency_share_type
     real(r8), allocatable :: QA_dust_def(:)         ! incoming deficit in the QA(dust) POC flux
     real(r8), allocatable :: CO3_fields(:)
     real(r8), allocatable :: HCO3_fields(:)         ! bicarbonate ion
     real(r8), allocatable :: H2CO3_fields(:)        ! carbonic acid
     real(r8), allocatable :: CO3_sat_calcite(:)
     real(r8), allocatable :: DOCtot_loc_fields(:)   ! local copy of model DOC+DOCr
     real(r8), allocatable :: DOCtot_remin_fields(:) ! remineralization of DOC+DOCr (mmol C/m^3/sec)
   contains
     procedure, public :: construct => marbl_interior_tendency_share_constructor
     procedure, public :: destruct => marbl_interior_tendency_share_destructor
  end type marbl_interior_tendency_share_type

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
     real(r8), allocatable :: CO3(:)           ! carbonate ion
     real(r8), allocatable :: HCO3(:)          ! bicarbonate ion
     real(r8), allocatable :: H2CO3(:)         ! carbonic acid
     real(r8), allocatable :: pH(:)
     real(r8), allocatable :: CO3_sat_calcite(:)
     real(r8), allocatable :: CO3_sat_aragonite(:)
     real(r8), allocatable :: CO3_ALT_CO2(:)   ! carbonate ion, alternative CO2
     real(r8), allocatable :: HCO3_ALT_CO2(:)  ! bicarbonate ion, alternative CO2
     real(r8), allocatable :: H2CO3_ALT_CO2(:) ! carbonic acid, alternative CO2
     real(r8), allocatable :: pH_ALT_CO2(:)
   contains
     procedure, public :: construct => carbonate_constructor
     procedure, public :: destruct  => carbonate_destructor
  end type carbonate_type

  !*****************************************************************************

  type, public :: dissolved_organic_matter_type
     real(r8), allocatable :: DOC_prod(:)         ! production of DOC (mmol C/m^3/sec)
     real(r8), allocatable :: DOC_remin(:)        ! remineralization of DOC (mmol C/m^3/sec)
     real(r8), allocatable :: DOCr_remin(:)       ! remineralization of DOCr
     real(r8), allocatable :: DON_prod(:)         ! production of DON
     real(r8), allocatable :: DON_remin(:)        ! remineralization of DON
     real(r8), allocatable :: DONr_remin(:)       ! remineralization of DONr
     real(r8), allocatable :: DOP_prod(:)         ! production of DOP
     real(r8), allocatable :: DOP_remin(:)        ! remineralization of DOP
     real(r8), allocatable :: DOPr_remin(:)       ! remineralization of DOPr
     real(r8), allocatable :: DOP_loss_P_bal(:)   ! DOP loss, due to P budget balancing
   contains
     procedure, public :: construct => dissolved_organic_matter_constructor
     procedure, public :: destruct => dissolved_organic_matter_destructor
  end type dissolved_organic_matter_type

  !***********************************************************************

  type, public :: marbl_surface_flux_share_type
     real(r8), allocatable :: PV_SURF_fields       (:) ! piston velocity (cm/s)
     real(r8), allocatable :: DIC_SURF_fields      (:) ! surface values of DIC for solver
     real(r8), allocatable :: CO2STAR_SURF_fields  (:) ! CO2STAR from solver
     real(r8), allocatable :: DCO2STAR_SURF_fields (:) ! DCO2STAR from solver
     real(r8), allocatable :: CO3_SURF_fields      (:) ! Surface carbonate ion
   contains
     procedure, public :: construct => marbl_surface_flux_share_constructor
     procedure, public :: destruct => marbl_surface_flux_share_destructor
  end type marbl_surface_flux_share_type

  !*****************************************************************************

  type, public :: marbl_living_tracer_index_type
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
    integer(int_kind) :: cnt = 0
    ! Index ranges
    integer(int_kind) :: ind_beg = 0
    integer(int_kind) :: ind_end = 0
  contains
    procedure, public :: update_count
    procedure, public :: reset => tracer_count_reset
  end type marbl_tracer_count_type

  !*****************************************************************************

  type, public :: marbl_tracer_index_type
    ! Book-keeping (tracer count and index ranges)
    integer (int_kind) :: total_cnt = 0
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
    integer (int_kind) :: do13ctot_ind    = 0 ! dissolved organic carbon 13 (semi-labile+refractory)
    integer (int_kind) :: di14c_ind       = 0 ! dissolved inorganic carbon 14
    integer (int_kind) :: do14ctot_ind    = 0 ! dissolved organic carbon 14 (semi-labile+refractory)

    ! Living tracers
    type(marbl_living_tracer_index_type), allocatable :: auto_inds(:)
    type(marbl_living_tracer_index_type), allocatable :: zoo_inds(:)
    ! For CISO, don't want individual C13 and C14 tracers for each zooplankton
    ! Instead we collect them into one tracer for each isotope, regardless of
    ! zooplankton_cnt
    integer (int_kind) :: zootot13C_ind   = 0 ! total zooplankton carbon 13
    integer (int_kind) :: zootot14C_ind   = 0 ! total zooplankton carbon 14

  contains
    procedure, public :: add_tracer_index
    procedure, public :: construct => tracer_index_constructor
    procedure, public :: destruct => tracer_index_destructor
  end type marbl_tracer_index_type

  !****************************************************************************

  type, public :: marbl_surface_flux_forcing_indexing_type
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
     procedure, public :: construct => surface_flux_forcing_index_constructor
  end type marbl_surface_flux_forcing_indexing_type

  !****************************************************************************

  type, public :: marbl_interior_tendency_forcing_indexing_type
     ! Surface forcing fields that affect interior forcings
     integer(int_kind) :: dustflux_id        = 0
     integer(int_kind) :: PAR_col_frac_id    = 0
     integer(int_kind) :: surf_shortwave_id  = 0

     ! Column fields
     integer(int_kind) :: potemp_id      = 0
     integer(int_kind) :: salinity_id    = 0
     integer(int_kind) :: pressure_id    = 0
     integer(int_kind) :: fesedflux_id   = 0
     integer(int_kind) :: o2_consumption_scalef_id = 0
     integer(int_kind) :: p_remin_scalef_id = 0

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
     procedure, public :: construct => interior_tendency_forcing_index_constructor
  end type marbl_interior_tendency_forcing_indexing_type

  !*****************************************************************************

  type, public :: marbl_surface_flux_saved_state_indexing_type
    integer :: ph_surf = 0
    integer :: ph_alt_co2_surf = 0
  end type marbl_surface_flux_saved_state_indexing_type

  !*****************************************************************************

  type, public :: marbl_interior_tendency_saved_state_indexing_type
    integer :: ph_col = 0
    integer :: ph_alt_co2_col = 0
  end type marbl_interior_tendency_saved_state_indexing_type

  !***********************************************************************

  type, public :: marbl_surface_flux_diagnostics_indexing_type
     integer(int_kind) :: ECOSYS_IFRAC
     integer(int_kind) :: ECOSYS_XKW
     integer(int_kind) :: ECOSYS_ATM_PRESS
     integer(int_kind) :: PV_O2
     integer(int_kind) :: SCHMIDT_O2
     integer(int_kind) :: O2SAT
     integer(int_kind) :: CO2STAR
     integer(int_kind) :: DCO2STAR
     integer(int_kind) :: pCO2SURF
     integer(int_kind) :: DpCO2
     integer(int_kind) :: PV_CO2
     integer(int_kind) :: SCHMIDT_CO2
     integer(int_kind) :: DIC_GAS_FLUX
     integer(int_kind) :: PH
     integer(int_kind) :: ATM_CO2
     integer(int_kind) :: CO2STAR_ALT_CO2
     integer(int_kind) :: DCO2STAR_ALT_CO2
     integer(int_kind) :: pCO2SURF_ALT_CO2
     integer(int_kind) :: DpCO2_ALT_CO2
     integer(int_kind) :: DIC_GAS_FLUX_ALT_CO2
     integer(int_kind) :: PH_ALT_CO2
     integer(int_kind) :: ATM_ALT_CO2
     integer(int_kind) :: IRON_FLUX
     integer(int_kind) :: DUST_FLUX
     integer(int_kind) :: NOx_FLUX
     integer(int_kind) :: NHy_FLUX
     integer(int_kind) :: NHx_SURFACE_EMIS

     integer(int_kind) :: CISO_DI13C_GAS_FLUX       ! di13c flux
     integer(int_kind) :: CISO_DI14C_GAS_FLUX       ! di14c flux
     integer(int_kind) :: CISO_DI13C_AS_GAS_FLUX    ! air-sea di13c flux
     integer(int_kind) :: CISO_DI14C_AS_GAS_FLUX    ! air-sea di14c flux
     integer(int_kind) :: CISO_DI13C_SA_GAS_FLUX    ! sea-air di13c flux
     integer(int_kind) :: CISO_DI14C_SA_GAS_FLUX    ! sea-air di14c flux
     integer(int_kind) :: CISO_d13C_GAS_FLUX        ! surface ocean delta 13C
     integer(int_kind) :: CISO_d14C_GAS_FLUX        ! surface ocean delta 14C
     integer(int_kind) :: CISO_R13C_DIC_SURF        ! 13C/12C ratio in total DIC
     integer(int_kind) :: CISO_R14C_DIC_SURF        ! 14C/12C ratio in total DIC
     integer(int_kind) :: CISO_R13C_atm             ! atmospheric ratio of 13C/12C
     integer(int_kind) :: CISO_R14C_atm             ! atmospheric ratio of 14C/12C
     integer(int_kind) :: CISO_D13C_atm             ! atmospheric delta13C in permil
     integer(int_kind) :: CISO_D14C_atm             ! atmospheric delta14C in permil
     integer(int_kind) :: CISO_eps_aq_g_surf        ! tavg id for eps_aq_g_surf
     integer(int_kind) :: CISO_eps_dic_g_surf       ! tavg id for eps_dic_g_surf
  end type marbl_surface_flux_diagnostics_indexing_type

  !***********************************************************************

  type, public :: marbl_interior_tendency_diagnostics_indexing_type
    ! General 2D diags
    integer(int_kind) :: zsatcalc
    integer(int_kind) :: zsatarag
    integer(int_kind) :: O2_ZMIN
    integer(int_kind) :: O2_ZMIN_DEPTH
    integer(int_kind) :: photoC_TOT_zint
    integer(int_kind) :: photoC_TOT_zint_100m
    integer(int_kind) :: photoC_NO3_TOT_zint
    integer(int_kind) :: photoC_NO3_TOT_zint_100m
    integer(int_kind) :: DOC_prod_zint
    integer(int_kind) :: DOC_prod_zint_100m
    integer(int_kind) :: DOC_remin_zint
    integer(int_kind) :: DOC_remin_zint_100m
    integer(int_kind) :: DOCr_remin_zint
    integer(int_kind) :: DOCr_remin_zint_100m
    integer(int_kind) :: Jint_Ctot
    integer(int_kind) :: Jint_Ntot
    integer(int_kind) :: Jint_Ptot
    integer(int_kind) :: Jint_Sitot
    integer(int_kind) :: Jint_Fetot

    ! Particulate 2D diags
    integer(int_kind) :: calcToFloor
    integer(int_kind) :: calcToSed
    integer(int_kind) :: calcToSed_ALT_CO2
    integer(int_kind) :: pocToFloor
    integer(int_kind) :: pocToSed
    integer(int_kind) :: ponToSed
    integer(int_kind) :: SedDenitrif
    integer(int_kind) :: OtherRemin
    integer(int_kind) :: popToSed
    integer(int_kind) :: bsiToSed
    integer(int_kind) :: dustToSed
    integer(int_kind) :: pfeToSed

    ! Autotroph 2D diags
    integer(int_kind), allocatable :: N_lim_surf(:)
    integer(int_kind), allocatable :: N_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: P_lim_surf(:)
    integer(int_kind), allocatable :: P_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: Fe_lim_surf(:)
    integer(int_kind), allocatable :: Fe_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: SiO3_lim_surf(:)
    integer(int_kind), allocatable :: SiO3_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: light_lim_surf(:)
    integer(int_kind), allocatable :: light_lim_Cweight_avg_100m(:)
    integer(int_kind), allocatable :: photoC_zint(:)
    integer(int_kind), allocatable :: photoC_zint_100m(:)
    integer(int_kind), allocatable :: photoC_NO3_zint(:)
    integer(int_kind), allocatable :: CaCO3_form_zint(:)
    integer(int_kind), allocatable :: CaCO3_form_zint_100m(:)
    integer(int_kind), allocatable :: auto_graze_zint(:)
    integer(int_kind), allocatable :: auto_graze_zint_100m(:)
    integer(int_kind), allocatable :: auto_graze_poc_zint(:)
    integer(int_kind), allocatable :: auto_graze_poc_zint_100m(:)
    integer(int_kind), allocatable :: auto_graze_doc_zint(:)
    integer(int_kind), allocatable :: auto_graze_doc_zint_100m(:)
    integer(int_kind), allocatable :: auto_graze_zoo_zint(:)
    integer(int_kind), allocatable :: auto_graze_zoo_zint_100m(:)
    integer(int_kind), allocatable :: auto_loss_zint(:)
    integer(int_kind), allocatable :: auto_loss_zint_100m(:)
    integer(int_kind), allocatable :: auto_loss_poc_zint(:)
    integer(int_kind), allocatable :: auto_loss_poc_zint_100m(:)
    integer(int_kind), allocatable :: auto_loss_doc_zint(:)
    integer(int_kind), allocatable :: auto_loss_doc_zint_100m(:)
    integer(int_kind), allocatable :: auto_agg_zint(:)
    integer(int_kind), allocatable :: auto_agg_zint_100m(:)
    integer(int_kind) :: tot_CaCO3_form_zint
    integer(int_kind) :: tot_CaCO3_form_zint_100m

    ! Zooplankton 2D diags
    integer(int_kind), allocatable :: zoo_loss_zint(:)
    integer(int_kind), allocatable :: zoo_loss_zint_100m(:)
    integer(int_kind), allocatable :: zoo_loss_poc_zint(:)
    integer(int_kind), allocatable :: zoo_loss_poc_zint_100m(:)
    integer(int_kind), allocatable :: zoo_loss_doc_zint(:)
    integer(int_kind), allocatable :: zoo_loss_doc_zint_100m(:)
    integer(int_kind), allocatable :: zoo_graze_zint(:)
    integer(int_kind), allocatable :: zoo_graze_zint_100m(:)
    integer(int_kind), allocatable :: zoo_graze_poc_zint(:)
    integer(int_kind), allocatable :: zoo_graze_poc_zint_100m(:)
    integer(int_kind), allocatable :: zoo_graze_doc_zint(:)
    integer(int_kind), allocatable :: zoo_graze_doc_zint_100m(:)
    integer(int_kind), allocatable :: zoo_graze_zoo_zint(:)
    integer(int_kind), allocatable :: zoo_graze_zoo_zint_100m(:)
    integer(int_kind), allocatable :: x_graze_zoo_zint(:)
    integer(int_kind), allocatable :: x_graze_zoo_zint_100m(:)

    ! General 3D diags
    integer(int_kind) :: insitu_temp
    integer(int_kind) :: CO3
    integer(int_kind) :: HCO3
    integer(int_kind) :: H2CO3
    integer(int_kind) :: pH_3D
    integer(int_kind) :: CO3_ALT_CO2
    integer(int_kind) :: HCO3_ALT_CO2
    integer(int_kind) :: H2CO3_ALT_CO2
    integer(int_kind) :: pH_3D_ALT_CO2
    integer(int_kind) :: co3_sat_calc
    integer(int_kind) :: co3_sat_arag
    integer(int_kind) :: NITRIF
    integer(int_kind) :: DENITRIF
    integer(int_kind) :: O2_PRODUCTION
    integer(int_kind) :: O2_CONSUMPTION_SCALEF
    integer(int_kind) :: O2_CONSUMPTION
    integer(int_kind) :: AOU
    integer(int_kind) :: PAR_avg
    integer(int_kind) :: auto_graze_TOT
    integer(int_kind) :: photoC_TOT
    integer(int_kind) :: photoC_NO3_TOT
    integer(int_kind) :: DOC_prod
    integer(int_kind) :: DOC_remin
    integer(int_kind) :: DOCr_remin
    integer(int_kind) :: DON_prod
    integer(int_kind) :: DON_remin
    integer(int_kind) :: DONr_remin
    integer(int_kind) :: DOP_prod
    integer(int_kind) :: DOP_remin
    integer(int_kind) :: DOPr_remin
    integer(int_kind) :: DOP_loss_P_bal
    integer(int_kind) :: Fe_scavenge
    integer(int_kind) :: Fe_scavenge_rate
    integer(int_kind) :: Lig_prod
    integer(int_kind) :: Lig_loss
    integer(int_kind) :: Lig_scavenge
    integer(int_kind) :: Fefree
    integer(int_kind) :: Lig_photochem
    integer(int_kind) :: Lig_deg
    integer(int_kind) :: fesedflux

    ! Particulate 2D diags
    integer(int_kind) :: POC_FLUX_at_ref_depth
    integer(int_kind) :: POP_FLUX_at_ref_depth
    integer(int_kind) :: CaCO3_FLUX_at_ref_depth
    integer(int_kind) :: SiO2_FLUX_at_ref_depth
    integer(int_kind) :: P_iron_FLUX_at_ref_depth
    integer(int_kind) :: POC_PROD_zint
    integer(int_kind) :: POC_PROD_zint_100m
    integer(int_kind) :: POC_REMIN_DOCr_zint
    integer(int_kind) :: POC_REMIN_DOCr_zint_100m
    integer(int_kind) :: POC_REMIN_DIC_zint
    integer(int_kind) :: POC_REMIN_DIC_zint_100m
    integer(int_kind) :: CaCO3_PROD_zint
    integer(int_kind) :: CaCO3_PROD_zint_100m
    integer(int_kind) :: CaCO3_REMIN_zint
    integer(int_kind) :: CaCO3_REMIN_zint_100m

    ! Particulate 3D diags
    integer(int_kind) :: P_REMIN_SCALEF
    integer(int_kind) :: POC_FLUX_IN
    integer(int_kind) :: POC_sFLUX_IN
    integer(int_kind) :: POC_hFLUX_IN
    integer(int_kind) :: POC_PROD
    integer(int_kind) :: POC_REMIN_DOCr
    integer(int_kind) :: POC_REMIN_DIC
    integer(int_kind) :: POP_FLUX_IN
    integer(int_kind) :: POP_PROD
    integer(int_kind) :: POP_REMIN_DOPr
    integer(int_kind) :: POP_REMIN_PO4
    integer(int_kind) :: PON_REMIN_DONr
    integer(int_kind) :: PON_REMIN_NH4
    integer(int_kind) :: CaCO3_FLUX_IN
    integer(int_kind) :: CaCO3_PROD
    integer(int_kind) :: CaCO3_REMIN
    integer(int_kind) :: CaCO3_ALT_CO2_FLUX_IN
    integer(int_kind) :: CaCO3_ALT_CO2_PROD
    integer(int_kind) :: CaCO3_ALT_CO2_REMIN
    integer(int_kind) :: SiO2_FLUX_IN
    integer(int_kind) :: SiO2_PROD
    integer(int_kind) :: SiO2_REMIN
    integer(int_kind) :: dust_FLUX_IN
    integer(int_kind) :: dust_REMIN
    integer(int_kind) :: P_iron_FLUX_IN
    integer(int_kind) :: P_iron_PROD
    integer(int_kind) :: P_iron_REMIN

    ! Autotroph 3D diags
    integer(int_kind), allocatable :: Qp(:)
    integer(int_kind), allocatable :: photoC(:)
    integer(int_kind), allocatable :: photoC_NO3(:)
    integer(int_kind), allocatable :: photoFe(:)
    integer(int_kind), allocatable :: photoNO3(:)
    integer(int_kind), allocatable :: photoNH4(:)
    integer(int_kind), allocatable :: DOP_uptake(:)
    integer(int_kind), allocatable :: PO4_uptake(:)
    integer(int_kind), allocatable :: auto_graze(:)
    integer(int_kind), allocatable :: auto_graze_poc(:)
    integer(int_kind), allocatable :: auto_graze_doc(:)
    integer(int_kind), allocatable :: auto_graze_zoo(:)
    integer(int_kind), allocatable :: auto_loss(:)
    integer(int_kind), allocatable :: auto_loss_poc(:)
    integer(int_kind), allocatable :: auto_loss_doc(:)
    integer(int_kind), allocatable :: auto_agg(:)
    integer(int_kind), allocatable :: bSi_form(:)
    integer(int_kind), allocatable :: CaCO3_form(:)
    integer(int_kind), allocatable :: Nfix(:)
    integer(int_kind) :: tot_bSi_form
    integer(int_kind) :: tot_CaCO3_form
    integer(int_kind) :: tot_Nfix

    ! Zooplankton 3D diags
    integer(int_kind), allocatable :: zoo_loss(:)
    integer(int_kind), allocatable :: zoo_loss_poc(:)
    integer(int_kind), allocatable :: zoo_loss_doc(:)
    integer(int_kind), allocatable :: zoo_graze(:)
    integer(int_kind), allocatable :: zoo_graze_poc(:)
    integer(int_kind), allocatable :: zoo_graze_doc(:)
    integer(int_kind), allocatable :: zoo_graze_zoo(:)
    integer(int_kind), allocatable :: x_graze_zoo(:)

     !  ciso ids for nonstandard 3d fields
     integer (int_kind) :: CISO_PO13C_FLUX_IN                                 ! po13c flux into cell
     integer (int_kind) :: CISO_PO14C_FLUX_IN                                 ! po14c flux into cell
     integer (int_kind) :: CISO_PO13C_PROD                                    ! po13c production
     integer (int_kind) :: CISO_PO14C_PROD                                    ! po14c production
     integer (int_kind) :: CISO_PO13C_REMIN                                   ! po13c remineralization
     integer (int_kind) :: CISO_PO14C_REMIN                                   ! po14c remineralization
     integer (int_kind) :: CISO_Ca13CO3_PROD                                  ! ca13co3 production
     integer (int_kind) :: CISO_Ca14CO3_PROD                                  ! ca14co3 production
     integer (int_kind) :: CISO_Ca13CO3_REMIN                                 ! ca13co3 remineralization
     integer (int_kind) :: CISO_Ca14CO3_REMIN                                 ! ca14co3 remineralization
     integer (int_kind) :: CISO_Ca13CO3_FLUX_IN                               ! ca13co3 flux into cell
     integer (int_kind) :: CISO_Ca14CO3_FLUX_IN                               ! ca14co3 flux into cell
     integer (int_kind) :: CISO_photo13C_TOT                                  ! total 13C fixation
     integer (int_kind) :: CISO_photo14C_TOT                                  ! total 14C fixation
     integer (int_kind) :: CISO_photo13C_TOT_zint                             ! total 13C fixation vertical integral
     integer (int_kind) :: CISO_photo14C_TOT_zint                             ! total 14C fixation vertical integral

     ! ciso ids for  MORE nonstandard 3d fields
     integer (int_kind), allocatable :: CISO_eps_autotroph(:)       ! epsilon for each autotroph
     integer (int_kind), allocatable :: CISO_mui_to_co2star(:)      ! mui_to_co2star for each autotroph
     integer (int_kind), allocatable :: CISO_Ca13CO3_form(:)        ! Ca13CO3 formation
     integer (int_kind), allocatable :: CISO_Ca14CO3_form(:)        ! Ca14CO3 formation
     integer (int_kind), allocatable :: CISO_Ca13CO3_form_zint(:)   ! Ca13CO3 formation vertical integral 0-100 m
     integer (int_kind), allocatable :: CISO_Ca14CO3_form_zint(:)   ! Ca14CO3 formation vertical integral 0-100 m
     integer (int_kind), allocatable :: CISO_photo13C(:)            ! 13C fixation
     integer (int_kind), allocatable :: CISO_photo14C(:)            ! 14C fixation
     integer (int_kind), allocatable :: CISO_photo13C_zint(:)       ! 13C fixation vertical integral
     integer (int_kind), allocatable :: CISO_photo14C_zint(:)       ! 14C fixation vertical integral
     integer (int_kind), allocatable :: CISO_d13C(:)                ! d13C of autotroph carbon
     integer (int_kind), allocatable :: CISO_d14C(:)                ! d14C of autotroph carbon
     integer (int_kind), allocatable :: CISO_autotrophCaCO3_d14C(:) ! d14C of autotrophCaCO3
     integer (int_kind), allocatable :: CISO_autotrophCaCO3_d13C(:) ! d13C of autotrophCaCO3

     integer (int_kind) :: CISO_eps_aq_g                                      ! eps_aq_g
     integer (int_kind) :: CISO_eps_dic_g                                     ! eps_dic_g
     integer (int_kind) :: CISO_DO13Ctot_prod                                 ! do13ctot production
     integer (int_kind) :: CISO_DO14Ctot_prod                                 ! do14ctot production
     integer (int_kind) :: CISO_DO13Ctot_remin                                ! do13ctot remineralization
     integer (int_kind) :: CISO_DO14Ctot_remin                                ! do14ctot remineralization
     integer (int_kind) :: CISO_Jint_13Ctot                                   ! vertically integrated source sink term, 13Ctot
     integer (int_kind) :: CISO_Jint_14Ctot                                   ! vertically integrated source sink term, 14Ctot
     integer (int_kind) :: CISO_zoototC_d13C                                  ! d13C of total zooC
     integer (int_kind) :: CISO_zoototC_d14C                                  ! d14C of total zooC
     integer (int_kind) :: CISO_DOCtot_d13C                                   ! d13C of DOCtot
     integer (int_kind) :: CISO_DOCtot_d14C                                   ! d14C of DOCtot
     integer (int_kind) :: CISO_DIC_d13C                                      ! d13C of DIC
     integer (int_kind) :: CISO_DIC_d14C                                      ! d14C of DIC
     integer (int_kind) :: calcToSed_13C                                      ! calcite flux sedimentary burial
     integer (int_kind) :: calcToSed_14C                                      ! calcite flux sedimentary burial
     integer (int_kind) :: pocToSed_13C                                       ! poc burial flux to sediments
     integer (int_kind) :: pocToSed_14C                                       ! poc burial flux to sediments

     ! restoring 3D diags
     integer(int_kind), dimension(:), allocatable :: restore_tend
   contains
     procedure, public :: lconstructed => interior_diag_ind_constructed
     procedure, public :: destruct => interior_diag_ind_destructor
  end type marbl_interior_tendency_diagnostics_indexing_type

  !***********************************************************************

  ! marbl interface should use marbl_internal_timers_type and
  ! marbl_timer_indexing_type from here
  public :: marbl_internal_timers_type
  public :: marbl_timer_indexing_type

contains

  !***********************************************************************

  subroutine column_sinking_particle_constructor(this, num_levels)

    class(column_sinking_particle_type), intent(out) :: this

    integer (int_kind) :: num_levels

    allocate(this%sflux_in (num_levels))
    allocate(this%hflux_in (num_levels))
    allocate(this%prod     (num_levels))
    allocate(this%sflux_out(num_levels))
    allocate(this%hflux_out(num_levels))
    allocate(this%sed_loss (num_levels))
    allocate(this%remin    (num_levels))

  end subroutine column_sinking_particle_constructor

  !***********************************************************************

  subroutine column_sinking_particle_destructor(this)

    class(column_sinking_particle_type), intent(inout) :: this

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

    class(marbl_particulate_share_type), intent(out) :: this

    integer (int_kind) :: num_levels

    allocate(this%decay_CaCO3_fields               (num_levels))
    allocate(this%decay_POC_E_fields               (num_levels))
    allocate(this%decay_Hard_fields                (num_levels))
    allocate(this%poc_diss_fields                  (num_levels))
    allocate(this%caco3_diss_fields                (num_levels))
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

  !***********************************************************************

  subroutine marbl_particulate_share_destructor(this)

    class(marbl_particulate_share_type), intent(inout) :: this

    deallocate(this%decay_CaCO3_fields)
    deallocate(this%decay_POC_E_fields)
    deallocate(this%decay_Hard_fields)
    deallocate(this%poc_diss_fields)
    deallocate(this%caco3_diss_fields)
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

    subroutine carbonate_constructor(this, num_levels)

      class(carbonate_type), intent(inout) :: this
      integer (int_kind),    intent(in)    :: num_levels

      allocate(this%CO3(num_levels))
      allocate(this%HCO3(num_levels))
      allocate(this%H2CO3(num_levels))
      allocate(this%pH(num_levels))
      allocate(this%CO3_sat_calcite(num_levels))
      allocate(this%CO3_sat_aragonite(num_levels))
      allocate(this%CO3_ALT_CO2(num_levels))
      allocate(this%HCO3_ALT_CO2(num_levels))
      allocate(this%H2CO3_ALT_CO2(num_levels))
      allocate(this%pH_ALT_CO2(num_levels))

    end subroutine carbonate_constructor

    !***********************************************************************

     subroutine carbonate_destructor(this)

       class(carbonate_type), intent(inout) :: this

       deallocate(this%CO3)
       deallocate(this%HCO3)
       deallocate(this%H2CO3)
       deallocate(this%pH)
       deallocate(this%CO3_sat_calcite)
       deallocate(this%CO3_sat_aragonite)
       deallocate(this%CO3_ALT_CO2)
       deallocate(this%HCO3_ALT_CO2)
       deallocate(this%H2CO3_ALT_CO2)
       deallocate(this%pH_ALT_CO2)

     end subroutine carbonate_destructor

   !***********************************************************************

    subroutine dissolved_organic_matter_constructor(this, num_levels)

      class(dissolved_organic_matter_type), intent(inout) :: this
      integer (int_kind),                   intent(in)    :: num_levels

      allocate(this%DOC_prod(num_levels))
      allocate(this%DOC_remin(num_levels))
      allocate(this%DOCr_remin(num_levels))
      allocate(this%DON_prod(num_levels))
      allocate(this%DON_remin(num_levels))
      allocate(this%DONr_remin(num_levels))
      allocate(this%DOP_prod(num_levels))
      allocate(this%DOP_remin(num_levels))
      allocate(this%DOPr_remin(num_levels))
      allocate(this%DOP_loss_P_bal(num_levels))

    end subroutine dissolved_organic_matter_constructor

    !***********************************************************************

     subroutine dissolved_organic_matter_destructor(this)

       class(dissolved_organic_matter_type), intent(inout) :: this

       deallocate(this%DOC_prod)
       deallocate(this%DOC_remin)
       deallocate(this%DOCr_remin)
       deallocate(this%DON_prod)
       deallocate(this%DON_remin)
       deallocate(this%DONr_remin)
       deallocate(this%DOP_prod)
       deallocate(this%DOP_remin)
       deallocate(this%DOPr_remin)
       deallocate(this%DOP_loss_P_bal)

     end subroutine dissolved_organic_matter_destructor

  !***********************************************************************

   subroutine marbl_surface_flux_share_constructor(this, num_elements)

     class(marbl_surface_flux_share_type), intent(out) :: this

     integer (int_kind) , intent(in) :: num_elements

     allocate(this%PV_SURF_fields       (num_elements)) ! piston velocity (cm/s)
     allocate(this%DIC_SURF_fields      (num_elements)) ! Surface values of DIC for solver
     allocate(this%CO2STAR_SURF_fields  (num_elements)) ! CO2STAR from solver
     allocate(this%DCO2STAR_SURF_fields (num_elements)) ! DCO2STAR from solver
     allocate(this%CO3_SURF_fields      (num_elements)) ! Surface carbonate ion

   end subroutine marbl_surface_flux_share_constructor

   !***********************************************************************

   subroutine marbl_surface_flux_share_destructor(this)

     class(marbl_surface_flux_share_type), intent(inout) :: this

     if (allocated(this%PV_SURF_fields)) then
       deallocate(this%PV_SURF_fields      ) ! piston velocity (cm/s)
       deallocate(this%DIC_SURF_fields     ) ! Surface values of DIC for solver
       deallocate(this%CO2STAR_SURF_fields ) ! CO2STAR from solver
       deallocate(this%DCO2STAR_SURF_fields) ! DCO2STAR from solver
       deallocate(this%CO3_SURF_fields     ) ! Surface carbonate ion
     end if

   end subroutine

  !*****************************************************************************

  subroutine marbl_PAR_constructor(this, num_levels, num_PAR_subcols)

    class(marbl_PAR_type), intent(out) :: this
    integer,               intent(in)  :: num_levels
    integer,               intent(in)  :: num_PAR_subcols

    allocate(this%interface(0:num_levels,num_PAR_subcols))
    allocate(this%avg      (  num_levels,num_PAR_subcols))
    allocate(this%KPARdz   (  num_levels                ))
    allocate(this%col_frac (             num_PAR_subcols))

  end subroutine marbl_PAR_constructor

  !*****************************************************************************

  subroutine marbl_PAR_destructor(this)

    class(marbl_PAR_type) , intent(inout) :: this

    deallocate(this%interface)
    deallocate(this%avg      )
    deallocate(this%KPARdz   )
    deallocate(this%col_frac )

  end subroutine marbl_PAR_destructor

  !***********************************************************************

  subroutine autotroph_derived_terms_constructor(self, autotroph_cnt, km)

    class(autotroph_derived_terms_type), intent(inout) :: self
    integer,                             intent(in)    :: autotroph_cnt
    integer,                             intent(in)    :: km

    allocate(self%thetaC(autotroph_cnt, km))
    allocate(self%QCaCO3(autotroph_cnt, km))
    allocate(self%Qp(autotroph_cnt, km))
    allocate(self%gQp(autotroph_cnt, km))
    allocate(self%Qfe(autotroph_cnt, km))
    allocate(self%gQfe(autotroph_cnt, km))
    allocate(self%Qsi(autotroph_cnt, km))
    allocate(self%gQsi(autotroph_cnt, km))
    allocate(self%VNO3(autotroph_cnt, km))
    allocate(self%VNH4(autotroph_cnt, km))
    allocate(self%VNtot(autotroph_cnt, km))
    allocate(self%NO3_V(autotroph_cnt, km))
    allocate(self%NH4_V(autotroph_cnt, km))
    allocate(self%PO4_V(autotroph_cnt, km))
    allocate(self%DOP_V(autotroph_cnt, km))
    allocate(self%VPO4(autotroph_cnt, km))
    allocate(self%VDOP(autotroph_cnt, km))
    allocate(self%VPtot(autotroph_cnt, km))
    allocate(self%f_nut(autotroph_cnt, km))
    allocate(self%VFe(autotroph_cnt, km))
    allocate(self%VSiO3(autotroph_cnt, km))
    allocate(self%light_lim(autotroph_cnt, km))
    allocate(self%PCphoto(autotroph_cnt, km))
    allocate(self%photoC(autotroph_cnt, km))
    allocate(self%photoFe(autotroph_cnt, km))
    allocate(self%photoSi(autotroph_cnt, km))
    allocate(self%photoacc(autotroph_cnt, km))
    allocate(self%auto_loss(autotroph_cnt, km))
    allocate(self%auto_loss_poc(autotroph_cnt, km))
    allocate(self%auto_loss_doc(autotroph_cnt, km))
    allocate(self%auto_loss_dic(autotroph_cnt, km))
    allocate(self%auto_agg(autotroph_cnt, km))
    allocate(self%auto_graze(autotroph_cnt, km))
    allocate(self%auto_graze_zoo(autotroph_cnt, km))
    allocate(self%auto_graze_poc(autotroph_cnt, km))
    allocate(self%auto_graze_doc(autotroph_cnt, km))
    allocate(self%auto_graze_dic(autotroph_cnt, km))
    allocate(self%Pprime(autotroph_cnt, km))
    allocate(self%CaCO3_form(autotroph_cnt, km))
    allocate(self%Nfix(autotroph_cnt, km))
    allocate(self%Nexcrete(autotroph_cnt, km))
    allocate(self%remaining_P_dop(autotroph_cnt, km))
    allocate(self%remaining_P_pop(autotroph_cnt, km))
    allocate(self%remaining_P_dip(autotroph_cnt, km))

  end subroutine autotroph_derived_terms_constructor

  !*****************************************************************************

  subroutine autotroph_derived_terms_destructor(self)

    class(autotroph_derived_terms_type), intent(inout) :: self

    deallocate(self%thetaC)
    deallocate(self%QCaCO3)
    deallocate(self%Qp)
    deallocate(self%gQp)
    deallocate(self%Qfe)
    deallocate(self%gQfe)
    deallocate(self%Qsi)
    deallocate(self%gQsi)
    deallocate(self%VNO3)
    deallocate(self%VNH4)
    deallocate(self%VNtot)
    deallocate(self%NO3_V)
    deallocate(self%NH4_V)
    deallocate(self%PO4_V)
    deallocate(self%DOP_V)
    deallocate(self%VPO4)
    deallocate(self%VDOP)
    deallocate(self%VPtot)
    deallocate(self%f_nut)
    deallocate(self%VFe)
    deallocate(self%VSiO3)
    deallocate(self%light_lim)
    deallocate(self%PCphoto)
    deallocate(self%photoC)
    deallocate(self%photoFe)
    deallocate(self%photoSi)
    deallocate(self%photoacc)
    deallocate(self%auto_loss)
    deallocate(self%auto_loss_poc)
    deallocate(self%auto_loss_doc)
    deallocate(self%auto_loss_dic)
    deallocate(self%auto_agg)
    deallocate(self%auto_graze)
    deallocate(self%auto_graze_zoo)
    deallocate(self%auto_graze_poc)
    deallocate(self%auto_graze_doc)
    deallocate(self%auto_graze_dic)
    deallocate(self%Pprime)
    deallocate(self%CaCO3_form)
    deallocate(self%Nfix)
    deallocate(self%Nexcrete)
    deallocate(self%remaining_P_dop)
    deallocate(self%remaining_P_pop)
    deallocate(self%remaining_P_dip)

  end subroutine autotroph_derived_terms_destructor

  !***********************************************************************

  subroutine autotroph_local_constructor(self, ciso_on, autotroph_cnt, km)

    class(autotroph_local_type), intent(inout) :: self
    logical,                     intent(in)    :: ciso_on
    integer,                     intent(in)    :: autotroph_cnt
    integer,                     intent(in)    :: km

    allocate(self%Chl(autotroph_cnt, km))
    allocate(self%C(autotroph_cnt, km))
    allocate(self%P(autotroph_cnt, km))
    allocate(self%Fe(autotroph_cnt, km))
    allocate(self%Si(autotroph_cnt, km))
    allocate(self%CaCO3(autotroph_cnt, km))
    if (ciso_on) then
      allocate(self%C13(autotroph_cnt, km))
      allocate(self%C14(autotroph_cnt, km))
      allocate(self%Ca13CO3(autotroph_cnt, km))
      allocate(self%Ca14CO3(autotroph_cnt, km))
    else
      allocate(self%C13(0,0))
      allocate(self%C14(0,0))
      allocate(self%Ca13CO3(0,0))
      allocate(self%Ca14CO3(0,0))
    end if

  end subroutine autotroph_local_constructor

  !***********************************************************************

  subroutine autotroph_local_destructor(self)

    class(autotroph_local_type), intent(inout) :: self

    deallocate(self%Chl)
    deallocate(self%C)
    deallocate(self%P)
    deallocate(self%Fe)
    deallocate(self%Si)
    deallocate(self%CaCO3)
    deallocate(self%C13)
    deallocate(self%C14)
    deallocate(self%Ca13CO3)
    deallocate(self%Ca14CO3)

  end subroutine autotroph_local_destructor

  !***********************************************************************

  subroutine zooplankton_derived_terms_constructor(self, zooplankton_cnt, km)

    class(zooplankton_derived_terms_type), intent(inout) :: self
    integer,                               intent(in)    :: zooplankton_cnt
    integer,                               intent(in)    :: km

    allocate(self%f_zoo_detr(zooplankton_cnt, km))
    allocate(self%x_graze_zoo(zooplankton_cnt, km))
    allocate(self%zoo_graze(zooplankton_cnt, km))
    allocate(self%zoo_graze_zoo(zooplankton_cnt, km))
    allocate(self%zoo_graze_poc(zooplankton_cnt, km))
    allocate(self%zoo_graze_doc(zooplankton_cnt, km))
    allocate(self%zoo_graze_dic(zooplankton_cnt, km))
    allocate(self%zoo_loss(zooplankton_cnt, km))
    allocate(self%zoo_loss_poc(zooplankton_cnt, km))
    allocate(self%zoo_loss_doc(zooplankton_cnt, km))
    allocate(self%zoo_loss_dic(zooplankton_cnt, km))
    allocate(self%Zprime(zooplankton_cnt, km))

  end subroutine zooplankton_derived_terms_constructor

  !***********************************************************************

  subroutine zooplankton_derived_terms_destructor(self)

    class(zooplankton_derived_terms_type), intent(inout) :: self

    deallocate(self%f_zoo_detr)
    deallocate(self%x_graze_zoo)
    deallocate(self%zoo_graze)
    deallocate(self%zoo_graze_zoo)
    deallocate(self%zoo_graze_poc)
    deallocate(self%zoo_graze_doc)
    deallocate(self%zoo_graze_dic)
    deallocate(self%zoo_loss)
    deallocate(self%zoo_loss_poc)
    deallocate(self%zoo_loss_doc)
    deallocate(self%zoo_loss_dic)
    deallocate(self%Zprime)

  end subroutine zooplankton_derived_terms_destructor

  !***********************************************************************

  subroutine zooplankton_local_constructor(self, zooplankton_cnt, km)

    class(zooplankton_local_type), intent(inout) :: self
    integer,                       intent(in)    :: zooplankton_cnt
    integer,                       intent(in)    :: km

    allocate(self%C(zooplankton_cnt, km))

  end subroutine zooplankton_local_constructor

  !***********************************************************************

  subroutine zooplankton_local_destructor(self)

    class(zooplankton_local_type), intent(inout) :: self

    deallocate(self%C)

  end subroutine zooplankton_local_destructor

  !***********************************************************************

  subroutine zooplankton_share_constructor(self, km)

    class(zooplankton_share_type), intent(inout) :: self
    integer,                       intent(in)    :: km

    allocate(self%zoototC_loc_fields(km))
    allocate(self%zootot_loss_fields(km))
    allocate(self%zootot_loss_poc_fields(km))
    allocate(self%zootot_loss_doc_fields(km))
    allocate(self%zootot_loss_dic_fields(km))
    allocate(self%zootot_graze_fields(km))
    allocate(self%zootot_graze_zoo_fields(km))
    allocate(self%zootot_graze_poc_fields(km))
    allocate(self%zootot_graze_doc_fields(km))
    allocate(self%zootot_graze_dic_fields(km))

  end subroutine zooplankton_share_constructor

  !***********************************************************************

  subroutine zooplankton_share_destructor(self)

    class(zooplankton_share_type), intent(inout) :: self

    deallocate(self%zoototC_loc_fields)
    deallocate(self%zootot_loss_fields)
    deallocate(self%zootot_loss_poc_fields)
    deallocate(self%zootot_loss_doc_fields)
    deallocate(self%zootot_loss_dic_fields)
    deallocate(self%zootot_graze_fields)
    deallocate(self%zootot_graze_zoo_fields)
    deallocate(self%zootot_graze_poc_fields)
    deallocate(self%zootot_graze_doc_fields)
    deallocate(self%zootot_graze_dic_fields)

  end subroutine zooplankton_share_destructor

  !*****************************************************************************

  subroutine marbl_surface_flux_internal_constructor(this, num_elements)

    use marbl_constants_mod, only : c0

    class(marbl_surface_flux_internal_type), intent(out) :: this
    integer (int_kind),                      intent(in)  :: num_elements

    allocate(this%piston_velocity (num_elements))
    this%piston_velocity  = c0
    allocate(this%flux_co2        (num_elements))
    this%flux_co2         = c0
    allocate(this%flux_alt_co2    (num_elements))
    this%flux_alt_co2     = c0
    allocate(this%co2star         (num_elements))
    this%co2star          = c0
    allocate(this%dco2star        (num_elements))
    this%dco2star         = c0
    allocate(this%pco2surf        (num_elements))
    this%pco2surf         = c0
    allocate(this%dpco2           (num_elements))
    this%dpco2            = c0
    allocate(this%co3             (num_elements))
    this%co3              = c0
    allocate(this%co2star_alt     (num_elements))
    this%co2star_alt      = c0
    allocate(this%dco2star_alt    (num_elements))
    this%dco2star_alt     = c0
    allocate(this%pco2surf_alt    (num_elements))
    this%pco2surf_alt     = c0
    allocate(this%dpco2_alt       (num_elements))
    this%dpco2_alt        = c0
    allocate(this%schmidt_co2     (num_elements))
    this%schmidt_co2      = c0
    allocate(this%schmidt_o2      (num_elements))
    this%schmidt_o2       = c0
    allocate(this%pv_o2           (num_elements))
    this%pv_o2            = c0
    allocate(this%pv_co2          (num_elements))
    this%pv_co2           = c0
    allocate(this%o2sat           (num_elements))
    this%o2sat            = c0
    allocate(this%nhx_surface_emis(num_elements))
    this%nhx_surface_emis = c0

  end subroutine marbl_surface_flux_internal_constructor

  !***********************************************************************

  subroutine marbl_surface_flux_internal_destructor(this)

    class(marbl_surface_flux_internal_type), intent(inout) :: this

    if (allocated(this%piston_velocity)) then
      deallocate(this%piston_velocity )
      deallocate(this%flux_co2        )
      deallocate(this%flux_alt_co2    )
      deallocate(this%co2star         )
      deallocate(this%dco2star        )
      deallocate(this%pco2surf        )
      deallocate(this%dpco2           )
      deallocate(this%co3             )
      deallocate(this%co2star_alt     )
      deallocate(this%dco2star_alt    )
      deallocate(this%pco2surf_alt    )
      deallocate(this%dpco2_alt       )
      deallocate(this%schmidt_co2     )
      deallocate(this%schmidt_o2      )
      deallocate(this%pv_o2           )
      deallocate(this%pv_co2          )
      deallocate(this%o2sat           )
      deallocate(this%nhx_surface_emis)
    end if

  end subroutine marbl_surface_flux_internal_destructor

  !*****************************************************************************

  subroutine marbl_interior_tendency_share_constructor(this, num_levels)

    class(marbl_interior_tendency_share_type), intent(out) :: this
    integer (int_kind),                        intent(in)  :: num_levels

    allocate(this%QA_dust_def(num_levels))
    allocate(this%CO3_fields(num_levels))
    allocate(this%HCO3_fields(num_levels))
    allocate(this%H2CO3_fields(num_levels))
    allocate(this%CO3_sat_calcite(num_levels))
    allocate(this%DOCtot_loc_fields(num_levels))
    allocate(this%DOCtot_remin_fields(num_levels))

  end subroutine marbl_interior_tendency_share_constructor

  !***********************************************************************

  subroutine marbl_interior_tendency_share_destructor(this)

    class(marbl_interior_tendency_share_type), intent(inout) :: this

    deallocate(this%QA_dust_def)
    deallocate(this%CO3_fields)
    deallocate(this%HCO3_fields)
    deallocate(this%H2CO3_fields)
    deallocate(this%CO3_sat_calcite)
    deallocate(this%DOCtot_loc_fields)
    deallocate(this%DOCtot_remin_fields)

  end subroutine marbl_interior_tendency_share_destructor

  !*****************************************************************************

  subroutine tracer_index_constructor(this, ciso_on, lvariable_PtoC, autotroph_settings, &
             zooplankton_settings, marbl_status_log)

    ! This subroutine sets the tracer indices for the non-autotroph tracers. To
    ! know where to start the indexing for the autotroph tracers, it increments
    ! tracer_cnt by 1 for each tracer that is included. Note that this gives an
    ! accurate count whether the carbon isotope tracers are included or not.

    use marbl_pft_mod, only : autotroph_settings_type
    use marbl_pft_mod, only : zooplankton_settings_type

    class(marbl_tracer_index_type),  intent(out)   :: this
    logical,                         intent(in)    :: ciso_on
    logical,                         intent(in)    :: lvariable_PtoC
    type(autotroph_settings_type),   intent(in)    :: autotroph_settings(:)
    type(zooplankton_settings_type), intent(in)    :: zooplankton_settings(:)
    type(marbl_log_type),            intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_interface_private_types:tracer_index_constructor'
    character(len=char_len) :: ind_name
    integer :: autotroph_cnt, zooplankton_cnt, n

    autotroph_cnt = size(autotroph_settings)
    zooplankton_cnt = size(zooplankton_settings)

    !Allocate memory
    allocate(this%auto_inds(autotroph_cnt))
    allocate(this%zoo_inds(zooplankton_cnt))

    ! General ecosys tracers
    call this%add_tracer_index('po4', 'ecosys_base', this%po4_ind, marbl_status_log)
    call this%add_tracer_index('no3', 'ecosys_base', this%no3_ind, marbl_status_log)
    call this%add_tracer_index('sio3', 'ecosys_base', this%sio3_ind, marbl_status_log)
    call this%add_tracer_index('nh4', 'ecosys_base', this%nh4_ind, marbl_status_log)
    call this%add_tracer_index('fe', 'ecosys_base', this%fe_ind, marbl_status_log)
    call this%add_tracer_index('lig', 'ecosys_base', this%lig_ind, marbl_status_log)
    call this%add_tracer_index('o2', 'ecosys_base', this%o2_ind, marbl_status_log)
    call this%add_tracer_index('dic', 'ecosys_base', this%dic_ind, marbl_status_log)
    call this%add_tracer_index('dic_alt_co2', 'ecosys_base', this%dic_alt_co2_ind, marbl_status_log)
    call this%add_tracer_index('alk', 'ecosys_base', this%alk_ind, marbl_status_log)
    call this%add_tracer_index('alk_alt_co2', 'ecosys_base', this%alk_alt_co2_ind, marbl_status_log)
    call this%add_tracer_index('doc', 'ecosys_base', this%doc_ind, marbl_status_log)
    call this%add_tracer_index('don', 'ecosys_base', this%don_ind, marbl_status_log)
    call this%add_tracer_index('dop', 'ecosys_base', this%dop_ind, marbl_status_log)
    call this%add_tracer_index('dopr', 'ecosys_base', this%dopr_ind, marbl_status_log)
    call this%add_tracer_index('donr', 'ecosys_base', this%donr_ind, marbl_status_log)
    call this%add_tracer_index('docr', 'ecosys_base', this%docr_ind, marbl_status_log)

    do n=1,zooplankton_cnt
      write(ind_name, "(2A)") trim(zooplankton_settings(n)%sname), "C"
      call this%add_tracer_index(ind_name, 'ecosys_base', this%zoo_inds(n)%C_ind, marbl_status_log)
    end do

    do n=1,autotroph_cnt
      write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "Chl"
      call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%Chl_ind, marbl_status_log)

      write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "C"
      call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%C_ind, marbl_status_log)

      if (lvariable_PtoC) then
        write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "P"
        call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%P_ind, marbl_status_log)
      end if

      write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "Fe"
      call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%Fe_ind, marbl_status_log)

      if (autotroph_settings(n)%silicifier) then
        write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "Si"
        call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%Si_ind, marbl_status_log)
      end if

      if (autotroph_settings(n)%imp_calcifier.or. &
          autotroph_settings(n)%exp_calcifier) then
        write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "CaCO3"
        call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%CaCO3_ind, marbl_status_log)
      end if
    end do

    if (ciso_on) then
      call this%add_tracer_index('di13c',    'ciso', this%di13c_ind,    marbl_status_log)
      call this%add_tracer_index('do13ctot', 'ciso', this%do13ctot_ind, marbl_status_log)
      call this%add_tracer_index('di14c',    'ciso', this%di14c_ind,    marbl_status_log)
      call this%add_tracer_index('do14ctot', 'ciso', this%do14ctot_ind, marbl_status_log)
      call this%add_tracer_index('zootot13C',   'ciso', this%zootot13C_ind,   marbl_status_log)
      call this%add_tracer_index('zootot14C',   'ciso', this%zootot14C_ind,   marbl_status_log)

      do n=1,autotroph_cnt
        write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "C13"
        call this%add_tracer_index(ind_name, 'ciso', this%auto_inds(n)%C13_ind, marbl_status_log)

        write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "C14"
        call this%add_tracer_index(ind_name, 'ciso', this%auto_inds(n)%C14_ind, marbl_status_log)

        if (autotroph_settings(n)%imp_calcifier .or. &
            autotroph_settings(n)%exp_calcifier) then
        write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "Ca13CO3"
          call this%add_tracer_index(ind_name, 'ciso', this%auto_inds(n)%Ca13CO3_ind, marbl_status_log)

        write(ind_name, "(2A)") trim(autotroph_settings(n)%sname), "Ca14CO3"
          call this%add_tracer_index(ind_name, 'ciso', this%auto_inds(n)%Ca14CO3_ind, marbl_status_log)
        end if
      end do
    end if

    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace("add_tracer_index", subname)
      return
    end if

  end subroutine tracer_index_constructor

  !*****************************************************************************

  subroutine tracer_index_destructor(this)

    class(marbl_tracer_index_type), intent(inout) :: this

    ! Zero out counts
    this%total_cnt = 0
    call this%ecosys_base%reset()
    call this%ciso%reset()

    ! Deallocate memory
    if (allocated(this%auto_inds)) then
      deallocate(this%auto_inds)
      deallocate(this%zoo_inds)
    end if

  end subroutine tracer_index_destructor

  !*****************************************************************************

  subroutine tracer_count_reset(this)
    class(marbl_tracer_count_type), intent(inout) :: this
    this%cnt = 0
    this%ind_beg = 0
    this%ind_end = 0
  end subroutine tracer_count_reset

  !*****************************************************************************

  subroutine add_tracer_index(this, ind_name, category, ind, marbl_status_log)

    class(marbl_tracer_index_type), intent(inout) :: this
    character(len=*),               intent(in)    :: ind_name
    character(len=*),               intent(in)    :: category
    integer(int_kind),              intent(out)   :: ind
    type(marbl_log_type),           intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_interface_private_types:add_tracer_index'
    character(len=char_len)     :: log_message

    ! This routine may be called multiple times after an error has been logged
    ! (tracer_index_constructor doesn't check log status until all indices have
    ! been added)
    if (marbl_status_log%labort_marbl) return

    ind = this%total_cnt+1
    select case (trim(category))
      case ('ecosys_base')
        call this%ecosys_base%update_count(ind, marbl_status_log)
      case ('ciso')
        call this%ciso%update_count(ind, marbl_status_log)
      case DEFAULT
        write(log_message, "(A,1X,A)") trim(category), &
              'is not a recognized tracer module!'
        call marbl_status_log%log_error(log_message, subname)
        write(log_message, "(2A)") "Error triggered by ", trim(ind_name)
        call marbl_status_log%log_error(log_message, subname)
        return
      ! TO-DO add default case with "category not found" error
    end select

    if (marbl_status_log%labort_marbl) then
      write(log_message, "(2A)") trim(category), "%update_count"
      call marbl_status_log%log_error_trace(log_message, subname)
      write(log_message, "(2A)") "Error triggered by ", trim(ind_name)
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    this%total_cnt = ind

  end subroutine add_tracer_index

  !*****************************************************************************

  subroutine update_count(this, ind, marbl_status_log)

    class(marbl_tracer_count_type), intent(inout) :: this
    integer(int_kind),              intent(in)    :: ind
    type(marbl_log_type),           intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_interface_private_types:update_count'
    character(len=char_len)     :: log_message

    ! (1) Make sure tracer modules have contiguous indices
    if ((this%ind_end .ne. 0) .and. (this%ind_end .ne. ind-1)) then
      write(log_message, "(2A,I0,A,I0)") "Can not add another tracer to this module", &
            " current tracer index is ", ind, " and last tracer in module is ",       &
            this%ind_end
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    ! (2) If this is first tracer in module, set ind_beg
    if (this%ind_beg .eq. 0) this%ind_beg = ind

    ! (3) Update ind_end and total count
    this%ind_end = ind
    this%cnt = this%cnt + 1

  end subroutine update_count

  !*****************************************************************************

  subroutine surface_flux_forcing_index_constructor(this, ciso_on, lflux_gas_o2,   &
             lflux_gas_co2, ladjust_bury_coeff, num_surface_flux_forcing_fields)

    ! This subroutine sets the surface forcing indices, which are used to
    ! determine what forcing fields are required from the driver.

    class(marbl_surface_flux_forcing_indexing_type), intent(out) :: this
    logical,                                         intent(in)  :: ciso_on
    logical,                                         intent(in)  :: lflux_gas_o2
    logical,                                         intent(in)  :: lflux_gas_co2
    logical,                                         intent(in)  :: ladjust_bury_coeff
    integer,                                         intent(out) :: num_surface_flux_forcing_fields

    associate(forcing_cnt => num_surface_flux_forcing_fields)

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

  end subroutine surface_flux_forcing_index_constructor

  !*****************************************************************************

  subroutine interior_tendency_forcing_index_constructor(this,                                 &
                                                         tracer_names,                         &
                                                         tracer_restore_vars,                  &
                                                         num_PAR_subcols,                      &
                                                         num_interior_tendency_forcing_fields, &
                                                         marbl_status_log)

    ! This subroutine sets the interior forcing indexes, which are used to
    ! determine what forcing fields are required from the driver.

    use marbl_settings_mod, only : lo2_consumption_scalef
    use marbl_settings_mod, only : lp_remin_scalef

    class(marbl_interior_tendency_forcing_indexing_type), intent(out)   :: this
    character(len=char_len), dimension(:),                intent(in)    :: tracer_names
    character(len=char_len), dimension(:),                intent(in)    :: tracer_restore_vars
    integer(int_kind),                                    intent(in)    :: num_PAR_subcols
    integer(int_kind),                                    intent(out)   :: num_interior_tendency_forcing_fields
    type(marbl_log_type),                                 intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_interface_private_types:interior_tendency_forcing_index_constructor'
    character(len=char_len)     :: log_message

    integer :: tracer_restore_cnt, tracer_cnt
    integer :: m, n

    associate(forcing_cnt => num_interior_tendency_forcing_fields)

      tracer_cnt = size(tracer_names)

      forcing_cnt = 0

      ! Dust Flux
      forcing_cnt = forcing_cnt + 1
      this%dustflux_id = forcing_cnt

      ! PAR column fraction (not needed if num_PAR_subcols = 1)
      if (num_PAR_subcols .gt. 1) then
        forcing_cnt = forcing_cnt + 1
        this%PAR_col_frac_id = forcing_cnt
      end if

      ! PAR column shortwave
      forcing_cnt = forcing_cnt + 1
      this%surf_shortwave_id = forcing_cnt

      ! Potential Temperature
      forcing_cnt = forcing_cnt + 1
      this%potemp_id = forcing_cnt

      ! Salinity
      forcing_cnt = forcing_cnt + 1
      this%salinity_id = forcing_cnt

      ! Pressure
      forcing_cnt = forcing_cnt + 1
      this%pressure_id = forcing_cnt

      ! Iron Sediment Flux
      forcing_cnt = forcing_cnt + 1
      this%fesedflux_id = forcing_cnt

      ! O2 Consumption Scale Factor
      if (lo2_consumption_scalef) then
        forcing_cnt = forcing_cnt + 1
        this%o2_consumption_scalef_id = forcing_cnt
      endif

      ! Particulate Remin Scale Factor
      if (lp_remin_scalef) then
        forcing_cnt = forcing_cnt + 1
        this%p_remin_scalef_id = forcing_cnt
      endif

      ! Tracer restoring
      ! Note that this section
      ! (1) sets tracer_restore_cnt and allocate memory for restoring
      !     arrays
      ! (2) includes consistency check on the tracer_restore_vars array
      ! (3) writes all tracer restore fields to log

      tracer_restore_cnt = count((len_trim(tracer_restore_vars).gt.0))
      allocate(this%tracer_restore_id(tracer_cnt))
      this%tracer_restore_id = 0
      allocate(this%inv_tau_id(tracer_cnt))
      this%inv_tau_id = 0
      allocate(this%tracer_id(tracer_restore_cnt))
      this%tracer_id = 0

      do m=1,tracer_restore_cnt ! loop over tracer_restore_vars
        ! Check for empty strings in first tracer_restore_cnt elements of
        ! tracer_restore_vars
        if (len_trim(tracer_restore_vars(m)).eq.0) then
          log_message = "Empty string appears in middle of tracer_restore_vars!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if

        ! Check for duplicate tracers in tracer_restore_vars
        if (m .lt. tracer_cnt) then
          if (any(tracer_restore_vars(m).eq.tracer_restore_vars(m+1:))) then
            write(log_message,"(A,1X,A)") trim(tracer_restore_vars(m)),           &
                                  "appears in tracer_restore_vars more than once"
            call marbl_status_log%log_error(log_message, subname)
            return
          end if
        end if

        ! For each element
        do n=1,tracer_cnt ! loop over tracer_names
          if (trim(tracer_restore_vars(m)).eq.trim(tracer_names(n))) then
            forcing_cnt = forcing_cnt + 1
            this%tracer_restore_id(n) = forcing_cnt
            forcing_cnt = forcing_cnt + 1
            this%inv_tau_id(n) = forcing_cnt
            exit
          end if
        end do

        ! Check to make sure match was found
        if (n.le.tracer_cnt) then
          this%tracer_id(m) = n
        else
          write(log_message, "(2A)") "Can not find tracer named ",            &
                trim(tracer_restore_vars(m))
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      end do

      if (tracer_restore_cnt .gt. 0) call marbl_status_log%log_noerror('', subname)

    end associate

  end subroutine interior_tendency_forcing_index_constructor

  !*****************************************************************************

  function interior_diag_ind_constructed(this) result(constructed)

    class(marbl_interior_tendency_diagnostics_indexing_type), intent(inout) :: this
    logical(log_kind) :: constructed

    constructed = allocated(this%restore_tend)

  end function interior_diag_ind_constructed

  !*****************************************************************************

  subroutine interior_diag_ind_destructor(this)

    use marbl_settings_mod, only : ciso_on

    class(marbl_interior_tendency_diagnostics_indexing_type), intent(inout) :: this

    if (this%lconstructed()) then
      deallocate(this%N_lim_surf)
      deallocate(this%N_lim_Cweight_avg_100m)
      deallocate(this%P_lim_surf)
      deallocate(this%P_lim_Cweight_avg_100m)
      deallocate(this%Fe_lim_surf)
      deallocate(this%Fe_lim_Cweight_avg_100m)
      deallocate(this%SiO3_lim_surf)
      deallocate(this%SiO3_lim_Cweight_avg_100m)
      deallocate(this%light_lim_surf)
      deallocate(this%light_lim_Cweight_avg_100m)
      deallocate(this%photoC_zint)
      deallocate(this%photoC_zint_100m)
      deallocate(this%photoC_NO3_zint)
      deallocate(this%CaCO3_form_zint)
      deallocate(this%CaCO3_form_zint_100m)
      deallocate(this%auto_graze_zint)
      deallocate(this%auto_graze_zint_100m)
      deallocate(this%auto_graze_poc_zint)
      deallocate(this%auto_graze_poc_zint_100m)
      deallocate(this%auto_graze_doc_zint)
      deallocate(this%auto_graze_doc_zint_100m)
      deallocate(this%auto_graze_zoo_zint)
      deallocate(this%auto_graze_zoo_zint_100m)
      deallocate(this%auto_loss_zint)
      deallocate(this%auto_loss_zint_100m)
      deallocate(this%auto_loss_poc_zint)
      deallocate(this%auto_loss_poc_zint_100m)
      deallocate(this%auto_loss_doc_zint)
      deallocate(this%auto_loss_doc_zint_100m)
      deallocate(this%auto_agg_zint)
      deallocate(this%auto_agg_zint_100m)
      deallocate(this%zoo_loss_zint)
      deallocate(this%zoo_loss_zint_100m)
      deallocate(this%zoo_loss_poc_zint)
      deallocate(this%zoo_loss_poc_zint_100m)
      deallocate(this%zoo_loss_doc_zint)
      deallocate(this%zoo_loss_doc_zint_100m)
      deallocate(this%zoo_graze_zint)
      deallocate(this%zoo_graze_zint_100m)
      deallocate(this%zoo_graze_poc_zint)
      deallocate(this%zoo_graze_poc_zint_100m)
      deallocate(this%zoo_graze_doc_zint)
      deallocate(this%zoo_graze_doc_zint_100m)
      deallocate(this%zoo_graze_zoo_zint)
      deallocate(this%zoo_graze_zoo_zint_100m)
      deallocate(this%x_graze_zoo_zint)
      deallocate(this%x_graze_zoo_zint_100m)
      deallocate(this%Qp)
      deallocate(this%photoC)
      deallocate(this%photoC_NO3)
      deallocate(this%photoFe)
      deallocate(this%photoNO3)
      deallocate(this%photoNH4)
      deallocate(this%DOP_uptake)
      deallocate(this%PO4_uptake)
      deallocate(this%auto_graze)
      deallocate(this%auto_graze_poc)
      deallocate(this%auto_graze_doc)
      deallocate(this%auto_graze_zoo)
      deallocate(this%auto_loss)
      deallocate(this%auto_loss_poc)
      deallocate(this%auto_loss_doc)
      deallocate(this%auto_agg)
      deallocate(this%bSi_form)
      deallocate(this%CaCO3_form)
      deallocate(this%Nfix)
      deallocate(this%zoo_loss)
      deallocate(this%zoo_loss_poc)
      deallocate(this%zoo_loss_doc)
      deallocate(this%zoo_graze)
      deallocate(this%zoo_graze_poc)
      deallocate(this%zoo_graze_doc)
      deallocate(this%zoo_graze_zoo)
      deallocate(this%x_graze_zoo)
      if (ciso_on) then
        deallocate(this%CISO_eps_autotroph)
        deallocate(this%CISO_mui_to_co2star)
        deallocate(this%CISO_Ca13CO3_form)
        deallocate(this%CISO_Ca14CO3_form)
        deallocate(this%CISO_Ca13CO3_form_zint)
        deallocate(this%CISO_Ca14CO3_form_zint)
        deallocate(this%CISO_photo13C)
        deallocate(this%CISO_photo14C)
        deallocate(this%CISO_photo13C_zint)
        deallocate(this%CISO_photo14C_zint)
        deallocate(this%CISO_d13C)
        deallocate(this%CISO_d14C)
        deallocate(this%CISO_autotrophCaCO3_d14C)
        deallocate(this%CISO_autotrophCaCO3_d13C)
      end if
      deallocate(this%restore_tend)
    end if

  end subroutine interior_diag_ind_destructor

  !*****************************************************************************

end module marbl_interface_private_types
