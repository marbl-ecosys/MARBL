module marbl_pft_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_constants_mod, only : c0, c1

  use marbl_logging, only : marbl_log_type

  implicit none
  private

  real(r8), parameter :: UnsetValue = 1e34_r8

  !****************************************************************************
  ! derived types for autotrophs

  type, public :: autotroph_type
    character(len=char_len) :: sname
    character(len=char_len) :: lname
    logical(log_kind)       :: Nfixer                             ! flag set to true if this autotroph fixes N2
    logical(log_kind)       :: imp_calcifier                      ! flag set to true if this autotroph implicitly handles calcification
    logical(log_kind)       :: exp_calcifier                      ! flag set to true if this autotroph explicitly handles calcification
    logical(log_kind)       :: silicifier                         ! flag set to true if this autotroph is a silicifier

    real(r8)                :: kFe, kPO4, kDOP, kNO3, kNH4, kSiO3 ! nutrient uptake half-sat constants
    real(r8)                :: Qp_fixed                           ! P/C ratio for fixed P/C ratios
    real(r8)                :: gQfe_0, gQfe_min                   ! initial and minimum Fe/C ratio for growth
    real(r8)                :: alphaPI_per_day                    ! init slope of P_I curve (GD98) (mmol C m^2/(mg Chl W day))
    real(r8)                :: alphaPI                            ! init slope of P_I curve (GD98) (mmol C m^2/(mg Chl W sec))
                                                                 !    (derived from alphaPI_per_day)
    real(r8)                :: PCref_per_day                      ! max C-spec. grth rate at tref (1/day)
    real(r8)                :: PCref                              ! max C-spec. grth rate at tref (1/sec) (derived from PCref_per_day)
    real(r8)                :: thetaN_max                         ! max thetaN (Chl/N) (mg Chl/mmol N)
    real(r8)                :: loss_thres, loss_thres2            ! conc. where losses go to zero
    real(r8)                :: temp_thres                         ! Temp. where concentration threshold and photosynth. rate drops
    real(r8)                :: mort_per_day, mort2_per_day        ! linear and quadratic mortality rates (1/day), (1/day/((mmol C/m3))
    real(r8)                :: mort, mort2                        ! linear and quadratic mortality rates (1/sec), (1/sec/((mmol C/m3))
                                                                 !    (derived from mort_per_day and mort2_per_day)
    real(r8)                :: agg_rate_max, agg_rate_min         ! max and min agg. rate (1/d)
    real(r8)                :: loss_poc                           ! routing of loss term
  contains
    procedure, public :: set_to_default => autotroph_set_to_default
  end type autotroph_type

  !****************************************************************************
  ! derived types for zooplankton

  type, public :: zooplankton_type
     character(len=char_len) :: sname
     character(len=char_len) :: lname
     real(r8)                :: z_mort_0_per_day   ! zoo linear mort rate (1/day)
     real(r8)                :: z_mort_0           ! zoo linear mort rate (1/sec) (derived from z_mort_0_per_day)
     real(r8)                :: z_mort2_0_per_day  ! zoo quad mort rate (1/day/((mmol C/m3))
     real(r8)                :: z_mort2_0          ! zoo quad mort rate (1/sec/((mmol C/m3)) (derived from z_mort2_0_per_day)
     real(r8)                :: loss_thres         ! zoo conc. where losses go to zero
   contains
     procedure, public :: set_to_default => zooplankton_set_to_default
  end type zooplankton_type

  !****************************************************************************
  ! derived types for grazing

  type, public :: grazing_type
    character(len=char_len) :: sname
    character(len=char_len) :: lname
    integer(int_kind)       :: auto_ind_cnt     ! number of autotrophs in prey-clase auto_ind
    integer(int_kind)       :: zoo_ind_cnt      ! number of zooplankton in prey-clase zoo_ind
    integer(int_kind)       :: grazing_function ! functional form of grazing parameterization
    real(r8)                :: z_umax_0_per_day ! max zoo growth rate at tref (1/day)
    real(r8)                :: z_umax_0         ! max zoo growth rate at tref (1/sec) (derived from z_umax_0_per_day)
    real(r8)                :: z_grz            ! grazing coef. (mmol C/m^3)^2
    real(r8)                :: graze_zoo        ! routing of grazed term, remainder goes to dic
    real(r8)                :: graze_poc        ! routing of grazed term, remainder goes to dic
    real(r8)                :: graze_doc        ! routing of grazed term, remainder goes to dic
    real(r8)                :: f_zoo_detr       ! fraction of zoo losses to detrital
    integer(int_kind), allocatable :: auto_ind(:)
    integer(int_kind), allocatable :: zoo_ind(:)
  contains
    procedure, public :: set_to_default => grazing_set_to_default
    procedure, public :: construct => grazing_constructor
  end type grazing_type

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

  type, public :: marbl_zooplankton_share_type
     real(r8) :: zooC_loc_fields     ! local copy of model zooC
     real(r8) :: zoo_loss_fields     ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
     real(r8) :: zoo_loss_poc_fields ! zoo_loss routed to large detrital (mmol C/m^3/sec)
     real(r8) :: zoo_loss_doc_fields ! zoo_loss routed to doc (mmol C/m^3/sec)
     real(r8) :: zoo_loss_dic_fields ! zoo_loss routed to dic (mmol C/m^3/sec)
  end type marbl_zooplankton_share_type

  !*****************************************************************************

  type, public :: autotroph_secondary_species_type
     real(r8) :: thetaC          ! current Chl/C ratio (mg Chl/mmol C)
     real(r8) :: QCaCO3          ! current CaCO3/C ratio (mmol CaCO3/mmol C)
     real(r8) :: Qp              ! current P/C ratio (mmol P/mmol C)
     real(r8) :: gQp             ! P/C for growth
     real(r8) :: Qfe             ! current Fe/C ratio (mmol Fe/mmol C)
     real(r8) :: gQfe            ! fe/C for growth
     real(r8) :: Qsi             ! current Si/C ratio (mmol Si/mmol C)
     real(r8) :: gQsi            ! diatom Si/C ratio for growth (new biomass)
     real(r8) :: VNO3            ! NH4 uptake rate (non-dim)
     real(r8) :: VNH4            ! NO3 uptake rate (non-dim)
     real(r8) :: VNtot           ! total N uptake rate (non-dim)
     real(r8) :: NO3_V           ! nitrate uptake (mmol NO3/m^3/sec)
     real(r8) :: NH4_V           ! ammonium uptake (mmol NH4/m^3/sec)
     real(r8) :: PO4_V           ! PO4 uptake (mmol PO4/m^3/sec)
     real(r8) :: DOP_V           ! DOP uptake (mmol DOP/m^3/sec)
     real(r8) :: VPO4            ! C-specific PO4 uptake (non-dim)
     real(r8) :: VDOP            ! C-specific DOP uptake rate (non-dim)
     real(r8) :: VPtot           ! total P uptake rate (non-dim)
     real(r8) :: f_nut           ! nut limitation factor, modifies C fixation (non-dim)
     real(r8) :: VFe             ! C-specific Fe uptake (non-dim)
     real(r8) :: VSiO3           ! C-specific SiO3 uptake (non-dim)
     real(r8) :: light_lim       ! light limitation factor
     real(r8) :: PCphoto         ! C-specific rate of photosynth. (1/sec)
     real(r8) :: photoC          ! C-fixation (mmol C/m^3/sec)
     real(r8) :: photoFe         ! iron uptake
     real(r8) :: photoSi         ! silicon uptake (mmol Si/m^3/sec)
     real(r8) :: photoacc        ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
     real(r8) :: auto_loss       ! autotroph non-grazing mort (mmol C/m^3/sec)
     real(r8) :: auto_loss_poc   ! auto_loss routed to poc (mmol C/m^3/sec)
     real(r8) :: auto_loss_doc   ! auto_loss routed to doc (mmol C/m^3/sec)
     real(r8) :: auto_loss_dic   ! auto_loss routed to dic (mmol C/m^3/sec)
     real(r8) :: auto_agg        ! autotroph aggregation (mmol C/m^3/sec)
     real(r8) :: auto_graze      ! autotroph grazing rate (mmol C/m^3/sec)
     real(r8) :: auto_graze_zoo  ! auto_graze routed to zoo (mmol C/m^3/sec)
     real(r8) :: auto_graze_poc  ! auto_graze routed to poc (mmol C/m^3/sec)
     real(r8) :: auto_graze_doc  ! auto_graze routed to doc (mmol C/m^3/sec)
     real(r8) :: auto_graze_dic  ! auto_graze routed to dic (mmol C/m^3/sec)
     real(r8) :: Pprime          ! used to limit autotroph mort at low biomass (mmol C/m^3)
     real(r8) :: CaCO3_form      ! calcification of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
     real(r8) :: Nfix            ! total Nitrogen fixation (mmol N/m^3/sec)
     real(r8) :: Nexcrete        ! fixed N excretion
     real(r8) :: remaining_P_dop ! remaining_P from grazing routed to DOP pool
     real(r8) :: remaining_P_pop ! remaining_P from grazing routed to POP pool
     real(r8) :: remaining_P_dip ! remaining_P from grazing routed to remin
  end type autotroph_secondary_species_type

  !*****************************************************************************

  type, public :: zooplankton_secondary_species_type
     real(r8) :: f_zoo_detr       ! frac of zoo losses into large detrital pool (non-dim)
     real(r8) :: x_graze_zoo      ! {auto, zoo}_graze routed to zoo (mmol C/m^3/sec)
     real(r8) :: zoo_graze        ! zooplankton losses due to grazing (mmol C/m^3/sec)
     real(r8) :: zoo_graze_zoo    ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
     real(r8) :: zoo_graze_poc    ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
     real(r8) :: zoo_graze_doc    ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
     real(r8) :: zoo_graze_dic    ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
     real(r8) :: zoo_loss         ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
     real(r8) :: zoo_loss_poc     ! zoo_loss routed to poc (mmol C/m^3/sec)
     real(r8) :: zoo_loss_doc     ! zoo_loss routed to doc (mmol C/m^3/sec)
     real(r8) :: zoo_loss_dic     ! zoo_loss routed to dic (mmol C/m^3/sec)
     real(r8) :: Zprime           ! used to limit zoo mort at low biomass (mmol C/m^3)
  end type zooplankton_secondary_species_type

  !****************************************************************************

  ! Public parameters
  real(r8), public, parameter :: Qp_zoo = c1 / 117.0_r8 ! P/C ratio (mmol/mmol) zoo

  ! grazing functions
  integer(int_kind), public, parameter :: grz_fnc_michaelis_menten = 1
  integer(int_kind), public, parameter :: grz_fnc_sigmoidal        = 2

contains

  !*****************************************************************************

  subroutine autotroph_set_to_default(self, autotroph_id, marbl_status_log)

    class(autotroph_type), intent(out)   :: self
    character(len=*),      intent(in)    :: autotroph_id
    type(marbl_log_type),  intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_pft_mod:autotroph_set_to_default'
    character(len=char_len)     :: log_message

    select case (autotroph_id)
      case ('sp')
        self%sname = 'sp'                        ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%lname = 'Small Phyto'               ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%Nfixer = .false.
        self%imp_calcifier = .true.
        self%exp_calcifier = .false.
        self%silicifier = .false.
        self%kFe             = 0.03e-3_r8        ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kPO4            = 0.005_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kDOP            = 0.3_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNO3            = 0.25_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNH4            = 0.01_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kSiO3           = 0.0_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%Qp_fixed        =  Qp_zoo           ! only used for lvariable_PtoC=.false.
        self%gQfe_0          = 35.0e-6_r8
        self%gQfe_min        = 3.0e-6_r8
        self%alphaPI_per_day = 0.39_r8
        self%PCref_per_day   = 5.0_r8
        self%thetaN_max      = 2.5_r8
        self%loss_thres      = 0.01_r8
        self%loss_thres2     = 0.0_r8
        self%temp_thres      = -10.0_r8
        self%mort_per_day    = 0.1_r8
        self%mort2_per_day   = 0.01_r8
        self%agg_rate_max    = 0.5_r8
        self%agg_rate_min    = 0.01_r8
        self%loss_poc        = 0.0_r8
      case ('diat')
        self%sname = 'diat'                      ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%lname = 'Diatom'                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%Nfixer = .false.
        self%imp_calcifier = .false.
        self%exp_calcifier = .false.
        self%silicifier = .true.
        self%kFe             = 0.06e-3_r8        ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kPO4            = 0.05_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kDOP            = 0.5_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNO3            = 0.5_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNH4            = 0.05_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kSiO3           = 0.7_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%Qp_fixed        =  Qp_zoo           ! only used for lvariable_PtoC=.false.
        self%gQfe_0          = 35.0e-6_r8
        self%gQfe_min        = 3.0e-6_r8
        self%alphaPI_per_day = 0.29_r8
        self%PCref_per_day   = 5.0_r8
        self%thetaN_max      = 4.0_r8
        self%loss_thres      = 0.02_r8
        self%loss_thres2     = 0.0_r8
        self%temp_thres      = -10.0_r8
        self%mort_per_day    = 0.1_r8
        self%mort2_per_day   = 0.01_r8
        self%agg_rate_max    = 0.5_r8
        self%agg_rate_min    = 0.02_r8
        self%loss_poc        = 0.0_r8
      case ('diaz')
        self%sname = 'diaz'                      ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%lname = 'Diazotroph'                ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%Nfixer = .true.
        self%imp_calcifier = .false.
        self%exp_calcifier = .false.
        self%silicifier = .false.
        self%kFe             = 0.045e-3_r8       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kPO4            = 0.015_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kDOP            = 0.075_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNO3            = 2.0_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNH4            = 0.2_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kSiO3           = 0.0_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%Qp_fixed        = 0.32_r8 * Qp_zoo  ! only used for lvariable_PtoC=.false.
        self%gQfe_0          = 70.0e-6_r8
        self%gQfe_min        = 6.0e-6_r8
        self%alphaPI_per_day = 0.39_r8
        self%PCref_per_day   = 2.2_r8
        self%thetaN_max      = 2.5_r8
        self%loss_thres      = 0.02_r8
        self%loss_thres2     = 0.001_r8
        self%temp_thres      = 15.0_r8
        self%mort_per_day    = 0.1_r8
        self%mort2_per_day   = 0.01_r8
        self%agg_rate_max    = 0.5_r8
        self%agg_rate_min    = 0.01_r8
        self%loss_poc        = 0.0_r8
      case ('unset')
        self%sname = 'unknown'
        self%lname = 'unknown'
        self%Nfixer        = .false.
        self%imp_calcifier = .false.
        self%exp_calcifier = .false.
        self%silicifier    = .false.
        self%kFe             = UnsetValue
        self%kPO4            = UnsetValue
        self%kDOP            = UnsetValue
        self%kNO3            = UnsetValue
        self%kNH4            = UnsetValue
        self%kSiO3           = UnsetValue
        self%Qp_fixed        = UnsetValue
        self%gQfe_0          = UnsetValue
        self%gQfe_min        = UnsetValue
        self%alphaPI_per_day = UnsetValue
        self%PCref_per_day   = UnsetValue
        self%thetaN_max      = UnsetValue
        self%loss_thres      = UnsetValue
        self%loss_thres2     = UnsetValue
        self%temp_thres      = UnsetValue
        self%mort_per_day    = UnsetValue
        self%mort2_per_day   = UnsetValue
        self%agg_rate_max    = UnsetValue
        self%agg_rate_min    = UnsetValue
        self%loss_poc        = UnsetValue
      case DEFAULT
        write(log_message, "(3A)") "'", autotroph_id, "' is not a valid autotroph ID"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select

  end subroutine autotroph_set_to_default

  !*****************************************************************************

  subroutine zooplankton_set_to_default(self, zooplankton_id, marbl_status_log)

    class(zooplankton_type), intent(out)   :: self
    character(len=*),        intent(in)    :: zooplankton_id
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_pft_mod:zooplankton_set_to_default'
    character(len=char_len)     :: log_message

    select case (zooplankton_id)
      case ('zoo')
        self%sname = 'zoo'                       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%lname = 'Zooplankton'               ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_mort_0_per_day   = 0.1_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_mort2_0_per_day  = 0.4_r8         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%loss_thres         = 0.075_r8       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
      case ('unset')
        self%sname = 'unknown'
        self%lname = 'unknown'
        self%z_mort_0_per_day   = UnsetValue
        self%z_mort2_0_per_day  = UnsetValue
        self%loss_thres         = UnsetValue
      case DEFAULT
        write(log_message, "(3A)") "'", zooplankton_id, "' is not a valid zooplankton ID"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select

  end subroutine zooplankton_set_to_default

  !*****************************************************************************

  subroutine grazing_set_to_default(self, grazing_id, marbl_status_log)

    class(grazing_type),  intent(inout) :: self
    character(len=*),     intent(in)    :: grazing_id
    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_pft_mod:grazing_set_to_default'
    character(len=char_len)     :: log_message

    select case (grazing_id)
      case ('sp_zoo')
        self%sname = 'grz_sp_zoo'                         ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%lname = 'Grazing of sp by zoo'               ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%auto_ind_cnt = 1                             ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%zoo_ind_cnt = 0                              ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_umax_0_per_day = 3.3_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_grz            = 1.2_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_zoo        = 0.3_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_poc        = 0.0_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_doc        = 0.06_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%f_zoo_detr       = 0.12_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%grazing_function = grz_fnc_michaelis_menten  ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%auto_ind(1) = 1                              ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
      case ('diat_zoo')
        self%sname = 'grz_diat_zoo'                       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%lname = 'Grazing of diat by zoo'             ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%auto_ind_cnt = 1                             ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%zoo_ind_cnt = 0                              ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_umax_0_per_day = 3.05_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_grz            = 1.2_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_zoo        = 0.25_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_poc        = 0.38_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_doc        = 0.06_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%f_zoo_detr       = 0.24_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%grazing_function = grz_fnc_michaelis_menten  ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%auto_ind(1) = 2
      case ('diaz_zoo')
        self%sname = 'grz_diaz_zoo'                       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%lname = 'Grazing of diaz by zoo'             ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%auto_ind_cnt = 1                             ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%zoo_ind_cnt = 0                              ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_umax_0_per_day = 3.1_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_grz            = 1.2_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_zoo        = 0.3_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_poc        = 0.1_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_doc        = 0.06_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%f_zoo_detr       = 0.12_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%grazing_function = grz_fnc_michaelis_menten  ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%auto_ind(1) = 3
      case ('unset')
        self%sname = 'unknown'
        self%lname = 'unknown'
        self%auto_ind_cnt = 0
        self%zoo_ind_cnt  = 0
        self%auto_ind     = -1
        self%zoo_ind      = -1
        self%z_umax_0_per_day = UnsetValue
        self%z_grz            = UnsetValue
        self%graze_zoo        = UnsetValue
        self%graze_poc        = UnsetValue
        self%graze_doc        = UnsetValue
        self%f_zoo_detr       = UnsetValue
        self%grazing_function = grz_fnc_michaelis_menten
      case DEFAULT
        write(log_message, "(3A)") "'", grazing_id, "' is not a valid grazing ID"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select

  end subroutine grazing_set_to_default
  !*****************************************************************************

  subroutine grazing_constructor(self, autotroph_cnt, zooplankton_cnt, marbl_status_log)

    class(grazing_type),  intent(out)   :: self
    integer(int_kind),    intent(in)    :: autotroph_cnt
    integer(int_kind),    intent(in)    :: zooplankton_cnt
    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_pft_mod:grazing_constructor'
    character(len=char_len)     :: log_message

    if (allocated(self%auto_ind)) then
      log_message = 'grazing%auto_inds is already allocated!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    if (allocated(self%zoo_ind)) then
      log_message = 'grazing%zoo_inds is already allocated!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    allocate(self%auto_ind(autotroph_cnt))
    allocate(self%zoo_ind(zooplankton_cnt))

  end subroutine grazing_constructor

  !*****************************************************************************

end module marbl_pft_mod