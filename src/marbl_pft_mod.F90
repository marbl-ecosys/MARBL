module marbl_pft_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

  use marbl_constants_mod, only : c1

  use marbl_logging, only : marbl_log_type

  implicit none
  private

  real(r8), parameter :: UnsetValue = 1e34_r8

  !****************************************************************************

  type, public :: autotroph_settings_type
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
  end type autotroph_settings_type

  !****************************************************************************

  type, public :: zooplankton_settings_type
     character(len=char_len) :: sname
     character(len=char_len) :: lname
     real(r8)                :: z_mort_0_per_day   ! zoo linear mort rate (1/day)
     real(r8)                :: z_mort_0           ! zoo linear mort rate (1/sec) (derived from z_mort_0_per_day)
     real(r8)                :: z_mort2_0_per_day  ! zoo quad mort rate (1/day/((mmol C/m3))
     real(r8)                :: z_mort2_0          ! zoo quad mort rate (1/sec/((mmol C/m3)) (derived from z_mort2_0_per_day)
     real(r8)                :: loss_thres         ! zoo conc. where losses go to zero
   contains
     procedure, public :: set_to_default => zooplankton_set_to_default
  end type zooplankton_settings_type

  !****************************************************************************

  type, public :: grazing_relationship_settings_type
    character(len=char_len) :: sname
    character(len=char_len) :: lname
    integer(int_kind)       :: auto_ind_cnt     ! number of autotrophs in prey-class auto_ind
    integer(int_kind)       :: zoo_ind_cnt      ! number of zooplankton in prey-class zoo_ind
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
    procedure, public :: set_to_default => grazer_set_to_default
    procedure, public :: construct => grazer_constructor
  end type grazing_relationship_settings_type

  !****************************************************************************

  ! Public parameters
  real(r8), public, parameter :: Qp_zoo = c1 / 117.0_r8 ! P/C ratio (mmol/mmol) zoo

  ! grazing functions
  integer(int_kind), public, parameter :: grz_fnc_michaelis_menten = 1
  integer(int_kind), public, parameter :: grz_fnc_sigmoidal        = 2

contains

  !*****************************************************************************

  subroutine autotroph_set_to_default(self, autotroph_id, marbl_status_log)

    class(autotroph_settings_type), intent(out)   :: self
    character(len=*),               intent(in)    :: autotroph_id
    type(marbl_log_type),           intent(inout) :: marbl_status_log

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
        self%kPO4            = 0.01_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kDOP            = 0.3_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNO3            = 0.25_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNH4            = 0.01_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kSiO3           = 0.0_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%Qp_fixed        =  Qp_zoo           ! only used for lvariable_PtoC=.false.
        self%gQfe_0          = 30.0e-6_r8
        self%gQfe_min        = 2.5e-6_r8
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
        self%kFe             = 0.07e-3_r8        ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kPO4            = 0.05_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kDOP            = 0.5_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNO3            = 0.5_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kNH4            = 0.05_r8           ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%kSiO3           = 0.7_r8            ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%Qp_fixed        =  Qp_zoo           ! only used for lvariable_PtoC=.false.
        self%gQfe_0          = 30.0e-6_r8
        self%gQfe_min        = 2.5e-6_r8
        self%alphaPI_per_day = 0.28_r8
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
        self%gQfe_0          = 60.0e-6_r8
        self%gQfe_min        = 2.5e-6_r8
        self%alphaPI_per_day = 0.39_r8
        self%PCref_per_day   = 2.5_r8
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

    class(zooplankton_settings_type), intent(out)   :: self
    character(len=*),                 intent(in)    :: zooplankton_id
    type(marbl_log_type),             intent(inout) :: marbl_status_log

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

  subroutine grazer_set_to_default(self, grazer_id, marbl_status_log)

    class(grazing_relationship_settings_type), intent(inout) :: self
    character(len=*),                          intent(in)    :: grazer_id
    type(marbl_log_type),                      intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_pft_mod:grazer_set_to_default'
    character(len=char_len)     :: log_message

    select case (grazer_id)
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
        self%z_umax_0_per_day = 3.15_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_grz            = 1.2_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_zoo        = 0.25_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_poc        = 0.39_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%graze_doc        = 0.06_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%f_zoo_detr       = 0.24_r8                   ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%grazing_function = grz_fnc_michaelis_menten  ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%auto_ind(1) = 2
      case ('diaz_zoo')
        self%sname = 'grz_diaz_zoo'                       ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%lname = 'Grazing of diaz by zoo'             ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%auto_ind_cnt = 1                             ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%zoo_ind_cnt = 0                              ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
        self%z_umax_0_per_day = 3.3_r8                    ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE in marbl_settings_mod
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
        write(log_message, "(3A)") "'", grazer_id, "' is not a valid grazing ID"
        call marbl_status_log%log_error(log_message, subname)
        return
    end select

  end subroutine grazer_set_to_default
  !*****************************************************************************

  subroutine grazer_constructor(self, autotroph_cnt, zooplankton_cnt, marbl_status_log)

    class(grazing_relationship_settings_type), intent(out)   :: self
    integer(int_kind),                         intent(in)    :: autotroph_cnt
    integer(int_kind),                         intent(in)    :: zooplankton_cnt
    type(marbl_log_type),                      intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_pft_mod:grazer_constructor'
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

  end subroutine grazer_constructor

  !*****************************************************************************

end module marbl_pft_mod
