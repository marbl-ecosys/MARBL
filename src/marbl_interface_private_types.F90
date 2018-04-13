module marbl_interface_private_types

  ! module definitions of types that are internal to marbl

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : char_len

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
     procedure, public :: destruct => marbl_surface_forcing_internal_destructor
  end type marbl_surface_forcing_internal_type

  !****************************************************************************
  !
  ! Shared data type definitions
  !
  !****************************************************************************

  type, public :: marbl_interior_share_type
     real(r8) :: QA_dust_def         ! incoming deficit in the QA(dust) POC flux
     real(r8) :: DIC_loc_fields      ! local copy of model DIC
     real(r8) :: DOCtot_loc_fields   ! local copy of model DOC+DOCr
     real(r8) :: O2_loc_fields       ! local copy of model O2
     real(r8) :: NO3_loc_fields      ! local copy of model NO3
     real(r8) :: CO3_fields
     real(r8) :: HCO3_fields         ! bicarbonate ion
     real(r8) :: H2CO3_fields        ! carbonic acid
     real(r8) :: CO3_sat_calcite
     real(r8) :: DOCtot_remin_fields ! remineralization of DOC+DOCr (mmol C/m^3/sec)
  end type marbl_interior_share_type

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
    integer (int_kind) :: zootot13C_ind   = 0 ! zooplankton carbon 13
    integer (int_kind) :: zootot14C_ind   = 0 ! zooplankton carbon 14

  contains
    procedure, public :: add_tracer_index
    procedure, public :: construct => tracer_index_constructor
    procedure, public :: destruct => tracer_index_destructor
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
     integer(int_kind) :: potemp_id      = 0
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

   subroutine marbl_surface_forcing_share_constructor(this, num_elements)

     class(marbl_surface_forcing_share_type), intent(out) :: this

     integer (int_kind) , intent(in) :: num_elements

     allocate(this%PV_SURF_fields       (num_elements)) ! piston velocity (cm/s)
     allocate(this%DIC_SURF_fields      (num_elements)) ! Surface values of DIC for solver
     allocate(this%CO2STAR_SURF_fields  (num_elements)) ! CO2STAR from solver
     allocate(this%DCO2STAR_SURF_fields (num_elements)) ! DCO2STAR from solver
     allocate(this%CO3_SURF_fields      (num_elements)) ! Surface carbonate ion

   end subroutine marbl_surface_forcing_share_constructor

   !***********************************************************************

   subroutine marbl_surface_forcing_share_destructor(this)

     class(marbl_surface_forcing_share_type), intent(inout) :: this

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

  subroutine marbl_surface_forcing_internal_constructor(this, num_elements)

    use marbl_constants_mod, only : c0

    class(marbl_surface_forcing_internal_type), intent(out) :: this
    integer (int_kind),                         intent(in)  :: num_elements

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

  end subroutine marbl_surface_forcing_internal_constructor

  !***********************************************************************

  subroutine marbl_surface_forcing_internal_destructor(this)

    class(marbl_surface_forcing_internal_type) , intent(inout) :: this

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

  end subroutine marbl_surface_forcing_internal_destructor

  !*****************************************************************************

  subroutine tracer_index_constructor(this, ciso_on, lvariable_PtoC, autotrophs, &
             zooplankton, marbl_status_log)

    ! This subroutine sets the tracer indices for the non-autotroph tracers. To
    ! know where to start the indexing for the autotroph tracers, it increments
    ! tracer_cnt by 1 for each tracer that is included. Note that this gives an
    ! accurate count whether the carbon isotope tracers are included or not.

    use marbl_constants_mod, only : c0
    use marbl_pft_mod, only : autotroph_type
    use marbl_pft_mod, only : zooplankton_type

    class(marbl_tracer_index_type), intent(out)   :: this
    logical,                        intent(in)    :: ciso_on
    logical,                        intent(in)    :: lvariable_PtoC
    type(autotroph_type),           intent(in)    :: autotrophs(:)
    type(zooplankton_type),         intent(in)    :: zooplankton(:)
    type(marbl_log_type),           intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_interface_private_types:tracer_index_constructor'
    character(len=char_len) :: ind_name
    integer :: autotroph_cnt, zooplankton_cnt, n

    autotroph_cnt = size(autotrophs)
    zooplankton_cnt = size(zooplankton)

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
      write(ind_name, "(2A)") trim(zooplankton(n)%sname), "C"
      call this%add_tracer_index(ind_name, 'ecosys_base', this%zoo_inds(n)%C_ind, marbl_status_log)
    end do

    do n=1,autotroph_cnt
      write(ind_name, "(2A)") trim(autotrophs(n)%sname), "Chl"
      call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%Chl_ind, marbl_status_log)

      write(ind_name, "(2A)") trim(autotrophs(n)%sname), "C"
      call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%C_ind, marbl_status_log)

      if (lvariable_PtoC) then
        write(ind_name, "(2A)") trim(autotrophs(n)%sname), "P"
        call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%P_ind, marbl_status_log)
      end if

      write(ind_name, "(2A)") trim(autotrophs(n)%sname), "Fe"
      call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%Fe_ind, marbl_status_log)

      if (autotrophs(n)%silicifier) then
        write(ind_name, "(2A)") trim(autotrophs(n)%sname), "Si"
        call this%add_tracer_index(ind_name, 'ecosys_base', this%auto_inds(n)%Si_ind, marbl_status_log)
      end if

      if (autotrophs(n)%imp_calcifier.or.                            &
          autotrophs(n)%exp_calcifier) then
        write(ind_name, "(2A)") trim(autotrophs(n)%sname), "CaCO3"
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
        write(ind_name, "(2A)") trim(autotrophs(n)%sname), "C13"
        call this%add_tracer_index(ind_name, 'ciso', this%auto_inds(n)%C13_ind, marbl_status_log)

        write(ind_name, "(2A)") trim(autotrophs(n)%sname), "C14"
        call this%add_tracer_index(ind_name, 'ciso', this%auto_inds(n)%C14_ind, marbl_status_log)

        if (autotrophs(n)%imp_calcifier .or. &
            autotrophs(n)%exp_calcifier) then
        write(ind_name, "(2A)") trim(autotrophs(n)%sname), "Ca13CO3"
          call this%add_tracer_index(ind_name, 'ciso', this%auto_inds(n)%Ca13CO3_ind, marbl_status_log)

        write(ind_name, "(2A)") trim(autotrophs(n)%sname), "Ca14CO3"
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

  subroutine surface_forcing_index_constructor(this, ciso_on, lflux_gas_o2,   &
             lflux_gas_co2, ladjust_bury_coeff, num_surface_forcing_fields)

    ! This subroutine sets the surface forcing indexes, which are used to
    ! determine what forcing fields are required from the driver.

    class(marbl_surface_forcing_indexing_type), intent(out) :: this
    logical,                                    intent(in)  :: ciso_on
    logical,                                    intent(in)  :: lflux_gas_o2
    logical,                                    intent(in)  :: lflux_gas_co2
    logical,                                    intent(in)  :: ladjust_bury_coeff
    integer,                                    intent(out) :: num_surface_forcing_fields

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
                                                num_PAR_subcols,              &
                                                num_interior_forcing_fields,  &
                                                marbl_status_log)

    ! This subroutine sets the interior forcing indexes, which are used to
    ! determine what forcing fields are required from the driver.

    class(marbl_interior_forcing_indexing_type), intent(out)   :: this
    character(len=char_len), dimension(:),       intent(in)    :: tracer_names
    character(len=char_len), dimension(:),       intent(in)    :: tracer_restore_vars
    integer(int_kind),                           intent(in)    :: num_PAR_subcols
    integer(int_kind),                           intent(out)   :: num_interior_forcing_fields
    type(marbl_log_type),                        intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_interface_private_types:interior_forcing_index_constructor'
    character(len=char_len)     :: log_message

    integer :: tracer_restore_cnt, tracer_cnt
    integer :: m, n

    associate(forcing_cnt => num_interior_forcing_fields)

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

  end subroutine interior_forcing_index_constructor

  !*****************************************************************************

end module marbl_interface_private_types
