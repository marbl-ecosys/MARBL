module marbl_share_mod

!-----------------------------------------------------------------------------
!  This contains the marbl namelist variables
!-----------------------------------------------------------------------------

  use marbl_kinds_mod       , only : r8
  use marbl_kinds_mod       , only : log_kind
  use marbl_kinds_mod       , only : int_kind
  use marbl_kinds_mod       , only : char_len
  use marbl_sizes           , only : ecosys_base_tracer_cnt
  use marbl_sizes           , only : ciso_tracer_cnt
  use marbl_interface_types , only : marbl_tracer_read_type
  use marbl_interface_types , only : marbl_forcing_monthly_every_ts_type

  implicit none

  public

  !-----------------------------------------------------------------------------
  !  parameters for time frequency options
  !-----------------------------------------------------------------------------

  integer (int_kind), parameter :: marbl_freq_opt_never    = 0
  integer (int_kind), parameter :: marbl_freq_opt_nyear    = 1
  integer (int_kind), parameter :: marbl_freq_opt_nmonth   = 2

  !-----------------------------------------------------------------------------
  ! namelist: ecosys_nml
  !-----------------------------------------------------------------------------

  !  options for forcing of gas fluxes
  integer (int_kind), parameter :: gas_flux_forcing_iopt_drv  = 1
  integer (int_kind), parameter :: gas_flux_forcing_iopt_file = 2
  integer (int_kind), parameter :: atm_co2_iopt_const         = 1
  integer (int_kind), parameter :: atm_co2_iopt_drv_prog      = 2
  integer (int_kind), parameter :: atm_co2_iopt_drv_diag      = 3

  type(marbl_tracer_read_type) :: tracer_init_ext(ecosys_base_tracer_cnt) ! namelist variable for initializing tracers 
  type(marbl_tracer_read_type) :: fesedflux_input                    ! namelist input for iron_flux

  character (char_len) :: gas_flux_forcing_file        ! file containing gas flux forcing fields
  integer   (int_kind) :: gas_flux_forcing_iopt
  integer   (int_kind) :: atm_co2_iopt
  integer   (int_kind) :: atm_alt_co2_iopt
  real      (r8)       :: atm_co2_const                ! value of atmospheric co2 (ppm, dry-air, 1 atm)
  real      (r8)       :: atm_alt_co2_const            ! value of atmospheric alternative co2 (ppm, dry-air, 1 atm)
  logical   (log_kind) :: lflux_gas_o2                 ! controls which portion of code are executed usefull for debugging
  logical   (log_kind) :: lflux_gas_co2                ! controls which portion of code are executed usefull for debugging
  character (char_len) :: init_ecosys_option           ! namelist option for initialization of bgc
  character (char_len) :: init_ecosys_init_file        ! filename for option 'file'
  character (char_len) :: init_ecosys_init_file_fmt    ! file format for option 'file'

  logical   (log_kind) :: liron_patch                  ! flag for iron patch fertilization
  character (char_len) :: iron_patch_flux_filename     ! file containing name of iron patch file
  integer   (int_kind) :: iron_patch_month             ! integer month to add patch flux

  character (char_len) :: ndep_data_type               ! type of ndep forcing
  integer   (int_kind) :: ndep_shr_stream_year_first   ! first year in stream to use
  integer   (int_kind) :: ndep_shr_stream_year_last    ! last year in stream to use
  integer   (int_kind) :: ndep_shr_stream_year_align   ! align ndep_shr_stream_year_first with this model year
  character (char_len) :: ndep_shr_stream_file         ! file containing domain and input data
  real      (r8)       :: ndep_shr_stream_scale_factor ! unit conversion factor

  character (char_len) :: dust_flux_source             ! option for atmospheric dust deposition
  character (char_len) :: iron_flux_source             ! option for atmospheric iron deposition
  real      (r8)       :: iron_frac_in_dust            ! fraction by weight of iron in dust
  real      (r8)       :: iron_frac_in_bc              ! fraction by weight of iron in black carbon

  type(marbl_forcing_monthly_every_ts_type), pointer :: fice_file             ! ice fraction, if read from file
  type(marbl_forcing_monthly_every_ts_type), pointer :: xkw_file              ! a * wind-speed ** 2, if read from file
  type(marbl_forcing_monthly_every_ts_type), pointer :: ap_file               ! atmoshperic pressure, if read from file
  type(marbl_forcing_monthly_every_ts_type), pointer :: dust_flux_file        ! surface dust flux
  type(marbl_forcing_monthly_every_ts_type), pointer :: iron_flux_file        ! iron component of surface dust flux
  type(marbl_forcing_monthly_every_ts_type), pointer :: nox_flux_monthly_file ! surface NOx species flux, added to nitrate pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: nhy_flux_monthly_file ! surface NHy species flux, added to ammonium pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: din_riv_flux_file     ! river DIN species flux, added to nitrate pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dip_riv_flux_file     ! river DIP species flux, added to phosphate pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: don_riv_flux_file     ! river DON flux, added to semi-lab don pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dop_riv_flux_file     ! river DOP flux, added to semi-lab dop pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dsi_riv_flux_file     ! river DSI flux, added to dsi pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dfe_riv_flux_file     ! river dfe flux, added to dfe pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: dic_riv_flux_file     ! river dic flux, added to dic pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: alk_riv_flux_file     ! river alk flux, added to alk pool
  type(marbl_forcing_monthly_every_ts_type), pointer :: doc_riv_flux_file     ! river doc flux, added to semi-labile DOC

  !-----------------------------------------------------------------------
  !  namelist: ecosys_ciso_nml 
  !-----------------------------------------------------------------------

  type(marbl_tracer_read_type) :: ciso_tracer_init_ext(ciso_tracer_cnt) ! namelist variable for initializing tracers

  logical   (log_kind)        :: ciso_lsource_sink              ! flag controlling which portion of code are executed
  character (char_len)        :: ciso_fract_factors             ! option for which biological fractionation calculation to use
  character (char_len)        :: ciso_init_ecosys_option        ! option for initialization of bgc
  character (char_len)        :: ciso_init_ecosys_init_file     ! filename for option 'file'
  character (char_len)        :: ciso_init_ecosys_init_file_fmt ! file format for option 'file'
  logical   (log_kind)        :: ciso_lecovars_full_depth_tavg  ! should ecosystem vars be written full depth

  ! ciso forcing related variables

  integer   (int_kind)        :: ciso_atm_model_year            ! arbitrary model year
  integer   (int_kind)        :: ciso_atm_data_year             ! year in atmospheric ciso data that corresponds to ciso_atm_model_year
  integer   (int_kind)        :: ciso_atm_d13c_data_nbval       ! number of values in ciso_atm_d13c_filename
  integer   (int_kind)        :: ciso_atm_d14c_data_nbval       ! number of values in ciso_atm_d14c_filename
  real      (r8), allocatable :: ciso_atm_d13c_data(:)          ! atmospheric D13C values in datafile
  real      (r8), allocatable :: ciso_atm_d13c_data_yr(:)       ! date of atmospheric D13C values in datafile
  real      (r8), allocatable :: ciso_atm_d14c_data(:,:)        ! atmospheric D14C values in datafile (sh, eq, nh, in permil)
  real      (r8), allocatable :: ciso_atm_d14c_data_yr(:,:)     ! date of atmospheric D14C values in datafile (sh, eq, nh)
  real      (r8)              :: ciso_atm_d13c_const            ! atmospheric D13C constant [permil]
  real      (r8)              :: ciso_atm_d14c_const            ! atmospheric D14C constant [permil]
  character (char_len)        :: ciso_atm_d13c_opt              ! option for CO2 and D13C varying or constant forcing
  character (char_len)        :: ciso_atm_d13c_filename         ! filenames for varying atm D13C
  character (char_len)        :: ciso_atm_d14c_opt              ! option for CO2 and D13C varying or constant forcing
  character (char_len)        :: ciso_atm_d14c_filename(3)      ! filenames for varying atm D14C (one each for NH, SH, EQ)

end module marbl_share_mod
