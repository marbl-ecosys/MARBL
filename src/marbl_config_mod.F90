module marbl_config_mod

  use marbl_kinds_mod, only : r8
  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : log_kind
  use marbl_kinds_mod, only : char_len

  use marbl_sizes, only : autotroph_cnt
  use marbl_sizes, only : zooplankton_cnt
  use marbl_sizes, only : grazer_prey_cnt

  use marbl_logging, only : marbl_log_type

  use marbl_constants_mod, only : c0
  use marbl_constants_mod, only : c1
  use marbl_constants_mod, only : dps

  use marbl_internal_types, only : autotroph_config_type
  use marbl_internal_types, only : zooplankton_config_type
  use marbl_internal_types, only : grazing_config_type

  use marbl_interface_types, only : marbl_tracer_read_type
  use marbl_interface_types, only : marbl_forcing_monthly_every_ts_type

  implicit none
  public
  save

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_config_nml
  !---------------------------------------------------------------------

  logical(log_kind), target ::  ciso_on                       ! control whether ciso tracer module is active
  logical(log_kind), target ::  lsource_sink                  ! control which portion of code is executed, useful for debugging
  logical(log_kind), target :: ciso_lsource_sink              ! control which portion of carbon isotope code is executed, useful for debugging
  logical(log_kind), target :: lecovars_full_depth_tavg       ! should base ecosystem vars be written full depth
  logical(log_kind), target :: ciso_lecovars_full_depth_tavg  ! should carbon isotope vars be written full depth
  logical(log_kind), target :: lflux_gas_o2                   ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: lflux_gas_co2                  ! controls which portion of code are executed usefull for debugging
  logical(log_kind), target :: locmip_k1_k2_bug_fix
  type(autotroph_config_type), dimension(autotroph_cnt), target :: autotrophs_config
  type(zooplankton_config_type), dimension(zooplankton_cnt), target :: zooplankton_config
  type(grazing_config_type), dimension(grazer_prey_cnt, zooplankton_cnt), target :: grazing_config

  !---------------------------------------------------------------------
  !  Variables read in via &marbl_forcing_tmp_nml
  !---------------------------------------------------------------------

  character(char_len),  target :: dust_flux_source             ! option for atmospheric dust deposition
  type(marbl_tracer_read_type), target :: dust_flux_input             ! namelist input for dust_flux
  character(char_len),  target :: iron_flux_source             ! option for atmospheric iron deposition
  type(marbl_tracer_read_type), target :: iron_flux_input             ! namelist input for iron_flux
  type(marbl_tracer_read_type), target :: fesedflux_input                    ! namelist input for iron_flux
  character(char_len),  target :: ndep_data_type               ! type of ndep forcing
  type(marbl_tracer_read_type), target :: nox_flux_monthly_input      ! namelist input for nox_flux_monthly
  type(marbl_tracer_read_type), target :: nhy_flux_monthly_input      ! namelist input for nhy_flux_monthly
  integer(int_kind),    target :: ndep_shr_stream_year_first   ! first year in stream to use
  integer(int_kind),    target :: ndep_shr_stream_year_last    ! last year in stream to use
  integer(int_kind),    target :: ndep_shr_stream_year_align   ! align ndep_shr_stream_year_first with this model year
  character(char_len),  target :: ndep_shr_stream_file         ! file containing domain and input data
  real(r8),             target :: ndep_shr_stream_scale_factor ! unit conversion factor
  type(marbl_tracer_read_type), target :: din_riv_flux_input          ! namelist input for din_riv_flux
  type(marbl_tracer_read_type), target :: dip_riv_flux_input          ! namelist input for dip_riv_flux
  type(marbl_tracer_read_type), target :: don_riv_flux_input          ! namelist input for don_riv_flux
  type(marbl_tracer_read_type), target :: dop_riv_flux_input          ! namelist input for dop_riv_flux
  type(marbl_tracer_read_type), target :: dsi_riv_flux_input          ! namelist input for dsi_riv_flux
  type(marbl_tracer_read_type), target :: dfe_riv_flux_input          ! namelist input for dfe_riv_flux
  type(marbl_tracer_read_type), target :: dic_riv_flux_input          ! namelist input for dic_riv_flux
  type(marbl_tracer_read_type), target :: alk_riv_flux_input          ! namelist input for alk_riv_flux
  type(marbl_tracer_read_type), target :: doc_riv_flux_input          ! namelist input for doc_riv_flux
  character(char_len),  target :: gas_flux_forcing_opt        ! option for forcing gas fluxes
  character(char_len),  target :: gas_flux_forcing_file        ! file containing gas flux forcing fields
  type(marbl_tracer_read_type), target :: gas_flux_fice               ! ice fraction for gas fluxes
  type(marbl_tracer_read_type), target :: gas_flux_ws                 ! wind speed for gas fluxes
  type(marbl_tracer_read_type), target :: gas_flux_ap                 ! atmospheric pressure for gas fluxes
  character(char_len),  target :: atm_co2_opt                 ! option for atmospheric co2 concentration
  real(r8),             target :: atm_co2_const                ! value of atmospheric co2 (ppm, dry-air, 1 atm)
  character(char_len),  target :: atm_alt_co2_opt             ! option for atmospheric alternative CO2
  real(r8),             target :: atm_alt_co2_const            ! value of atmospheric alternative co2 (ppm, dry-air, 1 atm)
  logical(log_kind),    target :: liron_patch                  ! flag for iron patch fertilization
  character(char_len),  target :: iron_patch_flux_filename     ! file containing name of iron patch file
  integer(int_kind),    target :: iron_patch_month             ! integer month to add patch flux
  integer(int_kind),    target :: ciso_atm_model_year            ! arbitrary model year
  integer(int_kind),    target :: ciso_atm_data_year             ! year in atmospheric ciso data that corresponds to ciso_atm_model_year
  integer(int_kind),    target :: ciso_atm_d13c_data_nbval       ! number of values in ciso_atm_d13c_filename
  integer(int_kind),    target :: ciso_atm_d14c_data_nbval       ! number of values in ciso_atm_d14c_filename
  real(r8), allocatable, target :: ciso_atm_d13c_data(:)          ! atmospheric D13C values in datafile
  real(r8), allocatable, target :: ciso_atm_d13c_data_yr(:)       ! date of atmospheric D13C values in datafile
  real(r8), allocatable, target :: ciso_atm_d14c_data(:,:)        ! atmospheric D14C values in datafile (sh, eq, nh, in permil)
  real(r8), allocatable, target :: ciso_atm_d14c_data_yr(:,:)     ! date of atmospheric D14C values in datafile (sh, eq, nh)
  real(r8),             target :: ciso_atm_d13c_const            ! atmospheric D13C constant [permil]
  real(r8),             target :: ciso_atm_d14c_const            ! atmospheric D14C constant [permil]
  character(char_len),  target :: ciso_atm_d13c_opt              ! option for CO2 and D13C varying or constant forcing
  character(char_len),  target :: ciso_atm_d13c_filename         ! filenames for varying atm D13C
  character(char_len),  target :: ciso_atm_d14c_opt              ! option for CO2 and D13C varying or constant forcing
  character(char_len),  target :: ciso_atm_d14c_filename(3)      ! filenames for varying atm D14C (one each for NH, SH, EQ)

  !---------------------------------------------------------------------
  !  Variables that don't match namelist read
  !---------------------------------------------------------------------

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

  !---------------------------------------------------------------------
  !  Auxiliary variables (str -> int conversions, indices, etc)
  !---------------------------------------------------------------------

  integer(int_kind)             :: gas_flux_forcing_iopt
  integer (int_kind), parameter :: gas_flux_forcing_iopt_drv  = 1
  integer (int_kind), parameter :: gas_flux_forcing_iopt_file = 2

  integer(int_kind)             :: atm_co2_iopt
  integer(int_kind)             :: atm_alt_co2_iopt
  integer (int_kind), parameter :: atm_co2_iopt_const         = 1
  integer (int_kind), parameter :: atm_co2_iopt_drv_prog      = 2
  integer (int_kind), parameter :: atm_co2_iopt_drv_diag      = 3

  !---------------------------------------------------------------------------
  !  Datatypes for marbl_instance%configuration and marbl_instance%parameters
  !---------------------------------------------------------------------------

  type, public :: marbl_single_config_or_parm_type
    ! Metadata
    character(len=char_len) :: long_name
    character(len=char_len) :: short_name
    character(len=char_len) :: units
    character(len=char_len) :: group
    character(len=char_len) :: datatype
    logical                 :: add_space ! used for formatting log output
    character(len=char_len) :: comment   ! used to add comment in log
    ! Actual parameter data
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()
  end type marbl_single_config_or_parm_type

  type, public :: marbl_config_and_parms_type
    logical :: locked = .false.
    integer :: cnt = 0
    type(marbl_single_config_or_parm_type), dimension(:), pointer :: vars => NULL()
  contains
    procedure          :: add_var        => marbl_var_add
    procedure          :: add_var_tcr_rd => marbl_var_add_tcr_rd
    procedure          :: add_var_1d_r8  => marbl_var_add_1d_r8
    procedure          :: add_var_1d_int => marbl_var_add_1d_int
    procedure          :: add_var_1d_str => marbl_var_add_1d_str
    procedure          :: finalize_vars  => marbl_vars_finalize
    procedure          :: inquire_id     => marbl_var_inquire_id
    generic            :: put            => put_real,                         &
                                            put_integer,                      &
                                            put_logical,                      &
                                            put_string
    generic            :: get            => get_real,                         &
                                            get_integer,                      &
                                            get_logical,                      &
                                            get_string
    procedure, private :: put_real       => marbl_var_put_real
    procedure, private :: put_integer    => marbl_var_put_integer
    procedure, private :: put_logical    => marbl_var_put_logical
    procedure, private :: put_string     => marbl_var_put_string
    procedure, private :: get_real       => marbl_var_get_real
    procedure, private :: get_integer    => marbl_var_get_integer
    procedure, private :: get_logical    => marbl_var_get_logical
    procedure, private :: get_string     => marbl_var_get_string
    procedure, private :: put_general    => marbl_var_put_all_types
    procedure, private :: get_general    => marbl_var_get_all_types
  end type marbl_config_and_parms_type

  !*****************************************************************************

  private :: r8, int_kind, log_kind, char_len
  private :: autotroph_cnt, zooplankton_cnt
  private :: marbl_log_type
  private :: marbl_var_put_real, marbl_var_put_integer, marbl_var_put_logical
  private :: marbl_var_put_string, marbl_var_put_all_types

contains

  !*****************************************************************************

  subroutine marbl_config_set_defaults()

    integer :: m, n

    !-----------------------------------------------------------------------
    !  &marbl_config_nml
    !-----------------------------------------------------------------------

    ciso_on                       = .false.
    lsource_sink                  = .true.
    ciso_lsource_sink             = .true.
    lecovars_full_depth_tavg      = .false.
    ciso_lecovars_full_depth_tavg = .false.
    lflux_gas_o2                  = .true.
    lflux_gas_co2                 = .true.
    locmip_k1_k2_bug_fix          = .true.

    do n=1,autotroph_cnt
      select case (n)
        case (1)
          autotrophs_config(n)%sname         = 'sp'
          autotrophs_config(n)%lname         = 'Small Phyto'
          autotrophs_config(n)%Nfixer        = .false.
          autotrophs_config(n)%imp_calcifier = .true.
          autotrophs_config(n)%exp_calcifier = .false.
          autotrophs_config(n)%silicifier    = .false.
        case (2)
          autotrophs_config(n)%sname         = 'diat'
          autotrophs_config(n)%lname         = 'Diatom'
          autotrophs_config(n)%Nfixer        = .false.
          autotrophs_config(n)%imp_calcifier = .false.
          autotrophs_config(n)%exp_calcifier = .false.
          autotrophs_config(n)%silicifier    = .true.
        case (3)
          autotrophs_config(n)%sname         = 'diaz'
          autotrophs_config(n)%lname         = 'Diazotroph'
          autotrophs_config(n)%Nfixer        = .true.
          autotrophs_config(n)%imp_calcifier = .false.
          autotrophs_config(n)%exp_calcifier = .false.
          autotrophs_config(n)%silicifier    = .false.
        case DEFAULT
          write(autotrophs_config(n)%sname,"(A,I0)") 'auto', n
          write(autotrophs_config(n)%lname,"(A,I0)") 'Autotroph number ', n
          autotrophs_config(n)%Nfixer        = .false.
          autotrophs_config(n)%imp_calcifier = .false.
          autotrophs_config(n)%exp_calcifier = .false.
          autotrophs_config(n)%silicifier    = .false.
      end select
    end do

    do n=1,zooplankton_cnt
      select case (n)
        case (1)
          zooplankton_config(n)%sname = 'zoo'
          zooplankton_config(n)%lname = 'Zooplankton'
        case DEFAULT
          write(zooplankton_config(n)%sname, "(A,I0)") 'zoo', n
          write(zooplankton_config(n)%lname, "(A,I0)") 'Zooplankton number ', n
      end select
    end do

    ! predator-prey relationships
    do n=1,zooplankton_cnt
      do m=1,grazer_prey_cnt

        write(grazing_config(m,n)%sname, "(4A)") 'grz_',                      &
                                   trim(autotrophs_config(m)%sname),          &
                                   '_', trim(zooplankton_config(n)%sname)
        write(grazing_config(m,n)%lname, "(4A)") 'Grazing of ',               &
                                   trim(autotrophs_config(m)%sname),          &
                                   ' by ', trim(zooplankton_config(n)%sname)
        grazing_config(m,n)%auto_ind_cnt = 1
        grazing_config(m,n)%zoo_ind_cnt  = 0
      end do
    end do

    !-----------------------------------------------------------------------
    !  &marbl_forcing_tmp_nml
    !-----------------------------------------------------------------------

    gas_flux_forcing_opt  = 'drv'
    gas_flux_forcing_file = 'unknown'
    call set_defaults_tcr_rd(gas_flux_fice, file_varname='FICE')
    call set_defaults_tcr_rd(gas_flux_ws, file_varname='XKW')
    call set_defaults_tcr_rd(gas_flux_ap, file_varname='P')
    dust_flux_source             = 'monthly-calendar'
    call set_defaults_tcr_rd(dust_flux_input, file_varname='dust_flux')
    iron_flux_source             = 'monthly-calendar'
    call set_defaults_tcr_rd(iron_flux_input, file_varname='iron_flux')
    call set_defaults_tcr_rd(fesedflux_input, file_varname='FESEDFLUXIN')
    ndep_data_type = 'monthly-calendar'
    call set_defaults_tcr_rd(nox_flux_monthly_input, file_varname='nox_flux')
    call set_defaults_tcr_rd(nhy_flux_monthly_input, file_varname='nhy_flux')
    ndep_shr_stream_year_first = 1
    ndep_shr_stream_year_last  = 1
    ndep_shr_stream_year_align = 1
    ndep_shr_stream_file       = 'unknown'
    ndep_shr_stream_scale_factor = c1
    call set_defaults_tcr_rd(din_riv_flux_input, file_varname='din_riv_flux', &
                                                 file_fmt='nc')
    call set_defaults_tcr_rd(dip_riv_flux_input, file_varname='dip_riv_flux', &
                                                 file_fmt='nc')
    call set_defaults_tcr_rd(don_riv_flux_input, file_varname='don_riv_flux', &
                                                 file_fmt='nc')
    call set_defaults_tcr_rd(dop_riv_flux_input, file_varname='dop_riv_flux', &
                                                 file_fmt='nc')
    call set_defaults_tcr_rd(dsi_riv_flux_input, file_varname='dsi_riv_flux', &
                                                 file_fmt='nc')
    call set_defaults_tcr_rd(dfe_riv_flux_input, file_varname='dfe_riv_flux', &
                                                 file_fmt='nc')
    call set_defaults_tcr_rd(dic_riv_flux_input, file_varname='dic_riv_flux', &
                                                 file_fmt='nc')
    call set_defaults_tcr_rd(alk_riv_flux_input, file_varname='alk_riv_flux', &
                                                 file_fmt='nc')
    call set_defaults_tcr_rd(doc_riv_flux_input, file_varname='doc_riv_flux', &
                                                 file_fmt='nc')
    liron_patch              = .false.
    iron_patch_flux_filename = 'unknown_iron_patch_filename'
    iron_patch_month         = 1
    atm_co2_opt   = 'const'
    atm_co2_const = 280.0_r8
    atm_alt_co2_opt   = 'const'
    atm_alt_co2_const = 280.0_r8
    ciso_atm_d13c_opt                       = 'const'
    ciso_atm_d13c_const                     = -6.379_r8
    ciso_atm_d13c_filename                  = 'unknown'
    ciso_atm_d14c_opt                       = 'const'
    ciso_atm_d14c_const                     = 0.0_r8
    ciso_atm_d14c_filename(1)               = 'unknown'
    ciso_atm_d14c_filename(2)               = 'unknown'
    ciso_atm_d14c_filename(3)               = 'unknown'
    ciso_atm_model_year                     = 1
    ciso_atm_data_year                      = 1

  end subroutine marbl_config_set_defaults

  !*****************************************************************************

  subroutine marbl_config_read_namelist(nl_buffer, marbl_status_log)

    use marbl_namelist_mod, only : marbl_nl_cnt
    use marbl_namelist_mod, only : marbl_nl_buffer_size
    use marbl_namelist_mod, only : marbl_namelist

    character(marbl_nl_buffer_size), intent(in)    :: nl_buffer(:)
    type(marbl_log_type),            intent(inout) :: marbl_status_log

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------
    character(*), parameter :: subname = 'marbl_config:marbl_config_read_namelist'
    character(len=char_len) :: log_message
    character(len=marbl_nl_buffer_size) :: tmp_nl_buffer
    integer (int_kind)           :: nml_error                   ! namelist i/o error flag

    namelist /marbl_config_nml/                                               &
         ciso_on, lsource_sink, ciso_lsource_sink, lecovars_full_depth_tavg,  &
         ciso_lecovars_full_depth_tavg, lflux_gas_o2, lflux_gas_co2,          &
         locmip_k1_k2_bug_fix, autotrophs_config, zooplankton_config,         &
         grazing_config

    namelist /marbl_forcing_tmp_nml/                                          &
         dust_flux_source, dust_flux_input, iron_flux_source,                 &
         iron_flux_input, fesedflux_input, ndep_data_type,                    &
         nox_flux_monthly_input, nhy_flux_monthly_input,                      &
         ndep_shr_stream_year_first, ndep_shr_stream_year_last,               &
         ndep_shr_stream_year_align, ndep_shr_stream_file,                    &
         ndep_shr_stream_scale_factor, din_riv_flux_input,                    &
         dip_riv_flux_input, don_riv_flux_input, dop_riv_flux_input,          &
         dsi_riv_flux_input, dfe_riv_flux_input, dic_riv_flux_input,          &
         alk_riv_flux_input, doc_riv_flux_input, gas_flux_forcing_opt,        &
         gas_flux_forcing_file, gas_flux_fice, gas_flux_ws, gas_flux_ap,      &
         atm_co2_opt, atm_co2_const, atm_alt_co2_opt, atm_alt_co2_const,      &
         liron_patch, iron_patch_flux_filename, iron_patch_month,             &
         ciso_atm_d13c_opt, ciso_atm_d13c_const, ciso_atm_d13c_filename,      &
         ciso_atm_d14c_opt, ciso_atm_d14c_const, ciso_atm_d14c_filename,      &
         ciso_atm_model_year, ciso_atm_data_year

    !-----------------------------------------------------------------------
    ! read the &marbl_config_nml namelist
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_config_nml')
    if (tmp_nl_buffer.ne.'') then
      read(tmp_nl_buffer, nml=marbl_config_nml, iostat=nml_error)
      if (nml_error /= 0) then
        write(log_message, "(A)") 'error reading &marbl_config_nml'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    else
      write(log_message, "(A)") '&marbl_config_nml not included in nl_buffer'
      call marbl_status_log%log_noerror(log_message, subname)
    end if

    !-----------------------------------------------------------------------
    ! read the &marbl_forcing_tmp_nml namelist
    !-----------------------------------------------------------------------

    tmp_nl_buffer = marbl_namelist(nl_buffer, 'marbl_forcing_tmp_nml')
    if (tmp_nl_buffer.ne.'') then
      read(tmp_nl_buffer, nml=marbl_forcing_tmp_nml, iostat=nml_error)
      if (nml_error /= 0) then
        write(log_message, "(A)") 'error reading &marbl_forcing_tmp_nml'
        call marbl_status_log%log_error(log_message, subname)
        return
      end if
    else
      write(log_message, "(A)") '&marbl_forcing_tmp_nml not included in nl_buffer'
      call marbl_status_log%log_noerror(log_message, subname)
    end if

  end subroutine marbl_config_read_namelist

  !*****************************************************************************

  subroutine marbl_config_set_local_pointers()

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

  end subroutine marbl_config_set_local_pointers

  !*****************************************************************************

  subroutine marbl_config_construct(this, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_config_construct'
    character(len=char_len) :: log_message
    character(len=char_len) :: sname, lname, units, datatype, group
    real(r8),                pointer :: rptr => NULL()
    integer(int_kind),       pointer :: iptr => NULL()
    logical(log_kind),       pointer :: lptr => NULL()
    character(len=char_len), pointer :: sptr => NULL()

    integer :: m, n
    character(len=char_len) :: prefix

    if (associated(this%vars)) then
      write(log_message, "(A)") "this%configuration has been constructed already"
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    this%cnt = 0
    allocate(this%vars(this%cnt))

    !------------------!
    ! marbl_config_nml !
    !------------------!

    sname     = 'ciso_on'
    lname     = 'Control whether CISO tracer module is active'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => ciso_on
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lsource_sink'
    lname     = 'Control which portions of code are executed (useful for debugging)'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lsource_sink
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lecovars_full_depth_tavg'
    lname     = 'Are base ecosystem tracers full depth?'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lecovars_full_depth_tavg
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    if (ciso_on) then
      sname     = 'ciso_lsource_sink'
      lname     = 'Control which portions of carbon isotope code are executed (useful for debugging)'
      units     = 'unitless'
      datatype  = 'logical'
      group     = 'marbl_config_nml'
      lptr      => ciso_lsource_sink
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      sname     = 'ciso_lecovars_full_depth_tavg'
      lname     = 'Are carbon isotope tracers full depth?'
      units     = 'unitless'
      datatype  = 'logical'
      group     = 'marbl_config_nml'
      lptr      => ciso_lecovars_full_depth_tavg
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if
    end if

    sname     = 'lflux_gas_o2'
    lname     = 'Run O2 gas flux portion of the code'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lflux_gas_o2
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'lflux_gas_co2'
    lname     = 'Run CO2 gas flux portion of the code'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => lflux_gas_co2
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'locmip_k1_k2_bug_fix'
    lname     = 'Fix bug that was in code in OCMIP runs?'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_config_nml'
    lptr      => locmip_k1_k2_bug_fix
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr, add_space=.true.)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    do n=1,autotroph_cnt
      write(prefix, "(A,I0,A)") 'autotrophs_config(', n, ')%'

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => autotrophs_config(n)%sname
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of autotroph'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => autotrophs_config(n)%lname
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'Nfixer'
      lname    = 'Flag is true if this autotroph fixes N2'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%Nfixer
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'imp_calcifier'
      lname    = 'Flag is true if this autotroph implicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%imp_calcifier
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'exp_calcifier'
      lname    = 'Flag is true if this autotroph explicitly handles calcification'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%exp_calcifier
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, lptr=lptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'silicifier'
      lname    = 'Flag is true if this autotroph is a silicifier'
      units    = 'unitless'
      datatype = 'logical'
      group    = 'marbl_config_nml'
      lptr     => autotrophs_config(n)%silicifier
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, lptr=lptr, add_space=.true.)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

    end do

    do n=1, zooplankton_cnt
      write(prefix, "(A,I0,A)") 'zooplankton_config(', n, ')%'

      write(sname, "(2A)") trim(prefix), 'sname'
      lname    = 'Short name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => zooplankton_config(n)%sname
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      write(sname, "(2A)") trim(prefix), 'lname'
      lname    = 'Long name of zooplankton'
      units    = 'unitless'
      datatype = 'string'
      group    = 'marbl_config_nml'
      sptr     => zooplankton_config(n)%lname
      call this%add_var(sname, lname, units, datatype, group,               &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if
    end do

    do n=1,zooplankton_cnt
      do m=1,grazer_prey_cnt
        write(prefix, "(A,I0,A,I0,A)") 'grazing_config(', m, ',', n, ')%'

        write(sname, "(2A)") trim(prefix), 'sname'
        lname    = 'Short name of grazer'
        units    = 'unitless'
        datatype = 'string'
        group    = 'marbl_config_nml'
        sptr     => grazing_config(m,n)%sname
        call this%add_var(sname, lname, units, datatype, group,               &
                          marbl_status_log, sptr=sptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'lname'
        lname    = 'Long name of grazer'
        units    = 'unitless'
        datatype = 'string'
        group    = 'marbl_config_nml'
        sptr     => grazing_config(m,n)%lname
        call this%add_var(sname, lname, units, datatype, group,               &
                          marbl_status_log, sptr=sptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'auto_ind_cnt'
        lname    = 'number of autotrophs in prey-clase auto_ind'
        units    = 'unitless'
        datatype = 'integer'
        group    = 'marbl_config_nml'
        iptr     => grazing_config(m,n)%auto_ind_cnt
        call this%add_var(sname, lname, units, datatype, group,               &
                          marbl_status_log, iptr=iptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if

        write(sname, "(2A)") trim(prefix), 'zoo_ind_cnt'
        lname    = 'number of zooplankton in prey-clase auto_ind'
        units    = 'unitless'
        datatype = 'integer'
        group    = 'marbl_config_nml'
        iptr     => grazing_config(m,n)%zoo_ind_cnt
        call this%add_var(sname, lname, units, datatype, group,               &
                          marbl_status_log, iptr=iptr)
        if (marbl_status_log%labort_marbl) then
          call log_add_var_error(marbl_status_log, sname, subname)
          return
        end if
      end do
    end do

    !-----------------------!
    ! marbl_forcing_tmp_nml !
    !-----------------------!

    sname     = 'dust_flux_source'
    lname     = 'Option for atmospheric dust deposition'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => dust_flux_source
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname = 'dust_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, dust_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname     = 'iron_flux_source'
    lname     = 'Option for atmospheric iron deposition'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => iron_flux_source
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname = 'iron_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, iron_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'fesedflux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, fesedflux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname     = 'ndep_data_type'
    lname     = 'Type of ndep forcing'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => ndep_data_type
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname = 'nox_flux_monthly_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, nox_flux_monthly_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'nhy_flux_monthly_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, nhy_flux_monthly_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname     = 'ndep_shr_stream_year_first'
    lname     = 'First year to use in ndep stream file'
    units     = 'years'
    datatype  = 'integer'
    group     = 'marbl_forcing_tmp_nml'
    iptr      => ndep_shr_stream_year_first
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_year_last'
    lname     = 'Last year to use in ndep stream file'
    units     = 'years'
    datatype  = 'integer'
    group     = 'marbl_forcing_tmp_nml'
    iptr      => ndep_shr_stream_year_last
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_year_align'
    lname     = 'Align ndep_shr_stream_year_first with this model year'
    units     = 'year CE'
    datatype  = 'integer'
    group     = 'marbl_forcing_tmp_nml'
    iptr      => ndep_shr_stream_year_align
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_year_file'
    lname     = 'File containing ndep domain and input data'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => ndep_shr_stream_file
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'ndep_shr_stream_scale_factor'
    lname     = 'Unit conversion factor'
    units     = 'unknown'
    datatype  = 'real'
    group     = 'marbl_forcing_tmp_nml'
    rptr      => ndep_shr_stream_scale_factor
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr, add_space=.true.)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname = 'din_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, din_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'dip_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, dip_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'don_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, don_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'dop_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, dop_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'dsi_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, dsi_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'dfe_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, dfe_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'dic_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, dic_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'alk_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, alk_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'doc_riv_flux_input'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, doc_riv_flux_input, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname     = 'gas_flux_forcing_opt'
    lname     = 'Option for where to get gas fluxes'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => gas_flux_forcing_opt
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'gas_flux_forcing_file'
    lname     = 'File containing gas flux forcing fields'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => gas_flux_forcing_file
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname = 'gas_flux_fice'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, gas_flux_fice, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'gas_flux_ws'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, gas_flux_ws, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname = 'gas_flux_ap'
    group     = 'marbl_forcing_tmp_nml'
    call this%add_var_tcr_rd(sname, group, gas_flux_ap, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      call marbl_status_log%log_error_trace('add_var_tcr_rd', subname)
      return
    end if

    sname     = 'atm_co2_opt'
    lname     = 'Option for where to get atmospheric CO2 concentration'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => atm_co2_opt
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'atm_co2_const'
    lname     = 'Value of atmospheric co2'
    units     = 'ppm (dry air; 1 atm)'
    datatype  = 'real'
    group     = 'marbl_forcing_tmp_nml'
    rptr      => atm_co2_const
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'atm_alt_co2_opt'
    lname     = 'Option for where to get alternative atmospheric CO2 concentration'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => atm_alt_co2_opt
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'atm_alt_co2_const'
    lname     = 'Value of atmospheric alternative co2'
    units     = 'ppm (dry air; 1 atm)'
    datatype  = 'real'
    group     = 'marbl_forcing_tmp_nml'
    rptr      => atm_alt_co2_const
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'liron_patch'
    lname     = 'Turn on iron patch fertilization'
    units     = 'unitless'
    datatype  = 'logical'
    group     = 'marbl_forcing_tmp_nml'
    lptr      => liron_patch
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, lptr=lptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'iron_patch_flux_filename'
    lname     = 'File containing iron patch flux'
    units     = 'unitless'
    datatype  = 'string'
    group     = 'marbl_forcing_tmp_nml'
    sptr      => iron_patch_flux_filename
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    sname     = 'iron_patch_month'
    lname     = 'integer month to add patch flux'
    units     = 'unitless'
    datatype  = 'integer'
    group     = 'marbl_forcing_tmp_nml'
    iptr      => iron_patch_month
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    if (ciso_on) then
      sname     = 'ciso_atm_model_year'
      lname     = 'Arbirtrary model year'
      units     = 'unitless'
      datatype  = 'integer'
      group     = 'marbl_forcing_tmp_nml'
      iptr      => ciso_atm_model_year
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      sname     = 'ciso_atm_data_year'
      lname     = 'Year in atmospheric ciso data that corresponds to ciso_atm_model_year'
      units     = 'unitless'
      datatype  = 'integer'
      group     = 'marbl_forcing_tmp_nml'
      iptr      => ciso_atm_data_year
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      sname     = 'ciso_atm_d13c_data_nbval'
      lname     = 'Number of values in ciso_atm_d13c_filename'
      units     = 'unitless'
      datatype  = 'integer'
      group     = 'marbl_forcing_tmp_nml'
      iptr      => ciso_atm_d13c_data_nbval
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, iptr=iptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      if (allocated(ciso_atm_d13c_data)) then
        sname     = 'ciso_atm_d13c_data'
        lname     = 'Atmospheric D13C values in datafile'
        units     = ''
        group     = 'marbl_forcing_tmp_nml'
        call this%add_var_1d_r8(sname, lname, units, group, ciso_atm_d13c_data,   &
                                marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace('add_var_1d_r8', subname)
          return
        end if
      end if

      if (allocated(ciso_atm_d13c_data_yr)) then
        sname     = 'ciso_atm_d13c_data_yr'
        lname     = 'Date of atmospheric D13C values in datafile'
        units     = 'date'
        group     = 'marbl_forcing_tmp_nml'
        call this%add_var_1d_r8(sname, lname, units, group, ciso_atm_d13c_data_yr, &
                                marbl_status_log)
        if (marbl_status_log%labort_marbl) then
          call marbl_status_log%log_error_trace('add_var_1d_r8', subname)
          return
        end if
      end if

      ! ciso_atm_d14c_data, ciso_atm_d14c_data_year
      sname     = 'ciso_atm_d13c_const'
      lname     = 'Atmospheric D13C constant'
      units     = 'permil'
      datatype  = 'real'
      group     = 'marbl_forcing_tmp_nml'
      rptr      => ciso_atm_d13c_const
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      sname     = 'ciso_atm_d14c_const'
      lname     = 'Atmospheric D14C constant'
      units     = 'permil'
      datatype  = 'real'
      group     = 'marbl_forcing_tmp_nml'
      rptr      => ciso_atm_d14c_const
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      sname     = 'ciso_atm_d13c_opt'
      lname     = 'Option for CO2 and D13C (varying or constant forcing)'
      units     = 'unitless'
      datatype  = 'string'
      group     = 'marbl_forcing_tmp_nml'
      sptr      => ciso_atm_d13c_opt
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      sname     = 'ciso_atm_d13c_filename'
      lname     = 'Filename for varying atm D13C'
      units     = 'unitless'
      datatype  = 'string'
      group     = 'marbl_forcing_tmp_nml'
      sptr      => ciso_atm_d13c_filename
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      sname     = 'ciso_atm_d14c_opt'
      lname     = 'Option for CO2 and D14C (varying or constant forcing)'
      units     = 'unitless'
      datatype  = 'string'
      group     = 'marbl_forcing_tmp_nml'
      sptr      => ciso_atm_d14c_opt
      call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, sptr=sptr)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname, subname)
        return
      end if

      sname     = 'ciso_atm_d14c_filename'
      lname     = 'Filenames for varying atm D14C'
      units     = 'unitless'
      group     = 'marbl_forcing_tmp_nml'
      call this%add_var_1d_str(sname, lname, units, group, ciso_atm_d14c_filename, &
                               marbl_status_log)
      if (marbl_status_log%labort_marbl) then
        call marbl_status_log%log_error_trace('add_var_1d_str', subname)
        return
      end if
    end if

  end subroutine marbl_config_construct

  !*****************************************************************************
  
  subroutine set_defaults_tcr_rd(tracer_read_var, mod_varname, filename,      &
             file_varname, scale_factor, default_val, file_fmt)

    type(marbl_tracer_read_type), intent(inout) :: tracer_read_var
    character(len=*), optional :: mod_varname
    character(len=*), optional :: filename
    character(len=*), optional :: file_varname
    real(r8),                optional :: scale_factor
    real(r8),                optional :: default_val
    character(len=*), optional :: file_fmt

    if (present(mod_varname)) then
      tracer_read_var%mod_varname = trim(mod_varname)
    else
      tracer_read_var%mod_varname = 'unknown'
    end if

    if (present(filename)) then
      tracer_read_var%filename = trim(filename)
    else
      tracer_read_var%filename = 'unknown'
    end if

    if (present(file_varname)) then
      tracer_read_var%file_varname = trim(file_varname)
    else
      tracer_read_var%file_varname = 'unknown'
    end if

    if (present(scale_factor)) then
      tracer_read_var%scale_factor = scale_factor
    else
      tracer_read_var%scale_factor = c1
    end if

    if (present(default_val)) then
      tracer_read_var%default_val = default_val
    else
      tracer_read_var%default_val = c0
    end if

    if (present(file_fmt)) then
      tracer_read_var%file_fmt = trim(file_fmt)
    else
      tracer_read_var%file_fmt = 'bin'
    end if

  end subroutine set_defaults_tcr_rd

  !*****************************************************************************

  subroutine marbl_var_add(this, sname, lname, units, datatype, group,        &
             marbl_status_log, rptr, iptr, lptr, sptr, comment, add_space)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),        intent(in)    :: sname
    character(len=*),        intent(in)    :: lname
    character(len=*),        intent(in)    :: units
    character(len=*),        intent(in)    :: datatype
    character(len=*),        intent(in)    :: group
    type(marbl_log_type),    intent(inout) :: marbl_status_log
    real(r8),                optional, pointer, intent(in) :: rptr
    integer,                 optional, pointer, intent(in) :: iptr
    logical,                 optional, pointer, intent(in) :: lptr
    character(len=char_len), optional, pointer, intent(in) :: sptr
    character(len=char_len), optional,          intent(in) :: comment
    logical,                 optional,          intent(in) :: add_space

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add'
    type(marbl_single_config_or_parm_type), dimension(:), pointer :: new_parms
    integer :: old_size, id, n
    character(len=char_len) :: log_message

    if (.not.associated(this%vars)) then
      write(log_message, "(A)") 'Constructor must be run before adding vars'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    old_size = size(this%vars)
    id = old_size+1

    ! 1) Allocate new_parms to be size N (one element larger than this%vars)
    allocate(new_parms(id))

    ! 2) copy this%vars into first N-1 elements of new_parms
    do n=1, old_size
      new_parms(n)%long_name  = this%vars(n)%long_name
      new_parms(n)%short_name = this%vars(n)%short_name
      new_parms(n)%units      = this%vars(n)%units
      new_parms(n)%datatype   = this%vars(n)%datatype
      new_parms(n)%group      = this%vars(n)%group
      new_parms(n)%add_space  = this%vars(n)%add_space
      new_parms(n)%comment    = this%vars(n)%comment
      if (associated(this%vars(n)%lptr)) &
        new_parms(n)%lptr => this%vars(n)%lptr
      if (associated(this%vars(n)%iptr)) &
        new_parms(n)%iptr => this%vars(n)%iptr
      if (associated(this%vars(n)%rptr)) &
        new_parms(n)%rptr => this%vars(n)%rptr
      if (associated(this%vars(n)%sptr)) &
        new_parms(n)%sptr => this%vars(n)%sptr
    end do

    ! 3) add newest parm variable
    select case (trim(datatype))
      case ('real')
        if (present(rptr)) then
          new_parms(id)%rptr => rptr
        else
          write(log_message, "(A)")                                           &
               "Defining real parameter but rptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('integer')
        if (present(iptr)) then
          new_parms(id)%iptr => iptr
        else
          write(log_message, "(A)")                                           &
               "Defining integer parameter but iptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('logical')
        if (present(lptr)) then
          new_parms(id)%lptr => lptr
        else
          write(log_message, "(A)")                                           &
               "Defining logical parameter but lptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case ('string')
        if (present(sptr)) then
          new_parms(id)%sptr => sptr
        else
          write(log_message, "(A)")                                           &
               "Defining string parameter but aptr not present!"
          call marbl_status_log%log_error(log_message, subname)
          return
        end if
      case DEFAULT
        write(log_message, "(2A)") "Unknown datatype: ", trim(datatype)
        call marbl_status_log%log_error(log_message, subname)
        return
    end select
    new_parms(id)%short_name = trim(sname)
    new_parms(id)%long_name  = trim(lname)
    new_parms(id)%units      = trim(units)
    new_parms(id)%datatype   = trim(datatype)
    new_parms(id)%group      = trim(group)
    if (present(add_space)) then
      new_parms(id)%add_space = add_space
    else
      new_parms(id)%add_space = .false.
    end if
    if (present(comment)) then
      new_parms(id)%comment = comment
    else
      new_parms(id)%comment = ''
    end if

    ! 4) deallocate / nullify this%vars
    deallocate(this%vars)
    nullify(this%vars)

    ! 5) point this%vars => new_parms and update parms_cnt
    this%vars => new_parms
    this%cnt = id

  end subroutine marbl_var_add

  !*****************************************************************************

  subroutine marbl_var_add_tcr_rd(this, sname, group, tracer_read_var,        &
                                 marbl_status_log)

    use marbl_interface_types, only : marbl_tracer_read_type

    class(marbl_config_and_parms_type),        intent(inout) :: this
    character(len=char_len),              intent(in)    :: sname
    character(len=char_len),              intent(in)    :: group
    type(marbl_tracer_read_type), target, intent(in)    :: tracer_read_var
    type(marbl_log_type),                 intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_tcr_rd'

    character(len=char_len)          :: sname_loc
    character(len=char_len)          :: lname
    character(len=char_len)          :: units
    character(len=char_len)          :: datatype
    character(len=char_len)          :: prefix
    character(len=char_len), pointer :: sptr => NULL()
    real(r8),                pointer :: rptr => NULL()

    write(prefix,"(2A)") trim(sname), '%'

    write(sname_loc, "(2A)") trim(prefix), 'mod_varname'
    lname     = 'Variable name in module'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => tracer_read_var%mod_varname
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'filename'
    lname     = 'File containing initial tracer values'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => tracer_read_var%filename
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'file_varname'
    lname     = 'Tracer variable name in filename'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => tracer_read_var%file_varname
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'file_fmt'
    lname     = 'Format of filename'
    units     = 'unitless'
    datatype  = 'string'
    sptr      => tracer_read_var%file_fmt
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, sptr=sptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'scale_factor'
    lname     = 'Scale factor to use when reading tracer from file'
    units     = 'unitless (or conversion)'
    datatype  = 'real'
    rptr      => tracer_read_var%scale_factor
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

    write(sname_loc, "(2A)") trim(prefix), 'default_val'
    lname     = 'Initial value of tracer if no file provided'
    units     = 'tracer units'
    datatype  = 'real'
    rptr      => tracer_read_var%default_val
    call this%add_var(sname_loc, lname, units, datatype, group,               &
                      marbl_status_log, rptr=rptr, add_space=.true.)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

  end subroutine marbl_var_add_tcr_rd

  !*****************************************************************************

  subroutine marbl_var_add_1d_r8(this, sname, lname, units, group, r8array,  &
                                 marbl_status_log, add_space)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    real(kind=r8), dimension(:), target, intent(in)    :: r8array
    type(marbl_log_type),                intent(inout) :: marbl_status_log
    logical, optional,                   intent(in)    :: add_space

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_r8'
    character(len=char_len) :: sname_loc
    real(r8), pointer :: rptr => NULL()
    logical :: space
    integer :: n

    do n=1,size(r8array)
      if (present(add_space)) then
        space = add_space.and.(n.eq.size(r8array))
      else
        space = .false.
      end if
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      rptr => r8array(n)
      call this%add_var(sname_loc, lname, units, 'real', group,             &
                          marbl_status_log, rptr=rptr, add_space=space)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_r8

  !*****************************************************************************

  subroutine marbl_var_add_1d_int(this, sname, lname, units, group, intarray, &
                                 marbl_status_log, add_space)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    integer, dimension(:), target,       intent(in)    :: intarray
    type(marbl_log_type),                intent(inout) :: marbl_status_log
    logical, optional,                   intent(in)    :: add_space

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_int'
    character(len=char_len) :: sname_loc
    integer, pointer :: iptr => NULL()
    integer :: n
    logical :: space

    do n=1,size(intarray)
      if (present(add_space)) then
        space = add_space.and.(n.eq.size(intarray))
      else
        space = .false.
      end if
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      iptr => intarray(n)
      call this%add_var(sname_loc, lname, units, 'integer', group,            &
                          marbl_status_log, iptr=iptr, add_space=space)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_int

  !*****************************************************************************

  subroutine marbl_var_add_1d_str(this, sname, lname, units, group,          &
                                  strarray, marbl_status_log)

    class(marbl_config_and_parms_type),       intent(inout) :: this
    character(len=char_len),             intent(in)    :: sname
    character(len=char_len),             intent(in)    :: lname
    character(len=char_len),             intent(in)    :: units
    character(len=char_len),             intent(in)    :: group
    character(len=char_len),     target, intent(in)    :: strarray(:)
    type(marbl_log_type),                intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_add_1d_str'
    character(len=char_len) :: sname_loc
    character(len=char_len), pointer :: sptr => NULL()
    integer :: n
    logical :: islast

    do n=1,size(strarray)
      islast = (n.eq.size(strarray))
      write(sname_loc, "(2A,I0,A)") trim(sname), '(', n, ')'
      sptr => strarray(n)
      call this%add_var(sname_loc, lname, units, 'string', group,           &
                          marbl_status_log, sptr=sptr, add_space=islast)
      if (marbl_status_log%labort_marbl) then
        call log_add_var_error(marbl_status_log, sname_loc, subname)
        return
      end if
    end do

  end subroutine marbl_var_add_1d_str

  !*****************************************************************************

  subroutine marbl_vars_finalize(this, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    type(marbl_log_type),    intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod:marbl_vars_finalize'
    character(len=char_len) :: log_message
    character(len=char_len) :: group
    character(len=7)        :: logic
    integer :: i,n

    ! (1) Lock data type (put calls will now cause MARBL to abort)
    this%locked = .true.
    group = ''

    do n=1,this%cnt
      ! (2) Log the group name if different than previous parameter
      if (this%vars(n)%group.ne.group) then
        group = trim(this%vars(n)%group)
        call marbl_status_log%log_noerror('', subname)
        log_message = ''
        do i=1,len(trim(group))
          log_message(i:i) = '-'
        end do
        call marbl_status_log%log_noerror(log_message, subname)
        call marbl_status_log%log_noerror(trim(group), subname)
        call marbl_status_log%log_noerror(log_message, subname)
        call marbl_status_log%log_noerror('', subname)
      end if

      ! (3) write parameter to log_message (format depends on datatype)
      select case(trim(this%vars(n)%datatype))
        case ('string')
!          print*, trim(this%vars(n)%short_name), len(trim(this%vars(n)%sptr))
          write(log_message, "(4A)") trim(this%vars(n)%short_name), " = '",  &
                                     trim(this%vars(n)%sptr), "'"
        case ('real')
          write(log_message, "(2A,E24.16)") trim(this%vars(n)%short_name),   &
                                            " = ", this%vars(n)%rptr
        case ('integer')
          write(log_message, "(2A,I0)") trim(this%vars(n)%short_name), " = ", &
                                        this%vars(n)%iptr
        case ('logical')
          if (this%vars(n)%lptr) then
            logic = '.true.'
          else
            logic = '.false.'
          end if
          write(log_message, "(3A)") trim(this%vars(n)%short_name), " = ",   &
                                     trim(logic)
        case DEFAULT
          write(log_message, "(2A)") trim(this%vars(n)%datatype),            &
                                     ' is not a valid datatype for parameter'
          call marbl_status_log%log_error(log_message, subname)
          return
      end select

      ! (4) Write log_message to the log
      if (this%vars(n)%comment.ne.'') &
        write(log_message, "(3A)") trim(log_message), ' ! ',                  &
                                   trim(this%vars(n)%comment)
      call marbl_status_log%log_noerror(log_message, subname)
      if (this%vars(n)%add_space) &
        call marbl_status_log%log_noerror('', subname)
    end do

  end subroutine marbl_vars_finalize

  !***********************************************************************

  subroutine log_add_var_error(marbl_status_log, sname, subname)

    type(marbl_log_type), intent(inout) :: marbl_status_log
    character(len=*),     intent(in)    :: sname
    character(len=*),     intent(in)    :: subname
    character(len=char_len) :: routine_name

    write(routine_name,"(3A)") "this%add_var(", trim(sname), ")"
    call marbl_status_log%log_error_trace(routine_name, subname)

  end subroutine log_add_var_error

  !*****************************************************************************

  subroutine set_derived_config(marbl_status_log)

    type(marbl_log_type), intent(inout) :: marbl_status_log

    character(len=*), parameter :: subname = 'marbl_config_mod:set_derived_config'
    character(len=char_len) :: log_message

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

  end subroutine set_derived_config

  !*****************************************************************************

  subroutine marbl_var_put_all_types(this, var, marbl_status_log, rval, ival, &
                                     lval, sval)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),         optional,         intent(in)    :: rval
    integer,          optional,         intent(in)    :: ival
    logical,          optional,         intent(in)    :: lval
    character(len=*), optional,         intent(in)    :: sval
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod%marbl_var_put_all_types'
    character(len=char_len) :: log_message
    integer :: varid

    if (this%locked) then
      write(log_message, "(3A)") 'Can not change value of ', trim(var),       &
                                 ', parameters are locked!'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    varid = this%inquire_id(var, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      write(log_message, "(3A)") 'config_parms%put(', trim(var), ')'
      call marbl_status_log%log_error_trace(log_message, subname)
      return
    end if

    select case(trim(this%vars(varid)%datatype))
      case ('real')
        if (present(rval)) then
          this%vars(varid)%rptr = rval
        else
          write(log_message, "(2A)") trim(var), ' requires real value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('integer')
        if (present(ival)) then
          this%vars(varid)%iptr = ival
        else
          write(log_message, "(2A)") trim(var), ' requires integer value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('logical')
        if (present(lval)) then
          this%vars(varid)%lptr = lval
        else
          write(log_message, "(2A)") trim(var), ' requires logical value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('string')
        if (present(sval)) then
          this%vars(varid)%sptr = sval
        else
          write(log_message, "(2A)") trim(var), ' requires string value'
          call marbl_status_log%log_error(log_message, subname)
        end if
    end select

  end subroutine marbl_var_put_all_types

  !*****************************************************************************

  subroutine marbl_var_get_all_types(this, var, marbl_status_log, rval, ival, &
                                     lval, sval)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),         optional,         intent(out)   :: rval
    integer,          optional,         intent(out)   :: ival
    logical,          optional,         intent(out)   :: lval
    character(len=*), optional,         intent(out)   :: sval
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    character(*), parameter :: subname = 'marbl_config_mod%marbl_var_get_all_types'
    character(len=char_len) :: log_message
    integer :: varid, cnt

    cnt = 0
    if (present(rval)) cnt = cnt + 1
    if (present(ival)) cnt = cnt + 1
    if (present(lval)) cnt = cnt + 1
    if (present(sval)) cnt = cnt + 1

    if (cnt.eq.0) then
      write(log_message, "(A)") 'Must provide rval, ival, lval, or sval to var_get()'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    if (cnt.gt.1) then
      write(log_message, "(A)") 'Must provide just one of rval, ival, lval, or sval to var_get()'
      call marbl_status_log%log_error(log_message, subname)
      return
    end if

    varid = this%inquire_id(var, marbl_status_log)
    if (marbl_status_log%labort_marbl) then
      write(log_message, "(3A)") 'config_parms%get(', trim(var), ')'
      call marbl_status_log%log_error_trace(log_message, subname)
      return
    end if

    select case(trim(this%vars(varid)%datatype))
      case ('real')
        if (present(rval)) then
          rval = this%vars(varid)%rptr
        else
          write(log_message, "(2A)") trim(var), ' requires real value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('integer')
        if (present(ival)) then
          ival = this%vars(varid)%iptr
        else
          write(log_message, "(2A)") trim(var), ' requires integer value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('logical')
        if (present(lval)) then
          lval = this%vars(varid)%lptr
        else
          write(log_message, "(2A)") trim(var), ' requires logical value'
          call marbl_status_log%log_error(log_message, subname)
        end if
      case ('string')
        if (present(sval)) then
          if (len(sval).lt.len(trim(this%vars(varid)%sptr))) then
            write(log_message, "(2A,I0,A,I0,A)") trim(var), ' requires ',     &
              len(trim(this%vars(varid)%sptr)), ' bytes to store, but only ', &
              len(sval), ' are provided.'
            call marbl_status_log%log_error(log_message, subname)
            return
          end if
          sval = trim(this%vars(varid)%sptr)
        else
          write(log_message, "(2A)") trim(var), ' requires string value'
          call marbl_status_log%log_error(log_message, subname)
        end if
    end select

  end subroutine marbl_var_get_all_types

  !*****************************************************************************

  subroutine marbl_var_put_real(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),                           intent(in)    :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%put_general(var, marbl_status_log, rval = val)

  end subroutine marbl_var_put_real

  !*****************************************************************************

  subroutine marbl_var_put_integer(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    integer,                            intent(in)    :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%put_general(var, marbl_status_log, ival=val, rval=real(val,r8))

  end subroutine marbl_var_put_integer

  !*****************************************************************************

  subroutine marbl_var_put_string(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    character(len=*),                   intent(in)    :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%put_general(var, marbl_status_log, sval=val)

  end subroutine marbl_var_put_string

  !*****************************************************************************

  subroutine marbl_var_put_logical(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    logical,                            intent(in)    :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%put_general(var, marbl_status_log, lval=val)

  end subroutine marbl_var_put_logical

  !*****************************************************************************

  subroutine marbl_var_get_real(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    real(r8),                           intent(out)   :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%get_general(var, marbl_status_log, rval = val)

  end subroutine marbl_var_get_real

  !*****************************************************************************

  subroutine marbl_var_get_integer(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    integer,                            intent(out)   :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%get_general(var, marbl_status_log, ival=val)

  end subroutine marbl_var_get_integer

  !*****************************************************************************

  subroutine marbl_var_get_string(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    character(len=*),                   intent(out)   :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%get_general(var, marbl_status_log, sval=val)

  end subroutine marbl_var_get_string

  !*****************************************************************************

  subroutine marbl_var_get_logical(this, var, val, marbl_status_log)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    logical,                            intent(out)   :: val
    type(marbl_log_type),               intent(inout) :: marbl_status_log

    call this%get_general(var, marbl_status_log, lval=val)

  end subroutine marbl_var_get_logical

  !*****************************************************************************

  function marbl_var_inquire_id(this, var, marbl_status_log) result(id)

    class(marbl_config_and_parms_type), intent(inout) :: this
    character(len=*),                   intent(in)    :: var
    type(marbl_log_type),               intent(inout) :: marbl_status_log
    integer                                           :: id

    character(*), parameter :: subname = 'marbl_config_mod:marbl_var_inquire_id'
    character(len=char_len) :: log_message
    integer :: n

    id = 0
    do n=1,this%cnt
      if (trim(var).eq.trim(this%vars(n)%short_name)) then
        id = n
        return
      end if
    end do
    write(log_message, "(2A)") trim(var), ' is not a valid variable name for put'
    call marbl_status_log%log_error(log_message, subname)

  end function marbl_var_inquire_id

  !*****************************************************************************
  
end module marbl_config_mod
