! -*- mode: f90; indent-tabs-mode: nil; f90-do-indent:3; f90-if-indent:3; f90-type-indent:3; f90-program-indent:2; f90-associate-indent:0; f90-continuation-indent:5  -*-

module marbl_temperature

  use marbl_kinds_mod, only : int_kind
  use marbl_kinds_mod, only : r8

  implicit none

  private

  public :: &
       marbl_temperature_potemp, &
       marbl_temperature_adtg

contains

  !*****************************************************************************

  function marbl_temperature_potemp(n, salt, temp, press, ref_press)

    ! !DESCRIPTION:
    !  Compute potential temperature at ref_press (potemp)
    !
    !  Ref : Algorithms for computation of fundamental properties of seawater,
    !  Unesco technical papers in marine science, 44.
    !
    !  Ref : Fofonoff 1977, Computation of potential temperature of seawater
    !  for an arbitrary reference pressure, Deep Sea Research, 24, 489-491,
    !  doi:10.1016/0146-6291(77)90485-4.
    !
    !  Unesco check value: salt = 40, temp = 40 degC, press = 1000 bar, ref_press = 0,
    !    potemp = 36.89073 degC.

    use marbl_constants_mod, only : c1, c2, c3, p5

    integer(int_kind), intent(in)  :: n
    real (r8),         intent(in)  :: salt(n)      ! salinity [psu]
    real (r8),         intent(in)  :: temp(n)      ! in situ temperature at press [degC]
    real (r8),         intent(in)  :: press(n)     ! pressure [bar]
    real (r8),         intent(in)  :: ref_press(n) ! reference pressure [bar]

    real (r8) :: marbl_temperature_potemp(n)    ! potential temperature [degC]

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    real (r8) :: dpress(n)
    real (r8) :: press_mid(n)
    real (r8) :: dtemp(n)
    real (r8) :: temp_arg(n)
    real (r8) :: q(n)

    real (r8), parameter :: q2w1 = (c2 - sqrt(c2))
    real (r8), parameter :: q2w2 = (-c2 + c3 / sqrt(c2))
    real (r8), parameter :: q3w1 = (c2 + sqrt(c2))
    real (r8), parameter :: q3w2 = (-c2 - c3 / sqrt(c2))
    real (r8), parameter :: t2w = (c1 - c1 / sqrt(c2))
    real (r8), parameter :: t3w = (c1 + c1 / sqrt(c2))
    real (r8), parameter :: sixth = c1 / 6.0_r8

    !-----------------------------------------------------------------------
    ! Note that Fofonoff 1973 has a typo in the equation for q3. The last term
    ! should be q2, not dtheta2. The last term in the equation for q2 should
    ! probably be q1, not dtheta1, but this doesn't matter, since q1=dtheta1.
    !-----------------------------------------------------------------------

    dpress(:)    = ref_press(:) - press(:)
    press_mid(:) = press(:) + p5 * dpress(:)

    ! set dtemp1
    dtemp(:)     = dpress(:) * marbl_temperature_adtg(n, salt, temp, press)

    ! set temp1, q1, dtemp2 (using dtemp=dtemp1)
    temp_arg(:)  = temp(:) + p5 * dtemp(:)
    q(:)         = dtemp(:)
    dtemp(:)     = dpress(:) * marbl_temperature_adtg(n, salt, temp_arg, press_mid)

    ! set temp2, q2, dtemp3 (using temp_arg=temp1, q=q1, dtemp=dtemp2)
    temp_arg(:)  = temp_arg(:) + t2w * (dtemp(:) - q(:))
    q(:)         = q2w1 * dtemp(:) + q2w2 * q(:)
    dtemp(:)     = dpress(:) * marbl_temperature_adtg(n, salt, temp_arg, press_mid)

    ! set temp3, q3, dtemp4 (using temp_arg=temp2, q=q2, dtemp=dtemp3)
    temp_arg(:)  = temp_arg(:) + t3w * (dtemp(:) - q(:))
    q(:)         = q3w1 * dtemp(:) + q3w2 * q(:)
    dtemp(:)     = dpress(:) * marbl_temperature_adtg(n, salt, temp_arg, ref_press)

    ! set potemp (using temp_arg=temp3, q=q3, dtemp=dtemp4)
    marbl_temperature_potemp(:) = temp_arg(:) + sixth * (dtemp(:) - c2 * q(:))

    !-----------------------------------------------------------------------

  end function marbl_temperature_potemp

  !*****************************************************************************

  function marbl_temperature_adtg(n, salt, temp, press)

    ! !DESCRIPTION:
    !  Compute adiabatic temperature gradient (adtg)
    !
    !  Ref : Algorithms for computation of fundamental properties of seawater,
    !  Unesco technical papers in marine science, 44.
    !
    !  Ref : Bryden 1973, New polynomials for thermal expansion, adiabatic
    !  temperature gradient and potential temperature of sea water, Deep Sea
    !  Research, 20, 401-408, doi:10.1016/0011-7471(73)90063-6.
    !
    !  Unesco check value: salt = 40, temp = 40 degC, press = 1000 bar, adtg = 3.255976e-3 degC/bar.

    integer(int_kind), intent(in)  :: n
    real (r8),         intent(in)  :: salt(n)  ! salinity [psu]
    real (r8),         intent(in)  :: temp(n)  ! in situ temperature [degC]
    real (r8),         intent(in)  :: press(n) ! pressure [bar]

    real (r8) :: marbl_temperature_adtg(n)  ! adiabatic temperature gradient [degC/bar]

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------

    real (r8) :: saltm35(n)

    ! note that in Bryden 1973, pressure units are dbar and gradient units are degC/(1000 dbar)
    real (r8), parameter :: bar_to_dbar           = 10.0_r8
    real (r8), parameter :: per_k_dbar_to_per_bar = 1.0e-2_r8 ! (1000 dbar = 100 bar)

    real (r8), parameter :: a000 =  0.35803e-1_r8  * per_k_dbar_to_per_bar
    real (r8), parameter :: a001 =  0.85258e-2_r8  * per_k_dbar_to_per_bar
    real (r8), parameter :: a002 = -0.68360e-4_r8  * per_k_dbar_to_per_bar
    real (r8), parameter :: a003 =  0.66228e-6_r8  * per_k_dbar_to_per_bar

    real (r8), parameter :: a010 =  0.18932e-2_r8  * per_k_dbar_to_per_bar
    real (r8), parameter :: a011 = -0.42393e-4_r8  * per_k_dbar_to_per_bar

    real (r8), parameter :: a100 =  0.18741e-4_r8  * per_k_dbar_to_per_bar * bar_to_dbar
    real (r8), parameter :: a101 = -0.67795e-6_r8  * per_k_dbar_to_per_bar * bar_to_dbar
    real (r8), parameter :: a102 =  0.87330e-8_r8  * per_k_dbar_to_per_bar * bar_to_dbar
    real (r8), parameter :: a103 = -0.54481e-10_r8 * per_k_dbar_to_per_bar * bar_to_dbar

    real (r8), parameter :: a110 = -0.11351e-6_r8  * per_k_dbar_to_per_bar * bar_to_dbar
    real (r8), parameter :: a111 =  0.27759e-8_r8  * per_k_dbar_to_per_bar * bar_to_dbar

    real (r8), parameter :: a200 = -0.46206e-9_r8  * per_k_dbar_to_per_bar * bar_to_dbar * bar_to_dbar
    real (r8), parameter :: a201 =  0.18676e-10_r8 * per_k_dbar_to_per_bar * bar_to_dbar * bar_to_dbar
    real (r8), parameter :: a202 = -0.21687e-12_r8 * per_k_dbar_to_per_bar * bar_to_dbar * bar_to_dbar

    !-----------------------------------------------------------------------

    saltm35(:) = salt(:) - 35.0_r8
    marbl_temperature_adtg(:) = a000 + temp(:) * (a001 + temp(:) * (a002 + temp(:) * a003)) &
      + saltm35(:) * (a010 + temp(:) * a011) &
      + press(:) * (a100 + temp(:) * (a101 + temp(:) * (a102 + temp(:) * a103)) &
                    + saltm35(:) * (a110 + temp(:) * a111) &
                    + press(:) * (a200 + temp(:) * (a201 + temp(:) * a202)))

    !-----------------------------------------------------------------------

  end function marbl_temperature_adtg

end module marbl_temperature
