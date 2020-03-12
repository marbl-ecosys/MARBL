module marbl_netcdf_mod
! This module provides an interface to the netCDF library if MARBL is built with -D_NETCDF
! and will report errors via the marbl_log_type if netCDF is not available.
!
! It is meant to be used with the call_compute_routines regression test, which must be built
! with netCDF support. Machines without netCDF can still built / run the other tests (without
! -D_NETCDF this module does not include netCDF functions but will not provide any I/O)
!
! Note that marbl_io_mod provides an I/O layer that depends on these subroutines so tests
! don't need to call subroutines in this file directly.

  use marbl_kinds_mod,              only : r8, char_len
  use marbl_interface,              only : marbl_interface_class
  use marbl_interface_public_types, only : marbl_diagnostics_type
  use marbl_logging,                only : marbl_log_type

#ifdef _NETCDF
   use netcdf
#endif

  implicit none
  private
  save

  public :: marbl_netcdf_open
  public :: marbl_netcdf_create
  public :: marbl_netcdf_def_dim
  public :: marbl_netcdf_def_var
  public :: marbl_netcdf_enddef
  public :: marbl_netcdf_inq_dimid
  public :: marbl_netcdf_inquire_dimension
  public :: marbl_netcdf_inq_varid
  public :: marbl_netcdf_get_var
  public :: marbl_netcdf_put_var
  public :: marbl_netcdf_close

  interface marbl_netcdf_get_var
    module procedure marbl_netcdf_get_var_int_0d
    module procedure marbl_netcdf_get_var_int_1d
    module procedure marbl_netcdf_get_var_r8_0d
    module procedure marbl_netcdf_get_var_r8_1d
  end interface marbl_netcdf_get_var

  interface marbl_netcdf_put_var
    module procedure marbl_netcdf_put_var_r8_1d
    module procedure marbl_netcdf_put_var_r8_2d
  end interface marbl_netcdf_put_var

contains

  !*****************************************************************************

  subroutine marbl_netcdf_open(file_name, ncid, driver_status_log)
    ! Opens a netCDF file in read-only mode

    character(len=*),     intent(in)    :: file_name
    integer,              intent(out)   :: ncid
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_open'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call open(', trim(file_name), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ncid = -1
    return
#else
    call netcdf_check(nf90_open(file_name, NF90_NOWRITE, ncid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_open(', trim(file_name), ')'
      call driver_status_log%log_error(log_message, subname)
    end if
#endif

  end subroutine marbl_netcdf_open

  !*****************************************************************************

  subroutine marbl_netcdf_create(file_name, ncid, driver_status_log)
    ! Opens a netCDF file in to write (clobbers file if it already exists)

    character(len=*),     intent(in)    :: file_name
    integer,              intent(out)   :: ncid
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_create'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call create(', trim(file_name), ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ncid = -1
    return
#else
    call netcdf_check(nf90_create(file_name, NF90_CLOBBER, ncid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_create(', trim(file_name), ')'
      call driver_status_log%log_error(log_message, subname)
    end if
#endif

  end subroutine marbl_netcdf_create

  !*****************************************************************************

  subroutine marbl_netcdf_def_dim(ncid, name, len, driver_status_log, dimid_out)

    integer,              intent(in)    :: ncid
    character(len=*),     intent(in)    :: name
    integer,              intent(in)    :: len
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(out)   :: dimid_out

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_def_dim'
    character(len=char_len) :: log_message
    integer :: dimid

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call def_dim without netCDF support'
    call driver_status_log%log_error(log_message, subname)

    ! use all variables to avoid warning when building without netcdf
    if (present(dimid_out)) &
      dimid = ncid + len + len_trim(name)

    return
#else
    call netcdf_check(nf90_def_dim(ncid, trim(name), len, dimid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, 3A)") 'nf90_def_dim(', ncid, ', ', trim(name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Store dimid if requested (otherwise we'll inquire for dimid)
    if (present(dimid_out)) dimid_out = dimid
#endif

  end subroutine marbl_netcdf_def_dim

  !*****************************************************************************

  subroutine marbl_netcdf_def_var(ncid, varname, type, dimids, long_name, units, &
                                  driver_status_log, ldef_fillval)

    integer,              intent(in)    :: ncid
    character(len=*),     intent(in)    :: varname
    character(len=*),     intent(in)    :: type
    integer,              intent(in)    :: dimids(:)
    character(len=*),     intent(in)    :: long_name
    character(len=*),     intent(in)    :: units
    type(marbl_log_type), intent(inout) :: driver_status_log
    logical,    optional, intent(in)    :: ldef_fillval

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_def_var'
    character(len=char_len) :: log_message
    integer :: xtype
    integer :: varid

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(3A)") 'Can not call def_var(', trim(varname),') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ! avoid unused variable warnings when building without netCDF
    if (present(ldef_fillval)) then
      varid = ncid + size(dimids) + len(long_name) + len(units) + len(type)
      xtype = 0
    end if
    return
#else

    select case (type)
      case ('double')
        xtype = NF90_DOUBLE
      case DEFAULT
        write(log_message, "(3A)") "Unrecognized type : '", type, "'"
        call driver_status_log%log_error(log_message, subname)
        return
    end select

    ! Define variable
    call netcdf_check(nf90_def_var(ncid, varname, xtype, dimids, varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_def_var(', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Add long_name attribute
    call netcdf_check(nf90_put_att(ncid, varid, "long_name", long_name), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_put_att(long_name) (for ', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Add units attribute
    call netcdf_check(nf90_put_att(ncid, varid, "units", units), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_put_att(units) (for ', trim(varname), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! If requested, add _FillValue attribute
    if (present(ldef_fillval)) then
      if (ldef_fillval) then
        select case (xtype)
          case (NF90_DOUBLE)
            call netcdf_check(nf90_put_att(ncid, varid, "_FillValue", NF90_FILL_DOUBLE), driver_status_log)
          case DEFAULT
            write(log_message, "(A, I0, 3A)") "No fill value defined for xtype = ", xtype, "(type = '", type, "')"
            call driver_status_log%log_error(log_message, subname)
            return
        end select
        if (driver_status_log%labort_marbl) then
          write(log_message, "(3A)") 'nf90_put_att(_FillValue) (for ', trim(varname), ')'
          call driver_status_log%log_error_trace(log_message, subname)
          return
        end if
      end if
    end if
#endif

  end subroutine marbl_netcdf_def_var

  !*****************************************************************************

  subroutine marbl_netcdf_enddef(ncid, driver_status_log)
    integer,              intent(in)    :: ncid
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_enddef'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(A, I0, A)") 'Can not call enddef(', ncid, ') without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    return
#else
    call netcdf_check(nf90_enddef(ncid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, A)") 'nf90_enddef(', ncid, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif
  end subroutine marbl_netcdf_enddef

  !*****************************************************************************

  subroutine marbl_netcdf_inq_dimid(ncid, name, dimid, driver_status_log)

    integer,              intent(in)    :: ncid
    character(len=*),     intent(in)    :: name
    integer,              intent(out)   :: dimid
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_inq_dimid'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call inquire_dimension() without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    dimid = ncid + len(name) ! avoid warning when building without netcdf
    return
#else
    call netcdf_check(nf90_inq_dimid(ncid, trim(name), dimid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_dimid(', trim(name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_inq_dimid

  !*****************************************************************************

  subroutine marbl_netcdf_inquire_dimension(ncid, dimid, len, driver_status_log)

    integer,              intent(in)    :: ncid
    integer,              intent(in)    :: dimid
    integer,              intent(out)   :: len
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_inquire_dimension'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call inquire_dimension without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    len = ncid + dimid ! avoid warning when building without netcdf
    return
#else

    call netcdf_check(nf90_inquire_dimension(ncid, dimid, len=len), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, A, I0, A)") 'nf90_inq_dimension(', ncid, ', ', dimid, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_inquire_dimension

  !*****************************************************************************

  subroutine marbl_netcdf_inq_varid(ncid, name, varid, driver_status_log)

    integer,              intent(in)    :: ncid
    character(len=*),     intent(in)    :: name
    integer,              intent(out)   :: varid
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_inq_varid'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call inquire_dimension() without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    varid = ncid + len(name) ! avoid warning when building without netcdf
    return
#else
    call netcdf_check(nf90_inq_varid(ncid, trim(name), varid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(3A)") 'nf90_inq_varid(', trim(name), ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_inq_varid

  !*****************************************************************************

  subroutine marbl_netcdf_get_var_int_0d(ncid, varid, var, driver_status_log, col_id)
    ! Given netCDF identifier, populate domain variables

    integer,              intent(in)    :: ncid
    integer,              intent(in)    :: varid
    integer,              intent(out)   :: var
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: col_id

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_get_var_int_0d'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call get_var() without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ! avoid unused variable warning when building without netcdf
    if (present(col_id)) var = ncid + varid
    return
#else

    if (present(col_id)) then
      call netcdf_check(nf90_get_var(ncid, varid, var, start=(/col_id/)), driver_status_log)
    else
      call netcdf_check(nf90_get_var(ncid, varid, var), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, A, I0, A)") 'nf90_get_var(', ncid, ', ', varid, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_get_var_int_0d

  !*****************************************************************************

  subroutine marbl_netcdf_get_var_int_1d(ncid, varid, var, driver_status_log)
    ! Given netCDF identifier, populate domain variables

    integer,              intent(in)    :: ncid
    integer,              intent(in)    :: varid
    integer,              intent(out)   :: var(:)
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_get_var_int_1d'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    ! (avoid unused variable warning when building without netcdf)
    var = varid + ncid

    log_message = 'Can not call get_var() without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    return
#else
    call netcdf_check(nf90_get_var(ncid, varid, var), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, A, I0, A)") 'nf90_get_var(', ncid, ', ', varid, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_get_var_int_1d

  !*****************************************************************************

  subroutine marbl_netcdf_get_var_r8_0d(ncid, varid, var, driver_status_log, col_id)
    ! Given netCDF identifier, populate domain variables

    integer,              intent(in)    :: ncid
    integer,              intent(in)    :: varid
    real(kind=r8),        intent(inout) :: var
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: col_id

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_get_var_r8_0d'
    character(len=char_len) :: log_message
    integer :: nc_status
    real(r8) :: scale_factor

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call get_var() without netCDF support'
    call driver_status_log%log_error(log_message, subname)

    ! avoid unused variable warning when building without netcdf
    if (present(col_id)) then
      var = real(ncid + varid, r8)
      nc_status = -1
      scale_factor = 0._r8
    end if
    return
#else
    if (present(col_id)) then
      call netcdf_check(nf90_get_var(ncid, varid, var, start=(/col_id/)), driver_status_log)
    else
      call netcdf_check(nf90_get_var(ncid, varid, var), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, A, I0, A)") 'nf90_get_var(', ncid, ', ', varid, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Get scale_factor, apply it if attribute exists
    nc_status = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    call netcdf_check(nc_status, driver_status_log, ignore_err=NF90_ENOTATT)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_iquire_attribute(scale_factor)', subname)
      return
    end if
    if (nc_status .ne. NF90_ENOTATT) var = scale_factor*var
#endif

  end subroutine marbl_netcdf_get_var_r8_0d

  !*****************************************************************************

  subroutine marbl_netcdf_get_var_r8_1d(ncid, varid, var, driver_status_log, surf_only, col_start, col_cnt)
    ! Given netCDF identifier, populate domain variables

    integer,              intent(in)    :: ncid
    integer,              intent(in)    :: varid
    real(kind=r8),        intent(out)   :: var(:)
    type(marbl_log_type), intent(inout) :: driver_status_log
    logical, optional,    intent(in)    :: surf_only ! if true, only read surface values
    integer, optional,    intent(in)    :: col_start
    integer, optional,    intent(in)    :: col_cnt

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_get_var_r8_1d'
    character(len=char_len) :: log_message
    integer :: num_levels, nc_status
    real(r8) :: scale_factor
    logical :: surf_only_loc

    if (present(surf_only)) then
      if (.not. (present(col_start) .and. present(col_cnt))) then
        call driver_status_log%log_error('Can not pass surf_only without col_start and col_cnt', subname)
        return
      end if
      surf_only_loc = surf_only
      call marbl_netcdf_inq_dimid(ncid, 'zt', num_levels, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('marbl_netcdf_inq_dimid(zt)', subname)
        return
      end if
    else
      surf_only_loc = .false.
    end if

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    ! (avoid unused variable warning when building without netcdf)
    var = real(varid, r8)
    nc_status = 0
    scale_factor = 1.0_r8

    log_message = 'Can not call get_var() without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    return
#else
    if (surf_only_loc) then
      call netcdf_check(nf90_get_var(ncid, varid, var, start=(/1, col_start/), &
                                     count=(/1, col_cnt/)), driver_status_log)
    else
      if (present(col_start)) then
        call netcdf_check(nf90_get_var(ncid, varid, var, start=(/1, col_start/)), driver_status_log)
      else
        call netcdf_check(nf90_get_var(ncid, varid, var), driver_status_log)
      end if
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, A, I0, A)") 'nf90_get_var(', ncid, ', ', varid, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if

    ! Get scale_factor, apply it if attribute exists
    nc_status = nf90_get_att(ncid, varid, 'scale_factor', scale_factor)
    call netcdf_check(nc_status, driver_status_log, ignore_err=NF90_ENOTATT)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('nf90_iquire_attribute(scale_factor)', subname)
      return
    end if
    if (nc_status .ne. NF90_ENOTATT) &
      var = scale_factor*var

#endif

  end subroutine marbl_netcdf_get_var_r8_1d

  !*****************************************************************************

  subroutine marbl_netcdf_put_var_r8_1d(ncid, varid, var, driver_status_log, mask_in)

    integer,              intent(in)    :: ncid
    integer,              intent(in)    :: varid
    real(r8),             intent(in)    :: var(:)
    type(marbl_log_type), intent(inout) :: driver_status_log
    logical, optional,    intent(in)    :: mask_in(:)

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_put_var_r8_1d'
    character(len=char_len) :: log_message
    integer :: col_id

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    log_message = 'Can not call put_var() without netCDF support'
    call driver_status_log%log_error(log_message, subname)
    ! avoid unused variable warnings when building without netCDF
    if (present(mask_in)) col_id = ncid + varid + size(var)
    return
#else

    if (present(mask_in)) then
      do col_id=1, size(mask_in)
        if (mask_in(col_id)) &
          call netcdf_check(nf90_put_var(ncid, varid, var(col_id), start=(/col_id/)), driver_status_log)
      end do
    else
      call netcdf_check(nf90_put_var(ncid, varid, var), driver_status_log)
    end if
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, A, I0,A)") 'nf90_put_var(', ncid, ', ', varid, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

end subroutine marbl_netcdf_put_var_r8_1d

!*****************************************************************************

subroutine marbl_netcdf_put_var_r8_2d(ncid, varid, var, active_level_cnt, driver_status_log)

  integer,              intent(in)    :: ncid
  integer,              intent(in)    :: varid
  real(r8),             intent(in)    :: var(:,:)
  integer,              intent(in)   :: active_level_cnt(:)
  type(marbl_log_type), intent(inout) :: driver_status_log

  character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_put_var_r8_2d'
  character(len=char_len) :: log_message
  integer :: col_id

#ifndef _NETCDF
  ! Abort if not built with -D_NETCDF
  log_message = 'Can not call put_var() without netCDF support'
  call driver_status_log%log_error(log_message, subname)
  ! avoid unused variable warnings when building without netCDF
  col_id = ncid + varid + size(var) + size(active_level_cnt)
  return
#else

  do col_id = 1, size(var, dim=2)
    call netcdf_check(nf90_put_var(ncid, varid, var(1:active_level_cnt(col_id), col_id), &
                                   start=(/1, col_id/)), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A,I0,A,I0,A)") 'nf90_put_var(varid=', varid, ', col_id=', col_id, ')'
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
  end do
#endif

end subroutine marbl_netcdf_put_var_r8_2d

  !*****************************************************************************

  subroutine marbl_netcdf_close(ncid, driver_status_log)
    ! Given netCDF file name, close the file and remove it from file_database

    integer,              intent(in)    :: ncid
    type(marbl_log_type), intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:marbl_netcdf_close'
    character(len=char_len) :: log_message

#ifndef _NETCDF
    ! Abort if not built with -D_NETCDF
    write(log_message, "(A, I0, A)") "Can not call close(", ncid, ") without netCDF support"
    call driver_status_log%log_error(log_message, subname)
    return
#else
    ! Close the netCDF file
    call netcdf_check(nf90_close(ncid), driver_status_log)
    if (driver_status_log%labort_marbl) then
      write(log_message, "(A, I0, A)") "nf90_close(", ncid, ")"
      call driver_status_log%log_error_trace(log_message, subname)
      return
    end if
#endif

  end subroutine marbl_netcdf_close

  !*****************************************************************************

#ifdef _NETCDF
  subroutine netcdf_check(status, driver_status_log, ignore_err)
    ! Private routine to handle errors returned from netcdf
    ! (can only be called if _NETCDF is defined)

    integer,              intent(in)    :: status
    type(marbl_log_type), intent(inout) :: driver_status_log
    integer, optional,    intent(in)    :: ignore_err

    character(len=*), parameter :: subname = 'marbl_netcdf_mod:netcdf_check'
    character(len=char_len) :: log_message

    ! User can specify an error to ignore
    if (present(ignore_err)) then
      if (status.eq.ignore_err) return
    end if

    if (status.ne.nf90_noerr) then
      write(log_message, "(A,I0,2A)") "netCDF error (", status, "): ", trim(nf90_strerror(status))
      call driver_status_log%log_error(log_message, subname)
    end if

  end subroutine netcdf_check
#endif

  !*****************************************************************************

end module marbl_netcdf_mod
