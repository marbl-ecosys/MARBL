module marbl_ciso_init_mod

  use marbl_kinds_mod, only : int_kind
  use marbl_settings_mod, only : ciso_on
  use marbl_interface_public_types, only : marbl_tracer_metadata_type
  use marbl_interface_private_types, only : marbl_tracer_index_type

  implicit none
  private

  public  :: marbl_ciso_init_tracer_metadata

contains

  !*****************************************************************************

  subroutine marbl_ciso_init_tracer_metadata(marbl_tracer_metadata,           &
                                             marbl_tracer_indices)

    !  Set tracer and forcing metadata
    use marbl_settings_mod, only : ciso_lecovars_full_depth_tavg
    use marbl_settings_mod, only : autotroph_cnt
    use marbl_settings_mod, only : autotroph_settings

    type (marbl_tracer_metadata_type) , intent(inout) :: marbl_tracer_metadata(:)   ! descriptors for each tracer
    type(marbl_tracer_index_type)     , intent(in)    :: marbl_tracer_indices

    !-----------------------------------------------------------------------
    !  local variables
    !-----------------------------------------------------------------------
    integer (int_kind) :: n                             ! tracer index
    integer (int_kind) :: auto_ind                      ! autotroph functional group index

    if (.not. ciso_on) return

    !-----------------------------------------------------------------------
    !  initialize non-autotroph metadata values
    !-----------------------------------------------------------------------

    associate(di13c_ind         => marbl_tracer_indices%di13c_ind,            &
              do13ctot_ind      => marbl_tracer_indices%do13ctot_ind,         &
              zootot13C_ind     => marbl_tracer_indices%zootot13C_ind,        &
              di14c_ind         => marbl_tracer_indices%di14c_ind,            &
              do14ctot_ind      => marbl_tracer_indices%do14ctot_ind,         &
              zootot14C_ind     => marbl_tracer_indices%zootot14C_ind,        &
              ciso_ind_beg      => marbl_tracer_indices%ciso%ind_beg,         &
              ciso_ind_end      => marbl_tracer_indices%ciso%ind_end          &
             )

    ! All CISO tracers share units, tend_units, flux_units, and
    ! tracer_module_name
    do n=ciso_ind_beg,ciso_ind_end
      marbl_tracer_metadata(n)%units      = 'mmol/m^3'
      marbl_tracer_metadata(n)%tend_units = 'mmol/m^3/s'
      marbl_tracer_metadata(n)%flux_units = 'mmol/m^3 cm/s'
      marbl_tracer_metadata(n)%tracer_module_name = 'ciso'
    end do
    marbl_tracer_metadata(di13c_ind)%short_name='DI13C'
    marbl_tracer_metadata(di13c_ind)%long_name='Dissolved Inorganic Carbon-13'

    marbl_tracer_metadata(do13ctot_ind)%short_name='DO13Ctot'
    marbl_tracer_metadata(do13ctot_ind)%long_name='Dissolved Organic Carbon-13 (semi-labile+refractory)'

    marbl_tracer_metadata(zootot13C_ind)%short_name='zootot13C'
    marbl_tracer_metadata(zootot13C_ind)%long_name='Zooplankton Carbon-13 (sum over all zooplankton)'

    marbl_tracer_metadata(di14c_ind)%short_name='DI14C'
    marbl_tracer_metadata(di14c_ind)%long_name='Dissolved Inorganic Carbon-14'

    marbl_tracer_metadata(do14ctot_ind)%short_name='DO14Ctot'
    marbl_tracer_metadata(do14ctot_ind)%long_name='Dissolved Organic Carbon-14 (semi-labile+refractory)'

    marbl_tracer_metadata(zootot14C_ind)%short_name='zootot14C'
    marbl_tracer_metadata(zootot14C_ind)%long_name='Zooplankton Carbon-14 (sum over all zooplankton)'

    !-----------------------------------------------------------------------
    !  initialize autotroph tracer_d values and tracer indices
    !-----------------------------------------------------------------------

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
       marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // '13C'
       marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Carbon-13'

       n = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
       marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // '14C'
       marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Carbon-14'

       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
       if (n .gt. 0) then
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Ca13CO3'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Ca13CO3'
        end if

       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
       if (n .gt. 0) then
          marbl_tracer_metadata(n)%short_name = trim(autotroph_settings(auto_ind)%sname) // 'Ca14CO3'
          marbl_tracer_metadata(n)%long_name  = trim(autotroph_settings(auto_ind)%lname) // ' Ca14CO3'
       endif
    end do

    !-----------------------------------------------------------------------
    !  set lfull_depth_tavg flag for short-lived ecosystem tracers
    !-----------------------------------------------------------------------

    marbl_tracer_metadata(zootot13C_ind)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg
    marbl_tracer_metadata(zootot14C_ind)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg

    do auto_ind = 1, autotroph_cnt
       n = marbl_tracer_indices%auto_inds(auto_ind)%C13_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%C14_ind
       marbl_tracer_metadata(n)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg

       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca13CO3_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg
       endif

       n = marbl_tracer_indices%auto_inds(auto_ind)%Ca14CO3_ind
       if (n > 0) then
          marbl_tracer_metadata(n)%lfull_depth_tavg = ciso_lecovars_full_depth_tavg
       endif
    end do

    end associate

  end subroutine marbl_ciso_init_tracer_metadata

  !*****************************************************************************

end module marbl_ciso_init_mod