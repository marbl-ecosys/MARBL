module marbl_abio_dic_interior_tendency_mod

    use marbl_settings_mod, only : abio_dic_on

    implicit none
    private

    public :: marbl_abio_dic_interior_tendency_compute

contains

    subroutine marbl_abio_dic_interior_tendency_compute()

        ! Return immediately if not running with abiotic dic tracer module
        if (.not. abio_dic_on) return

    end subroutine marbl_abio_dic_interior_tendency_compute

end module marbl_abio_dic_interior_tendency_mod
