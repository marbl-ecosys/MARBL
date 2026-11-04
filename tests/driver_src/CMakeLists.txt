add_executable(marbl_driver
    marbl_mpi_mod.F90
    marbl_tools_mod.F90
    marbl_netcdf_mod.F90
    marbl_io_mod.F90
    marbl_init_drv.F90
    marbl_call_compute_subroutines_drv.F90
    marbl_get_put_drv.F90
    marbl_utils_drv.F90
    marbl.F90
)

# Same executable names as tests/driver_src/Makefile
if(MARBL_USE_MPI)
    set(_marbl_exe_name marbl-mpi)
else()
    set(_marbl_exe_name marbl)
endif()
set_target_properties(marbl_driver PROPERTIES
    OUTPUT_NAME ${_marbl_exe_name}
    SUFFIX .exe
    RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/driver_exe
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod
)

target_link_libraries(marbl_driver PRIVATE MARBL::marbl)

if(MARBL_USE_MPI)
    target_compile_definitions(marbl_driver PRIVATE MARBL_TIMING_OPT=MPI)
    target_link_libraries(marbl_driver PRIVATE MPI::MPI_Fortran)
endif()

if(MARBL_DRIVER_NETCDF)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(NETCDF_FORTRAN REQUIRED IMPORTED_TARGET netcdf-fortran)
    target_compile_definitions(marbl_driver PRIVATE _NETCDF)
    target_link_libraries(marbl_driver PRIVATE PkgConfig::NETCDF_FORTRAN)
endif()

# Flags from the per-compiler targets in tests/driver_src/Makefile, on top of
# the library flags. Optimization and debug symbols come from the build type.
if(MARBL_USE_DEFAULT_FLAGS)
    target_compile_options(marbl_driver PRIVATE
        "$<$<Fortran_COMPILER_ID:GNU>:-Wall;-Wextra;-Wno-compare-reals;-finit-real=snan;-ffpe-trap=invalid>"
        "$<$<Fortran_COMPILER_ID:Intel,IntelLLVM>:-init=snan,arrays;-nogen-interface;SHELL:-fp-model source>"
    )
endif()
# From the nag target in tests/driver_src/Makefile; needed to compile, not a tuning flag
target_compile_options(marbl_driver PRIVATE "$<$<Fortran_COMPILER_ID:NAG>:-wmismatch=mpi_bcast>")
