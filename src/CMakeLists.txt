# src/Makefile builds every .F90 file in this directory; add new files here too
add_library(marbl STATIC
    marbl_abio_dic_diagnostics_mod.F90
    marbl_abio_dic_interior_tendency_mod.F90
    marbl_abio_dic_surface_flux_mod.F90
    marbl_ciso_diagnostics_mod.F90
    marbl_ciso_interior_tendency_mod.F90
    marbl_ciso_surface_flux_mod.F90
    marbl_co2calc_mod.F90
    marbl_constants_mod.F90
    marbl_debug_mod.F90
    marbl_diagnostics_mod.F90
    marbl_diagnostics_share_mod.F90
    marbl_glo_avg_mod.F90
    marbl_init_mod.F90
    marbl_init_tracer_metadata_mod.F90
    marbl_interface.F90
    marbl_interface_constants.F90
    marbl_interface_private_types.F90
    marbl_interface_public_types.F90
    marbl_interior_tendency_mod.F90
    marbl_interior_tendency_share_mod.F90
    marbl_kinds_mod.F90
    marbl_logging.F90
    marbl_nhx_surface_emis_mod.F90
    marbl_oxygen.F90
    marbl_pft_mod.F90
    marbl_restore_mod.F90
    marbl_saved_state_mod.F90
    marbl_schmidt_number_mod.F90
    marbl_settings_mod.F90
    marbl_surface_flux_mod.F90
    marbl_surface_flux_share_mod.F90
    marbl_temperature.F90
    marbl_timing_mod.F90
    marbl_utils_mod.F90
)
add_library(MARBL::marbl ALIAS marbl)

set(MARBL_MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod)
set_target_properties(marbl PROPERTIES
    Fortran_MODULE_DIRECTORY ${MARBL_MODULE_DIR}
)

target_include_directories(marbl
    INTERFACE $<BUILD_INTERFACE:${MARBL_MODULE_DIR}>
              $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/marbl>
)

# Consumers of the static library still link MPI; they do not compile against it
if(MARBL_USE_MPI)
    target_compile_definitions(marbl PRIVATE MARBL_TIMING_OPT=MPI)
    target_link_libraries(marbl PRIVATE MPI::MPI_Fortran)
endif()

# Flags from the per-compiler targets in src/Makefile. Optimization and
# free-form / module-directory flags are left to CMake. The Makefile's intel
# target predates ifx, which accepts the same flags. SHELL: keeps
# "-fp-model source" together when CMake de-duplicates a target's options.
if(MARBL_USE_DEFAULT_FLAGS)
    target_compile_options(marbl PRIVATE
        "$<$<Fortran_COMPILER_ID:GNU>:-Wall;-Wextra;-Wno-compare-reals>"
        "$<$<Fortran_COMPILER_ID:Intel,IntelLLVM>:-nogen-interface;SHELL:-fp-model source>"
    )
endif()
