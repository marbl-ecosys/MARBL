cmake_minimum_required(VERSION 3.24...4.4)

# CMake's Makefile generator writes a Makefile into the build directory and
# each of its subdirectories, so building in the source directory, src/ or
# tests/driver_src/ would overwrite src/Makefile or tests/driver_src/Makefile.
file(REAL_PATH "${CMAKE_CURRENT_BINARY_DIR}" _marbl_build_dir)
foreach(_marbl_dir . src tests/driver_src)
    file(REAL_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${_marbl_dir}" _marbl_source_dir)
    if(_marbl_build_dir STREQUAL _marbl_source_dir)
        message(FATAL_ERROR
            "MARBL can not be built in ${_marbl_source_dir}: CMake would overwrite "
            "src/Makefile or tests/driver_src/Makefile. Delete the CMakeCache.txt "
            "and CMakeFiles/ this attempt created there, then configure into a "
            "separate directory, e.g.\n"
            "  cmake -S ${CMAKE_CURRENT_SOURCE_DIR} -B ${CMAKE_CURRENT_SOURCE_DIR}/build")
    endif()
endforeach()

project(MARBL LANGUAGES Fortran)

include(GNUInstallDirs)

#################
# BUILD OPTIONS #
#################

option(MARBL_USE_MPI "Use MPI_Wtime() instead of cpu_time() for MARBL's timers" OFF)
option(MARBL_DRIVER_NETCDF "Build the stand-alone driver with netCDF output" OFF)
option(MARBL_INSTALL "Generate install and export rules" ON)

# Flag and driver defaults differ when MARBL is built on its own vs. pulled
# into another project with add_subdirectory(): an including project owns its
# compiler flags and does not need MARBL's test driver. To make warnings
# errors with gfortran, as the Makefiles do, use CMake's own
# -DCMAKE_COMPILE_WARNING_AS_ERROR=ON.
option(MARBL_USE_DEFAULT_FLAGS "Add the compiler flags from src/Makefile and tests/driver_src/Makefile" ${PROJECT_IS_TOP_LEVEL})
option(MARBL_BUILD_DRIVER "Build the stand-alone driver used by the test suite" ${PROJECT_IS_TOP_LEVEL})

# src/Makefile builds at -O2; RelWithDebInfo is the closest CMake build type
get_property(_marbl_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(PROJECT_IS_TOP_LEVEL AND NOT _marbl_multi_config AND NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build" FORCE)
endif()

if(MARBL_USE_MPI)
    find_package(MPI REQUIRED COMPONENTS Fortran)
endif()

add_subdirectory(src)

if(MARBL_BUILD_DRIVER)
    add_subdirectory(tests/driver_src)
endif()

######################
# INSTALL AND EXPORT #
######################

if(MARBL_INSTALL)
    include(CMakePackageConfigHelpers)
    set(MARBL_INSTALL_CMAKEDIR ${CMAKE_INSTALL_LIBDIR}/cmake/MARBL)

    install(TARGETS marbl EXPORT MARBLTargets)
    get_target_property(_marbl_module_dir marbl Fortran_MODULE_DIRECTORY)
    install(DIRECTORY ${_marbl_module_dir}/
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/marbl
        FILES_MATCHING PATTERN "*.mod"
    )
    install(EXPORT MARBLTargets
        NAMESPACE MARBL::
        DESTINATION ${MARBL_INSTALL_CMAKEDIR}
    )
    configure_package_config_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/cmake/MARBLConfig.cmake.in
        ${CMAKE_CURRENT_BINARY_DIR}/MARBLConfig.cmake
        INSTALL_DESTINATION ${MARBL_INSTALL_CMAKEDIR}
    )
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/MARBLConfig.cmake
        DESTINATION ${MARBL_INSTALL_CMAKEDIR}
    )
endif()
