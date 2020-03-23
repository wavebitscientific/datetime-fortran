cmake_minimum_required(VERSION 2.8)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "default to Release")
endif()
project(datetime-fortran
LANGUAGES Fortran
VERSION 1.6.1)

# set output paths for modules, archives, and executables
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# library to archive (libdatetime.a)
add_library(datetime src/lib/datetime.f90 src/lib/mod_datetime.f90 src/lib/mod_timedelta.f90 src/lib/mod_clock.f90 src/lib/mod_strftime.f90 src/lib/mod_constants.f90)

# tests
enable_testing()
add_executable(datetime_tests src/tests/datetime_tests.f90)
target_link_libraries(datetime_tests datetime)
add_test(datetime_tests bin/datetime_tests)
