cmake_minimum_required(VERSION 3.0)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "default to Release")
endif()
project(datetime-fortran
LANGUAGES Fortran
VERSION 1.6.2)
enable_testing()

# library to archive (libdatetime.a)
add_library(datetime src/lib/datetime.f90 src/lib/mod_datetime.f90 src/lib/mod_timedelta.f90
src/lib/mod_clock.f90 src/lib/mod_strftime.f90 src/lib/mod_constants.f90)
target_include_directories(datetime INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include)
set_target_properties(datetime PROPERTIES
Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

if(WIN32)
  enable_language(CXX)
  add_library(strptime OBJECT strptime.cpp)
  set_target_properties(strptime PROPERTIES CXX_STANDARD 11 CXX_STANDARD_REQUIRED true)
  target_link_libraries(datetime PRIVATE strptime)
endif()

# tests
add_executable(datetime_tests src/tests/datetime_tests.f90)
target_link_libraries(datetime_tests datetime)
set_target_properties(datetime_tests PROPERTIES
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
LINKER_LANGUAGE Fortran)
add_test(NAME datetime_tests COMMAND $<TARGET_FILE:datetime_tests>)
