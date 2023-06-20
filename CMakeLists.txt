cmake_minimum_required(VERSION 3.14)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "default to Release")
endif()

project(datetime-fortran
LANGUAGES Fortran
VERSION 1.8.0)
include(CTest)

include(cmake/check_strptime.cmake)
include(cmake/check_strftime.cmake)

# library to archive (libdatetime.a)
add_library(datetime src/datetime_module.f90)
target_include_directories(datetime INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET datetime PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)


if(NOT HAVE_C_STRPTIME)
  enable_language(CXX)
  set(CMAKE_CXX_STANDARD 11)
  target_sources(datetime PRIVATE src/strptime.cpp)
endif()

# MinGW GCC needs this for strftime to work
# https://sourceforge.net/p/mingw-w64/bugs/793/
target_link_libraries(datetime PRIVATE "$<$<BOOL:${MINGW}>:ucrtbase>")

# tests
if(BUILD_TESTING)
  add_executable(datetime_tests test/datetime_tests.f90)
  target_link_libraries(datetime_tests datetime)
  set_target_properties(datetime_tests PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  LINKER_LANGUAGE Fortran)
  add_test(NAME datetime_tests COMMAND $<TARGET_FILE:datetime_tests>)

  add_subdirectory(example)
endif(BUILD_TESTING)
