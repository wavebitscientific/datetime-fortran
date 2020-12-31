cmake_minimum_required(VERSION 3.0)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "default to Release")
endif()
project(datetime-fortran
LANGUAGES Fortran
VERSION 1.7.0)
include(CTest)

# library to archive (libdatetime.a)
add_library(datetime src/datetime_module.f90)
target_include_directories(datetime INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include)
set_target_properties(datetime PROPERTIES
Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)

if(WIN32)
  enable_language(CXX)
  add_library(strptime OBJECT src/strptime.cpp)
  target_compile_features(strptime PRIVATE cxx_std_11)
  target_link_libraries(datetime PRIVATE strptime)
endif()

if(MINGW)
  # MinGW / MSYS2 GCC needs this for strftime to work
  # https://sourceforge.net/p/mingw-w64/bugs/793/
  target_link_libraries(datetime PRIVATE "ucrtbase")
endif(MINGW)

# tests
if(BUILD_TESTING)
  add_executable(datetime_tests tests/datetime_tests.f90)
  target_link_libraries(datetime_tests datetime)
  set_target_properties(datetime_tests PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  LINKER_LANGUAGE Fortran)
  add_test(NAME datetime_tests COMMAND $<TARGET_FILE:datetime_tests>)

  add_subdirectory(examples)
endif(BUILD_TESTING)
