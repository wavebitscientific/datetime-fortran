add_executable(adds adds.f90)
target_link_libraries(adds datetime)
add_test(NAME "AddSubtract" COMMAND $<TARGET_FILE:adds>)
