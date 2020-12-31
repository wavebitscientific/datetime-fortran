add_executable(add_and_subtract add_and_subtract.f90)
target_link_libraries(add_and_subtract datetime)
add_test(NAME "add_and_subtract" COMMAND $<TARGET_FILE:add_and_subtract>)
