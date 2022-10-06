include_directories(${PROJECT_BINARY_DIR}/src)

add_executable(example_hybrd example_hybrd.f90)
target_link_libraries(example_hybrd minpack)
add_test(example_hybrd example_hybrd)


add_executable(example_hybrd1 example_hybrd1.f90)
target_link_libraries(example_hybrd1 minpack)
add_test(example_hybrd1 example_hybrd1)

add_executable(example_lmder1 example_lmder1.f90)
target_link_libraries(example_lmder1 minpack)
add_test(example_lmder1 example_lmder1)

add_executable(example_lmdif1 example_lmdif1.f90)
target_link_libraries(example_lmdif1 minpack)
add_test(example_lmdif1 example_lmdif1)

add_executable(example_primes example_primes.f90)
target_link_libraries(example_primes minpack)
add_test(example_primes example_primes)
