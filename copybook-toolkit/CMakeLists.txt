cmake_minimum_required(VERSION 3.16)
project(copybook-toolkit VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# ── Core library (standalone, no network dependencies) ──────────────────────
add_library(copybook-core
    src/core/record_buffer.cpp
    src/core/record_base.cpp
)
target_include_directories(copybook-core PUBLIC include)

# ── Example: standalone demo ────────────────────────────────────────────────
add_executable(standalone-demo examples/demo/standalone_demo.cpp)
target_link_libraries(standalone-demo copybook-core)
target_include_directories(standalone-demo PRIVATE examples/generated)

# ── Tests ───────────────────────────────────────────────────────────────────
include(FetchContent)
FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.tar.gz
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(copybook-tests
    test/record_buffer_test.cpp
    test/record_base_test.cpp
)
target_link_libraries(copybook-tests copybook-core GTest::gtest_main)
target_include_directories(copybook-tests PRIVATE include examples/generated)

include(GoogleTest)
gtest_discover_tests(copybook-tests)

# ── Interactive ncurses test/demo harness ─────────────────────────────────
find_package(Curses REQUIRED)
add_executable(test-harness tools/test_harness.cpp)
target_link_libraries(test-harness copybook-core ${CURSES_LIBRARIES})
target_include_directories(test-harness PRIVATE
    ${CURSES_INCLUDE_DIRS}
    examples/generated
)
