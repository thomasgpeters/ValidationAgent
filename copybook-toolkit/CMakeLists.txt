cmake_minimum_required(VERSION 3.16)
project(copybook-toolkit VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(FetchContent)

# ── Core library (standalone, no network dependencies) ──────────────────────
add_library(copybook-core
    src/core/record_buffer.cpp
    src/core/record_base.cpp
)
target_include_directories(copybook-core PUBLIC include)

# ── Parser library (copybook reader + C++ code generator) ─────────────────
add_library(copybook-parser
    src/parser/copybook_parser.cpp
    src/parser/codegen.cpp
)
target_link_libraries(copybook-parser copybook-core)
target_include_directories(copybook-parser PUBLIC include)

# ── nlohmann/json (header-only JSON library) ──────────────────────────────
FetchContent_Declare(
    nlohmann_json
    URL https://github.com/nlohmann/json/releases/download/v3.11.3/json.tar.xz
)
FetchContent_MakeAvailable(nlohmann_json)

# ── Serialization library (JSON + YAML) ──────────────────────────────────
add_library(copybook-serial
    src/serial/json_serializer.cpp
    src/serial/yaml_serializer.cpp
)
target_link_libraries(copybook-serial copybook-core nlohmann_json::nlohmann_json)
target_include_directories(copybook-serial PUBLIC include)

# ── Example: standalone demo ────────────────────────────────────────────────
add_executable(standalone-demo examples/demo/standalone_demo.cpp)
target_link_libraries(standalone-demo copybook-core)
target_include_directories(standalone-demo PRIVATE examples/generated)

# ── Tests ───────────────────────────────────────────────────────────────────
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
    test/copybook_parser_test.cpp
    test/codegen_test.cpp
    test/serializer_test.cpp
)
target_link_libraries(copybook-tests copybook-core copybook-parser copybook-serial GTest::gtest_main)
target_include_directories(copybook-tests PRIVATE include examples/generated)

include(GoogleTest)
gtest_discover_tests(copybook-tests)

# ── Interactive ncurses test/demo harness ─────────────────────────────────
find_package(Curses REQUIRED)
add_executable(test-harness tools/test_harness.cpp)
target_link_libraries(test-harness copybook-core copybook-parser copybook-serial ${CURSES_LIBRARIES})
target_include_directories(test-harness PRIVATE
    ${CURSES_INCLUDE_DIRS}
    examples/generated
)
