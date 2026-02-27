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

# ── Web-based visual designer ─────────────────────────────────────────
add_executable(web-designer tools/web_designer/main.cpp)
target_link_libraries(web-designer
    copybook-core
    copybook-parser
    copybook-serial
    nlohmann_json::nlohmann_json
    pthread
)
target_include_directories(web-designer PRIVATE
    tools/web_designer
    examples/generated
)

# ── gRPC / Protobuf transport layer ──────────────────────────────────────
find_package(PkgConfig REQUIRED)
pkg_check_modules(GRPC REQUIRED grpc++)
pkg_check_modules(PROTOBUF REQUIRED protobuf)

# Transport library (record registry + gRPC service + client)
add_library(copybook-transport
    src/transport/record_transport.cpp
    src/transport/grpc_service.cpp
    src/transport/grpc_client.cpp
    src/transport/copybook_service.pb.cc
    src/transport/copybook_service.grpc.pb.cc
)
target_include_directories(copybook-transport PUBLIC
    include
    src/transport
    ${GRPC_INCLUDE_DIRS}
    ${PROTOBUF_INCLUDE_DIRS}
)
target_link_libraries(copybook-transport
    copybook-core
    copybook-parser
    copybook-serial
    ${GRPC_LIBRARIES}
    ${PROTOBUF_LIBRARIES}
)
target_compile_options(copybook-transport PRIVATE ${GRPC_CFLAGS_OTHER} ${PROTOBUF_CFLAGS_OTHER})

# gRPC server executable
add_executable(grpc-server tools/grpc_server.cpp)
target_link_libraries(grpc-server copybook-transport)

# gRPC client demo executable
add_executable(grpc-client-demo tools/grpc_client_demo.cpp)
target_link_libraries(grpc-client-demo copybook-transport)

# Transport tests
add_executable(transport-tests test/transport_test.cpp)
target_link_libraries(transport-tests
    copybook-transport
    GTest::gtest_main
)
target_include_directories(transport-tests PRIVATE include src/transport)
gtest_discover_tests(transport-tests)

# ── Validation library ───────────────────────────────────────────────────────
add_library(copybook-validation
    src/validation/validation_engine.cpp
)
target_link_libraries(copybook-validation copybook-core)
target_include_directories(copybook-validation PUBLIC include)

# Validation tests
add_executable(validation-tests test/validation_test.cpp)
target_link_libraries(validation-tests
    copybook-validation
    copybook-transport
    GTest::gtest_main
)
target_include_directories(validation-tests PRIVATE include src/transport)
gtest_discover_tests(validation-tests)

# ── Integration tests (end-to-end) ──────────────────────────────────────────
add_executable(integration-tests test/integration_test.cpp)
target_link_libraries(integration-tests
    copybook-validation
    copybook-transport
    GTest::gtest_main
)
target_include_directories(integration-tests PRIVATE include src/transport)
gtest_discover_tests(integration-tests)
