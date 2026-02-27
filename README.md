# COBOL Copybook Toolkit

A C++17 toolkit for working with COBOL copybook record layouts. Replaces a
legacy Java-based `DataObjectBase` / `PassListBase` reflection-driven system
with a compile-time registry pattern that is faster, safer, and easier to
maintain.

---

## Executive Summary

This toolkit provides a complete solution for parsing, generating, validating,
serializing, and transporting COBOL copybook records in modern C++. It
eliminates the Java reflection overhead, fixes the child buffer synchronization
bug present in the original system, and adds capabilities that did not exist
before: a visual designer, gRPC transport, rule-based validation, and a CI/CD
pipeline.

**184 automated tests** across 4 test suites ensure correctness, with CI
running on every push across GCC and Clang in both Debug and Release modes.

### At a Glance

| Metric | Value |
|---|---|
| Language | C++17 |
| Libraries | 5 static libraries |
| Test suites | 4 (184 total tests) |
| Build system | CMake 3.16+ |
| CI/CD | GitHub Actions (GCC + Clang, Debug + Release) |
| External deps | nlohmann/json, GoogleTest (auto-fetched), gRPC, Protobuf |

---

## Features

### Core Engine
Parse and manipulate fixed-length COBOL record buffers with type-safe field
access. Supports CHARACTER, ZONED_NUMERIC, ZONED_UNSIGNED, PACKED_DECIMAL,
BINARY, GROUP, and FILLER field types.

- [User Guide: Core API Reference](copybook-toolkit/USER_GUIDE.md#core-api-reference)
- [Dev Options: Library Components](copybook-toolkit/DEVELOPMENT_OPTIONS.md#library-components)

### Copybook Parser & Code Generator
Automatically parse `.cpy` files and generate complete C++ record class headers
with field constants, constructors, typed accessors, and child GROUP support.

- [User Guide: Copybook Parser](copybook-toolkit/USER_GUIDE.md#copybook-parser)
- [User Guide: C++ Code Generator](copybook-toolkit/USER_GUIDE.md#c-code-generator)
- [Dev Options: Parser Reference](copybook-toolkit/DEVELOPMENT_OPTIONS.md#copybook-parser)
- [Dev Options: Code Generation Options](copybook-toolkit/DEVELOPMENT_OPTIONS.md#code-generation-options)

### JSON & YAML Serialization
Convert records to/from JSON (via nlohmann/json) and YAML with support for
numeric field conversion, nested GROUP objects, trimming options, and
cross-format round-trips (Buffer <-> JSON <-> YAML <-> Buffer).

- [User Guide: JSON Serialization](copybook-toolkit/USER_GUIDE.md#json-serialization)
- [User Guide: YAML Serialization](copybook-toolkit/USER_GUIDE.md#yaml-serialization)
- [Dev Options: Cross-Format Round-Trips](copybook-toolkit/DEVELOPMENT_OPTIONS.md#cross-format-round-trips)

### gRPC Transport Layer
Remote access to copybook records over gRPC with 7 RPCs including bidirectional
streaming. Includes `RecordRegistry` for dynamic record creation without code
generation, plus typed C++ client and server.

- [User Guide: gRPC Transport](copybook-toolkit/USER_GUIDE.md#grpc-transport-layer)
- [Dev Options: gRPC Transport](copybook-toolkit/DEVELOPMENT_OPTIONS.md#grpc-transport-layer)
- [Proto Definition](copybook-toolkit/proto/copybook_service.proto)

### Validation Engine
Rule-based validation framework with 6 built-in rule types (Required, Range,
Pattern, Length, Enum, Custom), configurable severity levels (ERROR, WARNING,
INFO), cross-field validation via lambdas, and human-readable or JSON error
reporting.

- [User Guide: Validation Engine](copybook-toolkit/USER_GUIDE.md#validation-engine)
- [Dev Options: Validation Engine](copybook-toolkit/DEVELOPMENT_OPTIONS.md#validation-engine)

### Visual Designer
Web-based HTML5/SVG class diagram designer served by a self-contained C++ HTTP
server. Drag-and-drop UML-style nodes, field detail dialogs, C++ code
generation, JSON/YAML export, and copybook import — all in the browser.

- [User Guide: Visual Designer](copybook-toolkit/USER_GUIDE.md#visual-designer)
- [Dev Options: Visual Designer](copybook-toolkit/DEVELOPMENT_OPTIONS.md#visual-designer-web-application)
- [Dev Options: REST API Reference](copybook-toolkit/DEVELOPMENT_OPTIONS.md#rest-api-reference-web-designer)

### Interactive Test Harness
ncurses-based interactive UI with 7 features: run tests, standalone demo,
record inspector with live hex dump, buffer round-trip testing, field layout
visualizer, serialization demo, and parser/codegen demo.

- [User Guide: Test Harness](copybook-toolkit/USER_GUIDE.md#test-harness)
- [Dev Options: Interactive Test Harness](copybook-toolkit/DEVELOPMENT_OPTIONS.md#interactive-test-harness)

### CI/CD Pipeline
GitHub Actions pipeline with a GCC/Clang x Debug/Release build matrix running
all 184 tests across 4 suites with artifact upload.

- [User Guide: CI/CD Pipeline](copybook-toolkit/USER_GUIDE.md#cicd-pipeline)
- [Dev Options: CI/CD Pipeline](copybook-toolkit/DEVELOPMENT_OPTIONS.md#cicd-pipeline)
- [Workflow Config](.github/workflows/ci.yml)

---

## Quick Start

```bash
# Build
cd copybook-toolkit
mkdir -p build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug
cmake --build .

# Run all tests (184 tests)
ctest --output-on-failure

# Start the visual designer
./web-designer            # http://localhost:8080

# Start the gRPC server
./grpc-server             # localhost:50051

# Run the interactive test harness
./test-harness
```

### Minimal Code Example

```cpp
#include <copybook/transport/record_transport.h>
#include <copybook/validation/validation_engine.h>
#include <copybook/serial/json_serializer.h>

using namespace copybook::transport;
using namespace copybook::validation;

// Load copybooks and create a record
RecordRegistry registry;
registry.loadDirectory("examples/copybooks");
auto record = registry.createBlankRecord("PERSON");
record->setData("ID", "EMP-001");
record->setData("FIRST_NAME", "Thomas");
record->setData("AGE", "060");

// Validate
ValidationEngine engine;
engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
auto result = engine.validate("PERSON", *record);
// result.valid == true

// Serialize to JSON
std::string json = JsonSerializer::toJson(*record);
```

---

## Documentation

| Document | Description |
|---|---|
| [USER_GUIDE.md](copybook-toolkit/USER_GUIDE.md) | Complete user guide with API reference, tutorials, and migration guide |
| [DEVELOPMENT_OPTIONS.md](copybook-toolkit/DEVELOPMENT_OPTIONS.md) | Feature reference with detailed examples, REST API docs, and workflow recipes |
| [DEVELOPMENT_LOG.md](copybook-toolkit/DEVELOPMENT_LOG.md) | Sprint-by-sprint development history and changelog |

---

## Project Structure

```
ValidationAgent/
├── README.md                               This file
├── .github/workflows/ci.yml               GitHub Actions CI pipeline
└── copybook-toolkit/
    ├── CMakeLists.txt                      Build system
    ├── USER_GUIDE.md                       User guide & API reference
    ├── DEVELOPMENT_OPTIONS.md              Feature reference & examples
    ├── DEVELOPMENT_LOG.md                  Sprint history & changelog
    ├── include/copybook/                   Public headers
    │   ├── core/                           RecordBuffer, RecordBase
    │   ├── parser/                         CopybookParser, Codegen
    │   ├── serial/                         JsonSerializer, YamlSerializer
    │   ├── transport/                      RecordRegistry, gRPC service/client
    │   └── validation/                     ValidationEngine, rule types
    ├── src/                                Implementation files
    ├── proto/                              Protobuf service definition
    ├── examples/                           Sample copybooks & generated code
    ├── test/                               184 tests across 4 suites
    └── tools/                              Server, client, harness, designer
```

---

## Development Sprints

| Sprint | Deliverables | Tests |
|---|---|---|
| **1: Core Engine** | RecordBuffer, RecordBase, Person proof-of-concept | 43 |
| **2: Parser + Serialization** | CopybookParser, Codegen, JSON/YAML serializers | 115 |
| **Visual Designer** | HTML5/SVG web app, REST API, drag-and-drop | — |
| **3: gRPC Transport** | 7-RPC service, RecordRegistry, streaming | 140 |
| **4: Validation Engine** | 6 rule types, severity levels, formatted reporting | 168 |
| **5: Integration & CI** | End-to-end tests, GitHub Actions pipeline | 184 |

See [DEVELOPMENT_LOG.md](copybook-toolkit/DEVELOPMENT_LOG.md) for detailed
sprint history including key decisions, bugs fixed, and design rationale.

---

## Migration from Java

The toolkit provides API-compatible `setData()` / `getData()` methods while
fixing critical issues in the Java implementation:

| Aspect | Java Original | C++ Toolkit |
|---|---|---|
| Field lookup | `java.lang.reflect.Field` | `unordered_map<string, FieldDescriptor>` |
| Child buffer sync | Broken (data lost) | Fixed via `syncChildren()` / `merge()` |
| Type safety | String constants | `enum class FieldType` |
| Error messages | Generic reflection errors | Field name in exception |
| Bounds checking | None (array crash) | Explicit `std::out_of_range` |

See [User Guide: Migration from Java](copybook-toolkit/USER_GUIDE.md#migration-from-java)
for a complete migration guide with before/after examples.
