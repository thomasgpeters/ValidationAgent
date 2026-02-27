# COBOL Copybook Toolkit — User Guide

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Quick Start](#quick-start)
4. [Building the Project](#building-the-project)
5. [Core API Reference](#core-api-reference)
   - [RecordBuffer](#recordbuffer)
   - [RecordBase](#recordbase)
   - [FieldType](#fieldtype)
   - [FieldDescriptor](#fielddescriptor)
6. [Defining a Record Class](#defining-a-record-class)
7. [Working with Records](#working-with-records)
8. [Child Records (GROUP Items)](#child-records-group-items)
9. [Numeric Fields](#numeric-fields)
10. [Buffer Serialization and Round-Trips](#buffer-serialization-and-round-trips)
11. [COBOL Type Mapping](#cobol-type-mapping)
12. [Copybook Parser](#copybook-parser)
13. [C++ Code Generator](#c-code-generator)
14. [JSON Serialization](#json-serialization)
15. [YAML Serialization](#yaml-serialization)
16. [gRPC Transport Layer](#grpc-transport-layer)
17. [Validation Engine](#validation-engine)
18. [Visual Designer](#visual-designer)
19. [Migration from Java](#migration-from-java)
20. [Test Harness](#test-harness)
21. [Running Tests](#running-tests)
22. [CI/CD Pipeline](#cicd-pipeline)
23. [Project Structure](#project-structure)
24. [Development Options Reference](#development-options-reference)
25. [Roadmap](#roadmap)

---

## Overview

The COBOL Copybook Toolkit is a C++17 library that provides direct, type-safe
access to COBOL copybook record layouts. It replaces the Java-based
`DataObjectBase` / `PassListBase` reflection-driven approach with a
compile-time registry pattern that is faster, safer, and easier to debug.

### Key Design Goals

- **API compatibility** — `setData()` / `getData()` behave identically to the
  Java originals, so migration is incremental
- **No reflection** — field metadata is stored in an `unordered_map` registered
  at construction time, not discovered via `java.lang.reflect.Field`
- **Buffer synchronization** — child GROUP records are properly merged back into
  the parent buffer before serialization (fixing the Java version's buffer-sync bug)
- **Bounds checking** — every buffer access is validated with descriptive error
  messages
- **Zero external dependencies** — the core library requires only the C++17
  standard library

---

## Architecture

```
┌───────────────────────────────────────────────────────────────┐
│                      Your Application                         │
├───────────────────────────────────────────────────────────────┤
│  Generated / Dynamic Record Classes                           │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐              │
│  │ Person     │  │ Account    │  │GenericRecord│  ...         │
│  │ : RecordBase  │ : RecordBase  │ : RecordBase               │
│  └────────────┘  └────────────┘  └────────────┘              │
├───────────────────────────────────────────────────────────────┤
│  Validation (libcopybook-validation.a)                        │
│  ┌───────────────────┐  ┌──────────────────────────────────┐  │
│  │ ValidationEngine  │  │ Rules: Required, Range, Pattern, │  │
│  │ validate()        │  │ Length, Enum, Custom (lambda)     │  │
│  │ formatResult()    │  │ Severity: ERROR / WARNING / INFO  │  │
│  └───────────────────┘  └──────────────────────────────────┘  │
├───────────────────────────────────────────────────────────────┤
│  Transport (libcopybook-transport.a)                          │
│  ┌────────────────┐  ┌──────────────────┐  ┌──────────────┐  │
│  │ RecordRegistry │  │CopybookServiceImpl│ │CopybookClient│  │
│  │ load/create    │  │ gRPC server impl  │ │ gRPC client   │  │
│  └────────────────┘  └──────────────────┘  └──────────────┘  │
├───────────────────────────────────────────────────────────────┤
│  Serialization (libcopybook-serial.a)                         │
│  ┌────────────────┐  ┌────────────────┐                       │
│  │ JsonSerializer │  │ YamlSerializer │                       │
│  └────────────────┘  └────────────────┘                       │
├───────────────────────────────────────────────────────────────┤
│  Parser (libcopybook-parser.a)                                │
│  ┌────────────────┐  ┌────────────────┐                       │
│  │ CopybookParser │  │    Codegen     │                       │
│  └────────────────┘  └────────────────┘                       │
├───────────────────────────────────────────────────────────────┤
│  Core Library (libcopybook-core.a)                            │
│  ┌──────────────┐  ┌──────────────┐  ┌───────────────┐       │
│  │ RecordBase   │  │ RecordBuffer │  │FieldDescriptor│       │
│  │ setData()    │  │ read/write   │  │FieldType      │       │
│  │ getData()    │  │ slice/merge  │  │ name, offset   │       │
│  │ setNumeric() │  │ boundsCheck  │  │ size, type     │       │
│  └──────────────┘  └──────────────┘  └───────────────┘       │
└───────────────────────────────────────────────────────────────┘
```

---

## Quick Start

```cpp
#include <copybook/core/record_base.h>
#include "person.h"   // generated from PERSON.cpy

using namespace copybook::examples;

int main() {
    // Create a blank 115-byte Person record
    Person person;

    // Set fields by name — same API as Java setData()
    person.setData("FIRST_NAME", "Thomas");
    person.setData("LAST_NAME",  "Peters");
    person.setNumeric("AGE", 60);

    // Access child GROUP records
    person.homePhone().setData("NUMBER", "555-123-4567");

    // Sync children back into the parent buffer
    person.syncChildren();

    // Serialize to fixed-length string (115 bytes)
    std::string raw = person.getData();

    // Deserialize from raw bytes
    Person copy(raw.c_str(), raw.size());
    assert(copy.getData("FIRST_NAME") == person.getData("FIRST_NAME"));
}
```

---

## Building the Project

### Prerequisites

| Requirement | Minimum Version | Purpose |
|---|---|---|
| C++ compiler | C++17 (GCC 7+, Clang 5+) | Build all targets |
| CMake | 3.16+ | Build system |
| gRPC | 1.51+ | Transport layer |
| Protobuf | 3.21+ | Wire protocol |
| ncurses | 6.x | Interactive test harness |
| pkg-config | any | Find gRPC/protobuf |

GoogleTest and nlohmann/json are fetched automatically by CMake — no manual installation needed.

**Install dependencies:**

```bash
# macOS (Homebrew)
brew update && brew install grpc protobuf pkg-config ncurses

# macOS 12 (Monterey) — if Homebrew fails, install from source:
#   See https://grpc.io/docs/languages/cpp/quickstart/ for building
#   gRPC from source with -DCMAKE_INSTALL_PREFIX=$HOME/.local
#   Then: export PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig

# Ubuntu/Debian
sudo apt-get install -y libgrpc++-dev libprotobuf-dev \
    protobuf-compiler-grpc libncurses5-dev pkg-config
```

### Build Commands

```bash
cd copybook-toolkit
mkdir -p build && cd build

# Configure
cmake .. -DCMAKE_BUILD_TYPE=Debug

# Build everything (libraries + tools + tests)
cmake --build .

# Run all tests
ctest --output-on-failure

# Run individual test suites
./copybook-tests          # 115 core/parser/serial tests
./transport-tests         # 25 gRPC transport tests
./validation-tests        # 28 validation rule/engine tests
./integration-tests       # 16 end-to-end integration tests

# Run standalone demo
./standalone-demo

# Start the visual designer
./web-designer            # http://localhost:8080

# Start the gRPC server
./grpc-server             # localhost:50051
```

### Build Targets

| Target | Type | Description |
|---|---|---|
| `copybook-core` | Library | RecordBuffer, RecordBase, FieldDescriptor |
| `copybook-parser` | Library | CopybookParser, Codegen |
| `copybook-serial` | Library | JsonSerializer, YamlSerializer |
| `copybook-transport` | Library | RecordRegistry, gRPC service/client |
| `copybook-validation` | Library | ValidationEngine, validation rules |
| `standalone-demo` | Executable | Non-interactive Person record demo |
| `copybook-tests` | Executable | 115 unit tests (GoogleTest) |
| `transport-tests` | Executable | 25 gRPC transport tests |
| `validation-tests` | Executable | 28 validation engine tests |
| `integration-tests` | Executable | 16 end-to-end integration tests |
| `test-harness` | Executable | ncurses interactive test/demo UI |
| `web-designer` | Executable | HTML5/SVG visual class diagram designer |
| `grpc-server` | Executable | Standalone gRPC server |
| `grpc-client-demo` | Executable | Client demo for all gRPC RPCs |

---

## Core API Reference

### RecordBuffer

**Header:** `include/copybook/core/record_buffer.h`

A fixed-length character buffer mirroring a COBOL record's raw byte layout.
This replaces the Java `char[] data_elements` array.

| Method | Description |
|---|---|
| `RecordBuffer(size_t size, char fill=' ')` | Create buffer of `size` bytes, filled with `fill` |
| `RecordBuffer(const char* raw, size_t size)` | Create buffer from raw byte array (copies data) |
| `write(int offset, int length, string value, char pad=' ')` | Write value at offset; pads or truncates to fit |
| `read(int offset, int length) → string` | Read `length` bytes from `offset` |
| `slice(int offset, int length) → RecordBuffer` | Extract independent sub-buffer copy |
| `merge(int offset, const RecordBuffer& child)` | Write child buffer back into parent at offset |
| `raw() → char*` | Direct pointer to underlying data |
| `size() → size_t` | Buffer size in bytes |
| `toString() → string` | Entire buffer as a string |

All methods throw `std::out_of_range` on invalid access.

### RecordBase

**Header:** `include/copybook/core/record_base.h`

Base class for all copybook record mappings. Subclasses register their fields
at construction time via `registerField()`.

#### String Field Access

```cpp
void   setData(const string& fieldName, const string& value);
string getData(const string& fieldName) const;
string getData() const;  // entire buffer
```

- `setData` right-pads with spaces if the value is shorter than the field
- `setData` truncates if the value is longer than the field
- Both throw `std::invalid_argument` if the field name is unknown

#### Numeric Field Access

```cpp
void setNumeric(const string& fieldName, long value);
long getNumeric(const string& fieldName) const;
```

- `setNumeric` writes a zero-padded, right-justified number
- `getNumeric` strips leading spaces/zeros and parses to long
- Returns 0 for blank or unparseable fields

#### Field Introspection

```cpp
bool                  hasField(const string& fieldName) const;
vector<string>        fieldNames() const;
const FieldDescriptor& fieldInfo(const string& fieldName) const;
size_t                recordSize() const;
```

#### Child Record Management

```cpp
shared_ptr<RecordBase> getChild(const string& name) const;
void syncChildren();     // flush child buffers → parent
void refreshChildren();  // reload children from parent buffer
```

#### Buffer I/O

```cpp
const RecordBuffer& buffer() const;
void loadFromBuffer(const char* raw, size_t size);
```

### FieldType

**Header:** `include/copybook/core/field_type.h`

```cpp
enum class FieldType {
    CHARACTER,       // PIC X(n)  — alphanumeric
    ZONED_NUMERIC,   // PIC S9(n) — signed zoned decimal
    ZONED_UNSIGNED,  // PIC 9(n)  — unsigned zoned decimal
    PACKED_DECIMAL,  // COMP-3    — packed BCD (future)
    BINARY,          // COMP      — binary integer (future)
    RECORD,          // GROUP     — contains sub-fields
    FILLER           // FILLER    — padding bytes
};
```

Use `fieldTypeName(FieldType t)` to get a printable string.

### FieldDescriptor

**Header:** `include/copybook/core/field_descriptor.h`

```cpp
struct FieldDescriptor {
    string         name;               // e.g. "COMPANY_NO"
    int            size;               // byte length
    int            offset;             // byte position in parent buffer
    FieldType      type;               // COBOL data type
    int            decimal_positions;  // implied decimal digits (V99 → 2)
    bool           is_group;           // true for GROUP items
    vector<string> children;           // child field names
};
```

---

## Defining a Record Class

Each COBOL copybook maps to a C++ class that inherits from `RecordBase`.
Here is the pattern:

```cpp
#pragma once
#include <copybook/core/record_base.h>

namespace copybook { namespace examples {

class MyRecord : public RecordBase {
public:
    static constexpr int RECORD_SIZE = 80;  // total record length

    MyRecord() : RecordBase(RECORD_SIZE) { initFields(); }
    MyRecord(const char* raw, size_t len) : RecordBase(raw, len) { initFields(); }

private:
    void initFields() {
        //                    name         size  offset  type
        registerField({"ACCOUNT_NO",  10,   0,   FieldType::CHARACTER});
        registerField({"BALANCE",      9,  10,   FieldType::ZONED_UNSIGNED, 2}); // V99
        registerField({"STATUS",       1,  19,   FieldType::CHARACTER});
        // ... remaining fields up to RECORD_SIZE
    }
};

}} // namespace
```

### Rules

1. **RECORD_SIZE** must equal the sum of all field sizes (including FILLER)
2. **Offsets** are cumulative byte positions from the start of the record
3. **Child GROUP items** use `FieldType::RECORD` and require `registerChild()`
4. **FILLER** bytes do not need to be registered (they remain as spaces)

---

## Working with Records

### Creating a Record

```cpp
// Blank record (space-filled)
Person p;

// From raw bytes (e.g., received over the wire or read from a file)
char raw[115];
memset(raw, ' ', 115);
Person p2(raw, sizeof(raw));
```

### Setting Fields

```cpp
p.setData("FIRST_NAME", "Thomas");     // right-padded: "Thomas                          "
p.setData("FIRST_NAME", "");           // clears to spaces
p.setData("CODE", "ABCDEFGHIJKLMNO");  // truncated to field size
```

### Getting Fields

```cpp
string name = p.getData("FIRST_NAME");  // "Thomas                          "
string full = p.getData();               // entire 115-byte buffer
```

### Field Existence Check

```cpp
if (p.hasField("SALARY")) {
    // field exists
}
```

---

## Child Records (GROUP Items)

COBOL GROUP items are modeled as child `RecordBase` objects. When a record
is constructed, child buffers are sliced from the parent. Modifications to
child records must be synced back before the parent is serialized.

```cpp
Person p;

// Modify child records directly
p.homePhone().setData("NUMBER", "555-123-4567");
p.workPhone().setData("NUMBER", "555-987-6543");

// IMPORTANT: sync before reading the full parent buffer
p.syncChildren();
string raw = p.getData();  // now contains phone data at correct offsets
```

### Why syncChildren() matters

In the original Java implementation, child record objects operated on
independent copies of the parent buffer segment. Mutations to children
were never written back to the parent, causing data loss during
serialization. The C++ toolkit fixes this with explicit `syncChildren()`
and `refreshChildren()` calls.

| Operation | When to call |
|---|---|
| `syncChildren()` | Before `getData()` or any serialization of the parent |
| `refreshChildren()` | After `loadFromBuffer()` (called automatically) |

---

## Numeric Fields

COBOL numeric fields (`PIC 9(n)`) store digits as character bytes.
The toolkit provides convenience methods for numeric access:

```cpp
person.setNumeric("AGE", 60);
// Stored in buffer as: "060" (zero-padded, right-justified)

long age = person.getNumeric("AGE");
// Returns: 60
```

### Implied Decimal (PIC 9(5)V99)

Fields with implied decimals have a `decimal_positions` value in their
`FieldDescriptor`. For example, `PIC 9(5)V99` stores the value 123.45
as `"0012345"` (7 bytes, no actual decimal point). The `decimal_positions`
metadata (set to 2 in this case) tells the application how to interpret
the stored value.

---

## Buffer Serialization and Round-Trips

Records serialize to and from fixed-length strings, maintaining wire
compatibility with the existing COBOL/Java infrastructure:

```cpp
// Serialize
Person p1;
p1.setData("FIRST_NAME", "Alice");
p1.syncChildren();
std::string wire = p1.getData();  // exactly 115 bytes

// Transmit wire over gRPC, write to file, etc.

// Deserialize
Person p2(wire.c_str(), wire.size());
assert(p2.getData("FIRST_NAME") == p1.getData("FIRST_NAME"));
```

---

## COBOL Type Mapping

| COBOL PIC Clause | FieldType | C++ Storage | Example |
|---|---|---|---|
| `PIC X(n)` | `CHARACTER` | string, space-padded | `"Thomas    "` |
| `PIC 9(n)` | `ZONED_UNSIGNED` | string, zero-padded | `"060"` |
| `PIC S9(n)` | `ZONED_NUMERIC` | string, sign in low nibble | `"060"` |
| `PIC 9(n) COMP-3` | `PACKED_DECIMAL` | packed BCD (future) | — |
| `PIC 9(n) COMP` | `BINARY` | binary integer (future) | — |
| GROUP item | `RECORD` | child RecordBase | nested object |
| `FILLER` | `FILLER` | not addressable | padding bytes |

---

## Copybook Parser

The parser reads standard COBOL `.cpy` files and produces a structured
`CopybookDefinition` with field names, offsets, sizes, and types computed
automatically.

```cpp
#include <copybook/parser/copybook_parser.h>

CopybookParser parser;
auto def = parser.parseFile("ACCOUNT.cpy");

std::cout << def.record_name << ": " << def.total_size << " bytes\n";
for (const auto& f : def.fields) {
    std::cout << f.cpp_name << " offset=" << f.offset
              << " size=" << f.size << "\n";
}
```

Supported PIC clauses: `X(n)`, `9(n)`, `S9(n)`, `9(n)V9(m)`, `9(n)V99`,
`COMP`, `COMP-3`, `FILLER`, and GROUP items (no PIC clause).

---

## C++ Code Generator

The code generator takes a parsed `CopybookDefinition` and emits a complete
C++ header file.

```cpp
#include <copybook/parser/codegen.h>

CopybookParser parser;
Codegen codegen;

auto def = parser.parseFile("PERSON.cpy");
codegen.generateFile(def, "generated/");  // writes generated/person.h
```

Generated headers include: `RECORD_SIZE` constants, field SIZE/OFFSET constants,
constructors, `registerField()` calls, child GROUP classes with typed accessors,
and `syncChildren()` support.

---

## JSON Serialization

Uses [nlohmann/json](https://github.com/nlohmann/json) for robust JSON
parsing and generation.

```cpp
#include <copybook/serial/json_serializer.h>

// Record → JSON
Person p = makeTestPerson();
std::string json = JsonSerializer::toJson(p);
// {"ID": "EMP-001", "FIRST_NAME": "Thomas", "AGE": 60, ...}

// JSON → Record
Person p2;
JsonSerializer::fromJson(p2, json);

// Access as nlohmann::json object
auto j = JsonSerializer::toJsonObject(p);
std::string name = j["FIRST_NAME"];
```

Numeric fields emit as JSON numbers. GROUP items become nested objects.
Trailing spaces are trimmed by default.

---

## YAML Serialization

Lightweight built-in YAML serializer — no external dependency.

```cpp
#include <copybook/serial/yaml_serializer.h>

// Record → YAML
std::string yaml = YamlSerializer::toYaml(person);
// ---
// ID: EMP-001
// FIRST_NAME: Thomas
// AGE: 60
// HOME_PHONE:
//   NUMBER: 555-123-4567

// YAML → Record
Person p;
YamlSerializer::fromYaml(p, yaml);
```

---

## gRPC Transport Layer

The transport layer provides remote access to copybook records over gRPC,
replacing the Java socket-based server.

### RecordRegistry

Manages copybook schemas and creates records dynamically at runtime (no
code generation needed):

```cpp
#include <copybook/transport/record_transport.h>
using namespace copybook::transport;

RecordRegistry registry;
registry.loadDirectory("examples/copybooks");  // load all .cpy files

// Create records by name
auto record = registry.createBlankRecord("PERSON");
record->setData("FIRST_NAME", "Alice");

// Create from raw COBOL buffer
auto record2 = registry.createRecord("PERSON", rawBuffer, 115);

// Extract/apply field values
auto fields = RecordRegistry::extractFields(*record);
RecordRegistry::applyFields(*record2, fields);
```

### gRPC Server

```bash
# Start the server (loads all copybooks from a directory)
./grpc-server 50051 ../examples/copybooks
```

The server exposes 7 RPCs: `SendRecord`, `GetFields`, `SetFields`,
`GetSchema`, `ListSchemas`, `Convert`, and `StreamRecords` (bidirectional).

### gRPC Client

```cpp
#include <copybook/transport/grpc_client.h>
using namespace copybook::transport;

CopybookClient client("localhost:50051");

// List available schemas
auto schemas = client.listSchemas();

// Pack fields into a COBOL buffer
std::string buffer = client.setFields("PERSON", {
    {"ID", "EMP-001"}, {"FIRST_NAME", "Thomas"}, {"AGE", "060"}
}, 115);

// Convert buffer to JSON
std::string json = client.toJson("PERSON", buffer.c_str(), buffer.size());

// Stream multiple records
auto results = client.streamRecords(batch);
```

---

## Validation Engine

A rule-based validation framework for COBOL copybook records.

### Quick Start

```cpp
#include <copybook/validation/validation_engine.h>
#include <copybook/validation/validation_rule.h>
using namespace copybook::validation;

ValidationEngine engine;

// Field-level rules
engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
engine.addRule("PERSON", std::make_shared<EnumRule>("SEX",
    std::vector<std::string>{"M", "F", "X"}));
engine.addRule("PERSON", std::make_shared<PatternRule>(
    "DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD"));

// Validate
auto result = engine.validate("PERSON", *record);
if (!result.valid) {
    std::cerr << ValidationEngine::formatResult(result);
}
```

### Built-in Rule Types

| Rule | Purpose |
|---|---|
| `RequiredRule` | Field must not be empty (all spaces) |
| `RangeRule` | Numeric field within [min, max] |
| `PatternRule` | Regex match on trimmed field value |
| `LengthRule` | Trimmed length within [min, max] |
| `EnumRule` | Value must be in allowed set |
| `CustomRule` | Lambda for cross-field logic |

### Severity Levels

Rules support `Severity::ERROR` (fails validation), `Severity::WARNING`,
and `Severity::INFO`. Only ERROR causes `result.valid` to be false.

### Cross-Field Validation

```cpp
engine.addRule("PERSON", std::make_shared<CustomRule>("active-needs-id",
    [](const RecordBase& rec, ValidationResult& res) {
        std::string status = rec.getData("STATUS");
        if (status.find_first_not_of(' ') != std::string::npos &&
            status[0] == 'A') {
            std::string id = rec.getData("ID");
            if (id.find_first_not_of(' ') == std::string::npos)
                res.addError("ID", "active-needs-id",
                    "Active records must have an ID");
        }
    }));
```

### Result Formatting

Results can be output as human-readable text or machine-readable JSON:

```cpp
std::string text = ValidationEngine::formatResult(result);
std::string json = ValidationEngine::formatResultJson(result);
```

See [DEVELOPMENT_OPTIONS.md](DEVELOPMENT_OPTIONS.md#validation-engine) for
the full API reference including global rules, engine management, and
detailed examples.

---

## Visual Designer

A web-based HTML5/SVG visual class diagram designer for COBOL copybook
definitions. Runs as a self-contained C++ HTTP server with no external
web framework dependencies.

```bash
cd copybook-toolkit/build
./web-designer 8080 ../examples/copybooks
# Open http://localhost:8080
```

### Features

- **Class diagram view** — UML-style nodes for each copybook record
- **Drag-and-drop** — rearrange nodes on the SVG canvas
- **Bezier curve arrows** — connect parent records to GROUP children
- **Field detail dialog** — double-click any node to see all fields with
  offsets, sizes, types, and PIC clauses
- **Generate C++** — produce a complete C++ header from any copybook
- **Export JSON/YAML** — export copybook schemas in structured formats
- **Import Copybook** — paste `.cpy` source to add new records to the diagram

See [DEVELOPMENT_OPTIONS.md](DEVELOPMENT_OPTIONS.md#visual-designer-web-application)
for the full REST API reference and keyboard shortcuts.

---

## Migration from Java

### Field Access — Before (Java)

```java
// Java: uses reflection to find FIELD_SIZE, FIELD_OFFSET, FIELD_TYPE
person.setData("FIRST_NAME", "Thomas");
String name = person.getData("FIRST_NAME");
```

### Field Access — After (C++)

```cpp
// C++: identical API, but uses registry map (no reflection)
person.setData("FIRST_NAME", "Thomas");
std::string name = person.getData("FIRST_NAME");
```

### Record Class — Before (Java)

```java
public class PersonImpl extends DataObjectBase {
    public static final int FIRST_NAME_SIZE   = 32;
    public static final int FIRST_NAME_OFFSET = 12;
    public static final String FIRST_NAME_TYPE = "CHARACTER";
    // ... repeated for every field
}
```

### Record Class — After (C++)

```cpp
class Person : public RecordBase {
    void initFields() {
        registerField({"FIRST_NAME", 32, 12, FieldType::CHARACTER});
        // ... one line per field
    }
};
```

### Key Differences

| Aspect | Java Original | C++ Toolkit |
|---|---|---|
| Field lookup | `java.lang.reflect.Field` | `unordered_map<string, FieldDescriptor>` |
| Child buffer sync | Broken (data lost) | Fixed via `syncChildren()` / `merge()` |
| Type safety | String constants (`"CHARACTER"`) | `enum class FieldType` |
| Error messages | Generic reflection errors | Field name included in exception |
| Bounds checking | None (array index crash) | Explicit with `std::out_of_range` |
| Dependencies | Full JRE + reflection API | C++17 standard library only |

---

## Test Harness

An interactive ncurses-based test harness is provided for running unit tests,
demos, and inspecting records interactively.

```bash
cd copybook-toolkit/build
./test-harness
```

### Harness Features

- **Run All Tests** — executes the GoogleTest suite and shows pass/fail results
- **Run Standalone Demo** — launches the non-interactive demo
- **Interactive Record Inspector** — create and edit records in real time, see
  the hex/character buffer update live
- **Keyboard navigation** — arrow keys to select, Enter to run, `q` to quit

---

## Running Tests

### All Tests

```bash
cd copybook-toolkit/build
ctest --output-on-failure
```

### Individual Test Suites

```bash
./copybook-tests          # 115 core/parser/serial tests
./transport-tests         # 25 gRPC transport tests
./validation-tests        # 28 validation rule/engine tests
./integration-tests       # 16 end-to-end integration tests
```

**Total: 184 tests** across 4 suites.

### Filtering Tests

```bash
./copybook-tests --gtest_filter="RecordBufferTest.*"
./copybook-tests --gtest_filter="PersonTest.*"
./validation-tests --gtest_filter="ValidationTest.Required*"
./integration-tests --gtest_filter="FullPipelineTest.PersonEndToEnd"
```

### Test Coverage

| Suite | Tests | Scope |
|---|---|---|
| `copybook-tests` | 115 | RecordBuffer, RecordBase, CopybookParser, Codegen, JSON/YAML serialization |
| `transport-tests` | 25 | RecordRegistry, gRPC service (in-process), all 7 RPCs, bidirectional streaming |
| `validation-tests` | 28 | All 6 rule types, ValidationEngine, severity levels, text/JSON formatting |
| `integration-tests` | 16 | Full pipeline: parse → create → validate → serialize → round-trip |

---

## CI/CD Pipeline

The project includes a GitHub Actions CI pipeline (`.github/workflows/ci.yml`)
that runs on every push and pull request.

### Build Matrix

| OS | Build Types |
|---|---|
| Ubuntu (latest) | Debug, Release |
| macOS (latest) | Debug, Release |

All 4 test suites run for each matrix combination. Test result XML artifacts
are uploaded for each run.

---

## Project Structure

```
copybook-toolkit/
├── CMakeLists.txt                          Build system
├── USER_GUIDE.md                           This file
├── DEVELOPMENT_OPTIONS.md                  Feature reference & examples
├── DEVELOPMENT_LOG.md                      Sprint history & changelog
├── include/copybook/
│   ├── core/
│   │   ├── field_type.h                    COBOL type enum
│   │   ├── field_descriptor.h              Field metadata struct
│   │   ├── record_buffer.h                 Fixed-length byte buffer
│   │   └── record_base.h                   Base class for all records
│   ├── parser/
│   │   ├── copybook_parser.h               .cpy file parser
│   │   └── codegen.h                       C++ header generator
│   ├── serial/
│   │   ├── json_serializer.h               JSON via nlohmann/json
│   │   └── yaml_serializer.h               YAML serializer
│   ├── transport/
│   │   ├── record_transport.h              RecordRegistry, GenericRecord
│   │   ├── grpc_service.h                  gRPC server implementation
│   │   └── grpc_client.h                   gRPC client wrapper
│   └── validation/
│       ├── validation_rule.h               Rule types & ValidationResult
│       └── validation_engine.h             ValidationEngine
├── src/
│   ├── core/
│   │   ├── record_buffer.cpp
│   │   └── record_base.cpp
│   ├── parser/
│   │   ├── copybook_parser.cpp
│   │   └── codegen.cpp
│   ├── serial/
│   │   ├── json_serializer.cpp
│   │   └── yaml_serializer.cpp
│   ├── transport/
│   │   ├── record_transport.cpp            RecordRegistry + GenericRecord
│   │   ├── grpc_service.cpp                CopybookServiceImpl
│   │   ├── grpc_client.cpp                 CopybookClient
│   │   ├── copybook_service.pb.cc          Generated protobuf
│   │   └── copybook_service.grpc.pb.cc     Generated gRPC stubs
│   └── validation/
│       └── validation_engine.cpp
├── proto/
│   └── copybook_service.proto              gRPC service definition (7 RPCs)
├── examples/
│   ├── copybooks/
│   │   ├── PERSON.cpy                      115 bytes — basic record
│   │   ├── ACCOUNT.cpy                     199 bytes — financial record
│   │   ├── BROKER-CONTROL.cpy              67 bytes — commission rates
│   │   └── TRADE-RECORD.cpy                192 bytes — trading record
│   ├── generated/
│   │   ├── person.h                        Person (hand-ported from Java)
│   │   ├── phone.h                         Phone sub-record
│   │   └── address.h                       Address sub-record
│   └── demo/
│       └── standalone_demo.cpp
├── test/
│   ├── data/PERSON.cpy
│   ├── record_buffer_test.cpp              19 tests
│   ├── record_base_test.cpp                24 tests
│   ├── copybook_parser_test.cpp            30 tests
│   ├── codegen_test.cpp                    22 tests
│   ├── serializer_test.cpp                 20 tests
│   ├── transport_test.cpp                  25 tests (gRPC in-process)
│   ├── validation_test.cpp                 28 tests
│   └── integration_test.cpp               16 tests (end-to-end)
├── tools/
│   ├── test_harness.cpp                    ncurses harness (7 features)
│   ├── grpc_server.cpp                     Standalone gRPC server
│   ├── grpc_client_demo.cpp                gRPC client demo
│   └── web_designer/
│       ├── main.cpp                        HTTP server
│       └── frontend.h                      Embedded HTML5/SVG SPA
└── .github/workflows/
    └── ci.yml                              GitHub Actions CI pipeline
```

---

## Development Options Reference

See [DEVELOPMENT_OPTIONS.md](DEVELOPMENT_OPTIONS.md) for comprehensive coverage of:
- All code generation options and configurations
- JSON/YAML serialization with detailed examples
- Cross-format round-trip patterns
- gRPC transport layer API reference with protobuf message types
- Validation engine API reference with all 6 rule types
- Visual designer REST API reference and keyboard shortcuts
- CI/CD pipeline configuration and test coverage summary
- Sample copybook descriptions
- Development workflow recipes

---

## Roadmap

| Sprint | Status | Deliverable |
|---|---|---|
| **1** | Complete | Core engine: RecordBuffer, RecordBase, Person proof-of-concept (43 tests) |
| **2** | Complete | Copybook parser, C++ code generator, JSON/YAML serialization (115 tests) |
| **Visual Designer** | Complete | HTML5/SVG web-based visual class diagram designer |
| **3** | Complete | gRPC transport layer: RecordRegistry, 7 RPCs, bidirectional streaming (140 tests) |
| **4** | Complete | Validation engine: 6 rule types, severity levels, formatted reporting (168 tests) |
| **5** | Complete | Integration tests, CI/CD pipeline (GitHub Actions), 184 total tests |
