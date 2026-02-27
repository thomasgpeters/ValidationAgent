# Development Options & Feature Reference

This document covers all available features, configuration options,
and development workflows for the COBOL Copybook Toolkit.

---

## Table of Contents

1. [Library Components](#library-components)
2. [Copybook Parser](#copybook-parser)
3. [C++ Code Generator](#c-code-generator)
4. [JSON Serialization (nlohmann/json)](#json-serialization)
5. [YAML Serialization](#yaml-serialization)
6. [Cross-Format Round-Trips](#cross-format-round-trips)
7. [Code Generation Options](#code-generation-options)
8. [Interactive Test Harness](#interactive-test-harness)
9. [Visual Designer (Web Application)](#visual-designer-web-application)
10. [gRPC Transport Layer](#grpc-transport-layer)
11. [Validation Engine](#validation-engine)
12. [Build Targets & Dependencies](#build-targets--dependencies)
13. [Sample Copybooks](#sample-copybooks)
14. [REST API Reference (Web Designer)](#rest-api-reference-web-designer)
15. [CI/CD Pipeline](#cicd-pipeline)
16. [Development Workflow Examples](#development-workflow-examples)
17. [Project Roadmap](#project-roadmap)

---

## Library Components

The toolkit is split into five static libraries:

| Library | Link Target | Purpose |
|---|---|---|
| `libcopybook-core.a` | `copybook-core` | RecordBuffer, RecordBase, FieldDescriptor, FieldType |
| `libcopybook-parser.a` | `copybook-parser` | CopybookParser, Codegen |
| `libcopybook-serial.a` | `copybook-serial` | JsonSerializer (nlohmann/json), YamlSerializer |
| `libcopybook-transport.a` | `copybook-transport` | RecordRegistry, gRPC service/client, protobuf messages |
| `libcopybook-validation.a` | `copybook-validation` | ValidationEngine, validation rules (required, range, pattern, length, enum, custom) |

Dependencies: `copybook-serial` → `copybook-core` + `nlohmann_json`, `copybook-parser` → `copybook-core`, `copybook-transport` → all libs + gRPC + protobuf, `copybook-validation` → `copybook-core`.

---

## Copybook Parser

**Headers:** `include/copybook/parser/copybook_parser.h`

Reads standard COBOL copybook `.cpy` files and produces a `CopybookDefinition` — an in-memory representation of the record layout.

### Supported PIC Clause Forms

| PIC Clause | FieldType | Example |
|---|---|---|
| `PIC X(n)` | CHARACTER | `PIC X(32)` → 32 bytes |
| `PIC X` | CHARACTER | 1 byte |
| `PIC XXX` | CHARACTER | 3 bytes |
| `PIC 9(n)` | ZONED_UNSIGNED | `PIC 9(5)` → 5 bytes |
| `PIC S9(n)` | ZONED_NUMERIC | `PIC S9(9)` → 9 bytes |
| `PIC 9(n)V9(m)` | ZONED_UNSIGNED | `PIC 9(5)V99` → 7 bytes, 2 decimals |
| `PIC 9(n)V99` | ZONED_UNSIGNED | `PIC 9(3)V9(4)` → 7 bytes, 4 decimals |
| `PIC 9(n) COMP-3` | PACKED_DECIMAL | Future support |
| `PIC 9(n) COMP` | BINARY | Future support |
| GROUP (no PIC) | RECORD | Contains child fields |
| `FILLER` | FILLER | Padding, not addressable |

### Usage

```cpp
#include <copybook/parser/copybook_parser.h>

CopybookParser parser;

// Parse from file
auto def = parser.parseFile("ACCOUNT.cpy");

// Parse from string (useful for testing)
auto def2 = parser.parseString(R"(
   01  MY-RECORD.
       05  NAME     PIC X(20).
       05  AGE      PIC 9(3).
)");

// Access parsed fields
std::cout << "Record: " << def.record_name << "\n";
std::cout << "Size: " << def.total_size << " bytes\n";
std::cout << "Fields: " << def.fields.size() << "\n";

for (const auto& field : def.fields) {
    std::cout << field.cpp_name << " offset=" << field.offset
              << " size=" << field.size << "\n";
}
```

### CopybookDefinition Structure

```cpp
struct CopybookDefinition {
    std::string                record_name;     // "ACCOUNT"
    std::string                cpp_class_name;  // "Account"
    int                        total_size;      // total byte length
    std::vector<CopybookField> fields;          // top-level fields
};

struct CopybookField {
    int         level;              // COBOL level (01, 05, 10, ...)
    std::string cobol_name;         // "FIRST-NAME"
    std::string cpp_name;           // "FIRST_NAME"
    std::string pic_clause;         // "X(32)"
    FieldType   type;
    int         size;
    int         decimal_positions;
    int         offset;
    bool        is_filler;
    bool        is_group;
    std::vector<CopybookField> children;
};
```

---

## C++ Code Generator

**Headers:** `include/copybook/parser/codegen.h`

Takes a `CopybookDefinition` and emits a complete C++ header file with a record class.

### Usage

```cpp
#include <copybook/parser/codegen.h>

CopybookParser parser;
Codegen codegen;

auto def = parser.parseFile("PERSON.cpy");

// Generate header string
std::string header = codegen.generateHeader(def);
std::cout << header;

// Or write directly to a file
std::string path = codegen.generateFile(def, "generated/");
// Writes: generated/person.h
```

### Generated Code Features

The generated header includes:
- `#pragma once` header guard
- `#include <copybook/core/record_base.h>`
- Child GROUP classes (emitted before the parent)
- `static constexpr int RECORD_SIZE`
- `static constexpr int` for each field's SIZE and OFFSET
- Default constructor (blank buffer) and raw-data constructor
- `initFields()` with `registerField()` calls
- `initChildren()` with `registerChild()` calls for GROUP items
- Typed accessor methods for GROUP children (e.g., `HomePhone& home_phone()`)

### Code Generation Options

```cpp
struct CodegenOptions {
    std::string ns = "copybook::generated";  // target namespace
    bool emit_offsets     = true;             // static constexpr constants
    bool emit_comments    = true;             // COBOL source in comments
    bool emit_child_accessors = true;         // typed accessor methods
};

// Example: custom namespace, no comments
CodegenOptions opts;
opts.ns = "myapp::records";
opts.emit_comments = false;
std::string code = codegen.generateHeader(def, opts);
```

---

## JSON Serialization

**Headers:** `include/copybook/serial/json_serializer.h`
**Depends on:** [nlohmann/json](https://github.com/nlohmann/json) v3.11.3 (fetched automatically)

### Record to JSON

```cpp
#include <copybook/serial/json_serializer.h>

Person person;
person.setData("FIRST_NAME", "Thomas");
person.setNumeric("AGE", 60);
person.homePhone().setData("NUMBER", "555-123-4567");
person.syncChildren();

// Pretty JSON (default)
std::string json = JsonSerializer::toJson(person);
```

Output:
```json
{
  "ID": "EMP-001",
  "FIRST_NAME": "Thomas",
  "LAST_NAME": "Peters",
  "MIDDLE_INITIAL": "G",
  "AGE": 60,
  "SEX": "M",
  "DATE_OF_BIRTH": "1965-03-15",
  "HOME_PHONE": {
    "NUMBER": "555-123-4567"
  },
  "WORK_PHONE": {
    "NUMBER": "555-987-6543"
  }
}
```

### JSON to Record

```cpp
Person p;
JsonSerializer::fromJson(p, R"({
    "FIRST_NAME": "Alice",
    "AGE": 30,
    "HOME_PHONE": {
        "NUMBER": "999-888-7777"
    }
})");

// Only specified fields are updated; others remain as spaces/zeros
std::cout << p.getData("FIRST_NAME");  // "Alice                           "
std::cout << p.getNumeric("AGE");       // 30
```

### Options

```cpp
// Pretty-printed with trimming (default)
JsonSerializer::toJson(record, true, true);

// Compact JSON (no whitespace)
JsonSerializer::toJson(record, false, true);

// Full field widths preserved (no trimming)
JsonSerializer::toJson(record, true, false);

// Get as nlohmann::json object for programmatic access
auto j = JsonSerializer::toJsonObject(record);
std::string name = j["FIRST_NAME"];

// Load from nlohmann::json object
nlohmann::json input = {{"FIRST_NAME", "Bob"}, {"AGE", 25}};
JsonSerializer::fromJsonObject(record, input);
```

### Numeric Field Handling in JSON

| Field Type | JSON Output | JSON Input |
|---|---|---|
| `PIC 9(3)` → value 60 | `"AGE": 60` | `"AGE": 60` |
| `PIC 9(5)V99` → value 12345 | `"BALANCE": 123.45` | `"BALANCE": 123.45` |
| `PIC S9(9)V99` → value -5000 | `"AMOUNT": -50.00` | `"AMOUNT": -50.0` |

---

## YAML Serialization

**Headers:** `include/copybook/serial/yaml_serializer.h`
**No external dependencies** — lightweight built-in serializer.

### Record to YAML

```cpp
#include <copybook/serial/yaml_serializer.h>

Person person = makeTestPerson();
std::string yaml = YamlSerializer::toYaml(person);
```

Output:
```yaml
---
ID: EMP-001
FIRST_NAME: Thomas
LAST_NAME: Peters
MIDDLE_INITIAL: G
AGE: 60
SEX: M
DATE_OF_BIRTH: 1965-03-15
HOME_PHONE:
  NUMBER: 555-123-4567
WORK_PHONE:
  NUMBER: 555-987-6543
```

### YAML to Record

```cpp
Person p;
YamlSerializer::fromYaml(p, R"(---
FIRST_NAME: Alice
AGE: 30
HOME_PHONE:
  NUMBER: 999-888-7777
)");
```

### Options

```cpp
// With trimming (default)
YamlSerializer::toYaml(record, true);

// Full field widths preserved
YamlSerializer::toYaml(record, false);
```

---

## Cross-Format Round-Trips

Data can flow between any combination of formats:

```
COBOL Buffer ←→ JSON ←→ YAML ←→ COBOL Buffer
```

### Example: JSON to YAML

```cpp
Person p1;
JsonSerializer::fromJson(p1, jsonString);
p1.syncChildren();
std::string yaml = YamlSerializer::toYaml(p1);
```

### Example: YAML to JSON

```cpp
Person p2;
YamlSerializer::fromYaml(p2, yamlString);
p2.syncChildren();
std::string json = JsonSerializer::toJson(p2);
```

### Example: COBOL Buffer to JSON to COBOL Buffer

```cpp
// Receive raw COBOL data (e.g., from gRPC or file)
Person p1(rawBytes, 115);
std::string json = JsonSerializer::toJson(p1);

// ... transmit JSON ...

Person p2;
JsonSerializer::fromJson(p2, json);
p2.syncChildren();
std::string cobolBuffer = p2.getData();  // 115-byte fixed buffer
```

---

## Interactive Test Harness

Run `./test-harness` from the build directory for an ncurses-based interactive UI.

### Menu Options

| # | Feature | Description |
|---|---|---|
| 1 | **Run Unit Tests** | Execute full GoogleTest suite (115 tests) with colorized output |
| 2 | **Run Standalone Demo** | Capture and display the standalone Person record demo |
| 3 | **Interactive Inspector** | Edit Person fields live with real-time hex dump |
| 4 | **Buffer Round-Trip Test** | Serialize/deserialize/verify with per-field PASS/FAIL |
| 5 | **Field Layout Visualizer** | Byte-level offset map with visual byte grid |
| 6 | **JSON/YAML Serialization** | Export Person as JSON and YAML, round-trip verification |
| 7 | **Copybook Parser + Codegen** | Parse all .cpy files, show field layouts, generate C++ header |

### Controls

| Key | Action |
|---|---|
| Up/Down arrows | Navigate menu or scroll |
| Enter | Select/activate |
| PgUp/PgDn | Scroll in output viewer |
| Home/End | Jump to top/bottom |
| q / Esc | Back / quit |

### Inspector Controls

| Key | Action |
|---|---|
| Up/Down | Select field |
| Enter | Edit field value |
| c | Clear selected field |
| r | Reset all fields to defaults |
| q | Return to main menu |

---

## Visual Designer (Web Application)

**Source:** `tools/web_designer/main.cpp`, `tools/web_designer/frontend.h`
**Build target:** `web-designer`
**No external dependencies** beyond the toolkit's existing libraries (no Wt, no Boost).

A self-contained C++ web application that serves an HTML5/SVG visual class diagram
designer for COBOL copybook definitions. The server is a lightweight POSIX socket
HTTP server with the entire frontend embedded as a single-page application.

### Starting the Designer

```bash
cd copybook-toolkit/build

# Default: port 8080, loads from examples/copybooks/
./web-designer

# Custom port and copybook directory
./web-designer 3000 /path/to/copybooks/
```

Then open `http://localhost:8080` in any browser.

### Diagram Features

The designer renders each copybook as a UML-style class node on an SVG canvas:

- **Parent nodes** (dark blue header) represent top-level records: Person, Account, TradeRecord, BrokerControl
- **Child nodes** (teal header) represent GROUP sub-records: HomePhone, WorkPhone, CustomerAddress, BrokerInfo, CustomerInfo
- **Bezier curve arrows** connect parent records to their GROUP children
- **Drag-and-drop** any node to rearrange the diagram layout
- **Click** a node to select it (highlighted red border) — required for toolbar actions
- **Double-click** a node to open the field detail dialog

### Field Detail Dialog (Double-Click)

Opens a modal with a complete field attribute table:

| Column | Description |
|---|---|
| **Field Name** | C++ field name (e.g., `FIRST_NAME`) |
| **PIC** | COBOL PIC clause (e.g., `X(32)`, `S9(9)V99`) or `GROUP` |
| **Offset** | Byte offset within the record |
| **Size** | Field size in bytes |
| **Type** | Color-coded badge: CHARACTER, ZONED_NUMERIC, ZONED_UNSIGNED, PACKED_DECIMAL, BINARY, RECORD, FILLER |
| **Decimals** | Number of decimal positions (for V-clause fields) |

GROUP items are highlighted and their children are shown indented below.

### Toolbar Actions

All actions operate on the currently selected node's parent copybook:

| Button | Action |
|---|---|
| **Reload Copybooks** | Re-scan the copybook directory and refresh the diagram |
| **Import Copybook** | Opens a dialog to paste `.cpy` source text; parses and adds to the diagram |
| **Generate C++** | Generates a complete C++ header via `Codegen` and displays in a code viewer |
| **Export JSON** | Exports the copybook schema (field names, types, offsets, sizes) as formatted JSON |
| **Export YAML** | Exports the copybook schema as YAML |

Each code/export modal includes a **Copy to Clipboard** button.

### Keyboard Shortcuts

| Key | Action |
|---|---|
| Escape | Close any open modal dialog |
| Click empty canvas | Deselect the current node |

### Architecture

```
Browser (HTML5/SVG SPA)
    │
    │  HTTP (localhost)
    │
C++ HTTP Server (POSIX sockets, thread-per-connection)
    │
    ├── GET /                → Serve embedded HTML frontend
    ├── GET /api/copybooks   → Return all parsed definitions as JSON
    ├── POST /api/generate   → Generate C++ header (Codegen)
    ├── POST /api/export/json→ Export schema as JSON
    ├── POST /api/export/yaml→ Export schema as YAML
    └── POST /api/parse      → Parse .cpy source, add to diagram
```

### Example: Using the Visual Designer

```bash
# 1. Build
cd copybook-toolkit/build && cmake --build . --target web-designer

# 2. Start
./web-designer 8080 ../examples/copybooks

# 3. Open http://localhost:8080

# 4. Click "Person" node → click "Generate C++" → view/copy generated header
# 5. Double-click "Account" node → inspect all 11 fields with offsets and types
# 6. Click "Import Copybook" → paste your own .cpy → see it appear in the diagram
# 7. Select new copybook → Export JSON / Export YAML to see the schema
```

---

## gRPC Transport Layer

**Proto:** `proto/copybook_service.proto`
**Headers:** `include/copybook/transport/record_transport.h`, `grpc_service.h`, `grpc_client.h`
**Library:** `libcopybook-transport.a`
**Depends on:** gRPC 1.51+, Protobuf 3.21+, all copybook libraries

Provides remote access to copybook records over gRPC. Supports sending/receiving
raw COBOL buffers, field extraction/packing, schema introspection, format
conversion (JSON/YAML), and bidirectional streaming.

### Starting the gRPC Server

```bash
cd copybook-toolkit/build

# Default: port 50051, loads from examples/copybooks/
./grpc-server

# Custom port and directory
./grpc-server 50051 /path/to/copybooks/
```

### Service RPCs

| RPC | Description |
|---|---|
| `SendRecord` | Send a raw COBOL buffer to the server for storage |
| `GetFields` | Extract named field values from a raw buffer |
| `SetFields` | Pack named field values into a raw COBOL buffer |
| `GetSchema` | Get the schema for a specific copybook record |
| `ListSchemas` | List all known copybook schemas |
| `Convert` | Convert between raw buffer and JSON/YAML formats |
| `StreamRecords` | Bidirectional streaming: send/receive records in batch |

### RecordRegistry

The `RecordRegistry` class manages copybook definitions and record factories:

```cpp
#include <copybook/transport/record_transport.h>

using namespace copybook::transport;

RecordRegistry registry;

// Load all .cpy files from a directory
registry.loadDirectory("examples/copybooks");

// Or register individual copybooks
CopybookParser parser;
auto def = parser.parseFile("ORDER.cpy");
registry.registerCopybook(def);

// Create records from the registry
auto record = registry.createBlankRecord("PERSON");
record->setData("FIRST_NAME", "Alice");

auto record2 = registry.createRecord("PERSON", rawBuffer, 115);

// Extract/apply fields
auto fields = RecordRegistry::extractFields(*record);
RecordRegistry::applyFields(*record2, fields);
```

### CopybookClient (C++ Client)

```cpp
#include <copybook/transport/grpc_client.h>

using namespace copybook::transport;

CopybookClient client("localhost:50051");

// List available schemas
auto schemas = client.listSchemas();
for (const auto& s : schemas) {
    std::cout << s.record_name() << " (" << s.total_size() << " bytes)\n";
}

// Get schema details
auto schema = client.getSchema("PERSON");

// Create a record from field values
std::vector<std::pair<std::string, std::string>> fields = {
    {"ID", "EMP-001"},
    {"FIRST_NAME", "Thomas"},
    {"AGE", "060"}
};
std::string buffer = client.setFields("PERSON", fields, 115);

// Send a record buffer to the server
auto [ok, msg] = client.sendRecord("PERSON", buffer.c_str(), buffer.size());

// Extract fields from a buffer
auto extracted = client.getFields("PERSON", buffer.c_str(), buffer.size());

// Convert buffer to JSON or YAML
std::string json = client.toJson("PERSON", buffer.c_str(), buffer.size());
std::string yaml = client.toYaml("PERSON", buffer.c_str(), buffer.size());

// Convert JSON/YAML back to a buffer
std::string roundTrip = client.fromJson("PERSON", json);

// Stream multiple records
std::vector<std::pair<std::string, std::string>> batch = {
    {"PERSON", buffer1},
    {"PERSON", buffer2},
};
auto results = client.streamRecords(batch);
```

### Protobuf Message Types

Key message types defined in `copybook_service.proto`:

| Message | Purpose |
|---|---|
| `RecordPayload` | Raw COBOL buffer with record name and size |
| `RecordMessage` | Named field-value pairs for a record |
| `FieldValue` | Single field name/value pair |
| `RecordSchema` | Full schema with field descriptors |
| `FieldDescriptor` | Field metadata: name, PIC, offset, size, type |
| `ConvertRequest` | Convert between buffer and JSON/YAML |

### Custom Record Factories

Register custom RecordBase subclasses for type-safe access:

```cpp
#include "generated/person.h"  // Generated from PERSON.cpy

RecordRegistry registry;

// Register with a custom factory that creates typed Person records
CopybookParser parser;
auto def = parser.parseFile("PERSON.cpy");
registry.registerCopybook(def, [](const char* raw, size_t size) {
    return std::make_shared<copybook::generated::Person>(raw, size);
});
```

---

## Validation Engine

**Headers:** `include/copybook/validation/validation_rule.h`, `include/copybook/validation/validation_engine.h`
**Source:** `src/validation/validation_engine.cpp`
**Library:** `libcopybook-validation.a`
**Depends on:** `copybook-core` only (no network dependencies)

A rule-based validation framework for COBOL copybook records. Supports field-level
validators (required, range, pattern, length, enum), record-level cross-field
validation via custom lambdas, configurable severity levels, and formatted
error reporting in both text and JSON formats.

### Severity Levels

| Level | Effect |
|---|---|
| `ERROR` | Causes `result.valid` to be `false` |
| `WARNING` | Does not fail validation; tracked separately |
| `INFO` | Informational; tracked separately |

### Built-in Validation Rules

| Rule | Class | Description |
|---|---|---|
| **Required** | `RequiredRule` | Field must not be empty (all spaces) |
| **Range** | `RangeRule` | Numeric field must be within `[min, max]` |
| **Pattern** | `PatternRule` | Field value must match a regex pattern |
| **Length** | `LengthRule` | Trimmed field length must be within `[min, max]` |
| **Enum** | `EnumRule` | Field value must be one of the allowed values |
| **Custom** | `CustomRule` | Lambda-based rule for arbitrary/cross-field logic |

All built-in rules skip empty (all-space) fields — use `RequiredRule` to enforce non-empty.

### Usage: Field-Level Validation

```cpp
#include <copybook/validation/validation_rule.h>
#include <copybook/validation/validation_engine.h>

using namespace copybook::validation;

ValidationEngine engine;

// Required fields
engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
engine.addRule("PERSON", std::make_shared<RequiredRule>("FIRST_NAME"));

// Range check (numeric PIC 9(3))
engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));

// Enumerated values
engine.addRule("PERSON", std::make_shared<EnumRule>("SEX",
    std::vector<std::string>{"M", "F", "X"}));

// Date pattern (regex)
engine.addRule("PERSON", std::make_shared<PatternRule>(
    "DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD"));

// Length constraint
engine.addRule("PERSON", std::make_shared<LengthRule>("FIRST_NAME", 2, 32));

// Validate a record
auto result = engine.validate("PERSON", *record);
if (!result.valid) {
    std::cerr << ValidationEngine::formatResult(result);
}
```

### Usage: Cross-Field Validation

```cpp
engine.addRule("PERSON", std::make_shared<CustomRule>("active-needs-id",
    [](const RecordBase& rec, ValidationResult& res) {
        std::string status = rec.getData("STATUS");
        auto end = status.find_last_not_of(' ');
        std::string s = (end != std::string::npos) ? status.substr(0, end + 1) : "";
        if (s == "A") {
            std::string id = rec.getData("ID");
            if (id.find_first_not_of(' ') == std::string::npos) {
                res.addError("ID", "active-needs-id",
                    "Active records must have an ID");
            }
        }
    }));
```

### Usage: Global Rules

Global rules apply to every record type:

```cpp
engine.addGlobalRule(std::make_shared<RequiredRule>("ID"));

// Applies to PERSON, ACCOUNT, etc.
auto result1 = engine.validate("PERSON", *personRecord);
auto result2 = engine.validate("ACCOUNT", *accountRecord);
```

### Usage: Warning / Info Severity

```cpp
// Warning: doesn't fail validation
engine.addRule("PERSON", std::make_shared<RequiredRule>("AGE", Severity::WARNING));

// Info: recommendation only
engine.addRule("PERSON", std::make_shared<RequiredRule>("DATE_OF_BIRTH", Severity::INFO));

auto result = engine.validate("PERSON", *record);
// result.valid may still be true even with warnings/info
std::cout << "Errors: " << result.errorCount() << "\n";
std::cout << "Warnings: " << result.warningCount() << "\n";
std::cout << "Info: " << result.infoCount() << "\n";
```

### Result Formatting

```cpp
// Text format (human-readable)
std::string text = ValidationEngine::formatResult(result);
// Output:
// Validation FAILED (2 error(s), 1 warning(s))
//   [ERROR] AGE: AGE value 200 outside range [0, 150] (rule: range)
//   [ERROR] SEX: SEX value "Z" not in allowed values: ["M", "F", "X"] (rule: enum)
//   [WARNING] ID: ID is required but empty (rule: required)

// JSON format (machine-readable)
std::string json = ValidationEngine::formatResultJson(result);
// Output:
// {
//   "valid": false,
//   "error_count": 2,
//   "warning_count": 1,
//   "errors": [
//     { "field": "AGE", "rule": "range", "severity": "ERROR", "message": "..." },
//     ...
//   ]
// }
```

### Engine Management

```cpp
// Count rules for a record type (includes global rules)
size_t count = engine.ruleCount("PERSON");

// Get all rules for a record type
auto rules = engine.getRules("PERSON");

// Clear rules for a specific record type (global rules remain)
engine.clearRules("PERSON");

// Clear everything
engine.clearAll();
```

---

## Build Targets & Dependencies

```
copybook-core              (no dependencies)
    │
    ├── copybook-parser         (depends on: copybook-core)
    │
    ├── copybook-serial         (depends on: copybook-core, nlohmann_json)
    │
    ├── copybook-transport      (depends on: all libs, gRPC, protobuf)
    │
    ├── copybook-validation     (depends on: copybook-core)
    │
    ├── standalone-demo         (depends on: copybook-core)
    │
    ├── copybook-tests          (depends on: core + parser + serial, GoogleTest)
    │
    ├── transport-tests         (depends on: copybook-transport, GoogleTest)
    │
    ├── validation-tests        (depends on: copybook-validation + transport, GoogleTest)
    │
    ├── integration-tests       (depends on: copybook-validation + transport, GoogleTest)
    │
    ├── test-harness            (depends on: core + parser + serial, ncurses)
    │
    ├── web-designer            (depends on: core + parser + serial, nlohmann_json)
    │
    ├── grpc-server             (depends on: copybook-transport)
    │
    └── grpc-client-demo        (depends on: copybook-transport)
```

### Build Targets Summary

| Target | Type | Description |
|---|---|---|
| `copybook-core` | Static library | RecordBuffer, RecordBase, FieldDescriptor, FieldType |
| `copybook-parser` | Static library | CopybookParser, Codegen |
| `copybook-serial` | Static library | JsonSerializer, YamlSerializer |
| `copybook-transport` | Static library | RecordRegistry, gRPC service, gRPC client, protobuf messages |
| `copybook-validation` | Static library | ValidationEngine, validation rules |
| `standalone-demo` | Executable | Minimal Person record demonstration |
| `copybook-tests` | Executable | 115 unit tests (GoogleTest) |
| `transport-tests` | Executable | 25 gRPC transport tests (GoogleTest) |
| `validation-tests` | Executable | 28 validation engine tests (GoogleTest) |
| `integration-tests` | Executable | 16 end-to-end integration tests (GoogleTest) |
| `test-harness` | Executable | ncurses interactive test/demo UI (7 features) |
| `web-designer` | Executable | HTML5/SVG web-based visual class diagram designer |
| `grpc-server` | Executable | Standalone gRPC server for copybook records |
| `grpc-client-demo` | Executable | Client demo exercising all gRPC RPCs |

### External Dependencies

| Library | Version | Source | Purpose |
|---|---|---|---|
| [nlohmann/json](https://github.com/nlohmann/json) | 3.11.3 | CMake FetchContent | JSON parsing and generation |
| [GoogleTest](https://github.com/google/googletest) | 1.14.0 | CMake FetchContent | Unit testing framework |
| [gRPC](https://grpc.io/) | 1.51+ | System (pkg-config) | Remote procedure calls |
| [Protobuf](https://protobuf.dev/) | 3.21+ | System (pkg-config) | Protocol buffer serialization |
| ncurses | system | find_package | Interactive test harness UI |
| pthread | system | direct link | Web designer threading |

---

## Sample Copybooks

Four copybooks are included for testing and demonstration:

### PERSON.cpy (115 bytes)
Simple record with CHARACTER, ZONED_UNSIGNED, and two GROUP items.

### ACCOUNT.cpy (199 bytes)
Financial record with FILLER, signed numerics (PIC S9(9)V99), unsigned decimals, and a nested CUSTOMER-ADDRESS group with 5 fields.

### BROKER-CONTROL.cpy (67 bytes)
Compact record with a commission rate field (`PIC 9(3)V9(4)` — 4 decimal places).

### TRADE-RECORD.cpy (192 bytes)
Complex trading record with two GROUP items (BROKER-INFO, CUSTOMER-INFO), multiple decimal fields (PRICE, TOTAL-AMOUNT, COMMISSION), and signed amounts.

---

## CI/CD Pipeline

**Config:** `.github/workflows/ci.yml`

The project includes a GitHub Actions CI pipeline that builds and tests across
multiple configurations on every push and pull request.

### Build Matrix

| OS | Build Types |
|---|---|
| Ubuntu (latest) | Debug, Release |
| macOS (latest) | Debug, Release |

### Pipeline Steps

1. **Install dependencies** — `brew install` (macOS) or `apt-get install` (Ubuntu) for gRPC, protobuf, ncurses, pkg-config
2. **Configure** — CMake with selected build type
3. **Build** — Full build including all libraries and executables
4. **Run core tests** — 115 unit tests (parser, serializer, core)
5. **Run transport tests** — 25 gRPC transport tests
6. **Run validation tests** — 28 validation engine tests
7. **Run integration tests** — 16 end-to-end pipeline tests
8. **Upload artifacts** — Test result XML files for each matrix entry

### Running Tests Locally

```bash
cd copybook-toolkit/build

# All tests via CTest
ctest --output-on-failure

# Individual test suites
./copybook-tests          # 115 core/parser/serial tests
./transport-tests         # 25 gRPC transport tests
./validation-tests        # 28 validation rule/engine tests
./integration-tests       # 16 end-to-end integration tests

# With XML output (same as CI)
./copybook-tests --gtest_output=xml:test-results.xml
```

### Test Coverage Summary

| Suite | Tests | Coverage |
|---|---|---|
| `copybook-tests` | 115 | RecordBuffer, RecordBase, CopybookParser, Codegen, JSON/YAML serialization |
| `transport-tests` | 25 | RecordRegistry, gRPC service (in-process), all 7 RPCs, streaming |
| `validation-tests` | 28 | All 6 rule types, ValidationEngine, severity, formatting |
| `integration-tests` | 16 | Full pipeline: parse → create → validate → serialize → round-trip |
| **Total** | **184** | |

---

## Development Workflow Examples

### Add a New Copybook Record

1. Create the `.cpy` file:
```
       01  ORDER.
           05  ORDER-ID        PIC X(16).
           05  CUSTOMER-ID     PIC X(12).
           05  TOTAL           PIC S9(9)V99.
           05  STATUS          PIC X(1).
```

2. Parse and generate:
```cpp
CopybookParser parser;
Codegen codegen;

auto def = parser.parseFile("ORDER.cpy");
codegen.generateFile(def, "generated/");
// Creates: generated/order.h
```

3. Use the generated class:
```cpp
#include "generated/order.h"
using namespace copybook::generated;

Order order;
order.setData("ORDER_ID", "ORD-2025-0001");
order.setData("CUSTOMER_ID", "CUST-12345");
order.setNumeric("TOTAL", 1234567);  // 12345.67 (V99)
order.setData("STATUS", "A");

std::string json = JsonSerializer::toJson(order);
```

### Convert Legacy COBOL Data to JSON

```cpp
// Read raw COBOL record from file or socket
char rawData[199];
file.read(rawData, 199);

Account acct(rawData, 199);
std::string json = JsonSerializer::toJson(acct);
// {
//   "COMPANY_NO": 12345,
//   "ACCOUNT_NO": "ACC-001",
//   "BALANCE": -1234.56,
//   ...
// }
```

### Load Configuration from YAML

```cpp
BrokerControl broker;
YamlSerializer::fromYaml(broker, R"(---
BROKER_ID: BRK-001
BROKER_NAME: Smith & Associates
COMMISSION_RATE: 3.5000
OFFICE_CODE: NYC1
)");

// Now serialize to COBOL buffer for transmission
broker.syncChildren();
std::string cobolBuffer = broker.getData();
```

### Run All Tests

```bash
cd copybook-toolkit/build
cmake --build . && ctest --output-on-failure

# Or run individual suites:
./copybook-tests          # 115 core/parser/serial tests
./transport-tests         # 25 gRPC transport tests
./validation-tests        # 28 validation rule/engine tests
./integration-tests       # 16 end-to-end integration tests
```

Or use the interactive harness:
```bash
./test-harness
# Select "Run Unit Tests" from the menu
```

### Use the Visual Designer

```bash
cd copybook-toolkit/build
cmake --build . --target web-designer && ./web-designer
# Open http://localhost:8080
```

### Validate Records Before Sending

```cpp
#include <copybook/validation/validation_engine.h>
#include <copybook/validation/validation_rule.h>
#include <copybook/transport/record_transport.h>

using namespace copybook::validation;
using namespace copybook::transport;

// Set up validation rules
ValidationEngine engine;
engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
engine.addRule("PERSON", std::make_shared<EnumRule>("SEX",
    std::vector<std::string>{"M", "F", "X"}));
engine.addRule("PERSON", std::make_shared<PatternRule>(
    "DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD"));

// Validate before processing
auto result = engine.validate("PERSON", *record);
if (!result.valid) {
    // Text report for logs
    std::cerr << ValidationEngine::formatResult(result);
    // JSON report for API responses
    std::string jsonReport = ValidationEngine::formatResultJson(result);
    return;
}

// Proceed with serialization/transport
std::string json = JsonSerializer::toJson(*record);
```

### Send Records Over gRPC

```bash
# Terminal 1: Start the server
cd copybook-toolkit/build
./grpc-server 50051 ../examples/copybooks

# Terminal 2: Run the client demo
./grpc-client-demo localhost:50051
```

```cpp
// Programmatic client usage
#include <copybook/transport/grpc_client.h>

using namespace copybook::transport;

CopybookClient client("localhost:50051");

// Pack fields into a COBOL buffer
std::string buffer = client.setFields("PERSON", {
    {"ID", "EMP-001"},
    {"FIRST_NAME", "Thomas"},
    {"AGE", "060"}
}, 115);

// Send the buffer
auto [ok, msg] = client.sendRecord("PERSON", buffer.c_str(), buffer.size());

// Convert to JSON for downstream systems
std::string json = client.toJson("PERSON", buffer.c_str(), buffer.size());
```

---

## REST API Reference (Web Designer)

The web designer serves a REST API on the same port as the frontend.
All request/response bodies use JSON (`Content-Type: application/json`).

### GET /api/copybooks

Returns all loaded copybook definitions.

**Response:**
```json
{
  "copybooks": [
    {
      "name": "ACCOUNT",
      "class_name": "Account",
      "total_size": 199,
      "fields": [
        {
          "name": "COMPANY_NO",
          "cobol_name": "COMPANY-NO",
          "pic_clause": "9(5)",
          "offset": 0,
          "size": 5,
          "type": "ZONED_UNSIGNED",
          "decimal_positions": 0,
          "is_group": false,
          "is_filler": false,
          "children": []
        }
      ]
    }
  ]
}
```

### POST /api/generate

Generate a C++ header for a copybook.

**Request:** `{ "index": 0 }`
**Response:** `{ "code": "#pragma once\n// Auto-generated..." }`

### POST /api/export/json

Export a copybook schema as formatted JSON.

**Request:** `{ "index": 0 }`
**Response:** `{ "output": "{ \"record\": \"ACCOUNT\", ... }" }`

### POST /api/export/yaml

Export a copybook schema as YAML.

**Request:** `{ "index": 0 }`
**Response:**
```json
{
  "output": "# Copybook Schema: ACCOUNT\nrecord: ACCOUNT\nclass: Account\ntotal_size: 199\nfields:\n  - name: COMPANY_NO\n    ..."
}
```

### POST /api/parse

Parse new copybook source and add it to the in-memory collection.

**Request:** `{ "source": "       01  MY-RECORD.\n           05  NAME   PIC X(20)." }`
**Response:** Returns the parsed `CopybookDefinition` as JSON (same schema as `/api/copybooks` entries).

### Error Responses

All errors return JSON: `{ "error": "description" }` with an appropriate HTTP status code (400 or 500).

---

## Project Roadmap

### Completed

| Phase | Deliverables |
|---|---|
| **Sprint 1: Core Engine** | RecordBuffer, RecordBase, FieldDescriptor, FieldType, Person/Phone/Address proof-of-concept, 43 unit tests |
| **Sprint 2: Parser + Serialization** | CopybookParser (.cpy reader), Codegen (C++ header generator), JsonSerializer (nlohmann/json), YamlSerializer, 4 sample copybooks, 115 total tests |
| **Visual Designer** | Web-based HTML5/SVG class diagram designer with REST API, drag-and-drop nodes, field detail dialogs, code generation, JSON/YAML export, copybook import |
| **Sprint 3: gRPC Transport** | Protocol Buffer service definition (7 RPCs), RecordRegistry, CopybookServiceImpl, CopybookClient, gRPC server/client executables, bidirectional streaming, JSON/YAML conversion over gRPC, 25 transport tests (140 total) |
| **Sprint 4: Validation Engine** | 6 built-in rule types (Required, Range, Pattern, Length, Enum, Custom), ValidationEngine with scoped and global rules, 3 severity levels (ERROR/WARNING/INFO), text and JSON result formatting, 28 validation tests (168 total) |
| **Sprint 5: Integration & CI** | 16 end-to-end integration tests (full pipeline: parse → create → validate → serialize → round-trip), GitHub Actions CI/CD pipeline (GCC + Clang, Debug + Release matrix), 184 total tests |
| **Documentation** | USER_GUIDE.md (21 sections), DEVELOPMENT_OPTIONS.md (this file, 17 sections) |
| **Tools** | ncurses test harness (7 features), web-based visual designer, gRPC server, gRPC client demo |
