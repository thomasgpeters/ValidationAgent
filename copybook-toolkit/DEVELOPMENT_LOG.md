# Development Log

This document chronicles the sprint-by-sprint development of the COBOL
Copybook Toolkit, from initial core engine through to CI/CD integration.

---

## Sprint 1: Core Engine

**Goal:** Establish the foundational C++ record buffer system to replace
Java's `DataObjectBase` / `PassListBase` reflection-driven approach.

### Deliverables

- **RecordBuffer** (`include/copybook/core/record_buffer.h`) — Fixed-length
  character buffer with bounds-checked `read()`, `write()`, `slice()`, and
  `merge()` operations. Replaces Java's `char[] data_elements`.
- **RecordBase** (`include/copybook/core/record_base.h`) — Base class for
  all copybook record mappings. Uses `unordered_map<string, FieldDescriptor>`
  instead of Java reflection. Provides `setData()`, `getData()`,
  `setNumeric()`, `getNumeric()`, `syncChildren()`, `refreshChildren()`.
- **FieldDescriptor** / **FieldType** — Metadata structures and COBOL type
  enum (CHARACTER, ZONED_NUMERIC, ZONED_UNSIGNED, PACKED_DECIMAL, BINARY,
  RECORD, FILLER).
- **Person proof-of-concept** — Hand-ported Person, Phone, and Address
  records from the Java codebase to validate API compatibility.
- **43 unit tests** — RecordBuffer (19 tests), RecordBase (24 tests).

### Key Decisions

- C++17 standard, CMake 3.16+ build system
- `unordered_map` field registry replaces Java reflection — O(1) lookup,
  compile-time type safety, descriptive error messages
- Explicit `syncChildren()` / `refreshChildren()` fixes the Java version's
  buffer-sync bug where child GROUP mutations were lost during serialization

---

## Sprint 2: Parser + Serialization

**Goal:** Parse COBOL `.cpy` files automatically and add JSON/YAML
serialization for record data.

### Deliverables

- **CopybookParser** (`include/copybook/parser/copybook_parser.h`) — Reads
  standard COBOL copybook files and produces `CopybookDefinition` with field
  names, offsets, sizes, types, and GROUP hierarchies computed automatically.
  Supports PIC X(n), 9(n), S9(n), 9(n)V9(m), COMP, COMP-3, FILLER, and
  GROUP items.
- **Codegen** (`include/copybook/parser/codegen.h`) — Takes a
  `CopybookDefinition` and emits a complete C++ header with: `RECORD_SIZE`
  constants, field SIZE/OFFSET constants, constructors, `registerField()`
  calls, child GROUP classes, typed accessors, and `syncChildren()` support.
  Configurable namespace, comments, and accessor generation.
- **JsonSerializer** (`include/copybook/serial/json_serializer.h`) — Record
  to/from JSON using nlohmann/json v3.11.3. Numeric fields emit as JSON
  numbers, GROUP items as nested objects, trailing spaces trimmed by default.
  Supports pretty/compact output and `toJsonObject()`/`fromJsonObject()` for
  programmatic access.
- **YamlSerializer** (`include/copybook/serial/yaml_serializer.h`) —
  Lightweight built-in YAML serializer (no external dependency). Supports
  nested GROUP items and trimming options.
- **4 sample copybooks** — PERSON.cpy (115B), ACCOUNT.cpy (199B),
  BROKER-CONTROL.cpy (67B), TRADE-RECORD.cpy (192B).
- **115 total tests** — Parser (30), Codegen (22), Serializer (20),
  plus original 43.

### Key Decisions

- nlohmann/json v3.11.3 fetched via CMake FetchContent (no manual install)
- Custom YAML serializer to avoid heavy yaml-cpp dependency
- Cross-format round-trips: Buffer <-> JSON <-> YAML <-> Buffer

---

## Visual Designer

**Goal:** Provide a visual tool for exploring and working with copybook
definitions without writing code.

### Deliverables

- **Web Designer** (`tools/web_designer/`) — Self-contained C++ HTTP server
  using POSIX sockets with the entire HTML5/SVG/CSS/JS frontend embedded as
  a C++ raw string literal. No external web framework needed (Wt was
  evaluated but rejected due to installation constraints).
- **SVG canvas** — UML-style class diagram nodes for each copybook record,
  with dark blue headers for parent records and teal for GROUP children.
  Bezier curve arrows connect parent-child relationships.
- **Interactivity** — Drag-and-drop node positioning, click to select,
  double-click for field detail dialogs with offset/size/type tables.
- **Toolbar actions** — Reload Copybooks, Import Copybook (paste .cpy
  source), Generate C++, Export JSON, Export YAML. All with copy-to-clipboard.
- **REST API** — 5 endpoints: `GET /`, `GET /api/copybooks`,
  `POST /api/generate`, `POST /api/export/json`, `POST /api/export/yaml`,
  `POST /api/parse`.

### Key Decisions

- Self-contained HTTP server instead of Wt library (apt install not available
  in all environments)
- Frontend embedded as C++ raw string literal in `frontend.h` for zero
  deployment complexity
- Thread-per-connection model for simplicity

---

## Sprint 3: gRPC Transport Layer

**Goal:** Replace the Java socket-based server with a modern gRPC transport
layer for remote copybook record access.

### Deliverables

- **Protocol Buffer definition** (`proto/copybook_service.proto`) — 7 RPCs:
  `SendRecord`, `GetFields`, `SetFields`, `GetSchema`, `ListSchemas`,
  `Convert`, `StreamRecords` (bidirectional streaming).
- **RecordRegistry** (`include/copybook/transport/record_transport.h`) —
  Manages copybook schemas, creates records dynamically via `GenericRecord`
  (no code generation needed), supports custom record factories.
- **CopybookServiceImpl** (`include/copybook/transport/grpc_service.h`) —
  Full gRPC server implementation with error handling, format conversion
  (JSON/YAML), and streaming support.
- **CopybookClient** (`include/copybook/transport/grpc_client.h`) — Typed
  C++ client wrapper for all 7 RPCs with error logging.
- **Server/Client executables** — `grpc-server` and `grpc-client-demo`.
- **25 transport tests** — RecordRegistry (9), gRPC integration (16,
  in-process server with random port). **140 total tests.**

### Key Decisions

- pkg-config for finding system gRPC/protobuf (vs bundling)
- `GenericRecord` class for runtime record creation from parsed definitions
- In-process gRPC testing with random port allocation

### Bugs Fixed

- **Child GROUP field offset bug** — Parser stores absolute offsets; child
  `GenericRecord` needs relative offsets. Fixed by subtracting parent group's
  offset: `c.offset -= f.offset`.
- **Missing try/catch in Convert handler** — Serialization errors caused
  silent gRPC INTERNAL errors. Added proper error propagation.

---

## Sprint 4: Validation Engine

**Goal:** Build a rule-based validation framework for COBOL copybook
records, replacing Java's `ValidateData` approach.

### Deliverables

- **Validation rule types** (`include/copybook/validation/validation_rule.h`):
  - `RequiredRule` — Field must not be empty (all spaces)
  - `RangeRule` — Numeric field within [min, max]
  - `PatternRule` — Regex match on trimmed field value
  - `LengthRule` — Trimmed length within [min, max]
  - `EnumRule` — Value must be in allowed set
  - `CustomRule` — Lambda-based rule for arbitrary cross-field logic
- **Severity levels** — `ERROR` (fails validation), `WARNING`, `INFO`.
  Only ERROR causes `result.valid` to be false.
- **ValidationEngine** (`include/copybook/validation/validation_engine.h`) —
  Manages per-record-type and global rules. Methods: `addRule()`,
  `addGlobalRule()`, `validate()`, `ruleCount()`, `clearRules()`.
- **Result formatting** — `formatResult()` for human-readable text,
  `formatResultJson()` for machine-readable JSON with error counts.
- **28 validation tests** — All 6 rule types, engine features, severity
  handling, cross-field validation, text and JSON formatting.
  **168 total tests.**

### Key Decisions

- Validation library depends only on `copybook-core` (no network dependencies)
- All built-in rules skip empty fields — use `RequiredRule` for non-empty
- `CustomRule` with `std::function` for maximum flexibility in cross-field
  validation without needing new rule subclasses

---

## Sprint 5: Integration & CI

**Goal:** End-to-end integration tests covering the full pipeline and
a CI/CD pipeline for automated builds and testing.

### Deliverables

- **16 integration tests** (`test/integration_test.cpp`) covering:
  - Full pipeline: parse → create → validate → serialize → round-trip
    (PERSON, ACCOUNT, BROKER-CONTROL)
  - Validation failure report pipeline (text + JSON)
  - Batch validation (5 records, mixed pass/fail)
  - Global rules across record types
  - JSON round-trip then validate
  - YAML round-trip then validate
  - Raw buffer round-trip (simulated mainframe I/O)
  - Multi-schema registry
  - Extract fields → apply to different record
  - Mixed severity end-to-end (ERROR + WARNING + INFO)
  - Copybook file I/O (write .cpy to /tmp, load, validate)
  - Parser error handling
  - Large record stress test (50 fields, 1000 bytes)
  - Cross-field validation integration
- **GitHub Actions CI pipeline** (`.github/workflows/ci.yml`) —
  2x2 build matrix: GCC/Clang x Debug/Release. Runs all 4 test suites,
  uploads test result XML artifacts.
- **184 total tests** across 4 suites.

### Key Decisions

- Integration tests use `RecordRegistry` and `GenericRecord` (no generated
  code) to test the full dynamic pipeline
- CI matrix covers both GCC and Clang to catch compiler-specific issues
- Test results uploaded as artifacts for post-run analysis

---

## Test Summary

| Suite | Tests | File | Scope |
|---|---|---|---|
| `copybook-tests` | 115 | `test/record_buffer_test.cpp` + 4 others | Core, parser, serialization |
| `transport-tests` | 25 | `test/transport_test.cpp` | RecordRegistry, gRPC (in-process) |
| `validation-tests` | 28 | `test/validation_test.cpp` | All rule types, engine, formatting |
| `integration-tests` | 16 | `test/integration_test.cpp` | Full pipeline end-to-end |
| **Total** | **184** | | |

---

## Libraries

| Library | Link Target | Source Files | Dependencies |
|---|---|---|---|
| `libcopybook-core.a` | `copybook-core` | `record_buffer.cpp`, `record_base.cpp` | C++17 stdlib |
| `libcopybook-parser.a` | `copybook-parser` | `copybook_parser.cpp`, `codegen.cpp` | copybook-core |
| `libcopybook-serial.a` | `copybook-serial` | `json_serializer.cpp`, `yaml_serializer.cpp` | copybook-core, nlohmann/json |
| `libcopybook-transport.a` | `copybook-transport` | `record_transport.cpp`, `grpc_service.cpp`, `grpc_client.cpp`, protobuf | all libs, gRPC, protobuf |
| `libcopybook-validation.a` | `copybook-validation` | `validation_engine.cpp` | copybook-core |
