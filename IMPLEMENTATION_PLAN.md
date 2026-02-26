# C++ COBOL Copybook Migration Toolkit — Implementation Plan

## Product Vision

A **generic COBOL copybook migration toolkit** written in C++ that allows clients to:

1. Parse COBOL copybook definitions (`.cpy` files)
2. Generate C++ record classes that mirror copybook layouts byte-for-byte
3. Read/write fixed-length record buffers using field names (replacing Java reflection)
4. Connect to backend data stores via gRPC (replacing JacORB CORBA)
5. Run a mock VSAM server for development and testing without mainframe access

The customs broker examples (BrokerControl, ShipmentHeader, etc.) become **sample data models**
shipped with the toolkit to demonstrate usage.

---

## Project Structure

```
copybook-toolkit/
├── CMakeLists.txt                          # Top-level CMake build
├── README.md
├── LICENSE
│
├── proto/                                  # gRPC service definitions
│   └── copybook_service.proto              # VSAM-style CRUD + browse operations
│
├── include/copybook/                       # Public headers (the SDK)
│   ├── core/
│   │   ├── field_type.h                    # Enum: CHARACTER, ZONED_NUMERIC, etc.
│   │   ├── field_descriptor.h              # Struct: size, offset, type metadata
│   │   ├── record_base.h                   # Base class: registry-map field access
│   │   ├── record_buffer.h                 # Fixed-length char buffer with bounds checking
│   │   └── copybook_parser.h              # Reads .cpy files into field descriptors
│   │
│   ├── codegen/
│   │   └── cpp_generator.h                 # Generates C++ record classes from .cpy
│   │
│   ├── client/
│   │   └── copybook_client.h               # gRPC client for VSAM operations
│   │
│   └── server/
│       ├── vsam_store.h                    # In-memory VSAM file simulation
│       └── mock_vsam_server.h              # gRPC server implementation
│
├── src/                                    # Library implementation
│   ├── core/
│   │   ├── record_base.cpp
│   │   ├── record_buffer.cpp
│   │   └── copybook_parser.cpp
│   │
│   ├── codegen/
│   │   └── cpp_generator.cpp
│   │
│   ├── client/
│   │   └── copybook_client.cpp
│   │
│   └── server/
│       ├── vsam_store.cpp
│       └── mock_vsam_server.cpp
│
├── tools/                                  # CLI utilities
│   ├── cpy2cpp.cpp                         # CLI: parse .cpy → generate C++ classes
│   └── mock_server_main.cpp                # CLI: launch standalone mock VSAM server
│
├── examples/                               # Sample copybook implementations
│   ├── copybooks/                          # COBOL copybook source files
│   │   ├── BROKER-CONTROL.cpy
│   │   ├── GENERIC-VALIDATION-BUFFER.cpy
│   │   ├── SHIPMENT-HEADER.cpy
│   │   ├── TRANS-MODE.cpy
│   │   └── PASS-LIST.cpy
│   │
│   ├── generated/                          # Output from cpy2cpp (checked in as reference)
│   │   ├── broker_control.h
│   │   ├── generic_validation_buffer.h
│   │   ├── shipment_header.h
│   │   ├── trans_mode.h
│   │   └── pass_list.h
│   │
│   └── demo/
│       ├── client_demo.cpp                 # Demo: connect to mock server, read/write records
│       └── standalone_demo.cpp             # Demo: in-process copybook parsing, no server
│
├── test/                                   # Google Test suite
│   ├── record_base_test.cpp
│   ├── record_buffer_test.cpp
│   ├── copybook_parser_test.cpp
│   ├── codegen_test.cpp
│   ├── vsam_store_test.cpp
│   ├── client_server_integration_test.cpp
│   └── data/                               # Test fixtures
│       ├── PERSON.cpy                      # Simple copybook for unit tests
│       └── sample_records.dat              # Sample fixed-length record data
│
└── docs/
    └── COPYBOOK_FORMAT.md                  # Supported COBOL copybook syntax reference
```

---

## Phase 1: Core Library — Record Engine (Replaces Java Reflection)

### 1.1 `FieldType` Enum

```cpp
// include/copybook/core/field_type.h
enum class FieldType {
    CHARACTER,          // PIC X(n) — alphanumeric
    ZONED_NUMERIC,      // PIC 9(n) — signed zoned decimal
    ZONED_UNSIGNED,     // PIC 9(n) — unsigned zoned decimal
    PACKED_DECIMAL,     // PIC 9(n) COMP-3 — packed BCD
    BINARY,             // PIC 9(n) COMP — binary integer
    RECORD,             // GROUP item — contains sub-fields
    FILLER              // FILLER — padding, not addressable by name
};
```

### 1.2 `FieldDescriptor` Struct

```cpp
// include/copybook/core/field_descriptor.h
struct FieldDescriptor {
    std::string name;
    int         size;       // byte length
    int         offset;     // position in parent buffer
    FieldType   type;
    int         decimal_positions;  // for numeric types
    bool        is_group;           // true if this field contains children
    std::vector<std::string> children;  // child field names (for GROUP items)
};
```

### 1.3 `RecordBuffer` — Bounds-Checked Fixed-Length Buffer

```cpp
// include/copybook/core/record_buffer.h
class RecordBuffer {
    std::vector<char> data_;
    size_t size_;

public:
    explicit RecordBuffer(size_t size, char fill = ' ');
    RecordBuffer(const char* raw, size_t size);
    RecordBuffer(const std::vector<char>& raw);

    void        write(int offset, int length, const std::string& value, char pad = ' ');
    std::string read(int offset, int length) const;
    char*       raw();
    const char* raw() const;
    size_t      size() const;

    // Extract a sub-buffer (for child record initialization)
    RecordBuffer slice(int offset, int length) const;

    // Write a sub-buffer back into the parent (solves Java's buffer-sync bug)
    void merge(int offset, const RecordBuffer& child);
};
```

### 1.4 `RecordBase` — Registry-Map Field Access (Replaces Java Reflection)

```cpp
// include/copybook/core/record_base.h
class RecordBase {
protected:
    RecordBuffer buffer_;
    std::unordered_map<std::string, FieldDescriptor> fields_;
    std::unordered_map<std::string, std::shared_ptr<RecordBase>> children_;

    // Called by subclass constructors to register fields
    void registerField(const FieldDescriptor& fd);
    void registerChild(const std::string& name, std::shared_ptr<RecordBase> child);

public:
    explicit RecordBase(size_t size);
    RecordBase(const char* raw, size_t size);
    virtual ~RecordBase() = default;

    // --- The same API as the Java version ---
    void        setData(const std::string& fieldName, const std::string& value);
    std::string getData(const std::string& fieldName) const;
    std::string getData() const;  // entire buffer as string

    // --- New features the Java version lacked ---
    void        setNumeric(const std::string& fieldName, long value);
    long        getNumeric(const std::string& fieldName) const;
    bool        hasField(const std::string& fieldName) const;
    std::vector<std::string> fieldNames() const;
    const FieldDescriptor&   fieldInfo(const std::string& fieldName) const;

    // Child record access
    std::shared_ptr<RecordBase> getChild(const std::string& name);

    // Buffer sync: flush child buffers back into parent
    void syncChildren();

    // Serialize / deserialize
    const RecordBuffer& buffer() const;
    void loadFromBuffer(const char* raw, size_t size);
};
```

**Key difference from Java:** Instead of `Class.getField("FOO_SIZE")` reflection, each
subclass constructor calls `registerField({"FOO", 5, 1068, FieldType::CHARACTER})`.
Same string-based API, zero runtime reflection, full type safety.

---

## Phase 2: COBOL Copybook Parser

### 2.1 Supported Syntax

The parser reads standard COBOL copybook definitions:

```cobol
       01  BROKER-CONTROL.
           05  COMPANY-NO              PIC X(2).
           05  DIVISION-NO             PIC X(4).
           05  IMPORT-DIVISION         PIC X(1).
           05  EXPORT-DIVISION         PIC X(1).
           05  NAME                    PIC X(35).
           05  ADDRESS-1               PIC X(35).
           05  CITY                    PIC X(35).
           05  STATE-PROVINCE          PIC X(2).
           05  ZIP                     PIC 9(9).
           05  PHONE-NO-ALPHA          PIC X(15).
           05  DATE-UPDATED            PIC 9(8).
           05  BOND-AMT                PIC 9(9)V99.
           05  FILLER                  PIC X(3).
           05  TRANS-MODE-RECORD.
               10  MOT-ALPHA           PIC X(2).
               10  MOT-DESC            PIC X(35).
```

### 2.2 Parser Capabilities

```cpp
// include/copybook/core/copybook_parser.h
class CopybookParser {
public:
    struct ParseResult {
        std::string record_name;
        int total_size;
        std::vector<FieldDescriptor> fields;
        std::vector<std::string> warnings;  // non-fatal parse issues
    };

    // Parse from file
    ParseResult parseFile(const std::string& filepath);

    // Parse from string (for embedded definitions or testing)
    ParseResult parseString(const std::string& copybook_text);
};
```

### 2.3 COBOL Features to Support

| Feature | COBOL Syntax | Priority |
|---|---|---|
| Alphanumeric fields | `PIC X(n)` | P0 — must have |
| Unsigned numeric | `PIC 9(n)` | P0 — must have |
| Signed numeric | `PIC S9(n)` | P0 — must have |
| Implied decimal | `PIC 9(5)V99` | P1 — needed |
| Group items (nesting) | Level numbers 01-49 | P0 — must have |
| FILLER | `FILLER PIC X(n)` | P0 — must have |
| OCCURS (arrays) | `OCCURS n TIMES` | P1 — needed |
| REDEFINES | `REDEFINES field-name` | P1 — needed for GenericValidationBuffer |
| COMP-3 (packed decimal) | `PIC 9(n) COMP-3` | P2 — later |
| COMP (binary) | `PIC 9(n) COMP` | P2 — later |
| 88-level conditions | `88 IS-ACTIVE VALUE 'Y'` | P2 — later |

---

## Phase 3: C++ Code Generator

### 3.1 Purpose

Reads a `.cpy` file and generates a C++ header/source pair that subclasses `RecordBase`:

```
$ ./cpy2cpp --input BROKER-CONTROL.cpy --output broker_control.h --class BrokerControl
```

### 3.2 Generated Code Pattern

```cpp
// Generated from BROKER-CONTROL.cpy
#pragma once
#include <copybook/core/record_base.h>

class BrokerControl : public copybook::RecordBase {
public:
    static constexpr int RECORD_SIZE = 1000;

    BrokerControl() : RecordBase(RECORD_SIZE) { initFields(); }
    BrokerControl(const char* raw, size_t len) : RecordBase(raw, len) { initFields(); }

private:
    void initFields() {
        registerField({"COMPANY_NO",     2,   0, FieldType::CHARACTER});
        registerField({"DIVISION_NO",    4,   2, FieldType::CHARACTER});
        registerField({"IMPORT_DIVISION",1,   6, FieldType::CHARACTER});
        registerField({"NAME",          35,   8, FieldType::CHARACTER});
        // ... all 223 fields
    }
};
```

### 3.3 Generator Features

- Converts COBOL field names to C++ identifiers (hyphens → underscores)
- Calculates offsets automatically from field sizes and nesting
- Generates `static constexpr` size constants for documentation
- Handles GROUP items by creating nested `RecordBase` children
- Generates `#pragma once` headers, optional `.cpp` split
- Emits a `// Source: BROKER-CONTROL.cpy` provenance comment

---

## Phase 4: gRPC Service Definition (Replaces JacORB CORBA)

### 4.1 Proto Definition

```protobuf
// proto/copybook_service.proto
syntax = "proto3";
package copybook;

service CopybookService {
    // VSAM-style CRUD operations
    rpc Read       (ReadRequest)       returns (ReadResponse);
    rpc Write      (WriteRequest)      returns (WriteResponse);
    rpc Rewrite    (RewriteRequest)    returns (WriteResponse);
    rpc Delete     (DeleteRequest)     returns (DeleteResponse);

    // VSAM-style sequential browse
    rpc StartBrowse(BrowseRequest)     returns (BrowseResponse);
    rpc ReadNext   (ReadNextRequest)   returns (ReadResponse);
    rpc ReadPrev   (ReadPrevRequest)   returns (ReadResponse);
    rpc EndBrowse  (EndBrowseRequest)  returns (EndBrowseResponse);

    // Batch operations
    rpc ReadBatch  (BatchReadRequest)  returns (stream ReadResponse);

    // Validation (mirrors original ValidateImpl.execute)
    rpc Validate   (ValidateRequest)   returns (ValidateResponse);
}

message RecordData {
    string copybook_name = 1;   // e.g. "BROKER-CONTROL"
    bytes  raw_buffer    = 2;   // fixed-length record bytes
    int32  record_size   = 3;   // expected buffer size
}

// --- CRUD Messages ---

message ReadRequest {
    string dataset_name  = 1;   // VSAM dataset / file name
    string key_field     = 2;   // field name to use as key
    string key_value     = 3;   // key value to look up
    string copybook_name = 4;   // which copybook layout to expect
}

message ReadResponse {
    bool       found       = 1;
    RecordData record      = 2;
    string     error       = 3;
}

message WriteRequest {
    string     dataset_name = 1;
    RecordData record       = 2;
    string     key_field    = 3;
}

message RewriteRequest {
    string     dataset_name = 1;
    RecordData record       = 2;
    string     key_field    = 3;
}

message DeleteRequest {
    string dataset_name = 1;
    string key_field    = 2;
    string key_value    = 3;
}

message DeleteResponse {
    bool   deleted = 1;
    string error   = 2;
}

// --- Browse Messages (Sequential Access) ---

message BrowseRequest {
    string dataset_name  = 1;
    string key_field     = 2;
    string start_key     = 3;   // position cursor at or after this key
    string copybook_name = 4;
}

message BrowseResponse {
    string browse_id = 1;       // handle for ReadNext/ReadPrev/EndBrowse
    bool   positioned = 2;
    string error     = 3;
}

message ReadNextRequest {
    string browse_id = 1;
}

message ReadPrevRequest {
    string browse_id = 1;
}

message EndBrowseRequest {
    string browse_id = 1;
}

message EndBrowseResponse {
    bool closed = 1;
}

// --- Batch ---

message BatchReadRequest {
    string dataset_name  = 1;
    string copybook_name = 2;
    int32  max_records   = 3;   // 0 = all
}

// --- Validation (mirrors original ValidateImpl) ---

message ValidateRequest {
    RecordData pass_list = 1;   // full 4612-byte PASS-LIST buffer
}

message ValidateResponse {
    RecordData pass_list     = 1;   // modified buffer with validation results
    bool       valid         = 2;
    string     error_message = 3;
}
```

### 4.2 Why gRPC Over Other Options

| Factor | gRPC | REST/HTTP | CORBA (omniORB) | ZeroMQ |
|---|---|---|---|---|
| C++ support | First-class | Needs framework | Legacy | Good |
| Type safety | Protobuf IDL | Manual | IDL (archaic) | None |
| Streaming | Built-in | SSE/WebSocket | Complex | Manual |
| Performance | High (HTTP/2) | Medium | High | Highest |
| Ecosystem | Very large | Ubiquitous | Shrinking | Medium |
| Client codegen | Automatic | Manual | Automatic | N/A |
| Future-proof | Yes | Yes | No | Niche |

gRPC provides the closest semantic match to CORBA's IDL-based RPC while being modern,
well-supported in C++, and having automatic client/server stub generation.

---

## Phase 5: Mock VSAM Server

### 5.1 Purpose

A standalone gRPC server that simulates VSAM file operations using in-memory storage.
This allows developers to build and test migration code without mainframe access.

### 5.2 `VsamStore` — In-Memory VSAM Simulation

```cpp
// include/copybook/server/vsam_store.h
class VsamStore {
public:
    struct DatasetConfig {
        std::string name;           // e.g. "BROKER.CONTROL.FILE"
        std::string copybook_name;  // e.g. "BROKER-CONTROL"
        int record_size;            // e.g. 1000
        std::string key_field;      // primary key field name
        int key_offset;
        int key_size;
    };

    // Dataset management
    void createDataset(const DatasetConfig& config);
    void loadFromFile(const std::string& dataset, const std::string& filepath);
    void dumpToFile(const std::string& dataset, const std::string& filepath);

    // VSAM KSDS operations
    ReadResult    read(const std::string& dataset, const std::string& key);
    WriteResult   write(const std::string& dataset, const std::vector<char>& record);
    WriteResult   rewrite(const std::string& dataset, const std::vector<char>& record);
    DeleteResult  remove(const std::string& dataset, const std::string& key);

    // VSAM browse (sequential access)
    std::string   startBrowse(const std::string& dataset, const std::string& startKey);
    ReadResult    readNext(const std::string& browseId);
    ReadResult    readPrev(const std::string& browseId);
    void          endBrowse(const std::string& browseId);

private:
    // Each dataset is a std::map<string, vector<char>> ordered by key
    // This mirrors VSAM KSDS (Key-Sequenced Data Set) behavior
    struct Dataset {
        DatasetConfig config;
        std::map<std::string, std::vector<char>> records;  // key → raw record
    };

    struct BrowseCursor {
        std::string dataset;
        std::map<std::string, std::vector<char>>::iterator position;
    };

    std::unordered_map<std::string, Dataset> datasets_;
    std::unordered_map<std::string, BrowseCursor> cursors_;
};
```

### 5.3 Mock Server Features

- **KSDS simulation:** Records stored in `std::map` (sorted by key), mirroring VSAM KSDS
- **Browse cursors:** Stateful iteration with `StartBrowse`/`ReadNext`/`ReadPrev`/`EndBrowse`
- **Data persistence:** Load/dump datasets from flat files (fixed-length record format)
- **Multi-dataset:** Run multiple VSAM files simultaneously
- **Validation endpoint:** Implements the `Validate` RPC with pluggable validation logic
- **Seed data:** Ships with sample customs broker records for the example copybooks
- **Configurable via YAML/JSON:** Dataset definitions, key fields, record sizes

### 5.4 Standalone Server Launch

```bash
$ ./mock_vsam_server --config datasets.yaml --port 50051

# datasets.yaml
datasets:
  - name: "BROKER.CONTROL.FILE"
    copybook: "BROKER-CONTROL"
    record_size: 1000
    key_field: "FILE_NO"
    data_file: "seed/broker_control.dat"

  - name: "SHIPMENT.HEADER.FILE"
    copybook: "SHIPMENT-HEADER"
    record_size: 1750
    key_field: "FILE_NO"
    data_file: "seed/shipment_header.dat"
```

---

## Phase 6: gRPC Client Library

### 6.1 Client API

```cpp
// include/copybook/client/copybook_client.h
class CopybookClient {
public:
    explicit CopybookClient(const std::string& server_address);

    // CRUD
    std::optional<RecordBase> read(const std::string& dataset,
                                    const std::string& keyField,
                                    const std::string& keyValue,
                                    const std::string& copybookName);

    bool write(const std::string& dataset, const RecordBase& record,
               const std::string& keyField);

    bool rewrite(const std::string& dataset, const RecordBase& record,
                 const std::string& keyField);

    bool remove(const std::string& dataset,
                const std::string& keyField,
                const std::string& keyValue);

    // Browse
    BrowseSession startBrowse(const std::string& dataset,
                               const std::string& keyField,
                               const std::string& startKey,
                               const std::string& copybookName);

    // Validate (mirrors original ValidateImpl.execute)
    RecordBase validate(const RecordBase& passList);
};

class BrowseSession {
public:
    std::optional<RecordBase> next();
    std::optional<RecordBase> prev();
    void close();
};
```

---

## Phase 7: Build & Dependencies

### 7.1 CMake Structure

```cmake
cmake_minimum_required(VERSION 3.20)
project(copybook-toolkit VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Dependencies (via FetchContent or vcpkg)
find_package(gRPC REQUIRED)
find_package(Protobuf REQUIRED)
find_package(GTest REQUIRED)
find_package(yaml-cpp REQUIRED)   # for server config

# Core library (no gRPC dependency — usable standalone)
add_library(copybook-core
    src/core/record_base.cpp
    src/core/record_buffer.cpp
    src/core/copybook_parser.cpp
    src/codegen/cpp_generator.cpp
)
target_include_directories(copybook-core PUBLIC include)

# gRPC generated code
protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS proto/copybook_service.proto)
grpc_generate_cpp(GRPC_SRCS GRPC_HDRS proto/copybook_service.proto)

# Client library
add_library(copybook-client
    src/client/copybook_client.cpp
    ${PROTO_SRCS} ${GRPC_SRCS}
)
target_link_libraries(copybook-client PUBLIC copybook-core gRPC::grpc++)

# Server library
add_library(copybook-server
    src/server/vsam_store.cpp
    src/server/mock_vsam_server.cpp
    ${PROTO_SRCS} ${GRPC_SRCS}
)
target_link_libraries(copybook-server PUBLIC copybook-core gRPC::grpc++ yaml-cpp)

# CLI tools
add_executable(cpy2cpp tools/cpy2cpp.cpp)
target_link_libraries(cpy2cpp copybook-core)

add_executable(mock-vsam-server tools/mock_server_main.cpp)
target_link_libraries(mock-vsam-server copybook-server)

# Tests
enable_testing()
add_executable(copybook-tests
    test/record_base_test.cpp
    test/record_buffer_test.cpp
    test/copybook_parser_test.cpp
    test/codegen_test.cpp
    test/vsam_store_test.cpp
    test/client_server_integration_test.cpp
)
target_link_libraries(copybook-tests copybook-client copybook-server GTest::gtest_main)
gtest_discover_tests(copybook-tests)
```

### 7.2 Dependencies

| Library | Purpose | Version |
|---|---|---|
| gRPC | RPC framework (replaces JacORB CORBA) | 1.60+ |
| Protobuf | Serialization for gRPC messages | 3.x / 4.x |
| Google Test | Unit/integration testing | 1.14+ |
| yaml-cpp | Server configuration | 0.8+ |
| CMake | Build system | 3.20+ |

---

## Implementation Order

### Sprint 1: Core Engine (Week 1-2)
1. `RecordBuffer` — fixed-length buffer with bounds checking
2. `FieldDescriptor` + `FieldType` — metadata structures
3. `RecordBase` — registry-map field access (`setData`/`getData`)
4. Unit tests for all of the above
5. Manually port `PersonImpl` as proof-of-concept

### Sprint 2: Copybook Parser (Week 2-3)
6. `CopybookParser` — read `.cpy` files, produce `FieldDescriptor` vectors
7. Support PIC X, PIC 9, group items, FILLER, level numbers
8. Write sample `.cpy` files from the existing Java field definitions
9. Parser unit tests

### Sprint 3: Code Generator (Week 3-4)
10. `CppGenerator` — consume parser output, emit C++ `RecordBase` subclass headers
11. `cpy2cpp` CLI tool
12. Generate all 5 example copybook classes and verify against Java originals
13. Codegen tests

### Sprint 4: gRPC Service & Mock Server (Week 4-5)
14. Write `copybook_service.proto`
15. Implement `VsamStore` (in-memory KSDS simulation)
16. Implement `MockVsamServer` (gRPC service backed by VsamStore)
17. `mock-vsam-server` standalone binary
18. VsamStore unit tests

### Sprint 5: gRPC Client & Integration (Week 5-6)
19. Implement `CopybookClient`
20. Implement `BrowseSession`
21. Client-server integration tests
22. End-to-end demo: parse copybook → generate class → start server → CRUD records

### Sprint 6: Polish & Packaging (Week 6-7)
23. YAML-based server configuration
24. Seed data for example datasets
25. Demo programs (standalone + client/server)
26. Documentation (README, API reference, copybook format guide)
27. Package for distribution (CMake install targets, vcpkg port)

---

## Key Design Decisions

### 1. Registry Map vs. Compile-Time Templates
**Decision: Registry map.** The Java API uses string-based field names (`setData("COMPANY_NO", value)`),
and customers migrating from COBOL will expect the same. A runtime map preserves this API while
being simple to implement and debug. Template metaprogramming adds complexity without clear
user benefit for this use case.

### 2. Buffer Sync (Fixes Java Bug)
**Decision: Explicit `syncChildren()` + automatic sync on `getData()`.** The Java version
copies sub-arrays into child objects but never writes them back. The C++ version provides
`merge()` on `RecordBuffer` and calls it automatically when the full buffer is requested.

### 3. Copybook Core Has No gRPC Dependency
**Decision: `copybook-core` is a standalone library.** Users who only need copybook parsing
and record manipulation (no networking) can link only against `copybook-core`. The gRPC
client/server are separate libraries. This keeps the core lightweight and maximizes adoption.

### 4. VSAM Operations as the RPC Model
**Decision: Model the gRPC service after VSAM semantics** (READ, WRITE, REWRITE, DELETE,
STARTBR/READNEXT/READPREV/ENDBR). This is the mental model COBOL developers already have,
making adoption intuitive. The mock server uses `std::map` to simulate KSDS key-sequenced access.

### 5. COBOL Name Convention
**Decision: Convert COBOL hyphens to C++ underscores.** `COMPANY-NO` becomes `COMPANY_NO`.
The field registry stores the underscore version. The parser handles the conversion.

---

## Success Criteria

- [ ] Parse any standard COBOL copybook with PIC X, PIC 9, group items, FILLER, OCCURS
- [ ] Generate C++ classes that produce byte-identical buffers to the Java originals
- [ ] Read/write all 819 fields across the 5 example copybooks
- [ ] Mock VSAM server handles concurrent CRUD + browse operations
- [ ] Full round-trip: `.cpy` file → C++ class → write record → send via gRPC → read back → verify
- [ ] Zero external dependencies for core library beyond standard C++20
- [ ] All tests pass with no memory leaks (valgrind/ASan clean)

---

*This plan implements the C++ port (Option B from DEVELOPMENT_OPTIONS.md) with a
gRPC connectivity layer replacing JacORB CORBA, and a mock VSAM server for
development without mainframe access.*
