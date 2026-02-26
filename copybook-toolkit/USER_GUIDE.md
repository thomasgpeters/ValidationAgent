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
12. [Migration from Java](#migration-from-java)
13. [Test Harness](#test-harness)
14. [Running Tests](#running-tests)
15. [Project Structure](#project-structure)
16. [Roadmap](#roadmap)

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
┌──────────────────────────────────────────────────────────┐
│                    Your Application                       │
├──────────────────────────────────────────────────────────┤
│  Generated Record Classes (Person, Account, etc.)        │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐         │
│  │ Person     │  │ Account    │  │ BrokerCtl  │  ...     │
│  │ : RecordBase  │ : RecordBase  │ : RecordBase          │
│  └────────────┘  └────────────┘  └────────────┘         │
├──────────────────────────────────────────────────────────┤
│  Core Library (libcopybook-core.a)                       │
│  ┌──────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │ RecordBase   │  │ RecordBuffer │  │FieldDescriptor│  │
│  │              │  │              │  │FieldType      │  │
│  │ setData()    │  │ read/write   │  │               │  │
│  │ getData()    │  │ slice/merge  │  │ name, offset  │  │
│  │ setNumeric() │  │ boundsCheck  │  │ size, type    │  │
│  └──────────────┘  └──────────────┘  └───────────────┘  │
└──────────────────────────────────────────────────────────┘
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

| Requirement | Minimum Version |
|---|---|
| C++ compiler | C++17 (GCC 7+, Clang 5+, MSVC 19.14+) |
| CMake | 3.16+ |
| ncurses (for test harness only) | 6.x |

GoogleTest is fetched automatically by CMake — no manual installation needed.

### Build Commands

```bash
cd copybook-toolkit
mkdir -p build && cd build

# Configure
cmake .. -DCMAKE_BUILD_TYPE=Debug

# Build everything (library + demo + tests + harness)
cmake --build .

# Run unit tests
ctest --output-on-failure

# Run standalone demo
./standalone-demo

# Run interactive test harness
./test-harness
```

### Build Targets

| Target | Description |
|---|---|
| `copybook-core` | Static library (`libcopybook-core.a`) |
| `standalone-demo` | Non-interactive demo showing full record lifecycle |
| `copybook-tests` | GoogleTest unit test binary |
| `test-harness` | ncurses interactive test/demo runner |

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

### Unit Tests (GoogleTest)

```bash
cd copybook-toolkit/build
ctest --output-on-failure
```

Or run the test binary directly for verbose output:

```bash
./copybook-tests --gtest_print_time=0
```

### Specific Test Suite

```bash
./copybook-tests --gtest_filter="RecordBufferTest.*"
./copybook-tests --gtest_filter="PersonTest.*"
```

---

## Project Structure

```
copybook-toolkit/
├── CMakeLists.txt                          Build system
├── include/copybook/core/
│   ├── field_type.h                        COBOL type enum
│   ├── field_descriptor.h                  Field metadata struct
│   ├── record_buffer.h                     Fixed-length byte buffer
│   └── record_base.h                       Base class for all records
├── src/core/
│   ├── record_buffer.cpp                   Buffer implementation
│   └── record_base.cpp                     RecordBase implementation
├── examples/
│   ├── copybooks/
│   │   └── PERSON.cpy                      Sample COBOL copybook
│   ├── generated/
│   │   ├── person.h                        Person record (ported from Java)
│   │   ├── phone.h                         Phone sub-record
│   │   └── address.h                       Address sub-record
│   └── demo/
│       └── standalone_demo.cpp             Non-interactive demo
├── test/
│   ├── data/
│   │   └── PERSON.cpy                      Test copybook data
│   ├── record_buffer_test.cpp              RecordBuffer unit tests
│   └── record_base_test.cpp                RecordBase + Person unit tests
└── tools/
    └── test_harness.cpp                    ncurses interactive harness
```

---

## Roadmap

| Sprint | Status | Deliverable |
|---|---|---|
| **1** | Complete | Core engine: RecordBuffer, RecordBase, Person proof-of-concept |
| **2** | Next | COBOL copybook parser: reads `.cpy` files, auto-generates C++ classes |
| **3** | Planned | gRPC transport layer: replaces Java socket server |
| **4** | Planned | Validation engine: port rule evaluation from Java `ValidateData` |
| **5** | Planned | Integration tests, CI pipeline, end-to-end demo |
