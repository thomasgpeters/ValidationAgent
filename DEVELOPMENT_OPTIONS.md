# ValidationAgent — Project Analysis & Development Options

## 1. Project Overview

This is a **COBOL Copybook-to-Java mapping framework** for a customs brokerage / import-export
workflow system (NAF — Network Application Framework). It was originally built with **JDK 1.3.1**,
developed in **BlueJ**, and used **JacORB (open-source CORBA)** as the RPC broker for connectivity
to the COBOL backend.

The system provides:

- A reflection-based engine for reading/writing fixed-length COBOL record buffers
- Java objects that mirror COBOL copybook layouts byte-for-byte
- A JNI bridge (`ValidateImpl`) to a native "VALIDATIONS" shared library
- CORBA-based connectivity via JacORB to the COBOL validation system

---

## 2. Architecture — How It Works

### Layer 1: Reflection Engine (`DataObjectBase` / `PassListBase`)

- Every COBOL field is represented by 3 static constants in the subclass:
  `FIELD_SIZE`, `FIELD_OFFSET`, `FIELD_TYPE`
- `setData("FIELD_NAME", value)` uses `java.lang.reflect.Field` to dynamically look up
  `FIELD_NAME_SIZE`, `FIELD_NAME_OFFSET`, and `FIELD_NAME_TYPE` at runtime
- It then reads/writes into a `char[]` buffer at the correct byte position
- This avoids writing hundreds of individual getter/setter methods

### Layer 2: Copybook Implementations (the `*Impl` classes)

Each class maps to a COBOL copybook record with exact byte offsets.
The main structure is `PassListImpl` (4,612 bytes total), which composes:

| Component | Size | Description |
|---|---|---|
| `BrokerControlImpl` | 1,000 bytes | 60+ fields: company, division, address, customs data |
| `GenericValidationBufferImpl` | 1,500 bytes | 80+ fields: validation flags, tariff data, case info |
| `ShipmentHeaderImpl` | 1,750 bytes | 70+ fields: file numbers, vessel info, entry data |
| `TransModeImpl` | 200 bytes | 25+ fields: transportation mode, validation flags |
| Scalar fields (fillers, dates, IDs) | 362 bytes | Control fields between the major record blocks |

### Layer 3: RPC Bridge (`ValidateImpl` + JacORB CORBA)

- `ValidateImpl` loads a native library called "VALIDATIONS" via `System.loadLibrary()`
- Declares `static native char[] validate(char[] pass_list)` — the RPC call to COBOL
- The original connectivity layer used **JacORB**, an open-source Java CORBA ORB, to broker
  communication between the Java client and the COBOL/mainframe backend
- The JacORB CORBA broker and the native VALIDATIONS library are no longer available

---

## 3. Reflection & Introspection Inventory

The Java reflection usage is concentrated in two identical base classes:

| Class | Package | Reflection APIs Used |
|---|---|---|
| `DataObjectBase` | `com.tgp.object` | `Class.getField()`, `Field.getInt()`, `Field.get()` |
| `PassListBase` | `com.workflow.client.naf.validate` | Identical copy of `DataObjectBase` |

**Pattern:** Convention-based field discovery. Given a field name `FOO`, reflection looks up:

```
this.getClass().getField("FOO_SIZE")   → int  (byte length)
this.getClass().getField("FOO_OFFSET") → int  (position in buffer)
this.getClass().getField("FOO_TYPE")   → String (data type)
```

This is **introspection only** — reading metadata from the class hierarchy. No `Method.invoke()`,
no `Constructor.newInstance()`, no dynamic proxying or bytecode manipulation.

---

## 4. Known Issues in Existing Code

| # | Issue | File(s) | Detail |
|---|---|---|---|
| 1 | **Duplicate base classes** | `DataObjectBase` vs `PassListBase` | Identical code in two packages — should be unified |
| 2 | **Off-by-one in truncation** | Both base classes, line 44 | `substring(0, sizeValue - 1)` should be `substring(0, sizeValue)` — loses 1 character |
| 3 | **Padding char is '+'** | Both base classes, line 41 | Uses `+` for padding instead of COBOL-standard spaces |
| 4 | **Bug in PersonImpl child init** | `PersonImpl.java:73-74` | Uses `HOME_PHONE_SIZE` where it should use `HOME_PHONE_OFFSET` |
| 5 | **Duplicate init blocks** | `PersonImpl.java:86-100` | Address objects reuse phone variables — copy/paste artifact |
| 6 | **Type constants unused** | All classes | `ZONED_NUMERIC_TYPE` and `ZONED_UNSIGNED_TYPE` defined but never handled differently |
| 7 | **Protected vs public** | `PassListBase` | `setData`/`getData` are `protected` vs `public` in `DataObjectBase` |
| 8 | **No buffer sync** | `PassListImpl` | Child objects get copies of sub-arrays, not views — mutations don't propagate back to parent |

---

## 5. C++ Migration Feasibility

### 5.1 Can This Be Rewritten in C++?

**Yes — and C++ is arguably a better fit for this use case.** The core operation is
reading/writing fixed-offset byte buffers, which is C++'s natural domain.

### 5.2 Java-to-C++ Feature Mapping

| Java Feature | C++ Equivalent | Complexity |
|---|---|---|
| `char[]` buffer | `std::vector<char>` or `char*` with `std::span` | Trivial |
| Reflection for field lookup | `std::unordered_map<string, FieldDescriptor>`, macros, or templates | Medium |
| Static constants (SIZE/OFFSET/TYPE) | `constexpr` constants or `offsetof()` macro | Trivial |
| Class inheritance (`PassListBase`) | C++ inheritance or CRTP pattern | Trivial |
| JNI native call | Direct function call — **no JNI overhead** | Simpler |
| Exception handling | `std::exception` / `try-catch` | Trivial |
| JacORB CORBA connectivity | See Section 6 below | Medium-High |

### 5.3 Replacing Java Reflection in C++ — Three Approaches

**Option A — Registry Map (Recommended)**

Each class registers its fields in a `std::unordered_map<std::string, FieldInfo>` at construction.
`setData("FIELD")` does a map lookup instead of reflection. Same API, same flexibility, fast.

```cpp
struct FieldInfo {
    int size;
    int offset;
    std::string type;
};

class CopybookBase {
protected:
    std::vector<char> data_elements;
    std::unordered_map<std::string, FieldInfo> field_registry;

public:
    void setData(const std::string& fieldName, const std::string& value);
    std::string getData(const std::string& fieldName) const;
    std::string getData() const;
};
```

**Option B — Macro-Based Metadata**

Use preprocessor macros to auto-generate field constants and map registration:

```cpp
#define DEFINE_FIELD(name, size, offset, type) \
    static constexpr int name##_SIZE = size; \
    static constexpr int name##_OFFSET = offset; \
    static constexpr const char* name##_TYPE = type; \
    FieldRegistrar _reg_##name{this, #name, {size, offset, type}};
```

**Option C — Template Metaprogramming**

Use `constexpr` structs and compile-time field descriptors. Most type-safe but most complex
to implement. Best suited if the field layout is fully known at compile time (which it is here).

---

## 6. Replacing JacORB CORBA — Connectivity Options for C++

The original system used **JacORB** (open-source CORBA ORB for Java) to connect the Java
client to the COBOL validation backend. In a C++ rebuild, CORBA can be replaced with a
modern alternative. The choice depends on what the COBOL backend looks like today.

### 6.1 If the COBOL System Is Still Running on a Mainframe

| Option | Description | Pros | Cons |
|---|---|---|---|
| **gRPC** | Google's high-performance RPC framework with C++ support | Fast, strongly typed via Protobuf, streaming support, large ecosystem | Requires a gateway/adapter on the mainframe side |
| **IBM CICS Transaction Gateway** | IBM's connector for CICS programs | Direct COBOL/CICS access, official IBM support | Vendor lock-in, cost, Java-centric SDK |
| **MQ (IBM MQ / RabbitMQ)** | Message queue with request/reply pattern | Reliable, async-capable, decoupled | Higher latency than direct RPC, more infrastructure |
| **TAO (The ACE ORB)** | Open-source C++ CORBA ORB | Drop-in CORBA replacement, IDL-compatible with JacORB | CORBA is legacy, limited community |
| **omniORB** | Lightweight open-source C++ CORBA ORB | Fast, mature, small footprint, well-maintained | Still CORBA — carries legacy baggage |

### 6.2 If the COBOL Logic Will Be Rewritten

| Option | Description | Pros | Cons |
|---|---|---|---|
| **gRPC (Recommended)** | Define service in Protobuf, implement in C++ | Modern, fast, type-safe, great C++ support | Learning curve for Protobuf |
| **REST/HTTP** | JSON or binary over HTTP | Simple, universal, easy to debug | Slower than gRPC, less type safety |
| **ZeroMQ** | Lightweight messaging library | Very fast, no broker needed, simple API | No built-in serialization, DIY protocol |
| **Apache Thrift** | Cross-language RPC framework (originated at Facebook) | IDL-based like CORBA, multi-language | Smaller community than gRPC |
| **Direct library call** | Compile validation logic as a C++ shared library | Zero overhead, simplest possible integration | Only works if validation runs in-process |

### 6.3 If CORBA Compatibility Is Required (Transitional)

If existing CORBA IDL definitions must be preserved during migration, two C++ ORBs
can serve as drop-in replacements for JacORB:

- **omniORB** — Lightweight, fast, well-maintained. Best choice for a transitional CORBA layer.
- **TAO** — Full-featured, supports RT-CORBA. Heavier but more complete.

Both can consume the same IDL files that JacORB used, making a phased migration possible:
keep CORBA temporarily while building out a gRPC replacement.

### 6.4 Recommended Path

```
Phase 1: C++ port with omniORB (reuse existing CORBA IDL if available)
    │
    ▼
Phase 2: Introduce gRPC service definitions alongside CORBA
    │
    ▼
Phase 3: Retire CORBA, run pure gRPC
```

If no CORBA IDL files survive, skip directly to gRPC.

---

## 7. Copybook Data Model — Complete Buffer Layout

### PassListImpl — 4,612 bytes

```
Offset    Size   Field                          Type
──────    ────   ─────                          ────
0         1000   BROKER_CONTROL                 RECORD
1000      32     T_ELEMENT                      CHARACTER
1032      32     T_FILE                         CHARACTER
1064      1      T_MODE                         CHARACTER
1065      3      T_FILLER_01                    CHARACTER
1068      5      T_VALIDATE_NO                  CHARACTER
1073      3      T_FILLER_02                    CHARACTER
1076      1      T_VALID_STATUS                 CHARACTER
1077      3      T_FILLER_03                    CHARACTER
1080      5      T_ERROR_NO                     CHARACTER
1085      3      T_FILLER_04                    CHARACTER
1088      1      T_ERROR_STATUS                 CHARACTER
1089      3      T_FILLER_05                    CHARACTER
1092      150    T_VALID_DATA                   CHARACTER
1242      3      T_FILLER_06                    CHARACTER
1245      78     T_DATE_STRING                  CHARACTER
1323      2      T_FILLER_07                    CHARACTER
1325      8      T_PROG_ID                      CHARACTER
1333      3      T_ERROR_COUNTER                CHARACTER
1336      1      T_FILLER_08                    CHARACTER
1337      1500   GENERIC_VALIDATION_BUFFER      RECORD
2837      1750   SHIPMENT_HEADER                RECORD
4587      2      T_FILLER_09                    CHARACTER
4589      10     T_FILE_NO                      CHARACTER
4599      12     LOGONID                        CHARACTER
                                                ─────
                                         Total: 4611 bytes + 1 = 4612
```

### Child Records

- **BrokerControlImpl** — 1,000 bytes, 60+ fields (customs broker configuration)
- **GenericValidationBufferImpl** — 1,500 bytes, 80+ fields (validation flags, tariff keys, case data)
  - Contains **TransModeImpl** — 200 bytes, 25+ fields (transportation mode configuration)
- **ShipmentHeaderImpl** — 1,750 bytes, 70+ fields (shipment and entry metadata)

---

## 8. Current Project Status

| Component | Status | Notes |
|---|---|---|
| Copybook field definitions | **Complete** | All SIZE/OFFSET/TYPE constants defined for all records |
| Reflection-based parser | **Functional with bugs** | Off-by-one, wrong padding char, no buffer sync |
| Data model classes | **Complete** | BrokerControl, GenericValidation, ShipmentHeader, TransMode, Person |
| RPC/CORBA bridge | **Missing** | JacORB connector and native VALIDATIONS library are gone |
| Tests | **Minimal** | 1 JUnit test + 1 main() test, both only exercise 2 fields |
| Build system | **Obsolete** | BlueJ/JCreator project files — no Maven/Gradle/CMake |
| Java version | **Outdated** | JDK 1.3.1 era — needs modernization if staying in Java |

---

## 9. Recommended Next Steps

### Path A: Modernize in Java

1. Fix the 8 known bugs documented in Section 4
2. Unify `DataObjectBase` and `PassListBase` into a single base class
3. Add Maven/Gradle build system
4. Upgrade to modern Java (17+) — the reflection API is stable and backward-compatible
5. Replace JacORB with gRPC-Java for the connectivity layer
6. Add comprehensive unit tests for all copybook fields

### Path B: Rewrite in C++

1. Create CMake project structure
2. Implement `CopybookBase` with registry-map pattern (replaces reflection)
3. Port all `*Impl` classes — field constants become `constexpr`, constructors register fields
4. Implement connectivity via gRPC (or omniORB if CORBA IDL files exist)
5. Link directly to COBOL libraries if available (no JNI overhead)
6. Add Google Test / Catch2 unit tests

### Path C: Hybrid Approach

1. Keep Java copybook definitions as the source of truth
2. Write a code generator that reads the Java `*Impl` classes and emits C++ equivalents
3. Build the C++ connectivity layer (gRPC) independently
4. Migrate incrementally

---

## 10. File Inventory

### Java Source Files (14 total)

| File | Package | Purpose |
|---|---|---|
| `DataObjectBase.java` | `com.tgp.object` | Reflection-based COBOL record base class |
| `PersonImpl.java` | `com.tgp.object.data` | Person record (115 bytes) — sample/demo |
| `PhoneImpl.java` | `com.tgp.object.data` | Phone record (12 bytes) — child of Person |
| `AddressImpl.java` | `com.tgp.object.data` | Address record (12 bytes) — child of Person |
| `PassListBase.java` | `com.workflow.client.naf.validate` | Validation record base class (copy of DataObjectBase) |
| `PassListImpl.java` | `com.workflow.client.naf.validate` | Main validation buffer (4,612 bytes) |
| `BrokerControlImpl.java` | `com.workflow.client.naf.validate` | Broker control record (1,000 bytes) |
| `GenericValidationBufferImpl.java` | `com.workflow.client.naf.validate` | Validation flags/data (1,500 bytes) |
| `ShipmentHeaderImpl.java` | `com.workflow.client.naf.validate` | Shipment metadata (1,750 bytes) |
| `TransModeImpl.java` | `com.workflow.client.naf.validate` | Transportation mode (200 bytes) |
| `PersonImpl.java` | `com.workflow.client.naf.validate` | Person record (91 bytes) — validation version |
| `ValidateImpl.java` | `com.workflow.client.naf.validate` | JNI/native library bridge (RPC stub) |
| `TestValidate.java` | `com.workflow.client.naf.validate` | Integration test with main() |
| `TestClass.java` | `com.workflow.client.naf.validate` | JUnit test suite |

---

*Document generated from codebase analysis. Original project by Thomas Peters.*
