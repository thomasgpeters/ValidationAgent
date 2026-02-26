#pragma once

#include "field_descriptor.h"
#include "field_type.h"
#include "record_buffer.h"

#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace copybook {

/// Base class for all COBOL copybook record mappings.
///
/// This is the C++ replacement for the Java DataObjectBase / PassListBase
/// classes. Instead of using java.lang.reflect.Field to look up FIELD_SIZE,
/// FIELD_OFFSET, and FIELD_TYPE constants at runtime, each subclass registers
/// its fields into an unordered_map during construction.
///
/// The public API is intentionally identical to the Java version:
///   setData("FIELD_NAME", "value")
///   getData("FIELD_NAME") → string
///   getData()             → entire buffer as string
///
/// Additional methods provide numeric access, field enumeration, and child
/// record management with proper buffer synchronization.
class RecordBase {
public:
    /// Construct with a blank buffer of the given size (space-filled).
    explicit RecordBase(size_t recordSize);

    /// Construct from existing raw data (e.g. received from gRPC or read from file).
    RecordBase(const char* raw, size_t recordSize);

    virtual ~RecordBase() = default;

    // ── String field access (mirrors Java setData/getData) ─────────────

    /// Set a field's value by name. Pads with spaces if short, truncates if long.
    /// Throws std::invalid_argument if the field name is not registered.
    void setData(const std::string& fieldName, const std::string& value);

    /// Get a field's value by name.
    /// Throws std::invalid_argument if the field name is not registered.
    std::string getData(const std::string& fieldName) const;

    /// Get the entire record buffer as a string.
    std::string getData() const;

    // ── Numeric field access (new — not in Java version) ───────────────

    /// Set a numeric field from a long. Right-justified, zero-padded.
    void setNumeric(const std::string& fieldName, long value);

    /// Get a numeric field as a long. Strips leading spaces/zeros.
    long getNumeric(const std::string& fieldName) const;

    // ── Field introspection ────────────────────────────────────────────

    /// Check if a field name is registered.
    bool hasField(const std::string& fieldName) const;

    /// Get all registered field names (insertion order not guaranteed).
    std::vector<std::string> fieldNames() const;

    /// Get the descriptor for a field. Throws if not found.
    const FieldDescriptor& fieldInfo(const std::string& fieldName) const;

    /// Total record size in bytes.
    size_t recordSize() const;

    // ── Child record management ────────────────────────────────────────

    /// Get a child record object by name (e.g. "BROKER_CONTROL").
    /// Returns nullptr if no child with that name exists.
    std::shared_ptr<RecordBase> getChild(const std::string& name) const;

    /// Flush all child record buffers back into this record's buffer.
    /// Call this before getData() or serialization if children were modified.
    void syncChildren();

    /// Reload child record buffers from this record's buffer.
    /// Call this after loading new raw data into the parent.
    void refreshChildren();

    // ── Raw buffer access ──────────────────────────────────────────────

    const RecordBuffer& buffer() const;

    /// Load new raw data into the buffer, replacing existing contents.
    void loadFromBuffer(const char* raw, size_t size);

protected:
    RecordBuffer buffer_;
    std::unordered_map<std::string, FieldDescriptor> fields_;
    std::unordered_map<std::string, std::shared_ptr<RecordBase>> children_;

    // Ordered list of field names for deterministic enumeration.
    std::vector<std::string> field_order_;

    /// Register a field descriptor. Called by subclass constructors.
    void registerField(const FieldDescriptor& fd);

    /// Register a child record at the given offset. Called by subclass constructors.
    void registerChild(const std::string& name, int offset, int size,
                       std::shared_ptr<RecordBase> child);

private:
    // Tracks child offsets for sync/refresh.
    struct ChildEntry {
        int offset;
        int size;
        std::shared_ptr<RecordBase> record;
    };
    std::vector<ChildEntry> child_entries_;

    const FieldDescriptor& lookupField(const std::string& fieldName) const;
};

} // namespace copybook
