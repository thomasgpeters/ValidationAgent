#pragma once

#include <copybook/core/record_base.h>
#include <copybook/core/field_type.h>
#include <copybook/parser/copybook_parser.h>

#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace copybook::transport {

/// Factory function that creates a RecordBase of the correct size
/// for a given copybook name. Used by the transport layer to
/// instantiate the right record type for incoming buffers.
using RecordFactory = std::function<std::shared_ptr<RecordBase>(const char* raw, size_t size)>;

/// Registry of copybook schemas and record factories.
/// The gRPC server uses this to:
///   - Look up schemas by name
///   - Create RecordBase instances for incoming buffers
///   - Convert between raw buffers and field-value maps
class RecordRegistry {
public:
    /// Register a copybook definition and an optional factory.
    /// If no factory is provided, a generic RecordBase is created
    /// using the parsed definition's field layout.
    void registerCopybook(const CopybookDefinition& def,
                          RecordFactory factory = nullptr);

    /// Register all .cpy files from a directory.
    void loadDirectory(const std::string& dir);

    /// Look up a schema by record name.
    /// Returns nullptr if not found.
    const CopybookDefinition* getSchema(const std::string& name) const;

    /// Get all registered schemas.
    std::vector<const CopybookDefinition*> allSchemas() const;

    /// Create a RecordBase for a given copybook name with raw data.
    /// Returns nullptr if the copybook is not registered.
    std::shared_ptr<RecordBase> createRecord(const std::string& name,
                                              const char* raw, size_t size) const;

    /// Create a blank RecordBase for a given copybook name.
    std::shared_ptr<RecordBase> createBlankRecord(const std::string& name) const;

    /// Extract field name-value pairs from a RecordBase.
    static std::vector<std::pair<std::string, std::string>>
    extractFields(const RecordBase& record, bool trim = true);

    /// Pack field name-value pairs into a RecordBase.
    static void applyFields(RecordBase& record,
                            const std::vector<std::pair<std::string, std::string>>& fields);

private:
    struct Entry {
        CopybookDefinition def;
        RecordFactory      factory;
    };

    std::unordered_map<std::string, Entry> entries_;

    /// Build a generic factory from a CopybookDefinition.
    static RecordFactory makeGenericFactory(const CopybookDefinition& def);

    /// Register fields from a parsed definition into a RecordBase.
    static void registerFieldsFromDef(RecordBase& record,
                                       const std::vector<CopybookField>& fields);
};

} // namespace copybook::transport
