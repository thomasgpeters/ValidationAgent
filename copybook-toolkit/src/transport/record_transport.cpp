#include <copybook/transport/record_transport.h>
#include <copybook/parser/copybook_parser.h>

#include <algorithm>
#include <filesystem>
#include <iostream>

namespace fs = std::filesystem;

namespace copybook::transport {

// ── A generic RecordBase that registers fields from a CopybookDefinition ──

class GenericRecord : public RecordBase {
public:
    GenericRecord(size_t size, const std::vector<CopybookField>& fields)
        : RecordBase(size) {
        registerFieldsRecursive(fields);
    }

    GenericRecord(const char* raw, size_t size,
                  const std::vector<CopybookField>& fields)
        : RecordBase(raw, size) {
        registerFieldsRecursive(fields);
    }

private:
    void registerFieldsRecursive(const std::vector<CopybookField>& fields) {
        for (const auto& f : fields) {
            if (f.is_filler) continue;

            FieldDescriptor fd;
            fd.name              = f.cpp_name;
            fd.size              = f.size;
            fd.offset            = f.offset;
            fd.type              = f.type;
            fd.decimal_positions = f.decimal_positions;
            registerField(fd);

            if (f.is_group && !f.children.empty()) {
                // Adjust child field offsets to be relative to the GROUP start.
                // The parser stores offsets relative to the top-level record,
                // but the child buffer starts at f.offset within the parent.
                auto adjustedChildren = f.children;
                for (auto& c : adjustedChildren) {
                    c.offset -= f.offset;
                }
                auto child = std::make_shared<GenericRecord>(
                    buffer_.raw() + f.offset, f.size, adjustedChildren);
                registerChild(f.cpp_name, f.offset, f.size, child);
            }
        }
    }
};

// ── RecordRegistry ────────────────────────────────────────────────────────

void RecordRegistry::registerCopybook(const CopybookDefinition& def,
                                       RecordFactory factory) {
    Entry entry;
    entry.def = def;
    entry.factory = factory ? factory : makeGenericFactory(def);
    entries_[def.record_name] = std::move(entry);
}

void RecordRegistry::loadDirectory(const std::string& dir) {
    if (!fs::exists(dir) || !fs::is_directory(dir)) return;

    CopybookParser parser;
    std::vector<std::string> files;

    for (const auto& e : fs::directory_iterator(dir)) {
        if (!e.is_regular_file()) continue;
        auto ext = e.path().extension().string();
        std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
        if (ext == ".cpy" || ext == ".cbl" || ext == ".cob") {
            files.push_back(e.path().string());
        }
    }
    std::sort(files.begin(), files.end());

    for (const auto& path : files) {
        try {
            auto def = parser.parseFile(path);
            registerCopybook(def);
        } catch (const std::exception& e) {
            std::cerr << "RecordRegistry: failed to parse " << path
                      << ": " << e.what() << "\n";
        }
    }
}

const CopybookDefinition* RecordRegistry::getSchema(const std::string& name) const {
    auto it = entries_.find(name);
    return (it != entries_.end()) ? &it->second.def : nullptr;
}

std::vector<const CopybookDefinition*> RecordRegistry::allSchemas() const {
    std::vector<const CopybookDefinition*> result;
    result.reserve(entries_.size());
    for (const auto& [name, entry] : entries_) {
        result.push_back(&entry.def);
    }
    return result;
}

std::shared_ptr<RecordBase> RecordRegistry::createRecord(
        const std::string& name, const char* raw, size_t size) const {
    auto it = entries_.find(name);
    if (it == entries_.end()) return nullptr;
    return it->second.factory(raw, size);
}

std::shared_ptr<RecordBase> RecordRegistry::createBlankRecord(
        const std::string& name) const {
    auto it = entries_.find(name);
    if (it == entries_.end()) return nullptr;
    // Create a blank buffer (space-filled) of the right size
    std::string blank(it->second.def.total_size, ' ');
    return it->second.factory(blank.c_str(), blank.size());
}

std::vector<std::pair<std::string, std::string>>
RecordRegistry::extractFields(const RecordBase& record, bool trim) {
    std::vector<std::pair<std::string, std::string>> result;
    for (const auto& name : record.fieldNames()) {
        std::string value = record.getData(name);
        if (trim) {
            auto end = value.find_last_not_of(' ');
            if (end != std::string::npos)
                value = value.substr(0, end + 1);
            else
                value.clear();
        }
        result.emplace_back(name, value);
    }
    return result;
}

void RecordRegistry::applyFields(RecordBase& record,
        const std::vector<std::pair<std::string, std::string>>& fields) {
    for (const auto& [name, value] : fields) {
        if (record.hasField(name)) {
            record.setData(name, value);
        }
    }
}

RecordFactory RecordRegistry::makeGenericFactory(const CopybookDefinition& def) {
    // Capture the definition by value for the factory lambda
    auto fields = def.fields;
    int totalSize = def.total_size;
    return [fields, totalSize](const char* raw, size_t size) -> std::shared_ptr<RecordBase> {
        if (raw && size > 0) {
            return std::make_shared<GenericRecord>(raw, size, fields);
        } else {
            return std::make_shared<GenericRecord>(totalSize, fields);
        }
    };
}

} // namespace copybook::transport
