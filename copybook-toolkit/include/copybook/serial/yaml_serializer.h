#pragma once

#include <copybook/core/record_base.h>

#include <string>

namespace copybook {

/// Serialize a RecordBase to YAML and parse YAML back into a RecordBase.
///
/// Example output for a Person record:
/// ID: EMP-001
/// FIRST_NAME: Thomas
/// AGE: 60
/// HOME_PHONE:
///   NUMBER: 555-123-4567
class YamlSerializer {
public:
    /// Convert a record to a YAML string.
    /// If `trim` is true, trailing spaces are stripped from CHARACTER fields.
    static std::string toYaml(const RecordBase& record, bool trim = true);

    /// Load YAML data into an existing record, setting fields by name.
    /// Only fields present in the YAML are updated; others are unchanged.
    /// Throws std::runtime_error on parse errors.
    static void fromYaml(RecordBase& record, const std::string& yaml);

private:
    static std::string trimRight(const std::string& s);
    static std::string escapeYamlString(const std::string& s);
    static bool needsQuoting(const std::string& s);

    static void emitRecord(std::ostringstream& out, const RecordBase& record,
                           bool trim, int depth);

    struct YamlLine {
        int         indent;
        std::string key;
        std::string value;
        bool        hasValue;
    };

    static std::vector<YamlLine> tokenize(const std::string& yaml);
    static void applyLines(RecordBase& record, const std::vector<YamlLine>& lines,
                           size_t& idx, int parentIndent);
};

} // namespace copybook
