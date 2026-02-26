#pragma once

#include <copybook/core/record_base.h>
#include <copybook/core/field_type.h>
#include <nlohmann/json.hpp>

#include <string>

namespace copybook {

/// Serialize a RecordBase to JSON and parse JSON back into a RecordBase.
/// Uses nlohmann/json for robust parsing and generation.
///
/// JSON output trims trailing spaces from CHARACTER fields and converts
/// numeric fields to JSON numbers. GROUP items become nested JSON objects.
///
/// Example output for a Person record:
/// {
///   "ID": "EMP-001",
///   "FIRST_NAME": "Thomas",
///   "AGE": 60,
///   "HOME_PHONE": {
///     "NUMBER": "555-123-4567"
///   }
/// }
class JsonSerializer {
public:
    /// Convert a record to a nlohmann::json object.
    static nlohmann::ordered_json toJsonObject(const RecordBase& record,
                                                bool trim = true);

    /// Convert a record to a JSON string.
    /// If `pretty` is true, output is indented with 2-space indentation.
    /// If `trim` is true, trailing spaces are stripped from CHARACTER fields.
    static std::string toJson(const RecordBase& record,
                              bool pretty = true,
                              bool trim = true);

    /// Load a nlohmann::json object into an existing record.
    /// Only fields present in the JSON are updated; others are unchanged.
    static void fromJsonObject(RecordBase& record, const nlohmann::json& j);

    /// Load JSON string into an existing record, setting fields by name.
    /// Only fields present in the JSON are updated; others are unchanged.
    /// Nested objects are handled for GROUP items with child records.
    /// Throws nlohmann::json::parse_error on malformed JSON.
    static void fromJson(RecordBase& record, const std::string& json);

private:
    static std::string trimRight(const std::string& s);
    static void recordToJson(nlohmann::ordered_json& j, const RecordBase& record,
                             bool trim);
    static void jsonToRecord(RecordBase& record, const nlohmann::json& j);
};

} // namespace copybook
