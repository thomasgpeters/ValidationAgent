#include "copybook/serial/json_serializer.h"

#include <sstream>
#include <stdexcept>

namespace copybook {

// ═══════════════════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════════════════

std::string JsonSerializer::trimRight(const std::string& s) {
    size_t end = s.find_last_not_of(' ');
    if (end == std::string::npos) return "";
    return s.substr(0, end + 1);
}

// ═══════════════════════════════════════════════════════════════════════════
// Serialize to JSON (record → nlohmann::json)
// ═══════════════════════════════════════════════════════════════════════════

void JsonSerializer::recordToJson(nlohmann::ordered_json& j,
                                   const RecordBase& record, bool trim) {
    auto names = record.fieldNames();

    for (const auto& name : names) {
        const auto& fi = record.fieldInfo(name);

        if (fi.type == FieldType::FILLER) continue;

        if (fi.type == FieldType::RECORD) {
            auto child = record.getChild(name);
            if (child) {
                nlohmann::ordered_json childJson;
                recordToJson(childJson, *child, trim);
                j[name] = childJson;
            } else {
                std::string val = record.getData(name);
                if (trim) val = trimRight(val);
                j[name] = val;
            }
        } else if (fi.type == FieldType::ZONED_UNSIGNED ||
                   fi.type == FieldType::ZONED_NUMERIC ||
                   fi.type == FieldType::PACKED_DECIMAL ||
                   fi.type == FieldType::BINARY) {
            std::string raw = record.getData(name);
            std::string trimmed = trimRight(raw);
            size_t start = trimmed.find_first_not_of(" 0");

            if (start == std::string::npos) {
                if (fi.decimal_positions > 0) {
                    j[name] = 0.0;
                } else {
                    j[name] = 0;
                }
            } else {
                std::string digits = trimmed.substr(start);
                if (fi.decimal_positions > 0 && (int)digits.size() > fi.decimal_positions) {
                    std::string intPart = digits.substr(0, digits.size() - fi.decimal_positions);
                    std::string decPart = digits.substr(digits.size() - fi.decimal_positions);
                    double val = std::stod(intPart + "." + decPart);
                    j[name] = val;
                } else {
                    try {
                        j[name] = std::stol(digits);
                    } catch (...) {
                        j[name] = 0;
                    }
                }
            }
        } else {
            std::string val = record.getData(name);
            if (trim) val = trimRight(val);
            j[name] = val;
        }
    }
}

nlohmann::ordered_json JsonSerializer::toJsonObject(const RecordBase& record,
                                                     bool trim) {
    const_cast<RecordBase&>(record).syncChildren();
    nlohmann::ordered_json j;
    recordToJson(j, record, trim);
    return j;
}

std::string JsonSerializer::toJson(const RecordBase& record,
                                    bool pretty, bool trim) {
    auto j = toJsonObject(record, trim);
    return pretty ? j.dump(2) + "\n" : j.dump();
}

// ═══════════════════════════════════════════════════════════════════════════
// Parse JSON into a record (nlohmann::json → record)
// ═══════════════════════════════════════════════════════════════════════════

void JsonSerializer::jsonToRecord(RecordBase& record, const nlohmann::json& j) {
    for (auto it = j.begin(); it != j.end(); ++it) {
        const std::string& key = it.key();

        if (!record.hasField(key)) continue;

        const auto& fi = record.fieldInfo(key);

        if (it->is_object()) {
            auto child = record.getChild(key);
            if (child) {
                jsonToRecord(*child, *it);
            }
        } else if (it->is_number_float()) {
            double val = it->get<double>();
            if (fi.decimal_positions > 0) {
                // Scale to integer: 123.45 with V99 → 12345
                double scaled = val;
                for (int i = 0; i < fi.decimal_positions; ++i) {
                    scaled *= 10;
                }
                record.setNumeric(key, (long)(scaled + 0.5));
            } else {
                record.setNumeric(key, (long)val);
            }
        } else if (it->is_number_integer() || it->is_number_unsigned()) {
            long val = it->get<long>();
            if (fi.type == FieldType::ZONED_UNSIGNED ||
                fi.type == FieldType::ZONED_NUMERIC ||
                fi.type == FieldType::PACKED_DECIMAL ||
                fi.type == FieldType::BINARY) {
                if (fi.decimal_positions > 0) {
                    for (int i = 0; i < fi.decimal_positions; ++i) val *= 10;
                }
                record.setNumeric(key, val);
            } else {
                record.setData(key, std::to_string(val));
            }
        } else if (it->is_string()) {
            record.setData(key, it->get<std::string>());
        }
    }
}

void JsonSerializer::fromJsonObject(RecordBase& record, const nlohmann::json& j) {
    jsonToRecord(record, j);
    record.syncChildren();
}

void JsonSerializer::fromJson(RecordBase& record, const std::string& json) {
    auto j = nlohmann::json::parse(json);
    jsonToRecord(record, j);
    record.syncChildren();
}

} // namespace copybook
