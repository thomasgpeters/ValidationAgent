#include "copybook/serial/yaml_serializer.h"
#include "copybook/core/field_type.h"

#include <algorithm>
#include <sstream>
#include <stdexcept>
#include <vector>

namespace copybook {

// ═══════════════════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════════════════

std::string YamlSerializer::trimRight(const std::string& s) {
    size_t end = s.find_last_not_of(' ');
    if (end == std::string::npos) return "";
    return s.substr(0, end + 1);
}

bool YamlSerializer::needsQuoting(const std::string& s) {
    if (s.empty()) return true;
    // Quote if contains special YAML characters or looks like a number
    if (s.find(':') != std::string::npos) return true;
    if (s.find('#') != std::string::npos) return true;
    if (s.find('\'') != std::string::npos) return true;
    if (s.find('"') != std::string::npos) return true;
    if (s.find('\n') != std::string::npos) return true;
    if (s[0] == ' ' || s.back() == ' ') return true;
    if (s == "true" || s == "false" || s == "null" ||
        s == "True" || s == "False" || s == "Null" ||
        s == "yes" || s == "no" || s == "Yes" || s == "No") return true;
    return false;
}

std::string YamlSerializer::escapeYamlString(const std::string& s) {
    if (!needsQuoting(s)) return s;
    std::string result = "\"";
    for (char c : s) {
        if (c == '"') result += "\\\"";
        else if (c == '\\') result += "\\\\";
        else if (c == '\n') result += "\\n";
        else result += c;
    }
    result += "\"";
    return result;
}

// ═══════════════════════════════════════════════════════════════════════════
// Serialize to YAML
// ═══════════════════════════════════════════════════════════════════════════

void YamlSerializer::emitRecord(std::ostringstream& out, const RecordBase& record,
                                 bool trim, int depth) {
    std::string indent(depth * 2, ' ');
    auto names = record.fieldNames();

    for (const auto& name : names) {
        const auto& fi = record.fieldInfo(name);

        if (fi.type == FieldType::FILLER) continue;

        if (fi.type == FieldType::RECORD) {
            auto child = record.getChild(name);
            if (child) {
                out << indent << name << ":\n";
                emitRecord(out, *child, trim, depth + 1);
            } else {
                std::string val = record.getData(name);
                if (trim) val = trimRight(val);
                out << indent << name << ": " << escapeYamlString(val) << "\n";
            }
        } else if (fi.type == FieldType::ZONED_UNSIGNED ||
                   fi.type == FieldType::ZONED_NUMERIC ||
                   fi.type == FieldType::PACKED_DECIMAL ||
                   fi.type == FieldType::BINARY) {
            std::string raw = record.getData(name);
            std::string trimmed = trimRight(raw);
            size_t start = trimmed.find_first_not_of(" 0");

            if (start == std::string::npos) {
                out << indent << name << ": 0\n";
            } else {
                std::string digits = trimmed.substr(start);
                if (fi.decimal_positions > 0 && (int)digits.size() > fi.decimal_positions) {
                    std::string intPart = digits.substr(0, digits.size() - fi.decimal_positions);
                    std::string decPart = digits.substr(digits.size() - fi.decimal_positions);
                    out << indent << name << ": " << intPart << "." << decPart << "\n";
                } else {
                    out << indent << name << ": " << digits << "\n";
                }
            }
        } else {
            std::string val = record.getData(name);
            if (trim) val = trimRight(val);
            out << indent << name << ": " << escapeYamlString(val) << "\n";
        }
    }
}

std::string YamlSerializer::toYaml(const RecordBase& record, bool trim) {
    const_cast<RecordBase&>(record).syncChildren();
    std::ostringstream out;
    out << "---\n";
    emitRecord(out, record, trim, 0);
    return out.str();
}

// ═══════════════════════════════════════════════════════════════════════════
// YAML tokenizer
// ═══════════════════════════════════════════════════════════════════════════

std::vector<YamlSerializer::YamlLine>
YamlSerializer::tokenize(const std::string& yaml) {
    std::vector<YamlLine> lines;
    std::istringstream iss(yaml);
    std::string line;

    while (std::getline(iss, line)) {
        // Skip empty lines and document markers
        if (line.empty()) continue;
        std::string trimmedLine = line;
        size_t firstNonSpace = trimmedLine.find_first_not_of(' ');
        if (firstNonSpace == std::string::npos) continue;
        if (trimmedLine.substr(firstNonSpace, 3) == "---") continue;
        if (trimmedLine.substr(firstNonSpace, 3) == "...") continue;
        if (trimmedLine[firstNonSpace] == '#') continue;

        int indent = (int)firstNonSpace;

        // Find the colon separator
        std::string content = trimmedLine.substr(firstNonSpace);
        size_t colonPos = content.find(':');
        if (colonPos == std::string::npos) continue;

        std::string key = content.substr(0, colonPos);
        std::string remainder = "";
        if (colonPos + 1 < content.size()) {
            remainder = content.substr(colonPos + 1);
            size_t vs = remainder.find_first_not_of(' ');
            if (vs != std::string::npos) {
                remainder = remainder.substr(vs);
            } else {
                remainder = "";
            }
        }

        // Strip trailing whitespace from remainder
        while (!remainder.empty() && std::isspace(remainder.back())) {
            remainder.pop_back();
        }

        // Unquote strings
        if (remainder.size() >= 2 &&
            ((remainder.front() == '"' && remainder.back() == '"') ||
             (remainder.front() == '\'' && remainder.back() == '\''))) {
            remainder = remainder.substr(1, remainder.size() - 2);
        }

        YamlLine yl;
        yl.indent = indent;
        yl.key = key;
        yl.value = remainder;
        yl.hasValue = !remainder.empty();
        lines.push_back(yl);
    }

    return lines;
}

// ═══════════════════════════════════════════════════════════════════════════
// Apply parsed YAML lines to a record
// ═══════════════════════════════════════════════════════════════════════════

void YamlSerializer::applyLines(RecordBase& record,
                                 const std::vector<YamlLine>& lines,
                                 size_t& idx, int parentIndent) {
    while (idx < lines.size()) {
        const auto& yl = lines[idx];

        // If this line is at or before the parent indent, we've exited the block
        if (yl.indent <= parentIndent && idx > 0) {
            return;
        }

        if (!record.hasField(yl.key)) {
            // Skip unknown fields and their children
            int skipIndent = yl.indent;
            idx++;
            while (idx < lines.size() && lines[idx].indent > skipIndent) {
                idx++;
            }
            continue;
        }

        const auto& fi = record.fieldInfo(yl.key);

        if (!yl.hasValue && fi.type == FieldType::RECORD) {
            // Nested object — key with no value, children indented below
            auto child = record.getChild(yl.key);
            if (child) {
                int currentIndent = yl.indent;
                idx++;
                applyLines(*child, lines, idx, currentIndent);
            } else {
                idx++;
            }
        } else {
            // Scalar value
            if (fi.type == FieldType::ZONED_UNSIGNED ||
                fi.type == FieldType::ZONED_NUMERIC ||
                fi.type == FieldType::PACKED_DECIMAL ||
                fi.type == FieldType::BINARY) {
                // Parse number
                std::string num = yl.value;
                size_t dot = num.find('.');
                if (dot != std::string::npos) {
                    std::string intPart = num.substr(0, dot);
                    std::string decPart = num.substr(dot + 1);
                    while ((int)decPart.size() < fi.decimal_positions) decPart += "0";
                    decPart = decPart.substr(0, fi.decimal_positions);
                    try {
                        long combined = std::stol(intPart + decPart);
                        record.setNumeric(yl.key, combined);
                    } catch (...) {
                        record.setData(yl.key, yl.value);
                    }
                } else {
                    try {
                        long val = std::stol(num);
                        if (fi.decimal_positions > 0) {
                            for (int i = 0; i < fi.decimal_positions; ++i) val *= 10;
                        }
                        record.setNumeric(yl.key, val);
                    } catch (...) {
                        record.setData(yl.key, yl.value);
                    }
                }
            } else {
                record.setData(yl.key, yl.value);
            }
            idx++;
        }
    }
}

void YamlSerializer::fromYaml(RecordBase& record, const std::string& yaml) {
    auto lines = tokenize(yaml);
    size_t idx = 0;
    applyLines(record, lines, idx, -1);
    record.syncChildren();
}

} // namespace copybook
