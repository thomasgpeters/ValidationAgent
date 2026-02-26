#pragma once

#include <string>

namespace copybook {

/// COBOL data types supported by the copybook toolkit.
/// Maps directly to COBOL PICTURE clause categories.
enum class FieldType {
    CHARACTER,          // PIC X(n)  — alphanumeric, space-padded
    ZONED_NUMERIC,      // PIC S9(n) — signed zoned decimal (EBCDIC sign in low nibble)
    ZONED_UNSIGNED,     // PIC 9(n)  — unsigned zoned decimal
    PACKED_DECIMAL,     // PIC 9(n) COMP-3 — packed BCD (future)
    BINARY,             // PIC 9(n) COMP   — binary integer (future)
    RECORD,             // GROUP item — contains sub-fields
    FILLER              // FILLER — padding bytes, not addressable by name
};

inline std::string fieldTypeName(FieldType t) {
    switch (t) {
        case FieldType::CHARACTER:      return "CHARACTER";
        case FieldType::ZONED_NUMERIC:  return "ZONED_NUMERIC";
        case FieldType::ZONED_UNSIGNED: return "ZONED_UNSIGNED";
        case FieldType::PACKED_DECIMAL: return "PACKED_DECIMAL";
        case FieldType::BINARY:         return "BINARY";
        case FieldType::RECORD:         return "RECORD";
        case FieldType::FILLER:         return "FILLER";
    }
    return "UNKNOWN";
}

} // namespace copybook
