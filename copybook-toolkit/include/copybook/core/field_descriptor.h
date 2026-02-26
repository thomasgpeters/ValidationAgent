#pragma once

#include "field_type.h"
#include <string>
#include <vector>

namespace copybook {

/// Metadata describing a single field within a COBOL copybook record.
/// Equivalent to the Java pattern of FIELD_SIZE / FIELD_OFFSET / FIELD_TYPE
/// static constants, but stored as a first-class data structure.
struct FieldDescriptor {
    std::string              name;               // field name (underscored, e.g. "COMPANY_NO")
    int                      size;               // byte length in the record buffer
    int                      offset;             // byte position within parent record
    FieldType                type;               // COBOL data type
    int                      decimal_positions;  // digits after implied decimal (PIC 9(5)V99 → 2)
    bool                     is_group;           // true if this field contains children
    std::vector<std::string> children;           // child field names (for GROUP items)

    FieldDescriptor()
        : size(0), offset(0), type(FieldType::CHARACTER),
          decimal_positions(0), is_group(false) {}

    FieldDescriptor(const std::string& name, int size, int offset, FieldType type)
        : name(name), size(size), offset(offset), type(type),
          decimal_positions(0), is_group(type == FieldType::RECORD) {}

    FieldDescriptor(const std::string& name, int size, int offset, FieldType type,
                    int decimals)
        : name(name), size(size), offset(offset), type(type),
          decimal_positions(decimals), is_group(type == FieldType::RECORD) {}
};

} // namespace copybook
