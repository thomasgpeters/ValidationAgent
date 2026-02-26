#pragma once

#include <copybook/core/field_descriptor.h>
#include <copybook/core/field_type.h>

#include <string>
#include <vector>

namespace copybook {

/// A parsed COBOL copybook field with hierarchy information.
/// This is an intermediate representation — the parser builds a tree of these
/// nodes, then the code generator flattens it into FieldDescriptors.
struct CopybookField {
    int                        level;           // COBOL level number (01, 05, 10, ...)
    std::string                cobol_name;      // original COBOL name (e.g. "FIRST-NAME")
    std::string                cpp_name;        // C++ name (e.g. "FIRST_NAME")
    std::string                pic_clause;      // raw PIC string (e.g. "X(32)")
    FieldType                  type;
    int                        size;            // byte length
    int                        decimal_positions;
    int                        offset;          // byte offset within parent (computed)
    bool                       is_filler;
    bool                       is_group;        // true if no PIC clause (has children)
    std::vector<CopybookField> children;

    CopybookField()
        : level(0), type(FieldType::CHARACTER), size(0),
          decimal_positions(0), offset(0), is_filler(false), is_group(false) {}
};

/// Result of parsing a COBOL copybook file.
struct CopybookDefinition {
    std::string                record_name;     // top-level 01 name (e.g. "PERSON")
    std::string                cpp_class_name;  // C++ class name (e.g. "Person")
    int                        total_size;      // total record size in bytes
    std::vector<CopybookField> fields;          // top-level fields (under 01)
    std::vector<std::string>   warnings;        // non-fatal parse warnings
};

/// Parse a COBOL copybook (.cpy) file into a CopybookDefinition.
///
/// Supported PIC clause forms:
///   PIC X(n)       → CHARACTER
///   PIC X          → CHARACTER, size 1
///   PIC XX...      → CHARACTER, size = count of X's
///   PIC 9(n)       → ZONED_UNSIGNED
///   PIC S9(n)      → ZONED_NUMERIC
///   PIC 9(n)V9(m)  → ZONED_UNSIGNED with decimal_positions
///   PIC 9(n)V99    → ZONED_UNSIGNED with decimal_positions
///   PIC 9(n) COMP-3 → PACKED_DECIMAL
///   PIC 9(n) COMP  → BINARY
///   FILLER         → FILLER type, not addressable
///   GROUP items    → RECORD type (no PIC clause, have children)
///
/// Throws std::runtime_error on malformed input.
class CopybookParser {
public:
    /// Parse from a file path.
    CopybookDefinition parseFile(const std::string& filePath) const;

    /// Parse from a string (for testing).
    CopybookDefinition parseString(const std::string& source) const;

private:
    struct RawLine {
        int         level;
        std::string name;
        std::string pic;
        std::string usage;      // COMP, COMP-3, etc.
        bool        is_filler;
    };

    std::vector<RawLine> tokenize(const std::string& source) const;
    CopybookField        buildField(const RawLine& line) const;
    void                 buildTree(std::vector<CopybookField>& parent,
                                   const std::vector<RawLine>& lines,
                                   size_t& idx, int parentLevel) const;
    void                 computeOffsets(std::vector<CopybookField>& fields,
                                        int& runningOffset) const;
    int                  computeGroupSize(const CopybookField& group) const;
    int                  parsePicSize(const std::string& pic) const;
    int                  parsePicDecimals(const std::string& pic) const;
    FieldType            parsePicType(const std::string& pic,
                                      const std::string& usage) const;
    std::string          cobolToCppName(const std::string& cobolName) const;
    std::string          toClassName(const std::string& cobolName) const;
};

} // namespace copybook
