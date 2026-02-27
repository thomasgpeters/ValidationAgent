#pragma once

#include "copybook_parser.h"

#include <string>

namespace copybook {

/// Configuration for the C++ code generator.
struct CodegenOptions {
    std::string ns = "copybook::generated";  // target namespace
    bool        emit_offsets     = true;      // static constexpr offset/size constants
    bool        emit_comments    = true;      // COBOL source as comments
    bool        emit_child_accessors = true;  // typed accessor methods for GROUP items
};

/// Generate a C++ header file from a parsed copybook definition.
///
/// The output is a complete, self-contained header that:
///   - Includes <copybook/core/record_base.h>
///   - Defines child GROUP classes (nested or separate)
///   - Defines the main record class with:
///       - RECORD_SIZE constant
///       - Field size/offset constants (optional)
///       - Constructor from blank or raw buffer
///       - registerField() calls in initFields()
///       - registerChild() calls in initChildren()
///       - Typed accessor methods for GROUP children
class Codegen {
public:
    /// Generate a C++ header string from a parsed definition.
    std::string generateHeader(const CopybookDefinition& def,
                               const CodegenOptions& opts = {}) const;

    /// Generate and write to a file. Returns the file path written.
    std::string generateFile(const CopybookDefinition& def,
                             const std::string& outputDir,
                             const CodegenOptions& opts = {}) const;

private:
    void emitClass(std::ostringstream& out,
                   const CopybookDefinition& def,
                   const CodegenOptions& opts) const;

    void emitChildClass(std::ostringstream& out,
                        const CopybookField& group,
                        const CodegenOptions& opts,
                        int indent) const;

    void emitFieldConstants(std::ostringstream& out,
                            const std::vector<CopybookField>& fields,
                            int baseOffset,
                            int indent) const;

    void emitRegisterFields(std::ostringstream& out,
                            const std::vector<CopybookField>& fields,
                            int indent) const;

    void emitRegisterChildren(std::ostringstream& out,
                              const std::vector<CopybookField>& fields,
                              int indent) const;

    void emitChildMembers(std::ostringstream& out,
                          const std::vector<CopybookField>& fields,
                          int indent) const;

    void emitChildAccessors(std::ostringstream& out,
                            const std::vector<CopybookField>& fields,
                            int indent) const;

    void emitChildInit(std::ostringstream& out,
                       const std::vector<CopybookField>& fields,
                       int indent) const;

    std::string toClassName(const std::string& cobolName) const;
    std::string toMemberName(const std::string& cppName) const;
    std::string indent(int n) const;
    std::string fieldTypeStr(FieldType t) const;
};

} // namespace copybook
