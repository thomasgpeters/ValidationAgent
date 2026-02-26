#include "copybook/parser/codegen.h"

#include <algorithm>
#include <cctype>
#include <fstream>
#include <sstream>

namespace copybook {

// ═══════════════════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════════════════

std::string Codegen::indent(int n) const {
    return std::string(n * 4, ' ');
}

std::string Codegen::toClassName(const std::string& cobolName) const {
    std::string result;
    bool capitalizeNext = true;
    for (char c : cobolName) {
        if (c == '-' || c == '_') {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            result += (char)std::toupper(c);
            capitalizeNext = false;
        } else {
            result += (char)std::tolower(c);
        }
    }
    return result;
}

std::string Codegen::toMemberName(const std::string& cppName) const {
    // "HOME_PHONE" → "home_phone_"
    std::string result = cppName;
    std::transform(result.begin(), result.end(), result.begin(), ::tolower);
    result += "_";
    return result;
}

std::string Codegen::fieldTypeStr(FieldType t) const {
    switch (t) {
        case FieldType::CHARACTER:      return "FieldType::CHARACTER";
        case FieldType::ZONED_NUMERIC:  return "FieldType::ZONED_NUMERIC";
        case FieldType::ZONED_UNSIGNED: return "FieldType::ZONED_UNSIGNED";
        case FieldType::PACKED_DECIMAL: return "FieldType::PACKED_DECIMAL";
        case FieldType::BINARY:         return "FieldType::BINARY";
        case FieldType::RECORD:         return "FieldType::RECORD";
        case FieldType::FILLER:         return "FieldType::FILLER";
    }
    return "FieldType::CHARACTER";
}

// ═══════════════════════════════════════════════════════════════════════════
// Emit child GROUP class
// ═══════════════════════════════════════════════════════════════════════════

void Codegen::emitChildClass(std::ostringstream& out,
                              const CopybookField& group,
                              const CodegenOptions& opts,
                              int ind) const {
    std::string className = toClassName(group.cobol_name);

    // Recursively emit any nested group classes first
    for (const auto& child : group.children) {
        if (child.is_group) {
            emitChildClass(out, child, opts, ind);
        }
    }

    out << indent(ind) << "class " << className << " : public RecordBase {\n";
    out << indent(ind) << "public:\n";
    out << indent(ind + 1) << "static constexpr int RECORD_SIZE = " << group.size << ";\n";
    out << "\n";

    if (opts.emit_offsets) {
        emitFieldConstants(out, group.children, group.offset, ind + 1);
        out << "\n";
    }

    out << indent(ind + 1) << className << "() : RecordBase(RECORD_SIZE) { initFields(); }\n";
    out << indent(ind + 1) << className << "(const char* raw, size_t len) : RecordBase(raw, len) { initFields(); }\n";

    // Child accessors
    bool hasGroupChildren = false;
    for (const auto& c : group.children) {
        if (c.is_group) { hasGroupChildren = true; break; }
    }

    if (hasGroupChildren && opts.emit_child_accessors) {
        out << "\n";
        emitChildAccessors(out, group.children, ind + 1);
    }

    out << "\n";
    out << indent(ind) << "private:\n";

    if (hasGroupChildren) {
        emitChildMembers(out, group.children, ind + 1);
        out << "\n";
    }

    out << indent(ind + 1) << "void initFields() {\n";
    emitRegisterFields(out, group.children, ind + 2);
    out << indent(ind + 1) << "}\n";

    if (hasGroupChildren) {
        out << "\n";
        out << indent(ind + 1) << "void initChildren() {\n";
        emitChildInit(out, group.children, ind + 2);
        out << indent(ind + 1) << "}\n";
    }

    out << indent(ind) << "};\n\n";
}

// ═══════════════════════════════════════════════════════════════════════════
// Emit field size/offset constants
// ═══════════════════════════════════════════════════════════════════════════

void Codegen::emitFieldConstants(std::ostringstream& out,
                                  const std::vector<CopybookField>& fields,
                                  int baseOffset,
                                  int ind) const {
    // Find max constant name length for alignment (name + "_OFFSET" is longest)
    size_t maxSizeLen = 0;
    size_t maxOffsetLen = 0;
    for (const auto& f : fields) {
        if (!f.is_filler) {
            maxSizeLen = std::max(maxSizeLen, f.cpp_name.size() + 5);   // _SIZE
            maxOffsetLen = std::max(maxOffsetLen, f.cpp_name.size() + 7); // _OFFSET
        }
    }

    out << indent(ind) << "// Field sizes\n";
    for (const auto& f : fields) {
        if (f.is_filler) continue;
        std::string constName = f.cpp_name + "_SIZE";
        constName.resize(maxOffsetLen, ' ');
        out << indent(ind) << "static constexpr int " << constName << " = "
            << f.size << ";\n";
    }

    out << "\n" << indent(ind) << "// Field offsets (relative to record start)\n";
    for (const auto& f : fields) {
        if (f.is_filler) continue;
        std::string constName = f.cpp_name + "_OFFSET";
        constName.resize(maxOffsetLen, ' ');
        out << indent(ind) << "static constexpr int " << constName << " = "
            << (f.offset - baseOffset) << ";\n";
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Emit registerField() calls
// ═══════════════════════════════════════════════════════════════════════════

void Codegen::emitRegisterFields(std::ostringstream& out,
                                  const std::vector<CopybookField>& fields,
                                  int ind) const {
    for (const auto& f : fields) {
        if (f.is_filler) continue;

        out << indent(ind) << "registerField({\"" << f.cpp_name << "\", "
            << f.cpp_name << "_SIZE, " << f.cpp_name << "_OFFSET, "
            << fieldTypeStr(f.type);

        if (f.decimal_positions > 0) {
            out << ", " << f.decimal_positions;
        }
        out << "});\n";
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Emit child-related code
// ═══════════════════════════════════════════════════════════════════════════

void Codegen::emitChildMembers(std::ostringstream& out,
                                const std::vector<CopybookField>& fields,
                                int ind) const {
    for (const auto& f : fields) {
        if (!f.is_group) continue;
        std::string cls = toClassName(f.cobol_name);
        std::string member = toMemberName(f.cpp_name);
        out << indent(ind) << "std::shared_ptr<" << cls << "> " << member << ";\n";
    }
}

void Codegen::emitChildAccessors(std::ostringstream& out,
                                  const std::vector<CopybookField>& fields,
                                  int ind) const {
    for (const auto& f : fields) {
        if (!f.is_group) continue;
        std::string cls = toClassName(f.cobol_name);
        std::string member = toMemberName(f.cpp_name);
        std::string accessor = member;
        // Remove trailing underscore for public accessor
        if (!accessor.empty() && accessor.back() == '_') accessor.pop_back();
        out << indent(ind) << cls << "& " << accessor << "() { return *"
            << member << "; }\n";
    }
}

void Codegen::emitChildInit(std::ostringstream& out,
                             const std::vector<CopybookField>& fields,
                             int ind) const {
    for (const auto& f : fields) {
        if (!f.is_group) continue;
        std::string cls = toClassName(f.cobol_name);
        std::string member = toMemberName(f.cpp_name);
        out << indent(ind) << "RecordBuffer slice_" << member
            << " = buffer_.slice(" << f.cpp_name << "_OFFSET, "
            << f.cpp_name << "_SIZE);\n";
        out << indent(ind) << member << " = std::make_shared<" << cls
            << ">(slice_" << member << ".raw(), slice_" << member << ".size());\n";
        out << indent(ind) << "registerChild(\"" << f.cpp_name << "\", "
            << f.cpp_name << "_OFFSET, " << f.cpp_name << "_SIZE, "
            << member << ");\n\n";
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Emit the main record class
// ═══════════════════════════════════════════════════════════════════════════

void Codegen::emitClass(std::ostringstream& out,
                         const CopybookDefinition& def,
                         const CodegenOptions& opts) const {
    std::string cls = def.cpp_class_name;

    // Emit child GROUP classes first
    for (const auto& f : def.fields) {
        if (f.is_group) {
            emitChildClass(out, f, opts, 0);
        }
    }

    // Main class
    if (opts.emit_comments) {
        out << "/// Generated from " << def.record_name << " copybook definition.\n";
        out << "/// Total record size: " << def.total_size << " bytes.\n";
    }

    out << "class " << cls << " : public RecordBase {\n";
    out << "public:\n";
    out << indent(1) << "static constexpr int RECORD_SIZE = " << def.total_size << ";\n\n";

    emitFieldConstants(out, def.fields, 0, 1);
    out << "\n";

    // Determine if we have children
    bool hasGroups = false;
    for (const auto& f : def.fields) {
        if (f.is_group) { hasGroups = true; break; }
    }

    // Constructors
    out << indent(1) << cls << "() : RecordBase(RECORD_SIZE) {\n";
    out << indent(2) << "initFields();\n";
    if (hasGroups) out << indent(2) << "initChildren();\n";
    out << indent(1) << "}\n\n";

    out << indent(1) << cls << "(const char* raw, size_t len) : RecordBase(raw, len) {\n";
    out << indent(2) << "initFields();\n";
    if (hasGroups) out << indent(2) << "initChildren();\n";
    out << indent(1) << "}\n";

    // Child accessors
    if (hasGroups && opts.emit_child_accessors) {
        out << "\n";
        emitChildAccessors(out, def.fields, 1);
    }

    out << "\nprivate:\n";

    // Child member pointers
    if (hasGroups) {
        emitChildMembers(out, def.fields, 1);
        out << "\n";
    }

    // initFields()
    out << indent(1) << "void initFields() {\n";
    emitRegisterFields(out, def.fields, 2);
    out << indent(1) << "}\n";

    // initChildren()
    if (hasGroups) {
        out << "\n" << indent(1) << "void initChildren() {\n";
        emitChildInit(out, def.fields, 2);
        out << indent(1) << "}\n";
    }

    out << "};\n";
}

// ═══════════════════════════════════════════════════════════════════════════
// Public API
// ═══════════════════════════════════════════════════════════════════════════

std::string Codegen::generateHeader(const CopybookDefinition& def,
                                     const CodegenOptions& opts) const {
    std::ostringstream out;

    out << "#pragma once\n";
    out << "// Auto-generated from " << def.record_name << " copybook definition.\n";
    out << "// DO NOT EDIT — regenerate from the .cpy source file.\n\n";

    out << "#include <copybook/core/record_base.h>\n";
    out << "#include <memory>\n\n";

    // Namespace
    out << "namespace " << opts.ns << " {\n\n";
    out << "using namespace copybook;\n\n";

    emitClass(out, def, opts);

    out << "\n} // namespace " << opts.ns << "\n";
    return out.str();
}

std::string Codegen::generateFile(const CopybookDefinition& def,
                                   const std::string& outputDir,
                                   const CodegenOptions& opts) const {
    std::string content = generateHeader(def, opts);

    // Build filename: "PERSON" → "person.h"
    std::string filename = def.record_name;
    std::transform(filename.begin(), filename.end(), filename.begin(), ::tolower);
    std::replace(filename.begin(), filename.end(), '-', '_');
    filename += ".h";

    std::string path = outputDir;
    if (!path.empty() && path.back() != '/') path += '/';
    path += filename;

    std::ofstream out(path);
    if (!out.is_open()) {
        throw std::runtime_error("Cannot write to: " + path);
    }
    out << content;
    out.close();

    return path;
}

} // namespace copybook
