#include "copybook/parser/copybook_parser.h"

#include <algorithm>
#include <cctype>
#include <fstream>
#include <regex>
#include <sstream>
#include <stdexcept>

namespace copybook {

// ═══════════════════════════════════════════════════════════════════════════
// Name conversion
// ═══════════════════════════════════════════════════════════════════════════

std::string CopybookParser::cobolToCppName(const std::string& cobolName) const {
    std::string result = cobolName;
    std::transform(result.begin(), result.end(), result.begin(), ::toupper);
    std::replace(result.begin(), result.end(), '-', '_');
    return result;
}

std::string CopybookParser::toClassName(const std::string& cobolName) const {
    // "PERSON" → "Person", "BROKER-CONTROL" → "BrokerControl"
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

// ═══════════════════════════════════════════════════════════════════════════
// PIC clause parsing
// ═══════════════════════════════════════════════════════════════════════════

int CopybookParser::parsePicSize(const std::string& pic) const {
    if (pic.empty()) return 0;

    int total = 0;
    std::string upper = pic;
    std::transform(upper.begin(), upper.end(), upper.begin(), ::toupper);

    // Remove the leading S if present (sign doesn't add bytes in zoned decimal)
    std::string work = upper;
    if (!work.empty() && work[0] == 'S') {
        work = work.substr(1);
    }

    // Split on 'V' — integer part + decimal part
    // Both contribute to byte length (V is an implied decimal, no storage)
    size_t vpos = work.find('V');
    std::string intPart = (vpos != std::string::npos) ? work.substr(0, vpos) : work;
    std::string decPart = (vpos != std::string::npos) ? work.substr(vpos + 1) : "";

    auto countChars = [](const std::string& part) -> int {
        int count = 0;
        size_t i = 0;
        while (i < part.size()) {
            char c = part[i];
            if (c == 'X' || c == '9' || c == 'A' || c == 'Z' || c == '0') {
                // Check for (n) repeat
                if (i + 1 < part.size() && part[i + 1] == '(') {
                    size_t close = part.find(')', i + 2);
                    if (close != std::string::npos) {
                        int repeat = std::stoi(part.substr(i + 2, close - i - 2));
                        count += repeat;
                        i = close + 1;
                        continue;
                    }
                }
                count += 1;
                i++;
            } else {
                i++;
            }
        }
        return count;
    };

    total = countChars(intPart) + countChars(decPart);
    return total;
}

int CopybookParser::parsePicDecimals(const std::string& pic) const {
    std::string upper = pic;
    std::transform(upper.begin(), upper.end(), upper.begin(), ::toupper);

    size_t vpos = upper.find('V');
    if (vpos == std::string::npos) return 0;

    std::string decPart = upper.substr(vpos + 1);
    int count = 0;
    size_t i = 0;
    while (i < decPart.size()) {
        char c = decPart[i];
        if (c == '9' || c == 'Z' || c == '0') {
            if (i + 1 < decPart.size() && decPart[i + 1] == '(') {
                size_t close = decPart.find(')', i + 2);
                if (close != std::string::npos) {
                    count += std::stoi(decPart.substr(i + 2, close - i - 2));
                    i = close + 1;
                    continue;
                }
            }
            count += 1;
            i++;
        } else {
            i++;
        }
    }
    return count;
}

FieldType CopybookParser::parsePicType(const std::string& pic,
                                        const std::string& usage) const {
    if (pic.empty()) return FieldType::RECORD;

    std::string upperUsage = usage;
    std::transform(upperUsage.begin(), upperUsage.end(), upperUsage.begin(), ::toupper);

    if (upperUsage.find("COMP-3") != std::string::npos) return FieldType::PACKED_DECIMAL;
    if (upperUsage.find("COMP") != std::string::npos)   return FieldType::BINARY;

    std::string upper = pic;
    std::transform(upper.begin(), upper.end(), upper.begin(), ::toupper);

    if (upper[0] == 'X' || upper[0] == 'A') return FieldType::CHARACTER;
    if (upper[0] == 'S') return FieldType::ZONED_NUMERIC;
    if (upper[0] == '9' || upper[0] == 'Z') return FieldType::ZONED_UNSIGNED;

    return FieldType::CHARACTER;
}

// ═══════════════════════════════════════════════════════════════════════════
// Tokenizer: raw COBOL text → list of RawLine
// ═══════════════════════════════════════════════════════════════════════════

std::vector<CopybookParser::RawLine>
CopybookParser::tokenize(const std::string& source) const {
    std::vector<RawLine> result;
    std::istringstream iss(source);
    std::string line;

    // Accumulates continued lines (no period at end)
    std::string accumulated;

    while (std::getline(iss, line)) {
        // Strip trailing whitespace
        while (!line.empty() && std::isspace(line.back())) line.pop_back();
        if (line.empty()) continue;

        // Skip COBOL comment lines (indicator in column 7, 0-indexed col 6)
        if (line.size() > 6 && (line[6] == '*' || line[6] == '/')) continue;

        // Remove sequence number area (columns 1-6) if present
        // Detect: if line starts with 6+ characters before the level number
        std::string trimmed = line;
        // Strip leading whitespace
        size_t first = trimmed.find_first_not_of(" \t");
        if (first != std::string::npos) {
            trimmed = trimmed.substr(first);
        }

        if (trimmed.empty()) continue;

        accumulated += " " + trimmed;

        // Check if statement is complete (ends with period)
        if (accumulated.back() == '.') {
            // Remove the period
            accumulated.pop_back();

            // Parse the accumulated statement
            std::string stmt = accumulated;
            accumulated.clear();

            // Trim
            size_t s = stmt.find_first_not_of(" \t");
            if (s != std::string::npos) stmt = stmt.substr(s);
            size_t e = stmt.find_last_not_of(" \t");
            if (e != std::string::npos) stmt = stmt.substr(0, e + 1);
            if (stmt.empty()) continue;

            // Parse level number
            size_t sp = stmt.find_first_of(" \t");
            if (sp == std::string::npos) continue;

            std::string levelStr = stmt.substr(0, sp);
            int level;
            try {
                level = std::stoi(levelStr);
            } catch (...) {
                continue; // skip non-level lines
            }

            std::string rest = stmt.substr(sp);
            // Trim
            size_t rs = rest.find_first_not_of(" \t");
            if (rs != std::string::npos) rest = rest.substr(rs);

            // Parse name
            sp = rest.find_first_of(" \t");
            std::string name;
            std::string remainder;
            if (sp == std::string::npos) {
                name = rest;
            } else {
                name = rest.substr(0, sp);
                remainder = rest.substr(sp);
                size_t rr = remainder.find_first_not_of(" \t");
                if (rr != std::string::npos) remainder = remainder.substr(rr);
            }

            // Check for FILLER
            std::string upperName = name;
            std::transform(upperName.begin(), upperName.end(), upperName.begin(), ::toupper);
            bool isFiller = (upperName == "FILLER");

            // Parse PIC clause
            std::string pic;
            std::string usage;

            // Find PIC or PICTURE keyword
            std::string upperRem = remainder;
            std::transform(upperRem.begin(), upperRem.end(), upperRem.begin(), ::toupper);

            size_t picPos = upperRem.find("PIC ");
            if (picPos == std::string::npos) picPos = upperRem.find("PICTURE ");
            if (picPos != std::string::npos) {
                // Skip "PIC " or "PICTURE "
                size_t after;
                if (upperRem.substr(picPos, 8) == "PICTURE ") {
                    after = picPos + 8;
                } else {
                    after = picPos + 4;
                }
                // Skip whitespace
                while (after < remainder.size() && std::isspace(remainder[after])) after++;

                // Read the PIC value (until whitespace or end)
                size_t picEnd = after;
                while (picEnd < remainder.size() && !std::isspace(remainder[picEnd])) {
                    picEnd++;
                }
                pic = remainder.substr(after, picEnd - after);

                // Check for USAGE after PIC
                if (picEnd < remainder.size()) {
                    usage = remainder.substr(picEnd);
                    size_t us = usage.find_first_not_of(" \t");
                    if (us != std::string::npos) usage = usage.substr(us);
                }
            } else {
                // No PIC clause — check for standalone USAGE
                size_t compPos = upperRem.find("COMP");
                if (compPos != std::string::npos) {
                    usage = remainder.substr(compPos);
                }
            }

            RawLine rl;
            rl.level = level;
            rl.name = name;
            rl.pic = pic;
            rl.usage = usage;
            rl.is_filler = isFiller;
            result.push_back(rl);
        }
    }

    return result;
}

// ═══════════════════════════════════════════════════════════════════════════
// Build field from a single parsed line
// ═══════════════════════════════════════════════════════════════════════════

CopybookField CopybookParser::buildField(const RawLine& line) const {
    CopybookField f;
    f.level = line.level;
    f.cobol_name = line.name;
    f.cpp_name = cobolToCppName(line.name);
    f.pic_clause = line.pic;
    f.is_filler = line.is_filler;

    if (line.pic.empty() && line.level != 88) {
        // GROUP item — no PIC clause
        f.is_group = true;
        f.type = FieldType::RECORD;
        f.size = 0;  // computed later from children
    } else if (line.is_filler && line.pic.empty()) {
        f.is_group = false;
        f.type = FieldType::FILLER;
        f.size = 0;
    } else {
        f.is_group = false;
        f.type = parsePicType(line.pic, line.usage);
        f.size = parsePicSize(line.pic);
        f.decimal_positions = parsePicDecimals(line.pic);

        if (f.is_filler) f.type = FieldType::FILLER;
    }

    return f;
}

// ═══════════════════════════════════════════════════════════════════════════
// Build tree: assign children to groups based on level numbers
// ═══════════════════════════════════════════════════════════════════════════

void CopybookParser::buildTree(std::vector<CopybookField>& parent,
                                const std::vector<RawLine>& lines,
                                size_t& idx, int parentLevel) const {
    while (idx < lines.size()) {
        const auto& line = lines[idx];

        // If this line's level is <= parent level, we've exited the group
        if (line.level <= parentLevel) {
            return;
        }

        CopybookField field = buildField(line);
        idx++;

        // If this is a group item, recurse to collect children
        if (field.is_group) {
            buildTree(field.children, lines, idx, field.level);
        }

        parent.push_back(std::move(field));
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Compute offsets and group sizes
// ═══════════════════════════════════════════════════════════════════════════

int CopybookParser::computeGroupSize(const CopybookField& group) const {
    int total = 0;
    for (const auto& child : group.children) {
        if (child.is_group) {
            total += computeGroupSize(child);
        } else {
            total += child.size;
        }
    }
    return total;
}

void CopybookParser::computeOffsets(std::vector<CopybookField>& fields,
                                     int& runningOffset) const {
    for (auto& f : fields) {
        f.offset = runningOffset;

        if (f.is_group) {
            // Recurse into children — their offsets continue from here
            computeOffsets(f.children, runningOffset);
            // Group size = sum of children
            f.size = computeGroupSize(f);
        } else {
            runningOffset += f.size;
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Public API
// ═══════════════════════════════════════════════════════════════════════════

CopybookDefinition CopybookParser::parseString(const std::string& source) const {
    auto lines = tokenize(source);

    if (lines.empty()) {
        throw std::runtime_error("Empty copybook — no lines parsed");
    }

    CopybookDefinition def;

    // The first line should be a level-01 record definition
    if (lines[0].level != 1) {
        throw std::runtime_error(
            "Expected level 01 record definition, got level " +
            std::to_string(lines[0].level) + " '" + lines[0].name + "'");
    }

    def.record_name = lines[0].name;
    def.cpp_class_name = toClassName(lines[0].name);

    // Build tree from remaining lines
    size_t idx = 1;
    buildTree(def.fields, lines, idx, 1);

    // Compute offsets
    int offset = 0;
    computeOffsets(def.fields, offset);
    def.total_size = offset;

    return def;
}

CopybookDefinition CopybookParser::parseFile(const std::string& filePath) const {
    std::ifstream in(filePath);
    if (!in.is_open()) {
        throw std::runtime_error("Cannot open copybook file: " + filePath);
    }
    std::string source((std::istreambuf_iterator<char>(in)),
                        std::istreambuf_iterator<char>());
    return parseString(source);
}

} // namespace copybook
