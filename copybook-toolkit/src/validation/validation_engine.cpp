#include <copybook/validation/validation_engine.h>

#include <sstream>

namespace copybook::validation {

void ValidationEngine::addRule(const std::string& recordName,
                                std::shared_ptr<ValidationRule> rule) {
    rules_[recordName].push_back(std::move(rule));
}

void ValidationEngine::addGlobalRule(std::shared_ptr<ValidationRule> rule) {
    globalRules_.push_back(std::move(rule));
}

ValidationResult ValidationEngine::validate(const std::string& recordName,
                                             const RecordBase& record) const {
    ValidationResult result;

    // Run global rules first
    for (const auto& rule : globalRules_) {
        rule->validate(record, result);
    }

    // Run record-specific rules
    auto it = rules_.find(recordName);
    if (it != rules_.end()) {
        for (const auto& rule : it->second) {
            rule->validate(record, result);
        }
    }

    return result;
}

std::vector<std::shared_ptr<ValidationRule>>
ValidationEngine::getRules(const std::string& recordName) const {
    std::vector<std::shared_ptr<ValidationRule>> all;
    all.insert(all.end(), globalRules_.begin(), globalRules_.end());

    auto it = rules_.find(recordName);
    if (it != rules_.end()) {
        all.insert(all.end(), it->second.begin(), it->second.end());
    }
    return all;
}

void ValidationEngine::clearRules(const std::string& recordName) {
    rules_.erase(recordName);
}

void ValidationEngine::clearAll() {
    rules_.clear();
    globalRules_.clear();
}

size_t ValidationEngine::ruleCount(const std::string& recordName) const {
    size_t count = globalRules_.size();
    auto it = rules_.find(recordName);
    if (it != rules_.end()) {
        count += it->second.size();
    }
    return count;
}

std::string ValidationEngine::formatResult(const ValidationResult& result) {
    std::ostringstream out;
    out << "Validation " << (result.valid ? "PASSED" : "FAILED");
    out << " (" << result.errorCount() << " error(s), "
        << result.warningCount() << " warning(s))\n";

    for (const auto& e : result.errors) {
        out << "  [" << severityName(e.severity) << "] ";
        if (!e.field_name.empty()) {
            out << e.field_name << ": ";
        }
        out << e.message;
        out << " (rule: " << e.rule_name << ")\n";
    }
    return out.str();
}

std::string ValidationEngine::formatResultJson(const ValidationResult& result) {
    std::ostringstream out;
    out << "{\n";
    out << "  \"valid\": " << (result.valid ? "true" : "false") << ",\n";
    out << "  \"error_count\": " << result.errorCount() << ",\n";
    out << "  \"warning_count\": " << result.warningCount() << ",\n";
    out << "  \"errors\": [\n";

    for (size_t i = 0; i < result.errors.size(); i++) {
        const auto& e = result.errors[i];
        out << "    {\n";
        out << "      \"field\": \"" << e.field_name << "\",\n";
        out << "      \"rule\": \"" << e.rule_name << "\",\n";
        out << "      \"severity\": \"" << severityName(e.severity) << "\",\n";
        // Escape quotes in message
        std::string msg = e.message;
        size_t pos = 0;
        while ((pos = msg.find('"', pos)) != std::string::npos) {
            msg.replace(pos, 1, "\\\"");
            pos += 2;
        }
        out << "      \"message\": \"" << msg << "\"\n";
        out << "    }";
        if (i + 1 < result.errors.size()) out << ",";
        out << "\n";
    }

    out << "  ]\n";
    out << "}";
    return out.str();
}

} // namespace copybook::validation
