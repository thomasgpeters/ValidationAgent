#pragma once

#include <copybook/validation/validation_rule.h>
#include <copybook/core/record_base.h>

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace copybook::validation {

/// Validation engine that manages and executes validation rules
/// against RecordBase instances.
///
/// Rules can be registered globally (apply to all records) or
/// scoped to a specific record type by name.
///
/// Example:
///   ValidationEngine engine;
///
///   // Register rules for PERSON records
///   engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
///   engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
///   engine.addRule("PERSON", std::make_shared<PatternRule>(
///       "DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD"));
///
///   // Validate a record
///   Person person;
///   person.setData("ID", "EMP-001");
///   person.setNumeric("AGE", 200);
///   auto result = engine.validate("PERSON", person);
///   // result.valid == false (AGE out of range)
class ValidationEngine {
public:
    /// Add a rule scoped to a specific record type.
    void addRule(const std::string& recordName,
                 std::shared_ptr<ValidationRule> rule);

    /// Add a global rule that applies to all record types.
    void addGlobalRule(std::shared_ptr<ValidationRule> rule);

    /// Validate a record against all applicable rules.
    /// Runs global rules first, then record-specific rules.
    ValidationResult validate(const std::string& recordName,
                              const RecordBase& record) const;

    /// Get all rules registered for a record type (including global).
    std::vector<std::shared_ptr<ValidationRule>>
    getRules(const std::string& recordName) const;

    /// Remove all rules for a record type.
    void clearRules(const std::string& recordName);

    /// Remove all rules (global and scoped).
    void clearAll();

    /// Number of rules registered for a record type (including global).
    size_t ruleCount(const std::string& recordName) const;

    /// Format a validation result as a human-readable string.
    static std::string formatResult(const ValidationResult& result);

    /// Format a validation result as JSON.
    static std::string formatResultJson(const ValidationResult& result);

private:
    std::unordered_map<std::string,
        std::vector<std::shared_ptr<ValidationRule>>> rules_;
    std::vector<std::shared_ptr<ValidationRule>> globalRules_;
};

} // namespace copybook::validation
