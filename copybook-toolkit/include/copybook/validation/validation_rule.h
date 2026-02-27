#pragma once

#include <copybook/core/record_base.h>

#include <functional>
#include <memory>
#include <regex>
#include <string>
#include <vector>

namespace copybook::validation {

// ── Severity levels ───────────────────────────────────────────────────────

enum class Severity { ERROR, WARNING, INFO };

inline std::string severityName(Severity s) {
    switch (s) {
        case Severity::ERROR:   return "ERROR";
        case Severity::WARNING: return "WARNING";
        case Severity::INFO:    return "INFO";
    }
    return "UNKNOWN";
}

// ── Validation result ─────────────────────────────────────────────────────

struct ValidationError {
    std::string field_name;   // Empty for record-level rules
    std::string rule_name;
    std::string message;
    Severity    severity;
};

struct ValidationResult {
    bool valid = true;
    std::vector<ValidationError> errors;

    void addError(const std::string& field, const std::string& rule,
                  const std::string& msg, Severity sev = Severity::ERROR) {
        if (sev == Severity::ERROR) valid = false;
        errors.push_back({field, rule, msg, sev});
    }

    size_t errorCount() const {
        size_t n = 0;
        for (const auto& e : errors)
            if (e.severity == Severity::ERROR) n++;
        return n;
    }

    size_t warningCount() const {
        size_t n = 0;
        for (const auto& e : errors)
            if (e.severity == Severity::WARNING) n++;
        return n;
    }

    size_t infoCount() const {
        size_t n = 0;
        for (const auto& e : errors)
            if (e.severity == Severity::INFO) n++;
        return n;
    }
};

// ── Base rule interface ───────────────────────────────────────────────────

class ValidationRule {
public:
    virtual ~ValidationRule() = default;
    virtual void validate(const RecordBase& record, ValidationResult& result) const = 0;
    virtual std::string name() const = 0;
};

// ── Field-level rules ─────────────────────────────────────────────────────

/// Field must not be empty (all spaces).
class RequiredRule : public ValidationRule {
public:
    RequiredRule(std::string field, Severity sev = Severity::ERROR)
        : field_(std::move(field)), severity_(sev) {}

    void validate(const RecordBase& record, ValidationResult& result) const override {
        if (!record.hasField(field_)) return;
        std::string val = record.getData(field_);
        bool empty = val.find_first_not_of(' ') == std::string::npos;
        if (empty) {
            result.addError(field_, name(),
                field_ + " is required but empty", severity_);
        }
    }
    std::string name() const override { return "required"; }
private:
    std::string field_;
    Severity severity_;
};

/// Numeric field must be within [min, max].
class RangeRule : public ValidationRule {
public:
    RangeRule(std::string field, long min, long max,
             Severity sev = Severity::ERROR)
        : field_(std::move(field)), min_(min), max_(max), severity_(sev) {}

    void validate(const RecordBase& record, ValidationResult& result) const override {
        if (!record.hasField(field_)) return;
        std::string raw = record.getData(field_);
        if (raw.find_first_not_of(' ') == std::string::npos) return; // Skip empty

        try {
            long val = record.getNumeric(field_);
            if (val < min_ || val > max_) {
                result.addError(field_, name(),
                    field_ + " value " + std::to_string(val) +
                    " outside range [" + std::to_string(min_) +
                    ", " + std::to_string(max_) + "]", severity_);
            }
        } catch (...) {
            result.addError(field_, name(),
                field_ + " is not a valid number", severity_);
        }
    }
    std::string name() const override { return "range"; }
private:
    std::string field_;
    long min_, max_;
    Severity severity_;
};

/// Field value must match a regex pattern.
class PatternRule : public ValidationRule {
public:
    PatternRule(std::string field, const std::string& pattern,
                std::string description = "",
                Severity sev = Severity::ERROR)
        : field_(std::move(field)), pattern_(pattern),
          patternStr_(pattern),
          description_(description.empty() ? pattern : std::move(description)),
          severity_(sev) {}

    void validate(const RecordBase& record, ValidationResult& result) const override {
        if (!record.hasField(field_)) return;
        std::string val = record.getData(field_);
        // Trim trailing spaces for pattern matching
        auto end = val.find_last_not_of(' ');
        std::string trimmed = (end != std::string::npos) ? val.substr(0, end + 1) : "";
        if (trimmed.empty()) return; // Skip empty (use RequiredRule for that)

        if (!std::regex_match(trimmed, pattern_)) {
            result.addError(field_, name(),
                field_ + " value \"" + trimmed +
                "\" does not match pattern: " + description_, severity_);
        }
    }
    std::string name() const override { return "pattern"; }
private:
    std::string field_;
    std::regex pattern_;
    std::string patternStr_;
    std::string description_;
    Severity severity_;
};

/// Field trimmed length must be within [min, max].
class LengthRule : public ValidationRule {
public:
    LengthRule(std::string field, size_t minLen, size_t maxLen,
               Severity sev = Severity::ERROR)
        : field_(std::move(field)), min_(minLen), max_(maxLen), severity_(sev) {}

    void validate(const RecordBase& record, ValidationResult& result) const override {
        if (!record.hasField(field_)) return;
        std::string val = record.getData(field_);
        auto end = val.find_last_not_of(' ');
        size_t len = (end != std::string::npos) ? end + 1 : 0;
        if (len == 0) return; // Skip empty

        if (len < min_ || len > max_) {
            result.addError(field_, name(),
                field_ + " length " + std::to_string(len) +
                " outside range [" + std::to_string(min_) +
                ", " + std::to_string(max_) + "]", severity_);
        }
    }
    std::string name() const override { return "length"; }
private:
    std::string field_;
    size_t min_, max_;
    Severity severity_;
};

/// Field value must be one of the allowed values.
class EnumRule : public ValidationRule {
public:
    EnumRule(std::string field, std::vector<std::string> allowed,
             Severity sev = Severity::ERROR)
        : field_(std::move(field)), allowed_(std::move(allowed)), severity_(sev) {}

    void validate(const RecordBase& record, ValidationResult& result) const override {
        if (!record.hasField(field_)) return;
        std::string val = record.getData(field_);
        auto end = val.find_last_not_of(' ');
        std::string trimmed = (end != std::string::npos) ? val.substr(0, end + 1) : "";
        if (trimmed.empty()) return;

        for (const auto& a : allowed_) {
            if (trimmed == a) return;
        }

        std::string allowedStr;
        for (size_t i = 0; i < allowed_.size(); i++) {
            if (i > 0) allowedStr += ", ";
            allowedStr += "\"" + allowed_[i] + "\"";
        }
        result.addError(field_, name(),
            field_ + " value \"" + trimmed +
            "\" not in allowed values: [" + allowedStr + "]", severity_);
    }
    std::string name() const override { return "enum"; }
private:
    std::string field_;
    std::vector<std::string> allowed_;
    Severity severity_;
};

// ── Record-level rule (custom lambda) ─────────────────────────────────────

/// Custom validation rule using a lambda.
class CustomRule : public ValidationRule {
public:
    using ValidatorFn = std::function<void(const RecordBase&, ValidationResult&)>;

    CustomRule(std::string ruleName, ValidatorFn fn)
        : name_(std::move(ruleName)), fn_(std::move(fn)) {}

    void validate(const RecordBase& record, ValidationResult& result) const override {
        fn_(record, result);
    }
    std::string name() const override { return name_; }
private:
    std::string name_;
    ValidatorFn fn_;
};

} // namespace copybook::validation
