#include <gtest/gtest.h>

#include <copybook/validation/validation_engine.h>
#include <copybook/validation/validation_rule.h>
#include <copybook/parser/copybook_parser.h>
#include <copybook/transport/record_transport.h>

using namespace copybook;
using namespace copybook::validation;
using namespace copybook::transport;

// ── Test fixture: creates a PERSON record for validation ──────────────────

class ValidationTest : public ::testing::Test {
protected:
    void SetUp() override {
        CopybookParser parser;
        auto def = parser.parseString(R"(
           01  PERSON.
               05  ID              PIC X(12).
               05  FIRST-NAME      PIC X(32).
               05  LAST-NAME       PIC X(32).
               05  AGE             PIC 9(3).
               05  SEX             PIC X(1).
               05  DATE-OF-BIRTH   PIC X(10).
               05  STATUS          PIC X(1).
        )");
        registry_.registerCopybook(def);
        record_ = registry_.createBlankRecord("PERSON");
    }

    RecordRegistry registry_;
    std::shared_ptr<RecordBase> record_;
};

// ═══════════════════════════════════════════════════════════════════════════
// RequiredRule Tests
// ═══════════════════════════════════════════════════════════════════════════

TEST_F(ValidationTest, RequiredPasses) {
    record_->setData("ID", "EMP-001");
    RequiredRule rule("ID");
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
    EXPECT_EQ(result.errors.size(), 0u);
}

TEST_F(ValidationTest, RequiredFails) {
    // ID is blank (all spaces)
    RequiredRule rule("ID");
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_FALSE(result.valid);
    EXPECT_EQ(result.errors.size(), 1u);
    EXPECT_EQ(result.errors[0].rule_name, "required");
    EXPECT_EQ(result.errors[0].field_name, "ID");
}

TEST_F(ValidationTest, RequiredWarning) {
    RequiredRule rule("ID", Severity::WARNING);
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);  // Warning doesn't fail validation
    EXPECT_EQ(result.warningCount(), 1u);
}

// ═══════════════════════════════════════════════════════════════════════════
// RangeRule Tests
// ═══════════════════════════════════════════════════════════════════════════

TEST_F(ValidationTest, RangeInBounds) {
    record_->setData("AGE", "060");
    RangeRule rule("AGE", 0, 150);
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

TEST_F(ValidationTest, RangeAtBoundary) {
    record_->setData("AGE", "000");
    RangeRule rule("AGE", 0, 150);
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

TEST_F(ValidationTest, RangeOutOfBounds) {
    record_->setData("AGE", "200");
    RangeRule rule("AGE", 0, 150);
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_FALSE(result.valid);
    EXPECT_EQ(result.errors[0].rule_name, "range");
}

TEST_F(ValidationTest, RangeSkipsEmpty) {
    // AGE is all spaces — range rule should skip
    RangeRule rule("AGE", 0, 150);
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

// ═══════════════════════════════════════════════════════════════════════════
// PatternRule Tests
// ═══════════════════════════════════════════════════════════════════════════

TEST_F(ValidationTest, PatternMatches) {
    record_->setData("DATE_OF_BIRTH", "1965-03-15");
    PatternRule rule("DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD");
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

TEST_F(ValidationTest, PatternFails) {
    record_->setData("DATE_OF_BIRTH", "March 15");
    PatternRule rule("DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD");
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_FALSE(result.valid);
    EXPECT_NE(result.errors[0].message.find("YYYY-MM-DD"), std::string::npos);
}

TEST_F(ValidationTest, PatternSkipsEmpty) {
    PatternRule rule("DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})");
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

// ═══════════════════════════════════════════════════════════════════════════
// LengthRule Tests
// ═══════════════════════════════════════════════════════════════════════════

TEST_F(ValidationTest, LengthInRange) {
    record_->setData("FIRST_NAME", "Thomas");
    LengthRule rule("FIRST_NAME", 1, 32);
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

TEST_F(ValidationTest, LengthTooShort) {
    record_->setData("FIRST_NAME", "A");
    LengthRule rule("FIRST_NAME", 2, 32);
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_FALSE(result.valid);
}

TEST_F(ValidationTest, LengthSkipsEmpty) {
    LengthRule rule("FIRST_NAME", 2, 32);
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

// ═══════════════════════════════════════════════════════════════════════════
// EnumRule Tests
// ═══════════════════════════════════════════════════════════════════════════

TEST_F(ValidationTest, EnumValid) {
    record_->setData("SEX", "M");
    EnumRule rule("SEX", {"M", "F", "X"});
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

TEST_F(ValidationTest, EnumInvalid) {
    record_->setData("SEX", "Z");
    EnumRule rule("SEX", {"M", "F", "X"});
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_FALSE(result.valid);
    EXPECT_NE(result.errors[0].message.find("\"M\""), std::string::npos);
}

TEST_F(ValidationTest, EnumSkipsEmpty) {
    EnumRule rule("SEX", {"M", "F"});
    ValidationResult result;
    rule.validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

// ═══════════════════════════════════════════════════════════════════════════
// CustomRule Tests (cross-field validation)
// ═══════════════════════════════════════════════════════════════════════════

TEST_F(ValidationTest, CustomRulePasses) {
    record_->setData("STATUS", "A");
    record_->setData("ID", "EMP-001");

    auto rule = std::make_shared<CustomRule>("active-needs-id",
        [](const RecordBase& rec, ValidationResult& res) {
            std::string status = rec.getData("STATUS");
            auto end = status.find_last_not_of(' ');
            std::string trimStatus = (end != std::string::npos)
                                     ? status.substr(0, end + 1) : "";
            if (trimStatus == "A") {
                std::string id = rec.getData("ID");
                if (id.find_first_not_of(' ') == std::string::npos) {
                    res.addError("", "active-needs-id",
                        "Active records must have an ID");
                }
            }
        });

    ValidationResult result;
    rule->validate(*record_, result);
    EXPECT_TRUE(result.valid);
}

TEST_F(ValidationTest, CustomRuleFails) {
    record_->setData("STATUS", "A");
    // ID is blank

    auto rule = std::make_shared<CustomRule>("active-needs-id",
        [](const RecordBase& rec, ValidationResult& res) {
            std::string status = rec.getData("STATUS");
            auto end = status.find_last_not_of(' ');
            std::string trimStatus = (end != std::string::npos)
                                     ? status.substr(0, end + 1) : "";
            if (trimStatus == "A") {
                std::string id = rec.getData("ID");
                if (id.find_first_not_of(' ') == std::string::npos) {
                    res.addError("", "active-needs-id",
                        "Active records must have an ID");
                }
            }
        });

    ValidationResult result;
    rule->validate(*record_, result);
    EXPECT_FALSE(result.valid);
}

// ═══════════════════════════════════════════════════════════════════════════
// ValidationEngine Tests
// ═══════════════════════════════════════════════════════════════════════════

TEST_F(ValidationTest, EngineAllPass) {
    record_->setData("ID", "EMP-001");
    record_->setData("FIRST_NAME", "Thomas");
    record_->setData("AGE", "060");
    record_->setData("SEX", "M");
    record_->setData("DATE_OF_BIRTH", "1965-03-15");

    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("FIRST_NAME"));
    engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
    engine.addRule("PERSON", std::make_shared<EnumRule>("SEX", std::vector<std::string>{"M", "F"}));
    engine.addRule("PERSON", std::make_shared<PatternRule>(
        "DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD"));

    auto result = engine.validate("PERSON", *record_);
    EXPECT_TRUE(result.valid);
    EXPECT_EQ(result.errorCount(), 0u);
}

TEST_F(ValidationTest, EngineMultipleFailures) {
    // All fields empty, multiple rules fail
    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("FIRST_NAME"));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("LAST_NAME"));

    auto result = engine.validate("PERSON", *record_);
    EXPECT_FALSE(result.valid);
    EXPECT_EQ(result.errorCount(), 3u);
}

TEST_F(ValidationTest, EngineGlobalRules) {
    ValidationEngine engine;
    engine.addGlobalRule(std::make_shared<RequiredRule>("ID"));

    auto result = engine.validate("PERSON", *record_);
    EXPECT_FALSE(result.valid);

    // Global rule should also apply to other record types
    auto result2 = engine.validate("ACCOUNT", *record_);
    EXPECT_FALSE(result2.valid);
}

TEST_F(ValidationTest, EngineMixedSeverity) {
    record_->setData("ID", "EMP-001");
    // AGE is empty → warning, FIRST_NAME empty → error

    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("FIRST_NAME", Severity::ERROR));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("AGE", Severity::WARNING));

    auto result = engine.validate("PERSON", *record_);
    EXPECT_FALSE(result.valid);  // Has error
    EXPECT_EQ(result.errorCount(), 1u);
    EXPECT_EQ(result.warningCount(), 1u);
}

TEST_F(ValidationTest, EngineRuleCount) {
    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
    engine.addGlobalRule(std::make_shared<RequiredRule>("ID"));

    EXPECT_EQ(engine.ruleCount("PERSON"), 3u);
    EXPECT_EQ(engine.ruleCount("ACCOUNT"), 1u); // only global
}

TEST_F(ValidationTest, EngineClearRules) {
    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addGlobalRule(std::make_shared<RequiredRule>("ID"));

    engine.clearRules("PERSON");
    EXPECT_EQ(engine.ruleCount("PERSON"), 1u); // only global remains

    engine.clearAll();
    EXPECT_EQ(engine.ruleCount("PERSON"), 0u);
}

TEST_F(ValidationTest, EngineWithCrossFieldRule) {
    record_->setData("STATUS", "A");
    record_->setData("ID", "EMP-001");
    record_->setData("FIRST_NAME", "Thomas");

    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<CustomRule>("active-needs-name",
        [](const RecordBase& rec, ValidationResult& res) {
            std::string status = rec.getData("STATUS");
            auto end = status.find_last_not_of(' ');
            std::string s = (end != std::string::npos) ? status.substr(0, end + 1) : "";
            if (s == "A") {
                std::string name = rec.getData("FIRST_NAME");
                if (name.find_first_not_of(' ') == std::string::npos) {
                    res.addError("FIRST_NAME", "active-needs-name",
                        "Active persons must have a first name");
                }
            }
        }));

    auto result = engine.validate("PERSON", *record_);
    EXPECT_TRUE(result.valid);
}

// ═══════════════════════════════════════════════════════════════════════════
// Formatting Tests
// ═══════════════════════════════════════════════════════════════════════════

TEST_F(ValidationTest, FormatResultText) {
    ValidationResult result;
    result.addError("AGE", "range", "AGE value 200 outside range [0, 150]");
    result.addError("SEX", "enum", "SEX value \"Z\" not in allowed values", Severity::WARNING);

    std::string text = ValidationEngine::formatResult(result);
    EXPECT_NE(text.find("FAILED"), std::string::npos);
    EXPECT_NE(text.find("[ERROR]"), std::string::npos);
    EXPECT_NE(text.find("[WARNING]"), std::string::npos);
    EXPECT_NE(text.find("AGE"), std::string::npos);
}

TEST_F(ValidationTest, FormatResultJson) {
    ValidationResult result;
    result.addError("ID", "required", "ID is required but empty");

    std::string json = ValidationEngine::formatResultJson(result);
    EXPECT_NE(json.find("\"valid\": false"), std::string::npos);
    EXPECT_NE(json.find("\"error_count\": 1"), std::string::npos);
    EXPECT_NE(json.find("\"field\": \"ID\""), std::string::npos);
    EXPECT_NE(json.find("\"rule\": \"required\""), std::string::npos);
}

TEST_F(ValidationTest, FormatPassingResult) {
    ValidationResult result;
    std::string text = ValidationEngine::formatResult(result);
    EXPECT_NE(text.find("PASSED"), std::string::npos);
}
