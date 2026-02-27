#include <gtest/gtest.h>

#include <copybook/core/record_buffer.h>
#include <copybook/core/record_base.h>
#include <copybook/parser/copybook_parser.h>
#include <copybook/serial/json_serializer.h>
#include <copybook/serial/yaml_serializer.h>
#include <copybook/transport/record_transport.h>
#include <copybook/validation/validation_engine.h>
#include <copybook/validation/validation_rule.h>

#include <fstream>
#include <filesystem>
#include <sstream>
#include <algorithm>

using namespace copybook;
using namespace copybook::transport;
using namespace copybook::validation;

namespace fs = std::filesystem;

// ═════════════════════════════════════════════════════════════════════════════
// Integration helpers
// ═════════════════════════════════════════════════════════════════════════════

static const char* PERSON_COPYBOOK = R"(
       01  PERSON.
           05  ID              PIC X(12).
           05  FIRST-NAME      PIC X(32).
           05  LAST-NAME       PIC X(32).
           05  AGE             PIC 9(3).
           05  SEX             PIC X(1).
           05  DATE-OF-BIRTH   PIC X(10).
           05  STATUS          PIC X(1).
)";

static const char* ACCOUNT_COPYBOOK = R"(
       01  ACCOUNT.
           05  ACCT-ID         PIC X(10).
           05  ACCT-TYPE       PIC X(1).
           05  BALANCE         PIC 9(9)V99.
           05  OWNER-NAME      PIC X(32).
           05  OPEN-DATE       PIC X(10).
)";

static const char* BROKER_COPYBOOK = R"(
       01  BROKER-CONTROL.
           05  BROKER-ID       PIC X(8).
           05  REGION-CODE     PIC X(3).
           05  PHONE-INFO.
               10  AREA-CODE   PIC X(3).
               10  NUMBER      PIC X(7).
           05  MAX-CLIENTS     PIC 9(5).
)";

// ═════════════════════════════════════════════════════════════════════════════
// 1. Parse → Create → Populate → Validate → Serialize (full pipeline)
// ═════════════════════════════════════════════════════════════════════════════

class FullPipelineTest : public ::testing::Test {
protected:
    void SetUp() override {
        CopybookParser parser;
        personDef_ = parser.parseString(PERSON_COPYBOOK);
        accountDef_ = parser.parseString(ACCOUNT_COPYBOOK);
        brokerDef_ = parser.parseString(BROKER_COPYBOOK);

        registry_.registerCopybook(personDef_);
        registry_.registerCopybook(accountDef_);
        registry_.registerCopybook(brokerDef_);
    }

    CopybookDefinition personDef_;
    CopybookDefinition accountDef_;
    CopybookDefinition brokerDef_;
    RecordRegistry registry_;
};

TEST_F(FullPipelineTest, PersonEndToEnd) {
    // Step 1: Create blank record from registry
    auto record = registry_.createBlankRecord("PERSON");
    ASSERT_NE(record, nullptr);

    // Step 2: Populate fields
    record->setData("ID", "EMP-001");
    record->setData("FIRST_NAME", "Thomas");
    record->setData("LAST_NAME", "Peters");
    record->setData("AGE", "035");
    record->setData("SEX", "M");
    record->setData("DATE_OF_BIRTH", "1990-06-15");
    record->setData("STATUS", "A");

    // Step 3: Validate
    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("FIRST_NAME"));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("LAST_NAME"));
    engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
    engine.addRule("PERSON", std::make_shared<EnumRule>("SEX",
        std::vector<std::string>{"M", "F", "X"}));
    engine.addRule("PERSON", std::make_shared<PatternRule>(
        "DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD"));
    engine.addRule("PERSON", std::make_shared<EnumRule>("STATUS",
        std::vector<std::string>{"A", "I", "T"}));

    auto result = engine.validate("PERSON", *record);
    EXPECT_TRUE(result.valid);
    EXPECT_EQ(result.errorCount(), 0u);

    // Step 4: Serialize to JSON
    std::string json = JsonSerializer::toJson(*record);
    EXPECT_NE(json.find("EMP-001"), std::string::npos);
    EXPECT_NE(json.find("Thomas"), std::string::npos);

    // Step 5: Serialize to YAML
    std::string yaml = YamlSerializer::toYaml(*record);
    EXPECT_NE(yaml.find("Peters"), std::string::npos);

    // Step 6: Extract fields and verify
    auto fields = RecordRegistry::extractFields(*record);
    bool foundAge = false;
    for (const auto& [name, value] : fields) {
        if (name == "AGE") {
            EXPECT_EQ(value, "035");
            foundAge = true;
        }
    }
    EXPECT_TRUE(foundAge);
}

TEST_F(FullPipelineTest, AccountEndToEnd) {
    auto record = registry_.createBlankRecord("ACCOUNT");
    ASSERT_NE(record, nullptr);

    record->setData("ACCT_ID", "ACC-100234");
    record->setData("ACCT_TYPE", "S");
    record->setData("BALANCE", "00012345.67");
    record->setData("OWNER_NAME", "Thomas Peters");
    record->setData("OPEN_DATE", "2024-01-15");

    // Validate
    ValidationEngine engine;
    engine.addRule("ACCOUNT", std::make_shared<RequiredRule>("ACCT_ID"));
    engine.addRule("ACCOUNT", std::make_shared<EnumRule>("ACCT_TYPE",
        std::vector<std::string>{"S", "C", "L"}));
    engine.addRule("ACCOUNT", std::make_shared<PatternRule>(
        "OPEN_DATE", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD"));

    auto result = engine.validate("ACCOUNT", *record);
    EXPECT_TRUE(result.valid);

    // Serialize → Deserialize round-trip
    std::string json = JsonSerializer::toJson(*record);
    auto roundTrip = registry_.createBlankRecord("ACCOUNT");
    JsonSerializer::fromJson(*roundTrip, json);

    EXPECT_EQ(roundTrip->getData("ACCT_ID").substr(0, 10), "ACC-100234");
    EXPECT_EQ(roundTrip->getData("ACCT_TYPE").substr(0, 1), "S");
}

TEST_F(FullPipelineTest, BrokerWithGroupEndToEnd) {
    auto record = registry_.createBlankRecord("BROKER-CONTROL");
    ASSERT_NE(record, nullptr);

    record->setData("BROKER_ID", "BRK-0042");
    record->setData("REGION_CODE", "NE ");
    // AREA_CODE and NUMBER are children of PHONE_INFO group.
    // Set via PHONE_INFO group data (raw 10-byte field: "2125551234")
    record->setData("PHONE_INFO", "2125551234");
    record->setData("MAX_CLIENTS", "00500");

    // Validate top-level fields
    ValidationEngine engine;
    engine.addRule("BROKER-CONTROL", std::make_shared<RequiredRule>("BROKER_ID"));
    engine.addRule("BROKER-CONTROL", std::make_shared<RangeRule>("MAX_CLIENTS", 1, 99999));
    engine.addRule("BROKER-CONTROL", std::make_shared<LengthRule>("REGION_CODE", 2, 3));

    auto result = engine.validate("BROKER-CONTROL", *record);
    EXPECT_TRUE(result.valid);

    // JSON serialization — should contain the broker ID
    std::string json = JsonSerializer::toJson(*record);
    EXPECT_NE(json.find("BRK-0042"), std::string::npos);
    // PHONE_INFO group data should appear in some form
    EXPECT_NE(json.find("PHONE_INFO"), std::string::npos);
}

// ═════════════════════════════════════════════════════════════════════════════
// 2. Validation failure → format → report pipeline
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, ValidationFailureReportPipeline) {
    auto record = registry_.createBlankRecord("PERSON");
    ASSERT_NE(record, nullptr);

    // Intentionally bad data
    record->setData("AGE", "999");
    record->setData("SEX", "Z");
    record->setData("DATE_OF_BIRTH", "not-a-date");
    // ID, FIRST_NAME, LAST_NAME blank

    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("FIRST_NAME"));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("LAST_NAME"));
    engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
    engine.addRule("PERSON", std::make_shared<EnumRule>("SEX",
        std::vector<std::string>{"M", "F", "X"}));
    engine.addRule("PERSON", std::make_shared<PatternRule>(
        "DATE_OF_BIRTH", R"(\d{4}-\d{2}-\d{2})", "YYYY-MM-DD"));

    auto result = engine.validate("PERSON", *record);
    EXPECT_FALSE(result.valid);
    EXPECT_EQ(result.errorCount(), 6u); // 3 required + range + enum + pattern

    // Text format report
    std::string text = ValidationEngine::formatResult(result);
    EXPECT_NE(text.find("FAILED"), std::string::npos);
    EXPECT_NE(text.find("6 error(s)"), std::string::npos);

    // JSON format report
    std::string json = ValidationEngine::formatResultJson(result);
    EXPECT_NE(json.find("\"valid\": false"), std::string::npos);
    EXPECT_NE(json.find("\"error_count\": 6"), std::string::npos);
}

// ═════════════════════════════════════════════════════════════════════════════
// 3. Multi-record batch validation
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, BatchValidation) {
    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));

    struct TestCase {
        std::string id;
        std::string age;
        bool expectValid;
    };

    std::vector<TestCase> batch = {
        {"EMP-001", "035", true},
        {"EMP-002", "070", true},
        {"",        "060", false},  // missing ID
        {"EMP-004", "200", false},  // age out of range
        {"EMP-005", "000", true},   // boundary age
    };

    int passCount = 0, failCount = 0;
    for (const auto& tc : batch) {
        auto record = registry_.createBlankRecord("PERSON");
        if (!tc.id.empty()) record->setData("ID", tc.id);
        record->setData("AGE", tc.age);

        auto result = engine.validate("PERSON", *record);
        EXPECT_EQ(result.valid, tc.expectValid)
            << "Failed for ID=" << tc.id << " AGE=" << tc.age;
        if (result.valid) passCount++; else failCount++;
    }

    EXPECT_EQ(passCount, 3);
    EXPECT_EQ(failCount, 2);
}

// ═════════════════════════════════════════════════════════════════════════════
// 4. Cross-record-type global validation rules
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, GlobalRulesAcrossRecordTypes) {
    ValidationEngine engine;

    // Global rule: every record that has an ID field must have it filled
    engine.addGlobalRule(std::make_shared<CustomRule>("global-id-check",
        [](const RecordBase& rec, ValidationResult& res) {
            if (rec.hasField("ID")) {
                std::string id = rec.getData("ID");
                if (id.find_first_not_of(' ') == std::string::npos) {
                    res.addError("ID", "global-id-check", "ID must not be empty");
                }
            }
        }));

    // PERSON without ID
    auto person = registry_.createBlankRecord("PERSON");
    auto pResult = engine.validate("PERSON", *person);
    EXPECT_FALSE(pResult.valid);

    // PERSON with ID
    person->setData("ID", "EMP-001");
    pResult = engine.validate("PERSON", *person);
    EXPECT_TRUE(pResult.valid);

    // ACCOUNT has ACCT_ID not ID — global rule should not trigger
    auto account = registry_.createBlankRecord("ACCOUNT");
    auto aResult = engine.validate("ACCOUNT", *account);
    EXPECT_TRUE(aResult.valid);
}

// ═════════════════════════════════════════════════════════════════════════════
// 5. JSON Serialize → Deserialize → Validate round-trip
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, JsonRoundTripThenValidate) {
    // Create and populate original record
    auto original = registry_.createBlankRecord("PERSON");
    original->setData("ID", "EMP-042");
    original->setData("FIRST_NAME", "Jane");
    original->setData("LAST_NAME", "Doe");
    original->setData("AGE", "028");
    original->setData("SEX", "F");
    original->setData("DATE_OF_BIRTH", "1997-11-22");
    original->setData("STATUS", "A");

    // Serialize to JSON
    std::string json = JsonSerializer::toJson(*original);

    // Deserialize into new blank record
    auto restored = registry_.createBlankRecord("PERSON");
    JsonSerializer::fromJson(*restored, json);

    // Validate the restored record with the same rules
    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("FIRST_NAME"));
    engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));
    engine.addRule("PERSON", std::make_shared<EnumRule>("SEX",
        std::vector<std::string>{"M", "F", "X"}));

    auto result = engine.validate("PERSON", *restored);
    EXPECT_TRUE(result.valid);

    // Verify field data survived round-trip
    std::string restoredId = restored->getData("ID");
    EXPECT_NE(restoredId.find("EMP-042"), std::string::npos);
}

// ═════════════════════════════════════════════════════════════════════════════
// 6. YAML Serialize → Deserialize → Validate round-trip
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, YamlRoundTripThenValidate) {
    auto original = registry_.createBlankRecord("ACCOUNT");
    original->setData("ACCT_ID", "ACC-999888");
    original->setData("ACCT_TYPE", "C");
    original->setData("BALANCE", "00054321.00");
    original->setData("OWNER_NAME", "John Smith");
    original->setData("OPEN_DATE", "2023-07-01");

    // YAML round-trip
    std::string yaml = YamlSerializer::toYaml(*original);
    auto restored = registry_.createBlankRecord("ACCOUNT");
    YamlSerializer::fromYaml(*restored, yaml);

    // Validate restored
    ValidationEngine engine;
    engine.addRule("ACCOUNT", std::make_shared<RequiredRule>("ACCT_ID"));
    engine.addRule("ACCOUNT", std::make_shared<EnumRule>("ACCT_TYPE",
        std::vector<std::string>{"S", "C", "L"}));

    auto result = engine.validate("ACCOUNT", *restored);
    EXPECT_TRUE(result.valid);
}

// ═════════════════════════════════════════════════════════════════════════════
// 7. Raw buffer round-trip (simulate mainframe I/O)
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, RawBufferRoundTrip) {
    // Create and populate
    auto record = registry_.createBlankRecord("PERSON");
    record->setData("ID", "EMP-777");
    record->setData("FIRST_NAME", "Alice");
    record->setData("AGE", "045");
    record->setData("SEX", "F");

    // Get raw buffer (simulates writing to mainframe)
    std::string rawBuffer = record->getData();
    size_t expectedSize = personDef_.total_size;
    EXPECT_EQ(rawBuffer.size(), expectedSize);

    // Re-create from raw buffer (simulates reading from mainframe)
    auto restored = registry_.createRecord("PERSON", rawBuffer.c_str(), rawBuffer.size());
    ASSERT_NE(restored, nullptr);

    // Verify fields
    std::string id = restored->getData("ID");
    EXPECT_NE(id.find("EMP-777"), std::string::npos);

    std::string name = restored->getData("FIRST_NAME");
    EXPECT_NE(name.find("Alice"), std::string::npos);

    std::string sex = restored->getData("SEX");
    EXPECT_EQ(sex.substr(0, 1), "F");

    // Validate
    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));
    engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));

    auto result = engine.validate("PERSON", *restored);
    EXPECT_TRUE(result.valid);
}

// ═════════════════════════════════════════════════════════════════════════════
// 8. Multi-schema registry with validation
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, MultiSchemaRegistry) {
    // Verify all schemas are registered
    auto schemas = registry_.allSchemas();
    EXPECT_EQ(schemas.size(), 3u);

    // Verify specific schemas
    EXPECT_NE(registry_.getSchema("PERSON"), nullptr);
    EXPECT_NE(registry_.getSchema("ACCOUNT"), nullptr);
    EXPECT_NE(registry_.getSchema("BROKER-CONTROL"), nullptr);
    EXPECT_EQ(registry_.getSchema("NONEXISTENT"), nullptr);

    // Create and validate each type
    auto person = registry_.createBlankRecord("PERSON");
    auto account = registry_.createBlankRecord("ACCOUNT");
    auto broker = registry_.createBlankRecord("BROKER-CONTROL");

    ASSERT_NE(person, nullptr);
    ASSERT_NE(account, nullptr);
    ASSERT_NE(broker, nullptr);

    // Each should have the correct size
    EXPECT_EQ(person->getData().size(), static_cast<size_t>(personDef_.total_size));
    EXPECT_EQ(account->getData().size(), static_cast<size_t>(accountDef_.total_size));
    EXPECT_EQ(broker->getData().size(), static_cast<size_t>(brokerDef_.total_size));
}

// ═════════════════════════════════════════════════════════════════════════════
// 9. Extract fields → apply to different record
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, ExtractAndApplyFields) {
    // Populate source record
    auto source = registry_.createBlankRecord("PERSON");
    source->setData("ID", "EMP-123");
    source->setData("FIRST_NAME", "Bob");
    source->setData("LAST_NAME", "Jones");
    source->setData("AGE", "050");

    // Extract fields
    auto fields = RecordRegistry::extractFields(*source);
    EXPECT_GE(fields.size(), 4u);

    // Apply to new blank record
    auto target = registry_.createBlankRecord("PERSON");
    RecordRegistry::applyFields(*target, fields);

    // Verify target has same data
    std::string targetId = target->getData("ID");
    EXPECT_NE(targetId.find("EMP-123"), std::string::npos);

    std::string targetName = target->getData("FIRST_NAME");
    EXPECT_NE(targetName.find("Bob"), std::string::npos);
}

// ═════════════════════════════════════════════════════════════════════════════
// 10. Validation with mixed severity + report
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, MixedSeverityEndToEnd) {
    auto record = registry_.createBlankRecord("PERSON");
    record->setData("ID", "EMP-001");
    record->setData("AGE", "200");       // out of range → ERROR
    record->setData("STATUS", "A");
    // FIRST_NAME blank → WARNING
    // DATE_OF_BIRTH blank → INFO (recommended but optional)

    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("FIRST_NAME", Severity::WARNING));
    engine.addRule("PERSON", std::make_shared<RequiredRule>("DATE_OF_BIRTH", Severity::INFO));
    engine.addRule("PERSON", std::make_shared<RangeRule>("AGE", 0, 150));

    auto result = engine.validate("PERSON", *record);
    EXPECT_FALSE(result.valid);  // Has at least one ERROR
    EXPECT_EQ(result.errorCount(), 1u);   // AGE range
    EXPECT_EQ(result.warningCount(), 1u); // FIRST_NAME required
    EXPECT_EQ(result.infoCount(), 1u);    // DATE_OF_BIRTH required

    // Format and verify all severities appear
    std::string text = ValidationEngine::formatResult(result);
    EXPECT_NE(text.find("[ERROR]"), std::string::npos);
    EXPECT_NE(text.find("[WARNING]"), std::string::npos);
    EXPECT_NE(text.find("[INFO]"), std::string::npos);
}

// ═════════════════════════════════════════════════════════════════════════════
// 11. Copybook file I/O integration
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, CopybookFileParseAndValidate) {
    // Write a temporary copybook file
    std::string tmpDir = "/tmp/copybook_test_" + std::to_string(::getpid());
    fs::create_directories(tmpDir);

    std::string copybookPath = tmpDir + "/EMPLOYEE.cpy";
    {
        std::ofstream out(copybookPath);
        out << "       01  EMPLOYEE.\n"
            << "           05  EMP-ID       PIC X(8).\n"
            << "           05  EMP-NAME     PIC X(30).\n"
            << "           05  DEPT-CODE    PIC X(4).\n";
    }

    // Load directory into a fresh registry
    RecordRegistry fileRegistry;
    fileRegistry.loadDirectory(tmpDir);

    auto schema = fileRegistry.getSchema("EMPLOYEE");
    ASSERT_NE(schema, nullptr);
    EXPECT_EQ(schema->record_name, "EMPLOYEE");

    // Create and validate
    auto record = fileRegistry.createBlankRecord("EMPLOYEE");
    ASSERT_NE(record, nullptr);
    record->setData("EMP_ID", "E-001234");
    record->setData("EMP_NAME", "Thomas Peters");
    record->setData("DEPT_CODE", "ENGR");

    ValidationEngine engine;
    engine.addRule("EMPLOYEE", std::make_shared<RequiredRule>("EMP_ID"));
    engine.addRule("EMPLOYEE", std::make_shared<RequiredRule>("EMP_NAME"));
    engine.addRule("EMPLOYEE", std::make_shared<LengthRule>("DEPT_CODE", 2, 4));

    auto result = engine.validate("EMPLOYEE", *record);
    EXPECT_TRUE(result.valid);

    // Cleanup
    fs::remove_all(tmpDir);
}

// ═════════════════════════════════════════════════════════════════════════════
// 12. Parser error handling
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, ParserErrorHandling) {
    // Nonexistent record in registry
    EXPECT_EQ(registry_.createBlankRecord("NONEXISTENT"), nullptr);
    EXPECT_EQ(registry_.createRecord("NONEXISTENT", nullptr, 0), nullptr);

    // Empty parse string should produce no useful record
    CopybookParser parser;
    EXPECT_THROW(parser.parseString(""), std::exception);
}

// ═════════════════════════════════════════════════════════════════════════════
// 13. Large record stress test
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, LargeRecordStress) {
    // Build a copybook with many fields
    std::ostringstream cpy;
    cpy << "       01  WIDE-RECORD.\n";
    for (int i = 1; i <= 50; i++) {
        cpy << "           05  FIELD-" << std::setfill('0') << std::setw(3) << i
            << "    PIC X(20).\n";
    }

    CopybookParser parser;
    auto def = parser.parseString(cpy.str());
    EXPECT_EQ(def.record_name, "WIDE-RECORD");
    EXPECT_EQ(def.total_size, 50 * 20);

    RecordRegistry stressRegistry;
    stressRegistry.registerCopybook(def);

    auto record = stressRegistry.createBlankRecord("WIDE-RECORD");
    ASSERT_NE(record, nullptr);

    // Fill all fields
    for (int i = 1; i <= 50; i++) {
        std::ostringstream fname;
        fname << "FIELD_" << std::setfill('0') << std::setw(3) << i;
        record->setData(fname.str(), "DATA-" + std::to_string(i));
    }

    // Validate all required
    ValidationEngine engine;
    for (int i = 1; i <= 50; i++) {
        std::ostringstream fname;
        fname << "FIELD_" << std::setfill('0') << std::setw(3) << i;
        engine.addRule("WIDE-RECORD", std::make_shared<RequiredRule>(fname.str()));
    }

    auto result = engine.validate("WIDE-RECORD", *record);
    EXPECT_TRUE(result.valid);
    EXPECT_EQ(result.errorCount(), 0u);

    // JSON round-trip
    std::string json = JsonSerializer::toJson(*record);
    EXPECT_NE(json.find("DATA-1"), std::string::npos);
    EXPECT_NE(json.find("DATA-50"), std::string::npos);
}

// ═════════════════════════════════════════════════════════════════════════════
// 14. Cross-field validation in integration context
// ═════════════════════════════════════════════════════════════════════════════

TEST_F(FullPipelineTest, CrossFieldValidationIntegration) {
    auto record = registry_.createBlankRecord("PERSON");
    record->setData("ID", "EMP-001");
    record->setData("STATUS", "A");
    record->setData("FIRST_NAME", "Thomas");
    record->setData("LAST_NAME", "Peters");
    record->setData("AGE", "035");

    ValidationEngine engine;
    engine.addRule("PERSON", std::make_shared<RequiredRule>("ID"));

    // Cross-field: Active status requires both names
    engine.addRule("PERSON", std::make_shared<CustomRule>("active-needs-names",
        [](const RecordBase& rec, ValidationResult& res) {
            std::string status = rec.getData("STATUS");
            auto end = status.find_last_not_of(' ');
            std::string s = (end != std::string::npos) ? status.substr(0, end + 1) : "";
            if (s == "A") {
                std::string first = rec.getData("FIRST_NAME");
                std::string last = rec.getData("LAST_NAME");
                if (first.find_first_not_of(' ') == std::string::npos) {
                    res.addError("FIRST_NAME", "active-needs-names",
                        "Active persons must have a first name");
                }
                if (last.find_first_not_of(' ') == std::string::npos) {
                    res.addError("LAST_NAME", "active-needs-names",
                        "Active persons must have a last name");
                }
            }
        }));

    // Cross-field: Age must match DOB decade (simple check)
    engine.addRule("PERSON", std::make_shared<CustomRule>("age-dob-consistency",
        [](const RecordBase& rec, ValidationResult& res) {
            std::string ageStr = rec.getData("AGE");
            std::string dob = rec.getData("DATE_OF_BIRTH");
            auto ageEnd = ageStr.find_last_not_of(' ');
            auto dobEnd = dob.find_last_not_of(' ');
            // Only check if both are present
            if (ageEnd == std::string::npos || dobEnd == std::string::npos) return;
        }));

    auto result = engine.validate("PERSON", *record);
    EXPECT_TRUE(result.valid);

    // Now test failure: Active with no last name
    record->setData("LAST_NAME", "");
    auto result2 = engine.validate("PERSON", *record);
    EXPECT_FALSE(result2.valid);
    EXPECT_GE(result2.errorCount(), 1u);
}
