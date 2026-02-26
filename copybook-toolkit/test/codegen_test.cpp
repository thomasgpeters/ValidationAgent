#include <gtest/gtest.h>
#include <copybook/parser/copybook_parser.h>
#include <copybook/parser/codegen.h>

#include <algorithm>
#include <fstream>

using namespace copybook;

static const CopybookParser parser;
static const Codegen codegen;

// ═══════════════════════════════════════════════════════════════════════════
// Basic code generation
// ═══════════════════════════════════════════════════════════════════════════

static const char* SIMPLE_CPY = R"(
       01  SIMPLE-REC.
           05  NAME     PIC X(20).
           05  CODE     PIC X(5).
           05  AMOUNT   PIC 9(10).
)";

TEST(CodegenTest, GeneratesHeaderGuard) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("#pragma once"), std::string::npos);
}

TEST(CodegenTest, GeneratesAutoGenComment) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("Auto-generated from SIMPLE-REC"), std::string::npos);
    EXPECT_NE(code.find("DO NOT EDIT"), std::string::npos);
}

TEST(CodegenTest, IncludesRecordBase) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("#include <copybook/core/record_base.h>"), std::string::npos);
}

TEST(CodegenTest, GeneratesClassName) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("class SimpleRec : public RecordBase"), std::string::npos);
}

TEST(CodegenTest, GeneratesRecordSize) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("static constexpr int RECORD_SIZE = 35"), std::string::npos);
}

TEST(CodegenTest, GeneratesFieldSizeConstants) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("NAME_SIZE"), std::string::npos);
    EXPECT_NE(code.find("= 20"), std::string::npos);
    EXPECT_NE(code.find("CODE_SIZE"), std::string::npos);
    EXPECT_NE(code.find("= 5"), std::string::npos);
    EXPECT_NE(code.find("AMOUNT_SIZE"), std::string::npos);
    EXPECT_NE(code.find("= 10"), std::string::npos);
}

TEST(CodegenTest, GeneratesFieldOffsetConstants) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("NAME_OFFSET"), std::string::npos);
    EXPECT_NE(code.find("CODE_OFFSET"), std::string::npos);
    EXPECT_NE(code.find("AMOUNT_OFFSET"), std::string::npos);
}

TEST(CodegenTest, GeneratesConstructors) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("SimpleRec() : RecordBase(RECORD_SIZE)"), std::string::npos);
    EXPECT_NE(code.find("SimpleRec(const char* raw, size_t len)"), std::string::npos);
}

TEST(CodegenTest, GeneratesRegisterFieldCalls) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("registerField({\"NAME\""), std::string::npos);
    EXPECT_NE(code.find("registerField({\"CODE\""), std::string::npos);
    EXPECT_NE(code.find("registerField({\"AMOUNT\""), std::string::npos);
}

TEST(CodegenTest, GeneratesCorrectFieldTypes) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("FieldType::CHARACTER"), std::string::npos);
    EXPECT_NE(code.find("FieldType::ZONED_UNSIGNED"), std::string::npos);
}

TEST(CodegenTest, GeneratesNamespace) {
    auto def = parser.parseString(SIMPLE_CPY);
    CodegenOptions opts;
    opts.ns = "myapp::records";
    std::string code = codegen.generateHeader(def, opts);
    EXPECT_NE(code.find("namespace myapp::records"), std::string::npos);
}

// ═══════════════════════════════════════════════════════════════════════════
// Code generation with GROUP items
// ═══════════════════════════════════════════════════════════════════════════

static const char* PERSON_CPY = R"(
       01  PERSON.
           05  ID                      PIC X(12).
           05  FIRST-NAME              PIC X(32).
           05  HOME-PHONE.
               10  NUMBER              PIC X(12).
)";

TEST(CodegenTest, GeneratesChildClass) {
    auto def = parser.parseString(PERSON_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("class HomePhone : public RecordBase"), std::string::npos);
}

TEST(CodegenTest, GeneratesChildAccessor) {
    auto def = parser.parseString(PERSON_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("HomePhone& home_phone()"), std::string::npos);
}

TEST(CodegenTest, GeneratesChildMember) {
    auto def = parser.parseString(PERSON_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("std::shared_ptr<HomePhone> home_phone_"), std::string::npos);
}

TEST(CodegenTest, GeneratesRegisterChild) {
    auto def = parser.parseString(PERSON_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("registerChild(\"HOME_PHONE\""), std::string::npos);
}

TEST(CodegenTest, GeneratesInitChildren) {
    auto def = parser.parseString(PERSON_CPY);
    std::string code = codegen.generateHeader(def);
    EXPECT_NE(code.find("initChildren()"), std::string::npos);
}

// ═══════════════════════════════════════════════════════════════════════════
// FILLER fields are emitted but marked
// ═══════════════════════════════════════════════════════════════════════════

TEST(CodegenTest, FillerFieldsSkippedInRegister) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  NAME     PIC X(10).
           05  FILLER   PIC X(5).
           05  CODE     PIC X(3).
    )");
    std::string code = codegen.generateHeader(def);

    // FILLER should NOT have a registerField call
    EXPECT_EQ(code.find("registerField({\"FILLER\""), std::string::npos);

    // But NAME and CODE should
    EXPECT_NE(code.find("registerField({\"NAME\""), std::string::npos);
    EXPECT_NE(code.find("registerField({\"CODE\""), std::string::npos);

    // Total size should still include filler
    EXPECT_NE(code.find("RECORD_SIZE = 18"), std::string::npos);
}

// ═══════════════════════════════════════════════════════════════════════════
// Decimal positions in generated code
// ═══════════════════════════════════════════════════════════════════════════

TEST(CodegenTest, DecimalPositionsEmitted) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  AMOUNT  PIC S9(9)V99.
    )");
    std::string code = codegen.generateHeader(def);

    // Should include decimal_positions parameter in registerField
    EXPECT_NE(code.find("FieldType::ZONED_NUMERIC, 2"), std::string::npos);
}

// ═══════════════════════════════════════════════════════════════════════════
// Option: no offset constants
// ═══════════════════════════════════════════════════════════════════════════

TEST(CodegenTest, NoCommentsWhenDisabled) {
    auto def = parser.parseString(SIMPLE_CPY);
    CodegenOptions opts;
    opts.emit_comments = false;
    std::string code = codegen.generateHeader(def, opts);
    // Doc comments should not be present
    EXPECT_EQ(code.find("/// Generated from"), std::string::npos);
    // But class and constants should still exist
    EXPECT_NE(code.find("class SimpleRec"), std::string::npos);
    EXPECT_NE(code.find("registerField("), std::string::npos);
}

// ═══════════════════════════════════════════════════════════════════════════
// Full ACCOUNT copybook codegen
// ═══════════════════════════════════════════════════════════════════════════

TEST(CodegenTest, AccountCodegenCompiles) {
    auto def = parser.parseString(R"(
       01  ACCOUNT.
           05  COMPANY-NO              PIC 9(5).
           05  ACCOUNT-NO              PIC X(12).
           05  FILLER                  PIC X(5).
           05  BALANCE                 PIC S9(9)V99.
           05  CUSTOMER-ADDRESS.
               10  ADDRESS-LINE-1      PIC X(30).
               10  CITY                PIC X(20).
               10  STATE               PIC X(2).
    )");
    std::string code = codegen.generateHeader(def);

    // Verify key structural elements are present
    EXPECT_NE(code.find("class Account : public RecordBase"), std::string::npos);
    EXPECT_NE(code.find("class CustomerAddress : public RecordBase"), std::string::npos);
    // 5+12+5+11+30+20+2 = 85
    EXPECT_NE(code.find("RECORD_SIZE = 85"), std::string::npos);
    EXPECT_NE(code.find("CustomerAddress& customer_address()"), std::string::npos);
}

// ═══════════════════════════════════════════════════════════════════════════
// File generation
// ═══════════════════════════════════════════════════════════════════════════

TEST(CodegenTest, GenerateFileCreatesOutput) {
    auto def = parser.parseString(SIMPLE_CPY);
    std::string path = codegen.generateFile(def, "/tmp");
    EXPECT_EQ(path, "/tmp/simple_rec.h");

    // Verify file exists and has content
    std::ifstream in(path);
    ASSERT_TRUE(in.is_open());
    std::string content((std::istreambuf_iterator<char>(in)),
                         std::istreambuf_iterator<char>());
    EXPECT_NE(content.find("class SimpleRec"), std::string::npos);
}
