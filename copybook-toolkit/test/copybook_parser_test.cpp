#include <gtest/gtest.h>
#include <copybook/parser/copybook_parser.h>

using namespace copybook;

static const CopybookParser parser;

// ═══════════════════════════════════════════════════════════════════════════
// Basic parsing: PERSON copybook
// ═══════════════════════════════════════════════════════════════════════════

static const char* PERSON_CPY = R"(
       01  PERSON.
           05  ID                      PIC X(12).
           05  FIRST-NAME              PIC X(32).
           05  LAST-NAME               PIC X(32).
           05  MIDDLE-INITIAL          PIC X(1).
           05  AGE                     PIC 9(3).
           05  SEX                     PIC X(1).
           05  DATE-OF-BIRTH           PIC X(10).
           05  HOME-PHONE.
               10  NUMBER              PIC X(12).
           05  WORK-PHONE.
               10  NUMBER              PIC X(12).
)";

TEST(CopybookParserTest, ParsePersonRecordName) {
    auto def = parser.parseString(PERSON_CPY);
    EXPECT_EQ(def.record_name, "PERSON");
    EXPECT_EQ(def.cpp_class_name, "Person");
}

TEST(CopybookParserTest, ParsePersonTotalSize) {
    auto def = parser.parseString(PERSON_CPY);
    EXPECT_EQ(def.total_size, 115);
}

TEST(CopybookParserTest, ParsePersonFieldCount) {
    auto def = parser.parseString(PERSON_CPY);
    EXPECT_EQ(def.fields.size(), 9u);
}

TEST(CopybookParserTest, ParsePersonFieldNames) {
    auto def = parser.parseString(PERSON_CPY);
    EXPECT_EQ(def.fields[0].cpp_name, "ID");
    EXPECT_EQ(def.fields[1].cpp_name, "FIRST_NAME");
    EXPECT_EQ(def.fields[2].cpp_name, "LAST_NAME");
    EXPECT_EQ(def.fields[3].cpp_name, "MIDDLE_INITIAL");
    EXPECT_EQ(def.fields[4].cpp_name, "AGE");
    EXPECT_EQ(def.fields[5].cpp_name, "SEX");
    EXPECT_EQ(def.fields[6].cpp_name, "DATE_OF_BIRTH");
    EXPECT_EQ(def.fields[7].cpp_name, "HOME_PHONE");
    EXPECT_EQ(def.fields[8].cpp_name, "WORK_PHONE");
}

TEST(CopybookParserTest, ParsePersonFieldSizes) {
    auto def = parser.parseString(PERSON_CPY);
    EXPECT_EQ(def.fields[0].size, 12);  // ID
    EXPECT_EQ(def.fields[1].size, 32);  // FIRST_NAME
    EXPECT_EQ(def.fields[2].size, 32);  // LAST_NAME
    EXPECT_EQ(def.fields[3].size, 1);   // MIDDLE_INITIAL
    EXPECT_EQ(def.fields[4].size, 3);   // AGE
    EXPECT_EQ(def.fields[5].size, 1);   // SEX
    EXPECT_EQ(def.fields[6].size, 10);  // DATE_OF_BIRTH
    EXPECT_EQ(def.fields[7].size, 12);  // HOME_PHONE (group)
    EXPECT_EQ(def.fields[8].size, 12);  // WORK_PHONE (group)
}

TEST(CopybookParserTest, ParsePersonFieldOffsets) {
    auto def = parser.parseString(PERSON_CPY);
    EXPECT_EQ(def.fields[0].offset, 0);    // ID
    EXPECT_EQ(def.fields[1].offset, 12);   // FIRST_NAME
    EXPECT_EQ(def.fields[2].offset, 44);   // LAST_NAME
    EXPECT_EQ(def.fields[3].offset, 76);   // MIDDLE_INITIAL
    EXPECT_EQ(def.fields[4].offset, 77);   // AGE
    EXPECT_EQ(def.fields[5].offset, 80);   // SEX
    EXPECT_EQ(def.fields[6].offset, 81);   // DATE_OF_BIRTH
    EXPECT_EQ(def.fields[7].offset, 91);   // HOME_PHONE
    EXPECT_EQ(def.fields[8].offset, 103);  // WORK_PHONE
}

TEST(CopybookParserTest, ParsePersonFieldTypes) {
    auto def = parser.parseString(PERSON_CPY);
    EXPECT_EQ(def.fields[0].type, FieldType::CHARACTER);       // ID
    EXPECT_EQ(def.fields[1].type, FieldType::CHARACTER);       // FIRST_NAME
    EXPECT_EQ(def.fields[4].type, FieldType::ZONED_UNSIGNED);  // AGE
    EXPECT_EQ(def.fields[7].type, FieldType::RECORD);          // HOME_PHONE
    EXPECT_EQ(def.fields[8].type, FieldType::RECORD);          // WORK_PHONE
}

TEST(CopybookParserTest, ParsePersonGroupChildren) {
    auto def = parser.parseString(PERSON_CPY);

    EXPECT_TRUE(def.fields[7].is_group);
    EXPECT_EQ(def.fields[7].children.size(), 1u);
    EXPECT_EQ(def.fields[7].children[0].cpp_name, "NUMBER");
    EXPECT_EQ(def.fields[7].children[0].size, 12);

    EXPECT_TRUE(def.fields[8].is_group);
    EXPECT_EQ(def.fields[8].children.size(), 1u);
}

// ═══════════════════════════════════════════════════════════════════════════
// PIC clause variations
// ═══════════════════════════════════════════════════════════════════════════

TEST(CopybookParserTest, ParseSignedNumeric) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  AMOUNT  PIC S9(9)V99.
    )");
    EXPECT_EQ(def.fields[0].type, FieldType::ZONED_NUMERIC);
    EXPECT_EQ(def.fields[0].size, 11);  // 9 + 2
    EXPECT_EQ(def.fields[0].decimal_positions, 2);
}

TEST(CopybookParserTest, ParseUnsignedWithDecimal) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  RATE  PIC 9(3)V9(4).
    )");
    EXPECT_EQ(def.fields[0].type, FieldType::ZONED_UNSIGNED);
    EXPECT_EQ(def.fields[0].size, 7);  // 3 + 4
    EXPECT_EQ(def.fields[0].decimal_positions, 4);
}

TEST(CopybookParserTest, ParseInlineDecimal) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  PRICE  PIC 9(5)V99.
    )");
    EXPECT_EQ(def.fields[0].size, 7);  // 5 + 2
    EXPECT_EQ(def.fields[0].decimal_positions, 2);
}

TEST(CopybookParserTest, ParseSingleCharPic) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  FLAG  PIC X.
    )");
    EXPECT_EQ(def.fields[0].size, 1);
    EXPECT_EQ(def.fields[0].type, FieldType::CHARACTER);
}

TEST(CopybookParserTest, ParseRepeatedXPic) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  CODE  PIC XXX.
    )");
    EXPECT_EQ(def.fields[0].size, 3);
    EXPECT_EQ(def.fields[0].type, FieldType::CHARACTER);
}

TEST(CopybookParserTest, ParseRepeated9Pic) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  NUM  PIC 999.
    )");
    EXPECT_EQ(def.fields[0].size, 3);
    EXPECT_EQ(def.fields[0].type, FieldType::ZONED_UNSIGNED);
}

// ═══════════════════════════════════════════════════════════════════════════
// FILLER handling
// ═══════════════════════════════════════════════════════════════════════════

TEST(CopybookParserTest, ParseFiller) {
    auto def = parser.parseString(R"(
       01  TEST-REC.
           05  NAME     PIC X(10).
           05  FILLER   PIC X(5).
           05  CODE     PIC X(3).
    )");

    EXPECT_EQ(def.total_size, 18);
    EXPECT_EQ(def.fields.size(), 3u);

    EXPECT_TRUE(def.fields[1].is_filler);
    EXPECT_EQ(def.fields[1].size, 5);
    EXPECT_EQ(def.fields[1].type, FieldType::FILLER);

    // CODE should be offset past FILLER
    EXPECT_EQ(def.fields[2].offset, 15);
}

// ═══════════════════════════════════════════════════════════════════════════
// Complex ACCOUNT copybook
// ═══════════════════════════════════════════════════════════════════════════

static const char* ACCOUNT_CPY = R"(
       01  ACCOUNT.
           05  COMPANY-NO              PIC 9(5).
           05  ACCOUNT-NO              PIC X(12).
           05  ACCOUNT-TYPE            PIC X(2).
           05  ACCOUNT-STATUS          PIC X(1).
           05  FILLER                  PIC X(5).
           05  BALANCE                 PIC S9(9)V99.
           05  CREDIT-LIMIT            PIC 9(9)V99.
           05  OPEN-DATE               PIC X(10).
           05  LAST-ACTIVITY-DATE      PIC X(10).
           05  CUSTOMER-NAME           PIC X(40).
           05  CUSTOMER-ADDRESS.
               10  ADDRESS-LINE-1      PIC X(30).
               10  ADDRESS-LINE-2      PIC X(30).
               10  CITY                PIC X(20).
               10  STATE               PIC X(2).
               10  ZIP-CODE            PIC X(10).
)";

TEST(CopybookParserTest, ParseAccountTotalSize) {
    auto def = parser.parseString(ACCOUNT_CPY);
    // 5+12+2+1+5+11+11+10+10+40+30+30+20+2+10 = 199
    EXPECT_EQ(def.total_size, 199);
}

TEST(CopybookParserTest, ParseAccountClassName) {
    auto def = parser.parseString(ACCOUNT_CPY);
    EXPECT_EQ(def.cpp_class_name, "Account");
}

TEST(CopybookParserTest, ParseAccountGroupField) {
    auto def = parser.parseString(ACCOUNT_CPY);

    // Find CUSTOMER_ADDRESS
    const CopybookField* addr = nullptr;
    for (const auto& f : def.fields) {
        if (f.cpp_name == "CUSTOMER_ADDRESS") { addr = &f; break; }
    }
    ASSERT_NE(addr, nullptr);
    EXPECT_TRUE(addr->is_group);
    EXPECT_EQ(addr->children.size(), 5u);
    EXPECT_EQ(addr->size, 92);  // 30+30+20+2+10
}

TEST(CopybookParserTest, ParseAccountSignedBalance) {
    auto def = parser.parseString(ACCOUNT_CPY);

    const CopybookField* bal = nullptr;
    for (const auto& f : def.fields) {
        if (f.cpp_name == "BALANCE") { bal = &f; break; }
    }
    ASSERT_NE(bal, nullptr);
    EXPECT_EQ(bal->type, FieldType::ZONED_NUMERIC);
    EXPECT_EQ(bal->size, 11);
    EXPECT_EQ(bal->decimal_positions, 2);
}

// ═══════════════════════════════════════════════════════════════════════════
// TRADE-RECORD with multiple nested groups
// ═══════════════════════════════════════════════════════════════════════════

static const char* TRADE_CPY = R"(
       01  TRADE-RECORD.
           05  TRADE-ID                PIC X(16).
           05  TRADE-DATE              PIC X(10).
           05  TRADE-TIME              PIC X(8).
           05  TRADE-TYPE              PIC X(4).
           05  SYMBOL                  PIC X(10).
           05  QUANTITY                PIC 9(9).
           05  PRICE                   PIC 9(7)V9(4).
           05  TOTAL-AMOUNT            PIC S9(11)V99.
           05  COMMISSION              PIC 9(7)V99.
           05  BROKER-INFO.
               10  BROKER-ID           PIC X(8).
               10  BROKER-NAME         PIC X(30).
           05  CUSTOMER-INFO.
               10  CUSTOMER-ID         PIC X(12).
               10  CUSTOMER-NAME       PIC X(40).
           05  SETTLEMENT-DATE         PIC X(10).
           05  STATUS                  PIC X(2).
)";

TEST(CopybookParserTest, ParseTradeRecordSize) {
    auto def = parser.parseString(TRADE_CPY);
    // 16+10+8+4+10+9+11+13+9+8+30+12+40+10+2 = 192
    EXPECT_EQ(def.total_size, 192);
}

TEST(CopybookParserTest, ParseTradeClassName) {
    auto def = parser.parseString(TRADE_CPY);
    EXPECT_EQ(def.cpp_class_name, "TradeRecord");
}

TEST(CopybookParserTest, ParseTradeMultipleGroups) {
    auto def = parser.parseString(TRADE_CPY);

    int groupCount = 0;
    for (const auto& f : def.fields) {
        if (f.is_group) groupCount++;
    }
    EXPECT_EQ(groupCount, 2);  // BROKER-INFO and CUSTOMER-INFO
}

TEST(CopybookParserTest, ParseTradeGroupOffsets) {
    auto def = parser.parseString(TRADE_CPY);

    const CopybookField* broker = nullptr;
    const CopybookField* customer = nullptr;
    for (const auto& f : def.fields) {
        if (f.cpp_name == "BROKER_INFO") broker = &f;
        if (f.cpp_name == "CUSTOMER_INFO") customer = &f;
    }

    ASSERT_NE(broker, nullptr);
    ASSERT_NE(customer, nullptr);

    // BROKER-INFO offset: 16+10+8+4+10+9+11+13+9 = 90
    EXPECT_EQ(broker->offset, 90);
    EXPECT_EQ(broker->size, 38);  // 8+30

    // CUSTOMER-INFO offset: 90+38 = 128
    EXPECT_EQ(customer->offset, 128);
    EXPECT_EQ(customer->size, 52);  // 12+40
}

// ═══════════════════════════════════════════════════════════════════════════
// BROKER-CONTROL with decimal commission rate
// ═══════════════════════════════════════════════════════════════════════════

static const char* BROKER_CPY = R"(
       01  BROKER-CONTROL.
           05  BROKER-ID               PIC X(8).
           05  BROKER-NAME             PIC X(30).
           05  BROKER-STATUS           PIC X(1).
           05  TRANSACTION-COUNT       PIC 9(7).
           05  COMMISSION-RATE         PIC 9(3)V9(4).
           05  LAST-LOGIN              PIC X(10).
           05  OFFICE-CODE             PIC X(4).
)";

TEST(CopybookParserTest, ParseBrokerControlSize) {
    auto def = parser.parseString(BROKER_CPY);
    // 8+30+1+7+7+10+4 = 67
    EXPECT_EQ(def.total_size, 67);
}

TEST(CopybookParserTest, ParseBrokerClassName) {
    auto def = parser.parseString(BROKER_CPY);
    EXPECT_EQ(def.cpp_class_name, "BrokerControl");
}

TEST(CopybookParserTest, ParseBrokerCommissionRate) {
    auto def = parser.parseString(BROKER_CPY);
    const CopybookField* rate = nullptr;
    for (const auto& f : def.fields) {
        if (f.cpp_name == "COMMISSION_RATE") { rate = &f; break; }
    }
    ASSERT_NE(rate, nullptr);
    EXPECT_EQ(rate->size, 7);
    EXPECT_EQ(rate->decimal_positions, 4);
    EXPECT_EQ(rate->type, FieldType::ZONED_UNSIGNED);
}

// ═══════════════════════════════════════════════════════════════════════════
// Error cases
// ═══════════════════════════════════════════════════════════════════════════

TEST(CopybookParserTest, EmptyInputThrows) {
    EXPECT_THROW(parser.parseString(""), std::runtime_error);
}

TEST(CopybookParserTest, NoLevel01Throws) {
    EXPECT_THROW(parser.parseString(R"(
       05  FIELD1  PIC X(10).
    )"), std::runtime_error);
}

// ═══════════════════════════════════════════════════════════════════════════
// Parse from file
// ═══════════════════════════════════════════════════════════════════════════

TEST(CopybookParserTest, ParseFromFile) {
    // Use the test data file
    auto def = parser.parseFile("../test/data/PERSON.cpy");
    EXPECT_EQ(def.record_name, "PERSON");
    EXPECT_EQ(def.total_size, 115);
    EXPECT_EQ(def.fields.size(), 9u);
}

TEST(CopybookParserTest, ParseFromFileMissing) {
    EXPECT_THROW(parser.parseFile("/nonexistent/file.cpy"), std::runtime_error);
}

// ═══════════════════════════════════════════════════════════════════════════
// Name conversion
// ═══════════════════════════════════════════════════════════════════════════

TEST(CopybookParserTest, CobolNameConversion) {
    auto def = parser.parseString(R"(
       01  MY-TEST-RECORD.
           05  SOME-LONG-FIELD-NAME    PIC X(10).
    )");
    EXPECT_EQ(def.cpp_class_name, "MyTestRecord");
    EXPECT_EQ(def.fields[0].cpp_name, "SOME_LONG_FIELD_NAME");
}
