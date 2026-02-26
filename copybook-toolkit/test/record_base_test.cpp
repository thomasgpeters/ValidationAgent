#include <gtest/gtest.h>
#include <copybook/core/record_base.h>
#include "person.h"

using namespace copybook;
using namespace copybook::examples;

// ═══════════════════════════════════════════════════════════════════════════
// RecordBase — basic field registration and access
// ═══════════════════════════════════════════════════════════════════════════

class SimpleRecord : public RecordBase {
public:
    static constexpr int RECORD_SIZE = 50;

    SimpleRecord() : RecordBase(RECORD_SIZE) {
        registerField({"NAME",   20, 0,  FieldType::CHARACTER});
        registerField({"CODE",    5, 20, FieldType::CHARACTER});
        registerField({"AMOUNT", 10, 25, FieldType::ZONED_UNSIGNED});
        registerField({"STATUS",  1, 35, FieldType::CHARACTER});
        // Leaves 14 bytes of filler at end (36-49)
    }
};

TEST(RecordBaseTest, ConstructBlankRecord) {
    SimpleRecord rec;
    EXPECT_EQ(rec.recordSize(), 50u);
    EXPECT_EQ(rec.getData().size(), 50u);
}

TEST(RecordBaseTest, SetAndGetCharacterField) {
    SimpleRecord rec;
    rec.setData("NAME", "John Doe");
    EXPECT_EQ(rec.getData("NAME"), "John Doe            ");
}

TEST(RecordBaseTest, SetTruncatesLongValue) {
    SimpleRecord rec;
    rec.setData("CODE", "ABCDEFGH");  // 8 chars into 5-byte field
    EXPECT_EQ(rec.getData("CODE"), "ABCDE");
}

TEST(RecordBaseTest, SetPadsShortValue) {
    SimpleRecord rec;
    rec.setData("CODE", "AB");
    EXPECT_EQ(rec.getData("CODE"), "AB   ");
}

TEST(RecordBaseTest, GetEntireBuffer) {
    SimpleRecord rec;
    rec.setData("NAME", "Test");
    std::string full = rec.getData();
    EXPECT_EQ(full.size(), 50u);
    EXPECT_EQ(full.substr(0, 4), "Test");
}

TEST(RecordBaseTest, ThrowsOnUnknownFieldSet) {
    SimpleRecord rec;
    EXPECT_THROW(rec.setData("NONEXISTENT", "value"), std::invalid_argument);
}

TEST(RecordBaseTest, ThrowsOnUnknownFieldGet) {
    SimpleRecord rec;
    EXPECT_THROW(rec.getData("NONEXISTENT"), std::invalid_argument);
}

// ── Numeric access ─────────────────────────────────────────────────────────

TEST(RecordBaseTest, SetAndGetNumeric) {
    SimpleRecord rec;
    rec.setNumeric("AMOUNT", 12345);
    EXPECT_EQ(rec.getNumeric("AMOUNT"), 12345);
    // Stored as zero-padded string: "0000012345"
    EXPECT_EQ(rec.getData("AMOUNT"), "0000012345");
}

TEST(RecordBaseTest, NumericZero) {
    SimpleRecord rec;
    rec.setNumeric("AMOUNT", 0);
    EXPECT_EQ(rec.getNumeric("AMOUNT"), 0);
    EXPECT_EQ(rec.getData("AMOUNT"), "0000000000");
}

TEST(RecordBaseTest, GetNumericFromSpaces) {
    SimpleRecord rec;  // buffer is space-filled
    EXPECT_EQ(rec.getNumeric("AMOUNT"), 0);
}

// ── Field introspection ────────────────────────────────────────────────────

TEST(RecordBaseTest, HasField) {
    SimpleRecord rec;
    EXPECT_TRUE(rec.hasField("NAME"));
    EXPECT_TRUE(rec.hasField("CODE"));
    EXPECT_FALSE(rec.hasField("MISSING"));
}

TEST(RecordBaseTest, FieldNames) {
    SimpleRecord rec;
    auto names = rec.fieldNames();
    EXPECT_EQ(names.size(), 4u);
    // Should contain all registered fields
    EXPECT_NE(std::find(names.begin(), names.end(), "NAME"),   names.end());
    EXPECT_NE(std::find(names.begin(), names.end(), "CODE"),   names.end());
    EXPECT_NE(std::find(names.begin(), names.end(), "AMOUNT"), names.end());
    EXPECT_NE(std::find(names.begin(), names.end(), "STATUS"), names.end());
}

TEST(RecordBaseTest, FieldInfo) {
    SimpleRecord rec;
    const auto& fi = rec.fieldInfo("NAME");
    EXPECT_EQ(fi.name, "NAME");
    EXPECT_EQ(fi.size, 20);
    EXPECT_EQ(fi.offset, 0);
    EXPECT_EQ(fi.type, FieldType::CHARACTER);
}

// ── Buffer round-trip ──────────────────────────────────────────────────────

TEST(RecordBaseTest, LoadFromBuffer) {
    SimpleRecord rec;
    rec.setData("NAME", "Alice");
    rec.setData("CODE", "XY99Z");

    std::string raw = rec.getData();

    SimpleRecord rec2;
    rec2.loadFromBuffer(raw.c_str(), raw.size());

    EXPECT_EQ(rec2.getData("NAME"), rec.getData("NAME"));
    EXPECT_EQ(rec2.getData("CODE"), rec.getData("CODE"));
}

TEST(RecordBaseTest, LoadFromBufferThrowsOnSizeMismatch) {
    SimpleRecord rec;
    const char small[] = "too short";
    EXPECT_THROW(rec.loadFromBuffer(small, 9), std::invalid_argument);
}

// ═══════════════════════════════════════════════════════════════════════════
// Person — proof-of-concept port of Java PersonImpl
// ═══════════════════════════════════════════════════════════════════════════

TEST(PersonTest, RecordSize) {
    Person p;
    EXPECT_EQ(p.recordSize(), 115u);
}

TEST(PersonTest, FieldCount) {
    Person p;
    EXPECT_EQ(p.fieldNames().size(), 9u);
}

TEST(PersonTest, SetAndGetAllFields) {
    Person p;
    p.setData("ID",             "EMP-001");
    p.setData("FIRST_NAME",     "Thomas");
    p.setData("LAST_NAME",      "Peters");
    p.setData("MIDDLE_INITIAL", "G");
    p.setNumeric("AGE",          60);
    p.setData("SEX",            "M");
    p.setData("DATE_OF_BIRTH",  "1965-03-15");

    EXPECT_EQ(p.getData("ID").substr(0, 7), "EMP-001");
    EXPECT_EQ(p.getData("FIRST_NAME").substr(0, 6), "Thomas");
    EXPECT_EQ(p.getData("LAST_NAME").substr(0, 6), "Peters");
    EXPECT_EQ(p.getData("MIDDLE_INITIAL"), "G");
    EXPECT_EQ(p.getNumeric("AGE"), 60);
    EXPECT_EQ(p.getData("SEX"), "M");
    EXPECT_EQ(p.getData("DATE_OF_BIRTH"), "1965-03-15");
}

TEST(PersonTest, ChildRecordAccess) {
    Person p;
    p.homePhone().setData("NUMBER", "555-123-4567");
    p.workPhone().setData("NUMBER", "555-987-6543");

    EXPECT_EQ(p.homePhone().getData("NUMBER"), "555-123-4567");
    EXPECT_EQ(p.workPhone().getData("NUMBER"), "555-987-6543");
}

TEST(PersonTest, ChildSyncToParentBuffer) {
    Person p;
    p.homePhone().setData("NUMBER", "555-123-4567");
    p.workPhone().setData("NUMBER", "555-987-6543");

    // Before sync, parent buffer still has spaces at phone offsets
    // After sync, parent buffer should contain the phone data
    p.syncChildren();

    std::string full = p.getData();
    EXPECT_EQ(full.substr(Person::HOME_PHONE_OFFSET, Person::HOME_PHONE_SIZE), "555-123-4567");
    EXPECT_EQ(full.substr(Person::WORK_PHONE_OFFSET, Person::WORK_PHONE_SIZE), "555-987-6543");
}

TEST(PersonTest, RoundTripWithChildren) {
    Person p1;
    p1.setData("ID",         "EMP-002");
    p1.setData("FIRST_NAME", "Jerry");
    p1.setData("LAST_NAME",  "Garcia");
    p1.setNumeric("AGE",      53);
    p1.homePhone().setData("NUMBER", "415-555-0001");
    p1.syncChildren();

    std::string raw = p1.getData();

    // Deserialize into a new Person
    Person p2(raw.c_str(), raw.size());

    EXPECT_EQ(p2.getData("FIRST_NAME").substr(0, 5), "Jerry");
    EXPECT_EQ(p2.getData("LAST_NAME").substr(0, 6), "Garcia");
    EXPECT_EQ(p2.getNumeric("AGE"), 53);

    // Child records should be populated from the parent buffer
    EXPECT_EQ(p2.homePhone().getData("NUMBER"), "415-555-0001");
}

TEST(PersonTest, FieldOffsetsMatchJavaOriginal) {
    // Verify the C++ offsets match the Java PersonImpl constants exactly
    EXPECT_EQ(Person::ID_OFFSET,              0);
    EXPECT_EQ(Person::FIRST_NAME_OFFSET,     12);
    EXPECT_EQ(Person::LAST_NAME_OFFSET,      44);
    EXPECT_EQ(Person::MIDDLE_INITIAL_OFFSET, 76);
    EXPECT_EQ(Person::AGE_OFFSET,            77);
    EXPECT_EQ(Person::SEX_OFFSET,            80);
    EXPECT_EQ(Person::DATE_OF_BIRTH_OFFSET,  81);
    EXPECT_EQ(Person::HOME_PHONE_OFFSET,     91);
    EXPECT_EQ(Person::WORK_PHONE_OFFSET,    103);

    // Total: 103 + 12 = 115
    EXPECT_EQ(Person::RECORD_SIZE, 115);
}

// ═══════════════════════════════════════════════════════════════════════════
// Edge cases and validation
// ═══════════════════════════════════════════════════════════════════════════

TEST(RecordBaseTest, MultipleFieldWritesDontOverlap) {
    SimpleRecord rec;
    rec.setData("NAME", "AAAAAAAAAAAAAAAAAAA");   // 19 A's into 20-byte field
    rec.setData("CODE", "BBBBB");                  // 5 B's into 5-byte field

    // NAME should not bleed into CODE
    EXPECT_EQ(rec.getData("NAME").substr(0, 19), "AAAAAAAAAAAAAAAAAAA");
    EXPECT_EQ(rec.getData("CODE"), "BBBBB");
}

TEST(RecordBaseTest, EmptyStringSetClearsField) {
    SimpleRecord rec;
    rec.setData("NAME", "Something");
    rec.setData("NAME", "");
    // Field should be all spaces
    EXPECT_EQ(rec.getData("NAME"), "                    ");
}
