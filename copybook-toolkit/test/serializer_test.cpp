#include <gtest/gtest.h>
#include <copybook/serial/json_serializer.h>
#include <copybook/serial/yaml_serializer.h>
#include <nlohmann/json.hpp>

#include "person.h"

using namespace copybook;
using namespace copybook::examples;

// ═══════════════════════════════════════════════════════════════════════════
// Helper: populate a Person record
// ═══════════════════════════════════════════════════════════════════════════

static Person makeTestPerson() {
    Person p;
    p.setData("ID",             "EMP-001");
    p.setData("FIRST_NAME",     "Thomas");
    p.setData("LAST_NAME",      "Peters");
    p.setData("MIDDLE_INITIAL", "G");
    p.setNumeric("AGE",          60);
    p.setData("SEX",            "M");
    p.setData("DATE_OF_BIRTH",  "1965-03-15");
    p.homePhone().setData("NUMBER", "555-123-4567");
    p.workPhone().setData("NUMBER", "555-987-6543");
    p.syncChildren();
    return p;
}

// ═══════════════════════════════════════════════════════════════════════════
// JSON Serialization
// ═══════════════════════════════════════════════════════════════════════════

TEST(JsonSerializerTest, ToJsonProducesValidJson) {
    Person p = makeTestPerson();
    std::string json = JsonSerializer::toJson(p);

    // Should parse without error
    auto j = nlohmann::json::parse(json);
    EXPECT_TRUE(j.is_object());
}

TEST(JsonSerializerTest, ToJsonContainsAllFields) {
    Person p = makeTestPerson();
    auto j = JsonSerializer::toJsonObject(p);

    EXPECT_EQ(j["ID"], "EMP-001");
    EXPECT_EQ(j["FIRST_NAME"], "Thomas");
    EXPECT_EQ(j["LAST_NAME"], "Peters");
    EXPECT_EQ(j["MIDDLE_INITIAL"], "G");
    EXPECT_EQ(j["AGE"], 60);
    EXPECT_EQ(j["SEX"], "M");
    EXPECT_EQ(j["DATE_OF_BIRTH"], "1965-03-15");
}

TEST(JsonSerializerTest, ToJsonNestedObjects) {
    Person p = makeTestPerson();
    auto j = JsonSerializer::toJsonObject(p);

    EXPECT_TRUE(j["HOME_PHONE"].is_object());
    EXPECT_EQ(j["HOME_PHONE"]["NUMBER"], "555-123-4567");
    EXPECT_TRUE(j["WORK_PHONE"].is_object());
    EXPECT_EQ(j["WORK_PHONE"]["NUMBER"], "555-987-6543");
}

TEST(JsonSerializerTest, ToJsonNumericAsNumber) {
    Person p = makeTestPerson();
    auto j = JsonSerializer::toJsonObject(p);
    EXPECT_TRUE(j["AGE"].is_number());
}

TEST(JsonSerializerTest, ToJsonTrimsTrailingSpaces) {
    Person p = makeTestPerson();
    std::string json = JsonSerializer::toJson(p, true, true);
    // "Thomas" not "Thomas                          "
    EXPECT_NE(json.find("\"Thomas\""), std::string::npos);
    EXPECT_EQ(json.find("Thomas       "), std::string::npos);
}

TEST(JsonSerializerTest, ToJsonNoTrimKeepsSpaces) {
    Person p = makeTestPerson();
    auto j = JsonSerializer::toJsonObject(p, false);
    std::string name = j["FIRST_NAME"];
    EXPECT_EQ(name.size(), 32u);  // full 32-byte field
}

TEST(JsonSerializerTest, ToJsonCompactFormat) {
    Person p = makeTestPerson();
    std::string compact = JsonSerializer::toJson(p, false);
    EXPECT_EQ(compact.find('\n'), std::string::npos);
}

// ── JSON Deserialization ───────────────────────────────────────────────────

TEST(JsonSerializerTest, FromJsonSetsFields) {
    Person p;
    JsonSerializer::fromJson(p, R"({
        "ID": "EMP-999",
        "FIRST_NAME": "Alice",
        "AGE": 30
    })");

    EXPECT_EQ(p.getData("ID").substr(0, 7), "EMP-999");
    EXPECT_EQ(p.getData("FIRST_NAME").substr(0, 5), "Alice");
    EXPECT_EQ(p.getNumeric("AGE"), 30);
}

TEST(JsonSerializerTest, FromJsonSetsNestedObjects) {
    Person p;
    JsonSerializer::fromJson(p, R"({
        "HOME_PHONE": {
            "NUMBER": "999-888-7777"
        }
    })");

    EXPECT_EQ(p.homePhone().getData("NUMBER"), "999-888-7777");
}

TEST(JsonSerializerTest, FromJsonIgnoresUnknownFields) {
    Person p;
    // Should not throw on unknown fields
    EXPECT_NO_THROW(JsonSerializer::fromJson(p, R"({
        "ID": "EMP-001",
        "UNKNOWN_FIELD": "ignored"
    })"));
    EXPECT_EQ(p.getData("ID").substr(0, 7), "EMP-001");
}

TEST(JsonSerializerTest, FromJsonThrowsOnMalformed) {
    Person p;
    EXPECT_THROW(JsonSerializer::fromJson(p, "not json"), nlohmann::json::parse_error);
}

// ── JSON Round-Trip ────────────────────────────────────────────────────────

TEST(JsonSerializerTest, RoundTrip) {
    Person p1 = makeTestPerson();
    std::string json = JsonSerializer::toJson(p1);

    Person p2;
    JsonSerializer::fromJson(p2, json);
    p2.syncChildren();

    EXPECT_EQ(p2.getData("FIRST_NAME").substr(0, 6), "Thomas");
    EXPECT_EQ(p2.getData("LAST_NAME").substr(0, 6), "Peters");
    EXPECT_EQ(p2.getNumeric("AGE"), 60);
    EXPECT_EQ(p2.homePhone().getData("NUMBER"), "555-123-4567");
    EXPECT_EQ(p2.workPhone().getData("NUMBER"), "555-987-6543");
}

// ═══════════════════════════════════════════════════════════════════════════
// YAML Serialization
// ═══════════════════════════════════════════════════════════════════════════

TEST(YamlSerializerTest, ToYamlProducesValidOutput) {
    Person p = makeTestPerson();
    std::string yaml = YamlSerializer::toYaml(p);
    EXPECT_NE(yaml.find("---"), std::string::npos);
    EXPECT_NE(yaml.find("FIRST_NAME:"), std::string::npos);
}

TEST(YamlSerializerTest, ToYamlContainsAllFields) {
    Person p = makeTestPerson();
    std::string yaml = YamlSerializer::toYaml(p);
    EXPECT_NE(yaml.find("ID: EMP-001"), std::string::npos);
    EXPECT_NE(yaml.find("FIRST_NAME: Thomas"), std::string::npos);
    EXPECT_NE(yaml.find("AGE: 60"), std::string::npos);
    EXPECT_NE(yaml.find("SEX: M"), std::string::npos);
}

TEST(YamlSerializerTest, ToYamlNestedObjects) {
    Person p = makeTestPerson();
    std::string yaml = YamlSerializer::toYaml(p);
    EXPECT_NE(yaml.find("HOME_PHONE:\n"), std::string::npos);
    EXPECT_NE(yaml.find("  NUMBER: 555-123-4567"), std::string::npos);
}

// ── YAML Deserialization ───────────────────────────────────────────────────

TEST(YamlSerializerTest, FromYamlSetsFields) {
    Person p;
    YamlSerializer::fromYaml(p, R"(---
ID: EMP-777
FIRST_NAME: Bob
AGE: 45
)");

    EXPECT_EQ(p.getData("ID").substr(0, 7), "EMP-777");
    EXPECT_EQ(p.getData("FIRST_NAME").substr(0, 3), "Bob");
    EXPECT_EQ(p.getNumeric("AGE"), 45);
}

TEST(YamlSerializerTest, FromYamlSetsNestedObjects) {
    Person p;
    YamlSerializer::fromYaml(p, R"(---
HOME_PHONE:
  NUMBER: 111-222-3333
)");

    EXPECT_EQ(p.homePhone().getData("NUMBER"), "111-222-3333");
}

TEST(YamlSerializerTest, FromYamlIgnoresUnknownFields) {
    Person p;
    EXPECT_NO_THROW(YamlSerializer::fromYaml(p, R"(---
ID: EMP-001
SALARY: 50000
)"));
    EXPECT_EQ(p.getData("ID").substr(0, 7), "EMP-001");
}

// ── YAML Round-Trip ────────────────────────────────────────────────────────

TEST(YamlSerializerTest, RoundTrip) {
    Person p1 = makeTestPerson();
    std::string yaml = YamlSerializer::toYaml(p1);

    Person p2;
    YamlSerializer::fromYaml(p2, yaml);
    p2.syncChildren();

    EXPECT_EQ(p2.getData("FIRST_NAME").substr(0, 6), "Thomas");
    EXPECT_EQ(p2.getData("LAST_NAME").substr(0, 6), "Peters");
    EXPECT_EQ(p2.getNumeric("AGE"), 60);
    EXPECT_EQ(p2.homePhone().getData("NUMBER"), "555-123-4567");
    EXPECT_EQ(p2.workPhone().getData("NUMBER"), "555-987-6543");
}

// ═══════════════════════════════════════════════════════════════════════════
// Cross-format: JSON → YAML → JSON round trip
// ═══════════════════════════════════════════════════════════════════════════

TEST(CrossFormatTest, JsonToYamlToJson) {
    Person p1 = makeTestPerson();

    // Person → JSON → Person2
    std::string json = JsonSerializer::toJson(p1);
    Person p2;
    JsonSerializer::fromJson(p2, json);
    p2.syncChildren();

    // Person2 → YAML → Person3
    std::string yaml = YamlSerializer::toYaml(p2);
    Person p3;
    YamlSerializer::fromYaml(p3, yaml);
    p3.syncChildren();

    // Person3 should match Person1
    EXPECT_EQ(p3.getData("FIRST_NAME").substr(0, 6), "Thomas");
    EXPECT_EQ(p3.getNumeric("AGE"), 60);
    EXPECT_EQ(p3.homePhone().getData("NUMBER"), "555-123-4567");
}
