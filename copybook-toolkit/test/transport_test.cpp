#include <gtest/gtest.h>

#include <copybook/transport/record_transport.h>
#include <copybook/transport/grpc_service.h>
#include <copybook/transport/grpc_client.h>
#include <copybook/parser/copybook_parser.h>

#include <grpcpp/grpcpp.h>
#include <thread>
#include <chrono>

using namespace copybook;
using namespace copybook::transport;

// ═══════════════════════════════════════════════════════════════════════════
// RecordRegistry Tests (no gRPC needed)
// ═══════════════════════════════════════════════════════════════════════════

class RecordRegistryTest : public ::testing::Test {
protected:
    void SetUp() override {
        CopybookParser parser;
        personDef_ = parser.parseString(R"(
           01  PERSON.
               05  ID              PIC X(12).
               05  FIRST-NAME      PIC X(32).
               05  LAST-NAME       PIC X(32).
               05  AGE             PIC 9(3).
               05  HOME-PHONE.
                   10  NUMBER      PIC X(12).
        )");
        registry_.registerCopybook(personDef_);
    }

    CopybookDefinition personDef_;
    RecordRegistry registry_;
};

TEST_F(RecordRegistryTest, GetSchema) {
    auto* schema = registry_.getSchema("PERSON");
    ASSERT_NE(schema, nullptr);
    EXPECT_EQ(schema->record_name, "PERSON");
    EXPECT_EQ(schema->cpp_class_name, "Person");
    EXPECT_EQ(schema->total_size, 91);
}

TEST_F(RecordRegistryTest, GetSchemaNotFound) {
    EXPECT_EQ(registry_.getSchema("UNKNOWN"), nullptr);
}

TEST_F(RecordRegistryTest, AllSchemas) {
    auto schemas = registry_.allSchemas();
    EXPECT_EQ(schemas.size(), 1u);
    EXPECT_EQ(schemas[0]->record_name, "PERSON");
}

TEST_F(RecordRegistryTest, CreateBlankRecord) {
    auto record = registry_.createBlankRecord("PERSON");
    ASSERT_NE(record, nullptr);
    EXPECT_EQ(record->recordSize(), 91u);
    EXPECT_TRUE(record->hasField("ID"));
    EXPECT_TRUE(record->hasField("FIRST_NAME"));
    EXPECT_TRUE(record->hasField("AGE"));
}

TEST_F(RecordRegistryTest, CreateBlankRecordNotFound) {
    EXPECT_EQ(registry_.createBlankRecord("UNKNOWN"), nullptr);
}

TEST_F(RecordRegistryTest, CreateRecordFromBuffer) {
    std::string buffer(91, ' ');
    buffer.replace(0, 6, "EMP001");
    buffer.replace(12, 6, "Thomas");

    auto record = registry_.createRecord("PERSON", buffer.c_str(), buffer.size());
    ASSERT_NE(record, nullptr);
    EXPECT_EQ(record->getData("ID").substr(0, 6), "EMP001");
    EXPECT_EQ(record->getData("FIRST_NAME").substr(0, 6), "Thomas");
}

TEST_F(RecordRegistryTest, ExtractFields) {
    auto record = registry_.createBlankRecord("PERSON");
    record->setData("ID", "EMP-001");
    record->setData("FIRST_NAME", "Thomas");
    record->setData("AGE", "060");

    auto fields = RecordRegistry::extractFields(*record);
    EXPECT_GE(fields.size(), 3u);

    bool foundId = false, foundName = false, foundAge = false;
    for (const auto& [name, value] : fields) {
        if (name == "ID") { EXPECT_EQ(value, "EMP-001"); foundId = true; }
        if (name == "FIRST_NAME") { EXPECT_EQ(value, "Thomas"); foundName = true; }
        if (name == "AGE") { EXPECT_EQ(value, "060"); foundAge = true; }
    }
    EXPECT_TRUE(foundId);
    EXPECT_TRUE(foundName);
    EXPECT_TRUE(foundAge);
}

TEST_F(RecordRegistryTest, ApplyFields) {
    auto record = registry_.createBlankRecord("PERSON");
    std::vector<std::pair<std::string, std::string>> fields = {
        {"ID", "EMP-002"},
        {"FIRST_NAME", "Alice"},
        {"AGE", "030"}
    };
    RecordRegistry::applyFields(*record, fields);

    EXPECT_EQ(record->getData("ID").substr(0, 7), "EMP-002");
    EXPECT_EQ(record->getData("FIRST_NAME").substr(0, 5), "Alice");
    EXPECT_EQ(record->getData("AGE"), "030");
}

TEST_F(RecordRegistryTest, LoadDirectory) {
    RecordRegistry dirRegistry;
    dirRegistry.loadDirectory("../examples/copybooks");

    EXPECT_NE(dirRegistry.getSchema("PERSON"), nullptr);
    EXPECT_NE(dirRegistry.getSchema("ACCOUNT"), nullptr);
    EXPECT_NE(dirRegistry.getSchema("BROKER-CONTROL"), nullptr);
    EXPECT_NE(dirRegistry.getSchema("TRADE-RECORD"), nullptr);
    EXPECT_EQ(dirRegistry.allSchemas().size(), 4u);
}

// ═══════════════════════════════════════════════════════════════════════════
// gRPC Integration Tests (client/server in-process)
// ═══════════════════════════════════════════════════════════════════════════

class GrpcTransportTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Set up registry
        registry_.loadDirectory("../examples/copybooks");

        // Start server on a random port
        service_ = std::make_unique<CopybookServiceImpl>(registry_);
        grpc::ServerBuilder builder;
        builder.AddListeningPort("localhost:0",
                                 grpc::InsecureServerCredentials(), &port_);
        builder.RegisterService(service_.get());
        server_ = builder.BuildAndStart();

        // Create client
        std::string address = "localhost:" + std::to_string(port_);
        client_ = std::make_unique<CopybookClient>(address);
    }

    void TearDown() override {
        server_->Shutdown();
    }

    RecordRegistry registry_;
    std::unique_ptr<CopybookServiceImpl> service_;
    std::unique_ptr<grpc::Server> server_;
    std::unique_ptr<CopybookClient> client_;
    int port_ = 0;
};

TEST_F(GrpcTransportTest, ListSchemas) {
    auto schemas = client_->listSchemas();
    EXPECT_EQ(schemas.size(), 4u);

    bool foundPerson = false;
    for (const auto& s : schemas) {
        if (s.record_name() == "PERSON") {
            foundPerson = true;
            EXPECT_EQ(s.cpp_class_name(), "Person");
            EXPECT_EQ(s.total_size(), 115);
        }
    }
    EXPECT_TRUE(foundPerson);
}

TEST_F(GrpcTransportTest, GetSchema) {
    auto schema = client_->getSchema("ACCOUNT");
    ASSERT_NE(schema, nullptr);
    EXPECT_EQ(schema->record_name(), "ACCOUNT");
    EXPECT_EQ(schema->total_size(), 199);
    EXPECT_GT(schema->fields_size(), 0);
}

TEST_F(GrpcTransportTest, GetSchemaNotFound) {
    auto schema = client_->getSchema("NONEXISTENT");
    EXPECT_EQ(schema, nullptr);
}

TEST_F(GrpcTransportTest, SetFieldsAndGetFields) {
    std::vector<std::pair<std::string, std::string>> fields = {
        {"ID", "EMP-001"},
        {"FIRST_NAME", "Thomas"},
        {"LAST_NAME", "Peters"},
        {"AGE", "060"},
    };

    // Pack fields into a buffer
    std::string buffer = client_->setFields("PERSON", fields, 115);
    ASSERT_FALSE(buffer.empty());
    EXPECT_EQ(buffer.size(), 115u);

    // Extract fields back
    auto extracted = client_->getFields("PERSON", buffer.c_str(), buffer.size());
    ASSERT_GE(extracted.size(), 4u);

    bool foundId = false, foundName = false;
    for (const auto& [name, value] : extracted) {
        if (name == "ID") { EXPECT_EQ(value, "EMP-001"); foundId = true; }
        if (name == "FIRST_NAME") { EXPECT_EQ(value, "Thomas"); foundName = true; }
    }
    EXPECT_TRUE(foundId);
    EXPECT_TRUE(foundName);
}

TEST_F(GrpcTransportTest, SendRecord) {
    std::string buffer(115, ' ');
    buffer.replace(0, 7, "EMP-001");

    auto [ok, msg] = client_->sendRecord("PERSON", buffer.c_str(), buffer.size());
    EXPECT_TRUE(ok);
    EXPECT_NE(msg.find("115"), std::string::npos);
}

TEST_F(GrpcTransportTest, SendRecordUnknown) {
    std::string buffer(10, ' ');
    auto [ok, msg] = client_->sendRecord("UNKNOWN", buffer.c_str(), buffer.size());
    EXPECT_FALSE(ok);
}

TEST_F(GrpcTransportTest, ConvertToJson) {
    // Create a person buffer
    std::vector<std::pair<std::string, std::string>> fields = {
        {"ID", "EMP-001"},
        {"FIRST_NAME", "Thomas"},
        {"AGE", "060"},
    };
    std::string buffer = client_->setFields("PERSON", fields, 115);
    ASSERT_FALSE(buffer.empty());

    // Convert to JSON
    std::string json = client_->toJson("PERSON", buffer.c_str(), buffer.size());
    ASSERT_FALSE(json.empty());
    EXPECT_NE(json.find("EMP-001"), std::string::npos);
    EXPECT_NE(json.find("Thomas"), std::string::npos);
}

TEST_F(GrpcTransportTest, ConvertToYaml) {
    std::vector<std::pair<std::string, std::string>> fields = {
        {"ID", "EMP-001"},
        {"FIRST_NAME", "Thomas"},
    };
    std::string buffer = client_->setFields("PERSON", fields, 115);
    ASSERT_FALSE(buffer.empty());

    std::string yaml = client_->toYaml("PERSON", buffer.c_str(), buffer.size());
    ASSERT_FALSE(yaml.empty());
    EXPECT_NE(yaml.find("EMP-001"), std::string::npos);
    EXPECT_NE(yaml.find("Thomas"), std::string::npos);
}

TEST_F(GrpcTransportTest, ConvertFromJson) {
    std::string json = R"({"ID": "EMP-001", "FIRST_NAME": "Alice", "AGE": 25})";
    std::string buffer = client_->fromJson("PERSON", json);
    ASSERT_FALSE(buffer.empty());
    EXPECT_EQ(buffer.size(), 115u);

    // Verify by extracting fields
    auto fields = client_->getFields("PERSON", buffer.c_str(), buffer.size());
    bool foundName = false;
    for (const auto& [name, value] : fields) {
        if (name == "FIRST_NAME") { EXPECT_EQ(value, "Alice"); foundName = true; }
    }
    EXPECT_TRUE(foundName);
}

TEST_F(GrpcTransportTest, ConvertFromYaml) {
    std::string yaml = "---\nID: EMP-002\nFIRST_NAME: Bob\nAGE: 40\n";
    std::string buffer = client_->fromYaml("PERSON", yaml);
    ASSERT_FALSE(buffer.empty());
    EXPECT_EQ(buffer.size(), 115u);
}

TEST_F(GrpcTransportTest, JsonRoundTrip) {
    // Create buffer with known data
    std::vector<std::pair<std::string, std::string>> fields = {
        {"ID", "EMP-001"},
        {"FIRST_NAME", "Thomas"},
        {"LAST_NAME", "Peters"},
        {"AGE", "060"},
    };
    std::string original = client_->setFields("PERSON", fields, 115);

    // Convert to JSON and back
    std::string json = client_->toJson("PERSON", original.c_str(), original.size());
    std::string roundTrip = client_->fromJson("PERSON", json);

    EXPECT_EQ(original, roundTrip);
}

TEST_F(GrpcTransportTest, StreamRecords) {
    std::string buf1(115, ' ');
    buf1.replace(0, 5, "REC-1");
    std::string buf2(115, ' ');
    buf2.replace(0, 5, "REC-2");

    std::vector<std::pair<std::string, std::string>> batch = {
        {"PERSON", buf1},
        {"PERSON", buf2},
    };

    auto results = client_->streamRecords(batch);
    ASSERT_EQ(results.size(), 2u);
    EXPECT_TRUE(results[0].first);
    EXPECT_TRUE(results[1].first);
    EXPECT_NE(results[0].second.find("#1"), std::string::npos);
    EXPECT_NE(results[1].second.find("#2"), std::string::npos);
}

TEST_F(GrpcTransportTest, SchemaFieldDetails) {
    auto schema = client_->getSchema("PERSON");
    ASSERT_NE(schema, nullptr);

    // Check that field details are populated
    bool foundFirstName = false;
    for (const auto& f : schema->fields()) {
        if (f.name() == "FIRST_NAME") {
            foundFirstName = true;
            EXPECT_EQ(f.cobol_name(), "FIRST-NAME");
            EXPECT_EQ(f.pic_clause(), "X(32)");
            EXPECT_EQ(f.type(), "CHARACTER");
            EXPECT_EQ(f.offset(), 12);
            EXPECT_EQ(f.size(), 32);
            EXPECT_FALSE(f.is_group());
            EXPECT_FALSE(f.is_filler());
        }
    }
    EXPECT_TRUE(foundFirstName);
}

TEST_F(GrpcTransportTest, SchemaGroupChildren) {
    auto schema = client_->getSchema("PERSON");
    ASSERT_NE(schema, nullptr);

    // Find HOME-PHONE group
    bool foundGroup = false;
    for (const auto& f : schema->fields()) {
        if (f.name() == "HOME_PHONE") {
            foundGroup = true;
            EXPECT_TRUE(f.is_group());
            EXPECT_EQ(f.type(), "RECORD");
            EXPECT_GT(f.children_size(), 0);
            EXPECT_EQ(f.children(0).name(), "NUMBER");
        }
    }
    EXPECT_TRUE(foundGroup);
}

TEST_F(GrpcTransportTest, BrokerControlSchema) {
    auto schema = client_->getSchema("BROKER-CONTROL");
    ASSERT_NE(schema, nullptr);
    EXPECT_EQ(schema->total_size(), 67);
    EXPECT_GT(schema->fields_size(), 0);
}

TEST_F(GrpcTransportTest, AccountWithGroups) {
    auto schema = client_->getSchema("ACCOUNT");
    ASSERT_NE(schema, nullptr);
    EXPECT_EQ(schema->total_size(), 199);

    // Should have CUSTOMER_ADDRESS group
    bool foundGroup = false;
    for (const auto& f : schema->fields()) {
        if (f.name() == "CUSTOMER_ADDRESS") {
            foundGroup = true;
            EXPECT_TRUE(f.is_group());
            EXPECT_GT(f.children_size(), 0);
        }
    }
    EXPECT_TRUE(foundGroup);
}
