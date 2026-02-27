// ═══════════════════════════════════════════════════════════════════════════
// Copybook gRPC Client Demo
//
// Demonstrates all CopybookService RPC calls.
//
// Usage:  ./grpc-client-demo [server-address]
//         Default: localhost:50051
// ═══════════════════════════════════════════════════════════════════════════

#include <copybook/transport/grpc_client.h>

#include <iostream>
#include <iomanip>
#include <string>

using namespace copybook::transport;

static void printSep(const std::string& title) {
    std::cout << "\n═══ " << title << " ";
    for (size_t i = title.size(); i < 60; i++) std::cout << "═";
    std::cout << "\n\n";
}

int main(int argc, char* argv[]) {
    std::string address = "localhost:50051";
    if (argc > 1) address = argv[1];

    std::cout << "Connecting to CopybookService at " << address << "...\n";
    CopybookClient client(address);

    // ── ListSchemas ───────────────────────────────────────────────────
    printSep("ListSchemas");
    auto schemas = client.listSchemas();
    std::cout << "Found " << schemas.size() << " schema(s):\n";
    for (const auto& s : schemas) {
        std::cout << "  " << std::setw(20) << std::left << s.record_name()
                  << " class=" << s.cpp_class_name()
                  << " size=" << s.total_size()
                  << " fields=" << s.fields_size() << "\n";
    }

    // ── GetSchema ─────────────────────────────────────────────────────
    printSep("GetSchema (PERSON)");
    auto personSchema = client.getSchema("PERSON");
    if (personSchema) {
        std::cout << "Record: " << personSchema->record_name()
                  << " (" << personSchema->total_size() << " bytes)\n";
        for (const auto& f : personSchema->fields()) {
            std::cout << "  " << std::setw(20) << std::left << f.name()
                      << " PIC " << std::setw(12) << f.pic_clause()
                      << " offset=" << std::setw(4) << f.offset()
                      << " size=" << std::setw(4) << f.size()
                      << " type=" << f.type() << "\n";
        }
    } else {
        std::cout << "PERSON schema not found.\n";
    }

    // ── SetFields (create a record) ───────────────────────────────────
    printSep("SetFields (create PERSON buffer)");
    std::vector<std::pair<std::string, std::string>> personFields = {
        {"ID", "EMP-001"},
        {"FIRST_NAME", "Thomas"},
        {"LAST_NAME", "Peters"},
        {"MIDDLE_INITIAL", "G"},
        {"AGE", "060"},
        {"SEX", "M"},
        {"DATE_OF_BIRTH", "1965-03-15"}
    };

    std::string personBuffer = client.setFields("PERSON", personFields, 115);
    if (!personBuffer.empty()) {
        std::cout << "Created " << personBuffer.size() << "-byte buffer.\n";
        std::cout << "Raw: [" << personBuffer << "]\n";
    } else {
        std::cout << "SetFields failed.\n";
        return 1;
    }

    // ── SendRecord ────────────────────────────────────────────────────
    printSep("SendRecord (store PERSON buffer)");
    auto [sendOk, sendMsg] = client.sendRecord(
        "PERSON", personBuffer.c_str(), personBuffer.size());
    std::cout << "Success: " << (sendOk ? "true" : "false")
              << "\nMessage: " << sendMsg << "\n";

    // ── GetFields ─────────────────────────────────────────────────────
    printSep("GetFields (extract from buffer)");
    auto fields = client.getFields(
        "PERSON", personBuffer.c_str(), personBuffer.size());
    for (const auto& [name, value] : fields) {
        std::cout << "  " << std::setw(20) << std::left << name
                  << " = \"" << value << "\"\n";
    }

    // ── Convert to JSON ───────────────────────────────────────────────
    printSep("Convert buffer → JSON");
    std::string json = client.toJson(
        "PERSON", personBuffer.c_str(), personBuffer.size());
    std::cout << json << "\n";

    // ── Convert to YAML ───────────────────────────────────────────────
    printSep("Convert buffer → YAML");
    std::string yaml = client.toYaml(
        "PERSON", personBuffer.c_str(), personBuffer.size());
    std::cout << yaml << "\n";

    // ── Convert JSON → buffer ─────────────────────────────────────────
    printSep("Convert JSON → buffer (round-trip)");
    std::string roundTrip = client.fromJson("PERSON", json);
    if (roundTrip == personBuffer) {
        std::cout << "Round-trip PASSED: buffers match.\n";
    } else {
        std::cout << "Round-trip FAILED: buffers differ.\n";
        std::cout << "Original:   [" << personBuffer << "]\n";
        std::cout << "Round-trip: [" << roundTrip << "]\n";
    }

    // ── StreamRecords ─────────────────────────────────────────────────
    printSep("StreamRecords (batch send)");
    std::vector<std::pair<std::string, std::string>> batch = {
        {"PERSON", personBuffer},
        {"PERSON", personBuffer},
    };
    auto streamResults = client.streamRecords(batch);
    for (const auto& [ok, msg] : streamResults) {
        std::cout << "  " << (ok ? "OK" : "FAIL") << ": " << msg << "\n";
    }

    std::cout << "\nAll demos complete.\n";
    return 0;
}
