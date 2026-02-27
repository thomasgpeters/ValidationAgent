#pragma once

#include "copybook_service.grpc.pb.h"

#include <grpcpp/grpcpp.h>

#include <memory>
#include <string>
#include <vector>

namespace copybook::transport {

/// Client wrapper for the CopybookService gRPC service.
///
/// Example usage:
///   CopybookClient client("localhost:50051");
///
///   // Send a raw record
///   auto [ok, msg] = client.sendRecord("PERSON", personBuffer, 115);
///
///   // Get fields from a buffer
///   auto fields = client.getFields("PERSON", personBuffer, 115);
///
///   // Convert buffer to JSON
///   auto json = client.toJson("PERSON", personBuffer, 115);
class CopybookClient {
public:
    /// Connect to a gRPC server at the given address.
    explicit CopybookClient(const std::string& address);

    /// Connect using an existing channel (useful for testing).
    explicit CopybookClient(std::shared_ptr<grpc::Channel> channel);

    // ── Record transport ──────────────────────────────────────────────

    /// Send a raw COBOL buffer to the server.
    /// Returns {success, message}.
    std::pair<bool, std::string>
    sendRecord(const std::string& name, const char* raw, size_t size);

    // ── Field extraction/packing ──────────────────────────────────────

    /// Extract named fields from a raw buffer.
    /// Returns field name-value pairs.
    std::vector<std::pair<std::string, std::string>>
    getFields(const std::string& name, const char* raw, size_t size);

    /// Pack named fields into a raw buffer.
    /// Returns the packed buffer, or empty string on error.
    std::string
    setFields(const std::string& name,
              const std::vector<std::pair<std::string, std::string>>& fields,
              int bufferSize);

    // ── Schema introspection ──────────────────────────────────────────

    /// Get the schema for a specific record.
    /// Returns nullptr on error.
    std::unique_ptr<RecordSchema> getSchema(const std::string& name);

    /// List all known schemas.
    std::vector<RecordSchema> listSchemas();

    // ── Format conversion ─────────────────────────────────────────────

    /// Convert a raw buffer to JSON.
    std::string toJson(const std::string& name, const char* raw, size_t size);

    /// Convert a raw buffer to YAML.
    std::string toYaml(const std::string& name, const char* raw, size_t size);

    /// Convert JSON to a raw buffer.
    std::string fromJson(const std::string& name, const std::string& json);

    /// Convert YAML to a raw buffer.
    std::string fromYaml(const std::string& name, const std::string& yaml);

    // ── Streaming ─────────────────────────────────────────────────────

    /// Send multiple records as a stream.
    /// Returns a vector of {success, message} for each record.
    std::vector<std::pair<bool, std::string>>
    streamRecords(const std::vector<std::pair<std::string, std::string>>& records);

private:
    std::unique_ptr<CopybookService::Stub> stub_;
};

} // namespace copybook::transport
