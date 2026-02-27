#pragma once

#include <copybook/transport/record_transport.h>
#include "copybook_service.grpc.pb.h"

#include <grpcpp/grpcpp.h>

#include <memory>
#include <mutex>
#include <string>
#include <vector>

namespace copybook::transport {

/// gRPC service implementation for the CopybookService.
///
/// Provides remote access to:
///   - Record transport (send/receive raw COBOL buffers)
///   - Field extraction and packing (buffer ↔ named fields)
///   - Schema introspection (list and query copybook definitions)
///   - Format conversion (buffer ↔ JSON/YAML)
///   - Streaming record transport
class CopybookServiceImpl final : public CopybookService::Service {
public:
    explicit CopybookServiceImpl(RecordRegistry& registry);

    // ── Unary RPCs ────────────────────────────────────────────────────

    grpc::Status SendRecord(grpc::ServerContext* context,
                            const SendRequest* request,
                            SendResponse* response) override;

    grpc::Status GetFields(grpc::ServerContext* context,
                           const GetFieldsRequest* request,
                           GetFieldsResponse* response) override;

    grpc::Status SetFields(grpc::ServerContext* context,
                           const SetFieldsRequest* request,
                           SetFieldsResponse* response) override;

    grpc::Status GetSchema(grpc::ServerContext* context,
                           const SchemaRequest* request,
                           SchemaResponse* response) override;

    grpc::Status ListSchemas(grpc::ServerContext* context,
                             const ListSchemasRequest* request,
                             ListSchemasResponse* response) override;

    grpc::Status Convert(grpc::ServerContext* context,
                         const ConvertRequest* request,
                         ConvertResponse* response) override;

    // ── Streaming RPC ─────────────────────────────────────────────────

    grpc::Status StreamRecords(
        grpc::ServerContext* context,
        grpc::ServerReaderWriter<SendResponse, SendRequest>* stream) override;

private:
    RecordRegistry& registry_;
    std::mutex      mutex_;

    // Stored records (keyed by record_name, most recent buffer)
    std::unordered_map<std::string, std::string> stored_buffers_;

    // Helper: populate a protobuf FieldDescriptor from a CopybookField
    static void populateFieldDescriptor(
        ::copybook::transport::FieldDescriptor* proto,
        const CopybookField& field);

    // Helper: populate a RecordSchema from a CopybookDefinition
    static void populateSchema(RecordSchema* schema,
                               const CopybookDefinition& def);
};

/// Start a gRPC server on the given address (e.g. "0.0.0.0:50051").
/// Blocks until the server is shut down.
void RunServer(const std::string& address, RecordRegistry& registry);

/// Start a gRPC server in a background thread. Returns the server
/// object so it can be shut down later.
std::unique_ptr<grpc::Server> StartServer(const std::string& address,
                                           RecordRegistry& registry);

} // namespace copybook::transport
