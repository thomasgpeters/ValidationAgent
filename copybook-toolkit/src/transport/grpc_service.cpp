#include <copybook/transport/grpc_service.h>
#include <copybook/serial/json_serializer.h>
#include <copybook/serial/yaml_serializer.h>

#include <iostream>
#include <thread>

namespace copybook::transport {

CopybookServiceImpl::CopybookServiceImpl(RecordRegistry& registry)
    : registry_(registry) {}

// ── SendRecord ────────────────────────────────────────────────────────────

grpc::Status CopybookServiceImpl::SendRecord(
        grpc::ServerContext* /*context*/,
        const SendRequest* request,
        SendResponse* response) {
    const auto& payload = request->payload();
    const auto& name    = payload.record_name();
    const auto& raw     = payload.raw_buffer();
    int32_t     size    = payload.buffer_size();

    if (name.empty()) {
        response->set_success(false);
        response->set_message("record_name is required");
        return grpc::Status::OK;
    }

    const auto* schema = registry_.getSchema(name);
    if (!schema) {
        response->set_success(false);
        response->set_message("Unknown record: " + name);
        return grpc::Status::OK;
    }

    if (size > 0 && static_cast<int>(raw.size()) != size) {
        response->set_success(false);
        response->set_message("Buffer size mismatch: expected " +
                              std::to_string(size) + ", got " +
                              std::to_string(raw.size()));
        return grpc::Status::OK;
    }

    // Store the buffer
    {
        std::lock_guard<std::mutex> lock(mutex_);
        stored_buffers_[name] = raw;
    }

    response->set_success(true);
    response->set_message("Stored " + std::to_string(raw.size()) +
                          " bytes for " + name);
    return grpc::Status::OK;
}

// ── GetFields ─────────────────────────────────────────────────────────────

grpc::Status CopybookServiceImpl::GetFields(
        grpc::ServerContext* /*context*/,
        const GetFieldsRequest* request,
        GetFieldsResponse* response) {
    const auto& payload = request->payload();
    const auto& name    = payload.record_name();
    const auto& raw     = payload.raw_buffer();

    auto record = registry_.createRecord(name, raw.data(), raw.size());
    if (!record) {
        return grpc::Status(grpc::StatusCode::NOT_FOUND,
                           "Unknown record: " + name);
    }

    auto* msg = response->mutable_record();
    msg->set_record_name(name);

    auto fields = RecordRegistry::extractFields(*record);
    for (const auto& [fname, fval] : fields) {
        auto* fv = msg->add_fields();
        fv->set_name(fname);
        fv->set_value(fval);
    }

    return grpc::Status::OK;
}

// ── SetFields ─────────────────────────────────────────────────────────────

grpc::Status CopybookServiceImpl::SetFields(
        grpc::ServerContext* /*context*/,
        const SetFieldsRequest* request,
        SetFieldsResponse* response) {
    const auto& msg  = request->record();
    const auto& name = msg.record_name();

    auto record = registry_.createBlankRecord(name);
    if (!record) {
        return grpc::Status(grpc::StatusCode::NOT_FOUND,
                           "Unknown record: " + name);
    }

    // Apply fields
    std::vector<std::pair<std::string, std::string>> fields;
    for (const auto& fv : msg.fields()) {
        fields.emplace_back(fv.name(), fv.value());
    }
    RecordRegistry::applyFields(*record, fields);
    record->syncChildren();

    // Return the packed buffer
    auto* payload = response->mutable_payload();
    payload->set_record_name(name);
    std::string buf = record->getData();
    payload->set_raw_buffer(buf);
    payload->set_buffer_size(buf.size());

    return grpc::Status::OK;
}

// ── GetSchema ─────────────────────────────────────────────────────────────

grpc::Status CopybookServiceImpl::GetSchema(
        grpc::ServerContext* /*context*/,
        const SchemaRequest* request,
        SchemaResponse* response) {
    const auto* def = registry_.getSchema(request->record_name());
    if (!def) {
        return grpc::Status(grpc::StatusCode::NOT_FOUND,
                           "Unknown record: " + request->record_name());
    }

    populateSchema(response->mutable_schema(), *def);
    return grpc::Status::OK;
}

// ── ListSchemas ───────────────────────────────────────────────────────────

grpc::Status CopybookServiceImpl::ListSchemas(
        grpc::ServerContext* /*context*/,
        const ListSchemasRequest* /*request*/,
        ListSchemasResponse* response) {
    for (const auto* def : registry_.allSchemas()) {
        populateSchema(response->add_schemas(), *def);
    }
    return grpc::Status::OK;
}

// ── Convert ───────────────────────────────────────────────────────────────

grpc::Status CopybookServiceImpl::Convert(
        grpc::ServerContext* /*context*/,
        const ConvertRequest* request,
        ConvertResponse* response) {
    const auto& format = request->format();
    const auto& name   = request->record_name();

    if (format != "json" && format != "yaml") {
        return grpc::Status(grpc::StatusCode::INVALID_ARGUMENT,
                           "Format must be 'json' or 'yaml'");
    }

    if (request->has_payload()) {
        // Buffer → format
        const auto& payload = request->payload();
        auto record = registry_.createRecord(
            payload.record_name(),
            payload.raw_buffer().data(),
            payload.raw_buffer().size());
        if (!record) {
            return grpc::Status(grpc::StatusCode::NOT_FOUND,
                               "Unknown record: " + payload.record_name());
        }

        try {
            std::string output;
            if (format == "json") {
                output = JsonSerializer::toJson(*record);
            } else {
                output = YamlSerializer::toYaml(*record);
            }
            response->set_format_data(output);
        } catch (const std::exception& e) {
            return grpc::Status(grpc::StatusCode::INTERNAL,
                               std::string("Serialization error: ") + e.what());
        }
    }
    else if (!request->format_data().empty()) {
        // Format → buffer
        if (name.empty()) {
            return grpc::Status(grpc::StatusCode::INVALID_ARGUMENT,
                               "record_name required when converting from format");
        }

        auto record = registry_.createBlankRecord(name);
        if (!record) {
            return grpc::Status(grpc::StatusCode::NOT_FOUND,
                               "Unknown record: " + name);
        }

        try {
            if (format == "json") {
                JsonSerializer::fromJson(*record, request->format_data());
            } else {
                YamlSerializer::fromYaml(*record, request->format_data());
            }
            record->syncChildren();
        } catch (const std::exception& e) {
            return grpc::Status(grpc::StatusCode::INVALID_ARGUMENT,
                               std::string("Parse error: ") + e.what());
        }

        auto* payload = response->mutable_payload();
        payload->set_record_name(name);
        std::string buf = record->getData();
        payload->set_raw_buffer(buf);
        payload->set_buffer_size(buf.size());
    }
    else {
        return grpc::Status(grpc::StatusCode::INVALID_ARGUMENT,
                           "Either payload or format_data must be provided");
    }

    return grpc::Status::OK;
}

// ── StreamRecords ─────────────────────────────────────────────────────────

grpc::Status CopybookServiceImpl::StreamRecords(
        grpc::ServerContext* /*context*/,
        grpc::ServerReaderWriter<SendResponse, SendRequest>* stream) {
    SendRequest req;
    int count = 0;

    while (stream->Read(&req)) {
        const auto& payload = req.payload();
        const auto& name    = payload.record_name();
        const auto& raw     = payload.raw_buffer();

        SendResponse resp;

        const auto* schema = registry_.getSchema(name);
        if (!schema) {
            resp.set_success(false);
            resp.set_message("Unknown record: " + name);
        } else {
            {
                std::lock_guard<std::mutex> lock(mutex_);
                stored_buffers_[name] = raw;
            }
            count++;
            resp.set_success(true);
            resp.set_message("Record #" + std::to_string(count) + ": " +
                            name + " (" + std::to_string(raw.size()) + " bytes)");
        }

        stream->Write(resp);
    }

    return grpc::Status::OK;
}

// ── Schema helpers ────────────────────────────────────────────────────────

void CopybookServiceImpl::populateFieldDescriptor(
        ::copybook::transport::FieldDescriptor* proto,
        const CopybookField& field) {
    proto->set_name(field.cpp_name);
    proto->set_cobol_name(field.cobol_name);
    proto->set_pic_clause(field.pic_clause);
    proto->set_type(fieldTypeName(field.type));
    proto->set_offset(field.offset);
    proto->set_size(field.size);
    proto->set_decimal_positions(field.decimal_positions);
    proto->set_is_group(field.is_group);
    proto->set_is_filler(field.is_filler);

    for (const auto& child : field.children) {
        populateFieldDescriptor(proto->add_children(), child);
    }
}

void CopybookServiceImpl::populateSchema(RecordSchema* schema,
                                          const CopybookDefinition& def) {
    schema->set_record_name(def.record_name);
    schema->set_cpp_class_name(def.cpp_class_name);
    schema->set_total_size(def.total_size);

    for (const auto& f : def.fields) {
        populateFieldDescriptor(schema->add_fields(), f);
    }
}

// ── Server startup ────────────────────────────────────────────────────────

void RunServer(const std::string& address, RecordRegistry& registry) {
    CopybookServiceImpl service(registry);

    grpc::ServerBuilder builder;
    builder.AddListeningPort(address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);

    auto server = builder.BuildAndStart();
    std::cout << "CopybookService listening on " << address << "\n";
    server->Wait();
}

std::unique_ptr<grpc::Server> StartServer(const std::string& address,
                                           RecordRegistry& registry) {
    auto service = std::make_unique<CopybookServiceImpl>(registry);

    grpc::ServerBuilder builder;
    builder.AddListeningPort(address, grpc::InsecureServerCredentials());
    builder.RegisterService(service.get());

    auto server = builder.BuildAndStart();
    std::cout << "CopybookService listening on " << address << "\n";

    // Note: the service must outlive the server. In production code,
    // you'd manage lifetimes more carefully. For the server executable,
    // we use RunServer() which keeps both alive.
    // This function is primarily for testing.
    service.release();  // server takes ownership conceptually
    return server;
}

} // namespace copybook::transport
