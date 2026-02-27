#include <copybook/transport/grpc_client.h>

namespace copybook::transport {

CopybookClient::CopybookClient(const std::string& address)
    : stub_(CopybookService::NewStub(
          grpc::CreateChannel(address, grpc::InsecureChannelCredentials()))) {}

CopybookClient::CopybookClient(std::shared_ptr<grpc::Channel> channel)
    : stub_(CopybookService::NewStub(channel)) {}

// ── SendRecord ────────────────────────────────────────────────────────────

std::pair<bool, std::string>
CopybookClient::sendRecord(const std::string& name,
                            const char* raw, size_t size) {
    SendRequest req;
    auto* payload = req.mutable_payload();
    payload->set_record_name(name);
    payload->set_raw_buffer(raw, size);
    payload->set_buffer_size(size);

    SendResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->SendRecord(&ctx, req, &resp);

    if (!status.ok()) {
        return {false, "RPC failed: " + status.error_message()};
    }
    return {resp.success(), resp.message()};
}

// ── GetFields ─────────────────────────────────────────────────────────────

std::vector<std::pair<std::string, std::string>>
CopybookClient::getFields(const std::string& name,
                           const char* raw, size_t size) {
    GetFieldsRequest req;
    auto* payload = req.mutable_payload();
    payload->set_record_name(name);
    payload->set_raw_buffer(raw, size);
    payload->set_buffer_size(size);

    GetFieldsResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->GetFields(&ctx, req, &resp);

    std::vector<std::pair<std::string, std::string>> result;
    if (status.ok()) {
        for (const auto& fv : resp.record().fields()) {
            result.emplace_back(fv.name(), fv.value());
        }
    }
    return result;
}

// ── SetFields ─────────────────────────────────────────────────────────────

std::string CopybookClient::setFields(
        const std::string& name,
        const std::vector<std::pair<std::string, std::string>>& fields,
        int bufferSize) {
    SetFieldsRequest req;
    auto* msg = req.mutable_record();
    msg->set_record_name(name);
    for (const auto& [fname, fval] : fields) {
        auto* fv = msg->add_fields();
        fv->set_name(fname);
        fv->set_value(fval);
    }
    req.set_buffer_size(bufferSize);

    SetFieldsResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->SetFields(&ctx, req, &resp);

    if (status.ok()) {
        return resp.payload().raw_buffer();
    }
    return "";
}

// ── GetSchema ─────────────────────────────────────────────────────────────

std::unique_ptr<RecordSchema> CopybookClient::getSchema(const std::string& name) {
    SchemaRequest req;
    req.set_record_name(name);

    SchemaResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->GetSchema(&ctx, req, &resp);

    if (status.ok()) {
        return std::make_unique<RecordSchema>(resp.schema());
    }
    return nullptr;
}

// ── ListSchemas ───────────────────────────────────────────────────────────

std::vector<RecordSchema> CopybookClient::listSchemas() {
    ListSchemasRequest req;
    ListSchemasResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->ListSchemas(&ctx, req, &resp);

    std::vector<RecordSchema> result;
    if (status.ok()) {
        for (const auto& s : resp.schemas()) {
            result.push_back(s);
        }
    }
    return result;
}

// ── Convert (buffer → format) ─────────────────────────────────────────────

std::string CopybookClient::toJson(const std::string& name,
                                    const char* raw, size_t size) {
    ConvertRequest req;
    req.set_format("json");
    auto* payload = req.mutable_payload();
    payload->set_record_name(name);
    payload->set_raw_buffer(raw, size);
    payload->set_buffer_size(size);

    ConvertResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->Convert(&ctx, req, &resp);

    if (!status.ok()) {
        std::cerr << "toJson RPC error: " << status.error_message() << "\n";
    }
    return status.ok() ? resp.format_data() : "";
}

std::string CopybookClient::toYaml(const std::string& name,
                                    const char* raw, size_t size) {
    ConvertRequest req;
    req.set_format("yaml");
    auto* payload = req.mutable_payload();
    payload->set_record_name(name);
    payload->set_raw_buffer(raw, size);
    payload->set_buffer_size(size);

    ConvertResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->Convert(&ctx, req, &resp);

    return status.ok() ? resp.format_data() : "";
}

// ── Convert (format → buffer) ─────────────────────────────────────────────

std::string CopybookClient::fromJson(const std::string& name,
                                      const std::string& json) {
    ConvertRequest req;
    req.set_format("json");
    req.set_format_data(json);
    req.set_record_name(name);

    ConvertResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->Convert(&ctx, req, &resp);

    return status.ok() ? resp.payload().raw_buffer() : "";
}

std::string CopybookClient::fromYaml(const std::string& name,
                                      const std::string& yaml) {
    ConvertRequest req;
    req.set_format("yaml");
    req.set_format_data(yaml);
    req.set_record_name(name);

    ConvertResponse resp;
    grpc::ClientContext ctx;
    auto status = stub_->Convert(&ctx, req, &resp);

    return status.ok() ? resp.payload().raw_buffer() : "";
}

// ── StreamRecords ─────────────────────────────────────────────────────────

std::vector<std::pair<bool, std::string>>
CopybookClient::streamRecords(
        const std::vector<std::pair<std::string, std::string>>& records) {
    grpc::ClientContext ctx;
    auto stream = stub_->StreamRecords(&ctx);

    std::vector<std::pair<bool, std::string>> results;

    for (const auto& [name, buffer] : records) {
        SendRequest req;
        auto* payload = req.mutable_payload();
        payload->set_record_name(name);
        payload->set_raw_buffer(buffer);
        payload->set_buffer_size(buffer.size());
        stream->Write(req);

        SendResponse resp;
        if (stream->Read(&resp)) {
            results.emplace_back(resp.success(), resp.message());
        }
    }

    stream->WritesDone();
    stream->Finish();

    return results;
}

} // namespace copybook::transport
