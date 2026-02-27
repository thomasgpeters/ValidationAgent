// ═══════════════════════════════════════════════════════════════════════════
// Copybook gRPC Server
//
// Starts a gRPC server that provides remote access to copybook records.
//
// Usage:  ./grpc-server [port] [copybook-dir]
//         Defaults: port=50051, copybook-dir=./examples/copybooks
// ═══════════════════════════════════════════════════════════════════════════

#include <copybook/transport/record_transport.h>
#include <copybook/transport/grpc_service.h>

#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
    int port = 50051;
    std::string copybookDir = "examples/copybooks";

    if (argc > 1) port = std::atoi(argv[1]);
    if (argc > 2) copybookDir = argv[2];

    // Load copybooks
    copybook::transport::RecordRegistry registry;
    std::cout << "Loading copybooks from: " << copybookDir << "\n";
    registry.loadDirectory(copybookDir);

    auto schemas = registry.allSchemas();
    std::cout << "Loaded " << schemas.size() << " copybook(s):";
    for (const auto* s : schemas) {
        std::cout << " " << s->record_name
                  << "(" << s->total_size << "B)";
    }
    std::cout << "\n\n";

    // Start server
    std::string address = "0.0.0.0:" + std::to_string(port);
    std::cout << "Starting CopybookService on " << address << "\n";
    copybook::transport::RunServer(address, registry);

    return 0;
}
