// ═══════════════════════════════════════════════════════════════════════════
// Copybook Visual Designer — Lightweight C++ Web Application
//
// A self-contained HTTP server that serves an HTML5/SVG visual class diagram
// designer for COBOL copybook definitions. Integrates with the copybook
// parser, code generator, and serialization libraries.
//
// Usage:  ./web-designer [port] [copybook-dir]
//         Defaults: port=8080, copybook-dir=./examples/copybooks
// ═══════════════════════════════════════════════════════════════════════════

#include <copybook/parser/copybook_parser.h>
#include <copybook/parser/codegen.h>
#include <copybook/core/field_type.h>
#include <nlohmann/json.hpp>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

#include <algorithm>
#include <atomic>
#include <csignal>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

#include "frontend.h"

using json = nlohmann::json;
using ordered_json = nlohmann::ordered_json;
namespace fs = std::filesystem;

// ── Global state ──────────────────────────────────────────────────────────

static std::vector<copybook::CopybookDefinition> g_copybooks;
static std::mutex g_mutex;
static std::atomic<bool> g_running{true};
static std::string g_copybook_dir;

// ── HTTP request/response types ───────────────────────────────────────────

struct HttpRequest {
    std::string method;
    std::string path;
    std::string body;
    std::map<std::string, std::string> headers;
};

struct HttpResponse {
    int status = 200;
    std::string content_type = "text/plain";
    std::string body;
};

// ── JSON conversion helpers ───────────────────────────────────────────────

static json fieldToJson(const copybook::CopybookField& f) {
    json j;
    j["name"]              = f.cpp_name;
    j["cobol_name"]        = f.cobol_name;
    j["pic_clause"]        = f.pic_clause;
    j["offset"]            = f.offset;
    j["size"]              = f.size;
    j["type"]              = copybook::fieldTypeName(f.type);
    j["decimal_positions"] = f.decimal_positions;
    j["is_group"]          = f.is_group;
    j["is_filler"]         = f.is_filler;
    if (!f.children.empty()) {
        j["children"] = json::array();
        for (const auto& c : f.children) {
            j["children"].push_back(fieldToJson(c));
        }
    } else {
        j["children"] = json::array();
    }
    return j;
}

static json copybookToJson(const copybook::CopybookDefinition& def) {
    json j;
    j["name"]       = def.record_name;
    j["class_name"]  = def.cpp_class_name;
    j["total_size"]  = def.total_size;
    j["fields"]      = json::array();
    for (const auto& f : def.fields) {
        j["fields"].push_back(fieldToJson(f));
    }
    return j;
}

// ── YAML export for copybook schema ───────────────────────────────────────

static void emitYamlField(std::ostringstream& out,
                           const copybook::CopybookField& f, int depth) {
    std::string indent(depth * 2, ' ');
    out << indent << "- name: " << f.cpp_name << "\n";
    out << indent << "  cobol_name: " << f.cobol_name << "\n";
    if (!f.pic_clause.empty())
        out << indent << "  pic: \"" << f.pic_clause << "\"\n";
    out << indent << "  type: " << copybook::fieldTypeName(f.type) << "\n";
    out << indent << "  offset: " << f.offset << "\n";
    out << indent << "  size: " << f.size << "\n";
    if (f.decimal_positions > 0)
        out << indent << "  decimal_positions: " << f.decimal_positions << "\n";
    if (f.is_group && !f.children.empty()) {
        out << indent << "  children:\n";
        for (const auto& c : f.children) {
            emitYamlField(out, c, depth + 2);
        }
    }
}

static std::string exportAsYaml(const copybook::CopybookDefinition& def) {
    std::ostringstream out;
    out << "# Copybook Schema: " << def.record_name << "\n";
    out << "record: " << def.record_name << "\n";
    out << "class: " << def.cpp_class_name << "\n";
    out << "total_size: " << def.total_size << "\n";
    out << "fields:\n";
    for (const auto& f : def.fields) {
        emitYamlField(out, f, 1);
    }
    return out.str();
}

// ── Load all .cpy files from a directory ──────────────────────────────────

static void loadCopybooksFromDir(const std::string& dir) {
    copybook::CopybookParser parser;
    std::lock_guard<std::mutex> lock(g_mutex);
    g_copybooks.clear();

    if (!fs::exists(dir) || !fs::is_directory(dir)) {
        std::cerr << "Warning: copybook directory not found: " << dir << "\n";
        return;
    }

    std::vector<std::string> files;
    for (const auto& entry : fs::directory_iterator(dir)) {
        if (entry.is_regular_file()) {
            auto ext = entry.path().extension().string();
            std::transform(ext.begin(), ext.end(), ext.begin(), ::tolower);
            if (ext == ".cpy" || ext == ".cbl" || ext == ".cob") {
                files.push_back(entry.path().string());
            }
        }
    }
    std::sort(files.begin(), files.end());

    for (const auto& path : files) {
        try {
            auto def = parser.parseFile(path);
            std::cout << "  Loaded: " << def.record_name
                      << " (" << def.total_size << " bytes, "
                      << def.fields.size() << " fields)\n";
            g_copybooks.push_back(std::move(def));
        } catch (const std::exception& e) {
            std::cerr << "  Warning: failed to parse " << path
                      << ": " << e.what() << "\n";
        }
    }
}

// ── HTTP parsing ──────────────────────────────────────────────────────────

static HttpRequest readRequest(int fd) {
    HttpRequest req;
    std::string data;
    char buf[4096];

    // Read until we find header terminator
    while (data.find("\r\n\r\n") == std::string::npos) {
        int n = recv(fd, buf, sizeof(buf), 0);
        if (n <= 0) return req;
        data.append(buf, n);
    }

    auto headerEnd = data.find("\r\n\r\n");
    std::string headers = data.substr(0, headerEnd);
    std::string bodyStart = data.substr(headerEnd + 4);

    // Parse request line
    auto lineEnd = headers.find("\r\n");
    std::string requestLine = headers.substr(0, lineEnd);
    auto sp1 = requestLine.find(' ');
    auto sp2 = requestLine.find(' ', sp1 + 1);
    if (sp1 == std::string::npos || sp2 == std::string::npos) return req;
    req.method = requestLine.substr(0, sp1);
    req.path   = requestLine.substr(sp1 + 1, sp2 - sp1 - 1);

    // Parse headers
    std::istringstream hs(headers.substr(lineEnd + 2));
    std::string line;
    while (std::getline(hs, line)) {
        if (!line.empty() && line.back() == '\r') line.pop_back();
        auto colon = line.find(':');
        if (colon != std::string::npos) {
            std::string key = line.substr(0, colon);
            std::string val = line.substr(colon + 1);
            while (!val.empty() && val[0] == ' ') val.erase(0, 1);
            std::transform(key.begin(), key.end(), key.begin(), ::tolower);
            req.headers[key] = val;
        }
    }

    // Read body based on Content-Length
    auto it = req.headers.find("content-length");
    if (it != req.headers.end()) {
        int contentLen = std::stoi(it->second);
        req.body = bodyStart;
        while (static_cast<int>(req.body.size()) < contentLen) {
            int n = recv(fd, buf, sizeof(buf), 0);
            if (n <= 0) break;
            req.body.append(buf, n);
        }
        req.body.resize(std::min(static_cast<int>(req.body.size()), contentLen));
    }

    return req;
}

static void sendResponse(int fd, const HttpResponse& resp) {
    std::string statusText;
    switch (resp.status) {
        case 200: statusText = "OK"; break;
        case 400: statusText = "Bad Request"; break;
        case 404: statusText = "Not Found"; break;
        case 500: statusText = "Internal Server Error"; break;
        default:  statusText = "OK"; break;
    }

    std::ostringstream out;
    out << "HTTP/1.1 " << resp.status << " " << statusText << "\r\n"
        << "Content-Type: " << resp.content_type << "\r\n"
        << "Content-Length: " << resp.body.size() << "\r\n"
        << "Connection: close\r\n"
        << "\r\n"
        << resp.body;

    std::string raw = out.str();
    size_t sent = 0;
    while (sent < raw.size()) {
        int n = send(fd, raw.c_str() + sent, raw.size() - sent, 0);
        if (n <= 0) break;
        sent += n;
    }
}

static HttpResponse jsonResponse(int status, const json& j) {
    return { status, "application/json", j.dump(2) };
}

static HttpResponse jsonError(int status, const std::string& msg) {
    json j;
    j["error"] = msg;
    return jsonResponse(status, j);
}

// ── API handlers ──────────────────────────────────────────────────────────

static HttpResponse handleGetCopybooks() {
    std::lock_guard<std::mutex> lock(g_mutex);
    json j;
    j["copybooks"] = json::array();
    for (const auto& def : g_copybooks) {
        j["copybooks"].push_back(copybookToJson(def));
    }
    return jsonResponse(200, j);
}

static HttpResponse handleGenerate(const std::string& body) {
    try {
        auto req = json::parse(body);
        int idx = req.value("index", -1);

        std::lock_guard<std::mutex> lock(g_mutex);
        if (idx < 0 || idx >= static_cast<int>(g_copybooks.size())) {
            return jsonError(400, "Invalid copybook index");
        }

        copybook::Codegen codegen;
        copybook::CodegenOptions opts;
        std::string code = codegen.generateHeader(g_copybooks[idx], opts);

        json j;
        j["code"] = code;
        return jsonResponse(200, j);
    } catch (const std::exception& e) {
        return jsonError(500, std::string("Generate error: ") + e.what());
    }
}

static HttpResponse handleExportJson(const std::string& body) {
    try {
        auto req = json::parse(body);
        int idx = req.value("index", -1);

        std::lock_guard<std::mutex> lock(g_mutex);
        if (idx < 0 || idx >= static_cast<int>(g_copybooks.size())) {
            return jsonError(400, "Invalid copybook index");
        }

        json schema;
        schema["record"]     = g_copybooks[idx].record_name;
        schema["class"]      = g_copybooks[idx].cpp_class_name;
        schema["total_size"] = g_copybooks[idx].total_size;
        schema["fields"]     = json::array();
        for (const auto& f : g_copybooks[idx].fields) {
            schema["fields"].push_back(fieldToJson(f));
        }

        json j;
        j["output"] = schema.dump(2);
        return jsonResponse(200, j);
    } catch (const std::exception& e) {
        return jsonError(500, std::string("Export error: ") + e.what());
    }
}

static HttpResponse handleExportYaml(const std::string& body) {
    try {
        auto req = json::parse(body);
        int idx = req.value("index", -1);

        std::lock_guard<std::mutex> lock(g_mutex);
        if (idx < 0 || idx >= static_cast<int>(g_copybooks.size())) {
            return jsonError(400, "Invalid copybook index");
        }

        json j;
        j["output"] = exportAsYaml(g_copybooks[idx]);
        return jsonResponse(200, j);
    } catch (const std::exception& e) {
        return jsonError(500, std::string("Export error: ") + e.what());
    }
}

static HttpResponse handleParse(const std::string& body) {
    try {
        auto req = json::parse(body);
        std::string source = req.value("source", "");
        if (source.empty()) {
            return jsonError(400, "Missing 'source' field");
        }

        copybook::CopybookParser parser;
        auto def = parser.parseString(source);

        // Add to global list
        {
            std::lock_guard<std::mutex> lock(g_mutex);
            g_copybooks.push_back(def);
        }

        return jsonResponse(200, copybookToJson(def));
    } catch (const std::exception& e) {
        return jsonError(400, std::string("Parse error: ") + e.what());
    }
}

// ── Request router ────────────────────────────────────────────────────────

static void handleClient(int fd) {
    auto req = readRequest(fd);
    HttpResponse resp;

    if (req.method == "GET" && req.path == "/") {
        resp = { 200, "text/html; charset=utf-8", FRONTEND_HTML };
    }
    else if (req.method == "GET" && req.path == "/api/copybooks") {
        resp = handleGetCopybooks();
    }
    else if (req.method == "POST" && req.path == "/api/generate") {
        resp = handleGenerate(req.body);
    }
    else if (req.method == "POST" && req.path == "/api/export/json") {
        resp = handleExportJson(req.body);
    }
    else if (req.method == "POST" && req.path == "/api/export/yaml") {
        resp = handleExportYaml(req.body);
    }
    else if (req.method == "POST" && req.path == "/api/parse") {
        resp = handleParse(req.body);
    }
    else {
        resp = { 404, "text/plain", "Not Found" };
    }

    sendResponse(fd, resp);
    close(fd);
}

// ── Signal handler ────────────────────────────────────────────────────────

static void signalHandler(int) {
    g_running = false;
}

// ── Main ──────────────────────────────────────────────────────────────────

int main(int argc, char* argv[]) {
    int port = 8080;
    g_copybook_dir = "examples/copybooks";

    if (argc > 1) port = std::atoi(argv[1]);
    if (argc > 2) g_copybook_dir = argv[2];

    std::signal(SIGINT, signalHandler);
    std::signal(SIGTERM, signalHandler);

    // Load copybooks
    std::cout << "Loading copybooks from: " << g_copybook_dir << "\n";
    loadCopybooksFromDir(g_copybook_dir);
    std::cout << "Loaded " << g_copybooks.size() << " copybook(s).\n\n";

    // Create server socket
    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        std::cerr << "Error: cannot create socket\n";
        return 1;
    }

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr{};
    addr.sin_family      = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port        = htons(port);

    if (bind(server_fd, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr)) < 0) {
        std::cerr << "Error: cannot bind to port " << port << "\n";
        close(server_fd);
        return 1;
    }

    if (listen(server_fd, 16) < 0) {
        std::cerr << "Error: listen failed\n";
        close(server_fd);
        return 1;
    }

    std::cout << "╔══════════════════════════════════════════════════╗\n"
              << "║   Copybook Visual Designer                      ║\n"
              << "║   http://localhost:" << port;
    // Pad to fit box
    int urlLen = 21 + std::to_string(port).size();
    for (int i = urlLen; i < 34; i++) std::cout << ' ';
    std::cout << "║\n"
              << "║   Press Ctrl+C to stop                          ║\n"
              << "╚══════════════════════════════════════════════════╝\n\n";

    // Accept loop
    while (g_running) {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);

        // Use a short timeout so we can check g_running
        struct timeval tv{};
        tv.tv_sec = 1;
        fd_set readfds;
        FD_ZERO(&readfds);
        FD_SET(server_fd, &readfds);

        int sel = select(server_fd + 1, &readfds, nullptr, nullptr, &tv);
        if (sel <= 0) continue;

        int client_fd = accept(server_fd,
                               reinterpret_cast<struct sockaddr*>(&client_addr),
                               &client_len);
        if (client_fd < 0) continue;

        // Handle each client in a detached thread
        std::thread([client_fd]() {
            handleClient(client_fd);
        }).detach();
    }

    close(server_fd);
    std::cout << "\nServer stopped.\n";
    return 0;
}
