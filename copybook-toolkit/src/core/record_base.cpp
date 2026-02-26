#include "copybook/core/record_base.h"

#include <algorithm>
#include <iomanip>
#include <sstream>
#include <stdexcept>

namespace copybook {

// ── Construction ───────────────────────────────────────────────────────────

RecordBase::RecordBase(size_t recordSize)
    : buffer_(recordSize) {}

RecordBase::RecordBase(const char* raw, size_t recordSize)
    : buffer_(raw, recordSize) {}

// ── Field registration (called by subclass constructors) ───────────────────

void RecordBase::registerField(const FieldDescriptor& fd) {
    fields_[fd.name] = fd;
    field_order_.push_back(fd.name);
}

void RecordBase::registerChild(const std::string& name, int offset, int size,
                               std::shared_ptr<RecordBase> child) {
    children_[name] = child;
    child_entries_.push_back({offset, size, child});
}

// ── Field lookup ───────────────────────────────────────────────────────────

const FieldDescriptor& RecordBase::lookupField(const std::string& fieldName) const {
    auto it = fields_.find(fieldName);
    if (it == fields_.end()) {
        throw std::invalid_argument(
            "Field '" + fieldName + "' is not registered in this record");
    }
    return it->second;
}

// ── String field access ────────────────────────────────────────────────────

void RecordBase::setData(const std::string& fieldName, const std::string& value) {
    const auto& fd = lookupField(fieldName);
    buffer_.write(fd.offset, fd.size, value, ' ');
}

std::string RecordBase::getData(const std::string& fieldName) const {
    const auto& fd = lookupField(fieldName);
    return buffer_.read(fd.offset, fd.size);
}

std::string RecordBase::getData() const {
    return buffer_.toString();
}

// ── Numeric field access ───────────────────────────────────────────────────

void RecordBase::setNumeric(const std::string& fieldName, long value) {
    const auto& fd = lookupField(fieldName);

    // Right-justify, zero-pad to field width
    std::ostringstream oss;
    oss << std::setw(fd.size) << std::setfill('0') << value;
    std::string formatted = oss.str();

    // If the formatted number is longer than the field, take the rightmost digits
    if (static_cast<int>(formatted.size()) > fd.size) {
        formatted = formatted.substr(formatted.size() - fd.size);
    }

    buffer_.write(fd.offset, fd.size, formatted, '0');
}

long RecordBase::getNumeric(const std::string& fieldName) const {
    const auto& fd = lookupField(fieldName);
    std::string raw = buffer_.read(fd.offset, fd.size);

    // Strip leading spaces and zeros, parse as long
    size_t start = raw.find_first_not_of(" 0");
    if (start == std::string::npos) {
        return 0;
    }

    try {
        return std::stol(raw.substr(start));
    } catch (const std::exception&) {
        return 0;
    }
}

// ── Field introspection ────────────────────────────────────────────────────

bool RecordBase::hasField(const std::string& fieldName) const {
    return fields_.find(fieldName) != fields_.end();
}

std::vector<std::string> RecordBase::fieldNames() const {
    return field_order_;
}

const FieldDescriptor& RecordBase::fieldInfo(const std::string& fieldName) const {
    return lookupField(fieldName);
}

size_t RecordBase::recordSize() const {
    return buffer_.size();
}

// ── Child record management ────────────────────────────────────────────────

std::shared_ptr<RecordBase> RecordBase::getChild(const std::string& name) const {
    auto it = children_.find(name);
    if (it == children_.end()) {
        return nullptr;
    }
    return it->second;
}

void RecordBase::syncChildren() {
    for (auto& entry : child_entries_) {
        buffer_.merge(entry.offset, entry.record->buffer());
    }
}

void RecordBase::refreshChildren() {
    for (auto& entry : child_entries_) {
        RecordBuffer slice = buffer_.slice(entry.offset, entry.size);
        entry.record->loadFromBuffer(slice.raw(), slice.size());
    }
}

// ── Raw buffer access ──────────────────────────────────────────────────────

const RecordBuffer& RecordBase::buffer() const {
    return buffer_;
}

void RecordBase::loadFromBuffer(const char* raw, size_t size) {
    if (size != buffer_.size()) {
        throw std::invalid_argument(
            "Buffer size mismatch: expected " + std::to_string(buffer_.size()) +
            " but got " + std::to_string(size));
    }
    buffer_ = RecordBuffer(raw, size);
    refreshChildren();
}

} // namespace copybook
