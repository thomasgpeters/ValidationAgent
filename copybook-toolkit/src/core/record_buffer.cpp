#include "copybook/core/record_buffer.h"

#include <algorithm>
#include <sstream>

namespace copybook {

RecordBuffer::RecordBuffer(size_t size, char fill)
    : data_(size, fill) {}

RecordBuffer::RecordBuffer(const char* raw, size_t size)
    : data_(raw, raw + size) {}

RecordBuffer::RecordBuffer(const std::vector<char>& raw)
    : data_(raw) {}

void RecordBuffer::boundsCheck(int offset, int length) const {
    if (offset < 0 || length < 0) {
        std::ostringstream oss;
        oss << "Negative offset (" << offset << ") or length (" << length << ")";
        throw std::out_of_range(oss.str());
    }
    if (static_cast<size_t>(offset + length) > data_.size()) {
        std::ostringstream oss;
        oss << "Buffer access [" << offset << ".." << (offset + length)
            << ") exceeds buffer size " << data_.size();
        throw std::out_of_range(oss.str());
    }
}

void RecordBuffer::write(int offset, int length, const std::string& value, char pad) {
    boundsCheck(offset, length);

    int copyLen = std::min(static_cast<int>(value.size()), length);

    // Copy the value bytes
    for (int i = 0; i < copyLen; ++i) {
        data_[offset + i] = value[i];
    }

    // Pad the remainder
    for (int i = copyLen; i < length; ++i) {
        data_[offset + i] = pad;
    }
}

std::string RecordBuffer::read(int offset, int length) const {
    boundsCheck(offset, length);
    return std::string(data_.data() + offset, length);
}

RecordBuffer RecordBuffer::slice(int offset, int length) const {
    boundsCheck(offset, length);
    return RecordBuffer(data_.data() + offset, length);
}

void RecordBuffer::merge(int offset, const RecordBuffer& child) {
    boundsCheck(offset, static_cast<int>(child.size()));
    std::copy(child.data_.begin(), child.data_.end(), data_.begin() + offset);
}

char* RecordBuffer::raw() {
    return data_.data();
}

const char* RecordBuffer::raw() const {
    return data_.data();
}

size_t RecordBuffer::size() const {
    return data_.size();
}

std::string RecordBuffer::toString() const {
    return std::string(data_.begin(), data_.end());
}

} // namespace copybook
