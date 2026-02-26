#pragma once

#include <cstddef>
#include <stdexcept>
#include <string>
#include <vector>

namespace copybook {

/// Fixed-length character buffer that mirrors a COBOL record's raw byte layout.
///
/// This is the C++ equivalent of the Java `char[] data_elements` array used
/// throughout the original project. It adds bounds checking and the `merge()`
/// operation that the Java version was missing (child buffers were copied out
/// but never written back).
class RecordBuffer {
public:
    /// Construct a buffer of `size` bytes, filled with `fill` (default: space).
    explicit RecordBuffer(size_t size, char fill = ' ');

    /// Construct from raw data. The buffer copies the data.
    RecordBuffer(const char* raw, size_t size);

    /// Construct from a vector.
    explicit RecordBuffer(const std::vector<char>& raw);

    /// Write `value` into the buffer at `offset` for `length` bytes.
    /// If value is shorter than length, it is right-padded with `pad`.
    /// If value is longer than length, it is truncated to length.
    /// Throws std::out_of_range if offset+length exceeds buffer size.
    void write(int offset, int length, const std::string& value, char pad = ' ');

    /// Read `length` bytes starting at `offset` as a string.
    /// Throws std::out_of_range if offset+length exceeds buffer size.
    std::string read(int offset, int length) const;

    /// Extract a sub-buffer (copy) starting at `offset` for `length` bytes.
    /// Used to initialize child record objects.
    RecordBuffer slice(int offset, int length) const;

    /// Write a child buffer back into this buffer at the given offset.
    /// This is the fix for the Java version's buffer-sync bug: child object
    /// mutations are merged back into the parent before serialization.
    void merge(int offset, const RecordBuffer& child);

    /// Direct access to the underlying data.
    char*       raw();
    const char* raw() const;

    /// Buffer size in bytes.
    size_t size() const;

    /// Return the entire buffer as a string.
    std::string toString() const;

private:
    std::vector<char> data_;

    void boundsCheck(int offset, int length) const;
};

} // namespace copybook
