#include <gtest/gtest.h>
#include <copybook/core/record_buffer.h>

using namespace copybook;

// ── Construction ───────────────────────────────────────────────────────────

TEST(RecordBufferTest, ConstructWithSizeAndFill) {
    RecordBuffer buf(10, ' ');
    EXPECT_EQ(buf.size(), 10u);
    EXPECT_EQ(buf.toString(), "          ");
}

TEST(RecordBufferTest, ConstructWithDefaultFill) {
    RecordBuffer buf(5);
    EXPECT_EQ(buf.toString(), "     ");
}

TEST(RecordBufferTest, ConstructFromRawData) {
    const char raw[] = "Hello";
    RecordBuffer buf(raw, 5);
    EXPECT_EQ(buf.size(), 5u);
    EXPECT_EQ(buf.toString(), "Hello");
}

TEST(RecordBufferTest, ConstructFromVector) {
    std::vector<char> v = {'A', 'B', 'C'};
    RecordBuffer buf(v);
    EXPECT_EQ(buf.size(), 3u);
    EXPECT_EQ(buf.toString(), "ABC");
}

// ── Write ──────────────────────────────────────────────────────────────────

TEST(RecordBufferTest, WriteExactFit) {
    RecordBuffer buf(10);
    buf.write(0, 5, "Hello");
    EXPECT_EQ(buf.read(0, 5), "Hello");
}

TEST(RecordBufferTest, WriteShorterValuePadsWithSpaces) {
    RecordBuffer buf(10);
    buf.write(0, 10, "Hi");
    EXPECT_EQ(buf.toString(), "Hi        ");
}

TEST(RecordBufferTest, WriteShorterValuePadsWithCustomChar) {
    RecordBuffer buf(10);
    buf.write(0, 10, "Hi", '0');
    EXPECT_EQ(buf.toString(), "Hi00000000");
}

TEST(RecordBufferTest, WriteLongerValueTruncates) {
    RecordBuffer buf(10);
    buf.write(0, 5, "HelloWorld");
    EXPECT_EQ(buf.read(0, 5), "Hello");
    // Bytes after the field should still be spaces
    EXPECT_EQ(buf.read(5, 5), "     ");
}

TEST(RecordBufferTest, WriteAtOffset) {
    RecordBuffer buf(20);
    buf.write(5, 5, "Hello");
    EXPECT_EQ(buf.read(0, 5), "     ");
    EXPECT_EQ(buf.read(5, 5), "Hello");
    EXPECT_EQ(buf.read(10, 10), "          ");
}

TEST(RecordBufferTest, WriteEmptyString) {
    RecordBuffer buf(5);
    buf.write(0, 5, "");
    EXPECT_EQ(buf.toString(), "     ");
}

TEST(RecordBufferTest, WriteThrowsOnOutOfBounds) {
    RecordBuffer buf(10);
    EXPECT_THROW(buf.write(8, 5, "Hello"), std::out_of_range);
}

TEST(RecordBufferTest, WriteThrowsOnNegativeOffset) {
    RecordBuffer buf(10);
    EXPECT_THROW(buf.write(-1, 5, "Hello"), std::out_of_range);
}

// ── Read ───────────────────────────────────────────────────────────────────

TEST(RecordBufferTest, ReadThrowsOnOutOfBounds) {
    RecordBuffer buf(10);
    EXPECT_THROW(buf.read(8, 5), std::out_of_range);
}

TEST(RecordBufferTest, ReadReturnsExactBytes) {
    const char raw[] = "ABCDEFGHIJ";
    RecordBuffer buf(raw, 10);
    EXPECT_EQ(buf.read(3, 4), "DEFG");
}

// ── Slice ──────────────────────────────────────────────────────────────────

TEST(RecordBufferTest, SliceReturnsIndependentCopy) {
    const char raw[] = "ABCDEFGHIJ";
    RecordBuffer buf(raw, 10);

    RecordBuffer slice = buf.slice(2, 4);
    EXPECT_EQ(slice.size(), 4u);
    EXPECT_EQ(slice.toString(), "CDEF");

    // Modifying the slice should not affect the original
    slice.write(0, 4, "XXXX");
    EXPECT_EQ(buf.read(2, 4), "CDEF");
    EXPECT_EQ(slice.toString(), "XXXX");
}

TEST(RecordBufferTest, SliceThrowsOnOutOfBounds) {
    RecordBuffer buf(10);
    EXPECT_THROW(buf.slice(8, 5), std::out_of_range);
}

// ── Merge ──────────────────────────────────────────────────────────────────

TEST(RecordBufferTest, MergeWritesChildBackToParent) {
    RecordBuffer parent(20);
    parent.write(0, 20, "AAAAAAAAAABBBBBBBBBB");

    RecordBuffer child(5);
    child.write(0, 5, "XXXXX");

    parent.merge(5, child);
    EXPECT_EQ(parent.toString(), "AAAAAXXXXXBBBBBBBBBB");
}

TEST(RecordBufferTest, MergeThrowsOnOutOfBounds) {
    RecordBuffer parent(10);
    RecordBuffer child(8);
    EXPECT_THROW(parent.merge(5, child), std::out_of_range);
}

// ── Raw access ─────────────────────────────────────────────────────────────

TEST(RecordBufferTest, RawPointerAccess) {
    RecordBuffer buf(5);
    buf.write(0, 5, "Hello");
    EXPECT_EQ(buf.raw()[0], 'H');
    EXPECT_EQ(buf.raw()[4], 'o');
}
