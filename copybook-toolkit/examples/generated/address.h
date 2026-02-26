#pragma once
// Generated from ADDRESS copybook definition
// Source: com.tgp.object.data.AddressImpl (Java original)

#include <copybook/core/record_base.h>

namespace copybook { namespace examples {

class Address : public RecordBase {
public:
    static constexpr int RECORD_SIZE = 12;

    Address() : RecordBase(RECORD_SIZE) { initFields(); }
    Address(const char* raw, size_t len) : RecordBase(raw, len) { initFields(); }

private:
    void initFields() {
        registerField({"ADDRESS1", 12, 0, FieldType::CHARACTER});
    }
};

}} // namespace copybook::examples
