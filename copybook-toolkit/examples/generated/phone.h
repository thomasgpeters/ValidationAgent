#pragma once
// Generated from PHONE copybook definition
// Source: com.tgp.object.data.PhoneImpl (Java original)

#include <copybook/core/record_base.h>

namespace copybook { namespace examples {

class Phone : public RecordBase {
public:
    static constexpr int RECORD_SIZE = 12;

    Phone() : RecordBase(RECORD_SIZE) { initFields(); }
    Phone(const char* raw, size_t len) : RecordBase(raw, len) { initFields(); }

private:
    void initFields() {
        registerField({"NUMBER", 12, 0, FieldType::CHARACTER});
    }
};

}} // namespace copybook::examples
