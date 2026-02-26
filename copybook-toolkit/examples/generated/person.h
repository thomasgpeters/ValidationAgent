#pragma once
// Generated from PERSON copybook definition
// Source: com.tgp.object.data.PersonImpl (Java original)
//
// COBOL layout:
//   01 PERSON.
//       05 ID               PIC X(12).
//       05 FIRST-NAME       PIC X(32).
//       05 LAST-NAME        PIC X(32).
//       05 MIDDLE-INITIAL   PIC X(1).
//       05 AGE              PIC 9(3).
//       05 SEX              PIC X(1).
//       05 DATE-OF-BIRTH    PIC X(10).
//       05 HOME-PHONE.
//           10 NUMBER       PIC X(12).
//       05 WORK-PHONE.
//           10 NUMBER       PIC X(12).
//   Total: 115 bytes

#include <copybook/core/record_base.h>
#include "phone.h"

#include <memory>

namespace copybook { namespace examples {

class Person : public RecordBase {
public:
    static constexpr int RECORD_SIZE = 115;

    // Field sizes (mirrors Java static final int constants)
    static constexpr int ID_SIZE              = 12;
    static constexpr int FIRST_NAME_SIZE      = 32;
    static constexpr int LAST_NAME_SIZE       = 32;
    static constexpr int MIDDLE_INITIAL_SIZE  = 1;
    static constexpr int AGE_SIZE             = 3;
    static constexpr int SEX_SIZE             = 1;
    static constexpr int DATE_OF_BIRTH_SIZE   = 10;
    static constexpr int HOME_PHONE_SIZE      = 12;
    static constexpr int WORK_PHONE_SIZE      = 12;

    // Field offsets (mirrors Java static final int constants)
    static constexpr int ID_OFFSET             = 0;
    static constexpr int FIRST_NAME_OFFSET     = 12;
    static constexpr int LAST_NAME_OFFSET      = 44;
    static constexpr int MIDDLE_INITIAL_OFFSET = 76;
    static constexpr int AGE_OFFSET            = 77;
    static constexpr int SEX_OFFSET            = 80;
    static constexpr int DATE_OF_BIRTH_OFFSET  = 81;
    static constexpr int HOME_PHONE_OFFSET     = 91;
    static constexpr int WORK_PHONE_OFFSET     = 103;

    Person() : RecordBase(RECORD_SIZE) {
        initFields();
        initChildren();
    }

    Person(const char* raw, size_t len) : RecordBase(raw, len) {
        initFields();
        initChildren();
    }

    Phone& homePhone() { return *home_phone_; }
    Phone& workPhone() { return *work_phone_; }

private:
    std::shared_ptr<Phone> home_phone_;
    std::shared_ptr<Phone> work_phone_;

    void initFields() {
        registerField({"ID",              ID_SIZE,              ID_OFFSET,              FieldType::CHARACTER});
        registerField({"FIRST_NAME",      FIRST_NAME_SIZE,      FIRST_NAME_OFFSET,      FieldType::CHARACTER});
        registerField({"LAST_NAME",       LAST_NAME_SIZE,       LAST_NAME_OFFSET,       FieldType::CHARACTER});
        registerField({"MIDDLE_INITIAL",  MIDDLE_INITIAL_SIZE,  MIDDLE_INITIAL_OFFSET,  FieldType::CHARACTER});
        registerField({"AGE",             AGE_SIZE,             AGE_OFFSET,             FieldType::ZONED_UNSIGNED});
        registerField({"SEX",             SEX_SIZE,             SEX_OFFSET,             FieldType::CHARACTER});
        registerField({"DATE_OF_BIRTH",   DATE_OF_BIRTH_SIZE,   DATE_OF_BIRTH_OFFSET,   FieldType::CHARACTER});
        registerField({"HOME_PHONE",      HOME_PHONE_SIZE,      HOME_PHONE_OFFSET,      FieldType::RECORD});
        registerField({"WORK_PHONE",      WORK_PHONE_SIZE,      WORK_PHONE_OFFSET,      FieldType::RECORD});
    }

    void initChildren() {
        // Extract child buffers from the parent — then register them so
        // syncChildren() will merge them back (fixing the Java buffer-sync bug).
        RecordBuffer homeSlice = buffer_.slice(HOME_PHONE_OFFSET, HOME_PHONE_SIZE);
        home_phone_ = std::make_shared<Phone>(homeSlice.raw(), homeSlice.size());
        registerChild("HOME_PHONE", HOME_PHONE_OFFSET, HOME_PHONE_SIZE, home_phone_);

        RecordBuffer workSlice = buffer_.slice(WORK_PHONE_OFFSET, WORK_PHONE_SIZE);
        work_phone_ = std::make_shared<Phone>(workSlice.raw(), workSlice.size());
        registerChild("WORK_PHONE", WORK_PHONE_OFFSET, WORK_PHONE_SIZE, work_phone_);
    }
};

}} // namespace copybook::examples
