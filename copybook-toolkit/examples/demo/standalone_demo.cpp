/// Standalone demo — COBOL copybook record manipulation without any server.
///
/// This is the C++ equivalent of the Java TestValidate.main() and TestClass.test1().
/// It demonstrates:
///   1. Creating a record from a copybook definition
///   2. Setting/getting fields by name (replaces Java reflection)
///   3. Numeric field access
///   4. Child record (GROUP item) access with buffer synchronization
///   5. Full buffer round-trip (serialize → deserialize → verify)

#include <copybook/core/record_base.h>
#include "person.h"

#include <iostream>
#include <iomanip>
#include <cassert>

using namespace copybook;
using namespace copybook::examples;

int main() {
    std::cout << "=== COBOL Copybook Toolkit — Standalone Demo ===" << std::endl;
    std::cout << std::endl;

    // ── 1. Create a Person record (115 bytes, space-filled) ────────────
    Person person;
    std::cout << "Created Person record: " << person.recordSize() << " bytes" << std::endl;
    std::cout << "Registered fields: " << person.fieldNames().size() << std::endl;
    std::cout << std::endl;

    // ── 2. Set fields by name (same API as Java setData) ───────────────
    person.setData("ID",             "EMP-001");
    person.setData("FIRST_NAME",     "Thomas");
    person.setData("LAST_NAME",      "Peters");
    person.setData("MIDDLE_INITIAL", "G");
    person.setData("SEX",            "M");
    person.setData("DATE_OF_BIRTH",  "1965-03-15");

    // ── 3. Numeric field access ────────────────────────────────────────
    person.setNumeric("AGE", 60);

    // ── 4. Child record access (GROUP items) ───────────────────────────
    person.homePhone().setData("NUMBER", "555-123-4567");
    person.workPhone().setData("NUMBER", "555-987-6543");

    // Sync child buffers back into parent (fixes the Java bug)
    person.syncChildren();

    // ── 5. Read back all fields ────────────────────────────────────────
    std::cout << "── Record Contents ──" << std::endl;
    for (const auto& name : person.fieldNames()) {
        const auto& fi = person.fieldInfo(name);
        std::cout << "  " << std::left << std::setw(20) << name
                  << " [offset=" << std::setw(3) << fi.offset
                  << " size=" << std::setw(3) << fi.size
                  << " type=" << std::setw(15) << fieldTypeName(fi.type) << "] = \""
                  << person.getData(name) << "\"" << std::endl;
    }
    std::cout << std::endl;

    // ── 6. Numeric round-trip ──────────────────────────────────────────
    std::cout << "Numeric AGE = " << person.getNumeric("AGE") << std::endl;
    std::cout << std::endl;

    // ── 7. Full buffer round-trip ──────────────────────────────────────
    std::string raw = person.getData();
    std::cout << "Raw buffer (" << raw.size() << " bytes):" << std::endl;
    std::cout << "  [" << raw << "]" << std::endl;
    std::cout << std::endl;

    // Deserialize into a new Person
    Person person2(raw.c_str(), raw.size());
    assert(person2.getData("FIRST_NAME") == person.getData("FIRST_NAME"));
    assert(person2.getData("LAST_NAME")  == person.getData("LAST_NAME"));
    assert(person2.getNumeric("AGE")     == 60);
    std::cout << "Round-trip verification: PASSED" << std::endl;

    // ── 8. Field introspection ─────────────────────────────────────────
    std::cout << std::endl;
    std::cout << "── Field Introspection ──" << std::endl;
    std::cout << "Has FIRST_NAME? " << (person.hasField("FIRST_NAME") ? "yes" : "no") << std::endl;
    std::cout << "Has SALARY?     " << (person.hasField("SALARY") ? "yes" : "no") << std::endl;

    std::cout << std::endl;
    std::cout << "Demo complete." << std::endl;
    return 0;
}
