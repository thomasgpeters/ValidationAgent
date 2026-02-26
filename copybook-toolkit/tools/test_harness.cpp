/// Interactive ncurses test/demo harness for the COBOL Copybook Toolkit.
///
/// Features:
///   - Run GoogleTest suite and display pass/fail results
///   - Run standalone demo with captured output
///   - Interactive record inspector: create, edit, and view Person records
///   - Live hex + character buffer display
///
/// Navigation: Arrow keys, Enter to select, 'q' to quit

#include <ncurses.h>
#include <panel.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <array>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <memory>
#include <functional>

#include <copybook/core/record_base.h>
#include <copybook/core/record_buffer.h>
#include <copybook/core/field_type.h>

// Include the Person example for the interactive inspector
#include "person.h"
#include "phone.h"

using namespace copybook;
using namespace copybook::examples;

// ═══════════════════════════════════════════════════════════════════════════
// Utility: run a shell command and capture output
// ═══════════════════════════════════════════════════════════════════════════

static std::string execCommand(const std::string& cmd) {
    std::array<char, 256> buf;
    std::string result;
    std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd.c_str(), "r"), pclose);
    if (!pipe) return "[error: failed to run command]";
    while (fgets(buf.data(), buf.size(), pipe.get()) != nullptr) {
        result += buf.data();
    }
    return result;
}

// ═══════════════════════════════════════════════════════════════════════════
// Color pairs
// ═══════════════════════════════════════════════════════════════════════════

enum Colors {
    PAIR_NORMAL   = 1,
    PAIR_TITLE    = 2,
    PAIR_SELECTED = 3,
    PAIR_PASS     = 4,
    PAIR_FAIL     = 5,
    PAIR_HEADER   = 6,
    PAIR_HEX      = 7,
    PAIR_FIELD    = 8,
    PAIR_STATUS   = 9,
};

static void initColors() {
    start_color();
    use_default_colors();
    init_pair(PAIR_NORMAL,   COLOR_WHITE,   -1);
    init_pair(PAIR_TITLE,    COLOR_CYAN,    -1);
    init_pair(PAIR_SELECTED, COLOR_BLACK,   COLOR_CYAN);
    init_pair(PAIR_PASS,     COLOR_GREEN,   -1);
    init_pair(PAIR_FAIL,     COLOR_RED,     -1);
    init_pair(PAIR_HEADER,   COLOR_YELLOW,  -1);
    init_pair(PAIR_HEX,      COLOR_MAGENTA, -1);
    init_pair(PAIR_FIELD,    COLOR_WHITE,   -1);
    init_pair(PAIR_STATUS,   COLOR_BLACK,   COLOR_GREEN);
}

// ═══════════════════════════════════════════════════════════════════════════
// Draw helpers
// ═══════════════════════════════════════════════════════════════════════════

static void drawTitle(WINDOW* win, int row, const std::string& text) {
    wattron(win, COLOR_PAIR(PAIR_TITLE) | A_BOLD);
    mvwprintw(win, row, 2, "%s", text.c_str());
    wattroff(win, COLOR_PAIR(PAIR_TITLE) | A_BOLD);
}

static void drawBox(WINDOW* win, const std::string& title) {
    box(win, 0, 0);
    if (!title.empty()) {
        wattron(win, COLOR_PAIR(PAIR_TITLE) | A_BOLD);
        mvwprintw(win, 0, 2, " %s ", title.c_str());
        wattroff(win, COLOR_PAIR(PAIR_TITLE) | A_BOLD);
    }
}

static void drawStatusBar(const std::string& text) {
    int rows, cols;
    getmaxyx(stdscr, rows, cols);
    attron(COLOR_PAIR(PAIR_STATUS));
    mvhline(rows - 1, 0, ' ', cols);
    mvprintw(rows - 1, 1, "%s", text.c_str());
    attroff(COLOR_PAIR(PAIR_STATUS));
    refresh();
}

// ═══════════════════════════════════════════════════════════════════════════
// Scrollable output viewer
// ═══════════════════════════════════════════════════════════════════════════

static void showOutputViewer(const std::string& title, const std::string& output) {
    // Split into lines
    std::vector<std::string> lines;
    std::istringstream iss(output);
    std::string line;
    while (std::getline(iss, line)) {
        lines.push_back(line);
    }
    if (lines.empty()) lines.push_back("(no output)");

    int rows, cols;
    getmaxyx(stdscr, rows, cols);

    int winH = rows - 4;
    int winW = cols - 4;
    WINDOW* win = newwin(winH, winW, 2, 2);
    keypad(win, TRUE);

    int scroll = 0;
    int maxScroll = std::max(0, (int)lines.size() - (winH - 4));

    while (true) {
        werase(win);
        drawBox(win, title);

        int viewH = winH - 4;
        for (int i = 0; i < viewH && (scroll + i) < (int)lines.size(); ++i) {
            const auto& l = lines[scroll + i];

            // Color: green for [  PASSED  ], [       OK ]; red for [  FAILED  ]
            if (l.find("[  PASSED  ]") != std::string::npos ||
                l.find("[       OK ]") != std::string::npos) {
                wattron(win, COLOR_PAIR(PAIR_PASS));
            } else if (l.find("[  FAILED  ]") != std::string::npos ||
                       l.find("[   FAIL   ]") != std::string::npos) {
                wattron(win, COLOR_PAIR(PAIR_FAIL));
            }

            mvwprintw(win, i + 2, 2, "%-*.*s", winW - 4, winW - 4, l.c_str());

            wattroff(win, COLOR_PAIR(PAIR_PASS));
            wattroff(win, COLOR_PAIR(PAIR_FAIL));
        }

        // Scroll indicator
        wattron(win, COLOR_PAIR(PAIR_HEADER));
        mvwprintw(win, winH - 2, 2,
                  "Lines %d-%d of %d  |  Up/Down/PgUp/PgDn to scroll  |  q/Esc to close",
                  scroll + 1, std::min(scroll + viewH, (int)lines.size()),
                  (int)lines.size());
        wattroff(win, COLOR_PAIR(PAIR_HEADER));

        wrefresh(win);

        int ch = wgetch(win);
        if (ch == 'q' || ch == 27) break;
        else if (ch == KEY_UP && scroll > 0) scroll--;
        else if (ch == KEY_DOWN && scroll < maxScroll) scroll++;
        else if (ch == KEY_PPAGE) scroll = std::max(0, scroll - viewH);
        else if (ch == KEY_NPAGE) scroll = std::min(maxScroll, scroll + viewH);
        else if (ch == KEY_HOME) scroll = 0;
        else if (ch == KEY_END) scroll = maxScroll;
    }

    delwin(win);
}

// ═══════════════════════════════════════════════════════════════════════════
// Feature 1: Run Unit Tests
// ═══════════════════════════════════════════════════════════════════════════

static void runUnitTests() {
    drawStatusBar("Running unit tests... please wait");
    std::string output = execCommand("./copybook-tests --gtest_color=no 2>&1");
    showOutputViewer("Unit Test Results", output);
}

// ═══════════════════════════════════════════════════════════════════════════
// Feature 2: Run Standalone Demo
// ═══════════════════════════════════════════════════════════════════════════

static void runStandaloneDemo() {
    drawStatusBar("Running standalone demo...");
    std::string output = execCommand("./standalone-demo 2>&1");
    showOutputViewer("Standalone Demo Output", output);
}

// ═══════════════════════════════════════════════════════════════════════════
// Feature 3: Interactive Record Inspector
// ═══════════════════════════════════════════════════════════════════════════

static std::string readStringInput(WINDOW* win, int row, int col, int maxLen) {
    char buf[256] = {0};
    int pos = 0;
    curs_set(1);
    wmove(win, row, col);
    wrefresh(win);

    while (true) {
        int ch = wgetch(win);
        if (ch == '\n' || ch == KEY_ENTER) break;
        else if (ch == 27) { buf[0] = '\0'; break; }  // Esc cancels
        else if ((ch == KEY_BACKSPACE || ch == 127 || ch == 8) && pos > 0) {
            pos--;
            buf[pos] = '\0';
            mvwprintw(win, row, col, "%-*s", maxLen, buf);
            wmove(win, row, col + pos);
        } else if (pos < maxLen && pos < 255 && ch >= 32 && ch < 127) {
            buf[pos++] = (char)ch;
            buf[pos] = '\0';
            mvwprintw(win, row, col, "%s", buf);
        }
        wrefresh(win);
    }

    curs_set(0);
    return std::string(buf);
}

static void drawHexDump(WINDOW* win, int startRow, int startCol, int maxW,
                        const std::string& data) {
    int bytesPerRow = 16;
    int rows = ((int)data.size() + bytesPerRow - 1) / bytesPerRow;

    for (int r = 0; r < rows; ++r) {
        int off = r * bytesPerRow;
        // Offset
        wattron(win, COLOR_PAIR(PAIR_HEADER));
        mvwprintw(win, startRow + r, startCol, "%04X: ", off);
        wattroff(win, COLOR_PAIR(PAIR_HEADER));

        // Hex bytes
        wattron(win, COLOR_PAIR(PAIR_HEX));
        for (int b = 0; b < bytesPerRow; ++b) {
            if (off + b < (int)data.size()) {
                wprintw(win, "%02X ", (unsigned char)data[off + b]);
            } else {
                wprintw(win, "   ");
            }
            if (b == 7) wprintw(win, " ");
        }
        wattroff(win, COLOR_PAIR(PAIR_HEX));

        // ASCII
        wprintw(win, " |");
        for (int b = 0; b < bytesPerRow && off + b < (int)data.size(); ++b) {
            char c = data[off + b];
            if (c >= 32 && c < 127) waddch(win, c);
            else waddch(win, '.');
        }
        wprintw(win, "|");
    }
}

static void interactiveInspector() {
    Person person;

    // Pre-populate with sample data
    person.setData("ID",             "EMP-001");
    person.setData("FIRST_NAME",     "Thomas");
    person.setData("LAST_NAME",      "Peters");
    person.setData("MIDDLE_INITIAL", "G");
    person.setNumeric("AGE",          60);
    person.setData("SEX",            "M");
    person.setData("DATE_OF_BIRTH",  "1965-03-15");
    person.homePhone().setData("NUMBER", "555-123-4567");
    person.workPhone().setData("NUMBER", "555-987-6543");
    person.syncChildren();

    auto names = person.fieldNames();
    int selected = 0;
    int rows, cols;

    while (true) {
        getmaxyx(stdscr, rows, cols);
        int winH = rows - 4;
        int winW = cols - 4;
        WINDOW* win = newwin(winH, winW, 2, 2);
        keypad(win, TRUE);

        werase(win);
        drawBox(win, "Record Inspector: Person (115 bytes)");

        // Left panel: field list
        int leftW = 50;
        drawTitle(win, 2, "Fields");
        wattron(win, COLOR_PAIR(PAIR_HEADER));
        mvwprintw(win, 3, 2, "%-20s %-4s %-4s %-15s  %-s", "Name", "Off", "Sz", "Type", "Value");
        wattroff(win, COLOR_PAIR(PAIR_HEADER));

        int maxFieldRows = winH - 8;
        for (int i = 0; i < (int)names.size() && i < maxFieldRows; ++i) {
            const auto& fi = person.fieldInfo(names[i]);
            std::string val = person.getData(names[i]);

            // Trim trailing spaces for display
            size_t end = val.find_last_not_of(' ');
            std::string trimmed = (end == std::string::npos) ? "" : val.substr(0, end + 1);

            if (i == selected) {
                wattron(win, COLOR_PAIR(PAIR_SELECTED));
            }

            mvwprintw(win, 4 + i, 2, "%-20s %3d  %3d  %-15s  %-.*s",
                      names[i].c_str(), fi.offset, fi.size,
                      fieldTypeName(fi.type).c_str(),
                      leftW - 50, trimmed.c_str());

            if (i == selected) {
                // Fill the rest of the selected row
                int curX, curY;
                getyx(win, curY, curX);
                for (int x = curX; x < leftW; ++x) waddch(win, ' ');
                wattroff(win, COLOR_PAIR(PAIR_SELECTED));
            }
        }

        // Right panel: hex dump
        int hexCol = leftW + 4;
        if (hexCol + 60 < winW) {
            drawTitle(win, 2, "                                                       Buffer Hex Dump");
            person.syncChildren();
            std::string raw = person.getData();
            drawHexDump(win, 4, hexCol, winW - hexCol - 2, raw);
        }

        // Bottom: instructions
        wattron(win, COLOR_PAIR(PAIR_HEADER));
        mvwprintw(win, winH - 2, 2,
                  "Up/Down: select | Enter: edit field | c: clear field | r: reset all | q: back");
        wattroff(win, COLOR_PAIR(PAIR_HEADER));

        wrefresh(win);

        int ch = wgetch(win);
        delwin(win);

        if (ch == 'q' || ch == 27) break;
        else if (ch == KEY_UP && selected > 0) selected--;
        else if (ch == KEY_DOWN && selected < (int)names.size() - 1) selected++;
        else if (ch == '\n' || ch == KEY_ENTER) {
            // Edit the selected field
            const auto& fi = person.fieldInfo(names[selected]);
            if (fi.type == FieldType::RECORD) {
                drawStatusBar("Cannot edit GROUP fields directly — edit child fields instead.");
                continue;
            }

            // Create a small input dialog
            int dlgH = 5, dlgW = 60;
            int dlgY = (rows - dlgH) / 2;
            int dlgX = (cols - dlgW) / 2;
            WINDOW* dlg = newwin(dlgH, dlgW, dlgY, dlgX);
            keypad(dlg, TRUE);
            drawBox(dlg, "Edit Field");
            mvwprintw(dlg, 1, 2, "%s [%d bytes]:", names[selected].c_str(), fi.size);
            mvwprintw(dlg, 2, 2, "> ");
            wrefresh(dlg);

            std::string newVal = readStringInput(dlg, 2, 4, std::min(fi.size, 50));
            delwin(dlg);

            if (!newVal.empty()) {
                if (fi.type == FieldType::ZONED_UNSIGNED || fi.type == FieldType::ZONED_NUMERIC) {
                    try {
                        long num = std::stol(newVal);
                        person.setNumeric(names[selected], num);
                    } catch (...) {
                        person.setData(names[selected], newVal);
                    }
                } else {
                    person.setData(names[selected], newVal);
                }
            }
        } else if (ch == 'c') {
            // Clear selected field
            person.setData(names[selected], "");
        } else if (ch == 'r') {
            // Reset all fields
            person = Person();
            person.setData("ID",             "EMP-001");
            person.setData("FIRST_NAME",     "Thomas");
            person.setData("LAST_NAME",      "Peters");
            person.setData("MIDDLE_INITIAL", "G");
            person.setNumeric("AGE",          60);
            person.setData("SEX",            "M");
            person.setData("DATE_OF_BIRTH",  "1965-03-15");
            person.homePhone().setData("NUMBER", "555-123-4567");
            person.workPhone().setData("NUMBER", "555-987-6543");
            person.syncChildren();
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Feature 4: Buffer Round-Trip Test
// ═══════════════════════════════════════════════════════════════════════════

static void runRoundTripTest() {
    drawStatusBar("Running buffer round-trip test...");

    std::ostringstream out;
    out << "Buffer Round-Trip Test\n";
    out << std::string(60, '=') << "\n\n";

    // Create and populate a Person
    Person p1;
    p1.setData("ID",             "RTT-001");
    p1.setData("FIRST_NAME",     "RoundTrip");
    p1.setData("LAST_NAME",      "TestUser");
    p1.setData("MIDDLE_INITIAL", "X");
    p1.setNumeric("AGE",          42);
    p1.setData("SEX",            "F");
    p1.setData("DATE_OF_BIRTH",  "1983-07-22");
    p1.homePhone().setData("NUMBER", "111-222-3333");
    p1.workPhone().setData("NUMBER", "444-555-6666");
    p1.syncChildren();

    std::string raw = p1.getData();
    out << "Original record (" << raw.size() << " bytes):\n";
    out << "  [" << raw << "]\n\n";

    // Deserialize
    Person p2(raw.c_str(), raw.size());

    out << "Verification:\n";

    auto check = [&](const std::string& field) {
        std::string v1 = p1.getData(field);
        std::string v2 = p2.getData(field);
        bool ok = (v1 == v2);
        out << "  " << (ok ? "[PASS]" : "[FAIL]") << " " << field
            << ": \"" << v1 << "\" == \"" << v2 << "\"\n";
    };

    for (const auto& name : p1.fieldNames()) {
        check(name);
    }

    out << "\nNumeric round-trip:\n";
    out << "  AGE: " << p1.getNumeric("AGE") << " == " << p2.getNumeric("AGE")
        << " → " << (p1.getNumeric("AGE") == p2.getNumeric("AGE") ? "[PASS]" : "[FAIL]") << "\n";

    out << "\nChild record round-trip:\n";
    out << "  HOME_PHONE: \"" << p2.homePhone().getData("NUMBER") << "\""
        << " → " << (p2.homePhone().getData("NUMBER") == "111-222-3333" ? "[PASS]" : "[FAIL]") << "\n";
    out << "  WORK_PHONE: \"" << p2.workPhone().getData("NUMBER") << "\""
        << " → " << (p2.workPhone().getData("NUMBER") == "444-555-6666" ? "[PASS]" : "[FAIL]") << "\n";

    out << "\n" << std::string(60, '=') << "\n";
    out << "All checks complete.\n";

    showOutputViewer("Buffer Round-Trip Test", out.str());
}

// ═══════════════════════════════════════════════════════════════════════════
// Feature 5: Field Layout Visualizer
// ═══════════════════════════════════════════════════════════════════════════

static void showFieldLayout() {
    Person person;
    auto names = person.fieldNames();

    std::ostringstream out;
    out << "Person Record Layout (115 bytes)\n";
    out << std::string(72, '=') << "\n\n";
    out << "  Offset  Size  Type              Name\n";
    out << "  ------  ----  ----------------  --------------------\n";

    for (const auto& name : names) {
        const auto& fi = person.fieldInfo(name);
        char line[128];
        snprintf(line, sizeof(line), "  %4d    %3d   %-16s  %s",
                 fi.offset, fi.size, fieldTypeName(fi.type).c_str(), fi.name.c_str());
        out << line << "\n";
    }

    out << "\n";
    out << "Visual byte map (each char = 1 byte):\n\n";

    // Build a visual map
    std::string map(Person::RECORD_SIZE, '.');

    // Assign a letter to each field
    const char* labels = "IFLMASEDHW";  // first letter of each field
    for (int i = 0; i < (int)names.size() && i < 10; ++i) {
        const auto& fi = person.fieldInfo(names[i]);
        for (int b = 0; b < fi.size; ++b) {
            map[fi.offset + b] = labels[i];
        }
    }

    // Print in rows of 40
    for (int off = 0; off < (int)map.size(); off += 40) {
        int len = std::min(40, (int)map.size() - off);
        char offStr[16];
        snprintf(offStr, sizeof(offStr), "  %3d |", off);
        out << offStr << map.substr(off, len) << "|\n";
    }

    out << "\n  Legend:\n";
    const char* longLabels[] = {
        "I=ID", "F=FIRST_NAME", "L=LAST_NAME", "M=MIDDLE_INITIAL",
        "A=AGE", "S=SEX", "E=DATE_OF_BIRTH", "D=HOME_PHONE(group)",
        "H=HOME_PHONE", "W=WORK_PHONE"
    };
    for (int i = 0; i < 10 && i < (int)names.size(); ++i) {
        out << "    " << labels[i] << " = " << names[i]
            << " (offset " << person.fieldInfo(names[i]).offset
            << ", " << person.fieldInfo(names[i]).size << " bytes)\n";
    }

    showOutputViewer("Field Layout Visualizer", out.str());
}

// ═══════════════════════════════════════════════════════════════════════════
// Main Menu
// ═══════════════════════════════════════════════════════════════════════════

struct MenuItem {
    std::string label;
    std::string description;
    std::function<void()> action;
};

int main() {
    // Initialize ncurses
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    initColors();

    std::vector<MenuItem> menu = {
        {"Run Unit Tests",          "Execute GoogleTest suite and show results",      runUnitTests},
        {"Run Standalone Demo",     "Run the non-interactive Person record demo",     runStandaloneDemo},
        {"Interactive Inspector",   "Create and edit Person records with live view",  interactiveInspector},
        {"Buffer Round-Trip Test",  "Serialize → deserialize → verify all fields",   runRoundTripTest},
        {"Field Layout Visualizer", "Show byte-level record layout and offset map",  showFieldLayout},
    };

    int selected = 0;

    while (true) {
        int rows, cols;
        getmaxyx(stdscr, rows, cols);

        erase();

        // Title banner
        attron(COLOR_PAIR(PAIR_TITLE) | A_BOLD);
        mvprintw(1, 2, "COBOL Copybook Toolkit — Test & Demo Harness");
        attroff(COLOR_PAIR(PAIR_TITLE) | A_BOLD);

        attron(COLOR_PAIR(PAIR_HEADER));
        mvprintw(2, 2, "v1.0.0  |  Sprint 1 Core Engine  |  %d test suites available",
                 (int)menu.size());
        attroff(COLOR_PAIR(PAIR_HEADER));

        mvhline(3, 2, ACS_HLINE, cols - 4);

        // Menu items
        for (int i = 0; i < (int)menu.size(); ++i) {
            int row = 5 + i * 2;

            if (i == selected) {
                attron(COLOR_PAIR(PAIR_SELECTED));
                mvhline(row, 2, ' ', cols - 4);
                mvprintw(row, 3, " > %s", menu[i].label.c_str());
                attroff(COLOR_PAIR(PAIR_SELECTED));
                attron(COLOR_PAIR(PAIR_NORMAL) | A_DIM);
                mvprintw(row + 1, 7, "%s", menu[i].description.c_str());
                attroff(COLOR_PAIR(PAIR_NORMAL) | A_DIM);
            } else {
                attron(COLOR_PAIR(PAIR_NORMAL));
                mvprintw(row, 5, "%s", menu[i].label.c_str());
                attroff(COLOR_PAIR(PAIR_NORMAL));
            }
        }

        // Bottom help
        int helpRow = 5 + (int)menu.size() * 2 + 2;
        mvhline(helpRow, 2, ACS_HLINE, cols - 4);
        attron(COLOR_PAIR(PAIR_HEADER));
        mvprintw(helpRow + 1, 2, "Up/Down: navigate  |  Enter: select  |  q: quit");
        attroff(COLOR_PAIR(PAIR_HEADER));

        drawStatusBar("Ready");
        refresh();

        int ch = getch();
        if (ch == 'q' || ch == 'Q') break;
        else if (ch == KEY_UP && selected > 0) selected--;
        else if (ch == KEY_DOWN && selected < (int)menu.size() - 1) selected++;
        else if (ch == '\n' || ch == KEY_ENTER) {
            menu[selected].action();
        }
    }

    endwin();
    return 0;
}
