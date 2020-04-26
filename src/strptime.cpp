// https://stackoverflow.com/a/33542189
#include <time.h>
#include <iomanip>
#include <sstream>

extern "C" char* strptime(const char* s,
                          const char* f,
                          struct tm* tm) {
  // Isn't the C++ standard lib nice? std::get_time is defined such that its
  // format parameters are the exact same as strptime. Of course, we have to
  // create a string stream first, and imbue it with the current C locale, and
  // we also have to make sure we return the right things if it fails, or
  // if it succeeds, but this is still far simpler an implementation than any
  // of the versions in any of the C standard libraries.
  std::istringstream input(s);
  input.imbue(std::locale(setlocale(LC_ALL, nullptr)));
  input >> std::get_time(tm, f);
  if (input.fail()) {
    return nullptr;
  }
  return (char*)(s + input.tellg());
}