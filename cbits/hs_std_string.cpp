#include <HsFFI.h>

#include <cstring>
#include <string>

// ----------------------------------------------------------------------------
extern "C" {

HsInt hs_std_string_size(std::string* str) {
  if (str)
    return (HsInt)str->size();
  else
    return 0;
}

const char* hs_std_string_cstr(std::string* str) {
  if (str)
    return str->c_str();
  else
    return 0;
}

void hs_copy_std_string(std::string* str, HsInt size, char* buf) {
  if (str != NULL)
    memcpy(buf, str->c_str(), size);
}

std::string* hs_new_std_string_def() { return new std::string; }

std::string* hs_new_std_string(char* s, HsInt length) {
  return new std::string(s, length);
}

void hs_delete_std_string(std::string* str) { delete str; }

// ----------------------------------------------------------------------------
} // End extern "C"
