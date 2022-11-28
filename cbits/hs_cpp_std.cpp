#include <HsFFI.h>

#include <cstring>
#include <string>
#include <vector>

#define CAL_OFFSET(NAME, VAL_TYPE)                                             \
  VAL_TYPE* cal_offset_##NAME(VAL_TYPE* current, HsInt offset) {               \
    return current + offset;                                                   \
  }

#define GET_SIZE(NAME, TYPE)                                                   \
  HsInt get_size_##NAME(const TYPE* t) { return (t) ? t->size() : 0; }

#define PEEK_VECTOR(NAME, VEC_TYPE, VAL_TYPE)                                  \
  void peek_##NAME(const VEC_TYPE* vec, HsInt len, VAL_TYPE* vals) {           \
    assert(("peek_##NAME: size mismatch!", len == vec->size()));               \
    for (int i = 0; i < len; i++) {                                            \
      (vals)[i] = (*vec)[i];                                                   \
    }                                                                          \
  }

// ----------------------------------------------------------------------------
extern "C" {

// std::string

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

std::string* hs_new_std_string_copy(std::string&& str) {
  auto value = new std::string;
  *value = str;
  return value;
}

void hs_delete_std_string(std::string* str) { delete str; }

// std::vector

GET_SIZE(vec_of_string, std::vector<std::string>)

HsInt hs_std_vector_string_size(std::vector<std::string>* v) {
  if (v)
    return (HsInt)v->size();
  else
    return 0;
}

std::string* hs_std_vector_string_data(std::vector<std::string>* v) {
  if (v)
    return v->data();
  else
    return nullptr;
}

void hs_delete_std_vector_string(std::vector<std::string>* v) { delete v; }

// ----------------------------------------------------------------------------
} // End extern "C"

#undef GET_SIZE
