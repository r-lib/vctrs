#include "vctrs.h"
#include "decl/rlang-dev-decl.h"

const char* r_obj_type_friendly_length(r_obj* x) {
  return r_obj_type_friendly_full(x, true, true);
}
