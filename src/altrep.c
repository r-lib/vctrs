#include <rlang.h>

// [[ register() ]]
r_obj* vctrs_is_altrep(r_obj* x) {
  return r_lgl(ALTREP(x));
}
