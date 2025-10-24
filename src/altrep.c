#include <rlang.h>
#include "R_ext/Altrep.h"

// [[ register() ]]
r_obj* vctrs_is_altrep(r_obj* x) {
  return r_lgl(ALTREP(x));
}
