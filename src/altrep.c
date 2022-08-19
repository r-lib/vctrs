#include <rlang.h>
#include "altrep.h"

// [[ register() ]]
r_obj* vctrs_is_altrep(r_obj* x) {
  return r_lgl(ALTREP(x));
}
