#include "optional.h"

struct optional_r_ssize optional_r_ssize_none;

void vctrs_init_optional(r_obj* ns) {
    optional_r_ssize_none.some = false;
    optional_r_ssize_none.value = -1;
}
