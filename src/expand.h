#ifndef VCTRS_EXPAND_H
#define VCTRS_EXPAND_H

#include "vctrs-core.h"
#include "names.h"

enum vctrs_expand_vary {
  VCTRS_EXPAND_VARY_slowest = 0,
  VCTRS_EXPAND_VARY_fastest = 1
};

r_obj* vec_expand_grid(r_obj* xs,
                       enum vctrs_expand_vary vary,
                       const struct name_repair_opts* p_name_repair_opts,
                       struct r_lazy error_call);

#endif
