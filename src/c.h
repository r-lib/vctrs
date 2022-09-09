#ifndef VCTRS_C_H
#define VCTRS_C_H

#include "vctrs-core.h"
#include "names.h"
#include "ptype2.h"


r_obj* vec_c(r_obj* xs,
             r_obj* ptype,
             r_obj* name_spec,
             const struct name_repair_opts* name_repair);

r_obj* vec_c_opts(r_obj* xs,
                  r_obj* ptype,
                  r_obj* name_spec,
                  const struct name_repair_opts* name_repair,
                  const struct fallback_opts* fallback_opts);

r_obj* vec_c_fallback_invoke(r_obj* xs, r_obj* name_spec);
r_obj* vec_c_fallback(r_obj* ptype,
                      r_obj* xs,
                      r_obj* name_spec,
                      const struct name_repair_opts* name_repair);

bool needs_vec_c_fallback(r_obj* ptype);
bool needs_vec_c_homogeneous_fallback(r_obj* xs, r_obj* ptype);


#endif
