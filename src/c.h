#ifndef VCTRS_C_H
#define VCTRS_C_H

#include "utils.h"


SEXP vec_c_opts(SEXP xs,
                SEXP ptype,
                SEXP name_spec,
                const struct name_repair_opts* name_repair,
                const struct fallback_opts* fallback_opts);

SEXP vec_c_fallback_invoke(SEXP xs, SEXP name_spec);
SEXP vec_c_fallback(SEXP ptype,
                    SEXP xs,
                    SEXP name_spec,
                    const struct name_repair_opts* name_repair);

bool needs_vec_c_fallback(SEXP ptype);
bool needs_vec_c_homogeneous_fallback(SEXP xs, SEXP ptype);


#endif
