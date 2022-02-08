#ifndef VCTRS_TYPE_FACTOR_H
#define VCTRS_TYPE_FACTOR_H

#include "vctrs-core.h"
#include "cast.h"
#include "ptype2.h"


SEXP fct_ptype2(const struct ptype2_opts* opts);
SEXP ord_ptype2(const struct ptype2_opts* opts);
SEXP ord_as_ordered(const struct cast_opts* opts);

#endif
