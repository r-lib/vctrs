#ifndef VCTRS_TYPE_TIBBLE_H
#define VCTRS_TYPE_TIBBLE_H

#include "vctrs-core.h"
#include "ptype2.h"


SEXP tib_ptype2(const struct ptype2_opts* opts);
SEXP tib_cast(const struct cast_opts* opts);


#endif
