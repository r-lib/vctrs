#ifndef VCTRS_RUNS_H
#define VCTRS_RUNS_H

#include "vctrs-core.h"

r_obj* vec_identify_runs(r_obj* x, struct r_lazy error_call);
r_obj* vec_run_sizes(r_obj* x, struct r_lazy error_call);

#endif
