#ifndef VCTRS_PROXY_RESTORE_H
#define VCTRS_PROXY_RESTORE_H

#include "vctrs-core.h"


r_obj* vec_restore(r_obj* x, r_obj* to, const enum vctrs_owned owned);
r_obj* vec_restore_default(r_obj* x, r_obj* to, const enum vctrs_owned owned);

r_obj* vec_bare_df_restore(r_obj* x, r_obj* to, const enum vctrs_owned owned);
r_obj* vec_df_restore(r_obj* x, r_obj* to, const enum vctrs_owned owned);


#endif
