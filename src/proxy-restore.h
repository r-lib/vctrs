#ifndef VCTRS_PROXY_RESTORE_H
#define VCTRS_PROXY_RESTORE_H

#include "vctrs-core.h"

struct vec_restore_opts {
  // The kind of ownership we have over the `proxy`
  enum vctrs_ownership ownership;

  // Whether the `proxy` was proxied recursively or not
  bool recursively_proxied;
};

r_obj* vec_restore_opts(
  r_obj* x,
  r_obj* to,
  const struct vec_restore_opts* p_opts
);

r_obj* vec_restore_default(r_obj* x, r_obj* to, enum vctrs_ownership ownership);

r_obj* vec_df_restore(
  r_obj* x,
  r_obj* to,
  const struct vec_restore_opts* p_opts
);

r_obj* vec_bare_df_restore(
  r_obj* x,
  r_obj* to,
  const struct vec_restore_opts* p_opts
);


#endif
