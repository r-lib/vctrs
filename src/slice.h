#ifndef VCTRS_SLICE_H
#define VCTRS_SLICE_H

#include "vctrs-core.h"

struct vec_slice_opts {
  struct vctrs_arg* x_arg;
  struct vctrs_arg* i_arg;
  struct r_lazy call;
};

enum vctrs_materialize {
  VCTRS_MATERIALIZE_false = 0,
  VCTRS_MATERIALIZE_true
};

r_obj* vec_slice_opts(r_obj* x,
                      r_obj* i,
                      const struct vec_slice_opts* opts);

static inline
r_obj* vec_slice(r_obj* x, r_obj* i) {
  const struct vec_slice_opts opts = { 0 };
  return vec_slice_opts(x, i, &opts);
}

r_obj* vec_init(r_obj* x, r_ssize n, struct r_lazy call);

r_obj* vec_slice_unsafe(r_obj* x, r_obj* i);

r_obj* vec_slice_base(enum vctrs_type type,
                      r_obj* x,
                      r_obj* subscript,
                      enum vctrs_materialize materialize);

r_obj* slice_names(r_obj* names, r_obj* subscript);
r_obj* slice_rownames(r_obj* names, r_obj* subscript);
r_obj* vec_slice_fallback(r_obj* x, r_obj* subscript);

bool vec_is_restored(r_obj* x, r_obj* to);


#endif
