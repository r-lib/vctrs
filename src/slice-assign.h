#ifndef VCTRS_SLICE_ASSIGN_H
#define VCTRS_SLICE_ASSIGN_H

#include "vctrs-core.h"
#include "ownership.h"

struct vec_assign_opts {
  bool assign_names;
  bool ignore_outer_names;
  enum vctrs_ownership ownership;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
  struct r_lazy call;
};

struct vec_proxy_assign_opts {
  bool assign_names;
  bool ignore_outer_names;
  enum vctrs_ownership ownership;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
  struct r_lazy call;
  // Whether the `proxy` was proxied recursively or not
  bool recursively_proxied;
};

r_obj* vec_assign_opts(r_obj* x,
                       r_obj* index,
                       r_obj* value,
                       const struct vec_assign_opts* p_opts);

r_obj* vec_proxy_assign_opts(r_obj* proxy,
                             r_obj* index,
                             r_obj* value,
                             const struct vec_proxy_assign_opts* p_opts);

r_obj* chr_assign(r_obj* out,
                  r_obj* index,
                  r_obj* value,
                  const enum vctrs_ownership ownership);

r_obj* list_assign(r_obj* out,
                   r_obj* index,
                   r_obj* value,
                   const enum vctrs_ownership ownership);

r_obj* df_assign(
  r_obj* x,
  r_obj* index,
  r_obj* value,
  const struct vec_proxy_assign_opts* p_opts
);

r_obj* vec_assign_shaped(r_obj* proxy, r_obj* index, r_obj* value, const enum vctrs_ownership ownership);

#endif
