#ifndef VCTRS_SLICE_ASSIGN_H
#define VCTRS_SLICE_ASSIGN_H

#include "vctrs-core.h"
#include "owned.h"

struct vec_assign_opts {
  bool assign_names;
  bool ignore_outer_names;
  struct vctrs_arg* x_arg;
  struct vctrs_arg* value_arg;
  struct r_lazy call;
};

r_obj* vec_assign_opts(r_obj* x,
                       r_obj* index,
                       r_obj* value,
                       const struct vec_assign_opts* opts);

static inline
r_obj* vec_assign(r_obj* x, r_obj* index, r_obj* value) {
  struct vec_assign_opts opts = { 0 };
  return vec_assign_opts(x, index, value, &opts);
}

static inline
r_obj* vec_check_assign(r_obj* x,
                        r_obj* index,
                        r_obj* value,
                        struct vctrs_arg* x_arg,
                        struct vctrs_arg* value_arg,
                        struct r_lazy call) {
  struct vec_assign_opts opts = {
    .x_arg = x_arg,
    .value_arg = value_arg,
    .call = call
  };
  return vec_assign_opts(x, index, value, &opts);
}

static inline
r_obj* vec_assign_n(r_obj* x,
                    r_obj* index,
                    r_obj* value,
                    bool assign_names,
                    bool ignore_outer_names,
                    struct vctrs_arg* x_arg,
                    struct vctrs_arg* value_arg,
                    struct r_lazy call) {
  struct vec_assign_opts opts = {
    .assign_names = assign_names,
    .ignore_outer_names = ignore_outer_names,
    .x_arg = x_arg,
    .value_arg = value_arg,
    .call = call
  };
  return vec_assign_opts(x, index, value, &opts);
}

r_obj* vec_proxy_assign_opts(r_obj* proxy,
                             r_obj* index,
                             r_obj* value,
                             const enum vctrs_owned owned,
                             const struct vec_assign_opts* opts);

static inline
r_obj* vec_proxy_assign(r_obj* proxy,
                        r_obj* index,
                        r_obj* value) {
  struct vec_assign_opts args = { 0 };
  return vec_proxy_assign_opts(proxy, index, value, vec_owned(proxy), &args);
}

static inline
r_obj* vec_proxy_check_assign(r_obj* proxy,
                              r_obj* index,
                              r_obj* value,
                              struct vctrs_arg* x_arg,
                              struct vctrs_arg* value_arg,
                              struct r_lazy call) {
  struct vec_assign_opts opts = {
    .x_arg = x_arg,
    .value_arg = value_arg,
    .call = call
  };
  return vec_proxy_assign_opts(proxy, index, value, vec_owned(proxy), &opts);
}

r_obj* chr_assign(r_obj* out,
                  r_obj* index,
                  r_obj* value,
                  const enum vctrs_owned owned);

r_obj* list_assign(r_obj* out,
                   r_obj* index,
                   r_obj* value,
                   const enum vctrs_owned owned);

r_obj* df_assign(r_obj* x, r_obj* index, r_obj* value,
                 const enum vctrs_owned owned,
                 const struct vec_assign_opts* opts);

r_obj* vec_assign_shaped(r_obj* proxy, r_obj* index, r_obj* value,
                         const enum vctrs_owned owned,
                         const struct vec_assign_opts* opts);

#endif
