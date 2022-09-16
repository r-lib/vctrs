#ifndef VCTRS_GLOBALS_H
#define VCTRS_GLOBALS_H

#include <rlang.h>
#include "rlang-dev.h"

struct syms {
  r_obj* arg;
  r_obj* dot_arg;
  r_obj* dot_call;
  r_obj* haystack_arg;
  r_obj* needles_arg;
  r_obj* repair_arg;
  r_obj* times_arg;
  r_obj* to_arg;
  r_obj* value_arg;
  r_obj* vec_default_cast;
  r_obj* vec_slice_dispatch_integer64;
  r_obj* vec_slice_fallback;
  r_obj* vec_slice_fallback_integer64;
  r_obj* x_arg;
  r_obj* y_arg;
};

// These structs must be in sync as their elements are defined
// together by the `INIT_STRING()` macro
struct strings {
  r_obj* AsIs;
  r_obj* repair;
};
struct chrs {
  r_obj* AsIs;
  r_obj* repair;
};

struct fns {
  r_obj* vec_slice_dispatch_integer64;
  r_obj* vec_slice_fallback;
  r_obj* vec_slice_fallback_integer64;
};

struct vec_args {
  struct vctrs_arg* dot_name_repair;
  struct vctrs_arg* dot_ptype;
  struct vctrs_arg* dot_size;
  struct vctrs_arg* empty;
  struct vctrs_arg* i;
  struct vctrs_arg* max_fill;
  struct vctrs_arg* n;
  struct vctrs_arg* value;
  struct vctrs_arg* x;
};

struct lazy_args {
  struct r_lazy dot_name_repair;
};

struct lazy_calls {
  struct r_lazy vec_assign;
  struct r_lazy vec_assign_params;
  struct r_lazy vec_assign_seq;
  struct r_lazy vec_init;
  struct r_lazy vec_ptype_finalise;
  struct r_lazy vec_recycle;
  struct r_lazy vec_recycle_common;
  struct r_lazy vec_size;
  struct r_lazy vec_size_common;
};

extern struct syms syms;
extern struct strings strings;
extern struct chrs chrs;
extern struct fns fns;
extern struct vec_args vec_args;
extern struct lazy_args lazy_args;
extern struct lazy_calls lazy_calls;


extern r_obj* vctrs_shared_empty_date;
extern r_obj* vctrs_shared_empty_uns;

extern Rcomplex vctrs_shared_na_cpl;
extern r_obj* vctrs_shared_na_lgl;
extern r_obj* vctrs_shared_na_list;


#endif
