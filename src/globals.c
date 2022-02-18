#include "vctrs.h"

struct syms syms;
struct strings strings;
struct fns fns;
struct vec_args vec_args;
struct lazy_calls lazy_calls;

struct r_dyn_array* globals_shelter = NULL;

void vctrs_init_globals(r_obj* ns) {
  size_t n_strings = sizeof(struct lazy_calls) / sizeof(struct r_lazy);
  size_t n_lazy_calls = sizeof(struct strings) / sizeof(r_obj*);
  size_t n_globals = n_strings + n_lazy_calls;

  globals_shelter = r_new_dyn_vector(R_TYPE_list, n_globals);
  r_preserve(globals_shelter->shelter);

  // Symbols -----------------------------------------------------------
  syms.arg = r_sym("arg");
  syms.dot_arg = r_sym(".arg");
  syms.dot_call = r_sym(".call");
  syms.haystack_arg = r_sym("haystack_arg");
  syms.needles_arg = r_sym("needles_arg");
  syms.repair_arg = r_sym("repair_arg");
  syms.to_arg = r_sym("to_arg");
  syms.value_arg = r_sym("value_arg");
  syms.x_arg = r_sym("x_arg");
  syms.y_arg = r_sym("y_arg");

  // Strings -----------------------------------------------------------
  strings.AsIs = r_str("AsIs");
  r_dyn_list_push_back(globals_shelter, strings.AsIs);

  // Args --------------------------------------------------------------
  static struct vctrs_arg dot_ptype; dot_ptype = new_wrapper_arg(NULL, ".ptype");
  static struct vctrs_arg dot_size; dot_size = new_wrapper_arg(NULL, ".size");
  static struct vctrs_arg empty; empty = new_wrapper_arg(NULL, "");
  static struct vctrs_arg i; i = new_wrapper_arg(NULL, "i");
  static struct vctrs_arg max_fill; max_fill = new_wrapper_arg(NULL, "max_fill");
  static struct vctrs_arg n; n = new_wrapper_arg(NULL, "n");
  static struct vctrs_arg x; x = new_wrapper_arg(NULL, "x");

  vec_args.dot_ptype = &dot_ptype;
  vec_args.dot_size = &dot_size;
  vec_args.empty = &empty;
  vec_args.i = &i;
  vec_args.max_fill = &max_fill;
  vec_args.n = &n;
  vec_args.x = &x;

  // Calls -------------------------------------------------------------
  lazy_calls.vec_recycle = (struct r_lazy) { .x = r_parse("vec_recycle()"), .env = r_null };
  r_dyn_list_push_back(globals_shelter, lazy_calls.vec_recycle.x);

  lazy_calls.vec_recycle_common = (struct r_lazy) { .x = r_parse("vec_recycle_common()"), .env = r_null };
  r_dyn_list_push_back(globals_shelter, lazy_calls.vec_recycle_common.x);

  lazy_calls.vec_size = (struct r_lazy) { .x = r_parse("vec_size()"), .env = r_null };
  r_dyn_list_push_back(globals_shelter, lazy_calls.vec_size.x);

  lazy_calls.vec_size_common = (struct r_lazy) { .x = r_parse("vec_size_common()"), .env = r_null };
  r_dyn_list_push_back(globals_shelter, lazy_calls.vec_size_common.x);
}
