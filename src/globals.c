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
  static struct vctrs_arg dot_size; dot_size = new_wrapper_arg(NULL, ".size");
  vec_args.dot_size = &dot_size;

  // Calls -------------------------------------------------------------
  lazy_calls.vec_recycle = (struct r_lazy) { .x = r_parse("vec_recycle()"), .env = r_null };
  r_dyn_list_push_back(globals_shelter, lazy_calls.vec_recycle.x);

  lazy_calls.vec_size = (struct r_lazy) { .x = r_parse("vec_size()"), .env = r_null };
  r_dyn_list_push_back(globals_shelter, lazy_calls.vec_size.x);
}
