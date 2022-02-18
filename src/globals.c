#include "vctrs.h"

struct syms syms;
struct strings strings;
struct fns fns;
struct vec_args vec_args;
struct lazy_calls lazy_calls;

struct r_dyn_array* globals_shelter = NULL;


#define INIT_ARG(ARG)                                             \
  static struct vctrs_arg ARG; ARG = new_wrapper_arg(NULL, #ARG); \
  vec_args.ARG = &ARG

#define INIT_ARG2(ARG, STR)                                      \
  static struct vctrs_arg ARG; ARG = new_wrapper_arg(NULL, STR); \
  vec_args.ARG = &ARG

#define INIT_STRING(ARG)                                \
  strings.ARG = r_str(#ARG);                            \
  r_dyn_list_push_back(globals_shelter, strings.ARG);

#define INIT_CALL(ARG)                                                  \
  lazy_calls.ARG = (struct r_lazy) { .x = r_parse(#ARG "()"), .env = r_null }; \
  r_dyn_list_push_back(globals_shelter, lazy_calls.ARG.x)


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
  INIT_STRING(AsIs);

  // Args --------------------------------------------------------------
  INIT_ARG2(dot_ptype, ".ptype");
  INIT_ARG2(dot_size, ".size");
  INIT_ARG2(empty, "");
  INIT_ARG(i);
  INIT_ARG(max_fill);
  INIT_ARG(n);
  INIT_ARG(value);
  INIT_ARG(x);

  // Calls -------------------------------------------------------------
  INIT_CALL(vec_assign);
  INIT_CALL(vec_assign_params);
  INIT_CALL(vec_assign_seq);
  INIT_CALL(vec_recycle);
  INIT_CALL(vec_recycle_common);
  INIT_CALL(vec_size);
  INIT_CALL(vec_size_common);
}
