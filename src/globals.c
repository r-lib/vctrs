#include "vctrs.h"

struct syms syms;
struct strings strings;
struct chrs chrs;
struct fns fns;
struct vec_args vec_args;
struct lazy_args lazy_args;
struct lazy_calls lazy_calls;

struct r_dyn_array* globals_shelter = NULL;


#define INIT_ARG(ARG)                                             \
  static struct vctrs_arg ARG; ARG = new_wrapper_arg(NULL, #ARG); \
  vec_args.ARG = &ARG

#define INIT_ARG2(ARG, STR)                                      \
  static struct vctrs_arg ARG; ARG = new_wrapper_arg(NULL, STR); \
  vec_args.ARG = &ARG

// Defines both a string and a length 1 character vector
#define INIT_STRING(ARG)                                \
  strings.ARG = r_str(#ARG);                            \
  r_dyn_list_push_back(globals_shelter, strings.ARG);   \
  chrs.ARG = r_chr(#ARG);                               \
  r_dyn_list_push_back(globals_shelter, chrs.ARG);

#define INIT_LAZY_ARG(ARG)                                              \
  lazy_args.ARG = (struct r_lazy) { .x = r_chr(#ARG), .env = r_null };  \
  r_dyn_list_push_back(globals_shelter, lazy_calls.ARG.x)

#define INIT_LAZY_ARG_2(ARG, STR)                                       \
  lazy_args.ARG = (struct r_lazy) { .x = r_chr(STR), .env = r_null };   \
  r_dyn_list_push_back(globals_shelter, lazy_args.ARG.x)

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
  syms.dot_error_call = r_sym(".error_call");
  syms.haystack_arg = r_sym("haystack_arg");
  syms.needles_arg = r_sym("needles_arg");
  syms.recurse = r_sym("recurse");
  syms.repair_arg = r_sym("repair_arg");
  syms.times_arg = r_sym("times_arg");
  syms.to_arg = r_sym("to_arg");
  syms.value_arg = r_sym("value_arg");
  syms.x_arg = r_sym("x_arg");
  syms.y_arg = r_sym("y_arg");

  // Strings and characters --------------------------------------------
  INIT_STRING(AsIs);
  INIT_STRING(repair);

  // Args --------------------------------------------------------------
  INIT_ARG2(dot_name_repair, ".name_repair");
  INIT_ARG2(dot_ptype, ".ptype");
  INIT_ARG2(dot_size, ".size");
  INIT_ARG2(empty, "");
  INIT_ARG(i);
  INIT_ARG(max_fill);
  INIT_ARG(n);
  INIT_ARG(value);
  INIT_ARG(x);
  INIT_ARG(indices);

  // Lazy args ---------------------------------------------------------
  INIT_LAZY_ARG_2(dot_name_repair, ".name_repair");

  // Calls -------------------------------------------------------------
  INIT_CALL(vec_assign);
  INIT_CALL(vec_assign_params);
  INIT_CALL(vec_assign_seq);
  INIT_CALL(vec_init);
  INIT_CALL(vec_ptype_finalise);
  INIT_CALL(vec_recycle);
  INIT_CALL(vec_recycle_common);
  INIT_CALL(vec_size);
  INIT_CALL(vec_size_common);
}
