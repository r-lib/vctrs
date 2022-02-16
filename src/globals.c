#include "vctrs.h"

struct syms syms;
struct strings strings;
struct fns fns;
struct vec_args vec_args;

struct r_dyn_array* strings_shelter = NULL;

void vctrs_init_globals(r_obj* ns) {
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

  strings_shelter = r_new_dyn_vector(R_TYPE_list, 20);
  r_preserve(strings_shelter->shelter);

  strings.AsIs = r_str("AsIs");
  r_dyn_list_push_back(strings_shelter, strings.AsIs);

  static struct vctrs_arg dot_size; dot_size = new_wrapper_arg(NULL, ".size");
  vec_args.dot_size = &dot_size;
}
