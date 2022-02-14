#include <rlang.h>
#include "globals.h"

struct syms syms;

void vctrs_init_globals(r_obj* ns) {
  syms.arg = r_sym("arg");
  syms.dot_call = r_sym(".call");
  syms.haystack_arg = r_sym("haystack_arg");
  syms.needles_arg = r_sym("needles_arg");
  syms.repair_arg = r_sym("repair_arg");
  syms.to_arg = r_sym("to_arg");
  syms.value_arg = r_sym("value_arg");
  syms.x_arg = r_sym("x_arg");
  syms.y_arg = r_sym("y_arg");
}
