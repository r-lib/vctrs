#ifndef VCTRS_GLOBALS_H
#define VCTRS_GLOBALS_H

#include <rlang.h>

struct syms {
  r_obj* arg;
  r_obj* dot_call;
  r_obj* haystack_arg;
  r_obj* needles_arg;
  r_obj* repair_arg;
  r_obj* to_arg;
  r_obj* value_arg;
  r_obj* vec_default_cast;
  r_obj* x_arg;
  r_obj* y_arg;
};

extern struct syms syms;

#endif
