#ifndef VCTRS_GLOBALS_H
#define VCTRS_GLOBALS_H

struct syms {
  r_obj* arg;
  r_obj* haystack_arg;
  r_obj* needles_arg;
  r_obj* repair_arg;
  r_obj* to_arg;
  r_obj* value_arg;
  r_obj* x_arg;
  r_obj* y_arg;
};

extern struct syms syms;


#endif
