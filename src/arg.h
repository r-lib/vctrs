#ifndef VCTRS_ARG_H
#define VCTRS_ARG_H

#include "vctrs-core.h"


// Materialise an argument tag as a CHARSXP.
r_obj* vctrs_arg(struct vctrs_arg* arg);

// Materialise an argument tag as a vmax-protected C string.
const char* vec_arg_format(struct vctrs_arg* p_arg);


// Simple wrapper around a string
struct vctrs_arg new_wrapper_arg(struct vctrs_arg* parent,
                                 const char* arg);


struct vctrs_arg new_lazy_arg(struct r_lazy* data);


// Wrapper around a counter representing the current position of the
// argument
struct arg_data_counter {
  struct vctrs_arg* p_parent;
  r_ssize* i;
  r_obj** names;
  r_ssize* names_i;
};

struct vctrs_arg new_counter_arg(struct vctrs_arg* parent,
                                 struct arg_data_counter* data);

struct arg_data_counter new_counter_arg_data(struct vctrs_arg* p_parent,
                                             r_ssize* i,
                                             r_obj** names,
                                             r_ssize* names_i);


struct vctrs_arg* new_subscript_arg_vec(struct vctrs_arg* parent,
                                        r_obj* x,
                                        r_ssize* p_i);

struct vctrs_arg* new_subscript_arg(struct vctrs_arg* parent,
                                    r_obj* names,
                                    r_ssize n,
                                    r_ssize* p_i);


#endif
