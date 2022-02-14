#ifndef VCTRS_ARG_H
#define VCTRS_ARG_H

#include "vctrs-core.h"


/**
 * Structure for argument tags
 *
 * Argument tags are used in error messages to provide information
 * about which elements of nested data structures (such as tibbles)
 * fail to match a given type. They are generated lazily by the `fill`
 * method in order to avoid any cost when there is no error.
 *
 * @member parent The previously active argument tag.
 * @member fill Takes a pointer to data, and a buffer to fill. If the
 *   buffer is too small according to the `remaining` argument,
 *   `fill()` must return a negative error value.
 */
struct vctrs_arg {
  r_obj* shelter;
  struct vctrs_arg* parent;
  r_ssize (*fill)(void* data, char* buf, r_ssize remaining);
  void* data;
};


// Simple wrapper around a string
struct vctrs_arg new_wrapper_arg(struct vctrs_arg* parent,
                                 const char* arg);


struct vctrs_arg new_lazy_arg(struct r_lazy* data);


// Wrapper around a counter representing the current position of the
// argument
struct arg_data_counter {
  r_ssize* i;
  r_obj** names;
  r_ssize* names_i;
};

struct vctrs_arg new_counter_arg(struct vctrs_arg* parent,
                                 struct arg_data_counter* data);

struct arg_data_counter new_counter_arg_data(r_ssize* i,
                                             r_obj** names,
                                             r_ssize* names_i);


struct vctrs_arg* new_subscript_arg_vec(struct vctrs_arg* parent,
                                        r_obj* x,
                                        r_ssize* p_i);

struct vctrs_arg* new_subscript_arg(struct vctrs_arg* parent,
                                    r_obj* names,
                                    r_ssize n,
                                    r_ssize* p_i);

// Materialise an argument tag as a CHARSXP.
r_obj* vctrs_arg(struct vctrs_arg* arg);


#endif
