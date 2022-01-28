#ifndef VCTRS_ARG_H
#define VCTRS_ARG_H

#include "rlang-dev.h"


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
  R_len_t* i;
  SEXP* names;
  R_len_t* names_i;
};

struct vctrs_arg new_counter_arg(struct vctrs_arg* parent,
                                 struct arg_data_counter* data);

struct arg_data_counter new_counter_arg_data(R_len_t* i,
                                             SEXP* names,
                                             R_len_t* names_i);


// Wrapper around a string that should be prefixed with `$`, unless
// that's the first argument of the chain

struct arg_data_index {
  const char* arg;
  struct vctrs_arg* parent;
};

struct vctrs_arg new_index_arg(struct vctrs_arg* parent,
                               struct arg_data_index* data);

struct arg_data_index new_index_arg_data(const char* arg,
                                         struct vctrs_arg* parent);


// Materialise an argument tag as a CHARSXP.
SEXP vctrs_arg(struct vctrs_arg* arg);


#endif
