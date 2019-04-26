#ifndef VCTRS_ARG_H
#define VCTRS_ARG_H


/**
 * Structure for argument tags
 *
 * Argument tags are used in error messages to provide information
 * about which elements of nested data structures (such as tibbles)
 * fail to match a given type. They are generated lazily by the `fill`
 * method in order to avoid any cost when there is no error.
 *
 * `vctrs_arg` is meant to be used with C-style inheritance. It should
 * be stored as the first member of derived structs. See
 * <https://www.python.org/dev/peps/pep-3123/>.
 *
 * @member parent The previously active argument tag.
 * @member fill Takes a pointer to self, which can be cast back to the
 *   original type containing the data, and a buffer to fill. If the
 *   buffer is too small according to the `remaining` argument,
 *   `fill()` must return a negative error value.
 */
struct vctrs_arg {
  struct vctrs_arg* parent;
  r_ssize_t (*fill)(struct vctrs_arg* self, char* buf, r_ssize_t remaining);
};


/**
 * Derived classes of `vctrs_arg`.
 *
 * - `vctrs_arg_wrapper` is a simple wrapper around a string.
 * - `vctrs_arg_counter` wraps a counter representing the current
 *   position of the argument.
 */
struct vctrs_arg_wrapper {
  struct vctrs_arg iface;
  const char* arg;
};
struct vctrs_arg_counter {
  struct vctrs_arg iface;
  R_len_t* i;
};


/**
 * Constructors for argument wrapper and counters.
 */
struct vctrs_arg_wrapper new_wrapper_arg(struct vctrs_arg* parent, const char* arg);
struct vctrs_arg_counter new_counter_arg(struct vctrs_arg* parent, R_len_t* i);

/**
 * Materialise an argument tag. Returns a CHARSXP.
 */
SEXP vctrs_arg(struct vctrs_arg* arg);


#endif
