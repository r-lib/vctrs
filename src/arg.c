#include "vctrs.h"
#include "decl/arg-decl.h"


// Materialising argument tags ------------------------------------------

#define DEFAULT_ARG_BUF_SIZE 100

/**
 * This takes a `struct vctrs_arg{}` linked list and calls the
 * recursive function `fill_arg_buffer()`. It allocates a buffer in a
 * RAWSXP of size 100, which is grown by a factor of 1.5 each time the
 * `fill()` methods return a negative value. Returns a character
 * vector of size 1 containing the materialised argument tag.
 */
r_obj* vctrs_arg(struct vctrs_arg* arg) {
  if (!arg) {
    return chrs_empty;
  }

  r_ssize next_size = DEFAULT_ARG_BUF_SIZE;
  r_ssize size;

  r_obj* buf_holder = KEEP(r_null);
  char* buf;

  do {
    size = next_size;

    FREE(1);
    buf_holder = KEEP(r_alloc_raw(size));
    buf = (char*) r_raw_begin(buf_holder);

    // Reallocate a larger buffer at the next iteration if the current
    // buffer turns out too small
    next_size *= 1.5;
  } while (fill_arg_buffer(arg, buf, 0, size) < 0);

  r_obj* out = r_chr(buf);

  FREE(1);
  return out;
}

// vmax-protected
const char* vec_arg_format(struct vctrs_arg* p_arg) {
  r_obj* arg = KEEP(vctrs_arg(p_arg));
  const char* out = r_format_error_arg(arg);
  FREE(1);
  return out;
}

/**
 * Takes a `struct vctrs_arg{}` linked list and a buffer and calls the
 * `fill()` method on each of those, recursively. Unless an error
 * occurred, it returns the current size written to the buffer so we
 * can track the remaining memory available in the buffer after
 * recursion.
 */
static
int fill_arg_buffer(struct vctrs_arg* arg,
                    char* buf,
                    r_ssize cur_size,
                    r_ssize tot_size) {
  if (arg->parent) {
    cur_size = fill_arg_buffer(arg->parent, buf, cur_size, tot_size);

    if (cur_size < 0) {
      return cur_size;
    }
  }

  r_ssize written = arg->fill(arg->data, buf + cur_size, tot_size - cur_size);

  if (written < 0) {
    return written;
  } else {
    return cur_size + written;
  }
}

static
r_ssize str_arg_fill(const char* data, char* buf, r_ssize remaining) {
  size_t len = strlen(data);

  if (len >= remaining) {
    return -1;
  }

  memcpy(buf, data, len);
  buf[len] = '\0';

  return len;
}


// Objects -------------------------------------------------------------

// Simple wrapper around a `const char*` argument tag

struct vctrs_arg new_wrapper_arg(struct vctrs_arg* parent,
                                 const char* arg) {
  return (struct vctrs_arg) {
    .parent = parent,
    .fill = &wrapper_arg_fill,
    .data = (void*) arg
  };
}

static
r_ssize wrapper_arg_fill(void* data, char* buf, r_ssize remaining) {
  return str_arg_fill((const char*) data, buf, remaining);
}


// Wrapper that accesses a symbol in an environment, for lazy evaluation

struct vctrs_arg new_lazy_arg(struct r_lazy* arg) {
  return (struct vctrs_arg) {
    .parent = NULL,
    .fill = &lazy_arg_fill,
    .data = arg
  };
}

static
r_ssize lazy_arg_fill(void* data_, char* buf, r_ssize remaining) {
  struct r_lazy* data = (struct r_lazy*) data_;

  r_obj* arg = KEEP(r_lazy_eval(*data));

  const char* arg_str = "";
  if (r_is_string(arg)) {
    arg_str = r_chr_get_c_string(arg, 0);
  } else if (arg != r_null) {
    r_abort("`arg` must be a string.");
  }

  r_ssize out = str_arg_fill(arg_str, buf, remaining);

  FREE(1);
  return out;
}


// Wrapper around a subscript, either numeric or character

struct subscript_arg_data {
  struct vctrs_arg self;
  r_obj* names;
  r_ssize n;
  r_ssize* p_i;
};

struct vctrs_arg* new_subscript_arg_vec(struct vctrs_arg* parent,
                                        r_obj* x,
                                        r_ssize* p_i) {
  r_obj* names = KEEP(vec_names(x));
  struct vctrs_arg* p_arg = new_subscript_arg(parent,
                                              names,
                                              vec_size(x),
                                              p_i);
  FREE(1);
  return p_arg;
}

struct vctrs_arg* new_subscript_arg(struct vctrs_arg* parent,
                                    r_obj* names,
                                    r_ssize n,
                                    r_ssize* p_i) {
  r_obj* shelter = KEEP(r_alloc_list(2));
  r_list_poke(shelter, 0, r_alloc_raw(sizeof(struct subscript_arg_data)));
  r_list_poke(shelter, 1, names);

  struct subscript_arg_data* p_data = r_raw_begin(r_list_get(shelter, 0));
  p_data->self = (struct vctrs_arg) {
    .shelter = shelter,
    .parent = parent,
    .fill = &subscript_arg_fill,
    .data = p_data
  };
  p_data->names = names;
  p_data->n = n;
  p_data->p_i = p_i;

  FREE(1);
  return (struct vctrs_arg*) p_data;
}

static
r_ssize subscript_arg_fill(void* p_data_, char* buf, r_ssize remaining) {
  struct subscript_arg_data* p_data = (struct subscript_arg_data*) p_data_;

  r_ssize i = *p_data->p_i;

  r_obj* names = p_data->names;
  r_ssize n = p_data->n;

  if (i >= n) {
    r_stop_internal("`i = %" R_PRI_SSIZE "` can't be greater than `vec_size(x) = %" R_PRI_SSIZE "`.",
                    i,
                    n);
  }

  int len = 0;
  bool child = !is_empty_arg(p_data->self.parent);

  if (child) {
    if (r_has_name_at(names, i)) {
      len = snprintf(buf, remaining, "$%s", r_chr_get_c_string(names, i));
    } else {
      len = snprintf(buf, remaining, "[[%td]]", i + 1);
    }
  } else {
    if (r_has_name_at(names, i)) {
      len = snprintf(buf, remaining, "%s", r_chr_get_c_string(names, i));
    } else {
      len = snprintf(buf, remaining, "..%td", i + 1);
    }
  }

  if (len >= remaining) {
    return -1;
  } else {
    return len;
  }
}


// Wrapper around a counter representing the current position of the
// argument

struct vctrs_arg new_counter_arg(struct vctrs_arg* parent,
                                 struct arg_data_counter* data) {
  return (struct vctrs_arg) {
    .parent = parent,
    .fill = &counter_arg_fill,
    .data = (void*) data
  };
}

struct arg_data_counter new_counter_arg_data(struct vctrs_arg* p_parent,
                                             r_ssize* i,
                                             r_obj** names) {
  return (struct arg_data_counter) {
    .p_parent = p_parent,
    .i = i,
    .names = names
  };
}

static
r_ssize counter_arg_fill(void* data_, char* buf, r_ssize remaining) {
  struct arg_data_counter* data = (struct arg_data_counter*) data_;
  r_ssize i = *data->i;

  r_obj* names = *data->names;

  int len;
  bool child = !is_empty_arg(data->p_parent);

  // FIXME: Check for syntactic names
  if (child) {
    if (r_has_name_at(names, i)) {
      len = snprintf(buf, remaining, "$%s", r_chr_get_c_string(names, i));
    } else {
      len = snprintf(buf, remaining, "[[%" R_PRI_SSIZE "]]", i + 1);
    }
  } else {
    if (r_has_name_at(names, i)) {
      len = snprintf(buf, remaining, "%s", r_chr_get_c_string(names, i));
    } else {
      len = snprintf(buf, remaining, "..%" R_PRI_SSIZE, i + 1);
    }
  }

  if (len >= remaining) {
    return -1;
  } else {
    return len;
  }
}


static
bool is_empty_arg(struct vctrs_arg* arg) {
  if (!arg) {
    return true;
  }
  char tmp[1];
  return arg->fill(arg->data, tmp, 1) == 0;
}
