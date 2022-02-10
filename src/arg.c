#include "vctrs.h"
#include "utils.h"

#include "decl/arg-decl.h"


// Materialising argument tags ------------------------------------------

#define DEFAULT_ARG_BUF_SIZE 100

static int fill_arg_buffer(struct vctrs_arg* arg,
                           char* buf,
                           r_ssize cur_size,
                           r_ssize tot_size);

/**
 * This takes a `struct vctrs_arg{}` linked list and calls the
 * recursive function `fill_arg_buffer()`. It allocates a buffer in a
 * RAWSXP of size 100, which is grown by a factor of 1.5 each time the
 * `fill()` methods return a negative value. Returns a character
 * vector of size 1 containing the materialised argument tag.
 */
SEXP vctrs_arg(struct vctrs_arg* arg) {
  if (!arg) {
    return chrs_empty;
  }

  r_ssize next_size = DEFAULT_ARG_BUF_SIZE;
  r_ssize size;

  SEXP buf_holder = PROTECT(R_NilValue);
  char* buf;

  do {
    size = next_size;

    UNPROTECT(1);
    buf_holder = PROTECT(Rf_allocVector(RAWSXP, size));
    buf = (char*) RAW(buf_holder);

    // Reallocate a larger buffer at the next iteration if the current
    // buffer turns out too small
    next_size *= 1.5;
  } while (fill_arg_buffer(arg, buf, 0, size) < 0);

  SEXP out = Rf_mkString(buf);

  UNPROTECT(1);
  return out;
}

/**
 * Takes a `struct vctrs_arg{}` linked list and a buffer and calls the
 * `fill()` method on each of those, recursively. Unless an error
 * occurred, it returns the current size written to the buffer so we
 * can track the remaining memory available in the buffer after
 * recursion.
 */
static int fill_arg_buffer(struct vctrs_arg* arg,
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

struct vctrs_arg new_wrapper_arg(struct vctrs_arg* parent, const char* arg) {
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
  r_obj* x;
  r_ssize* p_i;
};

struct vctrs_arg* new_subscript_arg(struct vctrs_arg* parent,
                                    r_obj* x,
                                    r_ssize* p_i) {
  r_obj* shelter = r_alloc_raw(sizeof(struct subscript_arg_data));

  struct subscript_arg_data* p_data = r_raw_begin(shelter);
  p_data->self = (struct vctrs_arg) {
    .shelter = shelter,
    .parent = parent,
    .fill = &subscript_arg_fill,
    .data = p_data
  };
  p_data->x = x;
  p_data->p_i = p_i;

  return (struct vctrs_arg*) p_data;
}

static
r_ssize subscript_arg_fill(void* p_data_, char* buf, r_ssize remaining) {
  struct subscript_arg_data* p_data = (struct subscript_arg_data*) p_data_;

  r_ssize i = *p_data->p_i;
  r_obj* x = p_data->x;

  r_obj* names = vec_names(x);
  r_ssize n = vec_size(x);

  if (i >= n) {
    r_stop_internal("subscript_arg_fill", "`i` can't be greater than `vec_size(x)`.");
  }

  int len;
  if (r_has_name_at(names, i)) {
    len = snprintf(buf, remaining, "$%s", r_chr_get_c_string(names, i));
  } else {
    len = snprintf(buf, remaining, "[[%td]]", i + 1);
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

struct arg_data_counter new_counter_arg_data(R_len_t* i, SEXP* names, R_len_t* names_i) {
  return (struct arg_data_counter) {
    .i = i,
    .names = names,
    .names_i = names_i
  };
}

static r_ssize counter_arg_fill(void* data_, char* buf, r_ssize remaining) {
  struct arg_data_counter* data = (struct arg_data_counter*) data_;
  R_len_t i = *data->i;

  SEXP names = *data->names;
  R_len_t names_i = *data->names_i;

  int len;
  if (r_has_name_at(names, names_i)) {
    // FIXME: Check for syntactic names
    len = snprintf(buf, remaining, "%s", r_chr_get_c_string(names, names_i));
  } else {
    len = snprintf(buf, remaining, "..%d", i + 1);
  }

  if (len >= remaining) {
    return -1;
  } else {
    return len;
  }
}


// Indexing tag that materialises as `$rhs`. The `$` is only written when
// the arg has a parent.

static r_ssize index_arg_fill(void* data, char* buf, r_ssize remaining);
static bool is_empty_arg(struct vctrs_arg* arg);

struct vctrs_arg new_index_arg(struct vctrs_arg* parent,
                               struct arg_data_index* data) {
  return (struct vctrs_arg) {
    .parent = parent,
    .fill = &index_arg_fill,
    .data = (void*) data
  };
}

struct arg_data_index new_index_arg_data(const char* arg,
                                         struct vctrs_arg* parent) {
  return (struct arg_data_index) {
    .arg = arg,
    .parent = parent
  };
}


static r_ssize index_arg_fill(void* data_, char* buf, r_ssize remaining) {
  struct arg_data_index* data = (struct arg_data_index*) data_;
  const char* src = data->arg;
  size_t len = strlen(src);

  bool child = is_empty_arg(data->parent);

  if (child) {
    ++len;
  }

  if (len >= remaining) {
    return -1;
  }

  if (child) {
    *buf++ = '$';
  }
  memcpy(buf, src, len);
  buf[len] = '\0';

  return len;
}

static bool is_empty_arg(struct vctrs_arg* arg) {
  if (!arg) {
    return true;
  }
  char tmp[1];
  return arg->fill(arg->data, tmp, 1) != 0;;
}
