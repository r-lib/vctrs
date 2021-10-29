#include <rlang.h>
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


// Objects -------------------------------------------------------------

// Simple wrapper around a `const char*` argument tag

static r_ssize wrapper_arg_fill(void* data, char* buf, r_ssize remaining);

struct vctrs_arg new_wrapper_arg(struct vctrs_arg* parent, const char* arg) {
  return (struct vctrs_arg) {
    .parent = parent,
    .fill = &wrapper_arg_fill,
    .data = (void*) arg
  };
}

static r_ssize wrapper_arg_fill(void* data, char* buf, r_ssize remaining) {
  const char* src = (const char*) data;

  size_t len = strlen(src);

  if (len >= remaining) {
    return -1;
  }

  memcpy(buf, src, len);
  buf[len] = '\0';

  return len;
}


// Wrapper that accesses a symbol in an environment, for lazy evaluation
static r_ssize lazy_arg_fill(void* data, char* buf, r_ssize remaining);

struct vctrs_arg new_lazy_arg(struct arg_data_lazy* data) {
  return (struct vctrs_arg) {
    .parent = NULL,
    .fill = &lazy_arg_fill,
    .data = data
  };
}

struct arg_data_lazy new_lazy_arg_data(SEXP environment, const char* arg_name) {
  if (environment == R_NilValue) {
    r_stop_internal("new_lazy_arg_data", "`env` can't be NULL.");
  }

  return (struct arg_data_lazy) {
    .environment = environment,
    .arg_name = arg_name
  };
}

static r_ssize lazy_arg_fill(void* data_, char* buf, r_ssize remaining) {
  struct arg_data_lazy* data = (struct arg_data_lazy*) data_;

  SEXP arg_name = r_sym(data->arg_name);
  SEXP value = PROTECT(r_env_get(data->environment, arg_name));
  const char* c_value;

  // NULL values are mapped to an empty string. We also silently ignore
  // non-character values or zero-length vectors and take the first element of a
  // character vector. We could do better with a warning or a combined error.
  if (!r_is_string(value) || r_length(value) < 1) {
    c_value = "";
  } else {
    c_value = r_chr_get_c_string(value, 0);
  }

  size_t len = strlen(c_value);

  if (len >= remaining) {
    UNPROTECT(1);
    return -1;
  }

  memcpy(buf, c_value, len);
  buf[len] = '\0';

  UNPROTECT(1);
  return len;
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
