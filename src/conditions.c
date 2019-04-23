#include "vctrs.h"
#include "utils.h"

#define DEFAULT_ARG_BUF 100


struct vctrs_arg new_vctrs_arg(struct vctrs_arg* parent, const char* arg) {
  struct vctrs_arg wrapper = {
    .parent = parent,
    .data = (const void* ) arg,
    .get = NULL
  };
  return wrapper;
}

static int fill_arg_buffer(struct vctrs_arg* arg,
                           char* buf,
                           size_t cur_size,
                           size_t tot_size) {
  if (arg->parent) {
    cur_size = fill_arg_buffer(arg->parent, buf, cur_size, tot_size);

    if (cur_size < 0) {
      return cur_size;
    }
  }

  const char* src = (arg->get) ? arg->get(arg) : (const char*) arg->data;
  size_t len = strlen(src);

  size_t next_size = cur_size + len;
  if (next_size >= tot_size) {
    return -1;
  }

  memcpy(buf + cur_size, src, len);
  buf[next_size] = '\0';

  return next_size;
}

SEXP vctrs_arg(struct vctrs_arg* arg) {
  size_t size = DEFAULT_ARG_BUF;

  SEXP buf_holder = PROTECT(R_NilValue);
  char* buf;

  do {
    UNPROTECT(1);
    buf_holder = PROTECT(Rf_allocVector(RAWSXP, size));
    buf = (char*) RAW(buf_holder);

    // Reallocate a larger buffer at the next iteration if the current
    // buffer turns out too small
    size *= 1.5;
  } while (fill_arg_buffer(arg, buf, 0, size) < 0);

  SEXP out = Rf_mkString(buf);

  UNPROTECT(1);
  return out;
}

struct vctrs_arg args_empty = { .data = (const void*) "", .get = NULL};


void stop_scalar_type(SEXP x, struct vctrs_arg* arg) {
  SEXP call = PROTECT(Rf_lang3(Rf_install("stop_scalar_type"),
                               PROTECT(r_protect(x)),
                               PROTECT(vctrs_arg(arg))));
  Rf_eval(call, vctrs_ns_env);
  Rf_error("Internal error: `stop_scalar_type()` should have jumped");
}

void vec_assert(SEXP x, struct vctrs_arg* arg) {
  if (!vec_is_vector(x)) {
    stop_scalar_type(x, arg);
  }
}
