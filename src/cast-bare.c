#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"

r_obj* int_as_logical(r_obj* x, bool* lossy) {
  int* data = r_int_begin(x);
  r_ssize n = r_length(x);

  r_obj* out = KEEP(r_alloc_logical(n));
  int* out_data = r_lgl_begin(out);

  for (r_ssize i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;

    if (elt == r_globals.na_int) {
      *out_data = r_globals.na_lgl;
      continue;
    }

    if (elt != 0 && elt != 1) {
      *lossy = true;
      FREE(1);
      return r_null;
    }

    *out_data = elt;
  }

  FREE(1);
  return out;
}

r_obj* dbl_as_logical(r_obj* x, bool* lossy) {
  double* data = r_dbl_begin(x);
  r_ssize n = r_length(x);

  r_obj* out = KEEP(r_alloc_logical(n));
  int* out_data = r_lgl_begin(out);

  for (r_ssize i = 0; i < n; ++i, ++data, ++out_data) {
    double elt = *data;

    if (isnan(elt)) {
      *out_data = r_globals.na_lgl;
      continue;
    }

    if (elt != 0 && elt != 1) {
      *lossy = true;
      FREE(1);
      return r_null;
    }

    *out_data = (int) elt;
  }

  FREE(1);
  return out;
}

r_obj* chr_as_logical(r_obj* x, bool* lossy) {
  r_obj* const* x_p = r_chr_cbegin(x);
  r_ssize n = r_length(x);

  r_obj* out = KEEP(r_alloc_logical(n));
  int* p_out = r_lgl_begin(out);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* str = x_p[i];
    if (str == r_globals.na_str) {
      p_out[i] = r_globals.na_lgl;
      continue;
    }

    const char* elt = r_str_c_string(str);
    switch (elt[0]) {
    case 'T':
      if (elt[1] == '\0' || strcmp(elt, "TRUE") == 0) {
        p_out[i] = 1;
        continue;
      }
      break;
    case 'F':
      if (elt[1] == '\0' || strcmp(elt, "FALSE") == 0) {
        p_out[i] = 0;
        continue;
      }
      break;
    case 't':
      if (strcmp(elt, "true") == 0) {
        p_out[i] = 1;
        continue;
      }
      break;
    case 'f':
      if (strcmp(elt, "false") == 0) {
        p_out[i] = 0;
        continue;
      }
      break;
    default:
      break;
    }

    *lossy = true;
    FREE(1);
    return r_null;
  }

  FREE(1);
  return out;
}

r_obj* lgl_as_integer(r_obj* x, bool* lossy) {
  return Rf_coerceVector(x, INTSXP);
}

r_obj* dbl_as_integer(r_obj* x, bool* lossy) {
  double* data = r_dbl_begin(x);
  r_ssize n = r_length(x);

  r_obj* out = KEEP(r_alloc_integer(n));
  int* out_data = r_int_begin(out);

  for (r_ssize i = 0; i < n; ++i, ++data, ++out_data) {
    double elt = *data;

    if (elt <= INT_MIN || elt >= INT_MAX + 1.0) {
      *lossy = true;
      FREE(1);
      return r_null;
    }

    if (isnan(elt)) {
      *out_data = r_globals.na_int;
      continue;
    }

    int value = (int) elt;

    if (value != elt) {
      *lossy = true;
      FREE(1);
      return r_null;
    }

    *out_data = value;
  }

  FREE(1);
  return out;
}

r_obj* lgl_as_double(r_obj* x, bool* lossy) {
  int* data = r_lgl_begin(x);
  r_ssize n = r_length(x);

  r_obj* out = KEEP(r_alloc_double(n));
  double* out_data = r_dbl_begin(out);

  for (r_ssize i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;
    *out_data = (elt == r_globals.na_lgl) ? r_globals.na_dbl : elt;
  }

  FREE(1);
  return out;
}

r_obj* int_as_double(r_obj* x, bool* lossy) {
  int* data = r_int_begin(x);
  r_ssize n = r_length(x);

  r_obj* out = KEEP(r_alloc_double(n));
  double* out_data = r_dbl_begin(out);

  for (r_ssize i = 0; i < n; ++i, ++data, ++out_data) {
    int elt = *data;
    *out_data = (elt == r_globals.na_int) ? r_globals.na_dbl : elt;
  }

  FREE(1);
  return out;
}
