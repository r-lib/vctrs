#include "vctrs.h"
#include "type-data-frame.h"
#include "dim.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy = NULL;
SEXP syms_vec_proxy_equal = NULL;
SEXP syms_vec_proxy_equal_array = NULL;
SEXP syms_vec_proxy_compare = NULL;
SEXP syms_vec_proxy_compare_array = NULL;
SEXP syms_vec_proxy_order = NULL;
SEXP syms_vec_proxy_order_array = NULL;

SEXP fns_vec_proxy_equal_array = NULL;
SEXP fns_vec_proxy_compare_array = NULL;
SEXP fns_vec_proxy_order_array = NULL;

static SEXP vec_proxy_unwrap(SEXP x);

SEXP vec_proxy_method(SEXP x);
SEXP vec_proxy_invoke(SEXP x, SEXP method);

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy(SEXP x) {
  int nprot = 0;
  struct vctrs_type_info info = vec_type_info(x);
  PROTECT_TYPE_INFO(&info, &nprot);

  SEXP out;
  if (info.type == vctrs_type_s3) {
    out = vec_proxy_invoke(x, info.proxy_method);
  } else {
    out = x;
  }

  UNPROTECT(nprot);
  return out;
}

static inline SEXP vec_proxy_equal_method(SEXP x);
static inline SEXP vec_proxy_equal_invoke(SEXP x, SEXP method);

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_equal(SEXP x) {
  SEXP method = PROTECT(vec_proxy_equal_method(x));
  SEXP out = vec_proxy_equal_invoke(x, method);
  UNPROTECT(1);
  return out;
}

static inline SEXP vec_proxy_compare_method(SEXP x);
static inline SEXP vec_proxy_compare_invoke(SEXP x, SEXP method);

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_compare(SEXP x) {
  SEXP method = PROTECT(vec_proxy_compare_method(x));
  SEXP out = vec_proxy_compare_invoke(x, method);
  UNPROTECT(1);
  return out;
}

static inline SEXP vec_proxy_order_method(SEXP x);
static inline SEXP vec_proxy_order_invoke(SEXP x, SEXP method);

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_order(SEXP x) {
  SEXP method = PROTECT(vec_proxy_order_method(x));
  SEXP out = vec_proxy_order_invoke(x, method);
  UNPROTECT(1);
  return out;
}


SEXP vec_proxy_method(SEXP x) {
  return s3_find_method("vec_proxy", x, vctrs_method_table);
}

// This should be faster than normal dispatch but also means that
// proxy methods can't call `NextMethod()`. This could be changed if
// it turns out a problem.
SEXP vec_proxy_invoke(SEXP x, SEXP method) {
  if (method == R_NilValue) {
    return x;
  } else {
    return vctrs_dispatch1(syms_vec_proxy, method, syms_x, x);
  }
}


#define VEC_PROXY_METHOD(GENERIC, FNS_PROXY_ARRAY) {                    \
  SEXP cls = PROTECT(s3_get_class(x));                                  \
  SEXP method = s3_class_find_method(GENERIC, cls, vctrs_method_table); \
                                                                        \
  if (method != R_NilValue) {                                           \
    UNPROTECT(1);                                                       \
    return method;                                                      \
  }                                                                     \
                                                                        \
  /* FIXME: Stopgap check for bare arrays */                            \
  /* which equality functions don't handle well */                      \
  if (vec_dim_n(x) > 1) {                                               \
    UNPROTECT(1);                                                       \
    return FNS_PROXY_ARRAY;                                             \
  }                                                                     \
                                                                        \
  UNPROTECT(1);                                                         \
  return R_NilValue;                                                    \
}

static inline
SEXP vec_proxy_equal_method(SEXP x) {
  VEC_PROXY_METHOD("vec_proxy_equal", fns_vec_proxy_equal_array);
}
static inline
SEXP vec_proxy_compare_method(SEXP x) {
  VEC_PROXY_METHOD("vec_proxy_compare", fns_vec_proxy_compare_array);
}
static inline
SEXP vec_proxy_order_method(SEXP x) {
  VEC_PROXY_METHOD("vec_proxy_order", fns_vec_proxy_order_array);
}

#undef VEC_PROXY_METHOD


#define VEC_PROXY_INVOKE(SYMS_PROXY, PROXY_DEFAULT) {          \
  if (method != R_NilValue) {                                  \
    return vctrs_dispatch1(SYMS_PROXY, method, syms_x, x);     \
  }                                                            \
                                                               \
  /* Fallback on S3 objects with no proxy */                   \
  if (vec_typeof(x) == vctrs_type_s3) {                        \
    return PROXY_DEFAULT(x);                                   \
  } else {                                                     \
    return x;                                                  \
  }                                                            \
}

static inline
SEXP vec_proxy_equal_invoke(SEXP x, SEXP method) {
  VEC_PROXY_INVOKE(syms_vec_proxy_equal, vec_proxy);
}
static inline
SEXP vec_proxy_compare_invoke(SEXP x, SEXP method) {
  VEC_PROXY_INVOKE(syms_vec_proxy_compare, vec_proxy_equal);
}
static inline
SEXP vec_proxy_order_invoke(SEXP x, SEXP method) {
  VEC_PROXY_INVOKE(syms_vec_proxy_order, vec_proxy_compare);
}

#undef VEC_PROXY_INVOKE


#define DF_PROXY(PROXY) do {                                   \
  R_len_t n_cols = Rf_length(x);                               \
                                                               \
  for (R_len_t i = 0; i < n_cols; ++i) {                       \
    SEXP col = VECTOR_ELT(x, i);                               \
    SET_VECTOR_ELT(x, i, PROXY(col));                          \
  }                                                            \
} while (0)

static
SEXP df_proxy(SEXP x, enum vctrs_proxy_kind kind) {
  x = PROTECT(r_clone_referenced(x));

  switch (kind) {
  case vctrs_proxy_kind_default: DF_PROXY(vec_proxy); break;
  case vctrs_proxy_kind_equal: DF_PROXY(vec_proxy_equal); break;
  case vctrs_proxy_kind_compare: DF_PROXY(vec_proxy_compare); break;
  case vctrs_proxy_kind_order: DF_PROXY(vec_proxy_order); break;
  }

  x = PROTECT(df_flatten(x));
  x = vec_proxy_unwrap(x);

  UNPROTECT(2);
  return x;
}

#undef DF_PROXY

// [[ register() ]]
SEXP vctrs_df_proxy(SEXP x, SEXP kind) {
  enum vctrs_proxy_kind c_kind;

  if (kind == Rf_install("default")) {
    c_kind = vctrs_proxy_kind_default;
  } else if (kind == Rf_install("equal")) {
    c_kind = vctrs_proxy_kind_equal;
  } else if (kind == Rf_install("compare")) {
    c_kind = vctrs_proxy_kind_compare;
  } else if (kind == Rf_install("order")) {
    c_kind = vctrs_proxy_kind_order;
  }else {
    Rf_error("Internal error: Unexpected proxy kind `%s`.", CHAR(PRINTNAME(kind)));
  }

  return df_proxy(x, c_kind);
}


static
SEXP vec_proxy_unwrap(SEXP x) {
  if (TYPEOF(x) == VECSXP && XLENGTH(x) == 1 && is_data_frame(x)) {
    x = vec_proxy_unwrap(VECTOR_ELT(x, 0));
  }
  return x;
}


// [[ register() ]]
SEXP vctrs_unset_s4(SEXP x) {
  x = r_clone_referenced(x);
  r_unmark_s4(x);
  return x;
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy = Rf_install("vec_proxy");

  syms_vec_proxy_equal = Rf_install("vec_proxy_equal");
  syms_vec_proxy_equal_array = Rf_install("vec_proxy_equal.array");

  syms_vec_proxy_compare = Rf_install("vec_proxy_compare");
  syms_vec_proxy_compare_array = Rf_install("vec_proxy_compare.array");

  syms_vec_proxy_order = Rf_install("vec_proxy_order");
  syms_vec_proxy_order_array = Rf_install("vec_proxy_order.array");

  fns_vec_proxy_equal_array = r_env_get(ns, syms_vec_proxy_equal_array);
  fns_vec_proxy_compare_array = r_env_get(ns, syms_vec_proxy_compare_array);
  fns_vec_proxy_order_array = r_env_get(ns, syms_vec_proxy_order_array);
}
