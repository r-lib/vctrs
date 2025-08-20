#ifndef VCTRS_H
#define VCTRS_H

#include "vctrs-core.h"


// Vector types -------------------------------------------------

SEXP vec_unspecified(R_len_t n);
bool vec_is_unspecified(SEXP x);


#include "type-info.h"

#include "arg-counter.h"
#include "arg.h"
#include "assert.h"
#include "c.h"
#include "c-unchop.h"
#include "case-when.h"
#include "cast-bare.h"
#include "cast-dispatch.h"
#include "cast.h"
#include "compare.h"
#include "complete.h"
#include "conditions.h"
#include "dictionary.h"
#include "dim.h"
#include "equal.h"
#include "expand.h"
#include "hash.h"
#include "lazy.h"
#include "match-compare.h"
#include "match-joint.h"
#include "missing.h"
#include "names.h"
#include "order-collate.h"
#include "order-groups.h"
#include "order-sortedness.h"
#include "order-truelength.h"
#include "order.h"
#include "owned.h"
#include "poly-op.h"
#include "proxy.h"
#include "proxy-restore.h"
#include "ptype-common.h"
#include "ptype.h"
#include "ptype2-dispatch.h"
#include "ptype2.h"
#include "rep.h"
#include "runs.h"
#include "set.h"
#include "shape.h"
#include "size-common.h"
#include "size.h"
#include "slice-assign.h"
#include "slice.h"
#include "slice-chop.h"
#include "strides.h"
#include "subscript-loc.h"
#include "subscript.h"
#include "translate.h"
#include "typeof2.h"
#include "typeof2-s3.h"
#include "utils-dispatch.h"
#include "utils.h"


// Vector methods ------------------------------------------------

enum vctrs_proxy_kind {
  VCTRS_PROXY_KIND_equal = 0,
  VCTRS_PROXY_KIND_compare,
  VCTRS_PROXY_KIND_order
};

SEXP vec_proxy(SEXP x);
SEXP vec_proxy_equal(SEXP x);
SEXP vec_proxy_compare(SEXP x);
SEXP vec_proxy_order(SEXP x);
SEXP vec_proxy_unwrap(SEXP x);
SEXP vec_slice_shaped(enum vctrs_type type, SEXP x, SEXP index);
bool vec_requires_fallback(SEXP x, struct vctrs_proxy_info info);
r_obj* vec_ptype(r_obj* x, struct vctrs_arg* x_arg, struct r_lazy call);
SEXP vec_ptype_finalise(SEXP x);
bool vec_is_unspecified(SEXP x);
SEXP vec_names(SEXP x);
SEXP vec_proxy_names(SEXP x);
SEXP vec_group_loc(SEXP x);
SEXP vec_match_params(SEXP needles,
                      SEXP haystack,
                      bool na_equal,
                      struct vctrs_arg* needles_arg,
                      struct vctrs_arg* haystack_arg,
                      struct r_lazy call);

static inline
SEXP vec_match(SEXP needles, SEXP haystack) {
  return vec_match_params(needles, haystack, true, NULL, NULL, r_lazy_null);
}


bool is_data_frame(SEXP x);

uint32_t hash_object(SEXP x);
void hash_fill(uint32_t* p, R_len_t n, SEXP x, bool na_equal);

SEXP vec_unique(SEXP x);
bool duplicated_any(SEXP names);

// Data frame column iteration ----------------------------------

// Used in functions that treat data frames as vectors of rows, but
// iterate over columns. Examples are `vec_equal()` and
// `vec_compare()`.

/**
 * @member row_known A boolean array of size `n_row`. Allocated on the R heap.
 *   Initially, all values are initialized to `false`. As we iterate along the
 *   columns, we flip the corresponding row's `row_known` value to `true` if we
 *   can determine the `out` value for that row from the current columns.
 *   Once a row's `row_known` value is `true`, we never check that row again
 *   as we continue through the columns.
 * @member p_row_known A pointer to the boolean array stored in `row_known`.
 *   Initialized with `(bool*) RAW(info.row_known)`.
 * @member remaining The number of `row_known` values that are still `false`.
 *   If this hits `0` before we traverse the entire data frame, we can exit
 *   immediately because all `out` values are already known.
 * @member size The number of rows in the data frame.
 */
struct df_short_circuit_info {
  SEXP row_known;
  bool* p_row_known;
  PROTECT_INDEX row_known_pi;
  R_len_t remaining;
  R_len_t size;
};

#define PROTECT_DF_SHORT_CIRCUIT_INFO(p_info, p_n) do {             \
  PROTECT_WITH_INDEX((p_info)->row_known, &(p_info)->row_known_pi); \
  *(p_n) += 1;                                                      \
} while (0)

static inline struct df_short_circuit_info new_df_short_circuit_info(R_len_t size, bool lazy) {
  SEXP row_known;
  bool* p_row_known;

  if (lazy) {
    row_known = PROTECT(R_NilValue);
    p_row_known = NULL;
  } else {
    row_known = PROTECT(Rf_allocVector(RAWSXP, size * sizeof(bool)));
    p_row_known = (bool*) RAW(row_known);

    // To begin with, no rows have a known comparison value
    memset(p_row_known, false, size * sizeof(bool));
  }

  struct df_short_circuit_info info = {
    .row_known = row_known,
    .p_row_known = p_row_known,
    .remaining = size,
    .size = size
  };

  UNPROTECT(1);
  return info;
}

static inline void init_lazy_df_short_circuit_info(struct df_short_circuit_info* p_info) {
  if (p_info->row_known != R_NilValue) {
    return;
  }

  p_info->row_known = Rf_allocVector(RAWSXP, p_info->size * sizeof(bool));
  REPROTECT(p_info->row_known, p_info->row_known_pi);

  p_info->p_row_known = (bool*) RAW(p_info->row_known);
}


// Factor methods -----------------------------------------------

SEXP chr_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg);
SEXP chr_as_ordered(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg);

SEXP fct_as_character(SEXP x, struct vctrs_arg* x_arg);
SEXP fct_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);

SEXP ord_as_character(SEXP x, struct vctrs_arg* x_arg);

// Datetime methods ---------------------------------------------

SEXP date_as_date(SEXP x);
SEXP date_as_posixct(SEXP x, SEXP to);
SEXP date_as_posixlt(SEXP x, SEXP to);
SEXP posixct_as_date(SEXP x, bool* lossy);
SEXP posixlt_as_date(SEXP x, bool* lossy);
SEXP posixct_as_posixct(SEXP x, SEXP to);
SEXP posixlt_as_posixct(SEXP x, SEXP to);
SEXP posixct_as_posixlt(SEXP x, SEXP to);
SEXP posixlt_as_posixlt(SEXP x, SEXP to);

SEXP vec_date_restore(SEXP x, SEXP to, const enum vctrs_owned owned);
SEXP vec_posixct_restore(SEXP x, SEXP to, const enum vctrs_owned owned);
SEXP vec_posixlt_restore(SEXP x, SEXP to, const enum vctrs_owned owned);

SEXP date_datetime_ptype2(SEXP x, SEXP y);
SEXP datetime_datetime_ptype2(SEXP x, SEXP y);

// Growable vector ----------------------------------------------

struct growable {
  SEXP x;
  SEXPTYPE type;
  void* array;
  PROTECT_INDEX idx;
  int n;
  int capacity;
};

struct growable new_growable(SEXPTYPE type, int capacity);
SEXP growable_values(struct growable* g);

static inline void growable_push_int(struct growable* g, int i) {
  if (g->n == g->capacity) {
    g->capacity *= 2;
    g->x = Rf_lengthgets(g->x, g->capacity);
    REPROTECT(g->x, g->idx);
    g->array = INTEGER(g->x);
  }

  int* p = (int*) g->array;
  p[g->n] = i;
  ++(g->n);
}

#define PROTECT_GROWABLE(g, n) do {             \
    PROTECT_WITH_INDEX((g)->x, &((g)->idx));    \
    *n += 1;                                    \
  } while(0)

#define UNPROTECT_GROWABLE(g) do { UNPROTECT(1);} while(0)

// Conditions ---------------------------------------------------

r_no_return
void stop_scalar_type(SEXP x,
                      struct vctrs_arg* arg,
                      struct r_lazy call);
__attribute__((noreturn))
void stop_assert_size(r_ssize actual,
                      r_ssize required,
                      struct vctrs_arg* arg,
                      struct r_lazy call);
__attribute__((noreturn))
void stop_incompatible_type(SEXP x,
                            SEXP y,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg,
                            bool cast);
__attribute__((noreturn))
void stop_recycle_incompatible_size(r_ssize x_size,
                                    r_ssize size,
                                    struct vctrs_arg* x_arg,
                                    struct r_lazy call);
__attribute__((noreturn))
void stop_incompatible_shape(SEXP x, SEXP y,
                             R_len_t x_size, R_len_t y_size, int axis,
                             struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);
void stop_corrupt_factor_levels(SEXP x, struct vctrs_arg* arg) __attribute__((noreturn));
void stop_corrupt_ordered_levels(SEXP x, struct vctrs_arg* arg) __attribute__((noreturn));

#endif
