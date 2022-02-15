#ifndef VCTRS_H
#define VCTRS_H

#include "vctrs-core.h"


// Vector types -------------------------------------------------

bool vec_is_partial(SEXP x);

// After adding a new `vctrs_dispatch` type, add the missing entries
// in `vec_typeof2()`
enum vctrs_type2 {
  vctrs_type2_null_null,
  vctrs_type2_null_unspecified,
  vctrs_type2_null_logical,
  vctrs_type2_null_integer,
  vctrs_type2_null_double,
  vctrs_type2_null_complex,
  vctrs_type2_null_character,
  vctrs_type2_null_raw,
  vctrs_type2_null_list,
  vctrs_type2_null_dataframe,
  vctrs_type2_null_s3,
  vctrs_type2_null_scalar,

  vctrs_type2_unspecified_unspecified,
  vctrs_type2_unspecified_logical,
  vctrs_type2_unspecified_integer,
  vctrs_type2_unspecified_double,
  vctrs_type2_unspecified_complex,
  vctrs_type2_unspecified_character,
  vctrs_type2_unspecified_raw,
  vctrs_type2_unspecified_list,
  vctrs_type2_unspecified_dataframe,
  vctrs_type2_unspecified_s3,
  vctrs_type2_unspecified_scalar,

  vctrs_type2_logical_logical,
  vctrs_type2_logical_integer,
  vctrs_type2_logical_double,
  vctrs_type2_logical_complex,
  vctrs_type2_logical_character,
  vctrs_type2_logical_raw,
  vctrs_type2_logical_list,
  vctrs_type2_logical_dataframe,
  vctrs_type2_logical_s3,
  vctrs_type2_logical_scalar,

  vctrs_type2_integer_integer,
  vctrs_type2_integer_double,
  vctrs_type2_integer_complex,
  vctrs_type2_integer_character,
  vctrs_type2_integer_raw,
  vctrs_type2_integer_list,
  vctrs_type2_integer_dataframe,
  vctrs_type2_integer_s3,
  vctrs_type2_integer_scalar,

  vctrs_type2_double_double,
  vctrs_type2_double_complex,
  vctrs_type2_double_character,
  vctrs_type2_double_raw,
  vctrs_type2_double_list,
  vctrs_type2_double_dataframe,
  vctrs_type2_double_s3,
  vctrs_type2_double_scalar,

  vctrs_type2_complex_complex,
  vctrs_type2_complex_character,
  vctrs_type2_complex_raw,
  vctrs_type2_complex_list,
  vctrs_type2_complex_dataframe,
  vctrs_type2_complex_s3,
  vctrs_type2_complex_scalar,

  vctrs_type2_character_character,
  vctrs_type2_character_raw,
  vctrs_type2_character_list,
  vctrs_type2_character_dataframe,
  vctrs_type2_character_s3,
  vctrs_type2_character_scalar,

  vctrs_type2_raw_raw,
  vctrs_type2_raw_list,
  vctrs_type2_raw_dataframe,
  vctrs_type2_raw_s3,
  vctrs_type2_raw_scalar,

  vctrs_type2_list_list,
  vctrs_type2_list_dataframe,
  vctrs_type2_list_s3,
  vctrs_type2_list_scalar,

  vctrs_type2_dataframe_dataframe,
  vctrs_type2_dataframe_s3,
  vctrs_type2_dataframe_scalar,

  vctrs_type2_s3_s3,
  vctrs_type2_s3_scalar,

  vctrs_type2_scalar_scalar
};

enum vctrs_type2_s3 {
  vctrs_type2_s3_null_bare_factor,
  vctrs_type2_s3_null_bare_ordered,
  vctrs_type2_s3_null_bare_date,
  vctrs_type2_s3_null_bare_posixct,
  vctrs_type2_s3_null_bare_posixlt,
  vctrs_type2_s3_null_bare_tibble,
  vctrs_type2_s3_null_unknown,

  vctrs_type2_s3_unspecified_bare_factor,
  vctrs_type2_s3_unspecified_bare_ordered,
  vctrs_type2_s3_unspecified_bare_date,
  vctrs_type2_s3_unspecified_bare_posixct,
  vctrs_type2_s3_unspecified_bare_posixlt,
  vctrs_type2_s3_unspecified_bare_tibble,
  vctrs_type2_s3_unspecified_unknown,

  vctrs_type2_s3_logical_bare_factor,
  vctrs_type2_s3_logical_bare_ordered,
  vctrs_type2_s3_logical_bare_date,
  vctrs_type2_s3_logical_bare_posixct,
  vctrs_type2_s3_logical_bare_posixlt,
  vctrs_type2_s3_logical_bare_tibble,
  vctrs_type2_s3_logical_unknown,

  vctrs_type2_s3_integer_bare_factor,
  vctrs_type2_s3_integer_bare_ordered,
  vctrs_type2_s3_integer_bare_date,
  vctrs_type2_s3_integer_bare_posixct,
  vctrs_type2_s3_integer_bare_posixlt,
  vctrs_type2_s3_integer_bare_tibble,
  vctrs_type2_s3_integer_unknown,

  vctrs_type2_s3_double_bare_factor,
  vctrs_type2_s3_double_bare_ordered,
  vctrs_type2_s3_double_bare_date,
  vctrs_type2_s3_double_bare_posixct,
  vctrs_type2_s3_double_bare_posixlt,
  vctrs_type2_s3_double_bare_tibble,
  vctrs_type2_s3_double_unknown,

  vctrs_type2_s3_complex_bare_factor,
  vctrs_type2_s3_complex_bare_ordered,
  vctrs_type2_s3_complex_bare_date,
  vctrs_type2_s3_complex_bare_posixct,
  vctrs_type2_s3_complex_bare_posixlt,
  vctrs_type2_s3_complex_bare_tibble,
  vctrs_type2_s3_complex_unknown,

  vctrs_type2_s3_character_bare_factor,
  vctrs_type2_s3_character_bare_ordered,
  vctrs_type2_s3_character_bare_date,
  vctrs_type2_s3_character_bare_posixct,
  vctrs_type2_s3_character_bare_posixlt,
  vctrs_type2_s3_character_bare_tibble,
  vctrs_type2_s3_character_unknown,

  vctrs_type2_s3_raw_bare_factor,
  vctrs_type2_s3_raw_bare_ordered,
  vctrs_type2_s3_raw_bare_date,
  vctrs_type2_s3_raw_bare_posixct,
  vctrs_type2_s3_raw_bare_posixlt,
  vctrs_type2_s3_raw_bare_tibble,
  vctrs_type2_s3_raw_unknown,

  vctrs_type2_s3_list_bare_factor,
  vctrs_type2_s3_list_bare_ordered,
  vctrs_type2_s3_list_bare_date,
  vctrs_type2_s3_list_bare_posixct,
  vctrs_type2_s3_list_bare_posixlt,
  vctrs_type2_s3_list_bare_tibble,
  vctrs_type2_s3_list_unknown,

  vctrs_type2_s3_dataframe_bare_factor,
  vctrs_type2_s3_dataframe_bare_ordered,
  vctrs_type2_s3_dataframe_bare_date,
  vctrs_type2_s3_dataframe_bare_posixct,
  vctrs_type2_s3_dataframe_bare_posixlt,
  vctrs_type2_s3_dataframe_bare_tibble,
  vctrs_type2_s3_dataframe_unknown,

  vctrs_type2_s3_scalar_bare_factor,
  vctrs_type2_s3_scalar_bare_ordered,
  vctrs_type2_s3_scalar_bare_date,
  vctrs_type2_s3_scalar_bare_posixct,
  vctrs_type2_s3_scalar_bare_posixlt,
  vctrs_type2_s3_scalar_bare_tibble,
  vctrs_type2_s3_scalar_unknown,

  vctrs_type2_s3_bare_factor_bare_factor,
  vctrs_type2_s3_bare_factor_bare_ordered,
  vctrs_type2_s3_bare_factor_bare_date,
  vctrs_type2_s3_bare_factor_bare_posixct,
  vctrs_type2_s3_bare_factor_bare_posixlt,
  vctrs_type2_s3_bare_factor_bare_tibble,
  vctrs_type2_s3_bare_factor_unknown,

  vctrs_type2_s3_bare_ordered_bare_ordered,
  vctrs_type2_s3_bare_ordered_bare_date,
  vctrs_type2_s3_bare_ordered_bare_posixct,
  vctrs_type2_s3_bare_ordered_bare_posixlt,
  vctrs_type2_s3_bare_ordered_bare_tibble,
  vctrs_type2_s3_bare_ordered_unknown,

  vctrs_type2_s3_bare_date_bare_date,
  vctrs_type2_s3_bare_date_bare_posixct,
  vctrs_type2_s3_bare_date_bare_posixlt,
  vctrs_type2_s3_bare_date_bare_tibble,
  vctrs_type2_s3_bare_date_unknown,

  vctrs_type2_s3_bare_posixct_bare_posixct,
  vctrs_type2_s3_bare_posixct_bare_posixlt,
  vctrs_type2_s3_bare_posixct_bare_tibble,
  vctrs_type2_s3_bare_posixct_unknown,

  vctrs_type2_s3_bare_posixlt_bare_posixlt,
  vctrs_type2_s3_bare_posixlt_bare_tibble,
  vctrs_type2_s3_bare_posixlt_unknown,

  vctrs_type2_s3_bare_tibble_bare_tibble,
  vctrs_type2_s3_bare_tibble_unknown,

  vctrs_type2_s3_unknown_unknown
};

enum vctrs_type2 vec_typeof2(SEXP x, SEXP y);
const char* vctrs_type2_as_str(enum vctrs_type2 type);

extern SEXP vctrs_shared_empty_lgl;
extern SEXP vctrs_shared_empty_int;
extern SEXP vctrs_shared_empty_dbl;
extern SEXP vctrs_shared_empty_cpl;
extern SEXP vctrs_shared_empty_chr;
extern SEXP vctrs_shared_empty_raw;
extern SEXP vctrs_shared_empty_list;
extern SEXP vctrs_shared_empty_date;
extern SEXP vctrs_shared_empty_uns;

extern SEXP vctrs_shared_true;
extern SEXP vctrs_shared_false;

extern Rcomplex vctrs_shared_na_cpl;
extern SEXP vctrs_shared_na_lgl;
extern SEXP vctrs_shared_na_list;

SEXP vec_unspecified(R_len_t n);
bool vec_is_unspecified(SEXP x);


#include "type-info.h"

#include "arg-counter.h"
#include "arg.h"
#include "assert.h"
#include "c.h"
#include "cast-bare.h"
#include "cast-dispatch.h"
#include "cast.h"
#include "compare.h"
#include "complete.h"
#include "dictionary.h"
#include "dim.h"
#include "hash.h"
#include "lazy.h"
#include "match-compare.h"
#include "match-joint.h"
#include "names.h"
#include "order-collate.h"
#include "order-groups.h"
#include "order-sortedness.h"
#include "order-truelength.h"
#include "order.h"
#include "owned.h"
#include "poly-op.h"
#include "ptype-common.h"
#include "ptype.h"
#include "ptype2-dispatch.h"
#include "ptype2.h"
#include "shape.h"
#include "size-common.h"
#include "size.h"
#include "slice-assign.h"
#include "slice.h"
#include "strides.h"
#include "subscript-loc.h"
#include "subscript.h"
#include "translate.h"
#include "utils.h"


// Vector methods ------------------------------------------------

enum vctrs_proxy_kind {
  VCTRS_PROXY_KIND_default,
  VCTRS_PROXY_KIND_equal,
  VCTRS_PROXY_KIND_compare,
  VCTRS_PROXY_KIND_order
};

SEXP vec_proxy(SEXP x);
SEXP vec_proxy_equal(SEXP x);
SEXP vec_proxy_compare(SEXP x);
SEXP vec_proxy_order(SEXP x);
r_obj* vec_joint_proxy_order(r_obj* x, r_obj* y);
SEXP vec_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned);
SEXP vec_restore_default(SEXP x, SEXP to, const enum vctrs_owned owned);
SEXP vec_chop(SEXP x, SEXP indices);
SEXP vec_slice_shaped(enum vctrs_type type, SEXP x, SEXP index);
SEXP vec_proxy_assign(SEXP proxy, SEXP index, SEXP value);
bool vec_requires_fallback(SEXP x, struct vctrs_proxy_info info);
r_obj* vec_ptype(r_obj* x, struct vctrs_arg* x_arg, struct r_lazy call);
SEXP vec_ptype_finalise(SEXP x);
bool vec_is_unspecified(SEXP x);
SEXP vec_names(SEXP x);
SEXP vec_proxy_names(SEXP x);
SEXP vec_group_loc(SEXP x);
SEXP vec_identify_runs(SEXP x);
SEXP vec_match_params(SEXP needles, SEXP haystack, bool na_equal,
                      struct vctrs_arg* needles_arg, struct vctrs_arg* haystack_arg);

// FIXME: Pass error call everywhere
static inline
r_obj* vec_recycle(r_obj* x,
                   r_ssize size,
                   struct vctrs_arg* x_arg) {
  return vec_recycle2(x, size, x_arg, r_lazy_null);
}

#include "cast.h"
static inline r_obj* vec_cast(r_obj* x,
                              r_obj* to,
                              struct vctrs_arg* x_arg,
                              struct vctrs_arg* to_arg,
                              struct r_lazy call) {
  struct cast_opts opts = {
    .x = x,
    .to = to,
    .x_arg = x_arg,
    .to_arg = to_arg,
    .call = call
  };
  return vec_cast_opts(&opts);
}

static inline SEXP vec_match(SEXP needles, SEXP haystack) {
  return vec_match_params(needles, haystack, true, NULL, NULL);
}


SEXP vec_c(SEXP xs,
           SEXP ptype,
           SEXP name_spec,
           const struct name_repair_opts* name_repair);

bool is_data_frame(SEXP x);

SEXP vec_bare_df_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned);
SEXP vec_df_restore(SEXP x, SEXP to, SEXP n, const enum vctrs_owned owned);

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
                      struct vctrs_arg* arg);
__attribute__((noreturn))
void stop_incompatible_size(SEXP x, SEXP y,
                            R_len_t x_size, R_len_t y_size,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg);
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

// Compatibility ------------------------------------------------

#if (R_VERSION < R_Version(3, 5, 0))
# define LOGICAL_RO(x) ((const int*) LOGICAL(x))
# define INTEGER_RO(x) ((const int*) INTEGER(x))
# define REAL_RO(x) ((const double*) REAL(x))
# define COMPLEX_RO(x) ((const Rcomplex*) COMPLEX(x))
# define STRING_PTR_RO(x) ((const SEXP*) STRING_PTR(x))
# define RAW_RO(x) ((const Rbyte*) RAW(x))
# define DATAPTR_RO(x) ((const void*) STRING_PTR(x))
#endif

#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))


#endif
