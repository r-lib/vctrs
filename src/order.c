#include "vctrs.h"
#include "utils.h"
#include "order-groups.h"

// -----------------------------------------------------------------------------

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

// This is the maximum range size that determines whether a counting sort
// is used on the input or not. When `x` has a range less than this boundary,
// a counting sort is often faster than a radix sort.
#define INT_COUNTING_ORDER_RANGE_BOUNDARY 100000

// A bit ad hoc - but seems to work well. Going up to 256 definitely has
// negative performance implications for completely random input.
// Base R seems to use 200, but I think this works a little better (from
// limited testing).
// Somewhat based on this post, which also uses 128.
// https://probablydance.com/2016/12/27/i-wrote-a-faster-sorting-algorithm/
#define INSERTION_ORDER_BOUNDARY 128

// -----------------------------------------------------------------------------

static struct group_info new_group_info();
static void group_realloc(struct group_info* p_group_info, R_xlen_t size);

static struct group_infos new_group_infos(struct group_info** p_p_group_info,
                                          bool requested,
                                          bool ignore);
static inline struct group_info* groups_current(struct group_infos* p_group_infos);
static void groups_swap(struct group_infos* p_group_infos);
static void groups_size_push(struct group_infos* p_group_infos, R_xlen_t size);

// -----------------------------------------------------------------------------

static SEXP vec_order(SEXP x, SEXP decreasing, bool na_last, bool groups);

// [[ register() ]]
SEXP vctrs_order(SEXP x, SEXP decreasing, SEXP na_last, SEXP groups) {
  if (!r_is_bool(na_last)) {
    Rf_errorcall(R_NilValue, "`na_last` must be either `TRUE` or `FALSE`.");
  }

  bool c_na_last = LOGICAL(na_last)[0];
  bool c_groups = LOGICAL(groups)[0];

  return vec_order(x, decreasing, c_na_last, c_groups);
}

// -----------------------------------------------------------------------------

static inline size_t vec_order_size_multiplier(SEXP x);
static bool vec_order_any_character(SEXP x, const enum vctrs_type type);

static void vec_order_switch(SEXP x,
                             void* p_x_slice,
                             void* p_x_aux,
                             int* p_x_chr_sizes,
                             int* p_x_chr_sizes_aux,
                             int* p_o,
                             int* p_o_aux,
                             uint8_t* p_bytes,
                             struct group_infos* p_group_infos,
                             SEXP decreasing,
                             bool na_last,
                             R_xlen_t size,
                             const enum vctrs_type type);

/*
 * Compute the order of a vector.
 * Can optionally compute the group sizes as well.
 *
 * TODO: Return group info as attributes for type stability? Might not be
 * important since we have different R level functions.
 */
static SEXP vec_order(SEXP x, SEXP decreasing, bool na_last, bool groups) {
  int n_prot = 0;
  int* p_n_prot = &n_prot;

  // TODO:
  // x = PROTECT(vec_proxy_compare(x));

  // TODO:
  // Should proxy-compare flatten df-cols?
  // How to track vector of `decreasing` if so?

  R_xlen_t size = vec_size(x);
  const enum vctrs_type type = vec_proxy_typeof(x);

  // Don't check length here. This might be vectorized if `x` is a data frame.
  if (TYPEOF(decreasing) != LGLSXP) {
    Rf_errorcall(R_NilValue, "`decreasing` must be logical");
  }
  if (lgl_any_na(decreasing)) {
    Rf_errorcall(R_NilValue, "`decreasing` must not contain missing values.");
  }

  const size_t multiplier = vec_order_size_multiplier(x);

  // Auxiliary vectors to hold intermediate results the same size as `x`.
  // If `x` is a data frame we allocate enough room for the largest column type.
  SEXP x_slice = PROTECT_N(Rf_allocVector(RAWSXP, size * multiplier), p_n_prot);
  void* p_x_slice = DATAPTR(x_slice);

  SEXP x_aux = PROTECT_N(Rf_allocVector(RAWSXP, size * multiplier), p_n_prot);
  void* p_x_aux = DATAPTR(x_aux);

  // Character ordering is much faster if we keep track of individual string
  // sizes, even if it is a bit of a headache
  bool any_character = vec_order_any_character(x, type);

  SEXP x_chr_sizes = vctrs_shared_empty_int;
  if (any_character) {
    x_chr_sizes = Rf_allocVector(INTSXP, size);
  }
  PROTECT_N(x_chr_sizes, p_n_prot);
  int* p_x_chr_sizes = INTEGER(x_chr_sizes);

  SEXP x_chr_sizes_aux = vctrs_shared_empty_int;
  if (any_character) {
    x_chr_sizes_aux = Rf_allocVector(INTSXP, size);
  }
  PROTECT_N(x_chr_sizes_aux, p_n_prot);
  int* p_x_chr_sizes_aux = INTEGER(x_chr_sizes_aux);

  SEXP o = PROTECT_N(Rf_allocVector(INTSXP, size), p_n_prot);
  int* p_o = INTEGER(o);

  SEXP o_aux = PROTECT_N(Rf_allocVector(INTSXP, size), p_n_prot);
  int* p_o_aux = INTEGER(o_aux);

  // Initialize `out` with sequential 1-based ordering
  for (R_xlen_t i = 0; i < size; ++i) {
    p_o[i] = i + 1;
  }

  uint8_t* p_bytes = (uint8_t*) R_alloc(size, sizeof(uint8_t));

  // Should group info be ignored?
  // Can only ignore if dealing with non-data-frame input and groups have not
  // been requested, but this is more efficient for atomic vectors.
  bool requested = groups;
  bool ignore = requested ? false : (is_data_frame(x) ? false : true);

  struct group_info group_info0 = new_group_info();
  struct group_info* p_group_info0 = &group_info0;
  PROTECT_GROUP_INFO(p_group_info0, p_n_prot);

  struct group_info group_info1 = new_group_info();
  struct group_info* p_group_info1 = &group_info1;
  PROTECT_GROUP_INFO(p_group_info1, p_n_prot);

  struct group_info* p_p_group_info[2];
  p_p_group_info[0] = p_group_info0;
  p_p_group_info[1] = p_group_info1;

  struct group_infos group_infos = new_group_infos(p_p_group_info, requested, ignore);
  struct group_infos* p_group_infos = &group_infos;

  vec_order_switch(
    x,
    p_x_slice,
    p_x_aux,
    p_x_chr_sizes,
    p_x_chr_sizes_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    decreasing,
    na_last,
    size,
    type
  );

  // Return all group info rather than just ordering
  if (groups) {
    struct group_info* p_group_info = groups_current(p_group_infos);

    R_xlen_t n_groups = p_group_info->n_groups;
    p_group_info->data = PROTECT_N(Rf_xlengthgets(p_group_info->data, n_groups), p_n_prot);

    SEXP out = PROTECT_N(Rf_allocVector(VECSXP, 4), p_n_prot);

    SET_VECTOR_ELT(out, 0, o);
    SET_VECTOR_ELT(out, 1, p_group_info->data);
    SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(p_group_info->n_groups));
    SET_VECTOR_ELT(out, 3, Rf_ScalarInteger(p_group_info->max_group_size));

    UNPROTECT(n_prot);
    return out;
  }

  UNPROTECT(n_prot);
  return o;
}

// -----------------------------------------------------------------------------

static void df_order(SEXP x,
                     void* p_x_slice,
                     void* p_x_aux,
                     int* p_x_chr_sizes,
                     int* p_x_chr_sizes_aux,
                     int* p_o,
                     int* p_o_aux,
                     uint8_t* p_bytes,
                     struct group_infos* p_group_infos,
                     SEXP decreasing,
                     bool na_last,
                     R_xlen_t size);

static void vec_order_immutable_switch(SEXP x,
                                       void* p_x_slice,
                                       void* p_x_aux,
                                       int* p_x_chr_sizes,
                                       int* p_x_chr_sizes_aux,
                                       int* p_o,
                                       int* p_o_aux,
                                       uint8_t* p_bytes,
                                       struct group_infos* p_group_infos,
                                       bool decreasing,
                                       bool na_last,
                                       R_xlen_t size,
                                       const enum vctrs_type type);

static void vec_order_switch(SEXP x,
                             void* p_x_slice,
                             void* p_x_aux,
                             int* p_x_chr_sizes,
                             int* p_x_chr_sizes_aux,
                             int* p_o,
                             int* p_o_aux,
                             uint8_t* p_bytes,
                             struct group_infos* p_group_infos,
                             SEXP decreasing,
                             bool na_last,
                             R_xlen_t size,
                             const enum vctrs_type type) {
  if (type == vctrs_type_dataframe) {
    df_order(
      x,
      p_x_slice,
      p_x_aux,
      p_x_chr_sizes,
      p_x_chr_sizes_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    return;
  }

  // We know it is logical with no missing values, but size hasn't been checked
  if (Rf_xlength(decreasing) != 1) {
    Rf_errorcall(
      R_NilValue,
      "`decreasing` must have length 1 when `x` is not a data frame."
    );
  }

  bool c_decreasing = LOGICAL(decreasing)[0];

  vec_order_immutable_switch(
    x,
    p_x_slice,
    p_x_aux,
    p_x_chr_sizes,
    p_x_chr_sizes_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    c_decreasing,
    na_last,
    size,
    type
  );
}

// -----------------------------------------------------------------------------

static void int_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size);

static void lgl_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size);

static void dbl_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size);

static void cpl_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size);

static void chr_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_x_chr_sizes,
                                int* p_x_chr_sizes_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size);

// Used on bare vectors and the first column of data frame `x`s
static void vec_order_immutable_switch(SEXP x,
                                       void* p_x_slice,
                                       void* p_x_aux,
                                       int* p_x_chr_sizes,
                                       int* p_x_chr_sizes_aux,
                                       int* p_o,
                                       int* p_o_aux,
                                       uint8_t* p_bytes,
                                       struct group_infos* p_group_infos,
                                       bool decreasing,
                                       bool na_last,
                                       R_xlen_t size,
                                       const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer: {
    int_order_immutable(
      x,
      p_x_slice,
      p_x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_logical: {
    lgl_order_immutable(
      x,
      p_x_slice,
      p_x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_double: {
    dbl_order_immutable(
      x,
      p_x_slice,
      p_x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_complex: {
    cpl_order_immutable(
      x,
      p_x_slice,
      p_x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_character: {
    chr_order_immutable(
      x,
      p_x_slice,
      p_x_aux,
      p_x_chr_sizes,
      p_x_chr_sizes_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_dataframe: {
    Rf_errorcall(R_NilValue, "Internal error: Data frames should have been handled by now");
  }
  default: {
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_radix_order()`.");
  }
  }
}

// -----------------------------------------------------------------------------

static void int_adjust(void* p_x,
                       const bool decreasing,
                       const bool na_last,
                       const R_xlen_t size);

static void int_compute_range(const int* p_x,
                              R_xlen_t size,
                              int* p_x_min,
                              uint32_t* p_range);

static void int_counting_order(const int* p_x,
                               int* p_o,
                               int* p_o_aux,
                               struct group_infos* p_group_infos,
                               R_xlen_t size,
                               int x_min,
                               uint32_t range,
                               bool decreasing,
                               bool na_last);

static void int_insertion_order(uint32_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos,
                                const R_xlen_t size);

static void int_radix_order(uint32_t* p_x,
                            uint32_t* p_x_aux,
                            int* p_o,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            struct group_infos* p_group_infos,
                            const R_xlen_t size);

/*
 * These are the main entry points for integer ordering. They are nearly
 * identical except `int_order_immutable()` assumes that `p_x` cannot be
 * modified directly and is user input.
 *
 * `int_order()` assumes `p_x` is modifiable by reference. It is called when
 * iterating over data frame columns and `p_x` is the 2nd or greater column,
 * in which case `p_x` is really a chunk of that column that has been copied
 * into `x_slice`.
 *
 * `int_order_immutable()` assumes `p_x` is user input which cannot be modified.
 * It copies `x` into another SEXP that can be modified directly unless a
 * counting sort is going to be used, in which case `p_x` can be used directly.
 */
static void int_order(void* p_x,
                      void* p_x_aux,
                      int* p_o,
                      int* p_o_aux,
                      uint8_t* p_bytes,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  if (size <= INSERTION_ORDER_BOUNDARY) {
    int_adjust(p_x, decreasing, na_last, size);
    int_insertion_order(p_x, p_o, p_group_infos, size);
    return;
  }

  uint32_t range;
  int x_min;

  int_compute_range(p_x, size, &x_min, &range);

  if (range < INT_COUNTING_ORDER_RANGE_BOUNDARY) {
    int_counting_order(p_x, p_o, p_o_aux, p_group_infos, size, x_min, range, decreasing, na_last);
    return;
  }

  int_adjust(p_x, decreasing, na_last, size);
  int_radix_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_group_infos, size);
}

static void int_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size) {
  const int* p_x = INTEGER_RO(x);

  if (size <= INSERTION_ORDER_BOUNDARY) {
    memcpy(p_x_slice, p_x, size * sizeof(int));
    int_adjust(p_x_slice, decreasing, na_last, size);
    int_insertion_order(p_x_slice, p_o, p_group_infos, size);
    return;
  }

  uint32_t range;
  int x_min;

  int_compute_range(p_x, size, &x_min, &range);

  // If in counting sort range, no need to copy over to `x_slice`
  if (range < INT_COUNTING_ORDER_RANGE_BOUNDARY) {
    int_counting_order(p_x, p_o, p_o_aux, p_group_infos, size, x_min, range, decreasing, na_last);
    return;
  }

  memcpy(p_x_slice, p_x, size * sizeof(int));
  int_adjust(p_x_slice, decreasing, na_last, size);
  int_radix_order(p_x_slice, p_x_aux, p_o, p_o_aux, p_bytes, p_group_infos, size);
}

// -----------------------------------------------------------------------------

static inline uint32_t int_map_to_uint32(int x);

/*
 * - Shifts the integer elements of `p_x` in a way that correctly maintains
 *   ordering for `na_last` and `decreasing`
 *
 * - After shifting, also maps the elements from `int32_t` to `uint32_t` and
 *   stores them back in `p_x`.
 *
 * - Used before both the integer insertion sort and radix sort, which both
 *   expect their input to already have been "adjusted" for `na_last` and
 *   `decreasing` and expect a `uint32_t` pointer input.
 *
 * - If `na_last = true`, `NA` is always the maximum element, so we set it to
 *   `UINT32_MAX`. In that case, we also shift all non-NA values down by 1 to
 *   make room for it (defined by `na_shift`).
 *
 * - If `na_last = false`, we set `NA` to the minimum element of 0.
 *
 * - The multiplication by `direction` applies to non-NA values and correctly
 *   orders inputs based on whether we are in a decreasing order or not.
 */
static void int_adjust(void* p_x,
                       const bool decreasing,
                       const bool na_last,
                       const R_xlen_t size) {
  const int direction = decreasing ? -1 : 1;
  const uint32_t na_u32 = na_last ? UINT32_MAX : 0;
  const int na_shift = na_last ? -1 : 0;

  const int* p_x_int = (const int*) p_x;
  uint32_t* p_x_u32 = (uint32_t*) p_x;

  for (R_xlen_t i = 0; i < size; ++i) {
    int elt = p_x_int[i];

    if (elt == NA_INTEGER) {
      p_x_u32[i] = na_u32;
      continue;
    }

    elt = elt * direction + na_shift;

    p_x_u32[i] = int_map_to_uint32(elt);
  }
}


#define HEX_UINT32_SIGN_BIT 0x80000000u

// Flipping the sign bit is all we need to do to map in an order preserving way.
// [INT32_MIN, INT32_MAX] => [0, UINT32_MAX]
static inline uint32_t int_map_to_uint32(int x) {
  return x ^ HEX_UINT32_SIGN_BIT;
}

#undef HEX_UINT32_SIGN_BIT

// -----------------------------------------------------------------------------

/*
 * `int_compute_range()` computes the range of all values in `p_x`.
 * It is used by counting sort to computes buckets with `p_x[i] - x_min`.
 *
 * - `p_range` and `p_x_min` are updated on the way out to retain both the
 *   range and the minimum value.
 *
 * - `NA` values are skipped over. If all values are `NA`, we defer to radix
 *   sort which definitely can handle that case by returning a `range` of the
 *   maximum uint32 value (which will be greater than
 *   INT_COUNTING_ORDER_RANGE_BOUNDARY).
 */
// Computes the inclusive range
// i.e. the number of values between `[min, max]` including min and max.
static void int_compute_range(const int* p_x,
                              R_xlen_t size,
                              int* p_x_min,
                              uint32_t* p_range) {
  uint32_t range = UINT32_MAX;

  int x_min = NA_INTEGER;
  int x_max = NA_INTEGER;

  R_xlen_t i = 0;

  // Find first non-NA value
  while (i < size) {
    const int elt = p_x[i];

    if (elt == NA_INTEGER) {
      ++i;
      continue;
    }

    x_min = elt;
    x_max = elt;
    range = 0;

    // Bump to next `i` since we know this one's value
    ++i;

    break;
  }

  // All NAs - Return max range to signal to use radix sort
  if (x_min == NA_INTEGER) {
    *p_x_min = x_min;
    *p_range = range;
    return;
  }

  // Now that we have initial values, iterate through the rest
  // to compute the final min/max.
  for (; i < size; ++i) {
    const int elt = p_x[i];

    if (elt == NA_INTEGER) {
      continue;
    }

    if (elt > x_max) {
      x_max = elt;
      continue;
    }

    if (elt < x_min) {
      x_min = elt;
      continue;
    }
  }

  /*
   * - Max possible range is from
   *   `c(.Machine$integer.max, -.Machine$integer.max)` which is exactly the
   *   max of a `uint32_t`.
   * - We need to go up to `int64_t` to avoid intermediate overflow.
   * - `+ 1` to get an inclusive range on both ends.
   */
  range = (uint32_t) ((int64_t) x_max - (int64_t) x_min + 1);

  *p_x_min = x_min;
  *p_range = range;
}


/*
 * The counting sort expects `p_x` to be unadjusted (i.e. `int_adjust()` has
 * not been used). It handles `decreasing` and `na_last` internally.
 *
 * Counting sort is used when `p_x` has a range less than
 * `INT_COUNTING_ORDER_RANGE_BOUNDARY`. In these cases radix sort
 * doesn't spread out values as much when looking at individual radixes.
 *
 * Counting sort does not modify `p_x` in any way.
 */
static void int_counting_order(const int* p_x,
                               int* p_o,
                               int* p_o_aux,
                               struct group_infos* p_group_infos,
                               R_xlen_t size,
                               int x_min,
                               uint32_t range,
                               bool decreasing,
                               bool na_last) {
  // - Only allocate this once (counts are reset to 0 at end)
  // - Allocating as static allows us to allocate an array this large
  // - `+ 1` to ensure there is room for the extra `NA` bucket
  static R_xlen_t p_counts[INT_COUNTING_ORDER_RANGE_BOUNDARY + 1] = { 0 };

  // `NA` values get counted in the last used bucket
  uint32_t na_bucket = range;
  R_xlen_t na_count = 0;

  // Sanity check
  if (range > INT_COUNTING_ORDER_RANGE_BOUNDARY) {
    Rf_errorcall(R_NilValue, "Internal error: `range > INT_COUNTING_ORDER_RANGE_BOUNDARY`.");
  }

  // Histogram pass
  for (R_xlen_t i = 0; i < size; ++i) {
    const int elt = p_x[i];

    if (elt == NA_INTEGER) {
      ++na_count;
    } else {
      const uint32_t bucket = elt - x_min;
      ++p_counts[bucket];
    }
  }

  // Add `NA` counts once at the end
  p_counts[na_bucket] = na_count;

  R_xlen_t cumulative = 0;

  // Handle decreasing/increasing by altering the order in which
  // counts are accumulated
  const int direction = decreasing ? -1 : 1;
  R_xlen_t j = decreasing ? range - 1 : 0;

  // `na_last = false` pushes NA counts to the front
  if (!na_last && na_count != 0) {
    p_counts[na_bucket] = cumulative;
    cumulative += na_count;
    groups_size_push(p_group_infos, na_count);
  }

  // Accumulate counts, skip zeros
  for (uint32_t i = 0; i < range; ++i) {
    R_xlen_t count = p_counts[j];

    if (count == 0) {
      j += direction;
      continue;
    }

    // Insert current cumulative value, then increment
    p_counts[j] = cumulative;
    cumulative += count;

    // At this point we will handle this group completely
    groups_size_push(p_group_infos, count);

    j += direction;
  }

  // `na_last = true` pushes NA counts to the back
  if (na_last && na_count != 0) {
    p_counts[na_bucket] = cumulative;
    groups_size_push(p_group_infos, na_count);
  }

  // Place in auxiliary in the right order, then copy back
  for (R_xlen_t i = 0; i < size; ++i) {
    const int elt = p_x[i];

    uint32_t bucket;

    if (elt == NA_INTEGER) {
      bucket = na_bucket;
    } else {
      bucket = elt - x_min;
    }

    const R_xlen_t loc = p_counts[bucket]++;

    p_o_aux[loc] = p_o[i];
  }

  // Copy back over
  memcpy(p_o, p_o_aux, size * sizeof(int));

  // Reset counts for next column.
  // Only reset what we might have touched.
  // `+ 1` to reset the NA bucket too.
  memset(p_counts, 0, (range + 1) * sizeof(R_xlen_t));
}

// -----------------------------------------------------------------------------

/*
 * `int_insertion_order()` is used in two ways:
 * - It is how we "finish off" radix sorts rather than deep recursion.
 * - If we have an original `x` input that is small enough, we just immediately
 *   insertion sort it.
 *
 * For small inputs, it is much faster than deeply recursing with
 * radix ordering.
 *
 * Insertion ordering expects that `p_x` has been adjusted with `int_adjust()`
 * which takes care of `na_last` and `decreasing` and also maps `int32_t` to
 * `uint32_t` ahead of time.
 */
static void int_insertion_order(uint32_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos,
                                const R_xlen_t size) {
  // Don't think this can occur, but safer this way
  if (size == 0) {
    return;
  }

  for (R_xlen_t i = 1; i < size; ++i) {
    const uint32_t x_elt = p_x[i];
    const int o_elt = p_o[i];

    R_xlen_t j = i - 1;

    while (j >= 0) {
      const uint32_t x_cmp_elt = p_x[j];

      if (x_elt >= x_cmp_elt) {
        break;
      }

      int o_cmp_elt = p_o[j];

      // Swap
      p_x[j + 1] = x_cmp_elt;
      p_o[j + 1] = o_cmp_elt;

      // Next
      --j;
    }

    // Place original elements in new location
    // closer to start of the vector
    p_x[j + 1] = x_elt;
    p_o[j + 1] = o_elt;
  }

  // We've ordered a small chunk, we need to push at least one group size.
  // Depends on the post-ordered results so we have to do this
  // in a separate loop.
  R_xlen_t group_size = 1;
  uint32_t previous = p_x[0];

  for (R_xlen_t i = 1; i < size; ++i) {
    const uint32_t current = p_x[i];

    // Continue the current group run
    if (current == previous) {
      ++group_size;
      continue;
    }

    // Push current run size and reset size tracker
    groups_size_push(p_group_infos, group_size);
    group_size = 1;

    previous = current;
  }

  // Push final group run
  groups_size_push(p_group_infos, group_size);
}

// -----------------------------------------------------------------------------

static uint8_t int_compute_skips(bool* p_skips, const uint32_t* p_x, R_xlen_t size);

static void int_radix_order_recurse(uint32_t* p_x,
                                    uint32_t* p_x_aux,
                                    int* p_o,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos,
                                    const R_xlen_t size,
                                    const uint8_t pass);

static void int_radix_order(uint32_t* p_x,
                            uint32_t* p_x_aux,
                            int* p_o,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            struct group_infos* p_group_infos,
                            const R_xlen_t size) {
  bool p_skips[4];

  uint8_t pass = int_compute_skips(p_skips, p_x, size);

  // Skipped everything, 1 value
  if (pass == 4) {
    groups_size_push(p_group_infos, size);
    return;
  }

  int_radix_order_recurse(
    p_x,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_skips,
    p_group_infos,
    size,
    pass
  );
}

// -----------------------------------------------------------------------------

static void int_radix_order_pass(uint32_t* p_x,
                                 uint32_t* p_x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 struct group_infos* p_group_infos,
                                 const R_xlen_t size,
                                 const uint8_t pass);

/*
 * Recursive function for radix ordering. Orders the current byte, then iterates
 * over the sub groups and recursively calls itself on each subgroup to order
 * the next byte.
 *
 * Expects that `int_adjust()` has been called on `p_x`, which takes care
 * of `na_last` and `decreasing` and also maps `int32_t` to `uint32_t` once
 * up front so we don't have to do it for each radix pass.
 */
static void int_radix_order_recurse(uint32_t* p_x,
                                    uint32_t* p_x_aux,
                                    int* p_o,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos,
                                    const R_xlen_t size,
                                    const uint8_t pass) {
  R_xlen_t p_counts[UINT8_MAX_SIZE] = { 0 };

  int_radix_order_pass(
    p_x,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos,
    size,
    pass
  );

  uint8_t next_pass = pass + 1;
  while (next_pass < 4 && p_skips[next_pass]) {
    ++next_pass;
  }

  R_xlen_t last_cumulative_count = 0;

  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const R_xlen_t cumulative_count = p_counts[i];

    if (!cumulative_count) {
      continue;
    }

    // Diff the accumulated counts to get the radix group size
    const R_xlen_t group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
      groups_size_push(p_group_infos, 1);
      ++p_x;
      ++p_o;
      continue;
    }

    // Can get here in the case of ties, like c(1L, 1L), which have a
    // `group_size` of 2 in the last radix, but there is nothing left to
    // compare so we are done.
    if (next_pass == 4) {
      groups_size_push(p_group_infos, group_size);
      p_x += group_size;
      p_o += group_size;
      continue;
    }

    // Order next byte of this subgroup
    int_radix_order_recurse(
      p_x,
      p_x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_skips,
      p_group_infos,
      group_size,
      next_pass
    );

    p_x += group_size;
    p_o += group_size;
  }
}

// -----------------------------------------------------------------------------

static inline uint8_t int_extract_uint32_byte(uint32_t x, uint8_t shift);

/*
 * Orders based on a single byte corresponding to the current `pass`.
 */
static void int_radix_order_pass(uint32_t* p_x,
                                 uint32_t* p_x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 struct group_infos* p_group_infos,
                                 const R_xlen_t size,
                                 const uint8_t pass) {
  if (size <= INSERTION_ORDER_BOUNDARY) {
    int_insertion_order(p_x, p_o, p_group_infos, size);
    return;
  }

  // TODO: Is this where we could modify for big endian?
  const uint8_t radix = 3 - pass;
  const uint8_t shift = radix * 8;

  uint8_t byte = 0;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint32_t x_elt = p_x[i];

    byte = int_extract_uint32_byte(x_elt, shift);

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Fast check to see if all bytes were the same. If so, skip `pass`.
  if (p_counts[byte] == size) {
    return;
  }

  R_xlen_t cumulative = 0;

  // Accumulate counts, skip zeros
  for (uint16_t i = 0; i < UINT8_MAX_SIZE; ++i) {
    R_xlen_t count = p_counts[i];

    if (count == 0) {
      continue;
    }

    // Replace with `cumulative` first, then bump `cumulative`.
    // `p_counts` now represents starting locations for each radix group.
    p_counts[i] = cumulative;
    cumulative += count;
  }

  // Place into auxiliary arrays in the correct order, then copy back over
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint8_t byte = p_bytes[i];
    const R_xlen_t loc = p_counts[byte]++;
    p_o_aux[loc] = p_o[i];
    p_x_aux[loc] = p_x[i];
  }

  // Copy back over
  memcpy(p_o, p_o_aux, size * sizeof(int));
  memcpy(p_x, p_x_aux, size * sizeof(uint32_t));
}

// -----------------------------------------------------------------------------

static uint8_t int_compute_skips(bool* p_skips, const uint32_t* p_x, R_xlen_t size) {
  for (uint8_t i = 0; i < 4; ++i) {
    p_skips[i] = true;
  }

  uint8_t p_bytes[4];
  const uint32_t elt0 = p_x[0];

  // Get bytes of first element in MSD->LSD order.
  // Placed in `p_bytes` in a way that aligns with the `pass` variable
  for (uint8_t pass = 0, shift = 24; pass < 4; ++pass, shift -= 8) {
    p_bytes[pass] = int_extract_uint32_byte(elt0, shift);
  }

  // Check to see which passes are skippable
  for (R_xlen_t i = 1; i < size; ++i) {
    uint8_t n_skips = 4;
    const uint32_t elt = p_x[i];

    for (uint8_t pass = 0, shift = 24; pass < 4; ++pass, shift -= 8) {
      bool skip = p_skips[pass];

      if (skip) {
        p_skips[pass] = (p_bytes[pass] == int_extract_uint32_byte(elt, shift));
      } else {
        --n_skips;
      }
    }

    // No passes are skippable
    if (n_skips == 0) {
      break;
    }
  }

  uint8_t pass = 0;

  // Shift forward to the first pass with varying bytes
  while (pass < 4 && p_skips[pass]) {
    ++pass;
  }

  return pass;
}

// -----------------------------------------------------------------------------

// Bytes will be extracted 8 bits at a time.
// This is a MSB radix sort, so they are extracted MSB->LSB.
// TODO: This probably depends on endianness?
static inline uint8_t int_extract_uint32_byte(uint32_t x, uint8_t shift) {
  return (x >> shift) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

// We could have some optimized sort for 2 values (American flag sort?) but
// honestly I don't see this as the main use case so we just pass through to
// `int_order()` and `int_order_immutable()`.

static void lgl_order(void* p_x,
                      void* p_x_aux,
                      int* p_o,
                      int* p_o_aux,
                      uint8_t* p_bytes,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  int_order(
    p_x,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    decreasing,
    na_last,
    size
  );
}

static void lgl_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size) {
  int_order_immutable(
    x,
    p_x_slice,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    decreasing,
    na_last,
    size
  );
}

// -----------------------------------------------------------------------------

static void dbl_adjust(void* p_x,
                       const bool decreasing,
                       const bool na_last,
                       const R_xlen_t size);

static void dbl_insertion_order(uint64_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos,
                                const R_xlen_t size);

static void dbl_radix_order(uint64_t* p_x,
                            uint64_t* p_x_aux,
                            int* p_o,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            struct group_infos* p_group_infos,
                            const R_xlen_t size);

/*
 * These are the main entry points for integer ordering. They are nearly
 * identical except `dbl_order_immutable()` assumes that `p_x` cannot be
 * modified directly and is user input.
 *
 * `dbl_order()` assumes `p_x` is modifiable by reference. It is called when
 * iterating over data frame columns and `p_x` is the 2nd or greater column,
 * in which case `p_x` is really a chunk of that column that has been copied
 * into `x_slice`.
 *
 * `dbl_order_immutable()` assumes `p_x` is user input which cannot be modified.
 * It copies `x` into another SEXP that can be modified directly unless a
 * counting sort is going to be used, in which case `p_x` can be used directly.
 *
 * Unlike `int_order()`, there is no intermediate counting sort, as it is
 * sort of unclear how to compute the range of a double vector in the same
 * way.
 */
static void dbl_order(void* p_x,
                      void* p_x_aux,
                      int* p_o,
                      int* p_o_aux,
                      uint8_t* p_bytes,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  if (size <= INSERTION_ORDER_BOUNDARY) {
    dbl_adjust(p_x, decreasing, na_last, size);
    dbl_insertion_order(p_x, p_o, p_group_infos, size);
    return;
  }

  dbl_adjust(p_x, decreasing, na_last, size);
  dbl_radix_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_group_infos, size);
}

static void dbl_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size) {
  const double* p_x = REAL_RO(x);

  if (size <= INSERTION_ORDER_BOUNDARY) {
    memcpy(p_x_slice, p_x, size * sizeof(double));
    dbl_adjust(p_x_slice, decreasing, na_last, size);
    dbl_insertion_order(p_x_slice, p_o, p_group_infos, size);
    return;
  }

  memcpy(p_x_slice, p_x, size * sizeof(double));
  dbl_adjust(p_x_slice, decreasing, na_last, size);
  dbl_radix_order(p_x_slice, p_x_aux, p_o, p_o_aux, p_bytes, p_group_infos, size);
}

// -----------------------------------------------------------------------------

static inline uint64_t dbl_map_to_uint64(double x);

/*
 * When mapping double -> uint64_t:
 *
 * Smallest possible value comes from:
 * dbl_map_to_uint64(-Inf) -> 4503599627370495
 *
 * One larger is:
 * dbl_map_to_uint64(-.Machine$double.xmax) -> 4503599627370496
 *
 * Largest possible value comes from:
 * dbl_map_to_uint64(Inf) -> 18442240474082181120
 *
 * One smaller is:
 * dbl_map_to_uint64(.Machine$double.xmax) -> 18442240474082181119
 *
 * This gives us room to manually map (depending on `na_last`):
 * dbl_map_to_uint64(NA_real_) -> 0 (UINT64_MAX)
 * dbl_map_to_uint64(NaN) -> 0 (UINT64_MAX)
 *
 */
static void dbl_adjust(void* p_x,
                       const bool decreasing,
                       const bool na_last,
                       const R_xlen_t size) {
  const int direction = decreasing ? -1 : 1;
  const uint64_t na_u64 = na_last ? UINT64_MAX : 0;

  double* p_x_dbl = (double*) p_x;
  uint64_t* p_x_u64 = (uint64_t*) p_x;

  for (R_xlen_t i = 0; i < size; ++i) {
    // Flip direction ahead of time. Won't affect `NA_real`, `NaN` values.
    double elt = p_x_dbl[i] * direction;

    if (isnan(elt)) {
      p_x_u64[i] = na_u64;
      continue;
    }

    p_x_u64[i] = dbl_map_to_uint64(elt);
  }
}


static inline uint64_t dbl_flip_uint64(uint64_t x);

static union {
  double d;
  uint64_t u64;
} d_u64;

// - Assumes `x` is not a `NA_real_` or `NaN` value
// - Correctly handles `Inf` and `-Inf`
static inline uint64_t dbl_map_to_uint64(double x) {
  // Catch `-0` vs `0`
  if (x == 0) {
    x = 0;
  }

  // Reinterpret as uint64_t without changing bytes
  d_u64.d = x;

  d_u64.u64 = dbl_flip_uint64(d_u64.u64);

  return d_u64.u64;
}


#define HEX_UINT64_SIGN 0x8000000000000000u
#define HEX_UINT64_ONES 0xffffffffffffffffu

// To retain ordering in mapping from double -> uint64_t we always have to
// flip the sign bit, and for negative numbers we also flip all of the other
// bits.
// http://stereopsis.com/radix.html
static inline uint64_t dbl_flip_uint64(uint64_t x) {
  const uint64_t mask = (x & HEX_UINT64_SIGN) ? HEX_UINT64_ONES : HEX_UINT64_SIGN;
  return x ^ mask;
}

#undef HEX_UINT64_SIGN
#undef HEX_UINT64_ONES

// -----------------------------------------------------------------------------

/*
 * `dbl_insertion_order()` is used in two ways:
 * - It is how we "finish off" radix sorts rather than deep recursion.
 * - If we have an original `x` input that is small enough, we just immediately
 *   insertion sort it.
 *
 * For small inputs, it is much faster than deeply recursing with
 * radix ordering.
 *
 * Insertion ordering expects that `p_x` has been adjusted with `dbl_adjust()`
 * which takes care of `na_last` and `decreasing` and also maps `double` to
 * `uint64_t` ahead of time.
 *
 * It is essentially the same as `int_insertion_sort()` with different types.
 */
static void dbl_insertion_order(uint64_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos,
                                const R_xlen_t size) {
  // Don't think this can occur, but safer this way
  if (size == 0) {
    return;
  }

  for (R_xlen_t i = 1; i < size; ++i) {
    const uint64_t x_elt = p_x[i];
    const int o_elt = p_o[i];

    R_xlen_t j = i - 1;

    while (j >= 0) {
      const uint64_t x_cmp_elt = p_x[j];

      if (x_elt >= x_cmp_elt) {
        break;
      }

      int o_cmp_elt = p_o[j];

      // Swap
      p_x[j + 1] = x_cmp_elt;
      p_o[j + 1] = o_cmp_elt;

      // Next
      --j;
    }

    // Place original elements in new location
    // closer to start of the vector
    p_x[j + 1] = x_elt;
    p_o[j + 1] = o_elt;
  }

  // We've ordered a small chunk, we need to push at least one group size.
  // Depends on the post-ordered results so we have to do this
  // in a separate loop.
  R_xlen_t group_size = 1;
  uint64_t previous = p_x[0];

  for (R_xlen_t i = 1; i < size; ++i) {
    const uint64_t current = p_x[i];

    // Continue the current group run
    if (current == previous) {
      ++group_size;
      continue;
    }

    // Push current run size and reset size tracker
    groups_size_push(p_group_infos, group_size);
    group_size = 1;

    previous = current;
  }

  // Push final group run
  groups_size_push(p_group_infos, group_size);
}

// -----------------------------------------------------------------------------

static uint8_t dbl_compute_skips(bool* p_skips, const uint64_t* p_x, R_xlen_t size);

static void dbl_radix_order_recurse(uint64_t* p_x,
                                    uint64_t* p_x_aux,
                                    int* p_o,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos,
                                    const R_xlen_t size,
                                    const uint8_t pass);

static void dbl_radix_order(uint64_t* p_x,
                            uint64_t* p_x_aux,
                            int* p_o,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            struct group_infos* p_group_infos,
                            const R_xlen_t size) {
  bool p_skips[8];

  uint8_t pass = dbl_compute_skips(p_skips, p_x, size);

  // Skipped everything, 1 value
  if (pass == 8) {
    groups_size_push(p_group_infos, size);
    return;
  }

  dbl_radix_order_recurse(
    p_x,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_skips,
    p_group_infos,
    size,
    pass
  );
}

// -----------------------------------------------------------------------------

static void dbl_radix_order_pass(uint64_t* p_x,
                                 uint64_t* p_x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 struct group_infos* p_group_infos,
                                 const R_xlen_t size,
                                 const uint8_t pass);

/*
 * Recursive function for radix ordering. Orders the current byte, then iterates
 * over the sub groups and recursively calls itself on each subgroup to order
 * the next byte.
 *
 * Expects that `dbl_adjust()` has been called on `p_x`, which takes care
 * of `na_last` and `decreasing` and also maps `double` to `uint64_t` once
 * up front so we don't have to do it for each radix pass.
 *
 * This needs 8 passes, unlike the 4 required by `int_radix_order()`.
 */
static void dbl_radix_order_recurse(uint64_t* p_x,
                                    uint64_t* p_x_aux,
                                    int* p_o,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos,
                                    const R_xlen_t size,
                                    const uint8_t pass) {
  R_xlen_t p_counts[UINT8_MAX_SIZE] = { 0 };

  dbl_radix_order_pass(
    p_x,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos,
    size,
    pass
  );

  // Find the next pass with varying bytes. For small doubles like 1 or 2 it
  // is fairly common for them to have `uint64_t` representations where passes
  // 0-2 have data, but 3-7 are filled with zeros.
  uint8_t next_pass = pass + 1;
  while (next_pass < 8 && p_skips[next_pass]) {
    ++next_pass;
  }

  R_xlen_t last_cumulative_count = 0;

  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const R_xlen_t cumulative_count = p_counts[i];

    if (!cumulative_count) {
      continue;
    }

    // Diff the accumulated counts to get the radix group size
    const R_xlen_t group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
      groups_size_push(p_group_infos, 1);
      ++p_x;
      ++p_o;
      continue;
    }

    // Can get here in the case of ties, like c(1, 1), which have a
    // `group_size` of 2 in the last radix, but there is nothing left to
    // compare so we are done.
    if (next_pass == 8) {
      groups_size_push(p_group_infos, group_size);
      p_x += group_size;
      p_o += group_size;
      continue;
    }

    // Order next byte of this subgroup
    dbl_radix_order_recurse(
      p_x,
      p_x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_skips,
      p_group_infos,
      group_size,
      next_pass
    );

    p_x += group_size;
    p_o += group_size;
  }
}

// -----------------------------------------------------------------------------

static inline uint8_t dbl_extract_uint64_byte(uint64_t x, uint8_t shift);

/*
 * Orders based on a single byte corresponding to the current `pass`.
 */
static void dbl_radix_order_pass(uint64_t* p_x,
                                 uint64_t* p_x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 struct group_infos* p_group_infos,
                                 const R_xlen_t size,
                                 const uint8_t pass) {
  if (size <= INSERTION_ORDER_BOUNDARY) {
    dbl_insertion_order(p_x, p_o, p_group_infos, size);
    return;
  }

  // TODO: Is this where we could modify for big endian?
  const uint8_t radix = 7 - pass;
  const uint8_t shift = radix * 8;

  uint8_t byte = 0;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint64_t x_elt = p_x[i];

    byte = dbl_extract_uint64_byte(x_elt, shift);

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Fast check to see if all bytes were the same. If so, skip `pass`.
  if (p_counts[byte] == size) {
    return;
  }

  R_xlen_t cumulative = 0;

  // Accumulate counts, skip zeros
  for (uint16_t i = 0; i < UINT8_MAX_SIZE; ++i) {
    R_xlen_t count = p_counts[i];

    if (count == 0) {
      continue;
    }

    // Replace with `cumulative` first, then bump `cumulative`.
    // `p_counts` now represents starting locations for each radix group.
    p_counts[i] = cumulative;
    cumulative += count;
  }

  // Place into auxiliary arrays in the correct order, then copy back over
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint8_t byte = p_bytes[i];
    const R_xlen_t loc = p_counts[byte]++;
    p_o_aux[loc] = p_o[i];
    p_x_aux[loc] = p_x[i];
  }

  // Copy back over
  memcpy(p_o, p_o_aux, size * sizeof(int));
  memcpy(p_x, p_x_aux, size * sizeof(uint64_t));
}

// -----------------------------------------------------------------------------

/*
 * Detect completely skippable bytes
 *
 * There are 8 passes over a double, 1 for each byte. Often times for the entire
 * `x` vector a few of those passes are useless because all of the bytes are
 * the same. This does an up front computation in 1 pass over the data to
 * determine which bytes are completely skippable.
 *
 * It is worth noting that just because byte 0 wasn't skippable doesn't mean
 * that byte 1 isn't. With the way that doubles are mapped to uint64_t, it
 * is often the case that, for small doubles, bytes 0-2 aren't skippable but
 * the rest of them are (for example, this happens with doubles in the range
 * of 1:128). This provides a nice performance increase there.
 */
static uint8_t dbl_compute_skips(bool* p_skips, const uint64_t* p_x, R_xlen_t size) {
  for (uint8_t i = 0; i < 8; ++i) {
    p_skips[i] = true;
  }

  uint8_t p_bytes[8];
  const uint64_t elt0 = p_x[0];

  // Get bytes of first element in MSD->LSD order.
  // Placed in `p_bytes` in a way that aligns with the `pass` variable
  for (uint8_t pass = 0, shift = 56; pass < 8; ++pass, shift -= 8) {
    p_bytes[pass] = dbl_extract_uint64_byte(elt0, shift);
  }

  // Check to see which passes are skippable
  for (R_xlen_t i = 1; i < size; ++i) {
    uint8_t n_skips = 8;
    const uint64_t elt = p_x[i];

    for (uint8_t pass = 0, shift = 56; pass < 8; ++pass, shift -= 8) {
      bool skip = p_skips[pass];

      if (skip) {
        p_skips[pass] = (p_bytes[pass] == dbl_extract_uint64_byte(elt, shift));
      } else {
        --n_skips;
      }
    }

    // No passes are skippable
    if (n_skips == 0) {
      break;
    }
  }

  uint8_t pass = 0;

  // Shift forward to the first pass with varying bytes
  while (pass < 8 && p_skips[pass]) {
    ++pass;
  }

  return pass;
}

// -----------------------------------------------------------------------------

// Bytes will be extracted 8 bits at a time.
// This is a MSB radix sort, so they are extracted MSB->LSB.
// TODO: This probably depends on endianness?
static inline uint8_t dbl_extract_uint64_byte(uint64_t x, uint8_t shift) {
  return (x >> shift) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

/*
 * `cpl_order_immutable()` uses the fact that Rcomplex is really just a rcrd
 * type of two double vectors. It orders first on the real vector, and then on
 * the imaginary vector.
 *
 * `cpl_order()` isn't required. It would only be called from data frames
 * when there is a complex column, but in those cases we split the column
 * into two double vectors (real / imaginary) and "rerun" the column using
 * `dbl_order()`.
 */
static void cpl_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size) {
  // We treat complex as a two column data frame, so we have to use group
  // information for at least the first column.
  // - If a complex atomic vector is used, `ignore` will be true unless the
  //   user also requested group information.
  // - If the first column of a df is a complex column, `ignore` will be false.
  bool reset_ignore = false;
  if (p_group_infos->ignore) {
    p_group_infos->ignore = false;
    reset_ignore = true;
  }

  const Rcomplex* p_x_cpl = COMPLEX_RO(x);

  // When a complex column is present,
  // `x_slice` and `x_aux` are allocated to have the
  // size of a double vector.
  double* p_x_slice_dbl = (double*) p_x_slice;

  // Handle the real portion first
  for (R_xlen_t i = 0; i < size; ++i) {
    p_x_slice_dbl[i] = p_x_cpl[i].r;
  }

  // Run it through double ordering
  dbl_order(
    p_x_slice_dbl,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    decreasing,
    na_last,
    size
  );

  // Reset `ignore` for the second pass if we don't need to track groups.
  // This happens if an atomic complex vector is passed in and the user
  // hasn't requested group information.
  if (reset_ignore) {
    p_group_infos->ignore = true;
  }

  // Get the number of group chunks from the first pass
  struct group_info* p_group_info_pre = groups_current(p_group_infos);
  R_xlen_t n_groups = p_group_info_pre->n_groups;

  // If there were no ties, we are completely done
  if (n_groups == size) {
    return;
  }

  // Swap to other group info to prepare for the imaginary section
  groups_swap(p_group_infos);

  // Fill with the imaginary portion.
  // Uses updated ordering to get it in sequential order.
  for (R_xlen_t i = 0; i < size; ++i) {
    const int loc = p_o[i] - 1;
    p_x_slice_dbl[i] = p_x_cpl[loc].i;
  }

  // Iterate over the group chunks from the first pass
  for (R_xlen_t group = 0; group < n_groups; ++group) {
    R_xlen_t group_size = p_group_info_pre->p_data[group];

    // Fast handling of simplest case
    if (group_size == 1) {
      ++p_x_slice_dbl;
      ++p_o;
      groups_size_push(p_group_infos, 1);
      continue;
    }

    dbl_order(
      p_x_slice_dbl,
      p_x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_group_infos,
      decreasing,
      na_last,
      group_size
    );

    p_x_slice_dbl += group_size;
    p_o += group_size;
  }
}

// -----------------------------------------------------------------------------

static R_len_t chr_fill_sizes(int* p_x_chr_sizes, const SEXP* p_x, R_xlen_t size);

static void chr_insertion_order(SEXP* p_x,
                                int* p_x_chr_sizes,
                                int* p_o,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                const R_xlen_t size,
                                const R_len_t pass);

static void chr_radix_order(SEXP* p_x,
                            SEXP* p_x_aux,
                            int* p_x_chr_sizes,
                            int* p_x_chr_sizes_aux,
                            int* p_o,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            struct group_infos* p_group_infos,
                            bool decreasing,
                            bool na_last,
                            const R_xlen_t size,
                            const R_len_t max_size);


static void chr_order(void* p_x,
                      void* p_x_aux,
                      int* p_x_chr_sizes,
                      int* p_x_chr_sizes_aux,
                      int* p_o,
                      int* p_o_aux,
                      uint8_t* p_bytes,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  const R_len_t max_size = chr_fill_sizes(p_x_chr_sizes, p_x, size);

  if (size <= INSERTION_ORDER_BOUNDARY) {
    const int pass = 0;
    chr_insertion_order(p_x, p_x_chr_sizes, p_o, p_group_infos, decreasing, na_last, size, pass);
    return;
  }

  chr_radix_order(
    p_x,
    p_x_aux,
    p_x_chr_sizes,
    p_x_chr_sizes_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    decreasing,
    na_last,
    size,
    max_size
  );
}

static void chr_order_immutable(SEXP x,
                                void* p_x_slice,
                                void* p_x_aux,
                                int* p_x_chr_sizes,
                                int* p_x_chr_sizes_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size) {
  const SEXP* p_x = STRING_PTR_RO(x);

  memcpy(p_x_slice, p_x, size * sizeof(SEXP));

  const R_len_t max_size = chr_fill_sizes(p_x_chr_sizes, p_x, size);

  if (size <= INSERTION_ORDER_BOUNDARY) {
    const int pass = 0;
    chr_insertion_order(p_x_slice, p_x_chr_sizes, p_o, p_group_infos, decreasing, na_last, size, pass);
    return;
  }

  chr_radix_order(
    p_x_slice,
    p_x_aux,
    p_x_chr_sizes,
    p_x_chr_sizes_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    decreasing,
    na_last,
    size,
    max_size
  );
}

// -----------------------------------------------------------------------------

static bool chr_str_ge(SEXP x,
                       SEXP y,
                       int x_size,
                       const int direction,
                       const bool na_last,
                       const R_len_t pass);

/*
 * `chr_insertion_order()` is used in two ways:
 * - It is how we "finish off" radix sorts rather than deep recursion.
 * - If we have an original `x` input that is small enough, we just immediately
 *   insertion sort it.
 *
 * For small inputs, it is much faster than deeply recursing with
 * radix ordering.
 *
 * Unlike `int/dbl_insertion_order()`, we have no way to order `p_x` ahead of
 * time based on `decreasing` and `na_last` so we have to handle that here.
 *
 * Assumes `p_x` is mutable.
 */
static void chr_insertion_order(SEXP* p_x,
                                int* p_x_chr_sizes,
                                int* p_o,
                                struct group_infos* p_group_infos,
                                bool decreasing,
                                bool na_last,
                                const R_xlen_t size,
                                const R_len_t pass) {
  const int direction = decreasing ? -1 : 1;

  // Don't think this can occur, but safer this way
  if (size == 0) {
    return;
  }

  for (R_xlen_t i = 1; i < size; ++i) {
    const SEXP x_elt = p_x[i];
    const int o_elt = p_o[i];
    const int x_elt_size = p_x_chr_sizes[i];

    R_xlen_t j = i - 1;

    while (j >= 0) {
      const SEXP x_cmp_elt = p_x[j];

      if (chr_str_ge(x_elt, x_cmp_elt, x_elt_size, direction, na_last, pass)) {
        break;
      }

      int o_cmp_elt = p_o[j];
      int x_cmp_elt_size = p_x_chr_sizes[j];

      // Swap
      p_x[j + 1] = x_cmp_elt;
      p_o[j + 1] = o_cmp_elt;
      p_x_chr_sizes[j + 1] = x_cmp_elt_size;

      // Next
      --j;
    }

    // Place original elements in new location
    // closer to start of the vector
    p_x[j + 1] = x_elt;
    p_o[j + 1] = o_elt;
    p_x_chr_sizes[j + 1] = x_elt_size;
  }

  // We've ordered a small chunk, we need to push at least one group size.
  // Depends on the post-ordered results so we have to do this
  // in a separate loop.
  R_xlen_t group_size = 1;
  SEXP previous = p_x[0];

  for (R_xlen_t i = 1; i < size; ++i) {
    const SEXP current = p_x[i];

    // Continue the current group run.
    // TODO: I think that comparing pointers is fine here, as equal strings
    // should always be cached to the same CHARSXP (including NAs).
    if (current == previous) {
      ++group_size;
      continue;
    }

    // Push current run size and reset size tracker
    groups_size_push(p_group_infos, group_size);
    group_size = 1;

    previous = current;
  }

  // Push final group run
  groups_size_push(p_group_infos, group_size);
}

// -----------------------------------------------------------------------------

static void chr_radix_order_recurse(SEXP* p_x,
                                    SEXP* p_x_aux,
                                    int* p_x_chr_sizes,
                                    int* p_x_chr_sizes_aux,
                                    int* p_o,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    struct group_infos* p_group_infos,
                                    bool decreasing,
                                    bool na_last,
                                    const R_xlen_t size,
                                    const R_len_t pass,
                                    const R_len_t max_size);

static void chr_radix_order(SEXP* p_x,
                            SEXP* p_x_aux,
                            int* p_x_chr_sizes,
                            int* p_x_chr_sizes_aux,
                            int* p_o,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            struct group_infos* p_group_infos,
                            bool decreasing,
                            bool na_last,
                            const R_xlen_t size,
                            const R_len_t max_size) {
  R_len_t pass = 0;

  chr_radix_order_recurse(
    p_x,
    p_x_aux,
    p_x_chr_sizes,
    p_x_chr_sizes_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    decreasing,
    na_last,
    size,
    pass,
    max_size
  );
}

// -----------------------------------------------------------------------------

static void chr_radix_order_pass(SEXP* p_x,
                                 SEXP* p_x_aux,
                                 int* p_x_chr_sizes,
                                 int* p_x_chr_sizes_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 struct group_infos* p_group_infos,
                                 const bool decreasing,
                                 const bool na_last,
                                 const R_xlen_t size,
                                 const R_len_t pass);

static void chr_radix_order_recurse(SEXP* p_x,
                                    SEXP* p_x_aux,
                                    int* p_x_chr_sizes,
                                    int* p_x_chr_sizes_aux,
                                    int* p_o,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    struct group_infos* p_group_infos,
                                    bool decreasing,
                                    bool na_last,
                                    const R_xlen_t size,
                                    const R_len_t pass,
                                    const R_len_t max_size) {
  R_xlen_t p_counts[UINT8_MAX_SIZE] = { 0 };

  chr_radix_order_pass(
    p_x,
    p_x_aux,
    p_x_chr_sizes,
    p_x_chr_sizes_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos,
    decreasing,
    na_last,
    size,
    pass
  );

  const int next_pass = pass + 1;
  R_xlen_t last_cumulative_count = 0;

  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const R_xlen_t cumulative_count = p_counts[i];

    if (!cumulative_count) {
      continue;
    }

    // Diff the accumulated counts to get the radix group size
    const R_xlen_t group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
      groups_size_push(p_group_infos, 1);
      ++p_x;
      ++p_x_chr_sizes;
      ++p_o;
      continue;
    }

    if (next_pass == max_size) {
      groups_size_push(p_group_infos, group_size);
      p_x += group_size;
      p_x_chr_sizes += group_size;
      p_o += group_size;
      continue;
    }

    // Order next byte of this subgroup
    chr_radix_order_recurse(
      p_x,
      p_x_aux,
      p_x_chr_sizes,
      p_x_chr_sizes_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_group_infos,
      decreasing,
      na_last,
      group_size,
      next_pass,
      max_size
    );

    p_x += group_size;
    p_x_chr_sizes += group_size;
    p_o += group_size;
  }
}

// -----------------------------------------------------------------------------

/*
 * Bucketing chars is a little tricky:
 * - There are 256 ASCII characters in the extended set, numbered 0-255.
 *
 * - Bucket 0 would hold a null character, but these are not allowed in R
 *   strings, so we reserve that one for `NA_STRING`
 *
 * - We also need a way to hold information about implicit `""`. These occur
 *   when a string is shorter than the `max_size`. For example, "car" vs "cars".
 *   If these were ordered together by char from left to right then we need a
 *   way to describe the extra missing character in "car_". Lexicographically
 *   it should sort smaller than anything longer, so we need to put it in
 *   bucket 1. Bucket 1 maps to "Start of Heading", `c("\x01")`
 */

/*
 * Orders based on a single byte corresponding to the current `pass`.
 */
static void chr_radix_order_pass(SEXP* p_x,
                                 SEXP* p_x_aux,
                                 int* p_x_chr_sizes,
                                 int* p_x_chr_sizes_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 struct group_infos* p_group_infos,
                                 const bool decreasing,
                                 const bool na_last,
                                 const R_xlen_t size,
                                 const R_len_t pass) {
  if (size <= INSERTION_ORDER_BOUNDARY) {
    chr_insertion_order(p_x, p_x_chr_sizes, p_o, p_group_infos, decreasing, na_last, size, pass);
    return;
  }

  uint8_t byte = 0;

  const uint8_t na_bucket = 0;
  R_xlen_t na_count = 0;

  // Bucket for implicit `""` when we run out of characters in strings that
  // are shorter than `max_size`. These sort less than longer strings, so we
  // put them in the first possible bucket (after the NA one). The ASCII table
  // says this aligns with the value of "Start of Heading" which forces these
  // to sort identically
  // order(c("\x01", "")) # how does this actually work?
  // order(c("", "\x01"))
  //
  // data.table:::forder(c("\x01", ""))
  // data.table:::forder(c("", "\x01"))
  const uint8_t missing_bucket = 1;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const SEXP x_elt = p_x[i];

    if (x_elt == NA_STRING) {
      byte = na_bucket;
      p_bytes[i] = byte;
      ++na_count;
      continue;
    }

    const R_len_t x_elt_size = p_x_chr_sizes[i];

    // Check if there are characters left in the string and extract the next
    // one if so, otherwise assume implicit "".
    if (pass < x_elt_size) {
      const char* c_x_elt = CHAR(x_elt);
      byte = (uint8_t) c_x_elt[pass];
    } else {
      byte = missing_bucket;
    }

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Assign `na_counts` once at the end
  p_counts[na_bucket] = na_count;

  // Fast check to see if all bytes were the same. If so, skip `pass`.
  if (p_counts[byte] == size) {
    return;
  }

  R_xlen_t cumulative = 0;

  // Handle decreasing/increasing by altering the order in which
  // counts are accumulated. If not `decreasing`, start `j` at
  // 1 to skip NA bucket.
  const int direction = decreasing ? -1 : 1;
  R_xlen_t j = decreasing ? UINT8_MAX_SIZE - 1 : 1;

  // `na_last = false` pushes NA counts to the front
  if (!na_last && na_count != 0) {
    p_counts[na_bucket] = cumulative;
    cumulative += na_count;
  }

  // Accumulate counts, skip zeros
  // Also start at `i = 1` to skip `NA` bucket
  for (uint16_t i = 1; i < UINT8_MAX_SIZE; ++i) {
    R_xlen_t count = p_counts[j];

    if (count == 0) {
      j += direction;
      continue;
    }

    // Insert current cumulative value, then increment
    p_counts[j] = cumulative;
    cumulative += count;

    j += direction;
  }

  // `na_last = true` pushes NA counts to the back
  if (na_last && na_count != 0) {
    p_counts[na_bucket] = cumulative;
  }

  // Place into auxiliary arrays in the correct order, then copy back over
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint8_t byte = p_bytes[i];
    const R_xlen_t loc = p_counts[byte]++;
    p_o_aux[loc] = p_o[i];
    p_x_aux[loc] = p_x[i];
    p_x_chr_sizes_aux[loc] = p_x_chr_sizes[i];
  }

  // Copy back over
  memcpy(p_o, p_o_aux, size * sizeof(int));
  memcpy(p_x, p_x_aux, size * sizeof(SEXP));
  memcpy(p_x_chr_sizes, p_x_chr_sizes_aux, size * sizeof(int));
}

// -----------------------------------------------------------------------------

/*
 * R strings cannot have null characters in them (try "\0").
 * This maps to the ASCII value of 0. Since it is unused, we are free to use
 * this bucket for NA_STRING values.
 */

SEXP chr_print(SEXP x) {
  R_xlen_t size = Rf_xlength(x);
  const SEXP* p_x = STRING_PTR_RO(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    const SEXP elt = p_x[i];
    const char* c_elt = CHAR(elt);
    const R_len_t n = Rf_length(elt);

    for (R_len_t j = 0; j < n; ++j) {
      Rprintf("%c %u\n", c_elt[j], (unsigned char) c_elt[j]);
    }
    Rprintf("%\n");
  }

  return R_NilValue;
}

// -----------------------------------------------------------------------------

/*
 * Is `x` greater than or equal to `y` lexicographically in the C locale?
 *
 * - Assume same encoding (UTF-8 / ASCII)
 * - Handles direction / na_last
 * - Starts comparison at current radix (they are equal up to that point, but
 *   possibly not at that point)
 */
static bool chr_str_ge(SEXP x,
                       SEXP y,
                       int x_size,
                       const int direction,
                       const bool na_last,
                       const R_len_t pass) {
  // Same pointer (including `NA`s)
  if (x == y) {
    return true;
  }

  // if `na_last` then `NA` is greater than anything else
  if (x == NA_STRING) {
    return na_last;
  }
  if (y == NA_STRING) {
    return !na_last;
  }

  // Pure insertion sort - we know nothing yet
  if (pass == 0) {
    const char* c_x = CHAR(x);
    const char* c_y = CHAR(y);

    int cmp = strcmp(c_x, c_y) * direction;
    return cmp >= 0;
  }

  // Otherwise we know they are equal up to the position before `pass`, but
  // it might have been equality with implicit "" so we need to check the
  // length of one of them
  const int last_pass = pass - 1;

  // We are comparing length with C 0-based indexing so we have to do +1.
  if (x_size < last_pass + 1) {
    return true;
  }

  const char* c_x = CHAR(x);
  const char* c_y = CHAR(y);

  // Now start the comparison at `last_pass`, which we know exists
  c_x = c_x + last_pass;
  c_y = c_y + last_pass;

  int cmp = strcmp(c_x, c_y) * direction;
  return cmp >= 0;
}

// -----------------------------------------------------------------------------

/*
 * Fills `p_x_chr_sizes` once for reuse in recursion.
 * Also returns the max size to track the depth of recursion.
 */
static R_len_t chr_fill_sizes(int* p_x_chr_sizes, const SEXP* p_x, R_xlen_t size) {
  R_len_t max_size = 0;

  for (R_xlen_t i = 0; i < size; ++i) {
    const SEXP elt = p_x[i];

    // Will never touch `p_x_chr_sizes` here
    if (elt == NA_STRING) {
      continue;
    }

    const R_len_t elt_size = Rf_length(elt);

    if (elt_size > max_size) {
      max_size = elt_size;
    }

    p_x_chr_sizes[i] = (int) elt_size;
  }

  return max_size;
}

// -----------------------------------------------------------------------------

static void col_order_switch(void* p_x,
                             void* p_x_aux,
                             int* p_x_chr_sizes,
                             int* p_x_chr_sizes_aux,
                             int* p_o,
                             int* p_o_aux,
                             uint8_t* p_bytes,
                             struct group_infos* p_group_infos,
                             bool decreasing,
                             bool na_last,
                             R_xlen_t size,
                             const enum vctrs_type type);


#define DF_ORDER_EXTRACT_CHUNK(CONST_DEREF, CTYPE) do {          \
  const CTYPE* p_col = CONST_DEREF(col);                         \
  CTYPE* p_x_slice_col = (CTYPE*) p_x_slice;                     \
                                                                 \
  /* Extract the next group chunk and place in */                \
  /* sequential order for cache friendliness */                  \
  for (R_xlen_t j = 0; j < group_size; ++j) {                    \
    const int loc = p_o_col[j] - 1;                              \
    p_x_slice_col[j] = p_col[loc];                               \
  }                                                              \
} while (0)

#define DF_ORDER_EXTRACT_CHUNK_CPL() do {                      \
  const Rcomplex* p_col = COMPLEX_RO(col);                     \
  double* p_x_slice_col = (double*) p_x_slice;                 \
                                                               \
  if (rerun_complex) {                                         \
    /* First pass - real */                                    \
    for (R_xlen_t j = 0; j < group_size; ++j) {                \
      const int loc = p_o_col[j] - 1;                          \
      p_x_slice_col[j] = p_col[loc].r;                         \
    }                                                          \
                                                               \
    /* Decrement `i` to rerun column */                        \
    --i;                                                       \
  } else {                                                     \
    /* Second pass - imaginary */                              \
    for (R_xlen_t j = 0; j < group_size; ++j) {                \
      const int loc = p_o_col[j] - 1;                          \
      p_x_slice_col[j] = p_col[loc].i;                         \
    }                                                          \
  }                                                            \
} while (0)

/*
 * `df_order()` is the main user of `p_group_infos`. It uses the grouping
 * of the current column to break up the next column into sub groups. That
 * process is continued until either all columns have been processed or we
 * can tell all of the values apart.
 */
static void df_order(SEXP x,
                     void* p_x_slice,
                     void* p_x_aux,
                     int* p_x_chr_sizes,
                     int* p_x_chr_sizes_aux,
                     int* p_o,
                     int* p_o_aux,
                     uint8_t* p_bytes,
                     struct group_infos* p_group_infos,
                     SEXP decreasing,
                     bool na_last,
                     R_xlen_t size) {
  R_xlen_t n_cols = Rf_xlength(x);

  bool recycle_decreasing;
  R_xlen_t n_decreasing = Rf_xlength(decreasing);
  int* p_decreasing = LOGICAL(decreasing);

  if (n_decreasing == 1) {
    recycle_decreasing = true;
  } else if (n_decreasing == n_cols) {
    recycle_decreasing = false;
  } else {
    Rf_errorcall(
      R_NilValue,
      "`decreasing` should have length 1 or length equal to the number of "
      "columns of `x` when `x` is a data frame."
    );
  }

  // Special case no columns
  if (n_cols == 0) {
    return;
  }

  SEXP col = VECTOR_ELT(x, 0);
  bool col_decreasing = p_decreasing[0];
  enum vctrs_type type = vec_proxy_typeof(col);

  // Apply on one column to fill `p_group_infos`.
  // First column is immutable and we must copy into `x_slice`.
  vec_order_immutable_switch(
    col,
    p_x_slice,
    p_x_aux,
    p_x_chr_sizes,
    p_x_chr_sizes_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_group_infos,
    col_decreasing,
    na_last,
    size,
    type
  );

  // For complex, we have to rerun the column a second time on the
  // imaginary part. This is done by decrementing `i` after processing
  // the real part so the column is rerun.
  bool rerun_complex = false;

  // Iterate over remaining columns by group chunk
  for (R_xlen_t i = 1; i < n_cols; ++i) {
    col = VECTOR_ELT(x, i);

    if (!recycle_decreasing) {
      col_decreasing = p_decreasing[i];
    }

    // Reset pointer between columns since we increment them as
    // we iterate through the groups
    int* p_o_col = p_o;

    // Get the number of group chunks from previous column group info
    struct group_info* p_group_info_pre = groups_current(p_group_infos);
    R_xlen_t n_groups = p_group_info_pre->n_groups;

    // If there were no ties, we are completely done
    if (n_groups == size) {
      break;
    }

    type = vec_proxy_typeof(col);

    // If we are on the rerun pass, flip this back off so the
    // imaginary part is extracted below.
    if (type == vctrs_type_complex) {
      rerun_complex = rerun_complex ? false : true;
    }

    // Turn off group tracking if:
    // - We are on the last column
    // - The user didn't request group information
    // - That column isn't the first pass of a complex column
    if (i == n_cols - 1 && !p_group_infos->requested && !rerun_complex) {
      p_group_infos->ignore = true;
    }

    // Swap to other group info to prepare for this column
    groups_swap(p_group_infos);

    // Iterate over this column's group chunks
    for (R_xlen_t group = 0; group < n_groups; ++group) {
      R_xlen_t group_size = p_group_info_pre->p_data[group];

      // Fast handling of simplest case
      if (group_size == 1) {
        ++p_o_col;
        groups_size_push(p_group_infos, 1);
        continue;
      }

      // Extract current chunk and place into `x_slice` in sequential order
      switch (type) {
      case vctrs_type_integer: DF_ORDER_EXTRACT_CHUNK(INTEGER_RO, int); break;
      case vctrs_type_logical: DF_ORDER_EXTRACT_CHUNK(LOGICAL_RO, int); break;
      case vctrs_type_double: DF_ORDER_EXTRACT_CHUNK(REAL_RO, double); break;
      case vctrs_type_complex: DF_ORDER_EXTRACT_CHUNK_CPL(); break;
      case vctrs_type_character: DF_ORDER_EXTRACT_CHUNK(STRING_PTR_RO, SEXP); break;
      default: Rf_errorcall(R_NilValue, "Unknown data frame column type in `vec_order()`.");
      }

      col_order_switch(
        p_x_slice,
        p_x_aux,
        p_x_chr_sizes,
        p_x_chr_sizes_aux,
        p_o_col,
        p_o_aux,
        p_bytes,
        p_group_infos,
        col_decreasing,
        na_last,
        group_size,
        type
      );

      p_o_col += group_size;
    }
  }
}

#undef DF_ORDER_EXTRACT_CHUNK
#undef DF_ORDER_EXTRACT_CHUNK_CPL

// -----------------------------------------------------------------------------

// Specifically for columns of a data frame where `decreasing` is known
// and is scalar. Assumes `p_x` holds the current group chunk.
static void col_order_switch(void* p_x,
                             void* p_x_aux,
                             int* p_x_chr_sizes,
                             int* p_x_chr_sizes_aux,
                             int* p_o,
                             int* p_o_aux,
                             uint8_t* p_bytes,
                             struct group_infos* p_group_infos,
                             bool decreasing,
                             bool na_last,
                             R_xlen_t size,
                             const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer: {
    int_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_group_infos, decreasing, na_last, size);
    break;
  }
  case vctrs_type_logical: {
    lgl_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_group_infos, decreasing, na_last, size);
    break;
  }
  case vctrs_type_double: {
    dbl_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_group_infos, decreasing, na_last, size);
    break;
  }
  case vctrs_type_complex: {
    // Complex types are run in two passes, once over real then over imaginary
    dbl_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_group_infos, decreasing, na_last, size);
    break;
  }
  case vctrs_type_character: {
    chr_order(p_x, p_x_aux, p_x_chr_sizes, p_x_chr_sizes_aux, p_o, p_o_aux, p_bytes, p_group_infos, decreasing, na_last, size);
    break;
  }
  case vctrs_type_dataframe: {
    Rf_errorcall(R_NilValue, "Internal error: df-cols should have already been flattened.");
    break;
  }
  default: {
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`");
  }
  }
}

// -----------------------------------------------------------------------------

// Pair with `PROTECT_GROUP_INFO()` in the caller
static struct group_info new_group_info() {
  struct group_info info;

  info.data_size = 0;

  info.data = PROTECT(Rf_allocVector(INTSXP, 0));

  info.n_groups = 0;
  info.max_group_size = 0;

  UNPROTECT(1);
  return info;
}

// A good bit faster than `Rf_xlengthgets()` because that fills the new extended
// locations with `NA` as well, which we don't need.
static SEXP group_extend(const int* p_data, R_xlen_t data_size, R_xlen_t size) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  memcpy(p_out, p_data, data_size * sizeof(int));

  UNPROTECT(1);
  return out;
}

/*
 * Reallocate `data` to be as long as `size`.
 */
static void group_realloc(struct group_info* p_group_info, R_xlen_t size) {
  // First allocation
  if (size == 0) {
    size = GROUP_DATA_SIZE_DEFAULT;
  }

  // Reallocate
  p_group_info->data = group_extend(
    p_group_info->p_data,
    p_group_info->data_size,
    size
  );

  // Reprotect
  REPROTECT(p_group_info->data, p_group_info->data_pi);

  // Update pointer
  p_group_info->p_data = INTEGER(p_group_info->data);

  // Update size
  p_group_info->data_size = size;
}

static struct group_infos new_group_infos(struct group_info** p_p_group_info,
                                          bool requested,
                                          bool ignore) {
  struct group_infos infos;

  infos.p_p_group_info = p_p_group_info;
  infos.current = 0;
  infos.requested = requested;
  infos.ignore = ignore;

  return infos;
}

/*
 * Extract the current `group_info*`
 */
static inline struct group_info* groups_current(struct group_infos* p_group_infos) {
  return p_group_infos->p_p_group_info[p_group_infos->current];
}

/*
 * `groups_swap()` is called after each data frame column is processed.
 * It handles switching the `current` group info that we are working on,
 * and ensures that the information that might have been there before has
 * been zeroed out. It also ensures that the new current group info has at
 * least as much space as the previous one, which is especially important for
 * the first column swap where the 2nd group info array starts as a size 0
 * integer vector (because we don't know if it will get used or not).
 */
static void groups_swap(struct group_infos* p_group_infos) {
  if (p_group_infos->ignore) {
    return;
  }

  struct group_info* p_group_info_pre = groups_current(p_group_infos);

  // Make the swap
  p_group_infos->current = 1 - p_group_infos->current;

  struct group_info* p_group_info_post = groups_current(p_group_infos);

  // Clear the info from last time the swap was made
  p_group_info_post->max_group_size = 0;
  p_group_info_post->n_groups = 0;

  // Ensure the new group info is at least as big as the old group info
  if (p_group_info_post->data_size < p_group_info_pre->data_size) {
    group_realloc(p_group_info_post, p_group_info_pre->data_size * 2);
  }
}

/*
 * Push a group size onto the current `group_info*`
 * - Does nothing if we are ignoring group info
 * - Reallocates as needed
 * - Updates number of groups / max group size as well
 */
static void groups_size_push(struct group_infos* p_group_infos, R_xlen_t size) {
  if (p_group_infos->ignore) {
    return;
  }

  if (size == 0) {
    Rf_errorcall(R_NilValue, "Internal error: Group `size` to push should never be zero.");
  }

  struct group_info* p_group_info = groups_current(p_group_infos);

  // Extend `data` as required - reprotects itself
  if (p_group_info->data_size == p_group_info->n_groups) {
    group_realloc(p_group_info, p_group_info->data_size * 2);
  }

  // Push group size
  p_group_info->p_data[p_group_info->n_groups] = size;

  // Bump number of groups
  ++p_group_info->n_groups;

  // Update max group size
  if (p_group_info->max_group_size < size) {
    p_group_info->max_group_size = size;
  }
}

// -----------------------------------------------------------------------------

static inline size_t df_size_multiplier(SEXP x);

/*
 * Compute the minimum size required for `x_aux` and `x_slice`.
 *
 * For complex, we split the vector into two double vectors. We only need to
 * allocate 1 double vector though, and it will be reused for both the real
 * and imaginary parts.
 */
static inline size_t vec_order_size_multiplier(SEXP x) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_integer:
  case vctrs_type_logical:
    return sizeof(int);
  case vctrs_type_double:
    return sizeof(double);
  case vctrs_type_complex:
    // Complex types will be split into two double vectors
    return sizeof(double);
  case vctrs_type_character:
    return sizeof(SEXP);
  case vctrs_type_dataframe:
    return df_size_multiplier(x);
  default:
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`.");
  }
}

// `x` should be a flattened df with no df-cols
static inline size_t df_size_multiplier(SEXP x) {
  R_xlen_t n_cols = Rf_xlength(x);

  // Should be an appropriate default for 0 column data frames
  size_t multiplier = 0;

  for (R_xlen_t i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    size_t col_multiplier = vec_order_size_multiplier(col);

    if (col_multiplier > multiplier) {
      multiplier = col_multiplier;
    }
  }

  return multiplier;
}

// -----------------------------------------------------------------------------

static bool df_any_character(SEXP x);

static bool vec_order_any_character(SEXP x, const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_character:
    return true;
  case vctrs_type_dataframe:
    return df_any_character(x);
  default:
    return false;
  }
}

static bool df_any_character(SEXP x) {
  R_xlen_t n_cols = Rf_xlength(x);

  for (R_xlen_t i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    const enum vctrs_type type = vec_proxy_typeof(col);

    if (vec_order_any_character(col, type)) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------

#undef UINT8_MAX_SIZE

#undef INT_COUNTING_ORDER_RANGE_BOUNDARY

#undef INSERTION_ORDER_BOUNDARY
