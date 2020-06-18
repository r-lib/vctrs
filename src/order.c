#include "vctrs.h"
#include "utils.h"
#include "order-groups.h"
#include "order-truelength.h"
#include "order-lazy.h"

// -----------------------------------------------------------------------------

/*
 * High level description of `vec_order()`
 *
 * Heavily inspired by `radixsort.c` in base R and `forder()` from data.table
 * https://github.com/wch/r-source/blob/trunk/src/main/radixsort.c
 * https://github.com/Rdatatable/data.table/blob/master/src/forder.c
 *
 * Additional resources about radix sorting:
 * http://codercorner.com/RadixSortRevisited.htm
 * http://stereopsis.com/radix.html
 *
 * The very end of this has a MSB radix sort implementation
 * https://eternallyconfuzzled.com/sorting-c-introduction-to-the-automatic-ordering-of-data
 *
 * -----------------------------------------------------------------------------
 * Integers
 *
 * This uses a combination of 3 ordering algorithms.
 *
 * - `int_insertion_order()` - An insertion sort is used when `x` is very
 *   small. This has less overhead than the counting or radix sort and is
 *   faster for small input.
 *
 * - `int_counting_order()` - A counting sort is used when `x` has a range
 *   of less than `INT_COUNTING_ORDER_RANGE_BOUNDARY`. For integers with a
 *   small range like this, the bucketing in the counting sort can be very
 *   fast when compared with the recursive multipass approach of the radix sort.
 *
 * - `int_radix_order()` - A radix sort is used for everything else.
 *   This is a MSB radix sort. It orders the vector 1 byte (8 bits) at a time,
 *   so for a 4 byte int this makes a maximum of 4 passes over each integer.
 *   It orders from most significant byte to least significant. After each
 *   pass, there are 256 buckets (1 for each possible byte value). Each bucket
 *   is then ordered separately on the next byte. This happens recursively for
 *   the 4 passes. When the buckets get small enough in size, the insertion
 *   sort is used to finish them off.
 *
 * For radix sorting, we have to use unsigned types for bit shifting to
 * work reliably. We map `int` to `uint32_t` in a way that preserves order,
 * and also handle `na_last` and `decreasing` in this mapping. This all happens
 * in `int_adjust()`. It is assumed and checked at load time that
 * `sizeof(int) == 4`.
 *
 * -----------------------------------------------------------------------------
 * Doubles
 *
 * This uses a combination of 2 ordering algorithms:
 *
 * - `dbl_insertion_order()` - An insertion sort is used when `x` is very small.
 *
 * - `dbl_radix_order()` - This is similar to `int_radix_order()`, see above,
 *   but makes a max of 8 passes over the data.
 *
 * For doubles, we assume `sizeof(double) == 8`, which should pretty much be
 * ensured by IEEE 754 specifications.
 *
 * For the mapping here, it is possible to map `double -> uint64_t` in an
 * order preserving way. This is very cool, and involves always flipping the
 * sign bit of the value, and flipping all other bits if the value was negative.
 * This is described more in: http://stereopsis.com/radix.html.
 * This is implemented in `dbl_adjust()` which also handles `na_last` and
 * `decreasing`. For `na_last`, we treat `NA_real_` and `NaN` equivalently.
 * Base R does as well, but data.table does not.
 *
 * -----------------------------------------------------------------------------
 * Characters
 *
 * Character vector ordering is a bit trickier than integers or doubles. It
 * uses two internal details in R:
 *
 * - CHARSXP values in R are in a global string pool, and
 *   repeated strings like `c("hi", "hi")` both point to the same CHARSXP.
 *
 * - There is a property that all vectors have called the TRUELENGTH. This is
 *   used to overallocate in lists, but is otherwise unused
 *   in CHARSXPs. The truelength is stored as a `R_xlen_t`.
 *
 * Character ordering is achieved by first iterating through `x` and extracting
 * the unique values by using the TRUELENGTH. If the truelength is positive
 * or zero, we save it as an unseen string and set the truelength to `-1`.
 * Otherwise if it is negative we assume we have seen it already. At the
 * end of this we have a vector of unique strings. We order this with
 * `chr_radix_order()`. Then we iterate over the now sorted unique strings
 * and set their truelength to `-i - 1` (where `i` is the index while
 * iterating). This marks the unique strings with their ordering (as a negative
 * value to be different from R) and also updates the original character vector.
 * We then iterate through the original vector again, plucking off the now
 * updated truelength integer values. This gives us an integer proxy for the
 * ordering, which we can run through `int_order_chunk()` to get the final
 * ordering.
 *
 * The ordering of unique characters uses a combination of 2 ordering
 * algorithms:
 *
 * - `chr_insertion_order()` - Used when `x` is small.
 *
 * - `chr_radix_order()` - Same principle as integer/double ordering, but
 *   we iterate 1 character at a time. We assume a C locale here, and any
 *   non-ASCII and non-UTF8 strings are translated to UTF8.
 *
 * -----------------------------------------------------------------------------
 * Logicals
 *
 * Just use the same infrastructure as integers.
 *
 * -----------------------------------------------------------------------------
 * Complex
 *
 * We treat complex as a data frame of two double columns. We order the
 * real part first using `dbl_order_chunk()`, then order the imaginary part also
 * using `dbl_order_chunk()`.
 *
 * -----------------------------------------------------------------------------
 * Data frames
 *
 * Multi-column data frame ordering uses the same principle as MSB ordering.
 * It starts with the first column (the most "significant" one) and orders it.
 * While ordering the column, group sizes are tracked ("groups" are duplicate
 * values in the column). The next column is broken into chunks corresponding
 * to these group sizes from the first column, and the chunks are ordered
 * individually. While ordering the chunks of the 2nd column, group sizes are
 * again tracked to use in subsequent columns.
 */

// -----------------------------------------------------------------------------

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

/*
 * Maximum number of passes required to completely sort ints and doubles
 */
#define INT_MAX_RADIX_PASS 4
#define DBL_MAX_RADIX_PASS 8

/*
 * Maximum range allowed when deciding whether or not to use a counting sort
 * vs a radix sort. Counting sort is somewhat faster when less than this
 * boundary value.
 */
#define INT_COUNTING_ORDER_RANGE_BOUNDARY 100000

/*
 * Size of `x` that determines when an insertion sort should be used. Seems
 * to work better than 256 (from limited testing), base R uses 200.
 * Somewhat based on this post:
 * https://probablydance.com/2016/12/27/i-wrote-a-faster-sorting-algorithm/
 */
#define INSERTION_ORDER_BOUNDARY 128

/*
 * Adjustments for endianness and integer / double radix ordering.
 * - On big-endian, MSB is on the far right.
 * - On little-endian, MSB is on the far left.
 */
#ifdef WORDS_BIGENDIAN
# define PASS_TO_RADIX(X, MAX) (X)
# define SHIFT_ADJUSTMENT 8
#else
# define PASS_TO_RADIX(X, MAX) (MAX - 1 - X)
# define SHIFT_ADJUSTMENT -8
#endif

/*
 * Utilities for deciding whether or not to re-encode as UTF-8
 */
#define CHAR_IS_UTF8(x)  (LEVELS(x) & 8)
#define CHAR_IS_ASCII(x) (LEVELS(x) & 64)
#define CHAR_NEEDS_REENCODE(x) (!(CHAR_IS_ASCII(x) || (x) == NA_STRING || CHAR_IS_UTF8(x)))
#define CHAR_REENCODE(x) Rf_mkCharCE(Rf_translateCharUTF8(x), CE_UTF8)

// -----------------------------------------------------------------------------

static SEXP vec_order(SEXP x, SEXP decreasing, bool na_last);

// [[ register() ]]
SEXP vctrs_order(SEXP x, SEXP decreasing, SEXP na_last) {
  if (!r_is_bool(na_last)) {
    Rf_errorcall(R_NilValue, "`na_last` must be either `TRUE` or `FALSE`.");
  }

  bool c_na_last = LOGICAL(na_last)[0];

  return vec_order(x, decreasing, c_na_last);
}


static SEXP vec_order_impl(SEXP x, SEXP decreasing, bool na_last, bool groups);

static SEXP vec_order(SEXP x, SEXP decreasing, bool na_last) {
  return vec_order_impl(x, decreasing, na_last, false);
}

// -----------------------------------------------------------------------------

static SEXP vec_order_groups(SEXP x, SEXP decreasing, bool na_last);

// [[ register() ]]
SEXP vctrs_order_groups(SEXP x, SEXP decreasing, SEXP na_last) {
  if (!r_is_bool(na_last)) {
    Rf_errorcall(R_NilValue, "`na_last` must be either `TRUE` or `FALSE`.");
  }

  bool c_na_last = LOGICAL(na_last)[0];

  return vec_order_groups(x, decreasing, c_na_last);
}


static SEXP vec_order_groups(SEXP x, SEXP decreasing, bool na_last) {
  return vec_order_impl(x, decreasing, na_last, true);
}

// -----------------------------------------------------------------------------

static SEXP vec_order_groups_info(SEXP o, struct group_info* p_group_info);
static inline size_t vec_order_size_multiplier(SEXP x, const enum vctrs_type type);
static inline size_t vec_order_counts_multiplier(SEXP x, const enum vctrs_type type);
static SEXP vec_order_check_decreasing(SEXP x, SEXP decreasing);

static void vec_order_switch(SEXP x,
                             int* p_o,
                             struct lazy_vec* p_lazy_x_slice,
                             struct lazy_vec* p_lazy_x_aux,
                             struct lazy_vec* p_lazy_o_aux,
                             struct lazy_vec* p_lazy_bytes,
                             struct lazy_vec* p_lazy_counts,
                             struct group_infos* p_group_infos,
                             struct truelength_info* p_truelength_info,
                             SEXP decreasing,
                             bool na_last,
                             R_xlen_t size,
                             const enum vctrs_type type);

/*
 * Returns an integer vector of the ordering unless `groups` is true. In
 * that case it returns a list of length 4 containing the ordering, the
 * starts of each group, the number of groups, and the maximum group size.
 */
static SEXP vec_order_impl(SEXP x, SEXP decreasing, bool na_last, bool groups) {
  int n_prot = 0;
  int* p_n_prot = &n_prot;

  // Call on `x` before potentially flattening cols with `vec_proxy_compare()`
  decreasing = PROTECT_N(vec_order_check_decreasing(x, decreasing), p_n_prot);

  x = PROTECT_N(vec_proxy_compare(x), p_n_prot);

  R_xlen_t size = vec_size(x);
  const enum vctrs_type type = vec_proxy_typeof(x);

  // Compute the maximum size required for auxiliary working memory
  const size_t multiplier = vec_order_size_multiplier(x, type);

  // Auxiliary vectors to hold intermediate results while ordering.
  // If `x` is a data frame we allocate enough room for the largest column type.
  struct lazy_vec lazy_x_slice = new_lazy_vec(size, multiplier);
  struct lazy_vec* p_lazy_x_slice = &lazy_x_slice;
  PROTECT_LAZY_VEC(p_lazy_x_slice, p_n_prot);

  struct lazy_vec lazy_x_aux = new_lazy_vec(size, multiplier);
  struct lazy_vec* p_lazy_x_aux = &lazy_x_aux;
  PROTECT_LAZY_VEC(p_lazy_x_aux, p_n_prot);

  struct lazy_vec lazy_o_aux = new_lazy_vec(size, sizeof(int));
  struct lazy_vec* p_lazy_o_aux = &lazy_o_aux;
  PROTECT_LAZY_VEC(p_lazy_o_aux, p_n_prot);

  struct lazy_vec lazy_bytes = new_lazy_vec(size, sizeof(uint8_t));
  struct lazy_vec* p_lazy_bytes = &lazy_bytes;
  PROTECT_LAZY_VEC(p_lazy_bytes, p_n_prot);

  // Compute the maximum size of the `counts` vector needed during radix
  // ordering. 4 * 256 for integers, 8 * 256 for doubles.
  size_t counts_multiplier = vec_order_counts_multiplier(x, type);
  R_xlen_t counts_size = UINT8_MAX_SIZE * counts_multiplier;

  struct lazy_vec lazy_counts = new_lazy_vec(counts_size, sizeof(R_xlen_t));
  struct lazy_vec* p_lazy_counts = &lazy_counts;
  PROTECT_LAZY_VEC(p_lazy_counts, p_n_prot);

  // Determine if group tracking can be turned off.
  // We turn if off if ordering non-data frame input as long as groups haven't
  // been requested by the user.
  // It is more efficient to ignore it when possible.
  bool requested = groups;
  bool ignore = requested ? false : (is_data_frame(x) ? false : true);

  // Construct the two sets of group info needed for tracking groups.
  // We switch between them after each data frame column is processed.
  struct group_info group_info0 = new_group_info();
  struct group_info* p_group_info0 = &group_info0;
  PROTECT_GROUP_INFO(p_group_info0, p_n_prot);

  struct group_info group_info1 = new_group_info();
  struct group_info* p_group_info1 = &group_info1;
  PROTECT_GROUP_INFO(p_group_info1, p_n_prot);

  struct group_info* p_p_group_info[2];
  p_p_group_info[0] = p_group_info0;
  p_p_group_info[1] = p_group_info1;

  struct group_infos group_infos = new_group_infos(
    p_p_group_info,
    size,
    requested,
    ignore
  );
  struct group_infos* p_group_infos = &group_infos;

  // Used for character ordering - lazily generated to be fast
  // when not ordering character vectors
  struct truelength_info truelength_info = new_truelength_info(size);
  struct truelength_info* p_truelength_info = &truelength_info;
  PROTECT_TRUELENGTH_INFO(p_truelength_info, p_n_prot);

  SEXP o = PROTECT_N(Rf_allocVector(INTSXP, size), p_n_prot);
  int* p_o = INTEGER(o);

  // Initialize `o` with sequential 1-based ordering
  for (R_xlen_t i = 0; i < size; ++i) {
    p_o[i] = i + 1;
  }

  vec_order_switch(
    x,
    p_o,
    p_lazy_x_slice,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    p_truelength_info,
    decreasing,
    na_last,
    size,
    type
  );

  // Return all group info rather than just ordering
  if (groups) {
    struct group_info* p_group_info = groups_current(p_group_infos);
    SEXP out = vec_order_groups_info(o, p_group_info);
    UNPROTECT(n_prot);
    return out;
  }

  UNPROTECT(n_prot);
  return o;
}

// -----------------------------------------------------------------------------

/*
 * Computes the full set of group information, not just the ordering.
 * Computes 1-based group `starts` from the group sizes.
 */
static SEXP vec_order_groups_info(SEXP o, struct group_info* p_group_info) {
  R_xlen_t n_groups = p_group_info->n_groups;

  const int* p_sizes = p_group_info->p_data;

  SEXP starts = PROTECT(Rf_allocVector(INTSXP, n_groups));
  int* p_starts = INTEGER(starts);

  int current = 1;

  for (R_xlen_t i = 0; i < n_groups; ++i) {
    p_starts[i] = current;
    current += p_sizes[i];
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(out, 0, o);
  SET_VECTOR_ELT(out, 1, starts);
  SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(p_group_info->n_groups));
  SET_VECTOR_ELT(out, 3, Rf_ScalarInteger(p_group_info->max_group_size));

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 4));

  SET_STRING_ELT(names, 0, Rf_mkChar("order"));
  SET_STRING_ELT(names, 1, Rf_mkChar("starts"));
  SET_STRING_ELT(names, 2, Rf_mkChar("n_groups"));
  SET_STRING_ELT(names, 3, Rf_mkChar("max_group_size"));

  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

static void df_order(SEXP x,
                     int* p_o,
                     struct lazy_vec* p_lazy_x_slice,
                     struct lazy_vec* p_lazy_x_aux,
                     struct lazy_vec* p_lazy_o_aux,
                     struct lazy_vec* p_lazy_bytes,
                     struct lazy_vec* p_lazy_counts,
                     struct group_infos* p_group_infos,
                     struct truelength_info* p_truelength_info,
                     SEXP decreasing,
                     bool na_last,
                     R_xlen_t size);

static void vec_order_base_switch(SEXP x,
                                  int* p_o,
                                  struct lazy_vec* p_lazy_x_slice,
                                  struct lazy_vec* p_lazy_x_aux,
                                  struct lazy_vec* p_lazy_o_aux,
                                  struct lazy_vec* p_lazy_bytes,
                                  struct lazy_vec* p_lazy_counts,
                                  struct group_infos* p_group_infos,
                                  struct truelength_info* p_truelength_info,
                                  bool decreasing,
                                  bool na_last,
                                  R_xlen_t size,
                                  const enum vctrs_type type);

static void vec_order_switch(SEXP x,
                             int* p_o,
                             struct lazy_vec* p_lazy_x_slice,
                             struct lazy_vec* p_lazy_x_aux,
                             struct lazy_vec* p_lazy_o_aux,
                             struct lazy_vec* p_lazy_bytes,
                             struct lazy_vec* p_lazy_counts,
                             struct group_infos* p_group_infos,
                             struct truelength_info* p_truelength_info,
                             SEXP decreasing,
                             bool na_last,
                             R_xlen_t size,
                             const enum vctrs_type type) {
  if (type == vctrs_type_dataframe) {
    df_order(
      x,
      p_o,
      p_lazy_x_slice,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      p_truelength_info,
      decreasing,
      na_last,
      size
    );

    return;
  }

  if (Rf_xlength(decreasing) != 1) {
    Rf_errorcall(
      R_NilValue,
      "Internal error: Size of decreasing != 1, but "
      "`vec_order_check_decreasing()` didn't catch it."
    );
  }

  bool c_decreasing = LOGICAL(decreasing)[0];

  vec_order_base_switch(
    x,
    p_o,
    p_lazy_x_slice,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    p_truelength_info,
    c_decreasing,
    na_last,
    size,
    type
  );
}

// -----------------------------------------------------------------------------

static void int_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size);

static void lgl_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size);

static void dbl_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size);

static void cpl_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size);

static void chr_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      struct truelength_info* p_truelength_info,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size);

// Used on bare vectors and the first column of data frame `x`s
static void vec_order_base_switch(SEXP x,
                                  int* p_o,
                                  struct lazy_vec* p_lazy_x_slice,
                                  struct lazy_vec* p_lazy_x_aux,
                                  struct lazy_vec* p_lazy_o_aux,
                                  struct lazy_vec* p_lazy_bytes,
                                  struct lazy_vec* p_lazy_counts,
                                  struct group_infos* p_group_infos,
                                  struct truelength_info* p_truelength_info,
                                  bool decreasing,
                                  bool na_last,
                                  R_xlen_t size,
                                  const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer: {
    int_order(
      x,
      p_o,
      p_lazy_x_slice,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_logical: {
    lgl_order(
      x,
      p_o,
      p_lazy_x_slice,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_double: {
    dbl_order(
      x,
      p_o,
      p_lazy_x_slice,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_complex: {
    cpl_order(
      x,
      p_o,
      p_lazy_x_slice,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_character: {
    chr_order(
      x,
      p_o,
      p_lazy_x_slice,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      p_truelength_info,
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
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`.");
  }
  }
}

// -----------------------------------------------------------------------------

static bool int_sorted(const int* p_x,
                       int* p_o,
                       struct group_infos* p_group_infos,
                       R_xlen_t size,
                       bool decreasing,
                       bool na_last);

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
                            int* p_o,
                            uint32_t* p_x_aux,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            R_xlen_t* p_counts,
                            struct group_infos* p_group_infos,
                            const R_xlen_t size);

/*
 * These are the main entry points for integer ordering. They are nearly
 * identical except `int_order()` assumes that `p_x` cannot be
 * modified directly and is user input.
 *
 * `int_order_chunk()` assumes `p_x` is modifiable by reference. It is called
 * when iterating over data frame columns and `p_x` is the 2nd or greater
 * column, in which case `p_x` is really a chunk of that column that has been
 * copied into `x_slice`.
 *
 * `int_order()` assumes `p_x` is user input which cannot be modified.
 * It copies `x` into another SEXP that can be modified directly unless a
 * counting sort is going to be used, in which case `p_x` can be used directly.
 */
static void int_order_chunk(void* p_x,
                            int* p_o,
                            struct lazy_vec* p_lazy_x_aux,
                            struct lazy_vec* p_lazy_o_aux,
                            struct lazy_vec* p_lazy_bytes,
                            struct lazy_vec* p_lazy_counts,
                            struct group_infos* p_group_infos,
                            bool decreasing,
                            bool na_last,
                            R_xlen_t size) {
  if (int_sorted(p_x, p_o, p_group_infos, size, decreasing, na_last)) {
    return;
  }

  if (size <= INSERTION_ORDER_BOUNDARY) {
    int_adjust(p_x, decreasing, na_last, size);
    int_insertion_order(p_x, p_o, p_group_infos, size);
    return;
  }

  lazy_vec_initialize(p_lazy_o_aux);
  int* p_o_aux = (int*) p_lazy_o_aux->p_data;

  uint32_t range;
  int x_min;

  int_compute_range(p_x, size, &x_min, &range);

  if (range < INT_COUNTING_ORDER_RANGE_BOUNDARY) {
    int_counting_order(
      p_x,
      p_o,
      p_o_aux,
      p_group_infos,
      size,
      x_min,
      range,
      decreasing,
      na_last
    );

    return;
  }

  int_adjust(p_x, decreasing, na_last, size);

  lazy_vec_initialize(p_lazy_x_aux);
  uint32_t* p_x_aux = (uint32_t*) p_lazy_x_aux->p_data;

  lazy_vec_initialize(p_lazy_bytes);
  uint8_t* p_bytes = (uint8_t*) p_lazy_bytes->p_data;

  lazy_vec_initialize(p_lazy_counts);
  R_xlen_t* p_counts = (R_xlen_t*) p_lazy_counts->p_data;
  memset(p_counts, 0, p_lazy_counts->size);

  int_radix_order(
    p_x,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos,
    size
  );
}

static void int_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  const int* p_x = INTEGER_RO(x);

  if (int_sorted(p_x, p_o, p_group_infos, size, decreasing, na_last)) {
    return;
  }

  if (size <= INSERTION_ORDER_BOUNDARY) {
    lazy_vec_initialize(p_lazy_x_slice);
    void* p_x_slice = p_lazy_x_slice->p_data;

    memcpy(p_x_slice, p_x, size * sizeof(int));

    int_adjust(p_x_slice, decreasing, na_last, size);

    int_insertion_order(p_x_slice, p_o, p_group_infos, size);

    return;
  }

  lazy_vec_initialize(p_lazy_o_aux);
  int* p_o_aux = (int*) p_lazy_o_aux->p_data;

  uint32_t range;
  int x_min;

  int_compute_range(p_x, size, &x_min, &range);

  // If in counting sort range, no need to copy over to `x_slice`
  if (range < INT_COUNTING_ORDER_RANGE_BOUNDARY) {
    int_counting_order(
      p_x,
      p_o,
      p_o_aux,
      p_group_infos,
      size,
      x_min,
      range,
      decreasing,
      na_last
    );

    return;
  }

  lazy_vec_initialize(p_lazy_x_slice);
  void* p_x_slice = p_lazy_x_slice->p_data;

  memcpy(p_x_slice, p_x, size * sizeof(int));
  int_adjust(p_x_slice, decreasing, na_last, size);

  lazy_vec_initialize(p_lazy_x_aux);
  uint32_t* p_x_aux = (uint32_t*) p_lazy_x_aux->p_data;

  lazy_vec_initialize(p_lazy_bytes);
  uint8_t* p_bytes = (uint8_t*) p_lazy_bytes->p_data;

  lazy_vec_initialize(p_lazy_counts);
  R_xlen_t* p_counts = (R_xlen_t*) p_lazy_counts->p_data;
  memset(p_counts, 0, p_lazy_counts->size);

  int_radix_order(
    p_x_slice,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos,
    size
  );
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
 *   sort (which definitely can handle that case) by returning a `range` of the
 *   maximum uint32 value (which will be greater than
 *   INT_COUNTING_ORDER_RANGE_BOUNDARY).
 */
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
  for (R_xlen_t j = i; j < size; ++j) {
    const int elt = p_x[j];

    if (elt == NA_INTEGER) {
      continue;
    }

    if (elt > x_max) {
      x_max = elt;
    } else if (elt < x_min) {
      x_min = elt;
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

// -----------------------------------------------------------------------------

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
                                    int* p_o,
                                    uint32_t* p_x_aux,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    R_xlen_t* p_counts,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos,
                                    const R_xlen_t size,
                                    const uint8_t pass);

/*
 * Integer radix ordering entry point
 *
 * Expects that `int_adjust()` has been called on `p_x`, which takes care
 * of `na_last` and `decreasing` and also maps `int32_t` to `uint32_t` once
 * up front so we don't have to do it for each radix pass.
 *
 * Sorts `p_x` and `p_o` in place
 */
static void int_radix_order(uint32_t* p_x,
                            int* p_o,
                            uint32_t* p_x_aux,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            R_xlen_t* p_counts,
                            struct group_infos* p_group_infos,
                            const R_xlen_t size) {
  bool p_skips[INT_MAX_RADIX_PASS];

  uint8_t pass = int_compute_skips(p_skips, p_x, size);

  // Skipped all passes - Happens when `x` is 1 value repeated
  if (pass == INT_MAX_RADIX_PASS) {
    groups_size_push(p_group_infos, size);
    return;
  }

  int_radix_order_recurse(
    p_x,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_skips,
    p_group_infos,
    size,
    pass
  );
}

// -----------------------------------------------------------------------------

static inline uint8_t int_extract_uint32_byte(uint32_t x, uint8_t shift);

/*
 * Recursive function for radix ordering. Orders the current byte, then iterates
 * over the sub groups and recursively calls itself on each subgroup to order
 * the next byte.
 */
static void int_radix_order_recurse(uint32_t* p_x,
                                    int* p_o,
                                    uint32_t* p_x_aux,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    R_xlen_t* p_counts,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos,
                                    const R_xlen_t size,
                                    const uint8_t pass) {
  // Exit as fast as possible if we are below the insertion order boundary
  if (size <= INSERTION_ORDER_BOUNDARY) {
    int_insertion_order(p_x, p_o, p_group_infos, size);
    return;
  }

  // Skip passes where our up front check told us that all bytes were the same
  uint8_t next_pass = pass + 1;
  R_xlen_t* p_counts_next_pass = p_counts + UINT8_MAX_SIZE;

  while (next_pass < INT_MAX_RADIX_PASS && p_skips[next_pass]) {
    ++next_pass;
    p_counts_next_pass += UINT8_MAX_SIZE;
  }

  const uint8_t radix = PASS_TO_RADIX(pass, INT_MAX_RADIX_PASS);
  const uint8_t shift = radix * 8;

  uint8_t byte = 0;

  // Histogram for this pass
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint32_t x_elt = p_x[i];

    byte = int_extract_uint32_byte(x_elt, shift);

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Fast check to see if all bytes were the same.
  // If so, skip this `pass` since we learned nothing.
  // No need to accumulate counts and iterate over chunks,
  // we know all others are zero.
  if (p_counts[byte] == size) {
    // Reset count for other group chunks
    p_counts[byte] = 0;

    // If we are already at the last pass, we are done
    if (next_pass == INT_MAX_RADIX_PASS) {
      groups_size_push(p_group_infos, size);
      return;
    }

    // Otherwise, recurse on next byte using the same `size` since
    // the group size hasn't changed
    int_radix_order_recurse(
      p_x,
      p_o,
      p_x_aux,
      p_o_aux,
      p_bytes,
      p_counts_next_pass,
      p_skips,
      p_group_infos,
      size,
      next_pass
    );

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

  R_xlen_t last_cumulative_count = 0;

  // Recurse on subgroups as required
  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const R_xlen_t cumulative_count = p_counts[i];

    if (!cumulative_count) {
      continue;
    }

    // Set to zero to clear for subsequent groups
    p_counts[i] = 0;

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
    if (next_pass == INT_MAX_RADIX_PASS) {
      groups_size_push(p_group_infos, group_size);
      p_x += group_size;
      p_o += group_size;
      continue;
    }

    // Order next byte of this subgroup
    int_radix_order_recurse(
      p_x,
      p_o,
      p_x_aux,
      p_o_aux,
      p_bytes,
      p_counts_next_pass,
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

/*
 * Do a parallel histogram run over all 4 passes to determine if any passes
 * can be skipped (because all bytes were the same)
 */
static uint8_t int_compute_skips(bool* p_skips, const uint32_t* p_x, R_xlen_t size) {
  uint8_t radix_start = PASS_TO_RADIX(0, INT_MAX_RADIX_PASS);
  uint8_t shift_start = radix_start * 8;

  for (uint8_t i = 0; i < INT_MAX_RADIX_PASS; ++i) {
    p_skips[i] = true;
  }

  uint8_t p_bytes[INT_MAX_RADIX_PASS];
  const uint32_t elt0 = p_x[0];

  // Get bytes of first element in MSD->LSD order.
  // Placed in `p_bytes` in a way that aligns with the `pass` variable
  for (uint8_t pass = 0, shift = shift_start; pass < INT_MAX_RADIX_PASS; ++pass, shift += SHIFT_ADJUSTMENT) {
    p_bytes[pass] = int_extract_uint32_byte(elt0, shift);
  }

  // Check to see which passes are skippable
  for (R_xlen_t i = 1; i < size; ++i) {
    uint8_t n_skips = INT_MAX_RADIX_PASS;
    const uint32_t elt = p_x[i];

    for (uint8_t pass = 0, shift = shift_start; pass < INT_MAX_RADIX_PASS; ++pass, shift += SHIFT_ADJUSTMENT) {
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
  while (pass < INT_MAX_RADIX_PASS && p_skips[pass]) {
    ++pass;
  }

  return pass;
}

// -----------------------------------------------------------------------------

// Bytes will be extracted 8 bits at a time.
// This is a MSB radix sort, so they are extracted MSB->LSB.
static inline uint8_t int_extract_uint32_byte(uint32_t x, uint8_t shift) {
  return (x >> shift) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

/*
 * Entry points for logical ordering. These just use integer infrastructure.
 */
static void lgl_order_chunk(void* p_x,
                            int* p_o,
                            struct lazy_vec* p_lazy_x_aux,
                            struct lazy_vec* p_lazy_o_aux,
                            struct lazy_vec* p_lazy_bytes,
                            struct lazy_vec* p_lazy_counts,
                            struct group_infos* p_group_infos,
                            bool decreasing,
                            bool na_last,
                            R_xlen_t size) {
  int_order_chunk(
    p_x,
    p_o,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    decreasing,
    na_last,
    size
  );
}

static void lgl_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  int_order(
    x,
    p_o,
    p_lazy_x_slice,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    decreasing,
    na_last,
    size
  );
}

// -----------------------------------------------------------------------------

static bool dbl_sorted(const double* p_x,
                       int* p_o,
                       struct group_infos* p_group_infos,
                       R_xlen_t size,
                       bool decreasing,
                       bool na_last);

static void dbl_adjust(void* p_x,
                       const bool decreasing,
                       const bool na_last,
                       const R_xlen_t size);

static void dbl_insertion_order(uint64_t* p_x,
                                int* p_o,
                                struct group_infos* p_group_infos,
                                const R_xlen_t size);

static void dbl_radix_order(uint64_t* p_x,
                            int* p_o,
                            uint64_t* p_x_aux,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            R_xlen_t* p_counts,
                            struct group_infos* p_group_infos,
                            const R_xlen_t size);

/*
 * These are the main entry points for double ordering. They are nearly
 * identical except `dbl_order()` assumes that `p_x` cannot be
 * modified directly and is user input.
 *
 * `dbl_order_chunk()` assumes `p_x` is modifiable by reference. It is called
 * when iterating over data frame columns and `p_x` is the 2nd or greater
 * column, in which case `p_x` is really a chunk of that column that has been
 * copied into `x_slice`.
 *
 * `dbl_order()` assumes `p_x` is user input which cannot be modified.
 * It copies `x` into another SEXP that can be modified directly.
 *
 * Unlike `int_order_chunk()`, there is no intermediate counting sort, as it is
 * sort of unclear how to compute the range of a double vector in the same
 * way, and even after adjusting to a `uint64_t`, it is unlikely that they
 * have a very small range of values.
 */
static void dbl_order_chunk(void* p_x,
                            int* p_o,
                            struct lazy_vec* p_lazy_x_aux,
                            struct lazy_vec* p_lazy_o_aux,
                            struct lazy_vec* p_lazy_bytes,
                            struct lazy_vec* p_lazy_counts,
                            struct group_infos* p_group_infos,
                            bool decreasing,
                            bool na_last,
                            R_xlen_t size) {
  if (dbl_sorted(p_x, p_o, p_group_infos, size, decreasing, na_last)) {
    return;
  }

  dbl_adjust(p_x, decreasing, na_last, size);

  if (size <= INSERTION_ORDER_BOUNDARY) {
    dbl_insertion_order(p_x, p_o, p_group_infos, size);
    return;
  }

  lazy_vec_initialize(p_lazy_x_aux);
  uint64_t* p_x_aux = (uint64_t*) p_lazy_x_aux->p_data;

  lazy_vec_initialize(p_lazy_o_aux);
  int* p_o_aux = (int*) p_lazy_o_aux->p_data;

  lazy_vec_initialize(p_lazy_bytes);
  uint8_t* p_bytes = (uint8_t*) p_lazy_bytes->p_data;

  lazy_vec_initialize(p_lazy_counts);
  R_xlen_t* p_counts = (R_xlen_t*) p_lazy_counts->p_data;
  memset(p_counts, 0, p_lazy_counts->size);

  dbl_radix_order(
    p_x,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos,
    size
  );
}

static void dbl_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  const double* p_x = REAL_RO(x);

  if (dbl_sorted(p_x, p_o, p_group_infos, size, decreasing, na_last)) {
    return;
  }

  lazy_vec_initialize(p_lazy_x_slice);
  void* p_x_slice = p_lazy_x_slice->p_data;

  memcpy(p_x_slice, p_x, size * sizeof(double));
  dbl_adjust(p_x_slice, decreasing, na_last, size);

  if (size <= INSERTION_ORDER_BOUNDARY) {
    dbl_insertion_order(p_x_slice, p_o, p_group_infos, size);
    return;
  }

  lazy_vec_initialize(p_lazy_x_aux);
  uint64_t* p_x_aux = (uint64_t*) p_lazy_x_aux->p_data;

  lazy_vec_initialize(p_lazy_o_aux);
  int* p_o_aux = (int*) p_lazy_o_aux->p_data;

  lazy_vec_initialize(p_lazy_bytes);
  uint8_t* p_bytes = (uint8_t*) p_lazy_bytes->p_data;

  lazy_vec_initialize(p_lazy_counts);
  R_xlen_t* p_counts = (R_xlen_t*) p_lazy_counts->p_data;
  memset(p_counts, 0, p_lazy_counts->size);

  dbl_radix_order(
    p_x_slice,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos,
    size
  );
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
 * dbl_map_to_uint64(NA_real_) -> UINT64_MAX (or 0 if `na_last = false`)
 * dbl_map_to_uint64(NaN) -> UINT64_MAX (or 0 if `na_last = false`)
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

/*
 * Map `double -> `uint64_t` retaining ordering.
 *
 * Assumes `x` is not a `NA_real_` or `NaN` value.
 * Correctly handles `Inf` and `-Inf`.
 */
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
// bits. Described more here: http://stereopsis.com/radix.html
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
                                    int* p_o,
                                    uint64_t* p_x_aux,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    R_xlen_t* p_counts,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos,
                                    const R_xlen_t size,
                                    const uint8_t pass);

/*
 * Double radix ordering entry point
 *
 * Expects that `dbl_adjust()` has been called on `p_x`, which takes care
 * of `na_last` and `decreasing` and also maps `double` to `uint64_t` once
 * up front so we don't have to do it for each radix pass.
 *
 * Sorts `p_x` and `p_o` in place
 */
static void dbl_radix_order(uint64_t* p_x,
                            int* p_o,
                            uint64_t* p_x_aux,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            R_xlen_t* p_counts,
                            struct group_infos* p_group_infos,
                            const R_xlen_t size) {
  bool p_skips[DBL_MAX_RADIX_PASS];

  uint8_t pass = dbl_compute_skips(p_skips, p_x, size);

  // Skipped all passes - Happens when `x` is 1 value repeated
  if (pass == DBL_MAX_RADIX_PASS) {
    groups_size_push(p_group_infos, size);
    return;
  }

  dbl_radix_order_recurse(
    p_x,
    p_o,
    p_x_aux,
    p_o_aux,
    p_bytes,
    p_counts,
    p_skips,
    p_group_infos,
    size,
    pass
  );
}

// -----------------------------------------------------------------------------

static inline uint8_t dbl_extract_uint64_byte(uint64_t x, uint8_t shift);

/*
 * Recursive function for radix ordering. Orders the current byte, then iterates
 * over the sub groups and recursively calls itself on each subgroup to order
 * the next byte.
 *
 * This needs 8 passes, unlike the 4 required by `int_radix_order()`.
 */
static void dbl_radix_order_recurse(uint64_t* p_x,
                                    int* p_o,
                                    uint64_t* p_x_aux,
                                    int* p_o_aux,
                                    uint8_t* p_bytes,
                                    R_xlen_t* p_counts,
                                    bool* p_skips,
                                    struct group_infos* p_group_infos,
                                    const R_xlen_t size,
                                    const uint8_t pass) {
  // Exit as fast as possible if we are below the insertion order boundary
  if (size <= INSERTION_ORDER_BOUNDARY) {
    dbl_insertion_order(p_x, p_o, p_group_infos, size);
    return;
  }

  // Skip passes where our up front check told us that all bytes were the same
  uint8_t next_pass = pass + 1;
  R_xlen_t* p_counts_next_pass = p_counts + UINT8_MAX_SIZE;

  while (next_pass < DBL_MAX_RADIX_PASS && p_skips[next_pass]) {
    ++next_pass;
    p_counts_next_pass += UINT8_MAX_SIZE;
  }

  const uint8_t radix = PASS_TO_RADIX(pass, DBL_MAX_RADIX_PASS);
  const uint8_t shift = radix * 8;

  uint8_t byte = 0;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint64_t x_elt = p_x[i];

    byte = dbl_extract_uint64_byte(x_elt, shift);

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Fast check to see if all bytes were the same.
  // If so, skip this `pass` since we learned nothing.
  // No need to accumulate counts and iterate over chunks,
  // we know all others are zero.
  if (p_counts[byte] == size) {
    // Reset count for other group chunks
    p_counts[byte] = 0;

    // If we are already at the last pass, we are done
    if (next_pass == DBL_MAX_RADIX_PASS) {
      groups_size_push(p_group_infos, size);
      return;
    }

    // Otherwise, recurse on next byte using the same `size` since
    // the group size hasn't changed
    dbl_radix_order_recurse(
      p_x,
      p_o,
      p_x_aux,
      p_o_aux,
      p_bytes,
      p_counts_next_pass,
      p_skips,
      p_group_infos,
      size,
      next_pass
    );

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

  R_xlen_t last_cumulative_count = 0;

  // Recurse on subgroups as required
  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const R_xlen_t cumulative_count = p_counts[i];

    if (!cumulative_count) {
      continue;
    }

    p_counts[i] = 0;

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
    if (next_pass == DBL_MAX_RADIX_PASS) {
      groups_size_push(p_group_infos, group_size);
      p_x += group_size;
      p_o += group_size;
      continue;
    }

    // Order next byte of this subgroup
    dbl_radix_order_recurse(
      p_x,
      p_o,
      p_x_aux,
      p_o_aux,
      p_bytes,
      p_counts_next_pass,
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
  uint8_t radix_start = PASS_TO_RADIX(0, DBL_MAX_RADIX_PASS);
  uint8_t shift_start = radix_start * 8;

  for (uint8_t i = 0; i < DBL_MAX_RADIX_PASS; ++i) {
    p_skips[i] = true;
  }

  uint8_t p_bytes[DBL_MAX_RADIX_PASS];
  const uint64_t elt0 = p_x[0];

  // Get bytes of first element in MSD->LSD order.
  // Placed in `p_bytes` in a way that aligns with the `pass` variable
  for (uint8_t pass = 0, shift = shift_start; pass < DBL_MAX_RADIX_PASS; ++pass, shift += SHIFT_ADJUSTMENT) {
    p_bytes[pass] = dbl_extract_uint64_byte(elt0, shift);
  }

  // Check to see which passes are skippable
  for (R_xlen_t i = 1; i < size; ++i) {
    uint8_t n_skips = DBL_MAX_RADIX_PASS;
    const uint64_t elt = p_x[i];

    for (uint8_t pass = 0, shift = shift_start; pass < DBL_MAX_RADIX_PASS; ++pass, shift += SHIFT_ADJUSTMENT) {
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
  while (pass < DBL_MAX_RADIX_PASS && p_skips[pass]) {
    ++pass;
  }

  return pass;
}

// -----------------------------------------------------------------------------

// Bytes will be extracted 8 bits at a time.
// This is a MSB radix sort, so they are extracted MSB->LSB.
static inline uint8_t dbl_extract_uint64_byte(uint64_t x, uint8_t shift) {
  return (x >> shift) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

/*
 * `cpl_order()` uses the fact that Rcomplex is really just a rcrd
 * type of two double vectors. It orders first on the real vector, and then on
 * the imaginary vector.
 *
 * `cpl_order_chunk()` isn't required. It would only be called from data frames
 * when there is a complex column, but in those cases we split the column
 * into two double vectors (real / imaginary) and "rerun" the column using
 * `dbl_order_chunk()`.
 */
static void cpl_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
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
  // `lazy_x_slice` and `lazy_x_aux` are created to have the
  // size of a double vector.
  lazy_vec_initialize(p_lazy_x_slice);
  double* p_x_slice_dbl = (double*) p_lazy_x_slice->p_data;

  // Handle the real portion first
  for (R_xlen_t i = 0; i < size; ++i) {
    p_x_slice_dbl[i] = p_x_cpl[i].r;
  }

  // Run it through double ordering
  dbl_order_chunk(
    p_x_slice_dbl,
    p_o,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
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
  // Uses updated ordering to place it in sequential order.
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

    dbl_order_chunk(
      p_x_slice_dbl,
      p_o,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
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

static bool chr_sorted(const SEXP* p_x,
                       int* p_o,
                       struct group_infos* p_group_infos,
                       R_xlen_t size,
                       bool decreasing,
                       bool na_last);

static void chr_mark_sorted_uniques(const SEXP* p_x,
                                    struct lazy_vec* p_lazy_x_aux,
                                    struct lazy_vec* p_lazy_bytes,
                                    struct truelength_info* p_truelength_info,
                                    R_xlen_t size);

static inline void chr_extract_ordering(int* p_x_aux, SEXP* p_x, R_xlen_t size);

static void chr_radix_order(SEXP* p_x,
                            SEXP* p_x_aux,
                            int* p_sizes,
                            int* p_sizes_aux,
                            uint8_t* p_bytes,
                            const R_xlen_t size,
                            const R_len_t max_size);

/*
 * These are the main entry points for character ordering.
 *
 * `chr_order_chunk()` assumes `p_x` is modifiable by reference. It also
 * assumes that `chr_mark_sorted_uniques()` has already been called. For data
 * frame columns where `chr_order_chunk()` is called on each group chunk,
 * `chr_mark_sorted_uniques()` is only called once on the entire column. It
 * also assumes that `p_x` has already been re-encoded as UTF-8 if required.
 *
 * `chr_order()` assumes `x` is user input which cannot be modified.
 * It copies `x` into another SEXP that can be modified directly and re-encodes
 * as UTF-8 if required.
 *
 * `chr_order_chunk()` essentially calls `int_order_chunk()`, however we can't
 * call it directly because we don't have access to all the required arguments.
 * Specifically we reuse `p_x` here as the auxiliary data structure for integer
 * ordering, but to call `int_order_chunk()` we would need the lazy wrapper
 * for it.
 *
 * Because these functions modify TRUELENGTHs, we have to reset them on the
 * way out. `chr_order()` does it directly, but `chr_order_chunk()` relies
 * on `df_order()` to do it after the entire column is processed. It is
 * important to not error inside these functions because the TRUELENGTHs won't
 * be reset if we do.
 */
static void chr_order_chunk(void* p_x,
                            int* p_o,
                            struct lazy_vec* p_lazy_x_aux,
                            struct lazy_vec* p_lazy_o_aux,
                            struct lazy_vec* p_lazy_bytes,
                            struct lazy_vec* p_lazy_counts,
                            struct group_infos* p_group_infos,
                            bool decreasing,
                            bool na_last,
                            R_xlen_t size) {
  if (chr_sorted(p_x, p_o, p_group_infos, size, decreasing, na_last)) {
    return;
  }

  lazy_vec_initialize(p_lazy_x_aux);
  void* p_x_aux = p_lazy_x_aux->p_data;

  // Move integer ordering into `p_x_aux`.
  // `p_x_aux` is allocated as the larger of `int` and `SEXP*`.
  chr_extract_ordering(p_x_aux, p_x, size);

  if (size <= INSERTION_ORDER_BOUNDARY) {
    int_adjust(p_x_aux, decreasing, na_last, size);
    int_insertion_order(p_x_aux, p_o, p_group_infos, size);
    return;
  }

  lazy_vec_initialize(p_lazy_o_aux);
  int* p_o_aux = (int*) p_lazy_o_aux->p_data;

  uint32_t range;
  int x_min;

  int_compute_range(p_x_aux, size, &x_min, &range);

  if (range < INT_COUNTING_ORDER_RANGE_BOUNDARY) {
    int_counting_order(
      p_x_aux,
      p_o,
      p_o_aux,
      p_group_infos,
      size,
      x_min,
      range,
      decreasing,
      na_last
    );

    return;
  }

  lazy_vec_initialize(p_lazy_bytes);
  uint8_t* p_bytes = (uint8_t*) p_lazy_bytes->p_data;

  lazy_vec_initialize(p_lazy_counts);
  R_xlen_t* p_counts = (R_xlen_t*) p_lazy_counts->p_data;
  memset(p_counts, 0, p_lazy_counts->size);

  int_adjust(p_x_aux, decreasing, na_last, size);

  // Reuse `p_x`, which was a `SEXP*` to the CHARSXPs but is no longer required.
  // We ensure that this is as large as an `int` when originally allocating.
  int_radix_order(
    p_x_aux,
    p_o,
    p_x,
    p_o_aux,
    p_bytes,
    p_counts,
    p_group_infos,
    size
  );
}

static void chr_copy_with_reencode(SEXP* p_x_slice, const SEXP* p_x, R_xlen_t size);

static void chr_order(SEXP x,
                      int* p_o,
                      struct lazy_vec* p_lazy_x_slice,
                      struct lazy_vec* p_lazy_x_aux,
                      struct lazy_vec* p_lazy_o_aux,
                      struct lazy_vec* p_lazy_bytes,
                      struct lazy_vec* p_lazy_counts,
                      struct group_infos* p_group_infos,
                      struct truelength_info* p_truelength_info,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  const SEXP* p_x = STRING_PTR_RO(x);

  if (chr_sorted(p_x, p_o, p_group_infos, size, decreasing, na_last)) {
    return;
  }

  lazy_vec_initialize(p_lazy_x_slice);
  void* p_x_slice = p_lazy_x_slice->p_data;

  chr_copy_with_reencode(p_x_slice, p_x, size);

  // Place sorted and marked uniques in `p_truelength_info`
  chr_mark_sorted_uniques(
    p_x_slice,
    p_lazy_x_aux,
    p_lazy_bytes,
    p_truelength_info,
    size
  );

  chr_order_chunk(
    p_x_slice,
    p_o,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    decreasing,
    na_last,
    size
  );

  // Reset TRUELENGTHs
  truelength_reset(p_truelength_info);
}

// -----------------------------------------------------------------------------

/*
 * Pull ordering off of marked `p_x` and place it into `p_x_aux` working memory.
 * We mark the CHARSXP TRUELENGTHs with negative ordering to be different from
 * what R might use, so that gets reversed here to get the true ordering back.
 */
static inline void chr_extract_ordering(int* p_x_aux, SEXP* p_x, R_xlen_t size) {
  for (R_xlen_t i = 0; i < size; ++i) {
    SEXP elt = p_x[i];

    if (elt == NA_STRING) {
      p_x_aux[i] = NA_INTEGER;
      continue;
    }

    // Negative to flip where we set the order using a negative value.
    // Cast to `int` because `TRUELENGTH()` returns a `R_xlen_t`.
    p_x_aux[i] = (int) -TRUELENGTH(elt);
  }
}

// -----------------------------------------------------------------------------

/*
 * `chr_mark_sorted_uniques()` runs through the strings in `p_x` and places the
 * unique strings in `p_truelength_info->p_uniques`. It marks the unique ones
 * with a negative TRUELENGTH as it goes. Since identical strings share the
 * same CHARSXP, this marks all strings in the vector at once.
 *
 * After detecting all unique strings, it sorts them in place with
 * `chr_radix_order()`.
 *
 * Finally, it loops over the now sorted unique strings and marks them with
 * their ordering (as a negative value). This allows `chr_order_chunk()` to loop
 * through `p_x` and just pluck off the TRUELENGTH value, which will be an
 * integer proxy for the value's ordering.
 *
 * `truelength_save()` also saves the unique strings and their original
 * TRUELENGTH values so they can be reset after each column with
 * `truelength_reset()`.
 */
static void chr_mark_sorted_uniques(const SEXP* p_x,
                                    struct lazy_vec* p_lazy_x_aux,
                                    struct lazy_vec* p_lazy_bytes,
                                    struct truelength_info* p_truelength_info,
                                    R_xlen_t size) {
  R_xlen_t max_size = 0;

  for (R_xlen_t i = 0; i < size; ++i) {
    SEXP elt = p_x[i];

    // These are replaced by `NA_INTEGER` for use in `int_order_chunk()`
    if (elt == NA_STRING) {
      continue;
    }

    R_xlen_t truelength = TRUELENGTH(elt);

    // We have already seen and saved this string
    if (truelength < 0) {
      continue;
    }

    R_xlen_t elt_size = Rf_xlength(elt);

    // Track max string size to know how deep to recurse
    if (max_size < elt_size) {
      max_size = elt_size;
    }

    // Save the truelength so we can reset it later.
    // Also saves this unique value so we can order uniques.
    truelength_save(p_truelength_info, elt, truelength, elt_size);

    // Mark as negative to note that we have seen this string
    // (R uses positive or zero truelengths)
    SET_TRUELENGTH(elt, -1);
  }

  lazy_vec_initialize(p_lazy_x_aux);
  SEXP* p_x_aux = (SEXP*) p_lazy_x_aux->p_data;

  lazy_vec_initialize(p_lazy_bytes);
  uint8_t* p_bytes = (uint8_t*) p_lazy_bytes->p_data;

  R_xlen_t n_uniques = p_truelength_info->size_used;

  // Sorts uniques in ascending order using `p_x_aux` for working memory.
  // Assumes no `NA`!
  chr_radix_order(
    p_truelength_info->p_uniques,
    p_x_aux,
    p_truelength_info->p_sizes,
    p_truelength_info->p_sizes_aux,
    p_bytes,
    n_uniques,
    max_size
  );

  // Mark unique sorted strings with their order.
  // Use a negative value to differentiate with R.
  for (R_xlen_t i = 0; i < n_uniques; ++i) {
    SEXP elt = p_truelength_info->p_uniques[i];
    SET_TRUELENGTH(elt, -i - 1);
  }
}

// -----------------------------------------------------------------------------

static bool chr_str_ge(SEXP x, SEXP y, int x_size, const R_len_t pass);

/*
 * Insertion order for character vectors. This occurs in the radix ordering
 * once we drop below a certain chunk size.
 *
 * One optimization done here is to take advantage of the `pass` info, which
 * will indicate that all characters before this pass are identical already
 * and don't need to be checked by `strcmp()`.
 */
static void chr_insertion_order(SEXP* p_x,
                                int* p_sizes,
                                const R_xlen_t size,
                                const R_len_t pass) {
  // Don't think this can occur, but safer this way
  if (size == 0) {
    return;
  }

  for (R_xlen_t i = 1; i < size; ++i) {
    const SEXP x_elt = p_x[i];
    const int x_size = p_sizes[i];

    R_xlen_t j = i - 1;

    while (j >= 0) {
      const SEXP x_cmp_elt = p_x[j];

      if (chr_str_ge(x_elt, x_cmp_elt, x_size, pass)) {
        break;
      }

      int x_cmp_size = p_sizes[j];

      // Swap
      p_x[j + 1] = x_cmp_elt;
      p_sizes[j + 1] = x_cmp_size;

      // Next
      --j;
    }

    // Place original elements in new location
    // closer to start of the vector
    p_x[j + 1] = x_elt;
    p_sizes[j + 1] = x_size;
  }
}

// -----------------------------------------------------------------------------

static void chr_radix_order_recurse(SEXP* p_x,
                                    SEXP* p_x_aux,
                                    int* p_sizes,
                                    int* p_sizes_aux,
                                    uint8_t* p_bytes,
                                    const R_xlen_t size,
                                    const R_len_t pass,
                                    const R_len_t max_size);

/*
 * Entry point for radix ordering of characters.
 *
 * This is different from with integers / doubles because:
 * - `p_x` will contain only unique strings
 * - `p_x` will not contain any `NA` strings
 * - We just need to sort `p_x` in place, no need to track group information,
 *   which is instead done by `int_order_chunk()` later
 * - The number of passes is variable here, because strings have a variable
 *   length.
 * - We also track the character sizes because repeated `Rf_xlength()` calls
 *   can get expensive over just indexing into the array.
 */
static void chr_radix_order(SEXP* p_x,
                            SEXP* p_x_aux,
                            int* p_sizes,
                            int* p_sizes_aux,
                            uint8_t* p_bytes,
                            const R_xlen_t size,
                            const R_len_t max_size) {
  R_len_t pass = 0;

  chr_radix_order_recurse(
    p_x,
    p_x_aux,
    p_sizes,
    p_sizes_aux,
    p_bytes,
    size,
    pass,
    max_size
  );
}

// -----------------------------------------------------------------------------

static void chr_radix_order_pass(SEXP* p_x,
                                 SEXP* p_x_aux,
                                 int* p_sizes,
                                 int* p_sizes_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 const R_xlen_t size,
                                 const R_len_t pass);

static void chr_radix_order_recurse(SEXP* p_x,
                                    SEXP* p_x_aux,
                                    int* p_sizes,
                                    int* p_sizes_aux,
                                    uint8_t* p_bytes,
                                    const R_xlen_t size,
                                    const R_len_t pass,
                                    const R_len_t max_size) {
  R_xlen_t p_counts[UINT8_MAX_SIZE] = { 0 };

  chr_radix_order_pass(
    p_x,
    p_x_aux,
    p_sizes,
    p_sizes_aux,
    p_bytes,
    p_counts,
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
      ++p_x;
      ++p_sizes;
      continue;
    }

    if (next_pass == max_size) {
      p_x += group_size;
      p_sizes += group_size;
      continue;
    }

    // Order next byte of this subgroup
    chr_radix_order_recurse(
      p_x,
      p_x_aux,
      p_sizes,
      p_sizes_aux,
      p_bytes,
      group_size,
      next_pass,
      max_size
    );

    p_x += group_size;
    p_sizes += group_size;
  }
}

// -----------------------------------------------------------------------------

/*
 * Order the `pass + 1`-th character of the `p_x` strings.
 *
 * For ASCII strings, 1 character aligns with 1 byte, so we can order them
 * 1 character at a time from left to right (MSB to LSB).
 *
 * For UTF-8 strings, the implementation of UTF-8 is done so that UTF-8
 * characters are made up of between 1-4 bytes. Luckily, treating them as
 * a sequence of single bytes like we do for ASCII orders identically to
 * treating them as their full 1-4 byte sequence.
 *
 * Because these are variable length, some strings are shorter than others.
 * Shorter strings should order lower than longer strings if they are otherwise
 * equivalent, so we reserve the 0-th bucket of `p_counts` for counting
 * implicit empty strings. Normally this would be an issue because this is
 * the bucket for ASCII value 0, but this is the null value, which is not
 * allowed in R strings!
 *
 * Additionally, we don't have to worry about having an `NA` bucket because
 * there will be no missing values in the unique set.
 */
static void chr_radix_order_pass(SEXP* p_x,
                                 SEXP* p_x_aux,
                                 int* p_sizes,
                                 int* p_sizes_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 const R_xlen_t size,
                                 const R_len_t pass) {
  if (size <= INSERTION_ORDER_BOUNDARY) {
    chr_insertion_order(p_x, p_sizes, size, pass);
    return;
  }

  uint8_t byte = 0;

  // NA values won't be in `p_x` so we can reserve the 0th bucket for ""
  const uint8_t missing_bucket = 0;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const R_len_t x_elt_size = p_sizes[i];

    // Check if there are characters left in the string and extract the next
    // one if so, otherwise assume implicit "".
    if (pass < x_elt_size) {
      const SEXP x_elt = p_x[i];
      const char* c_x_elt = CHAR(x_elt);
      byte = (uint8_t) c_x_elt[pass];
    } else {
      byte = missing_bucket;
    }

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

    // Insert current cumulative value, then increment
    p_counts[i] = cumulative;
    cumulative += count;
  }

  // Place into auxiliary arrays in the correct order, then copy back over
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint8_t byte = p_bytes[i];
    const R_xlen_t loc = p_counts[byte]++;
    p_x_aux[loc] = p_x[i];
    p_sizes_aux[loc] = p_sizes[i];
  }

  // Copy back over
  memcpy(p_x, p_x_aux, size * sizeof(SEXP));
  memcpy(p_sizes, p_sizes_aux, size * sizeof(int));
}

// -----------------------------------------------------------------------------

/*
 * Check if `x` is greater than `y` lexicographically in a C-locale.
 *
 * - `x` and `y` are guaranteed to be different and not `NA`, so we don't gain
 *   anything from pointer comparisons.
 *
 * - This is called from `chr_insertion_order()` from inside the radix ordering,
 *   so we can use information about the current `pass` to only compare
 *   characters that are actually different.
 */
static bool chr_str_ge(SEXP x, SEXP y, int x_size, const R_len_t pass) {
  // Pure insertion sort - we know nothing yet
  if (pass == 0) {
    const char* c_x = CHAR(x);
    const char* c_y = CHAR(y);

    int cmp = strcmp(c_x, c_y);
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

  int cmp = strcmp(c_x, c_y);
  return cmp >= 0;
}

// -----------------------------------------------------------------------------

/*
 * Copy from `p_x` to `p_x_slice`. Also re-encodes as UTF-8 if any strings are
 * not UTF-8 or ASCII. Most things are ASCII, so this should short circuit
 * quickly after the first check in `CHAR_NEEDS_REENCODE()`.
 */
static void chr_copy_with_reencode(SEXP* p_x_slice, const SEXP* p_x, R_xlen_t size) {
  const void* vmax = vmaxget();

  for (R_xlen_t i = 0; i < size; ++i) {
    SEXP elt = p_x[i];

    if (CHAR_NEEDS_REENCODE(elt)) {
      p_x_slice[i] = Rf_mkCharCE(Rf_translateCharUTF8(elt), CE_UTF8);
    } else {
      p_x_slice[i] = elt;
    }
  }

  vmaxset(vmax);
}

// -----------------------------------------------------------------------------

static void vec_order_chunk_switch(void* p_x,
                                   int* p_o,
                                   struct lazy_vec* p_lazy_x_aux,
                                   struct lazy_vec* p_lazy_o_aux,
                                   struct lazy_vec* p_lazy_bytes,
                                   struct lazy_vec* p_lazy_counts,
                                   struct group_infos* p_group_infos,
                                   struct truelength_info* p_truelength_info,
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

#define DF_ORDER_EXTRACT_CHUNK_CHR() do {                      \
  const SEXP* p_col = STRING_PTR_RO(col);                      \
  SEXP* p_x_slice_col = (SEXP*) p_x_slice;                     \
                                                               \
  const void* vmax = vmaxget();                                \
                                                               \
  /* Extract and reencode to UTF-8 as needed */                \
  for (R_xlen_t j = 0; j < group_size; ++j) {                  \
    const int loc = p_o_col[j] - 1;                            \
                                                               \
    SEXP elt = p_col[loc];                                     \
                                                               \
    if (CHAR_NEEDS_REENCODE(elt)) {                            \
      p_x_slice_col[j] = CHAR_REENCODE(elt);                   \
    } else {                                                   \
      p_x_slice_col[j] = elt;                                  \
    }                                                          \
  }                                                            \
                                                               \
  vmaxset(vmax);                                               \
} while (0)

/*
 * `df_order()` is the main user of `p_group_infos`. It uses the grouping
 * of the current column to break up the next column into sub groups. That
 * process is continued until either all columns have been processed or we
 * can tell all of the values apart.
 */
static void df_order(SEXP x,
                     int* p_o,
                     struct lazy_vec* p_lazy_x_slice,
                     struct lazy_vec* p_lazy_x_aux,
                     struct lazy_vec* p_lazy_o_aux,
                     struct lazy_vec* p_lazy_bytes,
                     struct lazy_vec* p_lazy_counts,
                     struct group_infos* p_group_infos,
                     struct truelength_info* p_truelength_info,
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
      "Internal error: `vec_order_check_decreasing()` should expand "
      "`decreasing` to have length 1 or length equal "
      "to the number of columns of `x` after calling `vec_proxy_compare()`."
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
  vec_order_base_switch(
    col,
    p_o,
    p_lazy_x_slice,
    p_lazy_x_aux,
    p_lazy_o_aux,
    p_lazy_bytes,
    p_lazy_counts,
    p_group_infos,
    p_truelength_info,
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

    // Reset pointer between columns since we increment it as
    // we iterate through the groups, but need it to start from the beginning
    // on the next column
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

    // Pre sort unique characters once for the whole column
    if (type == vctrs_type_character) {
      const SEXP* p_col = STRING_PTR_RO(col);

      chr_mark_sorted_uniques(
        p_col,
        p_lazy_x_aux,
        p_lazy_bytes,
        p_truelength_info,
        size
      );
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

    // Ensure `x_slice` is initialized to hold chunks
    lazy_vec_initialize(p_lazy_x_slice);
    void* p_x_slice = p_lazy_x_slice->p_data;

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
      case vctrs_type_character: DF_ORDER_EXTRACT_CHUNK_CHR(); break;
      default: Rf_errorcall(R_NilValue, "Unknown data frame column type in `vec_order()`.");
      }

      vec_order_chunk_switch(
        p_x_slice,
        p_o_col,
        p_lazy_x_aux,
        p_lazy_o_aux,
        p_lazy_bytes,
        p_lazy_counts,
        p_group_infos,
        p_truelength_info,
        col_decreasing,
        na_last,
        group_size,
        type
      );

      p_o_col += group_size;
    }

    // Reset TRUELENGTHs between columns
    if (type == vctrs_type_character) {
      truelength_reset(p_truelength_info);
    }
  }
}

#undef DF_ORDER_EXTRACT_CHUNK
#undef DF_ORDER_EXTRACT_CHUNK_CPL
#undef DF_ORDER_EXTRACT_CHUNK_CHR

// -----------------------------------------------------------------------------

/*
 * Switch function specifically for column chunks generated when
 * processing a data frame
 */
static void vec_order_chunk_switch(void* p_x,
                                   int* p_o,
                                   struct lazy_vec* p_lazy_x_aux,
                                   struct lazy_vec* p_lazy_o_aux,
                                   struct lazy_vec* p_lazy_bytes,
                                   struct lazy_vec* p_lazy_counts,
                                   struct group_infos* p_group_infos,
                                   struct truelength_info* p_truelength_info,
                                   bool decreasing,
                                   bool na_last,
                                   R_xlen_t size,
                                   const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer: {
    int_order_chunk(
      p_x,
      p_o,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_logical: {
    lgl_order_chunk(
      p_x,
      p_o,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_double: {
    dbl_order_chunk(
      p_x,
      p_o,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_complex: {
    // Complex types are run in two passes, once over real then over imaginary
    dbl_order_chunk(
      p_x,
      p_o,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_character: {
    chr_order_chunk(
      p_x,
      p_o,
      p_lazy_x_aux,
      p_lazy_o_aux,
      p_lazy_bytes,
      p_lazy_counts,
      p_group_infos,
      decreasing,
      na_last,
      size
    );

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

static inline size_t df_size_multiplier(SEXP x);

/*
 * Compute the minimum size required for `lazy_x_aux` and `lazy_x_slice`.
 *
 * For complex, we split the vector into two double vectors. We only need to
 * allocate 1 double vector though, and it will be reused for both the real
 * and imaginary parts.
 */
static inline size_t vec_order_size_multiplier(SEXP x, const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer:
  case vctrs_type_logical:
    return sizeof(int);
  case vctrs_type_double:
    return sizeof(double);
  case vctrs_type_complex:
    // Complex types will be split into two double vectors
    return sizeof(double);
  case vctrs_type_character:
    // Auxiliary data will store SEXP and ints, so return the larger
    return sizeof(SEXP) > sizeof(int) ? sizeof(SEXP) : sizeof(int);
  case vctrs_type_dataframe:
    return df_size_multiplier(x);
  default:
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`.");
  }
}

// `x` should be a flattened df with no df-cols
static inline size_t df_size_multiplier(SEXP x) {
  R_xlen_t n_cols = Rf_xlength(x);

  size_t multiplier = 0;

  for (R_xlen_t i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    const enum vctrs_type type = vec_proxy_typeof(col);

    size_t col_multiplier = vec_order_size_multiplier(col, type);

    if (col_multiplier > multiplier) {
      multiplier = col_multiplier;
    }
  }

  return multiplier;
}

// -----------------------------------------------------------------------------

static inline size_t df_counts_multiplier(SEXP x);

/*
 * Compute the minimum size required for `p_counts`
 *
 * - For integer, we use 4 passes.
 * - For double, we use 8 passes.
 * - Logical uses integer radix sorting.
 * - Character uses integer radix sorting.
 * - Complex uses double radix sorting.
 */
static inline size_t vec_order_counts_multiplier(SEXP x, const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer:
  case vctrs_type_logical:
  case vctrs_type_character:
    return INT_MAX_RADIX_PASS;
  case vctrs_type_double:
  case vctrs_type_complex:
    return DBL_MAX_RADIX_PASS;
  case vctrs_type_dataframe:
    return df_counts_multiplier(x);
  default:
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_order()`.");
  }
}

// `x` should be a flattened df with no df-cols
static inline size_t df_counts_multiplier(SEXP x) {
  R_xlen_t n_cols = Rf_xlength(x);

  size_t multiplier = 0;

  for (R_xlen_t i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    const enum vctrs_type type = vec_proxy_typeof(col);

    size_t col_multiplier = vec_order_counts_multiplier(col, type);

    if (col_multiplier > multiplier) {
      multiplier = col_multiplier;
    }
  }

  return multiplier;
}

// -----------------------------------------------------------------------------

static SEXP df_check_decreasing(SEXP x, SEXP decreasing);

/*
 * `vec_order_check_decreasing()` checks the type and length of `decreasing`,
 * and possibly expands it.
 *
 * `x` is expected to be the original input, before `vec_proxy_compare()` is
 * called on it.
 *
 * If `x` is not a data frame, `decreasing` must be a boolean value. If
 * `x` is something like a rcrd type with a multi-column data frame proxy,
 * then restricting to a boolean `decreasing` is correct, and works because
 * the single decreasing value will be recycled across the columns.
 *
 * If `x` is a data frame, and `decreasing` is size 1, we return it untouched
 * and it will be recycled correctly.
 *
 * If `x` is a data frame and the size of `decreasing` matches the number of
 * columns of `x`, we have to be careful to "expand" `decreasing` to match
 * the number of columns of `x` that will exist after `vec_proxy_compare()`
 * is called. It flattens df-cols which might either already exist in `x`,
 * or may arise from rcrd columns that have data frame proxies. The majority
 * of the code here is for tracking this expansion.
 */
static SEXP vec_order_check_decreasing(SEXP x, SEXP decreasing) {
  // Don't check length here. This might be vectorized if `x` is a data frame.
  if (TYPEOF(decreasing) != LGLSXP) {
    Rf_errorcall(R_NilValue, "`decreasing` must be logical");
  }
  if (lgl_any_na(decreasing)) {
    Rf_errorcall(R_NilValue, "`decreasing` must not contain missing values.");
  }

  if (is_data_frame(x)) {
    return df_check_decreasing(x, decreasing);
  }

  if (Rf_xlength(decreasing) != 1) {
    Rf_errorcall(R_NilValue, "`decreasing` must be a single `TRUE` or `FALSE` when `x` is not a data frame.");
  }

  return decreasing;
}

static SEXP df_expand_decreasing(SEXP x, SEXP decreasing, R_xlen_t n_cols);

static SEXP df_check_decreasing(SEXP x, SEXP decreasing) {
  R_xlen_t n_decreasing = Rf_xlength(decreasing);
  R_xlen_t n_cols = Rf_xlength(x);

  // It will be recycled correctly even if columns get flattened
  if (n_decreasing == 1) {
    return decreasing;
  }

  // Must start out with the same length as the number of columns
  if (n_decreasing != n_cols) {
    Rf_errorcall(
      R_NilValue,
      "`decreasing` should have length 1 or length equal to the number of "
      "columns of `x` when `x` is a data frame."
    );
  }

  return df_expand_decreasing(x, decreasing, n_cols);
}


static int vec_decreasing_expansion(SEXP x);

static SEXP df_expand_decreasing(SEXP x, SEXP decreasing, R_xlen_t n_cols) {
  SEXP expansions = PROTECT(Rf_allocVector(INTSXP, n_cols));
  int* p_expansions = INTEGER(expansions);

  int size = 0;
  bool needs_expansion = false;

  // Compute expansion factor
  for (R_xlen_t i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    int expansion = vec_decreasing_expansion(col);

    if (expansion != 1) {
      needs_expansion = true;
    }

    p_expansions[i] = expansion;
    size += expansion;
  }

  if (!needs_expansion) {
    UNPROTECT(1);
    return decreasing;
  }

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));
  int* p_out = LOGICAL(out);

  int* p_decreasing = LOGICAL(decreasing);

  int k = 0;

  // Fill `out` with repeated `decreasing` values to match expanded size
  for (R_xlen_t i = 0; i < n_cols; ++i) {
    int col_decreasing = p_decreasing[i];
    int expansion = p_expansions[i];

    for (R_xlen_t j = 0; j < expansion; ++j) {
      p_out[k] = col_decreasing;
      ++k;
    }
  }

  UNPROTECT(2);
  return out;
}


static int df_decreasing_expansion(SEXP x);

static int vec_decreasing_expansion(SEXP x) {
  // Bare columns
  if (!OBJECT(x)) {
    return 1;
  }

  // Compute number of cols in df-cols,
  // and do proxy-compare on the cols as needed
  if (is_data_frame(x)) {
    return df_decreasing_expansion(x);
  }

  int expansion;

  // Otherwise we have an S3 column that could have a data frame
  // comparison proxy containing multiple columns, so we need to check for that
  SEXP proxy = PROTECT(vec_proxy_compare(x));

  // If the `proxy` is a data frame, the expansion factor is the
  // number of columns. Otherwise it is 1.
  if (is_data_frame(proxy)) {
    expansion = Rf_length(proxy);
  } else {
    expansion = 1;
  }

  UNPROTECT(1);
  return expansion;
}

// 0-col df-cols get dropped from the comparison proxy, so returning `0` here
// when a df-col has no columns should be correct
static int df_decreasing_expansion(SEXP x) {
  R_xlen_t n_cols = Rf_xlength(x);

  int out = 0;

  // Accumulate the expansion factors of the cols of the df-col
  for (R_xlen_t i = 0; i < n_cols; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    out += vec_decreasing_expansion(col);
  }

  return out;
}

// -----------------------------------------------------------------------------

static inline void ord_reverse(int* p_o, R_xlen_t size);

static inline int dbl_cmp(double x,
                          double y,
                          const int direction,
                          const int na_order);

/*
 * Check if a double vector is ordered, handling `decreasing` and `na_last`
 *
 * If the double vector is in the expected ordering, no sorting needs to
 * occur. In these cases, if `p_x` is in exactly the expected ordering, `p_o`
 * is unmodified. If `p_x` is in exactly the opposite ordering, then `p_o`
 * is reversed (this only happens if it is strictly opposite of expected
 * ordering, ties would prevent the reversal from being stable). Group
 * information is also pushed in these cases for use in the next columns.
 */
static bool dbl_sorted(const double* p_x,
                       int* p_o,
                       struct group_infos* p_group_infos,
                       R_xlen_t size,
                       bool decreasing,
                       bool na_last) {
  if (size == 0) {
    return true;
  }

  if (size == 1) {
    groups_size_push(p_group_infos, 1);
    return true;
  }

  const int direction = decreasing ? -1 : 1;
  const int na_order = na_last ? 1 : -1;

  double previous = p_x[0];

  R_xlen_t count = 0;

  // Check for strictly opposite of expected order
  // (ties are not allowed so we can reverse the vector stably)
  for (R_xlen_t i = 1; i < size; ++i, ++count) {
    double current = p_x[i];

    int cmp = dbl_cmp(
      current,
      previous,
      direction,
      na_order
    );

    if (cmp >= 0) {
      break;
    }

    previous = current;
  }

  // Was in strictly opposite of expected order.
  if (count == size - 1) {
    ord_reverse(p_o, size);

    // Each group is size 1 since this is strict ordering
    for (R_xlen_t j = 0; j < size; ++j) {
      groups_size_push(p_group_infos, 1);
    }

    return true;
  }

  // Was partially in expected order. Need to sort.
  if (count != 0) {
    return false;
  }

  // Retain the original `n_groups` to be able to reset the group sizes if
  // it turns out we don't have expected ordering
  struct group_info* p_group_info = groups_current(p_group_infos);
  R_xlen_t original_n_groups = p_group_info->n_groups;

  R_xlen_t group_size = 1;

  // Check for expected ordering - allowing ties since we don't have to
  // reverse the ordering.
  for (R_xlen_t i = 1; i < size; ++i) {
    double current = p_x[i];

    int cmp = dbl_cmp(
      current,
      previous,
      direction,
      na_order
    );

    // Not expected ordering
    if (cmp < 0) {
      p_group_info->n_groups = original_n_groups;
      return false;
    }

    previous = current;

    // Continue group run
    if (cmp == 0) {
      ++group_size;
      continue;
    }

    // Expected ordering
    groups_size_push(p_group_infos, group_size);
    group_size = 1;
  }

  // Push final group run
  groups_size_push(p_group_infos, group_size);

  // Expected ordering
  return true;
}

/*
 * Compare two doubles, handling `na_order` and `direction`
 */
static inline int dbl_cmp(double x,
                          double y,
                          const int direction,
                          const int na_order) {
  if (isnan(x)) {
    if (isnan(y)) {
      return 0;
    } else {
      return na_order;
    }
  }

  if (isnan(y)) {
    return -na_order;
  }

  int cmp = (x > y) - (x < y);

  return cmp * direction;
}

// -----------------------------------------------------------------------------

static inline int int_cmp(int x,
                          int y,
                          const int direction,
                          const int na_order);

// Very similar to `dbl_sorted()`
static bool int_sorted(const int* p_x,
                       int* p_o,
                       struct group_infos* p_group_infos,
                       R_xlen_t size,
                       bool decreasing,
                       bool na_last) {
  if (size == 0) {
    return true;
  }

  if (size == 1) {
    groups_size_push(p_group_infos, 1);
    return true;
  }

  const int direction = decreasing ? -1 : 1;
  const int na_order = na_last ? 1 : -1;

  int previous = p_x[0];

  R_xlen_t count = 0;

  // Check for strictly opposite of expected order
  // (ties are not allowed so we can reverse the vector stably)
  for (R_xlen_t i = 1; i < size; ++i, ++count) {
    int current = p_x[i];

    int cmp = int_cmp(
      current,
      previous,
      direction,
      na_order
    );

    if (cmp >= 0) {
      break;
    }

    previous = current;
  }

  // Was in strictly opposite of expected order.
  if (count == size - 1) {
    ord_reverse(p_o, size);

    // Each group is size 1 since this is strict ordering
    for (R_xlen_t j = 0; j < size; ++j) {
      groups_size_push(p_group_infos, 1);
    }

    return true;
  }

  // Was partially in expected order. Need to sort.
  if (count != 0) {
    return false;
  }

  // Retain the original `n_groups` to be able to reset the group sizes if
  // it turns out we don't have expected ordering
  struct group_info* p_group_info = groups_current(p_group_infos);
  R_xlen_t original_n_groups = p_group_info->n_groups;

  R_xlen_t group_size = 1;

  // Check for expected ordering - allowing ties since we don't have to
  // reverse the ordering.
  for (R_xlen_t i = 1; i < size; ++i) {
    int current = p_x[i];

    int cmp = int_cmp(
      current,
      previous,
      direction,
      na_order
    );

    // Not expected ordering
    if (cmp < 0) {
      p_group_info->n_groups = original_n_groups;
      return false;
    }

    previous = current;

    // Continue group run
    if (cmp == 0) {
      ++group_size;
      continue;
    }

    // Expected ordering
    groups_size_push(p_group_infos, group_size);
    group_size = 1;
  }

  // Push final group run
  groups_size_push(p_group_infos, group_size);

  // Expected ordering
  return true;
}

// Very similar to `dbl_cmp()`
static inline int int_cmp(int x,
                          int y,
                          const int direction,
                          const int na_order) {
  if (x == NA_INTEGER) {
    if (y == NA_INTEGER) {
      return 0;
    } else {
      return na_order;
    }
  }

  if (y == NA_INTEGER) {
    return -na_order;
  }

  int cmp = (x > y) - (x < y);

  return cmp * direction;
}

// -----------------------------------------------------------------------------

static inline int chr_cmp(SEXP x,
                          SEXP y,
                          const char* c_x,
                          const char* c_y,
                          const int direction,
                          const int na_order);

/*
 * Check if `p_x` is in the "expected" ordering as defined by `decreasing` and
 * `na_last`. If `p_x` is in the expected ordering, or if it is in the strictly
 * opposite of the expected ordering (with no ties), then groups are pushed,
 * the order is finalized, and `true` is returned indicating that no sorting
 * is needed.
 */
static bool chr_sorted(const SEXP* p_x,
                       int* p_o,
                       struct group_infos* p_group_infos,
                       R_xlen_t size,
                       bool decreasing,
                       bool na_last) {
  if (size == 0) {
    return true;
  }

  if (size == 1) {
    groups_size_push(p_group_infos, 1);
    return true;
  }

  const int direction = decreasing ? -1 : 1;
  const int na_order = na_last ? 1 : -1;

  SEXP previous = p_x[0];
  const char* c_previous = CHAR(previous);

  R_xlen_t count = 0;

  // Check for strictly opposite of expected order
  // (ties are not allowed so we can reverse the vector stably)
  for (R_xlen_t i = 1; i < size; ++i, ++count) {
    SEXP current = p_x[i];
    const char* c_current = CHAR(current);

    int cmp = chr_cmp(
      current,
      previous,
      c_current,
      c_previous,
      direction,
      na_order
    );

    if (cmp >= 0) {
      break;
    }

    previous = current;
    c_previous = c_current;
  }

  // Was in strictly opposite of expected order.
  if (count == size - 1) {
    ord_reverse(p_o, size);

    // Each group is size 1 since this is strict ordering
    for (R_xlen_t j = 0; j < size; ++j) {
      groups_size_push(p_group_infos, 1);
    }

    return true;
  }

  // Was partially in expected order. Need to sort.
  if (count != 0) {
    return false;
  }

  // Retain the original `n_groups` to be able to reset the group sizes if
  // it turns out we don't have expected ordering
  struct group_info* p_group_info = groups_current(p_group_infos);
  R_xlen_t original_n_groups = p_group_info->n_groups;

  R_xlen_t group_size = 1;

  // Check for expected ordering - allowing ties since we don't have to
  // reverse the ordering.
  for (R_xlen_t i = 1; i < size; ++i) {
    SEXP current = p_x[i];
    const char* c_current = CHAR(current);

    int cmp = chr_cmp(
      current,
      previous,
      c_current,
      c_previous,
      direction,
      na_order
    );

    // Not expected ordering
    if (cmp < 0) {
      p_group_info->n_groups = original_n_groups;
      return false;
    }

    previous = current;
    c_previous = c_current;

    // Continue group run
    if (cmp == 0) {
      ++group_size;
      continue;
    }

    // Expected ordering
    groups_size_push(p_group_infos, group_size);
    group_size = 1;
  }

  // Push final group run
  groups_size_push(p_group_infos, group_size);

  // Expected ordering
  return true;
}

/*
 * `direction` is `1` for ascending and `-1` for descending.
 * `na_order` is `1` if `na_last = true` and `-1` if `na_last = false`.
 */
static inline int chr_cmp(SEXP x,
                          SEXP y,
                          const char* c_x,
                          const char* c_y,
                          const int direction,
                          const int na_order) {
  // Same pointer - including `NA`s
  if (x == y) {
    return 0;
  }

  if (x == NA_STRING) {
    return na_order;
  }

  if (y == NA_STRING) {
    return -na_order;
  }

  return direction * strcmp(c_x, c_y);
}

// -----------------------------------------------------------------------------

// Used when in strictly opposite of expected order.
// `size` will be at least 2.
static inline void ord_reverse(int* p_o, R_xlen_t size) {
  const R_xlen_t half = size / 2;

  for (R_xlen_t i = 0; i < half; ++i) {
    R_xlen_t swap = size - 1 - i;

    const int temp = p_o[i];

    p_o[i] = p_o[swap];
    p_o[swap] = temp;
  }
}

// -----------------------------------------------------------------------------

#undef UINT8_MAX_SIZE

#undef INT_MAX_RADIX_PASS
#undef DBL_MAX_RADIX_PASS

#undef INT_COUNTING_ORDER_RANGE_BOUNDARY

#undef INSERTION_ORDER_BOUNDARY

#undef PASS_TO_RADIX
#undef SHIFT_ADJUSTMENT

#undef CHAR_IS_UTF8
#undef CHAR_IS_ASCII
#undef CHAR_NEEDS_REENCODE
#undef CHAR_REENCODE
