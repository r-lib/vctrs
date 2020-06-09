#include "vctrs.h"
#include "utils.h"

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

// -----------------------------------------------------------------------------

#define GROUP_DATA_SIZE_DEFAULT 100000

struct group_info {
  SEXP data;
  int* p_data;
  PROTECT_INDEX data_pi;

  R_xlen_t data_size;

  R_xlen_t n_groups;
  R_xlen_t max_group_size;
};

static void group_realloc(struct group_info* p_ginfo, R_xlen_t size) {
  // Rellocate
  p_ginfo->data = Rf_xlengthgets(p_ginfo->data, size);

  // Reprotect
  REPROTECT(p_ginfo->data, p_ginfo->data_pi);

  // Update pointer
  p_ginfo->p_data = INTEGER(p_ginfo->data);

  // Update size
  p_ginfo->data_size = size;
}

struct group_infos {
  struct group_info** p_info;
  int current;
  bool ignore;
};

static inline struct group_info* groups_current(struct group_infos* p_ginfos) {
  return p_ginfos->p_info[p_ginfos->current];
}

// Swap groups after each column in a data frame `x`
static void groups_swap(struct group_infos* p_ginfos) {
  if (p_ginfos->ignore) {
    return;
  }

  struct group_info* p_ginfo_pre = groups_current(p_ginfos);

  // Make the swap
  p_ginfos->current = 1 - p_ginfos->current;

  struct group_info* p_ginfo_post = groups_current(p_ginfos);

  // Clear the info from last time the swap was made
  p_ginfo_post->max_group_size = 0;
  p_ginfo_post->n_groups = 0;

  // Ensure the new group info is at least as big as the old group info
  if (p_ginfo_post->data_size < p_ginfo_pre->data_size) {
    group_realloc(p_ginfo_post, p_ginfo_pre->data_size);
  }
}

static void groups_size_push(struct group_infos* p_ginfos, R_xlen_t size) {
  if (p_ginfos->ignore) {
    return;
  }

  if (size == 0) {
    Rf_errorcall(R_NilValue, "Internal error: Group `size` to push should never be zero.");
  }

  struct group_info* p_ginfo = groups_current(p_ginfos);

  // Extend `data` as required - reprotects itself
  if (p_ginfo->data_size == p_ginfo->n_groups) {
    group_realloc(p_ginfo, p_ginfo->data_size * 2);
  }

  // Push group size
  p_ginfo->p_data[p_ginfo->n_groups] = size;

  // Bump number of groups
  ++p_ginfo->n_groups;

  // Update max group size
  if (p_ginfo->max_group_size < size) {
    p_ginfo->max_group_size = size;
  }
}

// -----------------------------------------------------------------------------

// Shifts or flips `x` depending on the value of `na_last` and `direction` to
// maintain the correct order. `NA` is always last if `na_last` is true.
// `direction = 1` ascending
// `direction = -1` descending
static inline int int_adjust(int x, int direction, bool na_last) {
  if (na_last) {
    return (x == NA_INTEGER) ? INT_MAX : x * direction - 1;
  } else {
    return (x == NA_INTEGER) ? INT_MIN : x * direction;
  }
}

// -----------------------------------------------------------------------------

#define INT_RANGE_LIMIT 100000

// Computes the inclusive range - i.e. the number of values between `[min, max]`
// including min and max.
static void int_range(const int* p_x, R_xlen_t size, int* p_x_min, uint32_t* p_range) {
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
    ++i;

    break;
  }

  // All NAs - Return max range to signal that we don't use counting sort
  if (x_min == NA_INTEGER) {
    *p_x_min = x_min;
    *p_range = range;
    return;
  }

  // Find min and max
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

  // Max possible range is from `c(.Machine$integer.max, -.Machine$integer.max)`
  // which is exactly the max of a `uint32_t`. But we need to go up to `int64_t`
  // to avoid intermediate overflow.
  range = (uint32_t) ((int64_t) x_max - (int64_t) x_min + 1);

  *p_x_min = x_min;
  *p_range = range;
}

// `p_x` is unadjusted here
static void int_counting_sort(const int* p_x,
                              int* p_o,
                              int* p_o_aux,
                              struct group_infos* p_ginfos,
                              R_xlen_t size,
                              int x_min,
                              uint32_t range,
                              bool decreasing,
                              bool na_last) {
  // Needs to be static so:
  // - We only allocate it once (counts are reset to 0 at end)
  // - Allocating as static allows us to allocate an array this large
  // - + 1 to make room for `NA` bucket
  static R_xlen_t p_counts[INT_RANGE_LIMIT + 1] = { 0 };

  // `NA` values get counted in the last used bucket
  uint32_t na_bucket = range;
  R_xlen_t na_count = 0;

  // Sanity check
  if (range > INT_RANGE_LIMIT) {
    Rf_errorcall(R_NilValue, "Internal error: `range > INT_RANGE_LIMIT`.");
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
    groups_size_push(p_ginfos, na_count);
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
    groups_size_push(p_ginfos, count);

    j += direction;
  }

  // `na_last = true` pushes NA counts to the back
  if (na_last && na_count != 0) {
    p_counts[na_bucket] = cumulative;
    cumulative += na_count;
    groups_size_push(p_ginfos, na_count);
  }

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
  for (R_xlen_t i = 0; i < size; ++i) {
    p_o[i] = p_o_aux[i];
  }

  // Reset counts for next column.
  // Only reset what we might have touched.
  memset(p_counts, 0, (range + 1) * sizeof(R_xlen_t));
}

// -----------------------------------------------------------------------------

// A bit ad hoc
#define INT_INSERTION_SIZE 256

// Used as an internal algorithm for radix sorting once we hit a slice of
// size `INSERTION_SIZE`. Should be fast for these small slices, inserts
// into `p_o` directly.
static void int_insertion_sort(int* p_o,
                               int* p_x,
                               struct group_infos* p_ginfos,
                               const R_xlen_t size) {
  // Don't think this can occur, but safer this way
  if (size == 0) {
    return;
  }

  for (R_xlen_t i = 1; i < size; ++i) {
    const int x_elt = p_x[i];
    const int o_elt = p_o[i];

    R_xlen_t j = i - 1;

    while (j >= 0) {
      int x_cmp_elt = p_x[j];

      if (x_elt >= x_cmp_elt) {
        break;
      }

      int o_cmp_elt = p_o[j];

      // Shift
      p_x[j + 1] = x_cmp_elt;
      p_o[j + 1] = o_cmp_elt;

      // Next
      --j;
    }

    // Place original element in updated location
    p_x[j + 1] = x_elt;
    p_o[j + 1] = o_elt;
  }

  // We've ordered a small chunk, we need to push at least one group size
  R_xlen_t group_size = 1;
  int previous = p_x[0];

  for (R_xlen_t i = 1; i < size; ++i) {
    const int current = p_x[i];

    // Continue the current group run
    if (current == previous) {
      ++group_size;
      continue;
    }

    // Push current run size and reset size tracker
    groups_size_push(p_ginfos, group_size);
    group_size = 1;

    previous = current;
  }

  // Push final group run
  groups_size_push(p_ginfos, group_size);
}

// -----------------------------------------------------------------------------

#define HEX_UINT32_SIGN_BIT 0x80000000u

// [INT32_MIN, INT32_MAX] => [0, UINT32_MAX]
static inline uint32_t map_from_int32_to_uint32(int32_t x) {
  return x ^ HEX_UINT32_SIGN_BIT;
}

#undef HEX_UINT32_SIGN_BIT

static inline uint8_t extract_byte(uint32_t x, uint8_t shift) {
  return (x >> shift) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

static void int_radix_order_pass(int* p_x,
                                 int* p_x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 struct group_infos* p_ginfos,
                                 const R_xlen_t size,
                                 const uint8_t pass) {
  // Finish this group with insertion sort once it gets small enough
  if (size <= INT_INSERTION_SIZE) {
    int_insertion_sort(p_o, p_x, p_ginfos, size);
    return;
  }

  const uint8_t radix = 3 - pass;
  const uint8_t shift = radix * 8;

  uint8_t byte = 0;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const int x_elt = p_x[i];

    // Relies on `NA_INTEGER == INT_MIN`
    const uint32_t x_elt_mapped = map_from_int32_to_uint32(x_elt);

    byte = extract_byte(x_elt_mapped, shift);

    p_bytes[i] = byte;
    ++p_counts[byte];
  }

  // Fast check to see if all bytes were the same. If so, skip `pass`.
  if (p_counts[byte] == size) {
    return;
  }

  R_xlen_t cumulative = 0;

  // Cumulate counts
  for (uint16_t i = 0; i < UINT8_MAX_SIZE; ++i) {
    R_xlen_t count = p_counts[i];

    // Skip over zeros
    if (count == 0) {
      continue;
    }

    // Replace with `cumulative` first, then bump `cumulative`
    // `p_counts` now represents starting locations for each radix group
    p_counts[i] = cumulative;
    cumulative += count;
  }

  // Move into `aux` in the right order
  for (R_xlen_t i = 0; i < size; ++i) {
    const uint8_t byte = p_bytes[i];
    const R_xlen_t loc = p_counts[byte]++;
    p_o_aux[loc] = p_o[i];
    p_x_aux[loc] = p_x[i];
  }

  // Move from `aux` back to `out`
  // TODO: Maybe with pointer swaps?
  for (R_xlen_t i = 0; i < size; ++i) {
    p_o[i] = p_o_aux[i];
    p_x[i] = p_x_aux[i];
  }
}

// -----------------------------------------------------------------------------

static void int_radix_order_impl(int* p_x,
                                 int* p_x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 struct group_infos* p_ginfos,
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
    p_ginfos,
    size,
    pass
  );

  const uint8_t next_pass = pass + 1;
  R_xlen_t last_cumulative_count = 0;

  for (uint16_t i = 0; last_cumulative_count < size && i < UINT8_MAX_SIZE; ++i) {
    const R_xlen_t cumulative_count = p_counts[i];

    if (cumulative_count == 0) {
      continue;
    }

    // Diff the cumulated counts to get the radix group size
    const R_xlen_t group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
      groups_size_push(p_ginfos, 1);
      ++p_x;
      ++p_o;
      ++p_x_aux;
      ++p_o_aux;
      ++p_bytes;
      continue;
    }

    // Can get here in the case of ties, like c(1L, 1L), which have a
    // `group_size` of 2 in the last radix, but there is nothing left to compare
    if (next_pass == 4) {
      groups_size_push(p_ginfos, group_size);
      p_x += group_size;
      p_o += group_size;
      p_x_aux += group_size;
      p_o_aux += group_size;
      p_bytes += group_size;
      continue;
    }

    int_radix_order_impl(
      p_x,
      p_x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_ginfos,
      group_size,
      next_pass
    );

    p_x += group_size;
    p_o += group_size;
    p_x_aux += group_size;
    p_o_aux += group_size;
    p_bytes += group_size;
  }
}

// -----------------------------------------------------------------------------

static void int_radix_order(int* p_x,
                            int* p_x_aux,
                            int* p_o,
                            int* p_o_aux,
                            uint8_t* p_bytes,
                            struct group_infos* p_ginfos,
                            bool decreasing,
                            bool na_last,
                            R_xlen_t size) {
  const int direction = decreasing ? -1 : 1;

  // Correct the order once up front
  // - Adjusts based on decreasing / na_last
  // - Adjusts based on previous column's partial ordering in `p_o`
  for (R_xlen_t i = 0; i < size; ++i) {
    p_x[i] = int_adjust(p_x[i], direction, na_last);
  }

  const uint8_t pass = 0;

  int_radix_order_impl(
    p_x,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_ginfos,
    size,
    pass
  );
}

// -----------------------------------------------------------------------------

// Assumes `x` is modifiable by reference
static void int_order(SEXP x,
                      SEXP x_aux,
                      int* p_o,
                      int* p_o_aux,
                      uint8_t* p_bytes,
                      struct group_infos* p_ginfos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  int* p_x = INTEGER(x);

  if (size < INT_INSERTION_SIZE) {
    int_insertion_sort(p_o, p_x, p_ginfos, size);
    return;
  }

  uint32_t range;
  int x_min;

  int_range(p_x, size, &x_min, &range);

  if (range < INT_RANGE_LIMIT) {
    int_counting_sort(p_x, p_o, p_o_aux, p_ginfos, size, x_min, range, decreasing, na_last);
    return;
  }

  int* p_x_aux = INTEGER(x_aux);

  int_radix_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_ginfos, decreasing, na_last, size);
}

// Only called when original `x` is just an integer vector.
// We need `x` to be mutable so we copy it over into our temp vector.
// Special case the counting sort though, which doesn't touch `x`.
static void int_order_immutable(SEXP x,
                                SEXP x_adjusted,
                                SEXP x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_ginfos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size) {
  const int* p_x = INTEGER(x);

  uint32_t range;
  int x_min;

  int_range(p_x, size, &x_min, &range);

  // If in counting sort range, no need to copy over to `x_adjusted`
  if (range < INT_RANGE_LIMIT) {
    int_counting_sort(p_x, p_o, p_o_aux, p_ginfos, size, x_min, range, decreasing, na_last);
    return;
  }

  int* p_x_adjusted = INTEGER(x_adjusted);
  int* p_x_aux = INTEGER(x_aux);

  for (R_xlen_t i = 0; i < size; ++i) {
    p_x_adjusted[i] = p_x[i];
  }

  int_radix_order(
    p_x_adjusted,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_ginfos,
    decreasing,
    na_last,
    size
  );
}


// -----------------------------------------------------------------------------

static void vec_radix_order_immutable_switch(SEXP x,
                                             SEXP x_adjusted,
                                             SEXP x_aux,
                                             int* p_o,
                                             int* p_o_aux,
                                             uint8_t* p_bytes,
                                             struct group_infos* p_ginfos,
                                             bool decreasing,
                                             bool na_last,
                                             R_xlen_t size);

static void vec_col_radix_order_switch(SEXP x,
                                       SEXP x_aux,
                                       int* p_o,
                                       int* p_o_aux,
                                       uint8_t* p_bytes,
                                       struct group_infos* p_ginfos,
                                       bool decreasing,
                                       bool na_last,
                                       R_xlen_t size);

static void df_radix_order(SEXP x,
                           SEXP x_adjusted,
                           SEXP x_aux,
                           int* p_o,
                           int* p_o_aux,
                           uint8_t* p_bytes,
                           struct group_infos* p_ginfos,
                           SEXP decreasing,
                           bool na_last,
                           R_xlen_t size) {
  R_xlen_t n_cols = Rf_xlength(x);

  R_xlen_t n_decreasing = Rf_xlength(decreasing);
  int* p_decreasing = LOGICAL(decreasing);

  bool recycle;

  if (n_decreasing == 1) {
    recycle = true;
  } else if (n_decreasing == n_cols) {
    recycle = false;
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

  // Apply on one column to fill group info.
  // First column is immutable and we must copy into `x_adjusted`.
  SEXP col = VECTOR_ELT(x, 0);
  bool col_decreasing = p_decreasing[0];

  vec_radix_order_immutable_switch(
    col,
    x_adjusted,
    x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_ginfos,
    col_decreasing,
    na_last,
    size
  );

  // Iterate over remaining columns by group chunk
  for (R_xlen_t i = 1; i < n_cols; ++i) {
    col = VECTOR_ELT(x, i);
    col_decreasing = recycle ? p_decreasing[0] : p_decreasing[i];

    // Reset pointers between columns
    int* p_o_shift = p_o;
    int* p_o_aux_shift = p_o_aux;

    // Get number of group chunks from previous column
    struct group_info* p_ginfo_pre = groups_current(p_ginfos);
    R_xlen_t n_groups = p_ginfo_pre->n_groups;

    // If there were no ties, we are done early
    if (n_groups == size) {
      break;
    }

    // Swap to other group info for this column
    groups_swap(p_ginfos);

    // `x_adjusted` will hold current group slice of `x`
    int* p_x_adjusted = INTEGER(x_adjusted);
    const int* p_col = INTEGER(col);

    // Iterate over this column's group chunks
    for (R_xlen_t group = 0; group < n_groups; ++group) {
      R_xlen_t group_size = p_ginfo_pre->p_data[group];

      // Easy case
      // TODO: Do x_adjusted / x_aux need to be incremented? I don't think
      // so since they are just temp memory
      if (group_size == 1) {
        ++p_o_shift;
        ++p_o_aux_shift;
        groups_size_push(p_ginfos, 1);
        continue;
      }

      // Realign the partially sorted column group chunk
      for (R_xlen_t j = 0; j < group_size; ++j) {
        const int loc = p_o_shift[j] - 1;
        p_x_adjusted[j] = p_col[loc];
      }

      vec_col_radix_order_switch(
        x_adjusted,
        x_aux,
        p_o_shift,
        p_o_aux_shift,
        p_bytes,
        p_ginfos,
        col_decreasing,
        na_last,
        group_size
      );

      p_o_shift += group_size;
      p_o_aux_shift += group_size;
    }
  }
}

// -----------------------------------------------------------------------------

// Like `vec_radix_order_switch()`, but specifically for columns of a data
// frame where `decreasing` is known and is scalar.
// Assumes `x` is mutable.
static void vec_col_radix_order_switch(SEXP x,
                                       SEXP x_aux,
                                       int* p_o,
                                       int* p_o_aux,
                                       uint8_t* p_bytes,
                                       struct group_infos* p_ginfos,
                                       bool decreasing,
                                       bool na_last,
                                       R_xlen_t size) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_integer: {
    int_order(x, x_aux, p_o, p_o_aux, p_bytes, p_ginfos, decreasing, na_last, size);
    break;
  }
  case vctrs_type_dataframe: {
    Rf_errorcall(R_NilValue, "Internal error: df-cols should have already been flattened.");
    break;
  }
  default: {
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_radix_order()`");
  }
  }
}

// -----------------------------------------------------------------------------

static SEXP vec_radix_order(SEXP x, SEXP decreasing, bool na_last, bool groups);

// [[ register() ]]
SEXP vctrs_radix_order(SEXP x, SEXP decreasing, SEXP na_last, SEXP groups) {
  if (!r_is_bool(na_last)) {
    Rf_errorcall(R_NilValue, "`na_last` must be either `TRUE` or `FALSE`.");
  }

  bool c_na_last = LOGICAL(na_last)[0];
  bool c_groups = LOGICAL(groups)[0];

  return vec_radix_order(x, decreasing, c_na_last, c_groups);
}


static void vec_radix_order_switch(SEXP x,
                                   SEXP x_adjusted,
                                   SEXP x_aux,
                                   int* p_o,
                                   int* p_o_aux,
                                   uint8_t* p_bytes,
                                   struct group_infos* p_ginfos,
                                   SEXP decreasing,
                                   bool na_last,
                                   R_xlen_t size);

static SEXP vec_radix_order(SEXP x, SEXP decreasing, bool na_last, bool groups) {
  // TODO:
  // x = PROTECT(vec_proxy_compare(x));

  // TODO:
  // Should proxy-compare flatten df-cols?
  // How to track vector of `decreasing` if so?

  R_xlen_t size = vec_size(x);

  // Don't check length here. This might be vectorized if `x` is a data frame.
  if (TYPEOF(decreasing) != LGLSXP) {
    Rf_errorcall(R_NilValue, "`decreasing` must be logical");
  }
  if (lgl_any_na(decreasing)) {
    Rf_errorcall(R_NilValue, "`decreasing` must not contain missing values.");
  }

  // This is sometimes bigger than it needs to be, but will only
  // be much bigger than required if `x` is filled with numbers that are
  // incredibly spread out.
  SEXP x_adjusted = PROTECT(Rf_allocVector(INTSXP, size));
  SEXP x_aux = PROTECT(Rf_allocVector(INTSXP, size));

  SEXP o = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_o = INTEGER(o);

  SEXP o_aux = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_o_aux = INTEGER(o_aux);

  // Initialize `out` with sequential 1-based ordering
  for (R_xlen_t i = 0, j = 1; i < size; ++i, ++j) {
    p_o[i] = j;
  }

  uint8_t* p_bytes = (uint8_t*) R_alloc(size, sizeof(uint8_t));

  // Should group info be ignored?
  // Can only ignore if dealing with non-data-frame input and groups have not
  // been requested, but this is more efficient for atomic vectors.
  bool ignore = groups ? false : (is_data_frame(x) ? false : true);

  // First group info object is initialized with data
  struct group_info ginfo1;
  ginfo1.data_size = ignore ? 0 : GROUP_DATA_SIZE_DEFAULT;
  ginfo1.data = Rf_allocVector(INTSXP, ginfo1.data_size);
  PROTECT_WITH_INDEX(ginfo1.data, &ginfo1.data_pi);
  ginfo1.p_data = INTEGER(ginfo1.data);
  ginfo1.n_groups = 0;
  ginfo1.max_group_size = 0;

  // Second group info object is initialized, but is empty
  struct group_info ginfo2;
  ginfo2.data_size = 0;
  ginfo2.data = Rf_allocVector(INTSXP, ginfo2.data_size);
  PROTECT_WITH_INDEX(ginfo2.data, &ginfo2.data_pi);
  ginfo2.p_data = INTEGER(ginfo2.data);
  ginfo2.n_groups = 0;
  ginfo2.max_group_size = 0;

  // Store both of them in a `group_infos` object
  struct group_info* p_group_info[2];
  p_group_info[0] = &ginfo1;
  p_group_info[1] = &ginfo2;

  struct group_infos ginfos;
  struct group_infos* p_ginfos = &ginfos;
  p_ginfos->p_info = p_group_info;
  p_ginfos->current = 0;
  p_ginfos->ignore = ignore;

  vec_radix_order_switch(
    x,
    x_adjusted,
    x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_ginfos,
    decreasing,
    na_last,
    size
  );

  // Return all group info rather than just ordering
  if (groups) {
    struct group_info* p_ginfo = groups_current(p_ginfos);

    R_xlen_t n_groups = p_ginfo->n_groups;
    p_ginfo->data = PROTECT(Rf_xlengthgets(p_ginfo->data, n_groups));

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

    SET_VECTOR_ELT(out, 0, o);
    SET_VECTOR_ELT(out, 1, p_ginfo->data);
    SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(p_ginfo->n_groups));
    SET_VECTOR_ELT(out, 3, Rf_ScalarInteger(p_ginfo->max_group_size));

    UNPROTECT(8);
    return out;
  }

  UNPROTECT(6);
  return o;
}

// -----------------------------------------------------------------------------

static void vec_radix_order_switch(SEXP x,
                                   SEXP x_adjusted,
                                   SEXP x_aux,
                                   int* p_o,
                                   int* p_o_aux,
                                   uint8_t* p_bytes,
                                   struct group_infos* p_ginfos,
                                   SEXP decreasing,
                                   bool na_last,
                                   R_xlen_t size) {
  const enum vctrs_type type = vec_proxy_typeof(x);

  if (type == vctrs_type_dataframe) {
    df_radix_order(
      x,
      x_adjusted,
      x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_ginfos,
      decreasing,
      na_last,
      size
    );

    return;
  }

  // We know it is logical with no missing values, but size hasn't been checked
  if (Rf_xlength(decreasing) != 1) {
    Rf_errorcall(R_NilValue, "`decreasing` must have length 1 when `x` is not a data frame.");
  }

  bool c_decreasing = LOGICAL(decreasing)[0];

  vec_radix_order_immutable_switch(
    x,
    x_adjusted,
    x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    p_ginfos,
    c_decreasing,
    na_last,
    size
  );
}

// Used on bare vectors and the first column of data frame `x`s
static void vec_radix_order_immutable_switch(SEXP x,
                                             SEXP x_adjusted,
                                             SEXP x_aux,
                                             int* p_o,
                                             int* p_o_aux,
                                             uint8_t* p_bytes,
                                             struct group_infos* p_ginfos,
                                             bool decreasing,
                                             bool na_last,
                                             R_xlen_t size) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_integer: {
    int_order_immutable(
      x,
      x_adjusted,
      x_aux,
      p_o,
      p_o_aux,
      p_bytes,
      p_ginfos,
      decreasing,
      na_last,
      size
    );

    break;
  }
  case vctrs_type_dataframe: {
    Rf_errorcall(R_NilValue, "Data frames should have been handled by now");
  }
  default: {
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_radix_order()`");
  }
  }
}

// -----------------------------------------------------------------------------

#undef UINT8_MAX_SIZE

#undef GROUP_DATA_SIZE_DEFAULT

#undef INT_RANGE_LIMIT

#undef INT_INSERTION_SIZE
