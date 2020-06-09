#include "vctrs.h"
#include "utils.h"

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

// -----------------------------------------------------------------------------

// This seems to be a reasonable default to start with for tracking group sizes
// and is what base R uses. It is expanded by 2x every time we need to
// reallocate.
#define GROUP_DATA_SIZE_DEFAULT 100000

/*
 * Info related to 1 column / vector worth of groupings
 *
 * @member data An integer vector of group sizes.
 * @member p_data A pointer to `data`.
 * @member data_pi The protection index for `data` which allows us to
 *   `REPROTECT()` on the fly.
 * @member data_size The current allocated size of `data`.
 * @member n_groups The current number of groups seen so far.
 *   Always `<= data_size`.
 * @member max_group_size The maximum group size seen so far.
 */
struct group_info {
  SEXP data;
  int* p_data;
  PROTECT_INDEX data_pi;

  R_xlen_t data_size;

  R_xlen_t n_groups;
  R_xlen_t max_group_size;
};

/*
 * Reallocate `data` to be as long as `size`.
 */
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

/*
 * `group_infos` contains information about 2 `group_info` structs.
 *
 * For a single atomic vector, `current = 0` is always set and only one of the
 * structs is ever used.
 *
 * For a data frame with multiple columns, after every column `current` is
 * flipped between 0 and 1, giving us a chance to read the group information
 * off the previous column (which allows us to chunk the current column into
 * groups) while also updating the group information of the chunks of
 * the current one.
 *
 * @member p_info A pointer to an array of 2 `group_info*` to swap between.
 * @member current The current `p_info` we are on. This is either 0 or 1.
 * @member ignore Should group tracking be ignored entirely? This is the default
 *   for atomic vectors unless groups information is explicitly requested.
 */
struct group_infos {
  struct group_info** p_info;
  int current;
  bool ignore;
};

/*
 * Extract the current `group_info*`
 */
static inline struct group_info* groups_current(struct group_infos* p_ginfos) {
  return p_ginfos->p_info[p_ginfos->current];
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

/*
 * Push a group size onto the current `group_info*`
 * - Does nothing if we are ignoring group info
 * - Reallocates as needed
 * - Updates number of groups / max group size as well
 */
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

static inline void int_adjust_na_last(int* p_x, int direction, R_xlen_t size);
static inline void int_adjust_na_first(int* p_x, int direction, R_xlen_t size);

/*
 * - Shifts the elements of `p_x` in a way that correctly maintains ordering
 *   for `na_last` and `decreasing`
 *
 * - Used before both the integer insertion sort and radix sort, which both
 *   expect their input to already have been "adjusted" for `na_last` and
 *   `decreasing`.
 *
 * - If `na_last = true`, `NA` is always the maximum element, so we set it to
 *   `INT_MAX`. In that case, we also shift all non-NA values down by 1 to
 *   make room for it.
 *
 * - If `na_last = false`, there is really nothing to do in terms of shifting
 *   because `NA_INTEGER = INT_MIN`.
 *
 * - The multiplication by `direction` applies to non-NA values and correctly
 *   orders inputs based on whether we are in a decreasing order or not.
 */
static inline void int_adjust(int* p_x,
                              const bool decreasing,
                              const bool na_last,
                              const R_xlen_t size) {
  const int direction = decreasing ? -1 : 1;

  if (na_last) {
    int_adjust_na_last(p_x, direction, size);
  } else {
    int_adjust_na_first(p_x, direction, size);
  }
}


static inline void int_adjust_na_last(int* p_x,
                                      const int direction,
                                      const R_xlen_t size) {
  for (R_xlen_t i = 0; i < size; ++i) {
    const int elt = p_x[i];
    p_x[i] = (elt == NA_INTEGER) ? INT_MAX : elt * direction - 1;
  }
}

static inline void int_adjust_na_first(int* p_x,
                                       const int direction,
                                       const R_xlen_t size) {
  for (R_xlen_t i = 0; i < size; ++i) {
    const int elt = p_x[i];
    p_x[i] = (elt == NA_INTEGER) ? INT_MIN : elt * direction;
  }
}

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


#define INT_COUNTING_ORDER_RANGE_BOUNDARY 100000

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
                              struct group_infos* p_ginfos,
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

    // At this point we will handle this group completely
    groups_size_push(p_ginfos, count);

    j += direction;
  }

  // `na_last = true` pushes NA counts to the back
  if (na_last && na_count != 0) {
    p_counts[na_bucket] = cumulative;
    groups_size_push(p_ginfos, na_count);
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

// A bit ad hoc - but seems to work well. Going up to 256 definitely has
// negative performance implications for completely random input.
// Somewhat based on this post, which also uses 128.
// https://probablydance.com/2016/12/27/i-wrote-a-faster-sorting-algorithm/
#define INT_INSERTION_ORDER_BOUNDARY 128

// Used as an internal algorithm for radix sorting once we hit a slice of
// size `INSERTION_SIZE`. Should be fast for these small slices, inserts
// into `p_o` directly.

/*
 * `int_insertion_order()` is used in two ways:
 * - It is how we "finish off" radix sorts rather than deep recursion.
 * - If we have an original `x` input that is small enough, we just immediately
 *   insertion sort it.
 *
 * Insertion ordering expects that `p_x` has been adjusted with `int_adjust()`.
 */
static void int_insertion_order(int* p_o,
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

// Flipping the sign bit is all we need to do to map in an order preserving way.
// Relies on `NA_INTEGER == INT_MIN` which maps to 0 correctly.
// [INT32_MIN, INT32_MAX] => [0, UINT32_MAX]
static inline uint32_t map_from_int32_to_uint32(int32_t x) {
  return x ^ HEX_UINT32_SIGN_BIT;
}

#undef HEX_UINT32_SIGN_BIT

// Bytes will be extracted 8 bits at a time.
// This is a MSB radix sort, so they are extracted MSB->LSB.
// TODO: This probably depends on endianness?
static inline uint8_t extract_byte(uint32_t x, uint8_t shift) {
  return (x >> shift) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

/*
 * Orders based on a single byte corresponding to the current `pass`.
 */
static void int_radix_order_pass(int* p_x,
                                 int* p_x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 struct group_infos* p_ginfos,
                                 const R_xlen_t size,
                                 const uint8_t pass) {
  if (size <= INT_INSERTION_ORDER_BOUNDARY) {
    int_insertion_order(p_o, p_x, p_ginfos, size);
    return;
  }

  // TODO: Is this where we could modify for big endian?
  const uint8_t radix = 3 - pass;
  const uint8_t shift = radix * 8;

  uint8_t byte = 0;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const int x_elt = p_x[i];

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
  memcpy(p_x, p_x_aux, size * sizeof(int));
}

// -----------------------------------------------------------------------------

/*
 * Recursive function for radix ordering. Orders the current byte, then iterates
 * over the sub groups and recursively calls itself on each subgroup to order
 * the next byte.
 */
static void int_radix_order(int* p_x,
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

    // Diff the accumulated counts to get the radix group size
    const R_xlen_t group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
      groups_size_push(p_ginfos, 1);
      // TODO: Do we really need to increment all of these?
      // Maybe just `p_x` and `p_o`?
      ++p_x;
      ++p_o;
      ++p_x_aux;
      ++p_o_aux;
      ++p_bytes;
      continue;
    }

    // Can get here in the case of ties, like c(1L, 1L), which have a
    // `group_size` of 2 in the last radix, but there is nothing left to
    // compare so we are done.
    if (next_pass == 4) {
      groups_size_push(p_ginfos, group_size);
      p_x += group_size;
      p_o += group_size;
      p_x_aux += group_size;
      p_o_aux += group_size;
      p_bytes += group_size;
      continue;
    }

    // Order next byte of this subgroup
    int_radix_order(
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

static void int_order(int* p_x,
                      int* p_x_aux,
                      int* p_o,
                      int* p_o_aux,
                      uint8_t* p_bytes,
                      struct group_infos* p_ginfos,
                      bool decreasing,
                      bool na_last,
                      R_xlen_t size) {
  if (size < INT_INSERTION_ORDER_BOUNDARY) {
    int_adjust(p_x, decreasing, na_last, size);
    int_insertion_order(p_o, p_x, p_ginfos, size);
    return;
  }

  uint32_t range;
  int x_min;

  int_compute_range(p_x, size, &x_min, &range);

  if (range < INT_COUNTING_ORDER_RANGE_BOUNDARY) {
    int_counting_order(p_x, p_o, p_o_aux, p_ginfos, size, x_min, range, decreasing, na_last);
    return;
  }

  uint8_t pass = 0;

  int_adjust(p_x, decreasing, na_last, size);
  int_radix_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_ginfos, size, pass);
}


static void int_order_immutable(const int* p_x,
                                int* p_x_slice,
                                int* p_x_aux,
                                int* p_o,
                                int* p_o_aux,
                                uint8_t* p_bytes,
                                struct group_infos* p_ginfos,
                                bool decreasing,
                                bool na_last,
                                R_xlen_t size) {
  if (size < INT_INSERTION_ORDER_BOUNDARY) {
    memcpy(p_x_slice, p_x, size * sizeof(int));
    int_adjust(p_x_slice, decreasing, na_last, size);
    int_insertion_order(p_o, p_x_slice, p_ginfos, size);
    return;
  }

  uint32_t range;
  int x_min;

  int_compute_range(p_x, size, &x_min, &range);

  // If in counting sort range, no need to copy over to `x_slice`
  if (range < INT_COUNTING_ORDER_RANGE_BOUNDARY) {
    int_counting_order(p_x, p_o, p_o_aux, p_ginfos, size, x_min, range, decreasing, na_last);
    return;
  }

  uint8_t pass = 0;

  memcpy(p_x_slice, p_x, size * sizeof(int));
  int_adjust(p_x_slice, decreasing, na_last, size);
  int_radix_order(p_x_slice, p_x_aux, p_o, p_o_aux, p_bytes, p_ginfos, size, pass);
}


// -----------------------------------------------------------------------------

static void vec_order_immutable_switch(SEXP x,
                                       SEXP x_slice,
                                       SEXP x_aux,
                                       int* p_o,
                                       int* p_o_aux,
                                       uint8_t* p_bytes,
                                       struct group_infos* p_ginfos,
                                       bool decreasing,
                                       bool na_last,
                                       R_xlen_t size);

static void vec_col_order_switch(SEXP x,
                                 SEXP x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 struct group_infos* p_ginfos,
                                 bool decreasing,
                                 bool na_last,
                                 R_xlen_t size,
                                 const enum vctrs_type type);

/*
 * `df_order()` is the main user of `p_ginfos`. It uses the grouping
 * of the current column to break up the next column into sub groups. That
 * process is continued until either all columns have been processed or we
 * can tell all of the values apart.
 */
static void df_order(SEXP x,
                     SEXP x_slice,
                     SEXP x_aux,
                     int* p_o,
                     int* p_o_aux,
                     uint8_t* p_bytes,
                     struct group_infos* p_ginfos,
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

  // Apply on one column to fill `p_ginfos`.
  // First column is immutable and we must copy into `x_slice`.
  vec_order_immutable_switch(
    col,
    x_slice,
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
    // TODO: Minor optimization can be made by turning off the group
    // tracking when working on the last column if group info isn't requested.
    // Would probably need to track `requested` in `p_ginfos`.

    col = VECTOR_ELT(x, i);

    if (!recycle_decreasing) {
      col_decreasing = p_decreasing[i];
    }

    // Reset pointers between columns since we increment them as
    // we iterate through the groups
    int* p_o_col = p_o;
    int* p_o_aux_col = p_o_aux;

    // Get the number of group chunks from previous column group info
    struct group_info* p_ginfo_pre = groups_current(p_ginfos);
    R_xlen_t n_groups = p_ginfo_pre->n_groups;

    // If there were no ties, we are completely done
    if (n_groups == size) {
      break;
    }

    // Swap to other group info to prepare for this column
    groups_swap(p_ginfos);

    const enum vctrs_type type = vec_proxy_typeof(col);

    // `x_slice` will hold current group slice of `x`
    int* p_x_slice = INTEGER(x_slice);
    const int* p_col = INTEGER(col);

    // Iterate over this column's group chunks
    for (R_xlen_t group = 0; group < n_groups; ++group) {
      R_xlen_t group_size = p_ginfo_pre->p_data[group];

      // Fast handling of simplest case
      if (group_size == 1) {
        ++p_o_col;
        ++p_o_aux_col;
        groups_size_push(p_ginfos, 1);
        continue;
      }

      // Extract the next group chunk and place in sequential order
      // for cache friendliness
      for (R_xlen_t j = 0; j < group_size; ++j) {
        const int loc = p_o_col[j] - 1;
        p_x_slice[j] = p_col[loc];
      }

      vec_col_order_switch(
        x_slice,
        x_aux,
        p_o_col,
        p_o_aux_col,
        p_bytes,
        p_ginfos,
        col_decreasing,
        na_last,
        group_size,
        type
      );

      p_o_col += group_size;
      p_o_aux_col += group_size;
    }
  }
}

// -----------------------------------------------------------------------------

// Like `vec_order_switch()`, but specifically for columns of a data
// frame where `decreasing` is known and is scalar.
// Also assumes `x` is mutable!
static void vec_col_order_switch(SEXP x,
                                 SEXP x_aux,
                                 int* p_o,
                                 int* p_o_aux,
                                 uint8_t* p_bytes,
                                 struct group_infos* p_ginfos,
                                 bool decreasing,
                                 bool na_last,
                                 R_xlen_t size,
                                 const enum vctrs_type type) {
  switch (type) {
  case vctrs_type_integer: {
    int* p_x = INTEGER(x);
    int* p_x_aux = INTEGER(x_aux);

    int_order(p_x, p_x_aux, p_o, p_o_aux, p_bytes, p_ginfos, decreasing, na_last, size);

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


static void vec_order_switch(SEXP x,
                             SEXP x_slice,
                             SEXP x_aux,
                             int* p_o,
                             int* p_o_aux,
                             uint8_t* p_bytes,
                             struct group_infos* p_ginfos,
                             SEXP decreasing,
                             bool na_last,
                             R_xlen_t size);

/*
 * Compute the order of a vector.
 * Can optionally compute the group sizes as well.
 *
 * TODO: Return group info as attributes for type stability? Might not be
 * important since we have different R level functions.
 */
static SEXP vec_order(SEXP x, SEXP decreasing, bool na_last, bool groups) {
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
  SEXP x_slice = PROTECT(Rf_allocVector(INTSXP, size));
  SEXP x_aux = PROTECT(Rf_allocVector(INTSXP, size));

  SEXP o = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_o = INTEGER(o);

  SEXP o_aux = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_o_aux = INTEGER(o_aux);

  // Initialize `out` with sequential 1-based ordering
  for (R_xlen_t i = 0; i < size; ++i) {
    p_o[i] = i + 1;
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

  vec_order_switch(
    x,
    x_slice,
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

static void vec_order_switch(SEXP x,
                             SEXP x_slice,
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
    df_order(
      x,
      x_slice,
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
    Rf_errorcall(
      R_NilValue,
      "`decreasing` must have length 1 when `x` is not a data frame."
    );
  }

  bool c_decreasing = LOGICAL(decreasing)[0];

  vec_order_immutable_switch(
    x,
    x_slice,
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
static void vec_order_immutable_switch(SEXP x,
                                       SEXP x_slice,
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
    int* p_x = INTEGER(x);
    int* p_x_slice = INTEGER(x_slice);
    int* p_x_aux = INTEGER(x_aux);

    int_order_immutable(
      p_x,
      p_x_slice,
      p_x_aux,
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
    Rf_errorcall(R_NilValue, "Internal error: Data frames should have been handled by now");
  }
  default: {
    Rf_errorcall(R_NilValue, "This type is not supported by `vec_radix_order()`.");
  }
  }
}

// -----------------------------------------------------------------------------

#undef UINT8_MAX_SIZE

#undef GROUP_DATA_SIZE_DEFAULT

#undef INT_COUNTING_ORDER_RANGE_BOUNDARY

#undef INT_INSERTION_ORDER_BOUNDARY
