#include "vctrs.h"
#include "utils.h"

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

// -----------------------------------------------------------------------------

// A bit ad hoc
#define INT_INSERTION_SIZE 256

// Used as an internal algorithm for radix sorting once we hit a slice of
// size `INSERTION_SIZE`. Should be fast for these small slices, inserts
// into `p_o` directly.
static void int_insertion_sort(int* p_o,
                               int* p_x,
                               bool decreasing,
                               bool na_last,
                               const R_xlen_t size) {
  const int na_int = na_last ? INT_MAX : INT_MIN;

  // These values come from writing out the 4 combinations of na_last/decreasing
  // and seeing how adjustments have to be made to keep `NA` as the
  // largest/smallest value. They are ugly, but should be fast.
  const int adj_before = na_last ? INT_MAX - 1 : INT_MAX;
  const int adj_after = decreasing ? 1 : (na_last ? -1 : 0);

  for (R_xlen_t i = 1; i < size; ++i) {
    const int x_elt = p_x[i];
    const int o_elt = p_o[i];

    int x_elt_mapped;

    if (x_elt == NA_INTEGER) {
      x_elt_mapped = na_int;
    } else if (decreasing) {
      x_elt_mapped = adj_before - x_elt + adj_after;
    } else {
      x_elt_mapped = x_elt + adj_after;
    }

    R_xlen_t j = i - 1;

    while (j >= 0) {
      int x_cmp_elt = p_x[j];

      int x_cmp_elt_mapped;

      if (x_cmp_elt == NA_INTEGER) {
        x_cmp_elt_mapped = na_int;
      } else if (decreasing) {
        x_cmp_elt_mapped = adj_before - x_cmp_elt + adj_after;
      } else {
        x_cmp_elt_mapped = x_cmp_elt + adj_after;
      }

      if (x_elt_mapped >= x_cmp_elt_mapped) {
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
                                 bool decreasing,
                                 bool na_last,
                                 const R_xlen_t size,
                                 const uint8_t pass) {
  // Finish this group with insertion sort once it gets small enough
  if (size <= INT_INSERTION_SIZE) {
    int_insertion_sort(p_o, p_x, decreasing, na_last, size);
    return;
  }

  const uint8_t radix = 3 - pass;
  const uint8_t shift = radix * 8;

  // Rely on `NA_INTEGER == INT_MIN`, which is mapped to `0` as a `uint32_t`.
  const uint32_t na_uint32 = na_last ? UINT32_MAX : 0;

  // These values come from writing out the 4 combinations of na_last/decreasing
  // and seeing how adjustments have to be made to keep `NA` as the
  // largest/smallest value. They are ugly, but should be fast.
  const uint32_t adj_before = na_last ? UINT32_MAX - 1 : UINT32_MAX;
  const uint32_t adj_after = decreasing ? 1 : (na_last ? -1 : 0);

  uint8_t byte = 0;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const int x_elt = p_x[i];

    uint32_t x_elt_mapped;

    if (x_elt == NA_INTEGER) {
      x_elt_mapped = na_uint32;
    } else {
      x_elt_mapped = map_from_int32_to_uint32(x_elt);

      // Adjust based on combination of `na_last` and `decreasing`
      if (decreasing) {
        x_elt_mapped = adj_before - x_elt_mapped + adj_after;
      } else {
        x_elt_mapped += adj_after;
      }
    }

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
                                 bool decreasing,
                                 bool na_last,
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
    decreasing,
    na_last,
    size,
    pass
  );

  const uint8_t next_pass = pass + 1;
  R_xlen_t last_cumulative_count = 0;

  for (uint16_t i = 0; i < UINT8_MAX_SIZE; ++i) {
    const R_xlen_t cumulative_count = p_counts[i];

    if (cumulative_count == 0) {
      continue;
    }

    // Diff the cumulated counts to get the radix group size
    const R_xlen_t group_size = cumulative_count - last_cumulative_count;
    last_cumulative_count = cumulative_count;

    if (group_size == 1) {
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
      decreasing,
      na_last,
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

SEXP int_radix_order(SEXP x, bool decreasing, bool na_last);

// [[ register() ]]
SEXP vctrs_int_radix_order(SEXP x, SEXP decreasing, SEXP na_last) {
  if (!r_is_bool(decreasing)) {
    Rf_errorcall(R_NilValue, "`decreasing` must be either `TRUE` or `FALSE`.");
  }

  bool c_decreasing = LOGICAL(decreasing)[0];

  if (!r_is_bool(na_last)) {
    Rf_errorcall(R_NilValue, "`na_last` must be either `TRUE` or `FALSE`.");
  }

  bool c_na_last = LOGICAL(na_last)[0];

  return int_radix_order(x, c_decreasing, c_na_last);
}


SEXP int_radix_order(SEXP x, bool decreasing, bool na_last) {
  if (TYPEOF(x) != INTSXP) {
    Rf_errorcall(R_NilValue, "`x` must be an integer vector.");
  }

  R_xlen_t size = Rf_xlength(x);
  const int* p_x = INTEGER_RO(x);

  // This is sometimes bigger than it needs to be, but will only
  // be much bigger than required if `x` is filled with numbers that are
  // incredibly spread out.
  SEXP x_copy = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_x_copy = INTEGER(x_copy);

  // Fill `x_slice` with `x`
  memcpy(p_x_copy, p_x, size * sizeof(int));

  SEXP x_aux = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_x_aux = INTEGER(x_aux);

  SEXP o = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_o = INTEGER(o);

  // Initialize `out` with sequential 1-based ordering
  for (R_xlen_t i = 0, j = 1; i < size; ++i, ++j) {
    p_o[i] = j;
  }

  SEXP o_aux = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_o_aux = INTEGER(o_aux);

  uint8_t* p_bytes = (uint8_t*) R_alloc(size, sizeof(uint8_t));

  const uint8_t pass = 0;

  int_radix_order_impl(
    p_x_copy,
    p_x_aux,
    p_o,
    p_o_aux,
    p_bytes,
    decreasing,
    na_last,
    size,
    pass
  );

  UNPROTECT(4);
  return o;
}

// -----------------------------------------------------------------------------


#undef UINT8_MAX_SIZE

#undef INT_INSERTION_SIZE
