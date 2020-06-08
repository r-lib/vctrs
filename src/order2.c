#include "vctrs.h"
#include "utils.h"

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

// -----------------------------------------------------------------------------

// A bit ad hoc
#define INT_INSERTION_SIZE 256

// Used as an internal algorithm for radix sorting once we hit a slice of
// size `INSERTION_SIZE`. Should be fast for these small slices, inserts
// into `p_out_slice` directly.
static void int_insertion_sort(int* p_out_slice,
                               int* p_x_slice,
                               const R_xlen_t size) {
  for (R_xlen_t i = 1; i < size; ++i) {
    const int x_elt = p_x_slice[i];
    const int out_elt = p_out_slice[i];

    R_xlen_t j = i - 1;

    while (j >= 0) {
      int x_cmp_elt = p_x_slice[j];

      if (x_elt >= x_cmp_elt) {
        break;
      }

      int out_cmp_elt = p_out_slice[j];

      // Shift
      p_x_slice[j + 1] = x_cmp_elt;
      p_out_slice[j + 1] = out_cmp_elt;

      // Next
      --j;
    }

    // Place original element in updated location
    p_x_slice[j + 1] = x_elt;
    p_out_slice[j + 1] = out_elt;
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

static void int_radix_order_pass(int* p_out_slice,
                                 int* p_aux_slice,
                                 uint8_t* p_bytes,
                                 R_xlen_t* p_counts,
                                 int* p_x_slice,
                                 int* p_x_aux_slice,
                                 const R_xlen_t size,
                                 const uint8_t pass) {
  // Finish this group with insertion sort once it gets small enough
  if (size <= INT_INSERTION_SIZE) {
    int_insertion_sort(p_out_slice, p_x_slice, size);
    return;
  }

  const uint8_t radix = 3 - pass;
  const uint8_t shift = radix * 8;
  const uint32_t na_uint32 = 0;

  uint8_t byte = 0;

  // Histogram
  for (R_xlen_t i = 0; i < size; ++i) {
    const int x_elt = p_x_slice[i];

    uint32_t x_elt_mapped;

    if (x_elt == NA_INTEGER) {
      x_elt_mapped = na_uint32;
    } else {
      x_elt_mapped = map_from_int32_to_uint32(x_elt);
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
    p_aux_slice[loc] = p_out_slice[i];
    p_x_aux_slice[loc] = p_x_slice[i];
  }

  // Move from `aux` back to `out`
  // TODO: Maybe with pointer swaps?
  for (R_xlen_t i = 0; i < size; ++i) {
    p_out_slice[i] = p_aux_slice[i];
    p_x_slice[i] = p_x_aux_slice[i];
  }
}

// -----------------------------------------------------------------------------

static void int_radix_order_impl(int* p_out_slice,
                                 int* p_aux_slice,
                                 uint8_t* p_bytes,
                                 int* p_x_slice,
                                 int* p_x_aux_slice,
                                 const R_xlen_t size,
                                 const uint8_t pass) {
  R_xlen_t p_counts[UINT8_MAX_SIZE] = { 0 };

  int_radix_order_pass(
    p_out_slice,
    p_aux_slice,
    p_bytes,
    p_counts,
    p_x_slice,
    p_x_aux_slice,
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
      ++p_x_slice;
      ++p_out_slice;
      ++p_aux_slice;
      ++p_bytes;
      continue;
    }

    // Can get here in the case of ties, like c(1L, 1L), which have a
    // `group_size` of 2 in the last radix, but there is nothing left to compare
    if (next_pass == 4) {
      p_x_slice += group_size;
      p_out_slice += group_size;
      p_aux_slice += group_size;
      p_bytes += group_size;
      continue;
    }

    int_radix_order_impl(
      p_out_slice,
      p_aux_slice,
      p_bytes,
      p_x_slice,
      p_x_aux_slice,
      group_size,
      next_pass
    );

    p_x_slice += group_size;
    p_out_slice += group_size;
    p_aux_slice += group_size;
    p_bytes += group_size;
  }
}

// -----------------------------------------------------------------------------

SEXP int_radix_order(SEXP x) {
  if (TYPEOF(x) != INTSXP) {
    Rf_errorcall(R_NilValue, "`x` must be an integer vector.");
  }

  R_xlen_t size = Rf_xlength(x);
  const int* p_x = INTEGER_RO(x);

  // This is sometimes bigger than it needs to be, but will only
  // be much bigger than required if `x` is filled with numbers that are
  // incredibly spread out.
  SEXP x_slice = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_x_slice = INTEGER(x_slice);

  // Fill `x_slice` with `x`
  memcpy(p_x_slice, p_x, size * sizeof(int));

  SEXP x_aux_slice = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_x_aux_slice = INTEGER(x_aux_slice);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  // Initialize `out` with sequential 1-based ordering
  for (R_xlen_t i = 0; i < size; ++i) {
    p_out[i] = i + 1;
  }

  SEXP aux = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_aux = INTEGER(aux);

  uint8_t* p_bytes = (uint8_t*) R_alloc(size, sizeof(uint8_t));

  const uint8_t pass = 0;

  int_radix_order_impl(
    p_out,
    p_aux,
    p_bytes,
    p_x_slice,
    p_x_aux_slice,
    size,
    pass
  );

  UNPROTECT(4);
  return out;
}

// -----------------------------------------------------------------------------


#undef UINT8_MAX_SIZE

#undef INT_INSERTION_SIZE
