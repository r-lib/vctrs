#include "vctrs.h"

// -----------------------------------------------------------------------------

#define HEX_UINT32_SIGN_BIT 0x80000000u

// [INT32_MIN, INT32_MAX] => [0, UINT32_MAX]
static inline uint32_t map_from_int32_to_uint32(int32_t x) {
  return x ^ HEX_UINT32_SIGN_BIT;
}

#undef HEX_UINT32_SIGN_BIT

// -----------------------------------------------------------------------------

static inline uint8_t extract_byte(uint32_t x, uint8_t pass) {
  return (x >> (8 * pass)) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

SEXP vctrs_int_radix_sort(SEXP x) {
  static const uint8_t n_passes = 4;

  const int* p_x = INTEGER_RO(x);

  const R_xlen_t size = Rf_xlength(x);

  // Tracks the counts of each byte seen.
  // It is a long array that gets broken into `n_passes` parts.
  R_xlen_t* p_counts = (R_xlen_t*) R_alloc(UINT8_MAX_SIZE * n_passes, sizeof(R_xlen_t));
  memset(p_counts, 0, UINT8_MAX_SIZE * n_passes * sizeof(R_xlen_t));

  // Tracks the bytes themselves, since computing them requires some extra
  // work of mapping `x` to `uint32_t` before extracting bytes.
  uint8_t* p_bytes = (uint8_t*) R_alloc(size * n_passes, sizeof(uint8_t));

  // For jumping along the bytes/counts arrays
  R_xlen_t pass_start_bytes[n_passes];
  R_xlen_t pass_start_counts[n_passes];

  for (R_xlen_t pass = 0; pass < n_passes; ++pass) {
    pass_start_bytes[pass] = size * pass;
    pass_start_counts[pass] = UINT8_MAX_SIZE * pass;
  }

  // Build 4 histograms in one pass (one for each byte)
  for (R_xlen_t i = 0; i < size; ++i) {
    const int32_t elt_x = p_x[i];
    const uint32_t elt_mapped = map_from_int32_to_uint32(elt_x);

    for (uint8_t pass = 0; pass < n_passes; ++pass) {
      const uint8_t byte = extract_byte(elt_mapped, pass);
      p_bytes[pass_start_bytes[pass] + i] = byte;
      ++p_counts[pass_start_counts[pass] + byte];
    }
  }

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  // To store intermediate results after each pass
  SEXP copy = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_copy = INTEGER(copy);

  // Initialize `out` with sequential ordering
  for (R_xlen_t i = 0; i < size; ++i) {
    p_out[i] = i;
  }

  R_xlen_t p_offsets[UINT8_MAX_SIZE];

  for (uint8_t pass = 0; pass < n_passes; ++pass) {
    const R_xlen_t start_bytes = pass_start_bytes[pass];
    const R_xlen_t start_counts = pass_start_counts[pass];

    R_xlen_t offset = 0;

    for (R_xlen_t i = 0; i < UINT8_MAX_SIZE; ++i) {
      p_offsets[i] = offset;
      offset += p_counts[start_counts + i];
    }

    for (R_xlen_t i = 0; i < size; ++i) {
      const int32_t elt = p_out[i];
      const uint8_t loc = p_bytes[start_bytes + elt];

      p_copy[p_offsets[loc]++] = elt;
    }

    // Pointer swap before next pass
    SEXP temp = out;
    out = copy;
    copy = temp;
    p_out = INTEGER(out);
    p_copy = INTEGER(copy);
  }

  // Increment to 1-based ordering
  for (R_xlen_t i = 0; i < size; ++i) {
    ++p_out[i];
  }

  UNPROTECT(2);
  return out;
}

