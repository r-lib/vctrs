#ifndef VCTRS_CORE_H
#define VCTRS_CORE_H

#include <rlang.h>
#include "globals.h"
#include "rlang-dev.h"
#include "type-info.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>


extern bool vctrs_debug_verbose;

#define VCTRS_ASSERT(condition) ((void)sizeof(char[1 - 2*!(condition)]))


// An ERR indicates either a C NULL in case of no error, or a
// condition object otherwise
#define ERR SEXP


/**
 * Ownership modeling
 *
 * Shallow and deep ownership imply that "we" own the object, and is not
 * dependent on the refcount in any way.
 *
 * Foreign ownership implies that R owns the object, and can only be modified in
 * place if the refcount is 0.
 */
enum vctrs_ownership {
  // No known ownership
  //
  // The object is "foreign" to us, typically meaning it came through FFI
  // (_foreign_ function interface) from the R side.
  //
  // If there are any references on this object, it will be cloned before being
  // modified, otherwise it will still be modified in place without cloning.
  VCTRS_OWNERSHIP_foreign,

  // Shallow ownership
  // - For atomics, we own the vector
  // - For lists (data frames), we own the list, but not the contents (columns)
  VCTRS_OWNERSHIP_shallow,

  // Deep ownership
  // We own the object recursively. Only used when we create it fully at C level.
  VCTRS_OWNERSHIP_deep
};

/**
 * Index style
 *
 * - Location indices can be integer, character, or logical, but are ultimately
 *   converted to positive integer locations by `vec_as_location()` before the
 *   core assignment loop.
 *
 * - Condition indices are logical vectors the same size as `x`, where `TRUE`
 *   denotes that you should assign to that spot. They are not converted to
 *   integer locations before assignment.
 *
 * `vec_assign()` has separate optimized paths for each index style.
 *
 * TODO: `vec_slice()` should also have an optimized path for condition indices!
 * i.e. `vec_slice(x, <lgl>)` should not call `vec_as_location()`.
 *
 * Condition indices are the inputs to `dplyr::if_else()` and `dplyr::case_when()`,
 * so having an optimized path for these is very helpful.
 */
enum vctrs_index_style {
  VCTRS_INDEX_STYLE_location,
  VCTRS_INDEX_STYLE_condition
};

/**
 * Structure for argument tags
 *
 * Argument tags are used in error messages to provide information
 * about which elements of nested data structures (such as tibbles)
 * fail to match a given type. They are generated lazily by the `fill`
 * method in order to avoid any cost when there is no error.
 *
 * @member parent The previously active argument tag.
 * @member fill Takes a pointer to data, and a buffer to fill. If the
 *   buffer is too small according to the `remaining` argument,
 *   `fill()` must return a negative error value.
 */
struct vctrs_arg {
  r_obj* shelter;
  struct vctrs_arg* parent;
  r_ssize (*fill)(void* data, char* buf, r_ssize remaining);
  void* data;
};

struct vec_error_opts {
  struct vctrs_arg* p_arg;
  struct r_lazy call;
};


// Annex F of C99 specifies that `double` should conform to the IEEE 754
// type `binary64`, which is defined as:
// * 1  bit : sign
// * 11 bits: exponent
// * 52 bits: significand
//
// R stores the value "1954" in the last 32 bits: this payload marks
// the value as a NA, not a regular NaN.
//
// On big endian systems, this corresponds to the second element of an
// integer array of size 2. On little endian systems, this is flipped
// and the NA marker is in the first element.
//
// The type assumptions made here are asserted in `vctrs_init_utils()`

#ifdef WORDS_BIGENDIAN
static const int vctrs_indicator_pos = 1;
#else
static const int vctrs_indicator_pos = 0;
#endif

union vctrs_dbl_indicator {
  double value;        // 8 bytes
  unsigned int key[2]; // 4 * 2 bytes
};

enum vctrs_dbl {
  VCTRS_DBL_number,
  VCTRS_DBL_missing,
  VCTRS_DBL_nan
};

// Inlining `dbl_classify()` greatly improves `vec_match()` performance
// with doubles!
static inline
enum vctrs_dbl dbl_classify(double x) {
  if (!isnan(x)) {
    return VCTRS_DBL_number;
  }

  union vctrs_dbl_indicator indicator;
  indicator.value = x;

  if (indicator.key[vctrs_indicator_pos] == 1954) {
    return VCTRS_DBL_missing;
  } else {
    return VCTRS_DBL_nan;
  }
}


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

// Likely supplied in R 4.4.0
// https://github.com/wch/r-source/commit/38403c9c347dd5426da6009573b087188ec6be04
#ifndef R_PRIdXLEN_T
# ifdef LONG_VECTOR_SUPPORT
#  define R_PRIdXLEN_T "td"
# else
#  define R_PRIdXLEN_T "d"
# endif
#endif


#endif
