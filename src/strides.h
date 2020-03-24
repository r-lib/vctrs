#ifndef VCTRS_STRIDES_H
#define VCTRS_STRIDES_H

#include "vctrs.h"

/*
 * Array slicing works by treating the array as a 1D structure, and transforming
 * the `index` passed from R into a series of 1D indices that are used to
 * extract elements from `x` into the new result.
 *
 * Strides represent the offset between elements in the same dimension. For
 * a (2, 2, 2) array, the strides would be [1, 2, 4]. This means that if you
 * flattened this 3D array to 1D in a column major order, there is 1 space
 * between row elements, 2 spaces between column elements and 4 spaces between
 * elements in the third dimension. In practice, we only need the shape strides
 * since the first stride is always 1, so `vec_strides()` only returns the shape
 * strides. Strides are computed as a cumulative product of the `dim`, with an
 * initial value of `1`, this is what `vec_strides()` does.
 *
 * Using the strides, any array index can be converted to a 1D index.
 * This is what `vec_strided_loc()` does. In a (2, 2, 2) array, to find
 * the location at the index [1, 0, 1] (C-based index, 2nd row, 1st col,
 * 2nd elem in 3rd dim) you compute a sum product between the array index
 * and the strides. So it looks like:
 * loc = 1 * (1) + 0 * (2) + 1 * (4) = 5
 * (loc is a C-based index into `x`)
 * Since the first stride is always one, we leave it off and just do:
 * loc = 1 + 0 * (2) + 1 * (4) = 5
 *
 * Example:
 * x = (3, 3, 2) array
 * vec_slice(x, 2:3)
 *
 * strides = [3, 9] // (shape strides)
 *
 * Indices are C-based
 *
 *         | array index | x index | how?
 * -------------------------------------------------------
 * out[0]  | [1, 0, 0]   | 1       | 1 + 0 * (3) + 0 * (9)
 * out[1]  | [2, 0, 0]   | 2       |
 * out[2]  | [1, 1, 0]   | 4       |
 * ...     | ...         | ...     |
 * out[9]  | [2, 1, 1]   | 14      | 2 + 1 * (3) + 1 * (9)
 * out[10] | [1, 2, 1]   | 16      |
 * out[11] | [2, 2, 1]   | 17      |
 *            ^  ^  ^
 *            |   \/
 *            |    |- shape_index
 *            |- size_index
 */

static inline SEXP vec_strides(const int* p_dim, const R_len_t shape_n) {
  SEXP strides = PROTECT(Rf_allocVector(INTSXP, shape_n));
  int* p_strides = INTEGER(strides);
  int stride = 1;

  for (int i = 0; i < shape_n; ++i) {
    stride *= p_dim[i];
    p_strides[i] = stride;
  }

  UNPROTECT(1);
  return strides;
}

static inline int vec_strided_loc(const int size_index,
                                  const int* p_shape_index,
                                  const int* p_strides,
                                  const R_len_t shape_n) {
  int loc = size_index;

  for (R_len_t i = 0; i < shape_n; ++i) {
    loc += p_strides[i] * p_shape_index[i];
  }

  return loc;
}

struct vec_slice_shaped_info {
  const int* p_dim;
  const int* p_strides;
  const int* p_index;
  int* p_shape_index;
  R_len_t dim_n;
  R_len_t shape_n;
  R_len_t index_n;
  R_len_t shape_elem_n;
  SEXP out_dim;
};

#endif
