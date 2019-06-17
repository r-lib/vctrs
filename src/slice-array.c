#include "vctrs.h"
#include "utils.h"

SEXP vec_strides(SEXP dim) {
  int* p_dim = INTEGER(dim);
  R_len_t dim_n = Rf_length(dim);
  SEXP strides = PROTECT(Rf_allocVector(INTSXP, dim_n));
  int* p_strides = INTEGER(strides);
  int stride = 1;

  for (int i = 0; i < dim_n; ++i) {
    p_strides[i] = stride;
    stride *= p_dim[i];
  }

  UNPROTECT(1);
  return strides;
}

// The size_index/shape_index are C indexed, but this function returns
// an R based index for vec_slice() to use
int vec_strided_loc(int size_index, SEXP shape_index, SEXP strides) {
  if (size_index == NA_INTEGER) {
    return NA_INTEGER;
  }

  int* p_strides = INTEGER(strides);
  int* p_shape_index = INTEGER(shape_index);
  R_len_t n = Rf_length(strides);
  int loc = size_index;

  for (int i = 1; i < n; ++i) {
    loc += p_strides[i] * p_shape_index[i - 1];
  }

  return loc;
}

SEXP vec_slice_shaped(SEXP x, SEXP index) {

  int* p_index = INTEGER(index);
  R_len_t index_n = Rf_length(index);

  SEXP dim = PROTECT(vec_dim(x));
  int* p_dim = INTEGER(dim);
  R_len_t dim_n = Rf_length(dim);

  SEXP strides = PROTECT(vec_strides(dim));

  R_len_t shape_n = dim_n - 1;
  SEXP shape = PROTECT(Rf_allocVector(INTSXP, shape_n));
  int* p_shape = INTEGER(shape);

  for (int i = 1; i < dim_n; ++i) {
    p_shape[i - 1] = p_dim[i];
  }

  R_len_t out_n = index_n;
  R_len_t n_shape_elements = 1;

  for (int i = 0; i < shape_n; ++i) {
    n_shape_elements *= p_shape[i];
  }

  out_n *= n_shape_elements;

  SEXP out_dim = PROTECT(Rf_allocVector(INTSXP, dim_n));
  int* p_out_dim = INTEGER(out_dim);
  p_out_dim[0] = index_n;

  for (int i = 0; i < shape_n; ++i) {
    p_out_dim[i + 1] = p_shape[i];
  }

  int slice_index_pos = 0;
  SEXP slice_index = PROTECT(Rf_allocVector(INTSXP, out_n));
  int* p_slice_index = INTEGER(slice_index);

  SEXP shape_index = PROTECT(Rf_allocVector(INTSXP, shape_n));
  int* p_shape_index = INTEGER(shape_index);

  // Initialize shape index to 0
  for (int i = 0; i < shape_n; ++i) {
    p_shape_index[i] = 0;
  }

  for (int i = 0; i < n_shape_elements; ++i) {

    // Add next 1-D slice position
    for (int j = 0; j < index_n; ++j) {
      int size_index = p_index[j];
      p_slice_index[slice_index_pos] = vec_strided_loc(size_index, shape_index, strides);
      slice_index_pos++;
    }

    // Update shape index
    for (int j = 0; j < shape_n; ++j) {
      p_shape_index[j]++;
      if (p_shape_index[j] < p_shape[j]) {
        break;
      }
      p_shape_index[j] = 0;
    }
  }

  // duplicate for now before removing dim attrib
  x = Rf_shallow_duplicate(x);
  SET_ATTRIB(x, R_NilValue);

  SEXP out = PROTECT(vec_slice(x, slice_index));

  Rf_setAttrib(out, R_DimSymbol, out_dim);

  UNPROTECT(7);
  return out;
}








