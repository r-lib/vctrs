#ifndef VCTRS_LAZY_H
#define VCTRS_LAZY_H

#include "vctrs-core.h"

// -----------------------------------------------------------------------------

/*
 * @member self A RAWSXP for the struct memory.
 * @member data The RAWSXP that gets allocated lazily.
 * @member p_data A void pointer to the RAWSXP.
 * @member data_pi A protection index to `data` so it can reprotect itself
 *   upon allocation.
 * @member size The total size of the RAWSXP to allocate.
 *   This is computed as `size * n_bytes` in `new_lazy_raw()`, where `n_bytes`
 *   is from `sizeof(<type>)`.
 */
struct lazy_raw {
  SEXP self;
  SEXP data;
  void* p_data;
  PROTECT_INDEX data_pi;
  r_ssize size;
};

/*
 * @param size The size of the type you want to interpret the memory as.
 * @param n_bytes A `sizeof(<type>)` result for the type you are allocating
 *   memory for.
 */
static inline
struct lazy_raw* new_lazy_raw(r_ssize size, size_t n_bytes) {
  SEXP self = PROTECT(r_new_raw(sizeof(struct lazy_raw)));
  struct lazy_raw* p_out = (struct lazy_raw*) RAW(self);

  p_out->self = self;
  p_out->data = R_NilValue;
  p_out->size = size * n_bytes;

  UNPROTECT(1);
  return p_out;
}

/*
 * Allocate the lazy vector if it hasn't already been allocated.
 * This reprotects itself using the protection index.
 */
static inline
void* init_lazy_raw(struct lazy_raw* p_x) {
  if (p_x->data != R_NilValue) {
    return p_x->p_data;
  }

  p_x->data = Rf_allocVector(RAWSXP, p_x->size);
  REPROTECT(p_x->data, p_x->data_pi);

  p_x->p_data = (void*) RAW(p_x->data);

  return p_x->p_data;
}

// -----------------------------------------------------------------------------

/*
 * @member self A RAWSXP for the struct memory.
 * @member data The STRSXP that gets allocated lazily.
 * @member p_data A constant pointer to `data`. Modification to `data` should
 *   be done using `SET_STRING_ELT()`.
 * @member data_pi A protection index to `data` so it can reprotect itself
 *   upon allocation.
 * @member size The total size of the STRSXP to allocate.
 */
struct lazy_chr {
  SEXP self;
  SEXP data;
  const SEXP* p_data;
  PROTECT_INDEX data_pi;
  r_ssize size;
};

static inline
struct lazy_chr* new_lazy_chr(r_ssize size) {
  SEXP self = PROTECT(r_new_raw(sizeof(struct lazy_chr)));
  struct lazy_chr* p_out = (struct lazy_chr*) RAW(self);

  p_out->self = self;
  p_out->data = R_NilValue;
  p_out->size = size;

  UNPROTECT(1);
  return p_out;
}

static inline
const SEXP* init_lazy_chr(struct lazy_chr* p_x) {
  if (p_x->data != R_NilValue) {
    return p_x->p_data;
  }

  p_x->data = Rf_allocVector(STRSXP, p_x->size);
  REPROTECT(p_x->data, p_x->data_pi);

  p_x->p_data = STRING_PTR_RO(p_x->data);

  return p_x->p_data;
}

// -----------------------------------------------------------------------------


#define PROTECT_LAZY_VEC(p_info, p_n) do {                \
  PROTECT((p_info)->self);                                \
  PROTECT_WITH_INDEX((p_info)->data, &(p_info)->data_pi); \
  *(p_n) += 2;                                            \
} while (0)


#endif
