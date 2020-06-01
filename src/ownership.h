#ifndef VCTRS_OWNERSHIP_H
#define VCTRS_OWNERSHIP_H

// Ownership is recursive
enum vctrs_ownership {
  VCTRS_OWNERSHIP_shared,
  VCTRS_OWNERSHIP_total
};

static inline const enum vctrs_ownership vec_ownership(SEXP x) {
  return NO_REFERENCES(x) ? VCTRS_OWNERSHIP_total : VCTRS_OWNERSHIP_shared;
}

#endif
