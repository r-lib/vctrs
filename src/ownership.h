#ifndef VCTRS_OWNERSHIP_H
#define VCTRS_OWNERSHIP_H

// Ownership is recursive
enum vctrs_ownership {
  vctrs_ownership_shared,
  vctrs_ownership_total
};

static inline const enum vctrs_ownership vec_ownership(SEXP x) {
  return NO_REFERENCES(x) ? vctrs_ownership_total : vctrs_ownership_shared;
}

#endif
