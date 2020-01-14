#ifndef VCTRS_SUBSCRIPT_LOC_H
#define VCTRS_SUBSCRIPT_LOC_H


enum num_as_location_negative {
  LOC_NEGATIVE_INVERT,
  LOC_NEGATIVE_ERROR,
  LOC_NEGATIVE_IGNORE
};

struct vec_as_location_opts {
  enum num_as_location_negative negative;
};

SEXP vec_as_location(SEXP i, R_len_t n, SEXP names, SEXP arg);
SEXP vec_as_location_opts(SEXP i, R_len_t n, SEXP names,
                          struct vec_as_location_opts* opts,
                          SEXP arg);


#endif
