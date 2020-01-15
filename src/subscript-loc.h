#ifndef VCTRS_SUBSCRIPT_LOC_H
#define VCTRS_SUBSCRIPT_LOC_H


enum num_as_location_loc_negative {
  LOC_NEGATIVE_INVERT,
  LOC_NEGATIVE_ERROR,
  LOC_NEGATIVE_IGNORE
};
enum num_as_location_loc_oob {
  LOC_OOB_EXTEND,
  LOC_OOB_ERROR
};

struct vec_as_location_opts {
  enum num_as_location_loc_negative loc_negative;
  enum num_as_location_loc_oob loc_oob;
};

SEXP vec_as_location(SEXP i, R_len_t n, SEXP names);
SEXP vec_as_location_opts(SEXP i, R_len_t n, SEXP names, struct vec_as_location_opts* opts);


#endif
