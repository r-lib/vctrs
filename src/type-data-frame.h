#ifndef VCTRS_TYPE_DATA_FRAME_H
#define VCTRS_TYPE_DATA_FRAME_H

#include "vctrs-core.h"


SEXP new_data_frame(SEXP x, R_len_t n);
void init_data_frame(SEXP x, R_len_t n);
void init_tibble(SEXP x, R_len_t n);
void init_compact_rownames(SEXP x, R_len_t n);

static inline
SEXP df_rownames(SEXP x) {
  return r_attrib_get(x, R_RowNamesSymbol);
}

bool is_native_df(SEXP x);
SEXP df_poke(SEXP x, R_len_t i, SEXP value);
SEXP df_poke_at(SEXP x, SEXP name, SEXP value);
SEXP df_flatten(SEXP x);
SEXP df_repair_names(SEXP x, struct name_repair_opts* name_repair);

r_obj* df_cast_opts(const struct cast_opts* opts);

static inline
r_obj* df_cast(r_obj* x,
               r_obj* to,
               struct vctrs_arg* p_x_arg,
               struct vctrs_arg* p_to_arg) {
  const struct cast_opts opts = {
    .x = x,
    .to = to,
    .p_x_arg = p_x_arg,
    .p_to_arg = p_to_arg
  };

  return df_cast_opts(&opts);
}

enum rownames_type {
  ROWNAMES_AUTOMATIC,
  ROWNAMES_AUTOMATIC_COMPACT,
  ROWNAMES_IDENTIFIERS
};
enum rownames_type rownames_type(SEXP rn);
R_len_t rownames_size(SEXP rn);


SEXP df_ptype2(const struct ptype2_opts* opts);

static inline
SEXP df_ptype2_params(SEXP x,
                      SEXP y,
                      struct vctrs_arg* p_x_arg,
                      struct vctrs_arg* p_y_arg,
                      enum df_fallback df_fallback) {
  const struct ptype2_opts opts = {
    .x = x,
    .y = y,
    .p_x_arg = p_x_arg,
    .p_y_arg = p_y_arg,
    .fallback = {
      .df = df_fallback
    }
  };
  return df_ptype2(&opts);
}


#endif
