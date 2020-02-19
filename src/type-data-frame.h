#ifndef VCTRS_TYPE_DATA_FRAME_H
#define VCTRS_TYPE_DATA_FRAME_H


SEXP new_data_frame(SEXP x, R_len_t n);
void init_data_frame(SEXP x, R_len_t n);
void init_tibble(SEXP x, R_len_t n);
void init_compact_rownames(SEXP x, R_len_t n);
SEXP df_rownames(SEXP x);

bool is_native_df(SEXP x);
R_len_t compact_rownames_length(SEXP x);
SEXP df_container_type(SEXP x);
SEXP df_poke(SEXP x, R_len_t i, SEXP value);
SEXP df_poke_at(SEXP x, SEXP name, SEXP value);
R_len_t df_flat_width(SEXP x);

enum rownames_type {
  ROWNAMES_AUTOMATIC,
  ROWNAMES_AUTOMATIC_COMPACT,
  ROWNAMES_IDENTIFIERS
};
enum rownames_type rownames_type(SEXP rn);


#endif
