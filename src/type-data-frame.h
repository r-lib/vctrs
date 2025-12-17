#ifndef VCTRS_TYPE_DATA_FRAME_H
#define VCTRS_TYPE_DATA_FRAME_H

#include "vctrs-core.h"
#include "cast.h"
#include "names.h"
#include "ptype2.h"


r_obj* new_data_frame(r_obj* x, r_ssize n);
void init_data_frame(r_obj* x, r_ssize n);
void init_tibble(r_obj* x, r_ssize n);
void init_compact_rownames(r_obj* x, r_ssize n);

static inline
r_obj* df_rownames(r_obj* x) {
  return r_attrib_get(x, R_RowNamesSymbol);
}

bool is_native_df(r_obj* x);
r_obj* df_poke(r_obj* x, r_ssize i, r_obj* value);
r_obj* df_poke_at(r_obj* x, r_obj* name, r_obj* value);
r_obj* df_flatten(r_obj* x);
r_obj* df_repair_names(r_obj* x, struct name_repair_opts* name_repair);

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
  ROWNAMES_TYPE_automatic,
  ROWNAMES_TYPE_automatic_compact,
  ROWNAMES_TYPE_identifiers
};
enum rownames_type rownames_type(r_obj* rn);
r_ssize rownames_size(r_obj* rn);

r_obj* df_ptype2(
  r_obj* x,
  r_obj* y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg,
  struct r_lazy call,
  enum s3_fallback s3_fallback
);

#endif
