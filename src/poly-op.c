#include "vctrs.h"
#include "decl/poly-op-decl.h"

// TODO: Remove in favor of inlining `p_*_compare_na_equal()`
// through the use of a macro. As is, this prevents significant
// inlining optimizations. Currently only used in `interval.c`.
poly_binary_int_fn* poly_p_compare_na_equal(enum vctrs_type type) {
  switch (type) {
  case VCTRS_TYPE_null: return p_nil_compare_na_equal;
  case VCTRS_TYPE_logical: return p_lgl_compare_na_equal;
  case VCTRS_TYPE_integer: return p_int_compare_na_equal;
  case VCTRS_TYPE_double: return p_dbl_compare_na_equal;
  case VCTRS_TYPE_complex: return p_cpl_compare_na_equal;
  case VCTRS_TYPE_character: return p_chr_compare_na_equal;
  case VCTRS_TYPE_raw: return p_raw_compare_na_equal;
  case VCTRS_TYPE_list: return p_list_compare_na_equal;
  case VCTRS_TYPE_dataframe: return p_df_compare_na_equal;
  default: stop_unimplemented_vctrs_type("poly_p_compare_na_equal", type);
  }
}

// TODO: Remove in favor of inlining `p_*_is_missing()`
// through the use of a macro. As is, this prevents significant
// inlining optimizations. Currently only used in `interval.c`.
poly_unary_bool_fn* poly_p_is_missing(enum vctrs_type type) {
  switch (type) {
  case VCTRS_TYPE_null: return p_nil_is_missing;
  case VCTRS_TYPE_logical: return p_lgl_is_missing;
  case VCTRS_TYPE_integer: return p_int_is_missing;
  case VCTRS_TYPE_double: return p_dbl_is_missing;
  case VCTRS_TYPE_complex: return p_cpl_is_missing;
  case VCTRS_TYPE_character: return p_chr_is_missing;
  case VCTRS_TYPE_raw: return p_raw_is_missing;
  case VCTRS_TYPE_list: return p_list_is_missing;
  case VCTRS_TYPE_dataframe: return p_df_is_missing;
  default: stop_unimplemented_vctrs_type("poly_p_is_missing", type);
  }
}

struct poly_vec* new_poly_vec(r_obj* proxy, enum vctrs_type type) {
  r_obj* shelter = KEEP(r_alloc_list(2));

  r_obj* self = r_alloc_raw(sizeof(struct poly_vec));
  r_list_poke(shelter, 0, self);
  r_list_poke(shelter, 1, proxy);

  struct poly_vec* p_poly_vec = r_raw_begin(self);

  p_poly_vec->shelter = shelter;
  p_poly_vec->vec = proxy;
  p_poly_vec->type = type;

  switch (type) {
  case VCTRS_TYPE_null: init_nil_poly_vec(p_poly_vec); break;
  case VCTRS_TYPE_logical: init_lgl_poly_vec(p_poly_vec); break;
  case VCTRS_TYPE_integer: init_int_poly_vec(p_poly_vec); break;
  case VCTRS_TYPE_double: init_dbl_poly_vec(p_poly_vec); break;
  case VCTRS_TYPE_complex: init_cpl_poly_vec(p_poly_vec); break;
  case VCTRS_TYPE_character: init_chr_poly_vec(p_poly_vec); break;
  case VCTRS_TYPE_raw: init_raw_poly_vec(p_poly_vec); break;
  case VCTRS_TYPE_list: init_list_poly_vec(p_poly_vec); break;
  case VCTRS_TYPE_dataframe: init_df_poly_vec(p_poly_vec); break;
  default: stop_unimplemented_vctrs_type("new_poly_vec", type);
  }

  FREE(1);
  return p_poly_vec;
}

static
void init_nil_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = NULL;
}
static
void init_lgl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) r_lgl_cbegin(p_poly_vec->vec);
}
static
void init_int_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) r_int_cbegin(p_poly_vec->vec);
}
static
void init_dbl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) r_dbl_cbegin(p_poly_vec->vec);
}
static
void init_cpl_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) r_cpl_cbegin(p_poly_vec->vec);
}
static
void init_chr_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) r_chr_cbegin(p_poly_vec->vec);
}
static
void init_raw_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) r_raw_cbegin(p_poly_vec->vec);
}
static
void init_list_poly_vec(struct poly_vec* p_poly_vec) {
  p_poly_vec->p_vec = (const void*) r_list_cbegin(p_poly_vec->vec);
}
static
void init_df_poly_vec(struct poly_vec* p_poly_vec) {
  r_obj* df = p_poly_vec->vec;
  r_ssize n_col = r_length(df);

  r_obj* shelter = KEEP(r_alloc_list(4));

  r_list_poke(shelter, 0, p_poly_vec->shelter);
  p_poly_vec->shelter = shelter;

  r_obj* data_handle = KEEP(r_alloc_raw(sizeof(struct poly_df_data)));
  struct poly_df_data* data = (struct poly_df_data*) r_raw_begin(data_handle);
  r_list_poke(shelter, 1, data_handle);

  r_obj* col_type_handle = KEEP(r_alloc_raw(n_col * sizeof(enum vctrs_type)));
  enum vctrs_type* v_col_type = (enum vctrs_type*) r_raw_begin(col_type_handle);
  r_list_poke(shelter, 2, col_type_handle);

  r_obj* col_ptr_handle = KEEP(r_alloc_raw(n_col * sizeof(void*)));
  const void** v_col_ptr = (const void**) r_raw_begin(col_ptr_handle);
  r_list_poke(shelter, 3, col_ptr_handle);

  for (r_ssize i = 0; i < n_col; ++i) {
    r_obj* col = r_list_get(df, i);
    v_col_type[i] = vec_proxy_typeof(col);
    v_col_ptr[i] = r_vec_cbegin(col);
  }

  data->v_col_type = v_col_type;
  data->v_col_ptr = v_col_ptr;
  data->n_col = n_col;

  p_poly_vec->p_vec = (void*) data;

  FREE(4);
}
