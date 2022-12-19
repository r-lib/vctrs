#include "vctrs.h"

#include "decl/set2-decl.h"

r_obj* ffi_vec_set_intersect2(r_obj* x,
                              r_obj* y,
                              r_obj* ptype,
                              r_obj* frame) {
  struct r_lazy call = { .x = r_syms.error_call, .env = frame };

  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy y_arg_lazy = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_lazy);

  return vec_set_intersect2(x, y, ptype, &x_arg, &y_arg, call);
}

r_obj* vec_set_intersect2(r_obj* x,
                          r_obj* y,
                          r_obj* ptype,
                          struct vctrs_arg* x_arg,
                          struct vctrs_arg* y_arg,
                          struct r_lazy call) {
  int n_prot = 0;

  if (ptype == r_null) {
    int _;

    ptype = vec_ptype2_params(
      x,
      y,
      x_arg,
      y_arg,
      call,
      &_
    );
    KEEP_N(ptype, &n_prot);

    ptype = vec_ptype_finalise(ptype);
    KEEP_N(ptype, &n_prot);
  }

  x = vec_cast_params(
    x,
    ptype,
    x_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(x, &n_prot);

  y = vec_cast_params(
    y,
    ptype,
    y_arg,
    vec_args.empty,
    call,
    S3_FALLBACK_false
  );
  KEEP_N(y, &n_prot);

  r_obj* args = KEEP_N(vec_joint_proxy_order(x, y), &n_prot);

  r_obj* x_proxy = r_list_get(args, 0);
  x_proxy = KEEP_N(vec_normalize_encoding(x_proxy), &n_prot);

  r_obj* y_proxy = r_list_get(args, 1);
  y_proxy = KEEP_N(vec_normalize_encoding(y_proxy), &n_prot);

  // `na_value = "smallest"` to match comparison ordering
  r_obj* x_info = vec_order_info(
    x_proxy,
    chrs_asc,
    chrs_smallest,
    true,
    r_null,
    true
  );
  KEEP_N(x_info, &n_prot);

  r_obj* y_info = vec_order_info(
    y_proxy,
    chrs_asc,
    chrs_smallest,
    true,
    r_null,
    true
  );
  KEEP_N(y_info, &n_prot);

  r_obj* x_o = r_list_get(x_info, 0);
  const int* v_x_o = r_int_cbegin(x_o);

  r_obj* y_o = r_list_get(y_info, 0);
  const int* v_y_o = r_int_cbegin(y_o);

  // These are really group sizes, which we accumulate into group starts
  r_obj* x_group_starts = r_list_get(x_info, 1);
  int* v_x_group_starts = r_int_begin(x_group_starts);

  r_obj* y_group_starts = r_list_get(y_info, 1);
  int* v_y_group_starts = r_int_begin(y_group_starts);

  const r_ssize n_x_group_starts = r_length(x_group_starts);
  const r_ssize n_y_group_starts = r_length(y_group_starts);

  r_ssize start = 0;

  for (r_ssize i = 0; i < n_x_group_starts; ++i) {
    const r_ssize size = v_x_group_starts[i];
    v_x_group_starts[i] = start;
    start += size;
  }

  start = 0;

  for (r_ssize i = 0; i < n_y_group_starts; ++i) {
    const r_ssize size = v_y_group_starts[i];
    v_y_group_starts[i] = start;
    start += size;
  }

  const r_ssize x_size = vec_size(x_proxy);
  const enum vctrs_type type = vec_proxy_typeof(x_proxy);

  struct poly_vec* p_poly_x = new_poly_vec(x_proxy, type);
  KEEP_N(p_poly_x->shelter, &n_prot);
  const void* p_x = p_poly_x->p_vec;

  struct poly_vec* p_poly_y = new_poly_vec(y_proxy, type);
  KEEP_N(p_poly_y->shelter, &n_prot);
  const void* p_y = p_poly_y->p_vec;

  r_obj* marked_shelter = KEEP_N(r_alloc_raw(x_size * sizeof(bool)), &n_prot);
  bool* v_marked = (bool*) r_raw_begin(marked_shelter);
  memset(v_marked, 0, x_size * sizeof(bool));

  const r_ssize n_marked = mark_intersections(
    type,
    p_x,
    p_y,
    v_x_o,
    v_y_o,
    v_x_group_starts,
    v_y_group_starts,
    n_x_group_starts,
    n_y_group_starts,
    v_marked
  );

  r_obj* loc = KEEP_N(r_alloc_integer(n_marked), &n_prot);
  int* v_loc = r_int_begin(loc);
  r_ssize j = 0;

  for (r_ssize i = 0; i < x_size; ++i) {
    v_loc[j] = i + 1;
    j += v_marked[i];
  }

  r_obj* out = vec_slice_unsafe(x, loc);

  FREE(n_prot);
  return out;
}

#define MARK_INTERSECTIONS(FN_P_COMPARE) do {                                              \
  while (x_group_starts_loc < n_x_group_starts && y_group_starts_loc < n_y_group_starts) { \
    const r_ssize x_group_start = v_x_group_starts[x_group_starts_loc];                    \
    const r_ssize y_group_start = v_y_group_starts[y_group_starts_loc];                    \
                                                                                           \
    const r_ssize x_loc = v_x_o[x_group_start] - 1;                                        \
    const r_ssize y_loc = v_y_o[y_group_start] - 1;                                        \
                                                                                           \
    const int cmp = FN_P_COMPARE(p_x, x_loc, p_y, y_loc, true);                            \
                                                                                           \
    if (cmp == 0) {                                                                        \
      v_marked[x_loc] = true;                                                              \
      ++n_marked;                                                                          \
      ++x_group_starts_loc;                                                                \
      ++y_group_starts_loc;                                                                \
    } else if (cmp == -1) {                                                                \
      ++x_group_starts_loc;                                                                \
    } else {                                                                               \
      ++y_group_starts_loc;                                                                \
    }                                                                                      \
  }                                                                                        \
} while (0)

static
r_ssize mark_intersections(enum vctrs_type type,
                           const void* p_x,
                           const void* p_y,
                           const int* v_x_o,
                           const int* v_y_o,
                           const int* v_x_group_starts,
                           const int* v_y_group_starts,
                           r_ssize n_x_group_starts,
                           r_ssize n_y_group_starts,
                           bool* v_marked) {
  r_ssize x_group_starts_loc = 0;
  r_ssize y_group_starts_loc = 0;

  r_ssize n_marked = 0;

  switch (type) {
  case VCTRS_TYPE_logical: MARK_INTERSECTIONS(p_lgl_order_compare_na_equal); break;
  case VCTRS_TYPE_integer: MARK_INTERSECTIONS(p_int_order_compare_na_equal); break;
  case VCTRS_TYPE_double: MARK_INTERSECTIONS(p_dbl_order_compare_na_equal); break;
  case VCTRS_TYPE_complex: MARK_INTERSECTIONS(p_cpl_order_compare_na_equal); break;
  case VCTRS_TYPE_character: MARK_INTERSECTIONS(p_chr_order_compare_na_equal); break;
  case VCTRS_TYPE_dataframe: MARK_INTERSECTIONS(p_df_order_compare_na_equal); break;
  default: stop_unimplemented_vctrs_type("mark_intersections", type);
  }

  return n_marked;
}

#undef MARK_INTERSECTIONS
