static
r_obj* vec_chop_base(r_obj* x,
                     struct vctrs_proxy_info info,
                     struct vctrs_chop_indices* p_indices);

static
r_obj* chop(r_obj* x,
            struct vctrs_proxy_info info,
            struct vctrs_chop_indices* p_indices);
static
r_obj* chop_shaped(r_obj* x,
                   struct vctrs_proxy_info info,
                   struct vctrs_chop_indices* p_indices);
static
r_obj* chop_df(r_obj* x,
               struct vctrs_proxy_info info,
               struct vctrs_chop_indices* p_indices);

static
r_obj* chop_fallback(r_obj* x, struct vctrs_chop_indices* p_indices);
static
r_obj* chop_fallback_shaped(r_obj* x, struct vctrs_chop_indices* p_indices);

static
r_obj* vec_as_chop_sizes(r_obj* sizes, r_ssize size);
