static r_obj* vec_chop_base(r_obj* x, r_obj* indices, struct vctrs_chop_info info);

static r_obj* chop(r_obj* x, r_obj* indices, struct vctrs_chop_info info);
static r_obj* chop_shaped(r_obj* x, r_obj* indices, struct vctrs_chop_info info);
static r_obj* chop_df(r_obj* x, r_obj* indices, struct vctrs_chop_info info);
static r_obj* chop_fallback(r_obj* x, r_obj* indices, struct vctrs_chop_info info);
static r_obj* chop_fallback_shaped(r_obj* x, r_obj* indices, struct vctrs_chop_info info);
