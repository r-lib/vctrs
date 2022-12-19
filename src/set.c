#include "vctrs.h"

#include "decl/set-decl.h"

r_obj* ffi_vec_set_intersect(r_obj* x,
                             r_obj* y,
                             r_obj* ptype,
                             r_obj* frame) {
  struct r_lazy call = { .x = r_syms.error_call, .env = frame };

  struct r_lazy x_arg_lazy = { .x = syms.x_arg, .env = frame };
  struct vctrs_arg x_arg = new_lazy_arg(&x_arg_lazy);

  struct r_lazy y_arg_lazy = { .x = syms.y_arg, .env = frame };
  struct vctrs_arg y_arg = new_lazy_arg(&y_arg_lazy);

  return vec_set_intersect(x, y, ptype, &x_arg, &y_arg, call);
}

r_obj* vec_set_intersect(r_obj* x,
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

  r_obj* x_proxy = KEEP_N(vec_proxy_equal(x), &n_prot);
  x_proxy = KEEP_N(vec_normalize_encoding(x_proxy), &n_prot);

  r_obj* y_proxy = KEEP_N(vec_proxy_equal(y), &n_prot);
  y_proxy = KEEP_N(vec_normalize_encoding(y_proxy), &n_prot);

  const r_ssize x_size = vec_size(x_proxy);
  const r_ssize y_size = vec_size(y_proxy);

  struct dictionary* x_dict = new_dictionary(x_proxy);
  PROTECT_DICT(x_dict, &n_prot);

  // Load dictionary with `x`.
  // Key values point to first time we saw that `x` value.
  for (r_ssize i = 0; i < x_size; ++i) {
    const uint32_t hash = dict_hash_scalar(x_dict, i);

    if (x_dict->key[hash] == DICT_EMPTY) {
      dict_put(x_dict, hash, i);
    }
  }

  struct dictionary* y_dict = new_dictionary_partial(y_proxy);
  PROTECT_DICT(y_dict, &n_prot);

  r_obj* marked_shelter = KEEP_N(r_alloc_raw(x_size * sizeof(bool)), &n_prot);
  bool* v_marked = (bool*) r_raw_begin(marked_shelter);
  memset(v_marked, 0, x_size * sizeof(bool));

  // Mark unique elements of `x` that are also in `y`
  for (r_ssize i = 0; i < y_size; ++i) {
    const uint32_t hash = dict_hash_with(x_dict, y_dict, i);
    const r_ssize loc = x_dict->key[hash];

    if (loc != DICT_EMPTY) {
      v_marked[loc] = true;
    }
  }

  r_ssize n_marked = 0;
  for (r_ssize i = 0; i < x_size; ++i) {
    n_marked += v_marked[i];
  }

  r_obj* loc = KEEP_N(r_alloc_integer(n_marked), &n_prot);
  int* v_loc = r_int_begin(loc);
  r_ssize j = 0;

  for (r_ssize i = 0; i < x_size; ++i) {
    if (v_marked[i]) {
      v_loc[j] = i + 1;
      ++j;
    }
  }

  r_obj* out = vec_slice_unsafe(x, loc);

  FREE(n_prot);
  return out;
}
