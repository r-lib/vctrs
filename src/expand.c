#include "vctrs.h"
#include "type-data-frame.h"

#include "decl/expand-decl.h"

r_obj* ffi_vec_expand_grid(r_obj* xs,
                           r_obj* ffi_vary,
                           r_obj* ffi_name_repair,
                           r_obj* frame) {
  struct r_lazy error_call = { .x = syms.dot_error_call, .env = frame };

  enum vctrs_expand_vary vary = parse_vary(ffi_vary);

  struct name_repair_opts name_repair_opts = new_name_repair_opts(
    ffi_name_repair,
    lazy_args.dot_name_repair,
    false,
    error_call
  );
  KEEP(name_repair_opts.shelter);

  r_obj* out = vec_expand_grid(xs, vary, &name_repair_opts, error_call);

  FREE(1);
  return out;
}

r_obj* vec_expand_grid(r_obj* xs,
                       enum vctrs_expand_vary vary,
                       const struct name_repair_opts* p_name_repair_opts,
                       struct r_lazy error_call) {
  vec_check_list(xs, vec_args.empty, error_call);

  if (vec_any_missing(xs)) {
    // Drop `NULL`s before any other checks
    r_obj* complete = KEEP(vec_detect_complete(xs));
    xs = vec_slice(xs, complete);
    FREE(1);
  }
  KEEP(xs);

  const r_ssize n = r_length(xs);

  r_obj* out = KEEP(r_alloc_list(n));

  r_obj* names = KEEP(vec_names2(xs));
  if (!r_is_minimal_names(names)) {
    r_abort_lazy_call(error_call, "Each element must be named.");
  }
  names = vec_as_names(names, p_name_repair_opts);
  r_attrib_poke_names(out, names);

  const struct vec_error_opts error_opts = {
    .p_arg = vec_args.empty,
    .call = error_call
  };

  r_obj* sizes = KEEP(list_sizes(xs, &error_opts));
  const int* v_sizes = r_int_cbegin(sizes);

  r_obj* cumulative = KEEP(r_alloc_raw(n * sizeof(r_ssize)));
  r_ssize* v_cumulative = r_raw_begin(cumulative);

  r_ssize size = 1;

  for (r_ssize i = 0; i < n; ++i) {
    size = r_ssize_mult(size, v_sizes[i]);
    v_cumulative[i] = size;
  }

  // TODO: Support long vectors here
  if (size > R_SHORT_LEN_MAX) {
    r_abort_lazy_call(
      error_call,
      "Long vectors are not yet supported. "
      "Expansion results in an allocation larger than 2^31-1 elements. "
      "Attempted allocation size was %.0lf.",
      (double) size
    );
  }

  r_obj* const* v_xs = r_list_cbegin(xs);

  r_obj* ffi_times_each = KEEP(r_alloc_integer(1));
  int* p_ffi_times_each = r_int_begin(ffi_times_each);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* x = v_xs[i];

    r_ssize times_each = 0;
    r_ssize times = 0;

    if (size != 0) {
      switch (vary) {
      case VCTRS_EXPAND_VARY_slowest: {
        times_each = size / v_cumulative[i];
        times = v_cumulative[i] / v_sizes[i];
        break;
      };
      case VCTRS_EXPAND_VARY_fastest: {
        times_each = v_cumulative[i] / v_sizes[i];
        times = size / v_cumulative[i];
        break;
      }
      }
    }

    *p_ffi_times_each = r_ssize_as_integer(times_each);

    x = KEEP(vec_rep_each(x, ffi_times_each, error_call, vec_args.x, vec_args.empty));
    x = vec_rep(x, r_ssize_as_integer(times), error_call, vec_args.x, vec_args.empty);

    r_list_poke(out, i, x);

    FREE(1);
  }

  init_data_frame(out, size);

  FREE(6);
  return out;
}

static inline
enum vctrs_expand_vary parse_vary(r_obj* vary) {
  if (!r_is_string(vary)) {
    r_stop_internal("`vary` must be a string.");
  }

  const char* c_vary = r_chr_get_c_string(vary, 0);

  if (!strcmp(c_vary, "slowest")) return VCTRS_EXPAND_VARY_slowest;
  if (!strcmp(c_vary, "fastest")) return VCTRS_EXPAND_VARY_fastest;

  r_stop_internal(
    "`vary` must be either \"slowest\" or \"fastest\"."
  );
}
