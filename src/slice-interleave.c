#include "vctrs.h"
#include "decl/slice-interleave-decl.h"

// [[ register() ]]
r_obj* ffi_interleave_indices(r_obj* n, r_obj* size) {
  r_ssize c_n = r_arg_as_ssize(n, "n");
  r_ssize c_size = r_arg_as_ssize(size, "size");
  return vec_interleave_indices(c_n, c_size);
}

static
r_obj* vec_interleave_indices(r_ssize n, r_ssize size) {
  if (n < 0) {
    r_stop_internal(
      "`n` must be greater than or equal to 0."
    );
  }

  if (size < 0) {
    r_stop_internal(
      "`size` must be greater than or equal to 0."
    );
  }

  const r_ssize total_size = r_ssize_mult(n, size);

  if (total_size > R_LEN_T_MAX) {
    r_abort(
      "Long vectors are not yet supported in `vec_interleave()`. "
      "Result from interleaving would have size %td, which is larger "
      "than the maximum supported size of 2^31 - 1.",
      total_size
    );
  }

  r_obj* out = KEEP(r_alloc_list(n));

  for (r_ssize i = 0; i < n; ++i) {
    const r_ssize start = i + 1;

    r_obj* elt = r_alloc_integer(size);
    r_list_poke(out, i, elt);
    int* v_elt = r_int_begin(elt);

    for (r_ssize j = 0; j < size; ++j) {
      v_elt[j] = start + n * j;
    }
  }

  FREE(1);
  return out;
}
