#include "vctrs.h"
#include "decl/empty-decl.h"

// [[ register() ]]
r_obj* vctrs_list_drop_empty(r_obj* x) {
  return list_drop_empty(x);
}

static
r_obj* list_drop_empty(r_obj* x) {
  if (!vec_is_list(x)) {
    r_abort("`x` must be a list.");
  }

  r_ssize i = 0;
  const r_ssize size = vec_size(x);

  r_obj* const* v_x = r_list_cbegin(x);

  // Locate first element to drop
  for (; i < size; ++i) {
    if (vec_size(v_x[i]) == 0) {
      break;
    }
  }

  if (i == size) {
    // Nothing to drop
    return x;
  }

  r_obj* keep = KEEP(r_alloc_logical(size));
  int* v_keep = r_lgl_begin(keep);

  for (r_ssize j = 0; j < i; ++j) {
    // Keep everything before first element to drop
    v_keep[j] = true;
  }

  // `i` should be dropped so handle that here
  v_keep[i] = false;
  ++i;

  for (; i < size; ++i) {
    v_keep[i] = vec_size(v_x[i]) != 0;
  }

  r_obj* out = vec_slice(x, keep);

  FREE(1);
  return out;
}
