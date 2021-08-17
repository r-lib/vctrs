#include <rlang.h>
#include "vctrs.h"

enum vctrs_compact_drop {
  VCTRS_COMPACT_DROP_missing,
  VCTRS_COMPACT_DROP_empty
};

#include "decl/compact-decl.h"

// [[ register() ]]
r_obj* vctrs_list_compact(r_obj* x, r_obj* drop) {
  enum vctrs_compact_drop c_drop = parse_drop(drop);
  return list_compact(x, c_drop);
}

static
r_obj* list_compact(r_obj* x, enum vctrs_compact_drop drop) {
  if (!vec_is_list(x)) {
    r_abort("`x` must be a list.");
  }

  r_ssize i = 0;
  const r_ssize size = vec_size(x);

  r_obj* const* v_x = r_list_cbegin(x);

  // Locate first compactable element
  switch (drop) {
  case VCTRS_COMPACT_DROP_missing: {
    for (; i < size; ++i) {
      if (v_x[i] == r_null) {
        break;
      }
    }
  }
  case VCTRS_COMPACT_DROP_empty: {
    for (; i < size; ++i) {
      if (vec_size(v_x[i]) == 0) {
        break;
      }
    }
  }
  }

  if (i == size) {
    // Nothing to compact
    return x;
  }

  r_obj* keep = KEEP(r_alloc_logical(size));
  int* v_keep = r_lgl_begin(keep);

  for (r_ssize j = 0; j < i; ++j) {
    // Keep everything before first compactable element
    v_keep[j] = true;
  }

  // `i` is compactable so handle that here
  v_keep[i] = false;
  ++i;

  switch (drop) {
  case VCTRS_COMPACT_DROP_missing: {
    for (; i < size; ++i) {
      v_keep[i] = v_x[i] != r_null;
    }
  }
  case VCTRS_COMPACT_DROP_empty: {
    for (; i < size; ++i) {
      v_keep[i] = vec_size(v_x[i]) != 0;
    }
  }
  }

  r_obj* out = vec_slice(x, keep);

  FREE(1);
  return out;
}

static inline
enum vctrs_compact_drop parse_drop(r_obj* drop) {
  if (!r_is_string(drop)) {
    r_stop_internal("parse_drop", "`drop` must be a string.");
  }

  const char* c_drop = r_chr_get_c_string(drop, 0);

  if (!strcmp(c_drop, "missing")) return VCTRS_COMPACT_DROP_missing;
  if (!strcmp(c_drop, "empty")) return VCTRS_COMPACT_DROP_empty;

  r_stop_internal(
    "parse_drop",
    "`drop` must be one of: \"missing\" or \"empty\"."
  );
}
