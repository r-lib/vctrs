#include "order-groups.h"

// -----------------------------------------------------------------------------

// Pair with `PROTECT_GROUP_INFO()` in the caller
struct group_info new_group_info() {
  struct group_info info;

  info.data_size = 0;

  info.data = PROTECT(Rf_allocVector(INTSXP, 0));

  info.n_groups = 0;
  info.max_group_size = 0;

  UNPROTECT(1);
  return info;
}

// -----------------------------------------------------------------------------

static SEXP group_extend(const int* p_data, R_xlen_t data_size, R_xlen_t size);

/*
 * Reallocate `data` to be as long as `size`.
 */
void group_realloc(struct group_info* p_group_info, R_xlen_t size) {
  // First allocation
  if (size == 0) {
    size = GROUP_DATA_SIZE_DEFAULT;
  }

  // Reallocate
  p_group_info->data = group_extend(
    p_group_info->p_data,
    p_group_info->data_size,
    size
  );

  // Reprotect
  REPROTECT(p_group_info->data, p_group_info->data_pi);

  // Update pointer
  p_group_info->p_data = INTEGER(p_group_info->data);

  // Update size
  p_group_info->data_size = size;
}

// A good bit faster than `Rf_xlengthgets()` because that fills the new extended
// locations with `NA` as well, which we don't need.
static SEXP group_extend(const int* p_data, R_xlen_t data_size, R_xlen_t size) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  memcpy(p_out, p_data, data_size * sizeof(int));

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------

struct group_infos new_group_infos(struct group_info** p_p_group_info,
                                   bool requested,
                                   bool ignore) {
  struct group_infos infos;

  infos.p_p_group_info = p_p_group_info;
  infos.current = 0;
  infos.requested = requested;
  infos.ignore = ignore;

  return infos;
}

// -----------------------------------------------------------------------------

/*
 * `groups_swap()` is called after each data frame column is processed.
 * It handles switching the `current` group info that we are working on,
 * and ensures that the information that might have been there before has
 * been zeroed out. It also ensures that the new current group info has at
 * least as much space as the previous one, which is especially important for
 * the first column swap where the 2nd group info array starts as a size 0
 * integer vector (because we don't know if it will get used or not).
 */
void groups_swap(struct group_infos* p_group_infos) {
  if (p_group_infos->ignore) {
    return;
  }

  struct group_info* p_group_info_pre = groups_current(p_group_infos);

  // Make the swap
  p_group_infos->current = 1 - p_group_infos->current;

  struct group_info* p_group_info_post = groups_current(p_group_infos);

  // Clear the info from last time the swap was made
  p_group_info_post->max_group_size = 0;
  p_group_info_post->n_groups = 0;

  // Ensure the new group info is at least as big as the old group info
  if (p_group_info_post->data_size < p_group_info_pre->data_size) {
    group_realloc(p_group_info_post, p_group_info_pre->data_size * 2);
  }
}

// -----------------------------------------------------------------------------

/*
 * Push a group size onto the current `group_info*`
 * - Does nothing if we are ignoring group info
 * - Reallocates as needed
 * - Updates number of groups / max group size as well
 */
void groups_size_push(struct group_infos* p_group_infos, R_xlen_t size) {
  if (p_group_infos->ignore) {
    return;
  }

  if (size == 0) {
    Rf_errorcall(R_NilValue, "Internal error: Group `size` to push should never be zero.");
  }

  struct group_info* p_group_info = groups_current(p_group_infos);

  // Extend `data` as required - reprotects itself
  if (p_group_info->data_size == p_group_info->n_groups) {
    group_realloc(p_group_info, p_group_info->data_size * 2);
  }

  // Push group size
  p_group_info->p_data[p_group_info->n_groups] = size;

  // Bump number of groups
  ++p_group_info->n_groups;

  // Update max group size
  if (p_group_info->max_group_size < size) {
    p_group_info->max_group_size = size;
  }
}
