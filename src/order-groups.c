/*
 * The implementation of vec_order() is based on data.table’s forder() and their
 * earlier contribution to R’s order(). See LICENSE.note for more information.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2020, RStudio
 * Copyright (c) 2020, Data table team
 */

#include "order-groups.h"
#include "utils.h"

// -----------------------------------------------------------------------------

// Pair with `PROTECT_GROUP_INFO()` in the caller
struct group_info* new_group_info() {
  SEXP self = PROTECT(r_new_raw(sizeof(struct group_info)));
  struct group_info* p_group_info = (struct group_info*) RAW(self);

  p_group_info->self = self;
  p_group_info->data_size = 0;
  p_group_info->data = vctrs_shared_empty_int;
  p_group_info->n_groups = 0;
  p_group_info->max_group_size = 0;

  UNPROTECT(1);
  return p_group_info;
}

// -----------------------------------------------------------------------------

struct group_infos* new_group_infos(struct group_info* p_group_info0,
                                    struct group_info* p_group_info1,
                                    r_ssize max_data_size,
                                    bool force_groups,
                                    bool ignore_groups) {
  SEXP self = PROTECT(r_new_raw(sizeof(struct group_infos)));
  struct group_infos* p_group_infos = (struct group_infos*) RAW(self);

  SEXP p_p_group_info_data = PROTECT(r_new_raw(2 * sizeof(struct group_info*)));
  struct group_info** p_p_group_info = (struct group_info**) RAW(p_p_group_info_data);

  p_p_group_info[0] = p_group_info0;
  p_p_group_info[1] = p_group_info1;

  p_group_infos->self = self;
  p_group_infos->p_p_group_info_data = p_p_group_info_data;
  p_group_infos->p_p_group_info = p_p_group_info;
  p_group_infos->max_data_size = max_data_size;
  p_group_infos->current = 0;
  p_group_infos->force_groups = force_groups;
  p_group_infos->ignore_groups = ignore_groups;

  UNPROTECT(2);
  return p_group_infos;
}

// -----------------------------------------------------------------------------

static void group_realloc(r_ssize size, struct group_info* p_group_info);
static r_ssize groups_realloc_size(r_ssize data_size, r_ssize max_data_size);

/*
 * Push a group size onto the current `group_info*`
 * - Reallocates as needed
 * - Updates number of groups / max group size as well
 *
 * Should only be called through `groups_size_maybe_push()` to ensure
 * that we only push groups if we are tracking them.
 */
void groups_size_push(r_ssize size, struct group_infos* p_group_infos) {
  if (size == 0) {
    Rf_errorcall(R_NilValue, "Internal error: Group `size` to push should never be zero.");
  }

  struct group_info* p_group_info = groups_current(p_group_infos);

  // Extend `data` as required - reprotects itself
  if (p_group_info->data_size == p_group_info->n_groups) {
    r_ssize new_data_size = groups_realloc_size(
      p_group_info->data_size,
      p_group_infos->max_data_size
    );

    group_realloc(new_data_size, p_group_info);
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

// -----------------------------------------------------------------------------

/*
 * Reallocate `data` to be as long as `size`.
 */
static
void group_realloc(r_ssize size, struct group_info* p_group_info) {
  // Reallocate
  p_group_info->data = int_resize(
    p_group_info->data,
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

// -----------------------------------------------------------------------------

static
r_ssize groups_realloc_size(r_ssize data_size, r_ssize max_data_size) {
  uint64_t new_data_size;

  if (data_size == 0) {
    // First allocation
    new_data_size = GROUP_DATA_SIZE_DEFAULT;
  } else {
    // Avoid potential overflow when doubling size
    new_data_size = ((uint64_t) data_size) * 2;
  }

  // Clamp maximum allocation size to the size of the input
  if (new_data_size > max_data_size) {
    return max_data_size;
  }

  // Can now safely cast back to `r_ssize`
  return (r_ssize) new_data_size;
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
  if (p_group_infos->ignore_groups) {
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
    r_ssize new_data_size = p_group_info_pre->data_size;
    group_realloc(new_data_size, p_group_info_post);
  }
}
