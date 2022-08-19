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

#ifndef VCTRS_ORDER_GROUPS_H
#define VCTRS_ORDER_GROUPS_H

#include "vctrs-core.h"

// -----------------------------------------------------------------------------

// This seems to be a reasonable default to start with for tracking group sizes
// and is what base R uses. It is expanded by 2x every time we need to
// reallocate. It is also capped to the size of `x`.
#define GROUP_DATA_SIZE_DEFAULT 100000

// -----------------------------------------------------------------------------

/*
 * Info related to 1 column / vector worth of groupings
 *
 * @member self A RAWSXP for the struct memory.
 * @member data An integer vector of group sizes.
 * @member p_data A pointer to `data`.
 * @member data_pi The protection index for `data` which allows us to
 *   `REPROTECT()` on the fly.
 * @member data_size The current allocated size of `data`.
 * @member n_groups The current number of groups seen so far.
 *   Always `<= data_size`.
 * @member max_group_size The maximum group size seen so far.
 */
struct group_info {
  SEXP self;
  SEXP data;
  int* p_data;
  PROTECT_INDEX data_pi;

  r_ssize data_size;

  r_ssize n_groups;
  r_ssize max_group_size;
};

#define PROTECT_GROUP_INFO(p_info, p_n) do {              \
  PROTECT((p_info)->self);                                \
  PROTECT_WITH_INDEX((p_info)->data, &(p_info)->data_pi); \
  *(p_n) += 2;                                            \
} while(0)

// -----------------------------------------------------------------------------

/*
 * `group_infos` contains information about 2 `group_info` structs. It contains
 * a pointer which points to 2 `group_info` pointers.
 *
 * For a single atomic vector, `current = 0` is always set and only one of the
 * structs is ever used.
 *
 * For a data frame with multiple columns, after every column `current` is
 * flipped between 0 and 1, giving us a chance to read the group information
 * off the previous column (which allows us to chunk the current column into
 * groups) while also updating the group information of the chunks of
 * the current one.
 *
 * @member self A RAWSXP for the struct memory.
 * @member p_p_group_info_data A RAWSXP for the p_p_group_info array memory.
 * @member p_p_group_info A pointer to two `group_info` pointers.
 * @member max_data_size The maximum data size that can be allocated when
 *   reallocating an individual `p_group_info`. This is set to the size of
 *   `x`.
 * @member current The current `group_info` pointer we are using. This is
 *   either 0 or 1.
 * @member force_groups Was group information requested by the user? If so, we
 *   always have to track group information.
 * @member ignore_groups Should group tracking be ignored? This is the default
 *   for atomic vectors unless groups information is explicitly requested. For
 *   data frames, this is true over all columns except the last one (for
 *   performance) unless `force_groups` is true.
 */
struct group_infos {
  SEXP self;
  SEXP p_p_group_info_data;
  struct group_info** p_p_group_info;
  r_ssize max_data_size;
  int current;
  bool force_groups;
  bool ignore_groups;
};

#define PROTECT_GROUP_INFOS(p_info, p_n) do {               \
  PROTECT((p_info)->self);                                  \
  PROTECT((p_info)->p_p_group_info_data);                   \
  *(p_n) += 2;                                              \
  PROTECT_GROUP_INFO((p_info)->p_p_group_info[0], (p_n));   \
  PROTECT_GROUP_INFO((p_info)->p_p_group_info[1], (p_n));   \
} while(0)

// -----------------------------------------------------------------------------

struct group_info* new_group_info();

struct group_infos* new_group_infos(struct group_info* p_group_info0,
                                    struct group_info* p_group_info1,
                                    r_ssize max_data_size,
                                    bool force_groups,
                                    bool ignore_groups);

void groups_swap(struct group_infos* p_group_infos);

// -----------------------------------------------------------------------------

/*
 * Extract the current `group_info*`
 */
static inline
struct group_info* groups_current(struct group_infos* p_group_infos) {
  return p_group_infos->p_p_group_info[p_group_infos->current];
}

// -----------------------------------------------------------------------------

void groups_size_push(r_ssize size, struct group_infos* p_group_infos);

/*
 * Inline version of `groups_size_push()` that only attempts to push if
 * we aren't ignoring groups. Important for this to be inline for performance,
 * especially with atomic vectors where order generally isn't required.
 */
static inline
void groups_size_maybe_push(r_ssize size, struct group_infos* p_group_infos) {
  if (p_group_infos->ignore_groups) {
    return;
  } else {
    groups_size_push(size, p_group_infos);
  }
}

// -----------------------------------------------------------------------------
#endif
