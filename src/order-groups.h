#ifndef VCTRS_ORDER_GROUPS_H
#define VCTRS_ORDER_GROUPS_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

// This seems to be a reasonable default to start with for tracking group sizes
// and is what base R uses. It is expanded by 2x every time we need to
// reallocate.
#define GROUP_DATA_SIZE_DEFAULT 100000

// -----------------------------------------------------------------------------

/*
 * Info related to 1 column / vector worth of groupings
 *
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
  SEXP data;
  int* p_data;
  PROTECT_INDEX data_pi;

  r_ssize data_size;

  r_ssize n_groups;
  r_ssize max_group_size;
};

#define PROTECT_GROUP_INFO(p_info, p_n) do {              \
  PROTECT_WITH_INDEX((p_info)->data, &(p_info)->data_pi); \
  *(p_n) += 1;                                            \
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
 * @member p_p_group_info A pointer to two `group_info` pointers.
 * @member max_data_size The maximum data size that can be allocated when
 *   reallocating an individual `p_group_info`. This is set to the size of
 *   `x`.
 * @member current The current `group_info` pointer we are using. This is
 *   either 0 or 1.
 * @member requested Was group information requested by the user? If so, we
 *   always have to track group information.
 * @member ignore Should group tracking be ignored? This is the default
 *   for atomic vectors unless groups information is explicitly requested. For
 *   data frames, this is true over all columns except the last one (for
 *   performance) unless `requested` is true.
 */
struct group_infos {
  struct group_info** p_p_group_info;
  r_ssize max_data_size;
  int current;
  bool requested;
  bool ignore;
};

// -----------------------------------------------------------------------------

struct group_info new_group_info();

struct group_infos new_group_infos(struct group_info** p_p_group_info,
                                   r_ssize max_data_size,
                                   bool requested,
                                   bool ignore);

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
  if (p_group_infos->ignore) {
    return;
  } else {
    groups_size_push(size, p_group_infos);
  }
}

// -----------------------------------------------------------------------------
#endif
