#ifndef VCTRS_ORDER_GROUPS_H
#define VCTRS_ORDER_GROUPS_H

#include "vctrs.h"

// Group functionality is kept in `order.c` to remain `static` and fast, but
// some information like defines, macros, and structs live here for convenience

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

  R_xlen_t data_size;

  R_xlen_t n_groups;
  R_xlen_t max_group_size;
};

#define PROTECT_GROUP_INFO(p_info, p_n) do {              \
  PROTECT_WITH_INDEX((p_info)->data, &(p_info)->data_pi); \
  (p_info)->p_data = INTEGER((p_info)->data);             \
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
 * @member current The current `group_info` pointer we are using. This is
 *   either 0 or 1.
 * @member ignore Should group tracking be ignored entirely? This is the default
 *   for atomic vectors unless groups information is explicitly requested.
 */
struct group_infos {
  struct group_info** p_p_group_info;
  int current;
  bool ignore;
};

// -----------------------------------------------------------------------------
#endif
