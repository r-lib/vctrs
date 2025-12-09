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

#include "vctrs.h"

// -----------------------------------------------------------------------------

static inline int dbl_cmp(double x,
                          double y,
                          enum vctrs_dbl x_type,
                          enum vctrs_dbl y_type,
                          int direction,
                          int na_order,
                          int na_nan_order);

/*
 * Check if a double vector is ordered, handling `decreasing`, `na_last`, and
 * `nan_distinct`.
 *
 * If the double vector is in the expected ordering, no sorting needs to
 * occur. In these cases, if `p_x` is in exactly the expected ordering.
 * If `p_x` is in exactly the opposite ordering, the the ordering will later
 * be reversed (this only happens if it is strictly opposite of expected
 * ordering, ties would prevent the reversal from being stable). Group
 * information is also pushed in these cases for use in the next columns.
 */
enum vctrs_sortedness dbl_sortedness(const double* p_x,
                                     r_ssize size,
                                     bool decreasing,
                                     bool na_last,
                                     bool nan_distinct,
                                     struct group_infos* p_group_infos) {
  if (size == 0) {
    return VCTRS_SORTEDNESS_sorted;
  }

  if (size == 1) {
    groups_size_maybe_push(1, p_group_infos);
    return VCTRS_SORTEDNESS_sorted;
  }

  const int direction = decreasing ? -1 : 1;
  const int na_order = na_last ? 1 : -1;
  const int na_nan_order = nan_distinct ? na_order : 0;

  double previous = p_x[0];
  enum vctrs_dbl previous_type = dbl_classify(previous);

  r_ssize count = 0;

  // Check for strictly opposite of expected order
  // (ties are not allowed so we can reverse the vector stably)
  for (r_ssize i = 1; i < size; ++i, ++count) {
    double current = p_x[i];
    enum vctrs_dbl current_type = dbl_classify(current);

    int cmp = dbl_cmp(
      current,
      previous,
      current_type,
      previous_type,
      direction,
      na_order,
      na_nan_order
    );

    if (cmp >= 0) {
      break;
    }

    previous = current;
    previous_type = current_type;
  }

  // Was in strictly opposite of expected order.
  if (count == size - 1) {
    // Each group is size 1 since this is strict ordering
    for (r_ssize j = 0; j < size; ++j) {
      groups_size_maybe_push(1, p_group_infos);
    }

    return VCTRS_SORTEDNESS_reversed;
  }

  // Was partially in expected order. Need to sort.
  if (count != 0) {
    return VCTRS_SORTEDNESS_unsorted;
  }

  // Retain the original `n_groups` to be able to reset the group sizes if
  // it turns out we don't have expected ordering
  struct group_info* p_group_info = groups_current(p_group_infos);
  r_ssize original_n_groups = p_group_info->n_groups;

  r_ssize group_size = 1;

  // Check for expected ordering - allowing ties since we don't have to
  // reverse the ordering.
  for (r_ssize i = 1; i < size; ++i) {
    double current = p_x[i];
    enum vctrs_dbl current_type = dbl_classify(current);

    int cmp = dbl_cmp(
      current,
      previous,
      current_type,
      previous_type,
      direction,
      na_order,
      na_nan_order
    );

    // Not expected ordering
    if (cmp < 0) {
      p_group_info->n_groups = original_n_groups;
      return VCTRS_SORTEDNESS_unsorted;
    }

    previous = current;
    previous_type = current_type;

    // Continue group run
    if (cmp == 0) {
      ++group_size;
      continue;
    }

    // Expected ordering
    groups_size_maybe_push(group_size, p_group_infos);
    group_size = 1;
  }

  // Push final group run
  groups_size_maybe_push(group_size, p_group_infos);

  // Expected ordering
  return VCTRS_SORTEDNESS_sorted;
}

static inline int dbl_cmp_numbers(double x, double y, int direction);

/*
 * Compare two doubles, handling `na_order`, `direction`, and `na_nan_order`
 */
static inline
int dbl_cmp(double x,
            double y,
            enum vctrs_dbl x_type,
            enum vctrs_dbl y_type,
            int direction,
            int na_order,
            int na_nan_order) {
  switch (x_type) {
  case VCTRS_DBL_number:
    switch (y_type) {
    case VCTRS_DBL_number: return dbl_cmp_numbers(x, y, direction);
    case VCTRS_DBL_missing: return -na_order;
    case VCTRS_DBL_nan: return -na_order;
    }
  case VCTRS_DBL_missing:
    switch (y_type) {
    case VCTRS_DBL_number: return na_order;
    case VCTRS_DBL_missing: return 0;
    case VCTRS_DBL_nan: return na_nan_order;
    }
  case VCTRS_DBL_nan:
    switch (y_type) {
    case VCTRS_DBL_number: return na_order;
    case VCTRS_DBL_missing: return -na_nan_order;
    case VCTRS_DBL_nan: return 0;
    }
  }
  never_reached("dbl_cmp");
}

static inline
int dbl_cmp_numbers(double x, double y, int direction) {
  const int cmp = (x > y) - (x < y);
  return cmp * direction;
}

// -----------------------------------------------------------------------------

static inline int int_cmp(int x, int y, const int direction, const int na_order);

// Very similar to `dbl_sortedness()`
enum vctrs_sortedness int_sortedness(const int* p_x,
                                     r_ssize size,
                                     bool decreasing,
                                     bool na_last,
                                     struct group_infos* p_group_infos) {
  if (size == 0) {
    return VCTRS_SORTEDNESS_sorted;
  }

  if (size == 1) {
    groups_size_maybe_push(1, p_group_infos);
    return VCTRS_SORTEDNESS_sorted;
  }

  const int direction = decreasing ? -1 : 1;
  const int na_order = na_last ? 1 : -1;

  int previous = p_x[0];

  r_ssize count = 0;

  // Check for strictly opposite of expected order
  // (ties are not allowed so we can reverse the vector stably)
  for (r_ssize i = 1; i < size; ++i, ++count) {
    int current = p_x[i];

    int cmp = int_cmp(
      current,
      previous,
      direction,
      na_order
    );

    if (cmp >= 0) {
      break;
    }

    previous = current;
  }

  // Was in strictly opposite of expected order.
  if (count == size - 1) {
    // Each group is size 1 since this is strict ordering
    for (r_ssize j = 0; j < size; ++j) {
      groups_size_maybe_push(1, p_group_infos);
    }

    return VCTRS_SORTEDNESS_reversed;
  }

  // Was partially in expected order. Need to sort.
  if (count != 0) {
    return VCTRS_SORTEDNESS_unsorted;
  }

  // Retain the original `n_groups` to be able to reset the group sizes if
  // it turns out we don't have expected ordering
  struct group_info* p_group_info = groups_current(p_group_infos);
  r_ssize original_n_groups = p_group_info->n_groups;

  r_ssize group_size = 1;

  // Check for expected ordering - allowing ties since we don't have to
  // reverse the ordering.
  for (r_ssize i = 1; i < size; ++i) {
    int current = p_x[i];

    int cmp = int_cmp(
      current,
      previous,
      direction,
      na_order
    );

    // Not expected ordering
    if (cmp < 0) {
      p_group_info->n_groups = original_n_groups;
      return VCTRS_SORTEDNESS_unsorted;
    }

    previous = current;

    // Continue group run
    if (cmp == 0) {
      ++group_size;
      continue;
    }

    // Expected ordering
    groups_size_maybe_push(group_size, p_group_infos);
    group_size = 1;
  }

  // Push final group run
  groups_size_maybe_push(group_size, p_group_infos);

  // Expected ordering
  return VCTRS_SORTEDNESS_sorted;
}

// Very similar to `dbl_cmp()`
static inline
int int_cmp(int x, int y, const int direction, const int na_order) {
  if (x == NA_INTEGER) {
    if (y == NA_INTEGER) {
      return 0;
    } else {
      return na_order;
    }
  }

  if (y == NA_INTEGER) {
    return -na_order;
  }

  int cmp = (x > y) - (x < y);

  return cmp * direction;
}

// -----------------------------------------------------------------------------

/*
 * Check if the data is already in the "expected" ordering as defined by
 * `decreasing` and `na_last`. If the data is in the expected ordering, or if it
 * is in the strictly opposite of the expected ordering (with no ties), then
 * groups are pushed, and a `vctrs_sortedness` value is returned indicating how
 * to finalize the order.
 *
 * Note that only one of `p_x` or `p_x_info` are provided, with the other being
 * a `NULL` pointer. When doing `chr_order()`, the original `p_x` is provided,
 * when doing `chr_order_chunk()`, the `p_x_info` chunk is provided.
 */
enum vctrs_sortedness chr_sortedness(const SEXP* p_x,
                                     const struct str_info* p_x_info,
                                     r_ssize size,
                                     bool decreasing,
                                     bool na_last,
                                     struct group_infos* p_group_infos) {
  if (size == 0) {
    return VCTRS_SORTEDNESS_sorted;
  }

  if (size == 1) {
    groups_size_maybe_push(1, p_group_infos);
    return VCTRS_SORTEDNESS_sorted;
  }

  const int direction = decreasing ? -1 : 1;
  const int na_order = na_last ? 1 : -1;

  const bool has_p_x = p_x != NULL;

  SEXP previous = has_p_x ? p_x[0] : p_x_info[0].x;
  const char* c_previous = has_p_x ? CHAR(previous) : p_x_info[0].p_x;

  r_ssize count = 0;

  // Check for strictly opposite of expected order
  // (ties are not allowed so we can reverse the vector stably)
  for (r_ssize i = 1; i < size; ++i, ++count) {
    SEXP current = has_p_x ? p_x[i] : p_x_info[i].x;
    const char* c_current = has_p_x ? CHAR(current) : p_x_info[i].p_x;

    int cmp = str_cmp(
      current,
      previous,
      c_current,
      c_previous,
      direction,
      na_order
    );

    if (cmp >= 0) {
      break;
    }

    previous = current;
    c_previous = c_current;
  }

  // Was in strictly opposite of expected order.
  if (count == size - 1) {
    // Each group is size 1 since this is strict ordering
    for (r_ssize j = 0; j < size; ++j) {
      groups_size_maybe_push(1, p_group_infos);
    }
    return VCTRS_SORTEDNESS_reversed;
  }

  // Was partially in expected order. Need to sort.
  if (count != 0) {
    return VCTRS_SORTEDNESS_unsorted;
  }

  // Retain the original `n_groups` to be able to reset the group sizes if
  // it turns out we don't have expected ordering
  struct group_info* p_group_info = groups_current(p_group_infos);
  r_ssize original_n_groups = p_group_info->n_groups;

  r_ssize group_size = 1;

  // Check for expected ordering - allowing ties since we don't have to
  // reverse the ordering.
  for (r_ssize i = 1; i < size; ++i) {
    SEXP current = has_p_x ? p_x[i] : p_x_info[i].x;
    const char* c_current = has_p_x ? CHAR(current) : p_x_info[i].p_x;

    int cmp = str_cmp(
      current,
      previous,
      c_current,
      c_previous,
      direction,
      na_order
    );

    // Not expected ordering
    if (cmp < 0) {
      p_group_info->n_groups = original_n_groups;
      return VCTRS_SORTEDNESS_unsorted;
    }

    previous = current;
    c_previous = c_current;

    // Continue group run
    if (cmp == 0) {
      ++group_size;
      continue;
    }

    // Expected ordering
    groups_size_maybe_push(group_size, p_group_infos);
    group_size = 1;
  }

  // Push final group run
  groups_size_maybe_push(group_size, p_group_infos);

  // Expected ordering
  return VCTRS_SORTEDNESS_sorted;
}

// -----------------------------------------------------------------------------

static inline void int_incr(r_ssize size, int* p_x);
static inline void ord_reverse(r_ssize size, int* p_o);

/*
 * Resolve ordering based on the sortedness and whether or not `p_o` has
 * been initialized. For a vector / first column, this function has to
 * initialize the ordering (for reversed ordering this is faster than
 * initializing the order sequentially then reversing it).
 *
 * `size` will correspond to the size of `x` for the first column, but will
 * correspond to the size of the current group for subsequent columns.
 */
void ord_resolve_sortedness(enum vctrs_sortedness sortedness,
                            r_ssize size,
                            int* p_o) {
  switch (sortedness) {
  case VCTRS_SORTEDNESS_sorted: int_incr(size, p_o); return;
  case VCTRS_SORTEDNESS_reversed: ord_reverse(size, p_o); return;
  case VCTRS_SORTEDNESS_unsorted: Rf_errorcall(R_NilValue, "Internal error: Unsorted case should be handled elsewhere.");
  }

  never_reached("ord_resolve_sortedness");
}

// Initialize with sequential 1-based ordering
static inline
void int_incr(r_ssize size, int* p_x) {
  for (r_ssize i = 0; i < size; ++i) {
    p_x[i] = i + 1;
  }
}

// Used when in strictly opposite of expected order and uninitialized.
static inline
void ord_reverse(r_ssize size, int* p_o) {
  const r_ssize half = size / 2;

  for (r_ssize i = 0; i < half; ++i) {
    r_ssize swap = size - 1 - i;
    p_o[i] = swap + 1;
    p_o[swap] = i + 1;
  }

  // Initialize center value if odd number
  if (size % 2 != 0) {
    p_o[half] = half + 1;
  }
}


static inline void ord_reverse_chunk(r_ssize size, int* p_o);

void ord_resolve_sortedness_chunk(enum vctrs_sortedness sortedness,
                                  r_ssize size,
                                  int* p_o) {
  switch (sortedness) {
  case VCTRS_SORTEDNESS_sorted: return;
  case VCTRS_SORTEDNESS_reversed: ord_reverse_chunk(size, p_o); return;
  case VCTRS_SORTEDNESS_unsorted: Rf_errorcall(R_NilValue, "Internal error: Unsorted case should be handled elsewhere.");
  }

  never_reached("ord_resolve_sortedness_chunk");
}


// Used when in strictly opposite of expected order and initialized.
// No need to alter "center" value here, it will be initialized to a value
// already and it won't be swapped.
static inline
void ord_reverse_chunk(r_ssize size, int* p_o) {
  const r_ssize half = size / 2;

  for (r_ssize i = 0; i < half; ++i) {
    r_ssize swap = size - 1 - i;

    const int temp = p_o[i];

    p_o[i] = p_o[swap];
    p_o[swap] = temp;
  }
}
