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

/*
 * See the notes in the character ordering section at the top of `order.c`
 * for more details on how TRUELENGTH is used to detect unique strings.
 *
 * The helpers here are somewhat equivalent to the following from R's `order()`
 * https://github.com/wch/r-source/blob/91b4507bf6040c0167fc5b6037c202c8cbd98afd/src/main/radixsort.c#L66-L123
 */

// -----------------------------------------------------------------------------

/*
 * Construct a new `truelength_info`
 *
 * Pair with `PROTECT_TRUELENGTH_INFO()` in the caller
 */
struct truelength_info* new_truelength_info(r_ssize n_max) {
  SEXP self = PROTECT(r_new_raw(sizeof(struct truelength_info)));
  struct truelength_info* p_truelength_info = (struct truelength_info*) RAW(self);

  p_truelength_info->self = self;

  p_truelength_info->strings = r_globals.empty_chr;
  p_truelength_info->truelengths = r_globals.empty_raw;
  p_truelength_info->n_strings_alloc = 0;
  p_truelength_info->n_strings_used = 0;

  p_truelength_info->uniques = r_globals.empty_chr;
  p_truelength_info->n_uniques_alloc = 0;
  p_truelength_info->n_uniques_used = 0;

  p_truelength_info->sizes = r_globals.empty_int;
  p_truelength_info->sizes_aux = r_globals.empty_int;
  p_truelength_info->n_sizes_alloc = 0;
  p_truelength_info->n_sizes_used = 0;
  p_truelength_info->max_string_size = 0;

  p_truelength_info->n_max = n_max;

  UNPROTECT(1);
  return p_truelength_info;
}

// -----------------------------------------------------------------------------

/*
 * First, reset the TRUELENGTHs of all unique CHARSXPs in `uniques` to 0, which
 * is the default used by R.
 *
 * Then, reset the TRUELENGTHs of all CHARSXPs in `strings` to their original
 * value contained in `truelengths`. There should be very few of these,
 * if any, as R doesn't typically use the TRUELENGTH slot of CHARSXPs. One
 * exception seems to be very simple strings, such as `"a"`, which R probably
 * adds to the cache at startup, and sets their TRUELENGTH value for some
 * reason.
 *
 * It is important to reset `uniques` first, then `strings`, as the CHARSXPs
 * in `strings` are, by definition, also in `uniques`.
 *
 * This will be called after each character data frame column is processed, and
 * at the end of `chr_order()` for a single character vector.
 */
void truelength_reset(struct truelength_info* p_truelength_info) {
  r_ssize n_uniques_used = p_truelength_info->n_uniques_used;
  r_ssize n_strings_used = p_truelength_info->n_strings_used;

  // First reset uniques
  for (r_ssize i = 0; i < n_uniques_used; ++i) {
    SEXP unique = p_truelength_info->p_uniques[i];
    SET_TRUELENGTH(unique, 0);
  }

  // Then reset strings
  for (r_ssize i = 0; i < n_strings_used; ++i) {
    SEXP string = p_truelength_info->p_strings[i];
    r_ssize truelength = p_truelength_info->p_truelengths[i];
    SET_TRUELENGTH(string, truelength);
  }

  // Also reset vector specific details
  p_truelength_info->n_uniques_used = 0;
  p_truelength_info->n_strings_used = 0;
  p_truelength_info->n_sizes_used = 0;
  p_truelength_info->max_string_size = 0;
}

// -----------------------------------------------------------------------------

static r_ssize truelength_realloc_size(r_ssize n_x, r_ssize n_max);

static inline SEXP truelengths_resize(SEXP x, r_ssize x_size, r_ssize size);

void truelength_realloc_strings(struct truelength_info* p_truelength_info) {
  r_ssize size = truelength_realloc_size(
    p_truelength_info->n_strings_alloc,
    p_truelength_info->n_max
  );

  p_truelength_info->strings = chr_resize(
    p_truelength_info->strings,
    p_truelength_info->n_strings_alloc,
    size
  );
  REPROTECT(p_truelength_info->strings, p_truelength_info->strings_pi);
  p_truelength_info->p_strings = STRING_PTR(p_truelength_info->strings);

  p_truelength_info->truelengths = truelengths_resize(
    p_truelength_info->truelengths,
    p_truelength_info->n_strings_alloc,
    size
  );
  REPROTECT(p_truelength_info->truelengths, p_truelength_info->truelengths_pi);
  p_truelength_info->p_truelengths = (r_ssize*) RAW(p_truelength_info->truelengths);

  p_truelength_info->n_strings_alloc = size;
}

static inline
SEXP truelengths_resize(SEXP x, r_ssize x_size, r_ssize size) {
  return raw_resize(
    x,
    x_size * sizeof(r_ssize),
    size * sizeof(r_ssize)
  );
}

// -----------------------------------------------------------------------------

void truelength_realloc_uniques(struct truelength_info* p_truelength_info) {
  r_ssize size = truelength_realloc_size(
    p_truelength_info->n_uniques_alloc,
    p_truelength_info->n_max
  );

  p_truelength_info->uniques = chr_resize(
    p_truelength_info->uniques,
    p_truelength_info->n_uniques_alloc,
    size
  );
  REPROTECT(p_truelength_info->uniques, p_truelength_info->uniques_pi);
  p_truelength_info->p_uniques = STRING_PTR(p_truelength_info->uniques);

  p_truelength_info->n_uniques_alloc = size;
}

// -----------------------------------------------------------------------------

void truelength_realloc_sizes(struct truelength_info* p_truelength_info) {
  r_ssize size = truelength_realloc_size(
    p_truelength_info->n_sizes_alloc,
    p_truelength_info->n_max
  );

  p_truelength_info->sizes = int_resize(
    p_truelength_info->sizes,
    p_truelength_info->n_sizes_alloc,
    size
  );
  REPROTECT(p_truelength_info->sizes, p_truelength_info->sizes_pi);
  p_truelength_info->p_sizes = INTEGER(p_truelength_info->sizes);

  p_truelength_info->sizes_aux = int_resize(
    p_truelength_info->sizes_aux,
    p_truelength_info->n_sizes_alloc,
    size
  );
  REPROTECT(p_truelength_info->sizes_aux, p_truelength_info->sizes_aux_pi);
  p_truelength_info->p_sizes_aux = INTEGER(p_truelength_info->sizes_aux);

  p_truelength_info->n_sizes_alloc = size;
}

// -----------------------------------------------------------------------------

static
r_ssize truelength_realloc_size(r_ssize n_x, r_ssize n_max) {
  // First allocation
  if (n_x == 0) {
    if (TRUELENGTH_SIZE_ALLOC_DEFAULT < n_max) {
      return TRUELENGTH_SIZE_ALLOC_DEFAULT;
    } else {
      return n_max;
    }
  }

  // Avoid potential overflow when doubling size
  uint64_t n_new = ((uint64_t) n_x) * 2;

  // Clamp maximum allocation size to the size of the input
  if (n_new > n_max) {
    return n_max;
  }

  // Can now safely cast back to `r_ssize`
  return (r_ssize) n_new;
}
