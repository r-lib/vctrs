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

#include "order-truelength.h"
#include "utils.h"

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
struct truelength_info* new_truelength_info(r_ssize max_size_alloc) {
  SEXP self = PROTECT(r_new_raw(sizeof(struct truelength_info)));
  struct truelength_info* p_truelength_info = (struct truelength_info*) RAW(self);

  p_truelength_info->self = self;

  p_truelength_info->strings = vctrs_shared_empty_chr;
  p_truelength_info->lengths = vctrs_shared_empty_raw;
  p_truelength_info->uniques = vctrs_shared_empty_chr;
  p_truelength_info->sizes = vctrs_shared_empty_int;
  p_truelength_info->sizes_aux = vctrs_shared_empty_int;

  p_truelength_info->size_alloc = 0;
  p_truelength_info->max_size_alloc = max_size_alloc;
  p_truelength_info->size_used = 0;

  p_truelength_info->max_string_size = 0;

  p_truelength_info->reencode = false;

  UNPROTECT(1);
  return p_truelength_info;
}

// -----------------------------------------------------------------------------

/*
 * Reset the truelengths of all unique strings captured in `strings` using
 * the original truelengths in `lengths`.
 *
 * Will be called after each character data frame column is processed, and
 * at the end of `chr_order()` for a single character vector.
 */
void truelength_reset(struct truelength_info* p_truelength_info) {
  r_ssize size = p_truelength_info->size_used;

  for (r_ssize i = 0; i < size; ++i) {
    SEXP string = p_truelength_info->p_strings[i];
    r_ssize length = p_truelength_info->p_lengths[i];

    SET_TRUELENGTH(string, length);
  }

  // Also reset vector specific details
  p_truelength_info->size_used = 0;
  p_truelength_info->max_string_size = 0;
  p_truelength_info->reencode = false;
}

// -----------------------------------------------------------------------------

static void truelength_realloc(struct truelength_info* p_truelength_info);

/*
 * Saves a unique CHARSXP `x` along with its original truelength and
 * its "size" (i.e the number of characters). Will be reset later with
 * `truelength_reset()`.
 */
void truelength_save(SEXP x,
                     r_ssize truelength,
                     r_ssize size,
                     struct truelength_info* p_truelength_info) {
  // Reallocate as needed
  if (p_truelength_info->size_used == p_truelength_info->size_alloc) {
    truelength_realloc(p_truelength_info);
  }

  p_truelength_info->p_strings[p_truelength_info->size_used] = x;
  p_truelength_info->p_lengths[p_truelength_info->size_used] = truelength;
  p_truelength_info->p_uniques[p_truelength_info->size_used] = x;
  p_truelength_info->p_sizes[p_truelength_info->size_used] = size;

  ++p_truelength_info->size_used;
}

// -----------------------------------------------------------------------------

static r_ssize truelength_realloc_size(struct truelength_info* p_truelength_info);

static inline SEXP lengths_resize(SEXP x, r_ssize x_size, r_ssize size);

/*
 * Extend the vectors in `truelength_info`.
 * Reprotects itself.
 */
static
void truelength_realloc(struct truelength_info* p_truelength_info) {
  r_ssize size = truelength_realloc_size(p_truelength_info);

  p_truelength_info->strings = chr_resize(
    p_truelength_info->strings,
    p_truelength_info->size_used,
    size
  );
  REPROTECT(p_truelength_info->strings, p_truelength_info->strings_pi);
  p_truelength_info->p_strings = STRING_PTR(p_truelength_info->strings);

  p_truelength_info->lengths = lengths_resize(
    p_truelength_info->lengths,
    p_truelength_info->size_used,
    size
  );
  REPROTECT(p_truelength_info->lengths, p_truelength_info->lengths_pi);
  p_truelength_info->p_lengths = (r_ssize*) RAW(p_truelength_info->lengths);

  p_truelength_info->uniques = chr_resize(
    p_truelength_info->uniques,
    p_truelength_info->size_used,
    size
  );
  REPROTECT(p_truelength_info->uniques, p_truelength_info->uniques_pi);
  p_truelength_info->p_uniques = STRING_PTR(p_truelength_info->uniques);

  p_truelength_info->sizes = int_resize(
    p_truelength_info->sizes,
    p_truelength_info->size_used,
    size
  );
  REPROTECT(p_truelength_info->sizes, p_truelength_info->sizes_pi);
  p_truelength_info->p_sizes = INTEGER(p_truelength_info->sizes);

  p_truelength_info->sizes_aux = int_resize(
    p_truelength_info->sizes_aux,
    p_truelength_info->size_used,
    size
  );
  REPROTECT(p_truelength_info->sizes_aux, p_truelength_info->sizes_aux_pi);
  p_truelength_info->p_sizes_aux = INTEGER(p_truelength_info->sizes_aux);

  p_truelength_info->size_alloc = size;
}

static inline
SEXP lengths_resize(SEXP x, r_ssize x_size, r_ssize size) {
  return raw_resize(
    x,
    x_size * sizeof(r_ssize),
    size * sizeof(r_ssize)
  );
}

// -----------------------------------------------------------------------------

static
r_ssize truelength_realloc_size(struct truelength_info* p_truelength_info) {
  r_ssize size_alloc = p_truelength_info->size_alloc;
  r_ssize max_size_alloc = p_truelength_info->max_size_alloc;

  // First allocation
  if (size_alloc == 0) {
    if (TRUELENGTH_SIZE_ALLOC_DEFAULT < max_size_alloc) {
      return TRUELENGTH_SIZE_ALLOC_DEFAULT;
    } else {
      return max_size_alloc;
    }
  }

  // Avoid potential overflow when doubling size
  uint64_t new_size_alloc = ((uint64_t) size_alloc) * 2;

  // Clamp maximum allocation size to the size of the input
  if (new_size_alloc > max_size_alloc) {
    return max_size_alloc;
  }

  // Can now safely cast back to `r_ssize`
  return (r_ssize) new_size_alloc;
}
