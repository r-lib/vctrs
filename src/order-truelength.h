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

#ifndef VCTRS_ORDER_TRUELENGTH_H
#define VCTRS_ORDER_TRUELENGTH_H

#include "vctrs-core.h"

// -----------------------------------------------------------------------------

// This seems to be a reasonable default to start with for tracking the unique
// strings, and is what base R uses. It is expanded by 2x every time we need to
// reallocate.
#define TRUELENGTH_SIZE_ALLOC_DEFAULT 10000

// -----------------------------------------------------------------------------

/*
 * Struct of information required to track unique strings and their truelengths
 * when ordering them
 *
 * @member self A RAWSXP for the struct memory.
 *
 * @members strings,p_strings,strings_pi CHARSXPs originally containing a
 *   TRUELENGTH value >0, implying that base R was already using it and we
 *   need to reset it. These are rare.
 * @members truelengths,p_truelengths,truelengths_pi The original TRUELENGTHs
 *   of `strings`.
 * @member n_strings_alloc The allocated length of `strings`
 *   (and `truelengths`).
 * @member n_strings_used The number of `strings` currently in use.
 *
 * @members uniques,p_uniques,uniques_pi Unique CHARSXPs. Will be sorted in
 *   place by `chr_mark_sorted_uniques()`. We reset the TRUELENGTH of these
 *   to 0 (R's default) after ordering, then reset the TRUELENGTH of `strings`.
 * @member n_uniques_alloc The allocated length of `uniques`.
 * @member n_uniques_used The number of `uniques` currently in use.
 *
 * @members sizes,p_sizes,sizes_pi The sizes of each individual CHARSXP in
 *   `uniques`. Kept in the same ordering as `uniques` while sorting.
 * @members sizes_aux, p_sizes_aux, sizes_aux_pi Auxiliary vector of sizes
 *   that is used as working memory when sorting `sizes`.
 * @member n_sizes_alloc The allocated length of `sizes` (and `sizes_aux`).
 * @member n_sizes_used The number of `sizes` currently in use.
 * @member max_string_size The maximum string size of the unique strings stored
 *   in `uniques`. This controls the depth of recursion in `chr_radix_order()`.
 *
 * @member n_max The maximum allowed allocation size for the SEXP
 *   objects in this struct. Always set to the size of `x`, which would occur if
 *   all strings were unique.
 */
struct truelength_info {
  SEXP self;


  SEXP strings;
  SEXP* p_strings;
  PROTECT_INDEX strings_pi;

  SEXP truelengths;
  r_ssize* p_truelengths;
  PROTECT_INDEX truelengths_pi;

  r_ssize n_strings_alloc;
  r_ssize n_strings_used;


  SEXP uniques;
  SEXP* p_uniques;
  PROTECT_INDEX uniques_pi;

  r_ssize n_uniques_alloc;
  r_ssize n_uniques_used;


  SEXP sizes;
  int* p_sizes;
  PROTECT_INDEX sizes_pi;

  SEXP sizes_aux;
  int* p_sizes_aux;
  PROTECT_INDEX sizes_aux_pi;

  r_ssize n_sizes_alloc;
  r_ssize n_sizes_used;
  int max_string_size;


  r_ssize n_max;
};

#define PROTECT_TRUELENGTH_INFO(p_info, p_n) do {                       \
  PROTECT((p_info)->self);                                              \
  PROTECT_WITH_INDEX((p_info)->strings, &(p_info)->strings_pi);         \
  PROTECT_WITH_INDEX((p_info)->truelengths, &(p_info)->truelengths_pi); \
  PROTECT_WITH_INDEX((p_info)->uniques, &(p_info)->uniques_pi);         \
  PROTECT_WITH_INDEX((p_info)->sizes, &(p_info)->sizes_pi);             \
  PROTECT_WITH_INDEX((p_info)->sizes_aux, &(p_info)->sizes_aux_pi);     \
  *(p_n) += 6;                                                          \
} while(0)


struct truelength_info* new_truelength_info(r_ssize n_max);
void truelength_reset(struct truelength_info* p_truelength_info);

void truelength_realloc_strings(struct truelength_info* p_truelength_info);
void truelength_realloc_uniques(struct truelength_info* p_truelength_info);
void truelength_realloc_sizes(struct truelength_info* p_truelength_info);

static inline
void truelength_save_string(SEXP string,
                            r_ssize truelength,
                            struct truelength_info* p_truelength_info) {
  if (p_truelength_info->n_strings_used == p_truelength_info->n_strings_alloc) {
    truelength_realloc_strings(p_truelength_info);
  }
  p_truelength_info->p_strings[p_truelength_info->n_strings_used] = string;
  p_truelength_info->p_truelengths[p_truelength_info->n_strings_used] = truelength;
  ++p_truelength_info->n_strings_used;
}

static inline
void truelength_save_unique(SEXP unique,
                            struct truelength_info* p_truelength_info) {
  if (p_truelength_info->n_uniques_used == p_truelength_info->n_uniques_alloc) {
    truelength_realloc_uniques(p_truelength_info);
  }
  p_truelength_info->p_uniques[p_truelength_info->n_uniques_used] = unique;
  ++p_truelength_info->n_uniques_used;
}

static inline
void truelength_save_size(int size,
                          struct truelength_info* p_truelength_info) {
  if (p_truelength_info->n_sizes_used == p_truelength_info->n_sizes_alloc) {
    truelength_realloc_sizes(p_truelength_info);
  }
  p_truelength_info->p_sizes[p_truelength_info->n_sizes_used] = size;
  ++p_truelength_info->n_sizes_used;
}

// -----------------------------------------------------------------------------
#endif
