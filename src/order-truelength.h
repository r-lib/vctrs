#ifndef VCTRS_ORDER_TRUELENGTH_H
#define VCTRS_ORDER_TRUELENGTH_H

#include "vctrs.h"

// -----------------------------------------------------------------------------

// This seems to be a reasonable default to start with for tracking the original
// truelengths of the unique strings, and is what base R uses. It is expanded
// by 2x every time we need to reallocate.
#define TRUELENGTH_SIZE_ALLOC_DEFAULT 10000

// -----------------------------------------------------------------------------

/*
 * Struct of information required to track truelengths of character vectors
 * when ordering them
 *
 * @members strings,p_strings,strings_pi The unique CHARSXP seen during
 *   ordering.
 * @members lengths,p_lengths,lengths_pi The original truelengths of the
 *   `strings`.
 * @members uniques,p_uniques,uniques_pi At first, this is the same as `strings`
 *   until `chr_mark_sorted_uniques()` is called, which reorders them in place
 *   and sorts them.
 * @members sizes,p_sizes,sizes_pi The sizes of each individual CHARSXP in
 *   `uniques`. Kept in the same ordering as `uniques` while sorting.
 * @members sizes_aux, p_sizes_aux, sizes_aux_pi Auxiliary vector of sizes
 *   that is used as working memory when sorting `uniques`.
 *
 * @member size_alloc The current allocated size of the SEXP objects in this
 *   struct
 * @member max_size_alloc The maximum allowed allocation size for the SEXP
 *   objects in this struct. Set to the size of `x`, which would occur if
 *   all strings were unique.
 * @member size_used The number of unique strings currently in `strings`.
 * @member max_string_size The maximum string size of the unique strings stored
 *   in `strings`. This controls the depth of recursion in `chr_radix_order()`.
 * @member reencode Should the encoding be checked to see if a translation to
 *   UTF-8 is required? This is used by `df_order()` when extracting out each
 *   group chunk, and by `chr_order()` to decide whether the input needs to be
 *   copied and re-encoded.
 */
struct truelength_info {
  SEXP strings;
  SEXP* p_strings;
  PROTECT_INDEX strings_pi;

  SEXP lengths;
  R_xlen_t* p_lengths;
  PROTECT_INDEX lengths_pi;

  SEXP uniques;
  SEXP* p_uniques;
  PROTECT_INDEX uniques_pi;

  SEXP sizes;
  int* p_sizes;
  PROTECT_INDEX sizes_pi;

  SEXP sizes_aux;
  int* p_sizes_aux;
  PROTECT_INDEX sizes_aux_pi;

  R_xlen_t size_alloc;
  R_xlen_t max_size_alloc;
  R_xlen_t size_used;

  R_xlen_t max_string_size;

  bool reencode;
};

#define PROTECT_TRUELENGTH_INFO(p_info, p_n) do {                   \
  PROTECT_WITH_INDEX((p_info)->strings, &(p_info)->strings_pi);     \
  PROTECT_WITH_INDEX((p_info)->lengths, &(p_info)->lengths_pi);     \
  PROTECT_WITH_INDEX((p_info)->uniques, &(p_info)->uniques_pi);     \
  PROTECT_WITH_INDEX((p_info)->sizes, &(p_info)->sizes_pi);         \
  PROTECT_WITH_INDEX((p_info)->sizes_aux, &(p_info)->sizes_aux_pi); \
  *(p_n) += 5;                                                      \
} while(0)


struct truelength_info new_truelength_info(R_xlen_t max_size_alloc);
void truelength_reset(struct truelength_info* p_truelength_info);

void truelength_save(SEXP x,
                     R_xlen_t truelength,
                     R_xlen_t size,
                     struct truelength_info* p_truelength_info);

// -----------------------------------------------------------------------------
#endif
