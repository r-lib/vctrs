#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdbool.h>
#include <stdint.h>


// Vector types -------------------------------------------------

enum vctrs_type {
  vctrs_type_null      = 0,
  vctrs_type_logical   = 1,
  vctrs_type_integer   = 2,
  vctrs_type_double    = 3,
  vctrs_type_complex   = 4,
  vctrs_type_character = 5,
  vctrs_type_raw       = 6,
  vctrs_type_list      = 7,
  vctrs_type_dataframe = 8
};

enum vctrs_type vec_typeof(SEXP x);


// Vector methods ------------------------------------------------
R_len_t vec_size(SEXP x);

bool is_data_frame(SEXP x);
bool is_record(SEXP x);
bool is_scalar(SEXP x);

// Most vector predicates return `int` because missing values are
// propagated as `NA_LOGICAL`
int equal_object(SEXP x, SEXP y, bool na_equal);
bool equal_names(SEXP x, SEXP y);

/**
 * These functions are meant to be used in loops so it is the callers
 * responsibility to ensure that:
 *
 * - `x` and `y` have identical SEXTYPEs
 * - `i` is a valid index into `x`, and `j` is a valid index into `y`.
 *
 * The behaviour is undefined if these conditions are not true.
 */
int equal_scalar(SEXP x, int i, SEXP y, int j, bool na_equal);
int compare_scalar(SEXP x, int i, SEXP y, int j, bool na_equal);

int32_t hash_object(SEXP x);
int32_t hash_scalar(SEXP x, R_len_t i);

// Growable vector -----------------------------------------------

struct growable {
  SEXP x;
  int32_t idx;
  int n;
  int capacity;
};
typedef struct growable growable;

void growable_init(growable* g, SEXPTYPE type, int capacity);
void growable_free(growable* g);
void growable_push_int(growable* g, int i);
SEXP growable_values(growable* g);
