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
  vctrs_type_dataframe = 8,
  vctrs_type_s3        = 9,
  vctrs_type_scalar    = 10
};

enum vctrs_type vec_typeof(SEXP x);
const char* vec_type_as_str(enum vctrs_type type);
bool vec_is_vector(SEXP x);
void vctrs_stop_unsupported_type(enum vctrs_type, const char* fn) __attribute__((noreturn));

// After adding a new `vctrs_dispatch` type, add the missing entries
// in `vec_dispatch_typeof()`
enum vctrs_dispatch {
  vctrs_dispatch_null_null,
  vctrs_dispatch_null_logical,
  vctrs_dispatch_null_integer,
  vctrs_dispatch_null_double,
  vctrs_dispatch_null_complex,
  vctrs_dispatch_null_character,
  vctrs_dispatch_null_raw,
  vctrs_dispatch_null_list,
  vctrs_dispatch_null_dataframe,
  vctrs_dispatch_null_s3,
  vctrs_dispatch_null_scalar,

  vctrs_dispatch_logical_logical,
  vctrs_dispatch_logical_integer,
  vctrs_dispatch_logical_double,
  vctrs_dispatch_logical_complex,
  vctrs_dispatch_logical_character,
  vctrs_dispatch_logical_raw,
  vctrs_dispatch_logical_list,
  vctrs_dispatch_logical_dataframe,
  vctrs_dispatch_logical_s3,
  vctrs_dispatch_logical_scalar,

  vctrs_dispatch_integer_integer,
  vctrs_dispatch_integer_double,
  vctrs_dispatch_integer_complex,
  vctrs_dispatch_integer_character,
  vctrs_dispatch_integer_raw,
  vctrs_dispatch_integer_list,
  vctrs_dispatch_integer_dataframe,
  vctrs_dispatch_integer_s3,
  vctrs_dispatch_integer_scalar,

  vctrs_dispatch_double_double,
  vctrs_dispatch_double_complex,
  vctrs_dispatch_double_character,
  vctrs_dispatch_double_raw,
  vctrs_dispatch_double_list,
  vctrs_dispatch_double_dataframe,
  vctrs_dispatch_double_s3,
  vctrs_dispatch_double_scalar,

  vctrs_dispatch_complex_complex,
  vctrs_dispatch_complex_character,
  vctrs_dispatch_complex_raw,
  vctrs_dispatch_complex_list,
  vctrs_dispatch_complex_dataframe,
  vctrs_dispatch_complex_s3,
  vctrs_dispatch_complex_scalar,

  vctrs_dispatch_character_character,
  vctrs_dispatch_character_raw,
  vctrs_dispatch_character_list,
  vctrs_dispatch_character_dataframe,
  vctrs_dispatch_character_s3,
  vctrs_dispatch_character_scalar,

  vctrs_dispatch_raw_raw,
  vctrs_dispatch_raw_list,
  vctrs_dispatch_raw_dataframe,
  vctrs_dispatch_raw_s3,
  vctrs_dispatch_raw_scalar,

  vctrs_dispatch_list_list,
  vctrs_dispatch_list_dataframe,
  vctrs_dispatch_list_s3,
  vctrs_dispatch_list_scalar,

  vctrs_dispatch_dataframe_dataframe,
  vctrs_dispatch_dataframe_s3,
  vctrs_dispatch_dataframe_scalar,

  vctrs_dispatch_s3_s3,
  vctrs_dispatch_s3_scalar,

  vctrs_dispatch_scalar_scalar
};

enum vctrs_dispatch vec_dispatch_typeof(SEXP x, SEXP y);
const char* vctrs_dispatch_type_as_str(enum vctrs_dispatch type);

extern SEXP vctrs_shared_empty_lgl;
extern SEXP vctrs_shared_empty_int;
extern SEXP vctrs_shared_empty_dbl;
extern SEXP vctrs_shared_empty_cpl;
extern SEXP vctrs_shared_empty_chr;
extern SEXP vctrs_shared_empty_raw;
extern SEXP vctrs_shared_empty_list;

extern SEXP vctrs_shared_true;
extern SEXP vctrs_shared_false;

extern Rcomplex vctrs_shared_na_cpl;


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
int equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);
int compare_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);

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

// Shape --------------------------------------------------------

bool has_dim(SEXP x);
