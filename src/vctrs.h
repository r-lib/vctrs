#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdbool.h>
#include <stdint.h>

// Vector methods ------------------------------------------------
R_len_t vec_size(SEXP x);

bool is_data_frame(SEXP x);
bool is_record(SEXP x);
bool is_scalar(SEXP x);

bool equal_object(SEXP x, SEXP y);
bool equal_names(SEXP x, SEXP y);

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

// Growable int array -----------------------------------------------

struct growable_int {
  int* x;
  int n;
  int capacity;
  int instances;
};
typedef struct growable_int growable_int;

void growable_int_init(growable_int* g, int capacity);
void growable_int_free(growable_int* g);
void growable_int_push_int(growable_int* g, int i);
SEXP growable_int_values(growable_int* g);

struct growable_of_growable_int {
  growable_int* g_array;
  int n;
  int capacity;
  int instances;
};
typedef struct growable_of_growable_int growable_of_growable_int;

void growable_of_growable_int_init(growable_of_growable_int* g_of_gi, int capacity);
void growable_of_growable_int_push_growable_int(growable_of_growable_int* g_of_gi, growable_int* g);
void growable_of_growable_int_free(growable_of_growable_int* g_of_gi);
SEXP growable_of_growable_int_values(growable_of_growable_int* g_of_gi);
