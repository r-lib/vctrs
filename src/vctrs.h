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

struct list_of_growable {
  growable* g_array;
  int n;
  int capacity;
};
typedef struct list_of_growable list_of_growable;

void growable_init(growable* g, SEXPTYPE type, int capacity);
void growable_free(growable* g);
void growable_push_int(growable* g, int i);
SEXP growable_values(growable* g);

void list_of_growable_init(list_of_growable* g_lst, SEXPTYPE type, int capacity);
void list_of_growable_push_growable(list_of_growable* g_lst, growable* g);
void list_of_growable_free(list_of_growable* g_lst);
