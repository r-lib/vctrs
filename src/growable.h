#ifndef VCTRS_GROWABLE_H_
#define VCTRS_GROWABLE_H_

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdint.h>

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

#endif
