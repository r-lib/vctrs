#include "vctrs.h"
#include "utils.h"

struct growable new_growable(SEXPTYPE type, int capacity) {
  struct growable g;

  g.x = Rf_allocVector(type, capacity);
  g.type = type;
  g.array = r_vec_unwrap(type, g.x);
  g.n = 0;
  g.capacity = capacity;

  return g;
}

void growable_push_int(struct growable* g, int i) {
  if (g->n == g->capacity) {
    g->capacity *= 2;
    g->x = Rf_lengthgets(g->x, g->capacity);
    REPROTECT(g->x, g->idx);
    g->array = INTEGER(g->x);
  }

  int* p = (int*) g->array;
  p[g->n] = i;
  ++(g->n);
}

SEXP growable_values(struct growable* g) {
  return Rf_lengthgets(g->x, g->n);
}
