#include "vctrs.h"

struct growable new_growable(SEXPTYPE type, int capacity) {
  struct growable g;

  g.x = Rf_allocVector(type, capacity);
  g.type = type;
  g.array = r_vec_unwrap(type, g.x);
  g.n = 0;
  g.capacity = capacity;

  return g;
}

SEXP growable_values(struct growable* g) {
  return Rf_lengthgets(g->x, g->n);
}
