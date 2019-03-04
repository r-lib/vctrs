#include "vctrs.h"

void growable_init(growable* g, SEXPTYPE type, int capacity) {
  g->x = Rf_allocVector(type, capacity);
  PROTECT_WITH_INDEX(g->x, &g->idx);

  g->n = 0;
  g->capacity = capacity;
}

void growable_free(growable* g) {
  UNPROTECT(1);
}

void growable_push_int(growable* g, int i) {
  if (g->n == g->capacity) {
    g->capacity *= 2;
    g->x = Rf_lengthgets(g->x, g->capacity);
    REPROTECT(g->x, g->idx);
  }

  INTEGER(g->x)[g->n] = i;
  g->n++;
}

SEXP growable_values(growable* g) {
  return Rf_lengthgets(g->x, g->n);
}
