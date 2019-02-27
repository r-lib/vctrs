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

// Growable list of growable ---------------------------------------------------

void list_of_growable_init(list_of_growable* g_lst,
                           SEXPTYPE type,
                           int capacity) {

  // Preallocate growable things
  g_lst->g_array = (growable*) R_alloc(capacity, sizeof(growable));

  g_lst->n = 0;
  g_lst->capacity = capacity;
}

void list_of_growable_push_growable(list_of_growable* g_lst, growable* g) {
  if (g_lst->n == g_lst->capacity) {
    g_lst->capacity *= 2;

    // Reallocate and copy over?
    growable* old_g = g_lst->g_array;
    g_lst->g_array = (growable*) R_alloc(g_lst->capacity, sizeof(growable));

    for (int i = 0; i < g_lst->n; ++i) {
      g_lst->g_array[i] = old_g[i];
    }

  }

  (g_lst->g_array)[g_lst->n] = *g;
  g_lst->n++;
}

void list_of_growable_free(list_of_growable* g_lst) {
  for(int i = 0; i < g_lst->n; ++i) {
    growable_free(&(g_lst->g_array[i]));
  }
}
