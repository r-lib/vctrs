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

  // Preallocate growable array
  // Use R_Calloc() so we can R_Realloc() as needed
  g_lst->g_array = R_Calloc(capacity, growable);

  g_lst->n = 0;
  g_lst->capacity = capacity;
}

void list_of_growable_push_growable(list_of_growable* g_lst, growable* g) {
  if (g_lst->n == g_lst->capacity) {
    g_lst->capacity *= 2;
    g_lst->g_array = R_Realloc(g_lst->g_array, g_lst->capacity, growable);
  }

  (g_lst->g_array)[g_lst->n] = *g;
  g_lst->n++;
}

void list_of_growable_free(list_of_growable* g_lst) {

  // Free individual growables
  for(int i = 0; i < g_lst->n; ++i) {
    growable_free(&(g_lst->g_array[i]));
  }

  // Free R_Calloc()'d array pointer
  R_Free(g_lst->g_array);
}
