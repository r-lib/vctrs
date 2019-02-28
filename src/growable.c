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

// Growable int ---------------------------------------------------

void growable_int_init(growable_int* g, int capacity) {
  g->x = R_Calloc(capacity, int);
  g->n = 0;
  g->capacity = capacity;
  g->instances = 0;
}

void growable_int_unprotect(growable_int* g) {
  // manage as many instances as were created by
  // growable_int_values()
  UNPROTECT(g->instances);
}

void growable_int_free(growable_int* g) {
  R_Free(g->x);
}

void growable_int_push_int(growable_int* g, int i) {
  if (g->n == g->capacity) {
    g->capacity *= 2;
    g->x = R_Realloc(g->x, g->capacity, int);
  }

  g->x[g->n] = i;
  g->n++;
}

SEXP growable_int_values(growable_int* g) {

  // UNPROTECTed by a call to growable_int_unprotect()
  SEXP out = PROTECT(Rf_allocVector(INTSXP, g->n));
  int* p_out = INTEGER(out);

  for (int i = 0; i < g->n; ++i) {
    p_out[i] = g->x[i];
  }

  g->instances++;

  return out;
}

// Growable of Growable int ---------------------------------------------------


void growable_of_growable_int_init(growable_of_growable_int* g_of_gi,
                                   int capacity) {

  // Preallocate growable int array
  // Use R_Calloc() so we can R_Realloc() as needed
  g_of_gi->g_array = R_Calloc(capacity, growable_int);

  g_of_gi->n = 0;
  g_of_gi->capacity = capacity;
  g_of_gi->instances = 0;
}

void growable_of_growable_int_push_growable_int(growable_of_growable_int* g_of_gi,
                                                growable_int* g) {
  if (g_of_gi->n == g_of_gi->capacity) {
    g_of_gi->capacity *= 2;
    g_of_gi->g_array = R_Realloc(g_of_gi->g_array, g_of_gi->capacity, growable_int);
  }

  (g_of_gi->g_array)[g_of_gi->n] = *g;
  g_of_gi->n++;
}

void growable_of_growable_int_free(growable_of_growable_int* g_of_gi) {

  // Free individual growables_int memory
  // (but no need to unprotect as that was done by `values`)
  for(int i = 0; i < g_of_gi->n; ++i) {
    growable_int_free(&(g_of_gi->g_array[i]));
  }

  UNPROTECT(g_of_gi->instances);

  // Free R_Calloc()'d array pointer
  R_Free(g_of_gi->g_array);
}

SEXP growable_of_growable_int_values(growable_of_growable_int* g_of_gi) {

  // UNPROTECTED by a call to growable_of_growable_int_free()
  SEXP out = PROTECT(Rf_allocVector(VECSXP, g_of_gi->n));

  for (int i = 0; i < g_of_gi->n; ++i) {

    SET_VECTOR_ELT(out, i, growable_int_values(&g_of_gi->g_array[i]));

    // PROTECTion now managed by the list
    growable_int_unprotect(&g_of_gi->g_array[i]);
  }

  g_of_gi->instances++;

  return out;
}

