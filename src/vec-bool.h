#ifndef VCTRS_VEC_BOOL_H
#define VCTRS_VEC_BOOL_H

#include <rlang.h>

struct r_bool_vector {
  r_obj* shelter;

  r_obj* data;
  bool* v_data;

  r_ssize n;
};

static inline
struct r_bool_vector* r_new_bool_vector(r_ssize n) {
  r_obj* shelter = KEEP(r_alloc_list(2));

  r_obj* vec = r_alloc_raw(sizeof(struct r_bool_vector));
  r_list_poke(shelter, 0, vec);

  r_obj* data = r_alloc_raw(n * sizeof(bool));
  r_list_poke(shelter, 1, data);

  struct r_bool_vector* p_vec = r_raw_begin(vec);
  p_vec->shelter = shelter;
  p_vec->data = data;
  p_vec->v_data = r_raw_begin(data);
  p_vec->n = n;

  FREE(1);
  return p_vec;
}

static inline
bool* r_bool_vector_begin(struct r_bool_vector* p_vec) {
  return p_vec->v_data;
}
static inline
const bool* r_bool_vector_cbegin(struct r_bool_vector* p_vec) {
  return (const bool*) p_vec->v_data;
}

static inline
r_ssize r_bool_vector_length(struct r_bool_vector* p_vec) {
  return p_vec->n;
}

#endif
