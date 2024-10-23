#ifndef VCTRS_VEC_BOOL_H
#define VCTRS_VEC_BOOL_H

#include <rlang.h>

struct r_vector_bool {
  r_obj* shelter;

  r_obj* data;
  bool* v_data;

  r_ssize n;
};

static inline
struct r_vector_bool* r_new_vector_bool(r_ssize n) {
  r_obj* shelter = KEEP(r_alloc_list(2));

  r_obj* vec = r_alloc_raw(sizeof(struct r_vector_bool));
  r_list_poke(shelter, 0, vec);

  r_obj* data = r_alloc_raw(n * sizeof(bool));
  r_list_poke(shelter, 1, data);

  struct r_vector_bool* p_vec = r_raw_begin(vec);
  p_vec->shelter = shelter;
  p_vec->data = data;
  p_vec->v_data = r_raw_begin(data);
  p_vec->n = n;

  FREE(1);
  return p_vec;
}

static inline
bool* r_vector_bool_begin(struct r_vector_bool* p_vec) {
  return p_vec->v_data;
}
static inline
const bool* r_vector_bool_cbegin(struct r_vector_bool* p_vec) {
  return (const bool*) p_vec->v_data;
}

static inline
r_ssize r_vector_bool_length(struct r_vector_bool* p_vec) {
  return p_vec->n;
}

#endif
