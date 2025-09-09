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

static inline
void p_bool_fill(bool* v_x, r_ssize size, bool value) {
  for (r_ssize i = 0; i < size; ++i) {
    v_x[i] = value;
  }
}

static inline
void r_vector_bool_fill(struct r_vector_bool* p_vec, bool value) {
  bool* v_data = r_vector_bool_begin(p_vec);
  const r_ssize size = r_vector_bool_length(p_vec);
  p_bool_fill(v_data, size, value);
}

static inline
bool p_bool_any(const bool* v_x, r_ssize size) {
  for (r_ssize i = 0; i < size; ++i) {
    if (v_x[i]) {
      return true;
    }
  }

  return false;
}

static inline
bool r_vector_bool_any(struct r_vector_bool* p_vec) {
  const bool* v_data = r_vector_bool_cbegin(p_vec);
  const r_ssize size = r_vector_bool_length(p_vec);
  return p_bool_any(v_data, size);
}

static inline
r_ssize p_bool_sum(const bool* v_x, r_ssize size) {
  r_ssize out = 0;

  for (r_ssize i = 0; i < size; ++i) {
    out += v_x[i];
  }

  return out;
}

static inline
r_ssize r_vector_bool_sum(struct r_vector_bool* p_vec) {
  const bool* v_data = r_vector_bool_cbegin(p_vec);
  const r_ssize size = r_vector_bool_length(p_vec);
  return p_bool_sum(v_data, size);
}

static inline
r_obj* p_bool_which(const bool* v_x, r_ssize size) {
  const r_ssize out_size = p_bool_sum(v_x, size);

  r_obj* out = KEEP(r_alloc_integer(out_size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0, j = 0; i < size && j < out_size; ++i) {
    v_out[j] = i + 1;
    j += v_x[i];
  }

  FREE(1);
  return out;
}

static inline
r_obj* r_vector_bool_which(struct r_vector_bool* p_vec) {
  const bool* v_data = r_vector_bool_cbegin(p_vec);
  const r_ssize size = r_vector_bool_length(p_vec);
  return p_bool_which(v_data, size);
}

#endif
