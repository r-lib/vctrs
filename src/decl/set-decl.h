static inline
void vec_set_intersect_loop(
  struct dictionary* x_dict,
  struct dictionary* y_dict,
  r_ssize x_size,
  r_ssize y_size,
  bool* v_marked
);

static inline
void vec_set_difference_loop(
  struct dictionary* x_dict,
  struct dictionary* y_dict,
  r_ssize x_size,
  r_ssize y_size,
  bool* v_marked
);

static inline
r_ssize vec_set_union_x_loop(
  struct dictionary* x_dict,
  r_ssize x_size,
  bool* v_marked
);

static inline
r_ssize vec_set_union_y_loop(
  struct dictionary* x_dict,
  struct dictionary* y_dict,
  r_ssize x_size,
  r_ssize y_size,
  bool* v_marked
);

static inline
struct r_ssize_pair vec_set_symmetric_difference_loop(
  struct dictionary* x_dict,
  struct dictionary* y_dict,
  r_ssize x_size,
  r_ssize y_size,
  bool* v_x_marked,
  bool* v_y_marked
);
