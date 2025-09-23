static
r_obj* reduce_impl(r_obj* current,
                   r_obj* rest,
                   struct vctrs_arg* p_parent_arg,
                   struct counters* counters,
                   r_obj* (*impl)(r_obj* current,
                                  r_obj* next,
                                  struct counters* counters,
                                  void* data),
                   void* data);
