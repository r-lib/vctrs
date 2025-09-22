#ifndef VCTRS_RECODE_H
#define VCTRS_RECODE_H

#include "vctrs-core.h"
#include "list-combine.h"

r_obj* vec_recode_values(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_obj* default_,
  enum list_combine_unmatched unmatched,
  bool from_as_list_of_vectors,
  bool to_as_list_of_vectors,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_from_arg,
  struct vctrs_arg* p_to_arg,
  struct vctrs_arg* p_default_arg,
  r_obj* ptype,
  struct r_lazy error_call
);

r_obj* vec_replace_values(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  bool from_as_list_of_vectors,
  bool to_as_list_of_vectors,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_from_arg,
  struct vctrs_arg* p_to_arg,
  struct r_lazy error_call
);

#endif
