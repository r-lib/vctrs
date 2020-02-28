#ifndef VCTRS_SUBSCRIPT_H
#define VCTRS_SUBSCRIPT_H

#include "utils.h"


enum subscript_type_action {
  SUBSCRIPT_TYPE_ACTION_CAST,
  SUBSCRIPT_TYPE_ACTION_ERROR
};

struct vec_as_subscript_opts {
  enum subscript_type_action logical;
  enum subscript_type_action numeric;
  enum subscript_type_action character;
  struct vctrs_arg* subscript_arg;
};

SEXP vec_as_subscript_opts(SEXP subscript,
                           const struct vec_as_subscript_opts* opts,
                           ERR* err);

static inline SEXP subscript_type_action_chr(enum subscript_type_action action) {
  switch (action) {
  case SUBSCRIPT_TYPE_ACTION_CAST: return chrs_cast;
  case SUBSCRIPT_TYPE_ACTION_ERROR: return chrs_error;
  }
  never_reached("subscript_type_action_chr");
}

#endif
