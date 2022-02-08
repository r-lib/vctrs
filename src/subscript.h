#ifndef VCTRS_SUBSCRIPT_H
#define VCTRS_SUBSCRIPT_H

#include "vctrs-core.h"
#include "utils.h"


enum subscript_action {
  SUBSCRIPT_ACTION_DEFAULT = 0,
  SUBSCRIPT_ACTION_SUBSET,
  SUBSCRIPT_ACTION_EXTRACT,
  SUBSCRIPT_ACTION_ASSIGN,
  SUBSCRIPT_ACTION_RENAME,
  SUBSCRIPT_ACTION_REMOVE,
  SUBSCRIPT_ACTION_NEGATE
};
enum subscript_type_action {
  SUBSCRIPT_TYPE_ACTION_CAST = 0,
  SUBSCRIPT_TYPE_ACTION_ERROR
};

struct subscript_opts {
  enum subscript_action action;
  enum subscript_type_action logical;
  enum subscript_type_action numeric;
  enum subscript_type_action character;
  struct vctrs_arg* subscript_arg;
  struct r_lazy call;
};

static inline
struct subscript_opts new_subscript_opts_assign() {
  return (struct subscript_opts) {
    .action = SUBSCRIPT_ACTION_ASSIGN
  };
}

SEXP vec_as_subscript_opts(SEXP subscript,
                           const struct subscript_opts* opts,
                           ERR* err);

static inline SEXP subscript_type_action_chr(enum subscript_type_action action) {
  switch (action) {
  case SUBSCRIPT_TYPE_ACTION_CAST: return chrs_cast;
  case SUBSCRIPT_TYPE_ACTION_ERROR: return chrs_error;
  }
  never_reached("subscript_type_action_chr");
}

static inline SEXP get_opts_action(const struct subscript_opts* opts) {
  switch (opts->action) {
  case SUBSCRIPT_ACTION_DEFAULT: return R_NilValue;
  case SUBSCRIPT_ACTION_SUBSET: return chrs_subset;
  case SUBSCRIPT_ACTION_EXTRACT: return chrs_extract;
  case SUBSCRIPT_ACTION_ASSIGN: return chrs_assign;
  case SUBSCRIPT_ACTION_RENAME: return chrs_rename;
  case SUBSCRIPT_ACTION_REMOVE: return chrs_remove;
  case SUBSCRIPT_ACTION_NEGATE: return chrs_negate;
  }
  never_reached("get_opts_action");
}


#endif
