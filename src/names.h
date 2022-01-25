#ifndef VCTRS_NAMES_H
#define VCTRS_NAMES_H

#include "utils.h"

enum name_repair_type {
  name_repair_none = 0,
  name_repair_minimal,
  name_repair_unique,
  name_repair_universal,
  name_repair_check_unique,
  name_repair_custom = 99
};

struct name_repair_opts {
  r_obj* shelter;
  enum name_repair_type type;
  struct vctrs_arg* name_repair_arg;
  r_obj* fn;
  bool quiet;
  struct r_lazy call;
};

extern struct name_repair_opts unique_repair_default_opts;
extern struct name_repair_opts unique_repair_silent_opts;
extern struct name_repair_opts no_repair_opts;

static struct name_repair_opts const * const p_unique_repair_default_opts = &unique_repair_default_opts;
static struct name_repair_opts const * const p_unique_repair_silent_opts = &unique_repair_silent_opts;
static struct name_repair_opts const * const p_no_repair_opts = &no_repair_opts;

SEXP vec_as_names(SEXP names, const struct name_repair_opts* opts);
struct name_repair_opts new_name_repair_opts(r_obj* name_repair,
                                             struct vctrs_arg* name_repair_arg,
                                             bool quiet,
                                             struct r_lazy call);
const char* name_repair_arg_as_c_string(enum name_repair_type type);
bool is_unique_names(SEXP names);
SEXP vec_as_unique_names(SEXP names, bool quiet);

SEXP r_seq_chr(const char* prefix, R_len_t n);
SEXP r_chr_paste_prefix(SEXP names, const char* prefix, const char* sep);

#include "owned.h"
SEXP vec_proxy_set_names(SEXP x, SEXP names, const enum vctrs_owned owned);


#endif
