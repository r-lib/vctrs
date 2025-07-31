#ifndef VCTRS_NAMES_H
#define VCTRS_NAMES_H

#include "vctrs-core.h"
#include "owned.h"
#include "utils.h"

r_obj* vec_names(r_obj* x);
r_obj* vec_names2(r_obj* x);
r_obj* vec_proxy_names(r_obj* x);

r_obj* vec_unique_names(r_obj* x, bool quiet);
r_obj* vec_unique_colnames(r_obj* x, bool quiet);

r_obj* outer_names(r_obj* names, r_obj* outer, r_ssize n);
r_obj* apply_name_spec(r_obj* name_spec, r_obj* outer, r_obj* inner, r_ssize n);
bool name_spec_is_inner(r_obj* name_spec);

enum name_repair_type {
  NAME_REPAIR_none = 0,
  NAME_REPAIR_minimal,
  NAME_REPAIR_unique,
  NAME_REPAIR_universal,
  NAME_REPAIR_check_unique,
  NAME_REPAIR_custom = 99
};

struct name_repair_opts {
  r_obj* shelter;
  enum name_repair_type type;
  struct r_lazy name_repair_arg;
  r_obj* fn;
  bool quiet;
  struct r_lazy call;
};

struct name_repair_opts new_name_repair_opts(r_obj* name_repair,
                                             struct r_lazy name_repair_arg,
                                             bool quiet,
                                             struct r_lazy call);

r_obj* vec_as_universal_names(r_obj* names, bool quiet);
r_obj* vec_as_custom_names(r_obj* names, const struct name_repair_opts* opts);

extern r_obj* name_spec_inner;

extern struct name_repair_opts unique_repair_default_opts;
extern struct name_repair_opts unique_repair_silent_opts;
extern struct name_repair_opts no_repair_opts;

static struct name_repair_opts const * const p_unique_repair_default_opts = &unique_repair_default_opts;
static struct name_repair_opts const * const p_unique_repair_silent_opts = &unique_repair_silent_opts;
static struct name_repair_opts const * const p_no_repair_opts = &no_repair_opts;

r_obj* vec_as_names(r_obj* names, const struct name_repair_opts* opts);
const char* name_repair_arg_as_c_string(enum name_repair_type type);
bool is_unique_names(r_obj* names);
r_obj* vec_as_unique_names(r_obj* names, bool quiet);

r_obj* r_seq_chr(const char* prefix, r_ssize n);
r_obj* r_chr_paste_prefix(r_obj* names, const char* prefix, const char* sep);

r_obj* vec_set_names(r_obj* x, r_obj* names);
r_obj* vec_proxy_set_names(r_obj* x, r_obj* names, const enum vctrs_owned owned);


#endif
