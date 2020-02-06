#ifndef VCTRS_NAMES_H
#define VCTRS_NAMES_H

enum name_repair_arg {
  name_repair_none,
  name_repair_minimal,
  name_repair_unique,
  name_repair_universal,
  name_repair_check_unique
};

const char* name_repair_arg_as_c_string(enum name_repair_arg arg);
enum name_repair_arg validate_name_repair(SEXP arg);
SEXP vec_as_names(SEXP names, enum name_repair_arg type, bool quiet);
bool is_unique_names(SEXP names);
SEXP vec_as_unique_names(SEXP names, bool quiet);

#endif
