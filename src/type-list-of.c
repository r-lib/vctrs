#include "vctrs.h"
#include "utils.h"



// [[ include("utils.h") ]]
void init_list_of(SEXP x, SEXP ptype) {
  Rf_setAttrib(x, R_ClassSymbol, classes_list_of);
  Rf_setAttrib(x, syms_ptype, ptype);
}
