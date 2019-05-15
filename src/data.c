#include "vctrs.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy = NULL;


// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy(SEXP x) {
  int nprot = 0;
  struct vctrs_type_info info = PROTECT_TYPE_INFO(vec_typeof_info(x, false), &nprot);

  SEXP out;
  if (info.type == vctrs_type_s3) {
    out = vec_proxy_invoke(x, info.proxy_method);
  } else {
    out = x;
  }

  UNPROTECT(nprot);
  return out;
}
// [[ include("vctrs.h") ]]
SEXP vec_proxy_method(SEXP x) {
  return s3_find_method("vec_proxy", x);
}
// [[ include("vctrs.h") ]]
SEXP vec_proxy_invoke(SEXP x, SEXP method) {
  if (method == R_NilValue) {
    return x;
  } else {
    return vctrs_dispatch1(syms_vec_proxy, method, syms_x, x);
  }
}


void vctrs_init_data(SEXP ns) {
  syms_vec_proxy = Rf_install("vec_proxy");
}
