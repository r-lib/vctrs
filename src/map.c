#include "vctrs.h"
#include "slice.h"
#include "utils.h"

// Defined at load time
SEXP vec_map_call = NULL;


SEXP vctrs_map(SEXP args) {
    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP env = CAR(args); args = CDR(args);
    SEXP ptype = CAR(args);

    SEXP orig = x;
    bool list_input = vec_is_list(orig);
    bool list_output = vec_is_list(ptype);

    if (list_input) {
      x = PROTECT(x);
    } else {
      x = PROTECT(vec_chop2(x));
    }

    r_ssize n = r_length(x);
    const SEXP* p_x = VECTOR_PTR_RO(x);

    if (ptype == R_NilValue) {
      r_abort("`.ptype` can't be NULL.");
    }

    SEXP out;
    SEXP out_proxy;

    if (list_output) {
      out = PROTECT(r_clone_referenced(x));
      SET_ATTRIB(out, R_NilValue);
      SET_OBJECT(out, 0);
      out_proxy = PROTECT(R_NilValue);
    } else {
      out = PROTECT(vec_init(ptype, n));
      out_proxy = PROTECT(vec_proxy(out));
    }

    SEXP loc = PROTECT(compact_seq(0, 0, true));
    int* p_loc = INTEGER(loc);

    if (list_output) {
      for (r_ssize i = 0; i < n; ++i) {
        r_env_poke(env, syms_dot_elt, p_x[i]);
        SET_VECTOR_ELT(out, i, r_eval_force(vec_map_call, env));
      }

      // Should use a ptype identity check before casting. Probably
      // `vec_cast()` should make that check.
      if (OBJECT(ptype)) {
        out = vec_cast(out, ptype, NULL, NULL);
      }
      PROTECT(out);
    } else {
      PROTECT_INDEX out_proxy_pi;
      PROTECT_WITH_INDEX(out_proxy, &out_proxy_pi);

      for (r_ssize i = 0; i < n; ++i) {
        r_env_poke(env, syms_dot_elt, p_x[i]);

        SEXP elt_out = PROTECT(r_eval_force(vec_map_call, env));
        elt_out = PROTECT(vec_cast(elt_out, ptype, NULL, NULL));

        init_compact_seq(p_loc, i, 1, true);
        out_proxy = vec_proxy_assign(out_proxy, loc, elt_out);

        UNPROTECT(2);
        REPROTECT(out_proxy, out_proxy_pi);
      }

      out = vec_restore(out_proxy, ptype, R_NilValue, VCTRS_OWNED_true);
      UNPROTECT(1);
      PROTECT(out);
    }

    SEXP names = PROTECT(vec_names(orig));
    vec_set_names(out, names);

    UNPROTECT(6);
    return out;
}


void vctrs_init_map(SEXP ns) {
  vec_map_call = r_parse(".fn(.elt, ...)");
  R_PreserveObject(vec_map_call);
}
