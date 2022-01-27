#include "type-vctr.h"
#include "vctrs.h"
#include "utils.h"

#include "decl/type-vctr-decl.h"

// [[ register(external = TRUE) ]]
r_obj* vctrs_new_vctr(r_obj* args) {
  args = r_node_cdr(args);

  r_obj* data = r_node_car(args); args = r_node_cdr(args);
  r_obj* cls = r_node_car(args); args = r_node_cdr(args);
  r_obj* inherit_base_type = r_node_car(args); args = r_node_cdr(args);
  r_obj* attributes = args;

  return new_vctr(
    data,
    cls,
    inherit_base_type,
    attributes
  );
}

// [[ include("type-vctr.h") ]]
r_obj* new_vctr(r_obj* data,
                r_obj* cls,
                r_obj* inherit_base_type,
                r_obj* attributes) {
  if (!r_is_vector(data)) {
    r_abort("`.data` must be a vector type.");
  }
  if (r_typeof(cls) != R_TYPE_character) {
    r_abort("`class` must be a character vector.");
  }
  if ((inherit_base_type != r_null) && !r_is_bool(inherit_base_type)) {
    r_abort("`inherit_base_type` must be `NULL` or a single `TRUE` or `FALSE`.");
  }

  const enum r_type type_attributes = r_typeof(attributes);

  if (type_attributes != R_TYPE_pairlist && type_attributes != R_TYPE_null) {
    r_stop_internal("new_vctr", "`attributes` must be a pairlist or `NULL`.");
  }

  const enum r_type type = r_typeof(data);

  if (type == R_TYPE_list && r_inherits(data, "data.frame")) {
    r_abort("`.data` can't be a data frame.");
  }

  bool c_inherit_base_type = false;

  if (type == R_TYPE_list) {
    if (inherit_base_type == r_null) {
      // List types always inherit the base type
      c_inherit_base_type = true;
    } else {
      c_inherit_base_type = r_lgl_get(inherit_base_type, 0);

      if (!c_inherit_base_type) {
        r_abort("List `.data` must inherit from the base type.");
      }
    }
  } else {
    if (inherit_base_type == r_null) {
      c_inherit_base_type = false;
    } else {
      c_inherit_base_type = r_lgl_get(inherit_base_type, 0);
    }
  }

  bool has_names_in_attributes = false;

  for (r_obj* node = attributes; node != R_NilValue; node = r_node_cdr(node)) {
    r_obj* tag = r_node_tag(node);

    if (tag == R_ClassSymbol) {
      // Check for this in case we ever allow dynamic dots
      r_abort("Can't supply `class` in `...`.");
    }

    if (tag == R_NamesSymbol) {
      has_names_in_attributes = true;
    }
  }

  r_keep_t pi;
  KEEP_HERE(attributes, &pi);

  if (!has_names_in_attributes) {
    // Take names from `data` if `attributes` doesn't have any
    r_obj* names = KEEP(r_names(data));
    names = KEEP(names_repair_missing(names));

    if (names != r_null) {
      attributes = r_new_node3(names, attributes, R_NamesSymbol);
      KEEP_AT(attributes, pi);
    }

    FREE(2);
  }

  cls = KEEP(chr_c(cls, classes_vctrs_vctr));

  if (c_inherit_base_type) {
    r_obj* base_type = KEEP(r_type_as_character(type));
    cls = chr_c(cls, base_type);
    FREE(1);
  }
  KEEP(cls);

  attributes = r_new_node3(cls, attributes, R_ClassSymbol);
  KEEP_AT(attributes, pi);

  // Required conversion to VECSXP for `attributes<-`
  attributes = KEEP(Rf_PairToVectorList(attributes));

  // We don't have access to `Rf_shallow_duplicate_attr()`, which can create
  // an ALTREP wrapper cheaply, but `vec_set_attributes()` does through
  // `attributes<-`
  r_obj* out = r_eval_with_xy(
    vec_set_attributes_call,
    data,
    attributes,
    vctrs_ns_env
  );

  FREE(4);
  return out;
}


// [[ register() ]]
r_obj* vctrs_name_repair_missing(r_obj* x) {
  return names_repair_missing(x);
}

static
r_obj* names_repair_missing(r_obj* x) {
  // We never want to allow `NA_character_` names to slip through, but
  // erroring on them has caused issues. Instead, we repair them to the
  // empty string (#784).

  if (x == r_null) {
    return x;
  }

  if (r_typeof(x) != R_TYPE_character) {
    r_abort("`x` must be a character vector of names.");
  }

  const r_ssize size = r_length(x);
  r_obj* const* v_x = r_chr_cbegin(x);

  r_ssize i = 0;
  bool any_missing = false;

  for (; i < size; ++i) {
    if (v_x[i] == r_globals.na_str) {
      any_missing = true;
      break;
    }
  }

  if (!any_missing) {
    return x;
  }

  r_obj* out = KEEP(r_clone(x));

  for (; i < size; ++i) {
    if (v_x[i] == r_globals.na_str) {
      r_chr_poke(out, i, strings_empty);
    }
  }

  FREE(1);
  return out;
}


void vctrs_init_type_vctr(r_obj* ns) {
  classes_vctrs_vctr = r_new_shared_vector(R_TYPE_character, 1);
  r_chr_poke(classes_vctrs_vctr, 0, r_str("vctrs_vctr"));

  vec_set_attributes_call = r_parse("vec_set_attributes(x, y)");
  r_preserve(vec_set_attributes_call);
}
