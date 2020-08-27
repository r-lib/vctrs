#ifndef VCTRS_UTILS_RLANG_H
#define VCTRS_UTILS_RLANG_H

typedef struct SEXPREC sexp;
#define r_null R_NilValue

#define r_syms_names R_NamesSymbol
#define r_syms_class R_ClassSymbol

#define KEEP PROTECT
#define FREE UNPROTECT


// node.h ------------------------------------------------------------

static inline sexp* r_node_car(sexp* x) { return CAR(x); }
static inline sexp* r_node_cdr(sexp* x) { return CDR(x); }
static inline sexp* r_node_tag(sexp* x) { return TAG(x); }
static inline sexp* r_node_caar(sexp* x) { return CAAR(x); }
static inline sexp* r_node_cadr(sexp* x) { return CADR(x); }
static inline sexp* r_node_cdar(sexp* x) { return CDAR(x); }
static inline sexp* r_node_cddr(sexp* x) { return CDDR(x); }


static inline
sexp* r_node_poke_car(sexp* x, sexp* newcar) {
  SETCAR(x, newcar);
  return x;
}
static inline
sexp* r_node_poke_cdr(sexp* x, sexp* newcdr) {
  SETCDR(x, newcdr);
  return x;
}
static inline
sexp* r_node_poke_tag(sexp* x, sexp* tag) {
  SET_TAG(x, tag);
  return x;
}
static inline
sexp* r_node_poke_caar(sexp* x, sexp* newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
static inline
sexp* r_node_poke_cadr(sexp* x, sexp* newcar) {
  SETCADR(x, newcar);
  return x;
}
static inline
sexp* r_node_poke_cdar(sexp* x, sexp* newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
static inline
sexp* r_node_poke_cddr(sexp* x, sexp* newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

static inline
sexp* r_new_node(sexp* car, sexp* cdr) {
  return Rf_cons(car, cdr);
}
static inline
sexp* r_new_node3(sexp* car, sexp* cdr, sexp* tag) {
  sexp* out = Rf_cons(car, cdr);
  SET_TAG(out, tag);
  return out;
}

sexp* r_pairlist_find(sexp* node, sexp* tag);
sexp* r_pairlist_rev(sexp* node);

static inline
sexp* r_pairlist_get(sexp* node, sexp* tag) {
  return r_node_car(r_pairlist_find(node, tag));
}

static inline
sexp* r_pairlist_poke(sexp* node, sexp* tag, sexp* value) {
  sexp* x = r_pairlist_find(node, tag);

  if (x == R_NilValue) {
    node = r_new_node(value, node);
    r_node_poke_tag(node, tag);
    return node;
  } else {
    r_node_poke_car(x, value);
    return node;
  }
}

static inline
sexp* r_pairlist_find_last(sexp* x) {
  while (CDR(x) != R_NilValue)
    x = CDR(x);
  return x;
}


// attrs.h -----------------------------------------------------------

static inline
sexp* r_attrib(sexp* x) {
  return ATTRIB(x);
}
static inline
sexp* r_poke_attrib(sexp* x, sexp* attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}

// Unlike Rf_getAttrib(), this never allocates. This also doesn't bump
// refcounts or namedness.
static inline
sexp* r_attrib_get(sexp* x, sexp* tag) {
  return r_pairlist_get(r_attrib(x), tag);
}

SEXP r_clone_shared(SEXP x);

static inline
void r_attrib_poke(sexp* x, sexp* tag, sexp* value) {
  sexp* attrib = KEEP(r_clone_shared(r_attrib(x)));
  r_poke_attrib(x, r_pairlist_poke(attrib, tag, value));
  FREE(1);
  return;
}

static inline
sexp* r_names(sexp* x) {
  return r_attrib_get(x, r_syms_names);
}
static inline
sexp* r_class(sexp* x) {
  return r_attrib_get(x, r_syms_class);
}


#endif
