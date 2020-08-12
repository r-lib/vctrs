#include "vctrs.h"
#include "utils.h"
#include "utils-rlang.h"


// node.h ------------------------------------------------------------

sexp* r_pairlist_find(sexp* node, sexp* tag) {
  while (node != r_null) {
    if (r_node_tag(node) == tag) {
      return node;
    }
    node = r_node_cdr(node);
  }

  return r_null;
}
