#ifndef VCTRS_C_UNCHOP_H
#define VCTRS_C_UNCHOP_H

#include "vctrs-core.h"
#include "names.h"
#include "optional.h"

enum list_unchop_unmatched {
    LIST_UNCHOP_UNMATCHED_default = 0,
    LIST_UNCHOP_UNMATCHED_error = 1,
};

enum list_unchop_unmatched parse_unmatched(r_obj* unmatched, struct r_lazy call);
struct optional_r_ssize parse_size(r_obj* size);

r_obj* list_unchop(r_obj* xs,
                   r_obj* indices,
                   r_obj* default_,
                   r_obj* ptype,
                   struct optional_r_ssize size,
                   enum list_unchop_unmatched unmatched,
                   r_obj* name_spec,
                   const struct name_repair_opts* name_repair,
                   struct vctrs_arg* p_error_arg,
                   struct vctrs_arg* p_default_arg,
                   struct r_lazy error_call);


#endif
