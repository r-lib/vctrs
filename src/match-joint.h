#ifndef VCTRS_MATCH_JOINT_H
#define VCTRS_MATCH_JOINT_H

#include "vctrs-core.h"

r_obj* vec_joint_xtfrm(r_obj* x,
                       r_obj* y,
                       r_ssize x_size,
                       r_ssize y_size,
                       bool nan_distinct,
                       r_obj* chr_proxy_collate);

#endif
