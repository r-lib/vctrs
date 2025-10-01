#ifndef VCTRS_CONDITIONS_H
#define VCTRS_CONDITIONS_H

#include "vctrs-core.h"

r_no_return
void stop_scalar_type(SEXP x,
                      struct vctrs_arg* arg,
                      struct r_lazy call);
r_no_return
void stop_assert_size(r_ssize actual,
                      r_ssize required,
                      struct vctrs_arg* arg,
                      struct r_lazy call);
r_no_return
void stop_incompatible_type(SEXP x,
                            SEXP y,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg,
                            bool cast);

r_no_return
void stop_incompatible_size(r_obj* x,
                            r_obj* y,
                            r_ssize x_size,
                            r_ssize y_size,
                            struct vctrs_arg* x_arg,
                            struct vctrs_arg* y_arg,
                            struct r_lazy call);

r_no_return
void stop_recycle_incompatible_size(r_ssize x_size,
                                    r_ssize size,
                                    struct vctrs_arg* x_arg,
                                    struct r_lazy call);
r_no_return
void stop_incompatible_shape(SEXP x, SEXP y,
                             R_len_t x_size, R_len_t y_size, int axis,
                             struct vctrs_arg* p_x_arg, struct vctrs_arg* p_y_arg);
void stop_corrupt_factor_levels(SEXP x, struct vctrs_arg* arg) r_no_return;
void stop_corrupt_ordered_levels(SEXP x, struct vctrs_arg* arg) r_no_return;

#endif
