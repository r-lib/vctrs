#ifndef VCTRS_REP_H
#define VCTRS_REP_H

r_obj* vec_rep(r_obj* x,
               int times,
               struct r_lazy error_call,
               struct vctrs_arg* p_x_arg,
               struct vctrs_arg* p_times_arg);

r_obj* vec_rep_each(r_obj* x,
                    r_obj* times,
                    struct r_lazy error_call,
                    struct vctrs_arg* p_x_arg,
                    struct vctrs_arg* p_times_arg);

#endif
