#ifndef VCTRS_OPTIONAL_H
#define VCTRS_OPTIONAL_H

#include <rlang.h>

struct optional_r_ssize {
    bool some;
    r_ssize value;
};

extern struct optional_r_ssize optional_r_ssize_none;

static inline
struct optional_r_ssize new_optional_r_ssize(r_ssize x) {
    return (struct optional_r_ssize) {
        .some = true,
        .value = x
    };
}

static inline
bool optional_r_ssize_is_some(struct optional_r_ssize x) {
    return x.some;
}
static inline
bool optional_r_ssize_is_none(struct optional_r_ssize x) {
    return !x.some;
}

static inline
r_ssize optional_r_ssize_unwrap(struct optional_r_ssize x) {
    if (optional_r_ssize_is_some(x)) {
        return x.value;
    } else {
        r_stop_unreachable();
    }
}

#endif
