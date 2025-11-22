#ifndef VCTRS_PROXY_DATA_H
#define VCTRS_PROXY_DATA_H

#include "vctrs-core.h"

/// Returns the "core" data that underlies a proxy
///
/// Should only be called on the result of a call to `vec_proxy()`. Can be used
/// to ensure that all extraneous attributes and classes have been stripped from
/// a proxy object, which may help avoid inflooping in some cases.
///
/// - For atomics, clears all attributes but `names`.
/// - For arrays, clears all attributes but `dim` and `dimnames`.
/// - For data frames, clears all attributes but `names`, `row.names`, and a `"data.frame"` class.
r_obj* proxy_data(r_obj* x);

#endif
