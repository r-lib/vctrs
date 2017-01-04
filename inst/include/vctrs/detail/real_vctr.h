#ifndef VCTRS_VCTRS_DETAIL_REAL_VCTR_H
#define VCTRS_VCTRS_DETAIL_REAL_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>
#include "default_vctr.h"

namespace vctrs {
  namespace detail {

    using namespace Rcpp;

    typedef AtomicVctr<DoubleVector> RealVctr;

  }

  namespace traits {

    template <>
    struct vctr_class<VCTR_REAL> {
      typedef detail::RealVctr type;
    };

    template <>
    struct vctr_type<detail::RealVctr> {
      static const VctrTypes value = VCTR_REAL;
    };

  }

}

#endif // VCTRS_VCTRS_DETAIL_REAL_VCTR_H
