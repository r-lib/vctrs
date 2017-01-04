#ifndef VCTRS_VCTRS_DETAIL_INTEGER_VCTR_H
#define VCTRS_VCTRS_DETAIL_INTEGER_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>
#include "default_vctr.h"

namespace vctrs {
  namespace detail {

    using namespace Rcpp;

    typedef AtomicVctr<IntegerVector> IntegerVctr;

  }

  namespace traits {

    template <>
    struct vctr_class<VCTR_INTEGER> {
      typedef detail::IntegerVctr type;
    };

    template <>
    struct vctr_type<detail::IntegerVctr> {
      static const VctrTypes value = VCTR_INTEGER;
    };

  }

}

#endif // VCTRS_VCTRS_DETAIL_INTEGER_VCTR_H
