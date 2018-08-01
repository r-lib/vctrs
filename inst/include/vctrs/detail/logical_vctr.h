#ifndef VCTRS_VCTRS_DETAIL_LOGICAL_VCTR_H
#define VCTRS_VCTRS_DETAIL_LOGICAL_VCTR_H

#include <vctrs/detail/atomic_vctr.h>
#include <vctrs/traits/class.h>
#include <vctrs/traits/type.h>

namespace vctrs {
  namespace detail {

    using namespace Rcpp;

    typedef AtomicVctr<LogicalVector> LogicalVctr;

  }

  namespace traits {

    template <>
    struct vctr_class<VCTR_LOGICAL> {
      typedef detail::LogicalVctr type;
    };

    template <>
    struct vctr_type<detail::LogicalVctr> {
      static const VctrTypes value = VCTR_LOGICAL;
    };

  }

}

#endif // VCTRS_VCTRS_DETAIL_LOGICAL_VCTR_H
