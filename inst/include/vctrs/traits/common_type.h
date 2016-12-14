#ifndef VCTRS_VCTRS_TRAITS_COMMON_TYPE_H
#define VCTRS_VCTRS_TRAITS_COMMON_TYPE_H

#include <vctrs/detail/get_type.h>
#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>

namespace vctrs {
  namespace traits {

    template <VctrTypes type1, VctrTypes type2>
    struct common_type {
      static VctrTypes get(const typename vctr_class<type1>::type& v1, const typename vctr_class<type2>::type& v2) {
        if (type1 == type2) {
          return type1;
        }
        else if (type1 > type2) {
          return common_type<type2, type1>::get(v2, v1);
        }
        else {
          Rcpp::stop("Cannot find a common type for %d and %d", type1, type2);
        }
      }
    };

    template <VctrTypes type2>
    struct common_type<VCTR_LOGICAL, type2> {
      static const VctrTypes type1 = VCTR_LOGICAL;

      static VctrTypes get(const typename vctr_class<type1>::type& v1, const typename vctr_class<type2>::type& v2) {
        if (v1.all_na()) {
          return type2;
        }

        Rcpp::stop("Can only coerce NA logicals to other data types");
      }
    };

  }
}


#endif // VCTRS_VCTRS_TRAITS_COMMON_TYPE_H
