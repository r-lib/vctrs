#ifndef VCTRS_VCTRS_TRAITS_SELECTOR_H
#define VCTRS_VCTRS_TRAITS_SELECTOR_H

#include <vctrs/types.h>
#include <vctrs/traits/class.h>

namespace vctrs {
  namespace traits {

    template <template <class C> class X, class Base = typename X<void>::base_class, VctrTypes type = VCTR_DEFAULT>
    struct vctr_type_selector : public vctr_type_selector<X, Base, (VctrTypes)(type - 1)> {
      Base& select(VctrTypes type_)  {
        if (type_ != type) {
          return static_cast<vctr_type_selector<X, Base, (VctrTypes)(type - 1)>*>(this)->select(type_);
        }

        static X<typename traits::vctr_class<type>::type> x;
        return x;
      }
    };

    template <template <class C> class X, class Base>
    struct vctr_type_selector<X, Base, VCTR_NONE> {
      Base& select(VctrTypes type_)  {
        Rcpp::stop("Unknown type: ", type_);
      }
    };

  }
}

#endif // VCTRS_VCTRS_TRAITS_SELECTOR_H
