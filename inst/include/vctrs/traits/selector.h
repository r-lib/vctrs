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

    template <template <class C1, class C2> class X, class Base = typename X<void, void>::base_class, VctrTypes type1 = VCTR_DEFAULT, VctrTypes type2 = VCTR_DEFAULT>
    struct vctr_type_selector2 : public vctr_type_selector2<X, Base, type1, (VctrTypes)(type2 - 1)> {
      template <class C1>
      class XX : public X<C1, typename traits::vctr_class<type2>::type> {};

      Base& select(VctrTypes type1_, VctrTypes type2_)  {
        if (type2_ != type2) {
          return static_cast<vctr_type_selector2<X, Base, type1, (VctrTypes)(type2 - 1)>*>(this)->select(type1_, type2_);
        }

        return vctr_type_selector<XX, Base, type2>().select(type1_);
      }
    };

    template <template <class C1, class C2> class X, class Base, VctrTypes type1>
    struct vctr_type_selector2<X, Base, type1, VCTR_NONE> {
      Base& select(VctrTypes type1_, VctrTypes type2_) {
        Rcpp::stop("Unknown type: ", type2_);
      }
    };

  }
}

#endif // VCTRS_VCTRS_TRAITS_SELECTOR_H
