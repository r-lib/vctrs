#ifndef VCTRS_VCTRS_TRAITS_COERCE_TO_H
#define VCTRS_VCTRS_TRAITS_COERCE_TO_H

#include <vctrs/detail/get_type.h>
#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>

namespace vctrs {
  namespace traits {

    template <VctrTypes type1, VctrTypes type2>
    struct coerce_to {
      static Vctr* perform(const typename vctr_class<type1>::type& v1) {
        if (type1 == type2) {
          return NULL;
        }

        Rcpp::stop("Can't coerce %d to %d", type1, type2);
      }
    };

    template <VctrTypes type2>
    struct coerce_to<VCTR_LOGICAL, type2> {
      static const VctrTypes type1 = VCTR_LOGICAL;

      static Vctr* perform(const typename vctr_class<type1>::type& v1) {
        if (type1 == type2) {
          return NULL;
        }

        if (!v1.all_na()) {
          Rcpp::stop("Cannot coerce logical to integer if not NA");
        }

        return new typename vctr_class<type2>::type(v1.get_sexp());
      }
    };

    template <>
    struct coerce_to<VCTR_INTEGER, VCTR_REAL> {
      static const VctrTypes type1 = VCTR_INTEGER;
      static const VctrTypes type2 = VCTR_REAL;

      static Vctr* perform(const typename vctr_class<type1>::type& v1) {
        return new typename vctr_class<type2>::type(v1.get_sexp());
      }
    };

    template <VctrTypes type1>
    struct coerce_to<type1, VCTR_DEFAULT> {
      static const VctrTypes type2 = VCTR_DEFAULT;

      static Vctr* perform(const typename vctr_class<type1>::type& v1) {
        if (type1 == type2) {
          return NULL;
        }

        return new typename vctr_class<type2>::type(v1.get_sexp());
      }
    };

    template <>
    struct coerce_to<VCTR_LOGICAL, VCTR_DEFAULT> {
      static const VctrTypes type1 = VCTR_LOGICAL;
      static const VctrTypes type2 = VCTR_DEFAULT;

      static Vctr* perform(const typename vctr_class<type1>::type& v1) {
        if (type1 == type2) {
          return NULL;
        }

        return new typename vctr_class<type2>::type(v1.get_sexp());
      }
    };

  }
}


#endif // VCTRS_VCTRS_TRAITS_COERCE_TO_H
