#ifndef VCTRS_VCTRS_DETAIL_INTEGER_VCTR_H
#define VCTRS_VCTRS_DETAIL_INTEGER_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>
#include "default_vctr.h"

namespace vctrs {
  namespace detail {

    class IntegerVctr;

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

  namespace detail {

    using namespace Rcpp;

    class IntegerVctr : public TypedVctr<IntegerVctr> {
    public:
      IntegerVctr(SEXP x_) : x(x_) {}

    public:
      virtual size_t length() const {
        return Rf_length(x);
      }

      virtual Vctr* subset(const SlicingIndex& index) const {
        LogicalVector ret(index.size());

        for (size_t i = 0; i < index.size(); ++i) {
          ret[i] = x[index[i]];
        }

        return new IntegerVctr(ret);
      }

      virtual Vctr* combine(const Vctr& other) const {
        const IntegerVctr& my_other = static_cast<const IntegerVctr&>(other);

        LogicalVector ret(x.length() + my_other.x.length());
        for (R_xlen_t i = 0; i < x.length(); ++i) {
          ret[i] = x[i];
        }
        for (R_xlen_t i = 0; i < my_other.x.length(); ++i) {
          ret[x.length() + i] = my_other.x[i];
        }
        return new IntegerVctr(ret);
      }

      virtual Vctr* clone() const {
        return new IntegerVctr(x);
      }

      virtual SEXP get_sexp() const {
        return x;
      }

    public:
      bool all_na() const {
        for (R_xlen_t i = 0; i < x.length(); ++i) {
          if (!x.is_na(x[i]))
            return false;
        }
        return true;
      }

    private:
      IntegerVector x;
    };

  }

}

#endif // VCTRS_VCTRS_DETAIL_INTEGER_VCTR_H
