#ifndef VCTRS_VCTRS_DETAIL_LOGICAL_VCTR_H
#define VCTRS_VCTRS_DETAIL_LOGICAL_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>
#include "default_vctr.h"

namespace vctrs {
  namespace detail {

    class LogicalVctr;

  }

  namespace detail {

    using namespace Rcpp;

    class LogicalVctr : public TypedVctr<LogicalVctr> {
    public:
      LogicalVctr(SEXP x_) : x(x_) {}

    public:
      virtual size_t length() const {
        return Rf_length(x);
      }

      virtual Vctr* subset(const SlicingIndex& index) const {
        LogicalVector ret(index.size());

        for (size_t i = 0; i < index.size(); ++i) {
          ret[i] = x[index[i]];
        }

        return new LogicalVctr(ret);
      }

      virtual Vctr* combine(const Vctr& other) const {
        const LogicalVctr& my_other = static_cast<const LogicalVctr&>(other);

        LogicalVector ret(x.length() + my_other.x.length());
        for (R_xlen_t i = 0; i < x.length(); ++i) {
          ret[i] = x[i];
        }
        for (R_xlen_t i = 0; i < my_other.x.length(); ++i) {
          ret[x.length() + i] = my_other.x[i];
        }
        return new LogicalVctr(ret);
      }

      virtual Vctr* clone() const {
        return new LogicalVctr(x);
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
      LogicalVector x;
    };

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
