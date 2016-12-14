#ifndef VCTRS_VCTRS_DETAIL_LOGICAL_VCTR_H
#define VCTRS_VCTRS_DETAIL_LOGICAL_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>
#include "default_vctr.h"

namespace vctrs {
  namespace detail {

    class LogicalVctr;

  }

  namespace traits {

    template <>
    struct vctr_class<VCTR_LOGICAL> {
      typedef detail::LogicalVctr type;
    };

    template <>
    struct vctr_type<detail::LogicalVctr> {
      static const VctrTypes type = VCTR_LOGICAL;
    };

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

      virtual Vctr* coerce_to(const Vctr& other, size_t new_size) const {
        if (other.get_type() == VCTR_LOGICAL) {
          LogicalVector ret(x.length() + other.length());
          for (int i = 0; i < x.length(); ++i) {
            ret[i] = x[i];
          }
          return new LogicalVctr(ret);
        }
        else {
          return new DefaultVctr(x);
        }
      }

      virtual Vctr* copy(const Vctr& other, const SlicingIndex& index) {
        const LogicalVctr& my_other = static_cast<const LogicalVctr&>(other);

        for (size_t i = 0; i < index.size(); ++i) {
          x[index[i]] = my_other.x[i];
        }

        return NULL;
    }

      virtual Vctr* clone() const {
        return new LogicalVctr(x);
      }

      virtual SEXP get_sexp() const {
        return x;
      }

    private:
      LogicalVector x;

    };

  }

}

#endif // VCTRS_VCTRS_DETAIL_LOGICAL_VCTR_H
