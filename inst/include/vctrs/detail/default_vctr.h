#ifndef VCTRS_VCTRS_DETAIL_DEFAULT_VCTR_H
#define VCTRS_VCTRS_DETAIL_DEFAULT_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>
#include <vctrs/traits/type.h>
#include <vctrs/typed_vctr.h>

namespace vctrs {
  namespace detail {

    class DefaultVctr;

  }

  namespace traits {

    template <>
    struct vctr_class<VCTR_DEFAULT> {
      typedef detail::DefaultVctr type;
    };


    template <>
    struct vctr_type<detail::DefaultVctr> {
      static const VctrTypes type = VCTR_DEFAULT;
    };
  }

  namespace detail {

    using namespace Rcpp;

    class DefaultVctr : public TypedVctr<DefaultVctr> {
    public:
      DefaultVctr(SEXP x_) : x(x_) {}

    public:
      virtual size_t length() const {
        return Rf_length(x);
      }

      virtual Vctr* subset(const SlicingIndex& index) const {
        return new DefaultVctr(Function("[", R_BaseEnv)(x, compute_positions(index)));
      }

      virtual Vctr* coerce_to(const Vctr& other, size_t new_size) const {
        if (other.get_type() != VCTR_DEFAULT) {
          stop("Cannot coerce default vctr to anything other than a default vctr");
        }

        return NULL;
      }

      virtual Vctr* copy(const Vctr& other, const SlicingIndex& index) {
        const DefaultVctr& my_other = static_cast<const DefaultVctr&>(other);

        if (index.is_tight(other.length(), length())) {
          return new DefaultVctr(Function("c", R_BaseEnv)(x, my_other.x));
        }

        stop("DefaultVctr::copy() not yet implemented for the general case");
      }

      virtual Vctr* clone() const {
        return new DefaultVctr(x);
      }

      virtual SEXP get_sexp() const {
        return x;
      }

    private:
      static IntegerVector compute_positions(const SlicingIndex& index) {
        IntegerVector ret(index.size());

        for (size_t i = 0; i < index.size(); ++i) {
          ret[i] = index[i];
        }

        return ret;
      }

    private:
      RObject x;

    };

  }

}

#endif // VCTRS_VCTRS_DETAIL_DEFAULT_VCTR_H
