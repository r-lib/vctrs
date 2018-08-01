#ifndef VCTRS_VCTRS_VCTR_FACTORY_H
#define VCTRS_VCTRS_VCTR_FACTORY_H

#include <vctrs/vctr.h>
#include <vctrs/detail/get_type.h>
#include <vctrs/traits/selector.h>

namespace vctrs {

  using namespace Rcpp;

  class VctrFactory {
    class worker_base {
    public:
      virtual Vctr* create(SEXP x) = 0;
    };

    template <class C>
    class worker : public worker_base {
    public:
      typedef worker_base base_class;

      virtual Vctr* create(SEXP x) {
        return new C(x);
      }
    };

  public:
    static Vctr* create(const RObject& x) {
      VctrTypes type = detail::get_type(x);

      return traits::vctr_type_selector<worker>().select(type).create(x);
    }
  };

}

#endif // VCTRS_VCTRS_VCTR_FACTORY_H
