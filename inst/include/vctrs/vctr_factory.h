#ifndef VCTRS_VCTRS_VCTR_FACTORY_H
#define VCTRS_VCTRS_VCTR_FACTORY_H

#include <vctrs/vctr.h>
#include <vctrs/detail/default_vctr.h>

namespace vctrs {

  class VctrFactory {
  public:
    static Vctr* create(SEXP x) {
      return new detail::DefaultVctr(x);
    }
  };

}

#endif // VCTRS_VCTRS_VCTR_FACTORY_H
