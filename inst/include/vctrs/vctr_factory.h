#ifndef VCTRS_VCTRS_VCTR_FACTORY_H
#define VCTRS_VCTRS_VCTR_FACTORY_H

#include <vctrs/vctr.h>
#include <vctrs/detail/logical_vctr.h>
#include <vctrs/detail/default_vctr.h>

namespace vctrs {

  using namespace Rcpp;

  class VctrFactory {
  public:
    static Vctr* create(const RObject& x) {
      RObject class_ = x.attr("class");

      if (class_.isNULL()) {
        switch (x.sexp_type()) {
        case LGLSXP:
          return new detail::LogicalVctr(x);
        }
      }

      return new detail::DefaultVctr(x);
    }
  };

}

#endif // VCTRS_VCTRS_VCTR_FACTORY_H
