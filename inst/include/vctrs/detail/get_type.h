#ifndef VCTRS_VCTRS_DETAIL_GET_TYPE_H
#define VCTRS_VCTRS_DETAIL_GET_TYPE_H


#include <vctrs/types.h>
#include <vctrs/detail/logical_vctr.h>
#include <vctrs/detail/default_vctr.h>

namespace vctrs {
  namespace detail {

    inline VctrTypes get_type(const RObject& x) {
      RObject class_ = x.attr("class");

      if (class_.isNULL()) {
        switch (x.sexp_type()) {
        case LGLSXP:
          return VCTR_LOGICAL;
        }
      }

      return VCTR_DEFAULT;
    }

  }
}

#endif // VCTRS_VCTRS_DETAIL_GET_TYPE_H
