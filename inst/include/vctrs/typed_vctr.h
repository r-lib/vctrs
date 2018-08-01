#ifndef VCTRS_VCTRS_TYPED_VCTR_H
#define VCTRS_VCTRS_TYPED_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/types.h>
#include <vctrs/traits/type.h>

namespace vctrs {

  template <class C>
  class TypedVctr : public Vctr {
  public:
    VctrTypes get_type() const {
      return traits::vctr_type<C>::value;
    }
  };

}

#endif // VCTRS_VCTRS_TYPED_VCTR_H
