#ifndef VCTRS_VCTRS_TYPED_VCTR_H
#define VCTRS_VCTRS_TYPED_VCTR_H

#include <vctrs/vctr.h>
#include <vctrs/types.h>

namespace vctrs {

  template <class C>
  class TypedVctr : public Vctr {
  public:
    VctrTypes get_type() const {
      return traits::vctr_type<C>::type;
    }
  };

}

#endif // VCTRS_VCTRS_TYPED_VCTR_H
