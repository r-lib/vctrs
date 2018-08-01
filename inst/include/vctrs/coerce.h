#ifndef VCTRS_VCTRS_COERCE_H
#define VCTRS_VCTRS_COERCE_H

#include <vctrs/vctr.h>
#include <vctrs/traits/class.h>
#include <vctrs/traits/selector.h>
#include <vctrs/traits/common_type.h>
#include <vctrs/traits/coerce_to.h>

namespace vctrs {

  class CommonType {
    class worker_base {
    public:
      virtual VctrTypes lookup(const Vctr& v1, const Vctr& v2) = 0;
    };

    template<class C1, class C2>
    class worker : public worker_base {
    public:
      typedef worker_base base_class;

      virtual VctrTypes lookup(const Vctr& v1, const Vctr& v2) {
        const C1& v1_true = static_cast<const C1&>(v1);
        const C2& v2_true = static_cast<const C2&>(v2);
        return traits::common_type<traits::vctr_type<C1>::value, traits::vctr_type<C2>::value>::get(v1_true, v2_true);
      }
    };

  public:
    static VctrTypes get(const Vctr& v1, const Vctr& v2) {
      return traits::vctr_type_selector2<worker>().select(v1, v2).lookup(v1, v2);
    }
  };

  class CoerceTo {
    class worker_base {
    public:
      virtual Vctr* coerce(const Vctr& v) = 0;
    };

    template<class C1, class C2>
    class worker : public worker_base {
    public:
      typedef worker_base base_class;

      virtual Vctr* coerce(const Vctr& v) {
        const C1& v_true = static_cast<const C1&>(v);
        return traits::coerce_to<traits::vctr_type<C1>::value, traits::vctr_type<C2>::value>::perform(v_true);
      }
    };

  public:
    static void coerce_to(InPlaceVctr& vctr, const VctrTypes type) {
      Vctr* vctr_new = traits::vctr_type_selector2<worker>().select(vctr.get_vctr().get_type(), type).coerce(vctr.get_vctr());
      if (vctr_new) {
        vctr.reset(vctr_new);
      }
    }
  };

}

#endif // VCTRS_VCTRS_COERCE_H
