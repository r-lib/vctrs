#ifndef VCTRS_VCTRS_VCTR_BUILDER_H
#define VCTRS_VCTRS_VCTR_BUILDER_H

#include <vctrs/coerce.h>
#include <vctrs/in_place_vctr.h>
#include <vctrs/traits/common_type.h>

namespace vctrs {
  class VctrBuilder {
  public:
    VctrBuilder(const Vctr& vctr_) : vctr(vctr_) {}

  public:
    const Vctr& get_vctr() const {
      return vctr.get_vctr();
    }

    void append(const Vctr& other) {
      const size_t vctr_length = vctr.length();
      const VctrTypes type = CommonType::get(vctr.get_vctr(), other);

      InPlaceVctr new_this(vctr);
      InPlaceVctr new_other(other);

      CoerceTo::coerce_to(new_this, type);
      CoerceTo::coerce_to(new_other, type);
      new_this.combine(new_other);

      vctr.swap(new_this);
    }

  private:
    InPlaceVctr vctr;
  };
}

#endif // VCTRS_VCTRS_VCTR_BUILDER_H
