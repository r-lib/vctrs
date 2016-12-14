#ifndef VCTRS_VCTRS_VCTR_BUILDER_H
#define VCTRS_VCTRS_VCTR_BUILDER_H

#include <vctrs/in_place_vctr.h>

namespace vctrs {
  class VctrBuilder {
  public:
    const Vctr& get_vctr() const {
      return vctr.get_vctr();
    }

    void append(const Vctr& other) {
      const size_t& vctr_length = vctr.length();
      vctr.coerce_to(other, vctr_length + other.length());
      vctr.copy(other, OffsetSlicingIndex(vctr_length, other.length()));
    }

  private:
    InPlaceVctr vctr;
  };
}

#endif // VCTRS_VCTRS_VCTR_BUILDER_H
