#ifndef VCTRS_VCTRS_VCTR_H
#define VCTRS_VCTRS_VCTR_H

#include <vctrs/slicing_index.h>

namespace vctrs {
  class Vctr {
  public:
    virtual ~Vctr() {}

  public:
    virtual Vctr* get(size_t index) = 0;
    virtual Vctr* subset(const SlicingIndex& index) = 0;
    virtual Vctr* coerce_to(const Vctr& other) = 0;
  };
}

#endif //VCTRS_VCTRS_VCTR_H
