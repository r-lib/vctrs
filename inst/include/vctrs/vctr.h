#ifndef VCTRS_VCTRS_VCTR_H
#define VCTRS_VCTRS_VCTR_H

#include <vctrs/slicing_index.h>

namespace vctrs {
  class Vctr {
  public:
    virtual ~Vctr() {}

  public:
    virtual size_t length() const = 0;
    virtual Vctr* subset(const SlicingIndex& index) const = 0;
    virtual Vctr* coerce_to(const Vctr& other, size_t new_size) const = 0;
    virtual void copy(const Vctr& other, const SlicingIndex& index) = 0;
    virtual Vctr* clone() const;
  };
}

#endif //VCTRS_VCTRS_VCTR_H
