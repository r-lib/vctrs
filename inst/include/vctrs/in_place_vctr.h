#ifndef VCTRS_VCTRS_IN_PLACE_VCTR_H
#define VCTRS_VCTRS_IN_PLACE_VCTR_H

#include <memory>

#include <vctrs/vctr.h>

namespace vctrs {
  class InPlaceVctr {
  public:

  public:
    const Vctr& get_vctr() const {
      check_vctr();

      return *vctr;
    }

    size_t length() const {
      check_vctr();

      return vctr->length();
    }

    void subset(const SlicingIndex& index) {
      check_vctr();

      Vctr* subset_vctr = vctr->subset(index);
      if (subset_vctr) {
        vctr.reset(subset_vctr);
      }
    }

    void coerce_to(const Vctr& other, size_t new_size) {
      check_vctr();

      Vctr* coerce_vctr = vctr->coerce_to(other, new_size);
      if (coerce_vctr) {
        vctr.reset(coerce_vctr);
      }
    }

    virtual void copy(const Vctr& other, const SlicingIndex& index) {
      vctr->copy(other, index);
    }

  private:
    void check_vctr() const {
      if (!vctr.get())
        Rcpp::stop("InPlaceVctr not initialized");
    }

  private:
    std::auto_ptr<Vctr> vctr;
  };
}

#endif // VCTRS_VCTRS_IN_PLACE_VCTR_H
