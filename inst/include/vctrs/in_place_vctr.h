#ifndef VCTRS_VCTRS_IN_PLACE_VCTR_H
#define VCTRS_VCTRS_IN_PLACE_VCTR_H

#include <memory>

#include <vctrs/vctr.h>

namespace vctrs {
  class InPlaceVctr {
  public:
    InPlaceVctr(const Vctr& vctr_) : vctr(vctr_.clone()) {}
    InPlaceVctr(const InPlaceVctr& vctr_) : vctr(vctr_.get_vctr().clone()) {}

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

    void combine(const InPlaceVctr& other) {
      check_vctr();

      Vctr* combine_vctr = vctr->combine(other.get_vctr());
      if (combine_vctr) {
        vctr.reset(combine_vctr);
      }
    }

    void swap(InPlaceVctr& vctr_) {
      std::swap(vctr, vctr_.vctr);
    }

    void reset(Vctr* vctr_) {
      vctr.reset(vctr_);
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
