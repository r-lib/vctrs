#include <Rcpp.h>
#include <vctrs.h>


using namespace vctrs;

// [[Rcpp::export]]
SEXP combine(SEXP x1, SEXP x2) {
  std::auto_ptr<Vctr> v1(VctrFactory::create(x1));
  VctrBuilder vb(*v1);
  std::auto_ptr<Vctr> v2(VctrFactory::create(x2));
  vb.append(*v2);

  return vb.get_vctr().get_sexp();
}
