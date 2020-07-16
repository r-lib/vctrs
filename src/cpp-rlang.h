#ifndef CPP_RLANG_H
#define CPP_RLANG_H


#define R_NO_REMAP
#include <Rinternals.h>

#include "rlang-types.h"


// Prevent some unsafe usages
#define SEXP sexp_unsafe_in_this_file
#define SEXPREC sexp_unsafe_in_this_file
#define Rf_allocVector alloc_unsafe_in_this_file


namespace rlang {

/* Only supports types that allow memory access to the underlying C array */
template <enum r_type>
struct traits;

template <>
struct traits<r_type_logical> {
  typedef int array_type;
};

template <>
struct traits<r_type_integer> {
  typedef int array_type;
};

template <>
struct traits<r_type_double> {
  typedef double array_type;
};

template <>
struct traits<r_type_complex> {
  typedef r_complex_t array_type;
};

template <>
struct traits<r_type_raw> {
  typedef unsigned char* array_type;
};

} // namespace rlang


#define C_TYPE(R_TYPE) rlang::traits<R_TYPE>::array_type
#define DEFAULT_C_TYPE(R_TYPE) typename C_TYPE(R_TYPE)


template <enum r_type R_TYPE>
typename C_TYPE(R_TYPE) na_value();

template <> int na_value<r_type_logical>() { return NA_LOGICAL; }
template <> int na_value<r_type_integer>() { return NA_INTEGER; }


#endif
