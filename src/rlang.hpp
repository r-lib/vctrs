#define R_NO_REMAP
#include <Rinternals.h>

#define SEXP sexp_unsafe_in_this_file
#define SEXPREC sexp_unsafe_in_this_file


typedef R_xlen_t r_ssize;
#define R_SSIZE_MAX R_XLEN_T_MAX
typedef Rcomplex r_complex_t;

enum r_type {
  r_type_null        = 0,
  r_type_symbol      = 1,
  r_type_pairlist    = 2,
  r_type_closure     = 3,
  r_type_environment = 4,
  r_type_promise     = 5,
  r_type_call        = 6,
  r_type_special     = 7,
  r_type_builtin     = 8,
  r_type_string      = 9,
  r_type_logical     = 10,
  r_type_integer     = 13,
  r_type_double      = 14,
  r_type_complex     = 15,
  r_type_character   = 16,
  r_type_dots        = 17,
  r_type_any         = 18,
  r_type_list        = 19,
  r_type_expression  = 20,
  r_type_bytecode    = 21,
  r_type_pointer     = 22,
  r_type_weakref     = 23,
  r_type_raw         = 24,
  r_type_s4          = 25,

  r_type_new         = 30,
  r_type_free        = 31,

  r_type_function    = 99
};


namespace rlang {

/* Only supports types that allow memory access to the underlying C array */
template <enum r_type>
struct traits;

template <>
struct traits<r_type_logical> {
  typedef int array_type;
  const array_type na_value = NA_LOGICAL;
};

template <>
struct traits<r_type_integer> {
  typedef int array_type;
  const array_type na_value = NA_INTEGER;
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


#define DEFAULT_CTYPE(R_TYPE) typename rlang::traits<R_TYPE>::array_type
#define NA_VALUE(R_TYPE) rlang::traits<R_TYPE>::na_value
