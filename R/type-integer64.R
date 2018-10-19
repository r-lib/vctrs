
#' 64 bit integers
#'
#' A `integer64` vector is 64 bits integer vector. Details
#' are implented in the `bit64` package.
#'
#' These functions help the `integer64` class from `bit64` in to
#' the vctrs type system by providing constructors, coercion functions,
#' and casting functions. `new_int64()` is a low-level
#' constructor that only checks the type is valid, so
#' is for expert use only.
#'
#' @param x 64 bit integer vector
#' @param ...,class Used to for subclasses.
#' @keywords internal
#' @export
new_int64 <- function(x = bit64::integer64(), ..., class = character()) {
  stopifnot(inherits(x, "integer64"))
  x
}

# Print -------------------------------------------------------------------

#' @rdname new_int64
#' @export
vec_ptype_full.integer64 <- function(x) {
  "integer64"
}

#' @rdname new_int64
#' @export
vec_ptype_abbr.integer64 <- function(x) {
  "int64"
}


# Coerce ------------------------------------------------------------------

#' @export
#' @rdname new_int64
#' @export vec_type2.integer64
#' @method vec_type2 integer64
vec_type2.integer64 <- function(x, y) {
  UseMethod("vec_type2.integer64", y)
}

#' @method vec_type2.integer64 default
#' @export
vec_type2.integer64.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.integer64 integer64
#' @export
vec_type2.integer64.integer64 <- function(x, y) bit64::integer64()


#' @method vec_type2.integer64 integer
#' @export
vec_type2.integer64.integer <- function(x, y) bit64::integer64()

#' @method vec_type2.integer integer64
#' @export
vec_type2.integer.integer64 <- function(x, y) bit64::integer64()

#' @method vec_type2.integer64 logical
#' @export
vec_type2.integer64.logical <- function(x, y) bit64::integer64()

#' @method vec_type2.logical integer64
#' @export
vec_type2.logical.integer64 <- function(x, y) bit64::integer64()


# Cast --------------------------------------------------------------------

#' @export
#' @rdname new_int64
#' @export vec_cast.integer64
#' @method vec_cast integer64
vec_cast.integer64 <- function(x, to) UseMethod("vec_cast.integer64")

#' @export
#' @method vec_cast.integer64 default
vec_cast.integer64.default <- function(x, to) stop_incompatible_cast(x, to)

#' @export
#' @method vec_cast.integer64 integer64
vec_cast.integer64.integer64 <- function(x, to) x

#' @export
#' @method vec_cast.integer64 integer
vec_cast.integer64.integer <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.integer integer64
vec_cast.integer.integer64 <- function(x, to) {
  as.integer(x)
}

#' @export
#' @method vec_cast.integer64 logical
vec_cast.integer64.logical <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.logical integer64
vec_cast.logical.integer64 <- function(x, to) {
  as.logical(x)
}

#' @export
#' @method vec_cast.integer64 character
vec_cast.integer64.character <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.character integer64
vec_cast.character.integer64 <- function(x, to) {
  as.character(x)
}

#' @export
#' @method vec_cast.integer64 double
vec_cast.integer64.double <- function(x, to) {
  bit64::as.integer64(x)
}

#' @export
#' @method vec_cast.logical integer64
vec_cast.double.integer64 <- function(x, to) {
  as.double(x)
}

# Arithmetic --------------------------------------------------------------

# we need these otherwise `vec_data()`
vec_arith_integer64_other <- function(op, x, y){
  c(x, y) %<-% vec_recycle_common(x, y)

  op_fun <- getExportedValue("base", op)
  op_fun(x, vec_data(y))
}

vec_arith_other_integer64 <- function(op, x, y){
  c(x, y) %<-% vec_recycle_common(x, y)

  op_fun <- getExportedValue("base", op)
  op_fun(vec_data(x), y)
}

vec_arith_integer64_integer64 <- function(op, x, y){
  c(x, y) %<-% vec_recycle_common(x, y)

  op_fun <- getExportedValue("base", op)
  op_fun(x, y)
}

#' @rdname new_int64
#' @export vec_arith.integer64
#' @method vec_arith integer64
#' @export
vec_arith.integer64 <- function(op, x, y) UseMethod("vec_arith.integer64", y)

#' @method vec_arith.integer64 default
#' @export
vec_arith.integer64.default <- function(op, x, y) stop_incompatible_op(op, x, y)

#' @method vec_arith.integer64 integer64
#' @export
vec_arith.integer64.integer64 <- function(op, x, y) vec_arith_integer64_integer64(op, x, y)

#' @method vec_arith.integer64 integer
#' @export
vec_arith.integer64.integer <- function(op, x, y) vec_arith_integer64_other(op, x, y)

#' @method vec_arith.integer integer64
#' @export
vec_arith.integer.integer64 <- function(op, x, y) vec_arith_other_integer64(op, x, y)

#' @method vec_arith.integer64 numeric
#' @export
vec_arith.integer64.numeric <- function(op, x, y) vec_arith_integer64_other(op, x, y)

#' @method vec_arith.numeric integer64
#' @export
vec_arith.numeric.integer64 <- function(op, x, y) vec_arith_other_integer64(op, x, y)

#' @method vec_arith.integer64 logical
#' @export
vec_arith.integer64.logical <- function(op, x, y) vec_arith_integer64_other(op, x, y)

#' @method vec_arith.logical integer64
#' @export
vec_arith.logical.integer64 <- function(op, x, y) vec_arith_other_integer64(op, x, y)

#' @method vec_arith.integer64 MISSING
#' @export
vec_arith.integer64.MISSING <- function(op, x, y) {
  switch(op,
    `-` = -x,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}
