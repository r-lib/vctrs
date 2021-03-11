#' Find the prototype of a set of vectors
#'
#' `vec_ptype()` returns the unfinalised prototype of a single vector.
#' `vec_ptype_common()` finds the common type of multiple vectors.
#' `vec_ptype_show()` nicely prints the common type of any number of
#' inputs, and is designed for interactive exploration.
#'
#' @param x A vector
#' @param ... For `vec_ptype()`, these dots are for future extensions and must
#'   be empty.
#'
#'   For `vec_ptype_common()` and `vec_ptype_show()`, vector inputs.
#' @param x_arg Argument name for `x`. This is used in error messages to inform
#'   the user about the locations of incompatible types.
#' @param .ptype If `NULL`, the default, the output type is determined by
#'   computing the common type across all elements of `...`.
#'
#'   Alternatively, you can supply `.ptype` to give the output known type.
#'   If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this value:
#'   this is a convenient way to make production code demand fixed types.
#' @return `vec_ptype()` and `vec_ptype_common()` return a prototype
#'   (a size-0 vector)
#'
#' @section `vec_ptype()`:
#' `vec_ptype()` returns [size][vec_size] 0 vectors potentially
#' containing attributes but no data. Generally, this is just
#' `vec_slice(x, 0L)`, but some inputs require special
#' handling.
#'
#' * While you can't slice `NULL`, the prototype of `NULL` is
#'   itself. This is because we treat `NULL` as an identity value in
#'   the `vec_ptype2()` monoid.
#'
#' * The prototype of logical vectors that only contain missing values
#'   is the special [unspecified] type, which can be coerced to any
#'   other 1d type. This allows bare `NA`s to represent missing values
#'   for any 1d vector type.
#'
#' See [internal-faq-ptype2-identity] for more information about
#' identity values.
#'
#' `vec_ptype()` is a _performance_ generic. It is not necessary to implement it
#' because the default method will work for any vctrs type. However the default
#' method builds around other vctrs primitives like `vec_slice()` which incurs
#' performance costs. If your class has a static prototype, you might consider
#' implementing a custom `vec_ptype()` method that returns a constant. This will
#' improve the performance of your class in many cases ([common
#' type][vec_ptype2] imputation in particular).
#'
#' Because it may contain unspecified vectors, the prototype returned
#' by `vec_ptype()` is said to be __unfinalised__. Call
#' [vec_ptype_finalise()] to finalise it. Commonly you will need the
#' finalised prototype as returned by `vec_slice(x, 0L)`.
#'
#' @section `vec_ptype_common()`:
#' `vec_ptype_common()` first finds the prototype of each input, then
#' successively calls [vec_ptype2()] to find a common type. It returns
#' a [finalised][vec_ptype_finalise] prototype.
#'
#' @section Dependencies of `vec_ptype()`:
#' - [vec_slice()] for returning an empty slice
#'
#' @section Dependencies of `vec_ptype_common()`:
#' - [vec_ptype2()]
#' - [vec_ptype_finalise()]
#'
#' @export
#' @examples
#' # Unknown types ------------------------------------------
#' vec_ptype_show()
#' vec_ptype_show(NA)
#' vec_ptype_show(NULL)
#'
#' # Vectors ------------------------------------------------
#' vec_ptype_show(1:10)
#' vec_ptype_show(letters)
#' vec_ptype_show(TRUE)
#'
#' vec_ptype_show(Sys.Date())
#' vec_ptype_show(Sys.time())
#' vec_ptype_show(factor("a"))
#' vec_ptype_show(ordered("a"))
#'
#' # Matrices -----------------------------------------------
#' # The prototype of a matrix includes the number of columns
#' vec_ptype_show(array(1, dim = c(1, 2)))
#' vec_ptype_show(array("x", dim = c(1, 2)))
#'
#' # Data frames --------------------------------------------
#' # The prototype of a data frame includes the prototype of
#' # every column
#' vec_ptype_show(iris)
#'
#' # The prototype of multiple data frames includes the prototype
#' # of every column that in any data frame
#' vec_ptype_show(
#'   data.frame(x = TRUE),
#'   data.frame(y = 2),
#'   data.frame(z = "a")
#' )
vec_ptype <- function(x, ..., x_arg = "") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_ptype, x, x_arg))
  UseMethod("vec_ptype")
}

#' @export
#' @rdname vec_ptype
vec_ptype_common <- function(..., .ptype = NULL) {
  .External2(vctrs_type_common, .ptype)
}
vec_ptype_common_opts <- function(...,
                                  .ptype = NULL,
                                  .opts = fallback_opts()) {
  .External2(vctrs_ptype_common_opts, .ptype, .opts)
}
vec_ptype_common_params <- function(...,
                                    .ptype = NULL,
                                    .df_fallback = NULL,
                                    .s3_fallback = NULL) {
  opts <- fallback_opts(
    df_fallback = .df_fallback,
    s3_fallback = .s3_fallback
  )
  vec_ptype_common_opts(..., .ptype = .ptype, .opts = opts)
}
vec_ptype_common_fallback <- function(..., .ptype = NULL) {
  vec_ptype_common_opts(..., .ptype = .ptype, .opts = full_fallback_opts())
}

#' @export
#' @rdname vec_ptype
vec_ptype_show <- function(...) {
  args <- compact(list2(...))
  n <- length(args)
  if (n == 0) {
    cat_line("Prototype: NULL")
  } else if (n == 1) {
    cat_line("Prototype: ", vec_ptype_full(args[[1]]))
  } else {
    in_types <- map(args, vec_ptype)
    out_types <- vector("list", length(in_types))
    out_types[[1]] <- in_types[[1]]
    for (i in seq2(2, n)) {
      out_types[[i]] <- vec_ptype2(out_types[[i - 1]], in_types[[i]])
    }

    in_full <- paste0("<", map_chr(in_types, vec_ptype_full), ">")
    out_full <- paste0("<", map_chr(out_types, vec_ptype_full), ">")

    out <- cbind(
      n = paste0(seq(0, n - 1), ". "),
      lhs = c("", out_full[-n]),
      comma = " , ",
      rhs = in_full,
      equals = " = ",
      res = c(in_full[[1]], out_full[-1])
    )
    out <- t(apply(out, 1, pad_height))
    out <- apply(out, 2, pad_width)

    out[, "lhs"] <- parens(out[, "lhs"])
    out[, "rhs"] <- parens(out[, "rhs"], FALSE)

    lines <- strsplit(out, "\n")
    dim(lines) <- dim(out)

    steps <- apply(lines, 1, function(x) do.call(cbind, x))
    if (is.list(steps)) {
      step_lines <- unlist(lapply(steps, function(x) apply(x, 1, paste0, collapse = "")))
    } else {
      step_lines <- apply(steps, 2, paste0, collapse = "")
    }

    cat_line("Prototype: ", out_full[[n]])
    cat_line(step_lines)
  }

  invisible()
}

vec_typeof <- function(x) {
  .Call(vctrs_typeof, x, TRUE)
}
vec_typeof_bare <- function(x) {
  .Call(vctrs_typeof, x, FALSE)
}

vec_type_info <- function(x) {
  .Call(vctrs_type_info, x)
}
vec_proxy_info <- function(x) {
  .Call(vctrs_proxy_info, x)
}
