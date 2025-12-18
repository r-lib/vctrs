#' Unstructure a vector
#'
#' @description
#' `vec_unstructure()` takes a vector that meets the [native storage
#' requirements][theory_faq_native_storage] of vctrs and removes all extraneous
#' attributes, retaining only those that are natively supported by vctrs. Only
#' the following attributes are retained:
#'
#' - For atomic vectors, `names`
#' - For arrays, `dim` and `dimnames`
#' - For data frames, `names`, `row.names`, and a `class` of `"data.frame"`
#'
#' @details
#' Removing extraneous attributes is useful for avoiding unexpected side
#' effects, for example:
#'
#' - [vec_proxy()] calls `vec_unstructure()` on the proxy before returning it.
#'   This ensures that internal manipulation of the proxy avoids any unexpected
#'   S3 dispatch. Additionally, it means that the a future call to
#'   [vec_restore()] receives a minimal `x` object to build upon.
#'
#' - When implementing S3 methods for generics like `+` or `is.finite()`, it is
#'   often useful to `vec_unstructure()` your custom object to remove its class,
#'   call the generic again on the native type to use base R's native
#'   implementation, and then optionally regenerate your custom type with a
#'   `new_<my_type>()` constructor (which you would likely do for `+`, but would
#'   not do for `is.finite()`, which just returns a logical vector).
#'
#' `vec_unstructure()` is roughly the inverse of [base::structure()].
#'
#' @param x An object that meets vctrs's [native storage
#'   requirements][theory_faq_native_storage].
#'
#' @export
#' @examples
#' # Atomic vectors without attributes are returned unmodified
#' vec_unstructure(1)
#'
#' # Atomic vectors with attributes are unstructured back to their natively
#' # supported form, only `names` are retained here:
#' x <- structure(1, names = "a", foo = "bar", class = "myclass")
#' x
#' vec_unstructure(x)
#'
#' # Arrays retain `dim` and `dimnames` but all other attributes are lost
#' x <- array(1:4, c(2, 2))
#' rownames(x) <- c("a", "b")
#' attr(x, "foo") <- "bar"
#' x
#' vec_unstructure(x)
#'
#' # Data frames count as a native storage type in vctrs, so bare data frames
#' # are returned unmodified
#' x <- data_frame(x = 1:5, y = 6:10)
#' vec_unstructure(x)
#'
#' if (require("tibble")) {
#'   # Tibbles meet the native storage requirement, but have extraneous
#'   # attributes that are stripped away
#'   x <- tibble(x = 1:5, y = 6:10)
#'   x
#'   vec_unstructure(x)
#' }
#'
#' # Note that native storage types are orthogonal to proxies.
#' # Calling `vec_unstructure()` on a rcrd returns the underlying list storage,
#' # while the proxy of this type (meant for C manipulation) is a data frame.
#' x <- new_rcrd(list(a = 1:5, b = 6:10))
#' vec_unstructure(x)
#' vec_proxy(x)
#'
#' # Types that don't meet the native storage requirements result in an error
#' try(vec_unstructure(NULL))
#' try(vec_unstructure(environment()))
vec_unstructure <- function(x) {
  .Call(ffi_vec_unstructure, x)
}

# Thrown from C
stop_unsupported_storage_type <- function(x) {
  # It currently doesn't feel worth it to add `x_arg` and `error_call` arguments
  # to `vec_unstructure()`. This is a very low level function and the error is
  # likely only going to be seen by developers implementing their packages.
  x_arg <- glue::backtick("x")
  error_call <- call("vec_unstructure")

  message <- c(
    cli::format_inline(
      "{x_arg} must have a supported storage type, not <{typeof(x)}>."
    ),
    i = cli::format_inline(paste(
      "Read our FAQ about {.topic [native storage types](vctrs::theory_faq_native_storage)}",
      "to learn more."
    ))
  )

  stop_vctrs(
    message,
    "vctrs_error_unsupported_storage_type",
    call = error_call
  )
}
