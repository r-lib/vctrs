.rando <- setClass(
  "vctrs_rando",
  contains = "numeric",
  slots = list(.Data = "numeric")
)
rando <- function(n = 0) {
  .rando(as.numeric(seq_len(n)))
}

as_rando <- function(x) {
  rando(length(x))
}

setMethod("[", "vctrs_rando", function(x, i, j, ..., drop = TRUE) {
  new_n <- length(vec_as_location(i, length(x@.Data), names(x@.Data)))
  rando(new_n)
})

.Counts <- methods::setClass(
  "vctrs_Counts",
  contains = "integer",
  slots = c(name = "character")
)

local_c_counts <- function(frame = caller_env()) {
  c_counts <- function(x, ...) {
    xs <- list(x, ...)

    xs_data <- lapply(xs, function(x) x@.Data)
    new_data <- do.call(c, xs_data)

    .Counts(new_data, name = "Dispatched")
  }

  local_s4_method(
    frame = frame,
    "c",
    methods::signature(x = "vctrs_Counts"),
    c_counts
  )
}


local_s4_method <- function(generic, signature, method, frame = caller_env()) {
  methods::setMethod(generic, signature, method)
  exit_expr <- call2(
    methods::removeMethod,
    generic,
    signature,
    where = topenv(frame)
  )
  local_exit(exit_expr, frame = frame)
}
with_s4_method <- function(generic, signature, method, expr) {
  local_s4_method(generic, signature, method)
  expr
}

local_exit <- function(expr, frame = caller_env()) {
  # We are at top-level when only one frame refers to the global environment
  if (is_reference(frame, global_env())) {
    is_global_frame <- sys.parents() == 0
    if (sum(is_global_frame) == 1) {
      abort("Can't add an exit event at top-level")
    }
  }

  # Inline everything so the call will succeed in any environment
  expr <- call2(on.exit, expr, add = TRUE)
  eval_bare(expr, frame)

  invisible(expr)
}
