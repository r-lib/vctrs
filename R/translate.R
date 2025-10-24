vec_normalize_encoding <- function(x) {
  .Call(vctrs_normalize_encoding, x)
}

chr_is_normalized <- function(x) {
  .Call(ffi_chr_is_normalized, x)
}
