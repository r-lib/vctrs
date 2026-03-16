obj_encode_utf8 <- function(x) {
  .Call(ffi_obj_encode_utf8, x)
}

chr_is_ascii_or_utf8 <- function(x) {
  .Call(ffi_chr_is_ascii_or_utf8, x)
}
