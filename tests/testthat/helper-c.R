
class_type <- function(x) {
  .Call(vctrs_class_type, x)
}

s3_class_type <- function(x) {
  .Call(vctrs_s3_class_type, x)
}
