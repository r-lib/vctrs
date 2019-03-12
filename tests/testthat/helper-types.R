
empty_types <- list(
  null = NULL,
  logical = lgl(),
  integer = int(),
  double = dbl(),
  complex = cpl(),
  character = chr(),
  raw = bytes(),
  list = list(),
  dataframe = tibble::tibble(),
  s3 = foobar(),
  scalar = ~foobar
)
