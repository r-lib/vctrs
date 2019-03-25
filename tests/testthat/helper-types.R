
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

atomics <- list(TRUE, 1L, 1.0, 1i, "foo", bytes(1))
vectors <- c(atomics, list(list()))
records <- list(
  df = data.frame(x = 1),
  rcrd = new_rcrd(list(x = 1)),
  posixlt = as.POSIXlt("2020-01-01")
)
