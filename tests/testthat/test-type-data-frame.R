
# printing ----------------------------------------------------------------

test_that("data frames print nicely", {
  expect_equal(vec_ptype_abbr(mtcars), "df[,11]")
  expect_snapshot(vec_ptype_show(mtcars))
  expect_snapshot(vec_ptype_show(iris))
})

test_that("embedded data frames print nicely", {
  df <- data.frame(x = 1:3)
  df$a <- data.frame(a = 1:3, b = letters[1:3])
  df$b <- list_of(1, 2, 3)
  df$c <- as_list_of(split(data.frame(x = 1:3, y = letters[1:3]), 1:3))

  expect_snapshot(vec_ptype_show(df))
})

# coercing ----------------------------------------------------------------

test_that("data frame only combines with other data frames or NULL", {
  dt <- data.frame(x = 1)
  expect_equal(vec_ptype_common(dt, NULL), vec_ptype(dt))
  expect_error(vec_ptype_common(dt, 1:10), class = "vctrs_error_incompatible_type")
})

test_that("data frame takes max of individual variables", {
  dt1 <- data.frame(x = FALSE, y = 1L)
  dt2 <- data.frame(x = 1.5, y = 1.5)

  expect_equal(vec_ptype_common(dt1, dt2), vec_ptype_common(dt2))
})

test_that("data frame combines variables", {
  dt1 <- data.frame(x = 1)
  dt2 <- data.frame(y = 1)

  dt3 <- max(dt1, dt2)
  expect_equal(
    vec_ptype_common(dt1, dt2),
    vec_ptype_common(data.frame(x = double(), y = double()))
  )
})

test_that("empty data frame still has names", {
  df <- data.frame()
  out <- vec_ptype_common(df, df)

  expect_equal(names(out), character())
})

test_that("combining data frames with foreign classes uses fallback", {
  foo <- foobar(data.frame())
  df <- data.frame()

  # Same type fallback
  expect_identical(vec_ptype_common(foo, foo, foo), foo)
  expect_incompatible_df(vec_ptype_common(foo, foo, df, foo), df)

  expect_identical(
    expect_df_fallback_warning(vec_ptype2_fallback(foo, df)),
    new_fallback_df(df, c("vctrs_foobar", "data.frame"))
  )
  expect_identical(
    expect_df_fallback_warning(vec_ptype2_fallback(df, foo)),
    new_fallback_df(df, c("data.frame", "vctrs_foobar"))
  )
  expect_identical(
    expect_df_fallback_warning(vec_ptype_common_df_fallback(foo, df)),
    df
  )
  expect_identical(
    expect_df_fallback_warning(vec_ptype_common_df_fallback(df, foo)),
    df
  )

  cnds <- list()
  withCallingHandlers(
    warning = function(cnd) {
      cnds <<- append(cnds, list(cnd))
      invokeRestart("muffleWarning")
    },
    expect_identical(
      vec_ptype_common_df_fallback(foo, df, foo, foo),
      df
    )
  )

  # There should be only one warning even if many fallbacks
  expect_length(cnds, 1)
  expect_s3_class(cnds[[1]], "warning")
  expect_match(cnds[[1]]$message, "falling back to <data.frame>")

  expect_incompatible_df(
    vec_cbind(foobar(data.frame(x = 1)), data.frame(y = 2)),
    data.frame(x = 1, y = 2)
  )
  expect_incompatible_df(
    vec_rbind(foo, data.frame(), foo),
    df
  )

  foo <- structure(mtcars[1:3], class = c("foo", "data.frame"))
  bar <- structure(mtcars[4:6], class = c("bar", "data.frame"))
  baz <- structure(mtcars[7:9], class = c("baz", "data.frame"))

  with_fallback_warning(expect_df_fallback_warning(invisible(vec_rbind(foo, data.frame(), foo))))
  with_fallback_warning(expect_df_fallback_warning(invisible(vec_cbind(foo, data.frame(x = 1)))))
  with_fallback_warning(expect_df_fallback_warning(invisible(vec_cbind(foo, data.frame(x = 1), bar))))
  with_fallback_warning(expect_df_fallback_warning(invisible(vec_rbind(foo, baz, bar, baz, foo, bar))))

  expect_snapshot({
    vec_ptype_common_df_fallback(foo, bar, baz)
    vec_ptype_common_df_fallback(foo, baz, bar, baz, foo, bar)

    with_fallback_warning(invisible(vec_rbind(foo, data.frame(), foo)))
    with_fallback_warning(invisible(vec_cbind(foo, data.frame(x = 1))))
    with_fallback_warning(invisible(vec_cbind(foo, data.frame(x = 1), bar)))
    with_fallback_warning(invisible(vec_rbind(foo, baz, bar, baz, foo, bar)))

    with_fallback_quiet(invisible(vec_rbind(foo, data.frame(), foo)))
    with_fallback_quiet(invisible(vec_cbind(foo, data.frame(x = 1))))
    with_fallback_quiet(invisible(vec_cbind(foo, data.frame(x = 1), bar)))
    with_fallback_quiet(invisible(vec_rbind(foo, baz, bar, baz, foo, bar)))
  })
})


# casting -----------------------------------------------------------------

test_that("safe casts work as expected", {
  df <- data.frame(x = 1, y = 0)

  expect_equal(vec_cast(NULL, df), NULL)
  expect_equal(vec_cast(df, df), df)

  expect_equal(vec_cast(data.frame(x = TRUE, y = FALSE), df), df)
})

test_that("warn about lossy coercions", {
  df1 <- data.frame(x = factor("foo"), y = 1)
  df2 <- data.frame(x = factor("bar"))

  expect_lossy(vec_cast(df1, df1[1]), df1[1], x = df1, to = df1[1])

  expect_lossy(
    vec_cast(df1[1], df2),
    data.frame(x = factor(NA, levels = "bar")),
    x = factor("foo"),
    to = factor("bar")
  )

  out <-
    allow_lossy_cast(
      allow_lossy_cast(
        vec_cast(df1, df2),
        factor("foo"), factor("bar")
      ),
      df1, df2
    )

  expect_identical(out, data.frame(x = factor(NA, levels = "bar")))
})

test_that("invalid cast generates error", {
  expect_error(vec_cast(1L, data.frame()), class = "vctrs_error_incompatible_type")
})

test_that("column order matches type", {
  df1 <- data.frame(x = 1, y = "a")
  df2 <- data.frame(x = TRUE, z = 3)

  df3 <- vec_cast(df2, vec_ptype_common(df1, df2))
  expect_named(df3, c("x", "y", "z"))
})

test_that("restore generates correct row/col names", {
  df1 <- data.frame(x = NA, y = 1:4, z = 1:4)
  df1$x <- data.frame(a = 1:4, b = 1:4)

  df2 <- vec_restore(lapply(df1[1:3], vec_slice, 1:2), df1)

  expect_named(df2, c("x", "y", "z"))
  expect_equal(.row_names_info(df2), -2)
})

test_that("restore keeps automatic row/col names", {
  df1 <- data.frame(x = NA, y = 1:4, z = 1:4)
  df1$x <- data.frame(a = 1:4, b = 1:4)

  df2 <- vec_restore(df1, df1)

  expect_named(df2, c("x", "y", "z"))
  expect_equal(.row_names_info(df2), -4)
})

test_that("cast to empty data frame preserves number of rows", {
  out <- vec_cast(new_data_frame(n = 10L), new_data_frame())
  expect_equal(nrow(out), 10L)
})

test_that("can cast unspecified to data frame", {
  df <- data.frame(x = 1, y = 2L)
  expect_identical(vec_cast(unspecified(3), df), vec_init(df, 3))
})

test_that("cannot cast list to data frame", {
  df <- data.frame(x = 1, y = 2L)
  expect_error(vec_cast(list(df, df), df), class = "vctrs_error_incompatible_type")
})

test_that("can restore lists with empty names", {
  expect_identical(vec_restore(list(), data.frame()), data.frame())
})

test_that("can restore subclasses of data frames", {
  expect_identical(vec_restore(list(), subclass(data.frame())), subclass(data.frame()))
  local_methods(
    vec_restore.vctrs_foobar = function(x, to, ..., i) "dispatched"
  )
  expect_identical(vec_restore(list(), subclass(data.frame())), "dispatched")
})

test_that("df_cast() checks for names", {
  x <- new_data_frame(list(1))
  y <- new_data_frame(list(2))
  expect_error(vec_cast_common(x, y), "must have names")
})

test_that("casting to and from data frame preserves row names", {
  out <- vec_cast(mtcars, unrownames(mtcars))
  expect_identical(row.names(out), row.names(mtcars))

  out <- vec_cast(out, unrownames(mtcars))
  expect_identical(row.names(out), row.names(mtcars))
})


# new_data_frame ----------------------------------------------------------

test_that("can construct an empty data frame", {
  expect_identical(new_data_frame(), data.frame())
})

test_that("can validly set the number of rows when there are no columns", {
  expect <- structure(
    list(),
    class = "data.frame",
    row.names = .set_row_names(2L),
    names = character()
  )

  expect_identical(new_data_frame(n = 2L), expect)
})

test_that("can add additional classes", {
  expect_s3_class(new_data_frame(class = "foobar"), "foobar")
  expect_s3_class(new_data_frame(class = c("foo", "bar")), c("foo", "bar"))
})

test_that("can add additional attributes", {
  expect <- data.frame()
  attr(expect, "foo") <- "bar"
  attr(expect, "a") <- "b"

  expect_identical(new_data_frame(foo = "bar", a = "b"), expect)
})

test_that("size is pulled from first column if not supplied", {
  x <- new_data_frame(list(x = 1:5, y = 1:6))
  expect_identical(.row_names_info(x, type = 1), -5L)
})

test_that("can construct a data frame without column names", {
  expect_named(new_data_frame(list(1, 2)), NULL)
})

test_that("the names on an empty data frame are an empty character vector", {
  expect_identical(names(new_data_frame()), character())
})

test_that("class attribute", {
  expect_identical(
    class(new_data_frame(list(a = 1))),
    "data.frame"
  )
  expect_identical(
    class(new_data_frame(list(a = 1), class = "tbl_df")),
    c("tbl_df", "data.frame")
  )
  expect_identical(
    class(new_data_frame(list(a = 1), class = c("tbl_df", "tbl", "data.frame"))),
    c("tbl_df", "tbl", "data.frame", "data.frame")
  )
  expect_identical(
    class(new_data_frame(list(a = 1), class = "foo_frame")),
    c("foo_frame", "data.frame")
  )
  expect_identical(
    class(exec(new_data_frame, list(a = 1), !!!attributes(new_data_frame(list(), class = "tbl_df")))),
    c("tbl_df", "data.frame", "data.frame")
  )
  expect_identical(
    class(exec(new_data_frame, list(a = 1), !!!attributes(new_data_frame(list(b = 1), class = "tbl_df")))),
    c("tbl_df", "data.frame", "data.frame")
  )
})

test_that("attributes with special names are merged", {
  expect_identical(
    names(new_data_frame(list(a = 1))),
    "a"
  )

  expect_identical(
    names(new_data_frame(list(a = 1), names = "name")),
    "name"
  )

  expect_identical(
    names(new_data_frame(list(1), names = "name")),
    "name"
  )

  expect_identical(
    attr(new_data_frame(list()), "row.names"),
    integer()
  )

  expect_identical(
    .row_names_info(new_data_frame(list(), n = 3L)),
    -3L
  )

  expect_error(new_data_frame(list(), n = 1L, row.names = 1:3), ".")

  expect_identical(
    .row_names_info(new_data_frame(list(), n = 3L, row.names = 1:3)),
    3L
  )

  expect_identical(
    .row_names_info(new_data_frame(list(), n = 3L, row.names = c(NA, -3L))),
    -3L
  )

  expect_identical(
    attr(new_data_frame(list(), n = 1L, row.names = "rowname"), "row.names"),
    "rowname"
  )
})

test_that("n and row.names (#894)", {
  # Can omit n if row.names attribute is given
  expect_identical(
    row.names(new_data_frame(list(), row.names = "rowname")),
    "rowname"
  )
  expect_identical(
    attr(new_data_frame(list(), row.names = 2L), "row.names"),
    2L
  )
  expect_identical(
    row.names(new_data_frame(list(), row.names = chr())),
    chr()
  )
})

test_that("`x` must be a list", {
  expect_error(new_data_frame(1), "`x` must be a list")
})

test_that("if supplied, `n` must be an integer of size 1", {
  expect_error(new_data_frame(n = c(1L, 2L)), "must be an integer of size 1")
  expect_error(new_data_frame(n = "x"), "must be an integer of size 1")
})

test_that("`class` must be a character vector", {
  expect_error(new_data_frame(class = 1), "must be NULL or a character vector")
})

test_that("flatten info is computed", {
  df_flatten_info <- function(x) {
    .Call(vctrs_df_flatten_info, x)
  }
  expect_identical(df_flatten_info(mtcars), list(FALSE, ncol(mtcars)))

  df <- tibble(x = 1, y = tibble(x = 2, y = tibble(x = 3), z = 4), z = 5)
  expect_identical(df_flatten_info(df), list(TRUE, 5L))
})

test_that("can flatten data frames", {
  df_flatten <- function(x) {
    .Call(vctrs_df_flatten, x)
  }
  expect_identical(df_flatten(mtcars), mtcars)

  df <- tibble(x = 1, y = tibble(x = 2, y = tibble(x = 3), z = 4), z = 5)
  expect_identical(df_flatten(df), new_data_frame(list(x = 1, x = 2, x = 3, z = 4, z = 5)))
})

test_that("can flatten data frames with rcrd columns containing 1 field (#1318)", {
  col <- new_rcrd(list(x = 1))
  df <- data_frame(col = col, y = 1)
  expect_identical(vec_proxy_equal(df), data_frame(x = 1, y = 1))
})

test_that("new_data_frame() zaps existing attributes", {
  struct <- structure(list(), foo = 1)
  expect_identical(
    attributes(new_data_frame(struct)),
    attributes(new_data_frame(list())),
  )
  expect_identical(
    attributes(new_data_frame(struct, bar = 2)),
    attributes(new_data_frame(list(), bar = 2)),
  )
})

# data_frame --------------------------------------------------------------

test_that("can construct data frames with empty input", {
  expect_identical(data_frame(), new_data_frame())
  expect_named(data_frame(), character())
})

test_that("input is tidy recycled", {
  expect_identical(
    data_frame(x = 1, y = 1:3),
    data_frame(x = c(1, 1, 1), y = 1:3)
  )

  expect_identical(
    data_frame(x = 1, y = integer()),
    data_frame(x = double(), y = integer())
  )

  expect_error(data_frame(1:2, 1:3), class = "vctrs_error_incompatible_size")
})

test_that("dots are dynamic", {
  list_2_data_frame <- function(x) data_frame(!!!x)

  expect_identical(
    list_2_data_frame(list(x = 1, y = 2)),
    data_frame(x = 1, y = 2)
  )
})

test_that("unnamed input is auto named with empty strings", {
  expect_named(data_frame(1, 2, .name_repair = "minimal"), c("", ""))
})

test_that("unnamed data frames are auto spliced", {
  expect_identical(
    data_frame(w = 1, data_frame(x = 2, y = 3), z = 4),
    data_frame(w = 1, x = 2, y = 3, z = 4)
  )
})

test_that("named data frames are not spliced", {
  df_col <- data_frame(x = 2, y = 3)
  df <- data_frame(w = 1, col = data_frame(x = 2, y = 3), z = 4)

  expect_identical(df$col, df_col)
})

test_that("spliced data frames without names are caught", {
  df_col <- new_data_frame(list(1))
  expect_error(data_frame(df_col), "corrupt data frame")
})

test_that("`NULL` inputs are dropped", {
  expect_identical(data_frame(NULL, x = 1, NULL), data_frame(x = 1))
})

test_that("`NULL` inputs are dropped before name repair", {
  expect_identical(
    data_frame(x = NULL, x = 1, .name_repair = "check_unique"),
    data_frame(x = 1)
  )
})

test_that("`.size` can force a desired size", {
  df <- data_frame(x = 1, .size = 5)
  expect_identical(df$x, rep(1, 5))

  expect_size(data_frame(.size = 5), 5L)
})

test_that("`.name_repair` repairs names", {
  expect_named(
    expect_message(data_frame(x = 1, x = 1, .name_repair = "unique")),
    c("x...1", "x...2")
  )
})

test_that("`.name_repair` happens after auto-naming with empty strings", {
  expect_named(
    expect_message(data_frame(1, 2, .name_repair = "unique")),
    c("...1", "...2")
  )
})

test_that("`.name_repair` happens after splicing", {
  expect_named(
    expect_message(data_frame(x = 1, data_frame(x = 2), .name_repair = "unique")),
    c("x...1", "x...2")
  )
})

# fallback ----------------------------------------------------------------

test_that("data frame fallback handles column types (#999)", {
  df1 <- foobar(data.frame(x = 1))
  df2 <- foobar(data.frame(x = 1, y = 2))
  df3 <- foobar(data.frame(x = "", y = 2))

  common <- foobar(data.frame(x = dbl(), y = dbl()))
  expect_identical(vec_ptype2(df1, df2), common)
  expect_identical(vec_ptype2(df2, df1), common)

  expect_error(
    vec_ptype2(df1, df3),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype2(df3, df1),
    class = "vctrs_error_incompatible_type"
  )

  expect_identical(
    vec_cast(df1, df2),
    foobar(data.frame(x = 1, y = na_dbl))
  )
  expect_error(
    vec_cast(df2, df1),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_rbind(df1, df2),
    foobar(data.frame(x = c(1, 1), y = c(NA, 2)))
  )

  # Attributes are not restored
  df1_attrib <- foobar(df1, foo = "foo")
  df2_attrib <- foobar(df2, bar = "bar")
  exp <- data.frame(x = c(1, 1), y = c(NA, 2))
  expect_incompatible_df(
    vec_rbind(df1_attrib, df2_attrib),
    exp
  )

  out <- with_methods(
    `[.vctrs_foobar` = function(x, i, ...) {
      new_data_frame(
        NextMethod(),
        dispatched = TRUE,
        class = "vctrs_foobar"
      )
    },
    vec_rbind(df1_attrib, df2_attrib)
  )

  expect_identical(out, foobar(exp, dispatched = TRUE))
})

test_that("falls back to tibble for tibble subclasses (#1025)", {
  foo <- foobar(tibble::as_tibble(mtcars))
  expect_s3_class(expect_df_fallback_warning_maybe(vec_rbind(foo, mtcars)), "tbl_df")
  expect_s3_class(expect_df_fallback_warning_maybe(vec_rbind(foo, mtcars, mtcars)), "tbl_df")
  expect_s3_class(expect_df_fallback_warning_maybe(vec_rbind(foo, mtcars, foobar(mtcars))), "tbl_df")

  with_fallback_warning(expect_df_fallback_warning(
    vec_rbind(
      foobar(tibble::as_tibble(mtcars)),
      mtcars,
      foobaz(mtcars)
    )
  ))
  with_fallback_warning(expect_df_fallback_warning(
    vec_rbind(
      tibble::as_tibble(mtcars),
      foobar(tibble::as_tibble(mtcars))
    )
  ))
  with_fallback_warning(expect_df_fallback_warning(
    vec_rbind(
      foobar(tibble::as_tibble(mtcars)),
      mtcars,
      foobar(tibble::as_tibble(mtcars))
    )
  ))

  expect_snapshot({
    with_fallback_warning(
      invisible(vec_rbind(
        foobar(tibble::as_tibble(mtcars)),
        mtcars,
        foobaz(mtcars)
      ))
    )
    with_fallback_warning(
      invisible(vec_rbind(
        tibble::as_tibble(mtcars),
        foobar(tibble::as_tibble(mtcars))
      ))
    )
    with_fallback_warning(
      invisible(vec_rbind(
        foobar(tibble::as_tibble(mtcars)),
        mtcars,
        foobar(tibble::as_tibble(mtcars))
      ))
    )

    with_fallback_quiet(
      invisible(vec_rbind(
        foobar(tibble::as_tibble(mtcars)),
        mtcars,
        foobaz(mtcars)
      ))
    )
    with_fallback_quiet(
      invisible(vec_rbind(
        tibble::as_tibble(mtcars),
        foobar(tibble::as_tibble(mtcars))
      ))
    )
    with_fallback_quiet(
      invisible(vec_rbind(
        foobar(tibble::as_tibble(mtcars)),
        mtcars,
        foobar(tibble::as_tibble(mtcars))
      ))
    )
  })
})

test_that("fallback is recursive", {
  df <- mtcars[1:3, 1, drop = FALSE]

  foo <- new_data_frame(list(x = foobar(df, foo = TRUE)))
  bar <- new_data_frame(list(x = foobar(df, bar = TRUE)))
  baz <- new_data_frame(list(y = 1:3, x = foobar(df, bar = TRUE)))

  exp <- new_data_frame(list(x = vec_rbind(df, df)))
  expect_incompatible_df(vec_rbind(foo, bar), exp)

  exp <- new_data_frame(list(x = vec_rbind(df, df), y = c(NA, NA, NA, 1:3)))
  expect_incompatible_df(vec_rbind(foo, baz), exp)
})
