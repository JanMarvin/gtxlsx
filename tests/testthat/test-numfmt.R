test_that("formats are recovered when they reproduce the data", {
  out <- infer_numfmt(c("$1,234", "$56"), c(1234, 56))
  expect_equal(out$numfmt, "\"$\"#,##0")
  expect_equal(out$values, c(1234, 56))

  expect_equal(infer_numfmt(c("12.5%", "7.0%"), c(0.125, 0.07))$numfmt, "0.0%")
  expect_equal(infer_numfmt(c("1,204.50", "56.00"), c(1204.5, 56))$numfmt, "#,##0.00")
})

test_that("html entities in the rendered text are decoded first", {
  out <- infer_numfmt(c("&#8364;49.95", "&#8364;65,100.00"), c(49.95, 65100))
  expect_equal(out$numfmt, "\"\u20ac\"#,##0.00")
})

test_that("a minus inside the prefix flips the sign", {
  out <- infer_numfmt(c("\u2212$700", "$1,200"), c(-700, 1200))
  expect_equal(out$values, c(-700, 1200))
})

test_that("the rendered text may be a rounding of the value", {
  expect_false(is.null(infer_numfmt(c("13.26", "1.00"), c(13.255, 1))))
})

test_that("anything that cannot round trip stays text", {
  expect_null(infer_numfmt(c("1.2", "3.45"), c(1.2, 3.45)))
  expect_null(infer_numfmt("<b>1</b>", 1))
  expect_null(infer_numfmt(c("1.2K", "3.4M"), c(1200, 3400000)))
  expect_null(infer_numfmt(c("a", "b"), c("a", "b")))
  expect_null(infer_numfmt(NA_character_, NA_real_))
})

test_that("html text converts only with symbol affixes", {
  expect_equal(text_as_number("$447,000")$value, 447000)
  expect_equal(text_as_number("12.5%")$value, 0.125)
  expect_null(text_as_number("458 Speciale"))
  expect_null(text_as_number("1 kB"))
  expect_null(text_as_number(""))
})

test_that("values printed without a leading zero still become numbers", {
  # gt gained drop_leading_zero, which prints .75 rather than 0.75; "#" is the
  # number format code that suppresses the leading zero the same way
  out <- infer_numfmt(c(".75", ".20"), c(0.75, 0.20))
  expect_equal(out$numfmt, "#.00")
  expect_equal(out$values, c(0.75, 0.20))

  expect_equal(text_as_numbers(".75")$numfmt, "#.00")
  expect_equal(infer_numfmt(c("1.50", "2.25"), c(1.5, 2.25))$numfmt, "0.00")
})

test_that("marks are worked out from the whole input", {
  v <- function(x, ...) suppressWarnings(text_as_numbers(x, ...)$value)

  expect_equal(v(c("0.001", "0.025", "1.5")), c(0.001, 0.025, 1.5))
  expect_equal(v(c("0,001", "0,123", "12,5")), c(0.001, 0.123, 12.5))
  expect_equal(v(c("1,234,567", "1,234", "12.5")), c(1234567, 1234, 12.5))
  expect_equal(v(c("1.234.567", "1.234", "12,5")), c(1234567, 1234, 12.5))
  expect_equal(v(c("1 234 567", "1 234,5")), c(1234567, 1234.5))
  expect_equal(v(c("1.234,50", "1.234", "0,5")), c(1234.5, 1234, 0.5))
  expect_equal(v("$447,000"), 447000)

  # nothing settles a lone "." in front of three digits
  expect_true(all(is.na(v(c("1.234", "12.345", "5.678")))))
  expect_equal(v(c("1,234", "5,678")), c(1234, 5678))
})

test_that("a text that the number would not give back stays text", {
  v <- function(x, ...) suppressWarnings(text_as_numbers(x, ...)$value)
  expect_true(all(is.na(v(c("007", "00.5", "0012")))))
  expect_true(is.na(v("12345678901234567890")))
  expect_true(is.na(v("1.234.5")))
  expect_true(is.na(v("1,234.567,8")))
  expect_true(is.na(v("01.234")))
  expect_equal(v(c("0", "10", "-5")), c(0, 10, -5))
  expect_equal(v(".75"), 0.75)
})

test_that("a table that mixes marks is not converted", {
  expect_warning(
    out <- text_as_numbers(c("1,234.5", "1.234.567", "42")),
    "more than one decimal or group mark"
  )
  expect_equal(out$value, c(NA, NA, 42))
  expect_warning(text_as_numbers(c("1.234,5", "12.5")), "more than one")
})

test_that("marks given by the user settle every cell", {
  v <- function(x, ...) text_as_numbers(x, ...)$value
  expect_equal(v(c("1.234", "12.345"), dec_mark = "."), c(1.234, 12.345))
  expect_equal(v(c("1.234", "12.345"), dec_mark = ","), c(1234, 12345))
  expect_equal(v("1,234", dec_mark = ","), 1.234)
  expect_equal(v("1,234", dec_mark = "."), 1234)
  expect_equal(v("1.234", big_mark = "."), 1234)
  expect_equal(v("1.234", big_mark = ","), 1.234)
  expect_equal(v("1'234.5", big_mark = "'"), 1234.5)
  expect_equal(v("1.234", big_mark = ""), 1.234)
  expect_true(is.na(v("0.001", dec_mark = ",")))
  expect_true(is.na(v("007", dec_mark = ".")))

  expect_equal(text_as_numbers("1.234,5", dec_mark = ",")$numfmt, "#,##0.0")
  expect_equal(text_as_numbers("1.234", dec_mark = ".")$numfmt, "0.000")
  expect_null(text_as_number("1.234"))
  expect_equal(text_as_number("1.234", dec_mark = ".")$value, 1.234)
})

test_that("mark arguments are checked", {
  expect_error(text_as_numbers("1", dec_mark = ".."), "single character")
  expect_error(text_as_numbers("1", dec_mark = ""), "single character")
  expect_error(text_as_numbers("1", dec_mark = "1"), "single character")
  expect_error(text_as_numbers("1", big_mark = c(",", ".")), "single character")
  expect_error(text_as_numbers("1", dec_mark = ".", big_mark = "."), "must differ")
})

test_that("french style spaces are read as group marks", {
  for (sp in c(" ", " ", " ", " ")) {
    out <- text_as_numbers(c(paste0("1", sp, "000", sp, "000"),
                             paste0("2", sp, "500"), "999"))
    expect_equal(out$value, c(1e6, 2500, 999))
    expect_equal(out$numfmt[1L], "#,##0")
    dec <- text_as_numbers(c(paste0("1", sp, "234,50"), "12,00", "0,50"))
    expect_equal(dec$value, c(1234.5, 12, 0.5))
    expect_equal(dec$numfmt[1L], "#,##0.00")
  }
})
