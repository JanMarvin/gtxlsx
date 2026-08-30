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
