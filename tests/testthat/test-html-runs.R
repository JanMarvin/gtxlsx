test_that("inline markup becomes separate runs", {
  runs <- html_runs("plain <b>bold</b> and <em>italic</em>")
  expect_length(runs, 4L)
  expect_true(runs[[2L]]$bold)
  expect_true(runs[[4L]]$italic)
  expect_equal(runs[[2L]]$text, "bold")
})

test_that("entities and breaks are resolved", {
  runs <- html_runs("a &amp; b<br>c")
  expect_equal(vapply(runs, `[[`, "", "text"), c("a & b", "\n", "c"))
  expect_equal(html_unescape("&#x2014;&#8212;&nbsp;"), "\u2014\u2014\u00a0")
  expect_equal(html_unescape("plain"), "plain")
})

test_that("nested tags restore the state around them", {
  runs <- html_runs("<b>a<sup>1</sup>b</b>")
  expect_equal(vapply(runs, function(r) isTRUE(r$bold), NA), c(TRUE, TRUE, TRUE))
  expect_equal(runs[[2L]]$vert_align, "superscript")
  expect_null(runs[[3L]]$vert_align)
})

test_that("inline css and font attributes are read", {
  runs <- html_runs('<span style="color:#FF0000;font-weight:700">x</span>')
  expect_equal(runs[[1L]]$color, "FFFF0000")
  expect_true(runs[[1L]]$bold)

  runs <- html_runs(
    '<font color="blue" face="Georgia">y</font>'
  )
  expect_equal(runs[[1L]]$color, "FF0000FF")
  expect_equal(runs[[1L]]$font, "Georgia")
})

test_that("a class resolver reaches runs", {
  res <- function(tag, classes) {
    if ("hot" %in% classes) list(color = "#00FF00") else list()
  }
  runs <- html_runs('<span class="hot">z</span>', resolver = res)
  expect_equal(runs[[1L]]$color, "FF00FF00")
})

test_that("wrapper breaks are trimmed and tags stripped", {
  expect_equal(vapply(html_runs("<p>x</p>"), `[[`, "", "text"), "x")
  expect_equal(html_strip("a<br>b &amp; <b>c</b>"), "a\nb & c")
  expect_true(is.na(html_strip(NA_character_)))
  expect_true(is_rich("<b>x</b>"))
  expect_false(is_rich("plain"))
})

test_that("runs become an fmt_txt string", {
  out <- html_to_fmt("<b>x</b>", font = "Calibri", size = 11)
  expect_s3_class(out, "fmt_txt")
  # as.character() on fmt_txt gives the visible text, the run markup is beneath
  expect_match(unclass(out), "<b", fixed = TRUE)
  expect_match(unclass(out), "Calibri", fixed = TRUE)
  expect_s3_class(html_to_fmt(NA_character_), "fmt_txt")
})
