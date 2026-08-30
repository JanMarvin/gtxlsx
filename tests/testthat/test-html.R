test_that("inline markup becomes separate runs", {
  runs <- html_runs("plain <b>bold</b> and <em>italic</em>")
  expect_equal(length(runs), 4L)
  expect_true(runs[[2L]]$bold)
  expect_true(runs[[4L]]$italic)
  expect_equal(runs[[2L]]$text, "bold")
})

test_that("entities, breaks and spans are handled", {
  runs <- html_runs("a &amp; b<br>c")
  expect_equal(vapply(runs, `[[`, "", "text"), c("a & b", "\n", "c"))

  runs <- html_runs("<span style=\"color:#FF0000;font-weight:700\">x</span>")
  expect_equal(runs[[1L]]$color, "FFFF0000")
  expect_true(runs[[1L]]$bold)
})

test_that("nested tags restore the outer state", {
  runs <- html_runs("<b>a<sup>1</sup>b</b>")
  expect_equal(vapply(runs, function(r) isTRUE(r$bold), NA), c(TRUE, TRUE, TRUE))
  expect_equal(runs[[2L]]$vert_align, "superscript")
  expect_null(runs[[3L]]$vert_align)
})

test_that("css sizes and colours", {
  expect_equal(css_pt("16px"), 12)
  expect_equal(css_pt("125%", base = 16), 15)
  expect_equal(css_color("red"), "FFFF0000")
  expect_equal(css_color("#0000ff"), "FF0000FF")
  expect_equal(css_border("solid", "3px"), "thick")
  expect_equal(css_border("none"), "none")
})
