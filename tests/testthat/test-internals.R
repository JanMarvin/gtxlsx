test_that("cell records merge with later writes winning", {
  cc <- new_sheet_cells()
  put_cell(cc, 1L, 1L, "a", fill = "FF000000")
  put_cell(cc, 1L, 1L, bold = TRUE)
  recs <- merge_records(collect_cells(cc))

  expect_length(recs, 1L)
  expect_equal(recs[[1L]]$text, "a")
  expect_true(recs[[1L]]$bold)
  expect_equal(recs[[1L]]$fill, "FF000000")
})

test_that("scattered cells collapse into column runs", {
  expect_equal(ref_runs(c(2L, 3L, 4L, 7L), c(3L, 3L, 3L, 3L)), c("C2:C4", "C7"))
  expect_equal(ref_runs(2L, 3L), "C2")
  expect_equal(ref_runs(integer(0), integer(0)), character(0))
  expect_equal(ref_of(2L, 3L), "C2")
})

test_that("selector lists split at bracket depth", {
  expect_equal(split_top("a, b(1, 2), c"), c("a", "b(1, 2)", "c"))
  expect_equal(expand_is("x :is(td, th)"), c("x td", "x th"))
  expect_equal(join_sel("a", "&.b"), "a.b")
  expect_equal(join_sel("a", "b"), "a b")
  expect_equal(join_sel("", "b"), "b")
})

test_that("border shorthands are taken apart", {
  expect_equal(shorthand_part("1px solid red", "style"), "solid")
  expect_equal(shorthand_part("1px solid red", "width"), "1px")
  expect_equal(shorthand_part("1px solid red", "color"), "red")
  expect_null(shorthand_part(NULL, "style"))
})

test_that("small helpers behave", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_true(has_text("a"))
  expect_false(has_text(NULL))
  expect_false(has_text(""))
  expect_equal(pick_font(c("system-ui", "Georgia")), "Georgia")
  expect_equal(pick_font("sans-serif"), "Calibri")
  expect_equal(chr_col(list("a", NULL)), c("a", NA))
  expect_equal(px_to_width(96), 13)
  expect_null(as_df(NULL))
})
