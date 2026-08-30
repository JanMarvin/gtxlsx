skip_if_not_installed("gt")

test_that("a gt table lands in the expected range", {
  gt_tbl <- gt::gt(data.frame(a = c("x", "y"), b = c(1.5, 2.5))) |>
    gt::tab_header(title = "T", subtitle = "S") |>
    gt::fmt_number(columns = "b", decimals = 1) |>
    gt::tab_source_note("note")

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, gt_tbl, dims = "B2")

  df <- openxlsx2::wb_to_df(wb, col_names = FALSE, show_formula = FALSE)
  expect_true("T" %in% unlist(df))
  expect_true("note" %in% unlist(df))
  expect_true(length(wb$worksheets[[1]]$mergeCells) > 0L)
})

test_that("row groups produce heading rows", {
  df <- data.frame(g = c("A", "A", "B"), r = c("r1", "r2", "r3"), v = 1:3)
  gt_tbl <- gt::gt(df, rowname_col = "r", groupname_col = "g")

  g <- gtxlsx_extract(gt_tbl)
  expect_equal(nrow(g$groups_rows), 2L)
  expect_equal(g$groups_rows$group_label, c("A", "B"))
})
