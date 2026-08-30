test_that("a plain range becomes a gt table", {
  skip_no_gt()
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb$add_data(x = data.frame(a = c("x", "y"), b = c(1, 2), stringsAsFactors = FALSE))

  tbl <- wb_to_gt(wb, dims = "A1:B3", styles = FALSE)
  expect_s3_class(tbl, "gt_tbl")
  expect_equal(names(tbl$`_data`), c("a", "b"))
  expect_true(is.numeric(tbl$`_data`$b))
})

test_that("styles come back as tab_style entries", {
  skip_no_gt()
  src <- gt::tab_style(
    gt::gt(data.frame(a = c("x", "y"), b = c(1, 2), stringsAsFactors = FALSE)),
    style = list(gt::cell_fill(color = "lightblue"), gt::cell_text(weight = "bold")),
    locations = gt::cells_body(columns = "b", rows = 2)
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, src, dims = "A1")

  back <- wb_to_gt(wb, dims = "A1:B3")
  st <- back$`_styles`
  expect_true(nrow(st) > 0L)
  k <- which(st$rownum == 2L & st$colname == "b")
  expect_true(length(k) > 0L)
  expect_equal(st$styles[[k[1L]]]$cell_fill$color, "#ADD8E6")
})

test_that("merged rows are read as heading and source notes", {
  skip_no_gt()
  src <- gt::tab_source_note(
    gt::tab_header(gt::gt(data.frame(a = c("x", "y"), b = c(1, 2))),
                   title = "T", subtitle = "S"),
    "src"
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, src, dims = "A1")

  back <- wb_to_gt(wb, styles = FALSE)
  expect_equal(as.character(back$`_heading`$title), "T")
  expect_equal(as.character(back$`_heading`$subtitle), "S")
  expect_true(length(back$`_source_notes`) > 0L)
})

test_that("spanners survive the round trip", {
  skip_no_gt()
  src <- gt::tab_spanner(
    gt::gt(data.frame(a = c("x", "y"), b = c(1, 2), c = c(3, 4))),
    label = "Pair", columns = c("b", "c")
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, src, dims = "A1")

  back <- wb_to_gt(wb, styles = FALSE)
  expect_true("Pair" %in% back$`_spanners`$spanner_id)
})

test_that("structure = FALSE reads the range flat", {
  skip_no_gt()
  src <- gt::tab_header(gt::gt(data.frame(a = c("x", "y"))), title = "T")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, src, dims = "A1")

  back <- wb_to_gt(wb, styles = FALSE, structure = FALSE)
  expect_null(back$`_heading`$title)
})

test_that("empty header cells are given names", {
  skip_no_gt()
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb$add_data(dims = "A2", x = "v", col_names = FALSE)
  back <- wb_to_gt(wb, dims = "A1:A2", styles = FALSE)
  expect_true(all(nzchar(names(back$`_data`))))
})

test_that("bad input is rejected", {
  expect_error(wb_to_gt("nope"), "wbWorkbook")
})
