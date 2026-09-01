test_that("stub indentation reaches the cell and grows with the gt value", {
  skip_no_gt()
  d <- data.frame(k = c("Total", "Part", "Deep", "Plain"), v = 1:4,
                  stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "k")
  tbl <- gt::tab_stub_indent(tbl, rows = 2, indent = 2)
  tbl <- gt::tab_stub_indent(tbl, rows = 3, indent = 5)

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  none <- cell_alignment(wb, "A2")$indent
  two <- cell_alignment(wb, "A3")$indent
  five <- cell_alignment(wb, "A4")$indent
  plain <- cell_alignment(wb, "A5")$indent

  expect_null(none)
  expect_null(plain)
  expect_equal(as.integer(two), 2L)
  expect_equal(as.integer(five), 5L)
})

test_that("indentation matches what gt recorded in the stub", {
  skip_no_gt()
  d <- data.frame(k = paste0("r", 1:4), v = 1:4, stringsAsFactors = FALSE)
  tbl <- gt::tab_stub_indent(gt::gt(d, rowname_col = "k"), rows = 2:3, indent = 3)

  g <- gtxlsx_extract(tbl)
  th <- gtxlsx_theme(g$options)
  p <- gtxlsx_plan(g, th, 1L, 1L)
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  want <- suppressWarnings(as.integer(g$stub$indent))
  for (i in seq_along(want)) {
    got <- cell_alignment(wb, ref_of(p$body_row[i], 1L))$indent
    got <- if (is.null(got)) 0L else as.integer(got)
    expect_equal(got, if (is.na(want[i])) 0L else want[i], info = paste("row", i))
  }
})

test_that("row heights follow gt's padding per region", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B"), k = paste0("r", 1:3), v = c(1, 2, 3),
                  stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "k", groupname_col = "g")
  tbl <- gt::tab_header(tbl, title = "T", subtitle = "S")

  g <- gtxlsx_extract(tbl)
  th <- gtxlsx_theme(g$options)
  p <- gtxlsx_plan(g, th, 1L, 1L)
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1", row_heights = "gt")

  expect_equal(row_height(wb, p$label_row),
               row_height_pt(th$label_size, th$pad_label))
  expect_equal(row_height(wb, p$body_row[1L]),
               row_height_pt(th$size, th$pad_row))
  expect_equal(row_height(wb, p$group_head$row[1L]),
               row_height_pt(th$group_size, th$pad_group))
})

test_that("more padding makes taller rows", {
  skip_no_gt()
  tight <- gt::tab_options(small_gt(), data_row.padding = gt::px(2))
  loose <- gt::tab_options(small_gt(), data_row.padding = gt::px(20))

  wb1 <- openxlsx2::wb_workbook()$add_worksheet()
  wb1 <- wb_add_gt(wb1, tight, dims = "A1", row_heights = "gt")
  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  wb2 <- wb_add_gt(wb2, loose, dims = "A1", row_heights = "gt")

  expect_lt(row_height(wb1, 2L), row_height(wb2, 2L))
})

test_that("row heights are off unless asked for", {
  skip_no_gt()
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, small_gt(), dims = "A1")
  expect_true(is.na(row_height(wb, 2L)))
  expect_equal(cell_alignment(wb, "B2")$vertical, "bottom")

  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  wb2 <- wb_add_gt(wb2, small_gt(), dims = "A1", row_heights = 30)
  expect_equal(row_height(wb2, 2L), 30)
})

test_that("setting heights centres the text, since Excel aligns to the bottom", {
  skip_no_gt()
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, small_gt(), dims = "A1", row_heights = "gt")
  expect_equal(cell_alignment(wb, "B2")$vertical, "center")

  # an explicit v_align from gt still wins
  tbl <- gt::tab_style(small_gt(), gt::cell_text(v_align = "top"),
                       gt::cells_body(columns = "n", rows = 1))
  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  wb2 <- wb_add_gt(wb2, tbl, dims = "A1", row_heights = "gt")
  expect_equal(cell_alignment(wb2, "B2")$vertical, "top")
})

test_that("wrapped rows keep Excel's own sizing", {
  skip_no_gt()
  tbl <- gt::tab_source_note(small_gt(), "a source note")
  g <- gtxlsx_extract(tbl)
  th <- gtxlsx_theme(g$options)
  p <- gtxlsx_plan(g, th, 1L, 1L)

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1", row_heights = "gt")
  expect_true(is.na(row_height(wb, p$source_rows[1L])))
})
