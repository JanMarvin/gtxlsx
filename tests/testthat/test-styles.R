test_that("cell_text and cell_fill become record attributes", {
  skip_no_gt()
  th <- gtxlsx_theme(gtxlsx_extract(small_gt())$options)
  st <- gt::cell_text(color = "#FF0000", font = "Georgia", size = gt::px(20),
                      weight = "bold", style = "italic", decorate = "underline",
                      align = "right", v_align = "top", whitespace = "nowrap")
  st <- c(st, gt::cell_fill(color = "yellow"))
  a <- style_attrs(st, th)

  expect_equal(a$color, "FFFF0000")
  expect_equal(a$font, "Georgia")
  expect_equal(a$size, 15)
  expect_true(a$bold)
  expect_true(a$italic)
  expect_true(a$underline)
  expect_equal(a$halign, "right")
  expect_equal(a$valign, "top")
  expect_false(a$wrap)
  expect_equal(a$fill, "FFFFFF00")
})

test_that("cell_borders become per side entries", {
  skip_no_gt()
  b <- style_borders(gt::cell_borders(sides = c("top", "left"), color = "#00FF00",
                                      style = "dashed", weight = gt::px(2)))
  sides <- vapply(b, `[[`, "", "side")
  expect_setequal(sides, c("top", "left"))
  expect_equal(b[[1L]]$color, "FF00FF00")
  expect_equal(b[[1L]]$border, "mediumDashed")
})

test_that("every style location maps onto cells", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B"), r = paste0("r", 1:3), v = c(1, 2, 3),
                  stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "r", groupname_col = "g")
  tbl <- gt::tab_header(tbl, title = "T", subtitle = "S")
  tbl <- gt::tab_spanner(tbl, label = "Sp", columns = "v")
  tbl <- gt::tab_stubhead(tbl, label = "id")
  tbl <- gt::summary_rows(tbl, columns = "v", fns = list(tot = ~ sum(.x)))
  tbl <- gt::grand_summary_rows(tbl, columns = "v", fns = list(all = ~ sum(.x)))
  tbl <- gt::tab_source_note(tbl, "src")
  tbl <- gt::tab_footnote(tbl, "fn", locations = gt::cells_body(columns = "v", rows = 1))

  fill <- gt::cell_fill(color = "yellow")
  tbl <- gt::tab_style(tbl, fill, gt::cells_title(groups = "title"))
  tbl <- gt::tab_style(tbl, fill, gt::cells_title(groups = "subtitle"))
  tbl <- gt::tab_style(tbl, fill, gt::cells_stubhead())
  tbl <- gt::tab_style(tbl, fill, gt::cells_column_labels(columns = "v"))
  tbl <- gt::tab_style(tbl, fill, gt::cells_column_spanners(spanners = "Sp"))
  tbl <- gt::tab_style(tbl, fill, gt::cells_row_groups(groups = "A"))
  tbl <- gt::tab_style(tbl, fill, gt::cells_body(columns = "v", rows = 2))
  tbl <- gt::tab_style(tbl, fill, gt::cells_stub(rows = 1))
  tbl <- gt::tab_style(tbl, fill, gt::cells_summary(groups = "A", columns = "v"))
  tbl <- gt::tab_style(tbl, fill, gt::cells_grand_summary(columns = "v"))
  tbl <- gt::tab_style(tbl, fill, gt::cells_footnotes())
  tbl <- gt::tab_style(tbl, fill, gt::cells_source_notes())
  tbl <- gt::tab_style(tbl, gt::cell_borders(sides = "bottom"),
                       gt::cells_body(columns = "v", rows = 3))

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  expect_silent(wb <- wb_add_gt(wb, tbl, dims = "A1"))

  fills <- vapply(wb$styles_mgr$styles$fills, function(x) {
    a <- openxlsx2::xml_attr(x, "fill", "patternFill", "fgColor")
    if (length(a) && !is.null(a[[1L]][["rgb"]])) a[[1L]][["rgb"]] else "-"
  }, "")
  expect_true("FFFFFF00" %in% fills)
})

test_that("option borders are drawn without erroring", {
  skip_no_gt()
  tbl <- gt::opt_table_lines(gt::tab_header(small_gt(), title = "T"), "all")
  tbl <- gt::tab_options(tbl, table_body.vlines.style = "none",
                         stub.border.style = "solid")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  expect_true(length(wb$styles_mgr$styles$borders) > 1L)
})

test_that("a style with no usable target is skipped", {
  skip_no_gt()
  p <- list(title_row = NA_integer_, subtitle_row = NA_integer_,
            label_row = NA_integer_, col0 = 1L, w = 1L, stub_vars = character(0),
            col_vars = "a", spanners = NULL, group_head = data.frame(),
            group_span = data.frame(), summaries = list(),
            footnote_rows = integer(0), source_rows = integer(0),
            body_row = integer(0), col_of = function(v) 1L)
  expect_null(style_targets(list(locname = "title"), NULL, p))
  expect_null(style_targets(list(locname = "unknown"), NULL, p))
})
