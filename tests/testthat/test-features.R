# Each switch has to actually stop the workbook being touched, so these count
# what landed in the style tables rather than trusting the argument.

style_counts <- function(wb) {
  sm <- wb$styles_mgr$styles
  ws <- wb$worksheets[[1L]]
  list(fonts = length(sm$fonts), fills = length(sm$fills),
       borders = length(sm$borders), links = length(ws$hyperlinks),
       merges = length(ws$mergeCells))
}

styled_gt <- function() {
  tbl <- gt::gt(head(gt::exibble, 3))
  tbl <- gt::tab_header(tbl, title = "T")
  tbl <- gt::tab_spanner(tbl, label = "sp", columns = c("num", "char"))
  gt::tab_style(tbl, gt::cell_fill(color = "yellow"),
                gt::cells_body(columns = "num", rows = 1))
}

test_that("features = FALSE writes values and nothing else", {
  skip_no_gt()
  all <- style_counts(wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(),
                                styled_gt(), dims = "A1"))
  bare <- style_counts(wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(),
                                 styled_gt(), dims = "A1", features = FALSE))
  expect_lt(bare$fonts, all$fonts)
  expect_lt(bare$borders, all$borders)
  expect_equal(bare$merges, 0L)

  # the values are still there
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), styled_gt(),
                  dims = "A1", features = FALSE)
  expect_gt(nrow(openxlsx2::wb_to_df(wb, col_names = FALSE)), 2L)
})

test_that("switches can be picked one at a time", {
  skip_no_gt()
  all <- style_counts(wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(),
                                styled_gt(), dims = "A1"))
  picked <- c("font", "fill", "numfmt", "merge")
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), styled_gt(),
                  dims = "A1", features = picked)
  no_border <- style_counts(wb)

  expect_lt(no_border$borders, all$borders)
  expect_equal(no_border$fonts, all$fonts)
  expect_equal(no_border$merges, all$merges)
})

test_that("links can be switched off on the html path", {
  html <- paste0("<table><tr><td><a href=\"https://a.org\">a</a></td>",
                 "<td>1</td></tr></table>")
  on <- wb_add_html(openxlsx2::wb_workbook()$add_worksheet(), html, dims = "A1")
  off <- wb_add_html(openxlsx2::wb_workbook()$add_worksheet(), html, dims = "A1",
                     features = c("font", "fill", "border", "numfmt", "merge"))

  expect_length(unlist(on$worksheets[[1L]]$hyperlinks), 1L)
  expect_length(unlist(off$worksheets[[1L]]$hyperlinks), 0L)
  # the anchor text is still written
  expect_equal(openxlsx2::wb_to_df(off, col_names = FALSE)[1L, 1L], "a")
})

test_that("an unknown feature is refused by name", {
  skip_no_gt()
  expect_error(
    wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), styled_gt(),
              features = "colour"),
    "unknown feature: colour"
  )
})

test_that("freeze puts the pane below the heading and beside the stub", {
  skip_no_gt()
  d <- data.frame(k = c("a", "b"), v = c(1, 2), stringsAsFactors = FALSE)
  tbl <- gt::tab_header(gt::gt(d, rowname_col = "k"), title = "T")

  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), tbl, dims = "A1",
                  freeze = TRUE)
  pane <- wb$worksheets[[1L]]$freezePane
  expect_match(pane, "state=\"frozen\"", fixed = TRUE)
  # title, then column labels, so the body starts at row 3 and data at column B
  expect_match(pane, 'topLeftCell="B3"', fixed = TRUE)

  none <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), tbl, dims = "A1")
  expect_false(nzchar(none$worksheets[[1L]]$freezePane %||% ""))
})

test_that("freeze takes an explicit cell", {
  skip_no_gt()
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), styled_gt(),
                  dims = "A1", freeze = c(4, 3))
  expect_match(wb$worksheets[[1L]]$freezePane, 'topLeftCell="C4"', fixed = TRUE)

  expect_error(
    wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), styled_gt(),
              freeze = c(1, 2, 3)),
    "TRUE, FALSE, or c(row, col)", fixed = TRUE
  )
})

test_that("the html path works out its own header rows and stub column", {
  html <- paste0("<table><thead><tr><th>k</th><th>v</th></tr></thead>",
                 "<tbody><tr><th>a</th><td>1</td></tr>",
                 "<tr><th>b</th><td>2</td></tr></tbody></table>")
  wb <- wb_add_html(openxlsx2::wb_workbook()$add_worksheet(), html, dims = "A1",
                    freeze = TRUE)
  # one header row, one leading th column
  expect_match(wb$worksheets[[1L]]$freezePane, 'topLeftCell="B2"', fixed = TRUE)
})
