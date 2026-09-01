# gt drives most of its look from a stylesheet rather than from `_styles`, so
# `tab_options()` settings never appear as style rows and the checks in
# test-fidelity.R cannot see them. These tests read gt's own rendered CSS,
# resolve it per region with the package's CSS reader, and compare the result
# against the cells that region was written to.

test_that("the default theme matches gt's stylesheet region by region", {
  skip_no_gt()
  expect_css_matches_gt(full_tbl())
})

test_that("opt_stylize colours and weights match gt's stylesheet", {
  skip_no_gt()
  expect_css_matches_gt(gt::opt_stylize(full_tbl(), style = 3, color = "blue"))
})

test_that("tab_options settings reach the sheet", {
  skip_no_gt()
  tbl <- gt::tab_options(
    full_tbl(),
    column_labels.background.color = "#204060",
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    stub.font.weight = "bold",
    table.font.size = gt::px(20)
  )
  expect_css_matches_gt(tbl)
})

test_that("regions that gt gives a bottom border get one", {
  skip_no_gt()
  tbl <- full_tbl()
  g <- gtxlsx_extract(tbl)
  th <- gtxlsx_theme(g$options)
  p <- gtxlsx_plan(g, th, 1L, 1L)
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  html <- suppressWarnings(as.character(gt::as_raw_html(tbl, inline_css = FALSE)))
  doc <- xml2::read_html(html)
  rules <- parse_stylesheet(doc)

  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  has_bottom <- function(ref) {
    id <- as.integer(cc$c_s[cc$r == ref]) + 1L
    xf <- openxlsx2::xml_attr(st$cellXfs[[id]], "xf")[[1L]]
    grepl("<bottom style=", st$borders[[as.integer(xf[["borderId"]]) + 1L]],
          fixed = TRUE)
  }

  spec <- list(
    list("gt_column_spanner", "span", ref_of(p$level_row[[1L]], 2L)),
    list("gt_col_heading", "th", ref_of(p$label_row, 2L)),
    list("gt_group_heading", "th", ref_of(p$group_head$row[1L], 1L))
  )
  for (s in spec) {
    want <- decl_to_rec(css_for(rules, s[[2L]], s[[1L]], NA_character_), th$base_px)
    if (is.null(want$borders$bottom)) next
    expect_true(has_bottom(s[[3L]]), info = paste(s[[1L]], "at", s[[3L]]))
  }
})

test_that("opt_table_lines(\"all\") gives the column labels vertical lines", {
  skip_no_gt()
  tbl <- gt::opt_table_lines(full_tbl(), "all")
  g <- gtxlsx_extract(tbl)
  th <- gtxlsx_theme(g$options)
  p <- gtxlsx_plan(g, th, 1L, 1L)
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  id <- as.integer(cc$c_s[cc$r == ref_of(p$label_row, 2L)]) + 1L
  xf <- openxlsx2::xml_attr(st$cellXfs[[id]], "xf")[[1L]]
  bd <- st$borders[[as.integer(xf[["borderId"]]) + 1L]]
  expect_match(bd, "<left style=", fixed = TRUE)
  expect_match(bd, "<right style=", fixed = TRUE)
})

test_that("every themed variant agrees with gt's stylesheet", {
  skip_no_gt()
  variants <- list(
    gt::opt_stylize(full_tbl(), style = 2, color = "green"),
    gt::opt_stylize(full_tbl(), style = 6, color = "gray"),
    gt::opt_row_striping(full_tbl()),
    gt::opt_all_caps(full_tbl()),
    gt::opt_table_lines(full_tbl(), "all"),
    gt::tab_options(full_tbl(), table.font.size = gt::px(20))
  )
  for (tbl in variants) expect_css_matches_gt(tbl)
})
