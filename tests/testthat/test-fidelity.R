# These compare the workbook against the styles gt itself declared, cell by
# cell, so a style that lands on the wrong cell or gets collapsed onto its
# neighbours fails here rather than in a visual check.

test_that("data_color fills and its contrast text colour both survive", {
  skip_no_gt()
  d <- data.frame(k = c("a", "b", "c", "d"), v = c(1, 40, 70, 100),
                  stringsAsFactors = FALSE)
  tbl <- gt::data_color(gt::gt(d, rowname_col = "k"), columns = "v",
                        palette = c("#FFF7EC", "#FC8D59", "#7F0000"))
  wb <- expect_styles_match_gt(tbl)

  fills <- vapply(c("B2", "B3", "B4", "B5"),
                  function(r) sheet_style(wb, r)$fill, character(1L))
  expect_equal(length(unique(fills)), 4L)
  colors <- vapply(c("B2", "B5"), function(r) sheet_style(wb, r)$color, character(1L))
  expect_equal(length(unique(colors)), 2L)
})

test_that("mixed tab_style locations keep their own values", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B"), k = c("r1", "r2", "r3"),
                  v = c(1, 2, 3), w = c(4, 5, 6), stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "k", groupname_col = "g")
  tbl <- gt::tab_header(tbl, title = "T", subtitle = "S")
  tbl <- gt::tab_spanner(tbl, label = "Sp", columns = c("v", "w"))
  tbl <- gt::tab_style(tbl, gt::cell_fill(color = "yellow"),
                       gt::cells_body(columns = "v", rows = 1))
  tbl <- gt::tab_style(tbl, gt::cell_text(color = "red", weight = "bold"),
                       gt::cells_body(columns = "v", rows = 2))
  tbl <- gt::tab_style(tbl, gt::cell_text(style = "italic"),
                       gt::cells_body(columns = "w", rows = 3))
  tbl <- gt::tab_style(tbl, gt::cell_fill(color = "#DDDDDD"),
                       gt::cells_column_labels(columns = "w"))
  tbl <- gt::tab_style(tbl, gt::cell_text(color = "blue"), gt::cells_stub())
  tbl <- gt::tab_style(tbl, gt::cell_fill(color = "#EEEEEE"),
                       gt::cells_title(groups = "title"))
  expect_styles_match_gt(tbl)
})

test_that("the last style wins where two target the same cell", {
  skip_no_gt()
  tbl <- gt::tab_style(small_gt(), gt::cell_fill(color = "red"),
                       gt::cells_body(columns = "n", rows = 1))
  tbl <- gt::tab_style(tbl, gt::cell_fill(color = "lime"),
                       gt::cells_body(columns = "n", rows = 1))
  wb <- expect_styles_match_gt(tbl)
  expect_equal(sheet_style(wb, "B2")$fill, "FF00FF00")
})

test_that("opt_stylize keeps its per region colours", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B"), k = c("r1", "r2", "r3"), v = 1:3,
                  stringsAsFactors = FALSE)
  tbl <- gt::opt_stylize(gt::gt(d, rowname_col = "k", groupname_col = "g"),
                         style = 3, color = "blue")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  ops <- gtxlsx_extract(tbl)$options
  lab_bg <- css_color(opt_chr(ops, "column_labels_background_color"))
  stub_bg <- css_color(opt_chr(ops, "stub_background_color"))
  expect_equal(sheet_style(wb, "B1")$fill, lab_bg)
  expect_equal(sheet_style(wb, "A3")$fill, stub_bg)
  # a dark header takes the light foreground, as gt's font-color() rule does
  expect_equal(sheet_style(wb, "B1")$color, "FFFFFFFF")
})

test_that("styles written at an offset land at the offset", {
  skip_no_gt()
  tbl <- gt::tab_style(small_gt(), gt::cell_fill(color = "yellow"),
                       gt::cells_body(columns = "n", rows = 2))
  expect_styles_match_gt(tbl, dims = "D5")
})

# The HTML writer has no gt style table to compare against, so the expectation
# is written into the fixture: every cell carries a distinct style and the
# sheet has to reproduce each one, not the first one repeated.

test_that("distinct inline styles are not collapsed onto their neighbours", {
  fills <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF")
  colors <- c("#000000", "#111111", "#222222", "#333333", "#444444", "#555555")
  tds <- paste0("<td style=\"background-color:", fills, ";color:", colors, "\">v",
                seq_along(fills), "</td>")
  html <- paste0("<table><tr>", paste0(tds[1:3], collapse = ""), "</tr><tr>",
                 paste0(tds[4:6], collapse = ""), "</tr></table>")

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")

  refs <- c("A1", "B1", "C1", "A2", "B2", "C2")
  for (k in seq_along(refs)) {
    got <- sheet_style(wb, refs[k])
    expect_equal(got$fill, sub("#", "FF", fills[k]), info = refs[k])
    expect_equal(got$color, sub("#", "FF", colors[k]), info = refs[k])
  }
})

test_that("class based styles keep one value per class", {
  html <- paste0(
    "<style>.a{background-color:#FF0000;font-weight:bold}",
    ".b{background-color:#00FF00;font-style:italic}",
    ".c{color:#0000FF;font-size:20px}</style>",
    "<table><tr><td class=\"a\">1</td><td class=\"b\">2</td><td class=\"c\">3</td></tr>",
    "<tr><td class=\"b\">4</td><td class=\"c\">5</td><td class=\"a\">6</td></tr></table>"
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")

  a <- list(fill = "FFFF0000", bold = TRUE, italic = FALSE)
  b <- list(fill = "FF00FF00", bold = FALSE, italic = TRUE)
  cc <- list(color = "FF0000FF", size = 15)
  for (ref in c("A1", "C2")) {
    got <- sheet_style(wb, ref)
    expect_equal(got[names(a)], a, info = ref)
  }
  for (ref in c("B1", "A2")) {
    got <- sheet_style(wb, ref)
    expect_equal(got[names(b)], b, info = ref)
  }
  for (ref in c("C1", "B2")) {
    got <- sheet_style(wb, ref)
    expect_equal(got[names(cc)], cc, info = ref)
  }
})

test_that("a per row style does not leak into other rows", {
  html <- paste0(
    "<table>",
    "<tr style=\"background-color:#EEEEEE\"><td>a</td><td>b</td></tr>",
    "<tr><td>c</td><td style=\"background-color:#FFCCCC\">d</td></tr>",
    "</table>"
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")

  expect_equal(sheet_style(wb, "A1")$fill, "FFEEEEEE")
  expect_equal(sheet_style(wb, "B1")$fill, "FFEEEEEE")
  expect_equal(sheet_style(wb, "A2")$fill, "-")
  expect_equal(sheet_style(wb, "B2")$fill, "FFFFCCCC")
})

test_that("weights follow the gt option rather than being forced", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B"), k = paste0("r", 1:3), v = c(1, 2, 3),
                  stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "k", groupname_col = "g")
  tbl <- gt::summary_rows(tbl, columns = "v", fns = list(tot = ~ sum(.x)))

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  # row_group.font.weight and the summary rows default to "initial", not bold
  expect_false(sheet_style(wb, "A2")$bold)
  expect_false(sheet_style(wb, "A5")$bold)

  bold <- gt::tab_options(tbl, row_group.font.weight = "bold")
  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  wb2 <- wb_add_gt(wb2, bold, dims = "A1")
  expect_true(sheet_style(wb2, "A2")$bold)
})

test_that("spanners carry the column label bottom border", {
  skip_no_gt()
  d <- data.frame(k = c("a", "b"), q1 = c(1, 2), q2 = c(3, 4),
                  stringsAsFactors = FALSE)
  tbl <- gt::tab_spanner(gt::gt(d, rowname_col = "k"), label = "2024",
                         columns = c("q1", "q2"))

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "B2")

  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  for (ref in c("C4", "D4")) {
    id <- as.integer(cc$c_s[cc$r == ref]) + 1L
    xf <- openxlsx2::xml_attr(st$cellXfs[[id]], "xf")[[1L]]
    bd <- st$borders[[as.integer(xf[["borderId"]]) + 1L]]
    expect_match(bd, "<bottom style=", fixed = TRUE, info = ref)
  }
})
