test_that("a plain table lands where it is told to", {
  skip_no_gt()
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, small_gt(), dims = "B2")

  df <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  expect_equal(rownames(df)[1L], "2")
  expect_true("alpha" %in% unlist(df))
})

test_that("a heading is merged across the table", {
  skip_no_gt()
  tbl <- gt::tab_header(small_gt(), title = gt::md("**T**"), subtitle = "S")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  merges <- unlist(wb$worksheets[[1L]]$mergeCells)
  expect_true(any(grepl("A1:", merges, fixed = TRUE)))
  # md() markup keeps its bold as a rich text run inside the cell
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  expect_match(cc$is[cc$r == "A1"], "<b")
})

test_that("no heading means no empty rows above the table", {
  skip_no_gt()
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, small_gt(), dims = "A1")
  expect_equal(rownames(openxlsx2::wb_to_df(wb, col_names = FALSE))[1L], "1")
})

test_that("tab_style becomes fills and fonts", {
  skip_no_gt()
  tbl <- gt::tab_style(
    small_gt(),
    style = list(gt::cell_fill(color = "lightblue"),
                 gt::cell_text(weight = "bold", color = "red")),
    locations = gt::cells_body(columns = "n", rows = 1)
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  s <- sheet_style(wb, "B2")
  expect_equal(s$fill, "FFADD8E6")
  expect_true(s$bold)
  expect_equal(s$color, "FFFF0000")
})

test_that("currency columns become numbers with a format", {
  skip_no_gt()
  tbl <- gt::fmt_currency(small_gt(), columns = "n", decimals = 2)
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  cc <- wb$worksheets[[1L]]$sheet_data$cc
  expect_equal(cc$v[cc$r == "B2"], "1234.5")
  fmts <- paste(unlist(wb$styles_mgr$styles$numFmts), collapse = " ")
  expect_match(fmts, "#,##0.00")
})

test_that("numeric = FALSE keeps everything as text", {
  skip_no_gt()
  tbl <- gt::fmt_currency(small_gt(), columns = "n", decimals = 2)
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1", numeric = FALSE)

  cc <- wb$worksheets[[1L]]$sheet_data$cc
  expect_equal(cc$c_t[cc$r == "B2"], "inlineStr")
})

test_that("row groups get their own heading rows", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B"), r = c("r1", "r2", "r3"), v = 1:3,
                  stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "r", groupname_col = "g")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  col <- openxlsx2::wb_to_df(wb, col_names = FALSE)[[1L]]
  expect_true(all(c("A", "B", "r1", "r3") %in% col))
})

test_that("spanners, footnotes and source notes are written", {
  skip_no_gt()
  tbl <- small_gt()
  tbl <- gt::tab_spanner(tbl, label = "Values", columns = c("n", "p"))
  tbl <- gt::tab_footnote(tbl, "a note",
                          locations = gt::cells_column_labels(columns = "n"))
  tbl <- gt::tab_source_note(tbl, "src")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  flat <- unlist(openxlsx2::wb_to_df(wb, col_names = FALSE))
  expect_true("Values" %in% flat)
  expect_true(any(grepl("a note", flat, fixed = TRUE)))
  expect_true("src" %in% flat)
})

test_that("summary rows are written below their group", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B", "B"), r = paste0("r", 1:4), v = c(1, 2, 3, 4),
                  stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "r", groupname_col = "g")
  tbl <- gt::summary_rows(tbl, columns = "v", fns = list(total = ~ sum(.x)))
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  expect_true("total" %in% openxlsx2::wb_to_df(wb, col_names = FALSE)[[1L]])
})

test_that("the foreground follows the background of each region", {
  skip_no_gt()
  tbl <- gt::tab_options(gt::tab_header(small_gt(), title = "T"),
                         column_labels.background.color = "#004D80")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  expect_equal(sheet_style(wb, "B2")$color, "FFFFFFFF")
})

test_that("generic font keywords are skipped", {
  skip_no_gt()
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, small_gt(), dims = "A1")
  expect_false(identical(sheet_style(wb, "A2")$name, "system-ui"))
})

test_that("column widths and the ignoredErrors block are written", {
  skip_no_gt()
  tbl <- gt::cols_width(small_gt(), n ~ gt::px(200))
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  expect_true(length(wb$worksheets[[1L]]$cols_attr) > 0L)

  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  wb2 <- wb_add_gt(wb2, small_gt(), dims = "A1", col_widths = NULL,
                   ignore_errors = FALSE)
  expect_equal(length(wb2$worksheets[[1L]]$cols_attr), 0L)
})

test_that("ignoredErrors entries always carry a sqref", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B"), r = paste0("r", 1:3), v = c(1.5, 2.5, 3.5),
                  stringsAsFactors = FALSE)
  tbl <- gt::summary_rows(
    gt::gt(d, rowname_col = "r", groupname_col = "g"),
    columns = "v", fns = list(total = ~ sum(.x))
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "B2")

  ie <- wb$worksheets[[1L]]$ignoredErrors
  if (length(ie) && nzchar(ie)) {
    n <- lengths(regmatches(ie, gregexpr("<ignoredError ", ie, fixed = TRUE)))
    refs <- lengths(regmatches(ie, gregexpr("sqref=\"", ie, fixed = TRUE)))
    expect_equal(n, refs)
  } else {
    succeed()
  }
})

test_that("bad input is rejected", {
  skip_no_gt()
  expect_error(wb_add_gt("not a workbook", small_gt()), "wbWorkbook")
  expect_error(gtxlsx_extract(data.frame(a = 1)), "gt_tbl")
})

test_that("row groups as a column merge downwards", {
  skip_no_gt()
  d <- data.frame(g = c("A", "A", "B"), r = paste0("r", 1:3), v = 1:3,
                  stringsAsFactors = FALSE)
  tbl <- gt::tab_options(gt::gt(d, rowname_col = "r", groupname_col = "g"),
                         row_group.as_column = TRUE)
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  expect_true(any(grepl(":A", unlist(wb$worksheets[[1L]]$mergeCells), fixed = TRUE)))
})

test_that("striping, indentation and hidden columns are handled", {
  skip_no_gt()
  tbl <- gt::opt_row_striping(small_gt())
  tbl <- gt::tab_stub_indent(tbl, rows = 2, indent = 3)
  tbl <- gt::cols_hide(tbl, columns = "p")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  expect_equal(ncol(openxlsx2::wb_to_df(wb, col_names = FALSE)), 2L)
})

test_that("hidden column labels leave the header out", {
  skip_no_gt()
  tbl <- gt::tab_options(small_gt(), column_labels.hidden = TRUE)
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  expect_false("n" %in% unlist(openxlsx2::wb_to_df(wb, col_names = FALSE)))
})

test_that("nested spanners occupy separate rows", {
  skip_no_gt()
  d <- data.frame(a = 1, `x.y.p` = 1, `x.y.q` = 2, check.names = FALSE)
  tbl <- gt::tab_spanner_delim(gt::gt(d), delim = ".")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  expect_true(nrow(openxlsx2::wb_to_df(wb, col_names = FALSE)) >= 3L)
})

test_that("a footnote on the title is marked", {
  skip_no_gt()
  tbl <- gt::tab_footnote(gt::tab_header(small_gt(), title = "T"), "note",
                          locations = gt::cells_title(groups = "title"))
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  expect_match(cc$is[cc$r == "A1"], "superscript")
})

test_that("per cell fills are not collapsed onto the whole column", {
  skip_no_gt()
  d <- data.frame(a = c("w", "x", "y", "z"), v = c(1, 2, 3, 4),
                  stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "a")
  tbl <- gt::data_color(tbl, columns = "v",
                        palette = c("#FFFFFF", "#FF0000"))
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  fills <- vapply(c("B2", "B3", "B4", "B5"),
                  function(ref) sheet_style(wb, ref)$fill, character(1L))
  expect_equal(length(unique(fills)), 4L)
})

test_that("a tab_style fill on one row stays on that row", {
  skip_no_gt()
  tbl <- gt::tab_style(
    small_gt(),
    style = gt::cell_fill(color = "yellow"),
    locations = gt::cells_body(columns = "n", rows = 2)
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  expect_equal(sheet_style(wb, "B3")$fill, "FFFFFF00")
  expect_false(identical(sheet_style(wb, "B2")$fill, "FFFFFF00"))
  expect_false(identical(sheet_style(wb, "B4")$fill, "FFFFFF00"))
})

test_that("a gt_group is written one table after another", {
  skip_no_gt()
  grp <- gt::gt_group(
    gt::tab_header(gt::gt(data.frame(a = c("x", "y"))), title = "First"),
    gt::tab_header(gt::gt(data.frame(a = c("p", "q", "r"))), title = "Second")
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, grp, dims = "A1")

  col <- openxlsx2::wb_to_df(wb, col_names = FALSE)[[1L]]
  expect_true(all(c("First", "Second", "x", "r") %in% col))
  # the blank row between them
  expect_true(any(is.na(col)))
})

test_that("gap controls the space between grouped tables", {
  skip_no_gt()
  grp <- gt::gt_group(gt::gt(data.frame(a = "x")), gt::gt(data.frame(a = "y")))

  tight <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), grp,
                     dims = "A1", gap = 0)
  loose <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet(), grp,
                     dims = "A1", gap = 3)

  expect_lt(nrow(openxlsx2::wb_to_df(tight, col_names = FALSE)),
            nrow(openxlsx2::wb_to_df(loose, col_names = FALSE)))
})

test_that("gt_split output is written as a group", {
  skip_no_gt()
  d <- data.frame(a = paste0("r", 1:6), stringsAsFactors = FALSE)
  sp <- gt::gt_split(gt::gt(d), row_every_n = 3)

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, sp, dims = "A1")

  # Take the expectation from what gt actually renders, not from the data it
  # was handed: gt 1.3.0.9000 leaves the second table of a split with stub row
  # numbers pointing past its own data, so its own HTML comes out empty too.
  tbls <- sp$gt_tbls
  n <- if (is.data.frame(tbls)) nrow(tbls) else length(tbls)
  want <- unlist(lapply(seq_len(n), function(i) {
    as.character(gtxlsx_extract(gt::grp_pull(sp, which = i))$body$a)
  }))
  want <- want[!is.na(want)]
  col <- openxlsx2::wb_to_df(wb, col_names = FALSE)[[1L]]

  expect_gt(n, 1L)
  expect_gt(length(want), 0L)
  expect_true(all(want %in% col))
})
