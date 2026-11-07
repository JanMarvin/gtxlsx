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
  skip_no_gt_fn("tab_stub_indent")
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
  skip_no_gt_fn("data_color")
  skip_if_gt_cannot(gt::data_color(gt::gt(data.frame(v = 1:2)), columns = "v",
                                   palette = c("#FFFFFF", "#FF0000")))
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
  skip_no_gt_fn("gt_group", "grp_pull")
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
  skip_no_gt_fn("gt_group", "grp_pull")
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
  skip_no_gt_fn("gt_split", "grp_pull")
  d <- data.frame(a = paste0("r", 1:6), stringsAsFactors = FALSE)
  sp <- gt::gt_split(gt::gt(d), row_every_n = 3)

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, sp, dims = "A1")

  # Compare against what gt renders, not against the data it was handed: on
  # gt 1.3.0 gt_split() leaves the later tables unable to render their own
  # rows, and the sheet reproduces whatever gt built.
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

test_that("fmt_url and fmt_email produce working links", {
  skip_no_gt()
  # fmt_email() arrived after gt 0.10
  skip_if_not("fmt_email" %in% getNamespaceExports("gt"),
              "this gt has no fmt_email()")
  d <- data.frame(what = c("CRAN", "mail"),
                  url = c("https://cran.r-project.org", "a@b.org"),
                  stringsAsFactors = FALSE)
  tbl <- gt::fmt_email(gt::fmt_url(gt::gt(d), columns = "url", rows = 1),
                       columns = "url", rows = 2)

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = "A1")

  links <- unlist(wb$worksheets[[1L]]$hyperlinks)
  refs <- sub('.*ref="([A-Z]+[0-9]+)".*', "\\1", links)
  expect_setequal(refs, c("B2", "B3"))
})

test_that("a footnote without a location is written, unmarked, before the others", {
  skip_no_gt()
  tbl <- gt::gt(data.frame(a = c(1, 2), b = c("x", "y"))) |>
    gt::tab_footnote("with a location",
                     locations = gt::cells_column_labels(columns = a)) |>
    gt::tab_footnote("without a location")
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet("x"), tbl, dims = "A1")
  df <- openxlsx2::wb_to_df(wb, col_names = FALSE, skip_empty_rows = FALSE)
  notes <- as.character(df[[1L]])
  expect_true("without a location" %in% notes)
  expect_true(any(grepl("with a location", notes, fixed = TRUE)))
  expect_lt(match("without a location", notes), grep("^1 with", notes))
  only <- gt::gt(data.frame(a = 1)) |> gt::tab_footnote("only a note")
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet("x"), only, dims = "A1")
  df <- openxlsx2::wb_to_df(wb, col_names = FALSE, skip_empty_rows = FALSE)
  expect_true("only a note" %in% as.character(df[[1L]]))
})

test_that("tab_caption() is written above the table", {
  skip_if_not_installed("gt")
  tbl <- gt::tab_caption(gt::gt(data.frame(a = 1:2)), "My caption")
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet("x"), tbl)
  df <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  expect_equal(df[1L, 1L], "My caption")
  expect_equal(df[2L, 1L], "a")
})

test_that("summary rows on the top side follow the group heading", {
  skip_if_not_installed("gt")
  tbl <- gt::gt(data.frame(g = c("a", "a", "b"), v = c(1, 2, 3)), groupname_col = "g")
  tbl <- gt::summary_rows(tbl, columns = v, fns = list(tot = ~ sum(.x)), side = "top")
  tbl <- gt::grand_summary_rows(tbl, columns = v, fns = list(all = ~ sum(.x)), side = "top")
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet("x"), tbl)
  col1 <- openxlsx2::wb_to_df(wb, col_names = FALSE)[[1L]]
  expect_equal(col1[1:3], c(NA, "all", "a"))
  expect_equal(col1[4L], "tot")
})

test_that("cols_width() in pixels sets the column width", {
  skip_if_not_installed("gt")
  tbl <- gt::cols_width(gt::gt(data.frame(a = 1, b = 2)), a ~ gt::px(300))
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet("x"), tbl)
  w <- as.numeric(sub(".*width=\"([0-9.]+)\".*", "\\1", wb$worksheets[[1L]]$cols_attr))
  expect_gt(w[1L], 30)
  expect_lt(w[2L], 30)
})

test_that("a nanoplot leaves no svg or css text in the cell", {
  skip_if_not_installed("gt")
  skip_no_gt_fn("cols_nanoplot")
  tbl <- gt::cols_nanoplot(gt::gt(data.frame(v = "1 2 3 4")), columns = v)
  wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet("x"), tbl)
  txt <- unlist(openxlsx2::wb_to_df(wb, col_names = FALSE))
  expect_false(any(grepl("font-family|stroke", txt)))
})
