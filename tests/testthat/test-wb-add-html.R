simple_html <- paste0(
  "<style>th{background-color:#204060;color:#fff}",
  "td.num{text-align:right}</style>",
  "<table><tr><th>a</th><th>b</th></tr>",
  "<tr><td>x</td><td class=\"num\">1,204.50</td></tr></table>"
)

test_that("a stylesheet reaches the cells", {
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, simple_html, dims = "A1")

  expect_equal(sheet_style(wb, "A1")$fill, "FF204060")
  expect_equal(sheet_style(wb, "A1")$color, "FFFFFFFF")
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  expect_equal(cc$v[cc$r == "B2"], "1204.5")
})

test_that("colspan and rowspan become merges", {
  html <- paste0("<table><tr><th rowspan=\"2\">k</th><th colspan=\"2\">pair</th></tr>",
                 "<tr><th>a</th><th>b</th></tr>",
                 "<tr><td>1</td><td>2</td><td>3</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  merges <- unlist(wb$worksheets[[1L]]$mergeCells)
  expect_true(any(grepl("A1:A2", merges, fixed = TRUE)))
  expect_true(any(grepl("B1:C1", merges, fixed = TRUE)))
})

test_that("rowspan zero stops at the end of its section", {
  html <- paste0("<table><tfoot><tr><td colspan=\"3\">f</td></tr></tfoot>",
                 "<tbody><tr><td rowspan=\"0\">s</td><td>1</td><td>2</td></tr>",
                 "<tr><td>3</td><td>4</td></tr></tbody></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(ncol(openxlsx2::wb_to_df(wb, col_names = FALSE)), 3L)
})

test_that("tfoot is written last whatever its place in the source", {
  html <- paste0("<table><tfoot><tr><td>foot</td></tr></tfoot>",
                 "<tbody><tr><td>body</td></tr></tbody></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(openxlsx2::wb_to_df(wb, col_names = FALSE)[[1L]], c("body", "foot"))
})

test_that("a nested table does not leak rows into its parent", {
  html <- "<table><tr><td>outer</td><td><table><tr><td>inner</td></tr></table></td></tr></table>"
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(nrow(openxlsx2::wb_to_df(wb, col_names = FALSE)), 1L)
})

test_that("presentational attributes are honoured", {
  html <- paste0("<table border=\"1\"><tr bgcolor=\"#336699\">",
                 "<th align=\"left\">h</th></tr>",
                 "<tr><td bgcolor=\"#ffe4e1\" align=\"right\">v</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "FF336699")
  expect_equal(sheet_style(wb, "A2")$fill, "FFFFE4E1")
})

test_that("descendant rules do not spill onto every cell", {
  html <- paste0("<style>.foot td{font-style:italic}</style>",
                 "<table><tbody><tr><td>plain</td></tr></tbody>",
                 "<tfoot class=\"foot\"><tr><td>note</td></tr></tfoot></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_false(sheet_style(wb, "A1")$italic)
  expect_true(sheet_style(wb, "A2")$italic)
})

test_that("structural pseudo-classes are skipped, not applied everywhere", {
  html <- paste0("<style>tr:nth-child(even) td{background-color:#ff0000}</style>",
                 "<table><tr><td>a</td></tr><tr><td>b</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "-")
  expect_equal(sheet_style(wb, "A2")$fill, "-")
})

test_that("important declarations win", {
  html <- paste0("<style>td{color:red !important}td{color:blue}</style>",
                 "<table><tr><td>x</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$color, "FFFF0000")
})

test_that("nested rules and :is() are resolved", {
  html <- paste0("<style>table{ .body :is(td,th){background-color:#00ff00} }</style>",
                 "<table class=\"body\"><tr><td>x</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "FF00FF00")
})

test_that("a wrapper chain carries its styling into the cell", {
  html <- paste0("<style>.c{background-color:#ffff00}",
                 ".p{background-color:transparent}.s{font-weight:bold}</style>",
                 "<table><tr><td class=\"c\"><p class=\"p\">",
                 "<span class=\"s\">v</span></p></td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "FFFFFF00")
  expect_true(sheet_style(wb, "A1")$bold)
})

test_that("caption and neighbouring blocks become rows", {
  html <- paste0("<div><div>Title</div><table><caption>Cap</caption>",
                 "<tr><td>v</td></tr></table><div>Note</div></div>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  flat <- openxlsx2::wb_to_df(wb, col_names = FALSE)[[1L]]
  expect_true(all(c("Title", "Cap", "v", "Note") %in% flat))

  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  wb2 <- wb_add_html(wb2, html, dims = "A1", context = FALSE)
  expect_false("Note" %in% openxlsx2::wb_to_df(wb2, col_names = FALSE)[[1L]])
})

test_that("nbsp only cells are written empty", {
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, "<table><tr><td>&nbsp;</td><td>x</td></tr></table>", dims = "A1")
  v <- openxlsx2::wb_to_df(wb, col_names = FALSE)[1L, 1L]
  expect_true(is.na(v) || !nzchar(trimws(v)))
})

test_that("text-transform and rotation are applied", {
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, paste0("<style>td{text-transform:uppercase;",
                               "transform:rotate(-90deg)}</style>",
                               "<table><tr><td>hi</td></tr></table>"), dims = "A1")
  expect_equal(openxlsx2::wb_to_df(wb, col_names = FALSE)[1L, 1L], "HI")
  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  al <- openxlsx2::xml_attr(st$cellXfs[[as.integer(cc$c_s[cc$r == "A1"]) + 1L]],
                            "xf", "alignment")
  expect_equal(al[[1L]][["textRotation"]], "90")
})

test_that("widths, a file path and which are handled", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)
  writeLines(paste0("<table><tr><td>one</td></tr></table>",
                    "<table><tr><td>two</td></tr></table>"), f)

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, f, dims = "A1", which = 2L, col_widths = 12)
  expect_equal(openxlsx2::wb_to_df(wb, col_names = FALSE)[1L, 1L], "two")
  expect_true(length(wb$worksheets[[1L]]$cols_attr) > 0L)
})

test_that("bad input is rejected", {
  expect_error(wb_add_html("nope", "<table><tr><td>x</td></tr></table>"), "wbWorkbook")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  expect_error(wb_add_html(wb, "<p>no table here</p>"), "no <table>")
  expect_error(wb_add_html(wb, "<table><tr><td>x</td></tr></table>", which = 5L),
               "past the last table")
})

test_that("lt input is validated", {
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  expect_error(wb_add_lt(wb, data.frame(a = 1)), "lt")
})

test_that("colgroup widths and legacy font tags are read", {
  html <- paste0("<table><colgroup><col style=\"width:200px\">",
                 "<col span=\"2\" width=\"80\"></colgroup>",
                 "<tr><td><font color=\"red\">a</font></td><td>b</td><td>c</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_true(length(wb$worksheets[[1L]]$cols_attr) > 0L)
})

test_that("row and section rules reach their cells", {
  html <- paste0("<style>thead td{font-weight:bold}</style>",
                 "<table><thead><tr style=\"background-color:#eeeeee\">",
                 "<td>h</td></tr></thead><tbody><tr><td>v</td></tr></tbody></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "FFEEEEEE")
  expect_true(sheet_style(wb, "A1")$bold)
  expect_false(sheet_style(wb, "A2")$bold)
})

test_that("@media rules and hsl colours are handled", {
  html <- paste0("<style>@media screen{td{background-color:hsl(0,100%,50%)}}</style>",
                 "<table><tr><td>x</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "FFFF0000")
})

test_that("borders from shorthand and per side rules are drawn", {
  html <- paste0("<style>td{border:1px solid #808080;border-bottom:2px dashed red}",
                 "</style><table><tr><td>x</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  xf <- openxlsx2::xml_attr(st$cellXfs[[as.integer(cc$c_s[cc$r == "A1"]) + 1L]], "xf")[[1L]]
  bd <- st$borders[[as.integer(xf[["borderId"]]) + 1L]]
  expect_match(bd, "mediumDashed")
  expect_match(bd, "FF808080")
})

test_that("an xml_document can be passed straight in", {
  doc <- xml2::read_html("<table><tr><td>x</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, doc, dims = "A1")
  expect_equal(openxlsx2::wb_to_df(wb, col_names = FALSE)[1L, 1L], "x")
})

test_that("a table without rows is refused", {
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  expect_error(wb_add_html(wb, "<table><caption>c</caption></table>"), "no rows")
})
