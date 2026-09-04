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
  html <- paste0("<table><tr><td>outer</td>",
                 "<td><table><tr><td>inner</td></tr></table></td></tr></table>")
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
                 "<table><tbody class=\"body\"><tr><td>x</td></tr></tbody></table>")
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
                 "<tr><td><font color=\"red\">a</font></td>",
                 "<td>b</td><td>c</td></tr></table>")
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
  id <- as.integer(cc$c_s[cc$r == "A1"]) + 1L
  xf <- openxlsx2::xml_attr(st$cellXfs[[id]], "xf")[[1L]]
  bd <- st$borders[[as.integer(xf[["borderId"]]) + 1L]]
  expect_match(bd, "mediumDashed")
  expect_match(bd, "FF808080")
})

test_that("an already parsed document can be passed straight in", {
  doc <- html_parse("<table><tr><td>x</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, doc, dims = "A1")
  expect_equal(openxlsx2::wb_to_df(wb, col_names = FALSE)[1L, 1L], "x")
})

test_that("a table without rows is refused", {
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  expect_error(wb_add_html(wb, "<table><caption>c</caption></table>"), "no rows")
})

test_that("a border on many rows lands on every one of them", {
  # openxlsx2 treats a range as a block, so a bottom border applied to A2:A4 at
  # once would only reach A4
  rows <- paste0("<tr><td>a", 1:4, "</td><td>b", 1:4, "</td></tr>", collapse = "")
  html <- paste0("<style>td{border-bottom:1px solid #888}</style><table>",
                 rows, "</table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")

  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  for (ref in c("A1", "A2", "A3", "A4", "B1", "B4")) {
    id <- as.integer(cc$c_s[cc$r == ref]) + 1L
    xf <- openxlsx2::xml_attr(st$cellXfs[[id]], "xf")[[1L]]
    bd <- st$borders[[as.integer(xf[["borderId"]]) + 1L]]
    expect_match(bd, "<bottom style=", fixed = TRUE, info = ref)
  }
})

test_that("a left border on many rows lands on every one of them", {
  rows <- paste0("<tr><td>a", 1:3, "</td></tr>", collapse = "")
  html <- paste0("<style>td{border-left:1px solid #888}</style><table>",
                 rows, "</table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")

  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  for (ref in c("A1", "A2", "A3")) {
    id <- as.integer(cc$c_s[cc$r == ref]) + 1L
    xf <- openxlsx2::xml_attr(st$cellXfs[[id]], "xf")[[1L]]
    bd <- st$borders[[as.integer(xf[["borderId"]]) + 1L]]
    expect_match(bd, "<left style=", fixed = TRUE, info = ref)
  }
})

test_that("a child combinator does not match a distant ancestor", {
  # "div > td" must not reach a cell whose parent is a tr
  html <- paste0("<style>div > td{background-color:#ff0000}",
                 "tr > td{background-color:#00ff00}</style>",
                 "<div><table><tr><td>x</td></tr></table></div>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "FF00FF00")
})

test_that("a descendant combinator reaches through any depth", {
  html <- paste0("<style>div td{background-color:#0000ff}</style>",
                 "<div><table><tr><td>x</td></tr></table></div>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "FF0000FF")
})

test_that("selector order is respected, not just membership", {
  # "tfoot thead td" names real ancestors in an impossible order
  html <- paste0("<style>tfoot thead td{background-color:#ff0000}</style>",
                 "<table><thead><tr><td>h</td></tr></thead>",
                 "<tfoot><tr><td>f</td></tr></tfoot></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "A1")$fill, "-")
  expect_equal(sheet_style(wb, "A2")$fill, "-")
})

test_that("sibling combinators are skipped rather than guessed", {
  html <- paste0("<style>th + td{background-color:#ff0000}</style>",
                 "<table><tr><th>h</th><td>v</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")
  expect_equal(sheet_style(wb, "B1")$fill, "-")
})

test_that("a table level rule is found even when the stylesheet scopes it", {
  # gt scopes its rules under a wrapper div; looking the table rule up without
  # the table's own ancestors finds nothing and the font falls back
  html <- paste0("<style>#wrap .tbl{font-family:Georgia;font-size:20px}</style>",
                 "<div id=\"wrap\"><table class=\"tbl\"><tr><td>x</td></tr>",
                 "</table></div>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")

  got <- sheet_style(wb, "A1")
  expect_equal(got$name, "Georgia")
  expect_equal(got$size, 15)
})

test_that("markup that is not well formed XML still parses", {
  # unclosed void elements, unquoted attributes and missing end tags are all
  # legal HTML and all rejected by an XML parser
  html <- paste0("<TABLE BORDER=2><TR><TD NOWRAP>a<br>b<TD>c</TR>",
                 "<TR><TD>d<TD>e</TR></TABLE>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")

  df <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  expect_equal(dim(df), c(2L, 2L))
  expect_equal(df[1L, 1L], "a\nb")
  expect_equal(df[2L, 2L], "e")
})

test_that("an anchor in a cell becomes a hyperlink", {
  html <- paste0("<table><tr>",
                 "<td><a href=\"https://example.org/t\">Table 17-10-0005</a></td>",
                 "<td><a href=\"#fn1\">1</a></td>",
                 "<td><a href='mailto:a@b.org'>mail</a></td>",
                 "<td>plain</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  # the page fragment is reported, as it cannot be followed from a workbook
  expect_warning(wb <- wb_add_html(wb, html, dims = "A1"), "source page")

  # the anchor text is what the cell shows
  df <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  expect_equal(as.character(df[1L, 1L]), "Table 17-10-0005")

  links <- unlist(wb$worksheets[[1L]]$hyperlinks)
  refs <- sub('.*ref="([A-Z]+[0-9]+)".*', "\\1", links)
  # A1 and C1 get one; a bare page fragment and a plain cell do not
  expect_setequal(refs, c("A1", "C1"))
})

test_that("hrefs are read whatever the quoting", {
  expect_equal(html_href('<a href="https://x.org/a">t</a>'), "https://x.org/a")
  expect_equal(html_href("<a class='c' href='https://y.org/b'>t</a>"),
               "https://y.org/b")
  # a bare page fragment is not a destination in a workbook
  expect_true(is.na(html_href("<a href=#fn1>1</a>")))
  expect_equal(html_href('<a href="https://z.org/?a=1&amp;b=2">t</a>'),
               "https://z.org/?a=1&b=2")
  expect_true(is.na(html_href("plain text")))
  expect_true(is.na(html_href(NA_character_)))
})

test_that("anything that turns into HTML can be handed straight over", {
  # What rvest and xml2 hand back is accepted because they carry an
  # as.character() method, which is the contract this relies on. A stub with
  # the same method exercises that path without depending on either.
  page <- paste0("<html><body><h2>Report</h2><table>",
                 "<tr><th>region</th><th>value</th></tr>",
                 "<tr><td>north</td><td>1,204.50</td></tr></table></body></html>")
  scraped <- structure(list(doc = page), class = "stub_html_node")
  registerS3method("as.character", "stub_html_node",
                   function(x, ...) x$doc, envir = environment())

  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, scraped, dims = "A1")
  expect_equal(dim(openxlsx2::wb_to_df(wb, col_names = FALSE)), c(2L, 2L))

  # and the document this package parses itself round trips the same way
  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  wb2 <- wb_add_html(wb2, html_parse(page), dims = "A1")
  expect_equal(dim(openxlsx2::wb_to_df(wb2, col_names = FALSE)), c(2L, 2L))
})

test_that("a footnote anchor does not hide the real link beside it", {
  # Statistics Canada puts a footnote marker and a map link in the same cell:
  # the marker only points at a fragment of its own page, so the map wins
  html <- paste0(
    "<table><tr>",
    "<th>Nunavut<sup><a href=\"#Footnote5\">5</a></sup> ",
    "<a href=\"https://example.gc.ca/map?v={'a':['1']}&amp;b=2\">(map)</a></th>",
    "<td>41,798</td></tr>",
    "<tr><th>Yukon <a href=\"https://example.gc.ca/yk\">(map)</a></th>",
    "<td>48,089</td></tr></table>"
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  expect_warning(wb <- wb_add_html(wb, html, dims = "A1"), "source page")

  links <- unlist(wb$worksheets[[1L]]$hyperlinks)
  expect_length(links, 2L)

  targets <- sub('.*Target="([^"]*)".*', "\\1", unlist(wb$worksheets_rels[[1L]]))
  expect_true(any(grepl("map?v=", targets, fixed = TRUE)))
  expect_false(any(grepl("Footnote", targets, fixed = TRUE)))
})

test_that("rows without cells do not become blank sheet rows", {
  html <- paste0("<table><tbody>",
                 "<tr id=\"bufferRowTop\" style=\"height: 0px\"></tr>",
                 "<tr><td>a</td></tr>",
                 "<tr><td>b</td></tr>",
                 "<tr id=\"bufferRowBottom\" style=\"height: 0px\"></tr>",
                 "</tbody></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_html(wb, html, dims = "A1")

  df <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  expect_equal(nrow(df), 2L)
  expect_equal(as.character(df[[1L]]), c("a", "b"))

  # a row whose cells are merely empty is still a row
  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  wb2 <- wb_add_html(wb2, "<table><tr><td>a</td></tr><tr><td></td></tr></table>",
                     dims = "A1")
  df2 <- openxlsx2::wb_to_df(wb2, col_names = FALSE)
  expect_equal(nrow(df2), 2L)
})

test_that("links that cannot be written are reported once", {
  # a cell can hold one hyperlink, so anything past the first is lost
  two <- paste0("<table><tr><td>",
                "<a href=\"https://a.org\">a</a> and <a href=\"https://b.org\">b</a>",
                "</td></tr></table>")
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  expect_warning(wb_add_html(wb, two, dims = "A1"), "one hyperlink")

  # a link into the page the table came from has no destination in a workbook
  frag <- paste0("<table><tr><td>Nunavut<a href=\"#Footnote5\">5</a> ",
                 "<a href=\"https://a.org/map\">(map)</a></td></tr></table>")
  wb2 <- openxlsx2::wb_workbook()$add_worksheet()
  expect_warning(wb2 <- wb_add_html(wb2, frag, dims = "A1"), "source page")
  # the usable link is still written
  expect_length(unlist(wb2$worksheets[[1L]]$hyperlinks), 1L)

  # one link, one cell, nothing to say
  ok <- "<table><tr><td><a href=\"https://a.org\">a</a></td></tr></table>"
  wb3 <- openxlsx2::wb_workbook()$add_worksheet()
  expect_no_warning(wb_add_html(wb3, ok, dims = "A1"))
})
