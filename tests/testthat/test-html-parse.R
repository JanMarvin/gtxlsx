test_that("elements, attributes and nesting are read", {
  d <- html_parse(paste0('<div id="w"><table class="t a">',
                         '<tr><td rowspan="2">x</td></tr></table></div>'))
  tbl <- nd_find_all(d, "table")
  expect_length(tbl, 1L)
  expect_equal(nd_classes(d, tbl[[1L]]), c("t", "a"))
  cell <- nd_find(d, tbl[[1L]], c("td", "th"))
  expect_equal(nd_attr(d, cell[[1L]], "rowspan"), "2")
  expect_equal(nd_inner(d, cell[[1L]]), "x")
  expect_true(is.na(nd_attr(d, cell[[1L]], "colspan")))
})

test_that("void elements do not swallow what follows", {
  d <- html_parse("<table><tr><td>a<br>b</td><td>c</td></tr></table>")
  cells <- nd_find_all(d, "td")
  expect_length(cells, 2L)
  expect_equal(nd_inner(d, cells[[1L]]), "a<br>b")
  expect_equal(nd_inner(d, cells[[2L]]), "c")
})

test_that("unquoted attributes and upper case tags are read", {
  d <- html_parse("<TABLE BORDER=2><TR><TD NOWRAP ALIGN=right>v</TD></TR></TABLE>")
  tbl <- nd_find_all(d, "table")[[1L]]
  expect_equal(nd_attr(d, tbl, "border"), "2")
  cell <- nd_find_all(d, "td")[[1L]]
  expect_equal(nd_attr(d, cell, "align"), "right")
  expect_equal(nd_attr(d, cell, "nowrap"), "")
})

test_that("unclosed cells and rows are closed by their successor", {
  d <- html_parse("<table><tr><td>a<td>b<tr><td>c</table>")
  expect_length(nd_find_all(d, "tr"), 2L)
  expect_length(nd_find_all(d, "td"), 3L)
})

test_that("comments and stray end tags are ignored", {
  d <- html_parse("<table><!-- note --><tr><td>a</td></span></tr></table>")
  expect_length(nd_find_all(d, "td"), 1L)
  expect_equal(nd_inner(d, nd_find_all(d, "td")[[1L]]), "a")
})

test_that("parents, siblings and text are available", {
  d <- html_parse("<div><p>one</p><table><tr><td>x</td></tr></table><p>two</p></div>")
  cell <- nd_find_all(d, "td")[[1L]]
  anc <- vapply(seq_along(node_ancestors(d, cell)),
                function(k) node_ancestors(d, cell)[[k]]$tag, character(1L))
  expect_equal(anc[1:3], c("tr", "table", "div"))

  tbl <- nd_find_all(d, "table")[[1L]]
  sib <- nd_sibling_index(d, tbl)
  expect_equal(sib$i, 2L)
  expect_equal(sib$n, 3L)
  expect_equal(nd_text(d, nd_find_all(d, "p")[[2L]]), "two")
})

test_that("entities survive to the text accessor", {
  d <- html_parse("<table><tr><td>a &amp; b</td></tr></table>")
  expect_equal(nd_text(d, nd_find_all(d, "td")[[1L]]), "a & b")
})

# The shapes below were checked against what xml2's HTML parser produces for
# the same markup, so that dropping it did not quietly change any of them.

grid_shape <- function(html) {
  d <- html_parse(html)
  tb <- nd_find_all(d, "table")
  if (!length(tb)) return("no table")
  g <- html_grid(d, tb[[1L]])
  if (is.null(g)) return("no rows")
  paste(vapply(g$cells, function(c0) {
    sprintf("%s@%d,%d[%dx%d]{%s}", c0$tag, c0$row, c0$col, c0$rowspan, c0$colspan,
            trimws(html_strip(nd_inner(d, c0$node))))
  }, character(1L)), collapse = " | ")
}

test_that("an unclosed cell ends where the next one begins", {
  expect_equal(grid_shape("<table><tr><td>a<td>b</tr><tr><td>c</table>"),
               "td@1,1[1x1]{a} | td@1,2[1x1]{b} | td@2,1[1x1]{c}")
})

test_that("a self closing cell is empty, not greedy", {
  expect_equal(grid_shape("<table><tr><td/><td>b</td></tr></table>"),
               "td@1,1[1x1]{} | td@1,2[1x1]{b}")
})

test_that("a quoted attribute may contain angle brackets", {
  expect_equal(grid_shape('<table><tr><td title="a>b">v</td></tr></table>'),
               "td@1,1[1x1]{v}")
  expect_equal(grid_shape('<table><tr><td title="a<b">v</td></tr></table>'),
               "td@1,1[1x1]{v}")
})

test_that("spans, junk span values and rowspan zero behave", {
  expect_equal(grid_shape('<table><tr><td colspan="abc">v</td><td>w</td></tr></table>'),
               "td@1,1[1x1]{v} | td@1,2[1x1]{w}")
  expect_equal(
    grid_shape(paste0("<table><tbody><tr><td rowspan=0>s</td><td>1</td></tr>",
                      "<tr><td>2</td></tr></tbody></table>")),
    "td@1,1[2x1]{s} | td@1,2[1x1]{1} | td@2,2[1x1]{2}"
  )
})

test_that("a nested table stays inside its own cell", {
  expect_equal(
    grid_shape("<table><tr><td>o<table><tr><td>i</td></tr></table></td></tr></table>"),
    "td@1,1[1x1]{oi}"
  )
})

test_that("mixed case tags and attributes without spaces are read", {
  expect_equal(grid_shape("<TaBlE><Tr><tD>v</Td></tR></TaBlE>"),
               "td@1,1[1x1]{v}")
  expect_equal(grid_shape('<table><tr><td colspan="2"id="x">v</td></tr></table>'),
               "td@1,1[1x2]{v}")
})

test_that("badly nested inline markup does not break the cell", {
  expect_equal(grid_shape("<table><tr><td><b>x</td></b></tr></table>"),
               "td@1,1[1x1]{x}")
})

test_that("a document wrapper and surrounding text are ignored", {
  expect_equal(
    grid_shape(paste0("<!DOCTYPE html><html><body>before",
                      "<table><tr><td>a</td></tr></table>after</body></html>")),
    "td@1,1[1x1]{a}"
  )
})
