suppressWarnings(suppressMessages({library(gtxlsx); library(xml2)}))
gx <- function(n) getFromNamespace(n, "gtxlsx")

# shape of the first table as my parser sees it
mine <- function(h) {
  d <- gx("html_parse")(h)
  tb <- gx("nd_find_all")(d, "table")
  if (!length(tb)) return("no table")
  g <- tryCatch(gx("html_grid")(d, tb[[1L]]), error = function(e) NULL)
  if (is.null(g)) return("grid error")
  paste(vapply(g$cells, function(c0) sprintf("%s@%d,%d[%dx%d]{%s}", c0$tag,
    c0$row, c0$col, c0$rowspan, c0$colspan,
    trimws(gsub("\\s+", " ", gx("html_strip")(gx("nd_inner")(d, c0$node))))),
    character(1L)), collapse = " | ")
}

# the same shape via xml2, laid out the same way
theirs <- function(h) {
  doc <- read_html(h)
  tb <- xml_find_all(doc, "//table")
  if (!length(tb)) return("no table")
  tbl <- tb[[1L]]
  sect <- function(p) { n <- xml_find_all(tbl, p); if (length(n)) as.list(n) else list() }
  rows <- c(sect("./thead/tr"), sect("./tbody/tr"), sect("./tr"), sect("./tfoot/tr"))
  n <- length(rows); if (!n) return("no rows")
  taken <- matrix(FALSE, n, 64L); out <- character(0)
  for (i in seq_len(n)) {
    kids <- xml_children(rows[[i]])
    kids <- kids[tolower(xml_name(kids)) %in% c("td", "th")]
    j <- 1L
    for (k in seq_along(kids)) {
      nd <- kids[[k]]
      while (j <= ncol(taken) && taken[i, j]) j <- j + 1L
      cs <- suppressWarnings(as.integer(xml_attr(nd, "colspan"))); if (is.na(cs)) cs <- 1L
      rs <- suppressWarnings(as.integer(xml_attr(nd, "rowspan"))); if (is.na(rs)) rs <- 1L
      if (rs == 0L) rs <- n - i + 1L
      taken[seq.int(i, min(i + rs - 1L, n)), seq.int(j, j + cs - 1L)] <- TRUE
      inner <- sub("^<[^>]*>", "", as.character(nd)); inner <- sub("</[a-zA-Z0-9]+>\\s*$", "", inner)
      out <- c(out, sprintf("%s@%d,%d[%dx%d]{%s}", tolower(xml_name(nd)), i, j,
                            min(rs, n - i + 1L), cs,
                            trimws(gsub("\\s+", " ", gx("html_strip")(inner)))))
      j <- j + cs
    }
  }
  paste(out, collapse = " | ")
}

cases <- list(
  plain            = "<table><tr><td>a</td><td>b</td></tr></table>",
  unclosed_td      = "<table><tr><td>a<td>b</tr><tr><td>c</table>",
  no_tbody         = "<table><tr><td>a</td></tr></table>",
  void_br          = "<table><tr><td>a<br>b</td></tr></table>",
  void_img         = "<table><tr><td><img src='x.png'>t</td></tr></table>",
  self_closed      = "<table><tr><td/><td>b</td></tr></table>",
  upper_unquoted   = "<TABLE BORDER=2><TR><TD NOWRAP ALIGN=right>v</TD></TR></TABLE>",
  single_quotes    = "<table><tr><td class='a b' colspan='2'>v</td></tr></table>",
  gt_in_attr       = '<table><tr><td title="a>b">v</td></tr></table>',
  spaced_attrs     = "<table><tr><td   colspan = \"2\"  >v</td></tr></table>",
  comment          = "<table><!-- c --><tr><td>a</td></tr></table>",
  stray_end        = "<table><tr><td>a</td></span></tr></table>",
  nested_table     = "<table><tr><td>o<table><tr><td>i</td></tr></table></td></tr></table>",
  tfoot_first      = "<table><tfoot><tr><td>f</td></tr></tfoot><tbody><tr><td>b</td></tr></tbody></table>",
  rowspan_zero     = "<table><tbody><tr><td rowspan=0>s</td><td>1</td></tr><tr><td>2</td></tr></tbody></table>",
  caption          = "<table><caption>C</caption><tr><td>a</td></tr></table>",
  entities         = "<table><tr><td>a &amp; b &#x2014; c&nbsp;d</td></tr></table>",
  no_close_table   = "<table><tr><td>a</td></tr>",
  text_around      = "before<table><tr><td>a</td></tr></table>after",
  dup_attr         = "<table><tr><td class='a' class='b'>v</td></tr></table>",
  empty_cells      = "<table><tr><td></td><td>b</td></tr></table>",
  newline_in_tag   = "<table><tr><td\n  colspan=\"2\">v</td></tr></table>",
  deep_wrapper     = "<table><tr><td><div><p><span>v</span></p></div></td></tr></table>",
  th_and_td        = "<table><tr><th>h</th><td>d</td></tr></table>",
  colgroup         = "<table><colgroup><col><col span=2></colgroup><tr><td>a</td><td>b</td><td>c</td></tr></table>",
  doctype          = "<!DOCTYPE html><html><body><table><tr><td>a</td></tr></table></body></html>",
  uppercase_close  = "<table><tr><TD>a</Td></tr></table>",
  attr_no_space    = "<table><tr><td colspan=\"2\"id=\"x\">v</td></tr></table>",
  script_gt        = "<table><tr><td>a</td></tr></table><script>if (a<b) {}</script>",
  attr_lt          = '<table><tr><td title="a<b">v</td></tr></table>',
  empty_table      = "<table></table>",
  only_thead       = "<table><thead><tr><th>h</th></tr></thead></table>",
  nested_unclosed  = "<table><tr><td>o<table><tr><td>i</tr></table></td></tr></table>",
  multi_space      = "<table>\n  <tr>\n    <td>  a  </td>\n  </tr>\n</table>",
  bad_nesting      = "<table><tr><td><b>x</td></b></tr></table>",
  numeric_entity   = "<table><tr><td>&#8364;5</td></tr></table>",
  colspan_junk     = "<table><tr><td colspan=\"abc\">v</td><td>w</td></tr></table>",
  rowspan_big      = "<table><tr><td rowspan=\"9\">s</td><td>1</td></tr><tr><td>2</td></tr></table>",
  tag_case_mix     = "<TaBlE><Tr><tD>v</Td></tR></TaBlE>",
  attr_empty_val   = '<table><tr><td class="">v</td></tr></table>',
  slash_in_attr    = '<table><tr><td title="a/b">v</td></tr></table>',
  unicode          = "<table><tr><td>caf\u00e9 \u2014 na\u00efve</td></tr></table>",
  last             = "<table><tr><td>z</td></tr></table>"
)

bad <- 0L
for (nm in names(cases)) {
  a <- tryCatch(mine(cases[[nm]]), error = function(e) paste("ERR:", conditionMessage(e)))
  b <- tryCatch(theirs(cases[[nm]]), error = function(e) paste("ERR:", conditionMessage(e)))
  if (identical(a, b)) {
    cat(sprintf("%-16s same\n", nm))
  } else {
    bad <- bad + 1L
    cat(sprintf("%-16s DIFF\n   mine : %s\n   xml2 : %s\n", nm, a, b))
  }
}
cat("\ndifferences:", bad, "of", length(cases), "\n")
