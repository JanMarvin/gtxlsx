skip_no_gt <- function() testthat::skip_if_not_installed("gt")

small_gt <- function() {
  gt::gt(data.frame(
    item = c("alpha", "beta", "gamma"),
    n = c(1234.5, 67.89, 0.5),
    p = c(0.12, 0.5, 0.98),
    stringsAsFactors = FALSE
  ), rowname_col = "item")
}

sheet_style <- function(wb, ref, sheet = 1L) {
  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[sheet]]$sheet_data$cc
  s <- cc$c_s[cc$r == ref]
  if (!length(s) || is.na(s)) return(NULL)
  id <- as.integer(s)
  xf <- openxlsx2::xml_attr(st$cellXfs[[id + 1L]], "xf")[[1L]]
  fill <- "-"
  if ("fillId" %in% names(xf)) {
    a <- openxlsx2::xml_attr(st$fills[[as.integer(xf[["fillId"]]) + 1L]],
                             "fill", "patternFill", "fgColor")
    if (length(a) && !is.null(a[[1L]][["rgb"]])) fill <- a[[1L]][["rgb"]]
  }
  font <- st$fonts[[as.integer(xf[["fontId"]]) + 1L]]
  numfmt <- if ("numFmtId" %in% names(xf)) xf[["numFmtId"]] else NA_character_
  size <- suppressWarnings(as.numeric(sub('.*<sz val="([0-9.]+)".*', "\\1", font)))
  list(fill = fill, font = font, numfmt = numfmt, size = size,
       bold = grepl("<b", font, fixed = TRUE),
       italic = grepl("<i ", font, fixed = TRUE),
       color = sub(".*color rgb=\"([0-9A-F]{8})\".*", "\\1", font),
       name = sub('.*name val="([^"]+)".*', "\\1", font))
}


# Walk every style gt declared and check the cell it lands on carries it.
# Later styles win over earlier ones for the same cell, as in gt itself.
expect_styles_match_gt <- function(tbl, dims = "A1") {
  g <- gtxlsx_extract(tbl)
  th <- gtxlsx_theme(g$options)
  rc <- openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  p <- gtxlsx_plan(g, th, min(rc$row), min(rc$col))
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = dims)

  st <- g$styles
  want <- list()
  for (i in seq_len(nrow(st))) {
    tgt <- style_targets(as.list(st[i, ]), g, p)
    if (is.null(tgt)) next
    attrs <- style_attrs(st$styles[[i]], th)
    if (!length(attrs)) next
    for (rr in tgt$rows) {
      for (jj in tgt$cols) {
        ref <- ref_of(rr, jj)
        want[[ref]][names(attrs)] <- attrs
      }
    }
  }
  testthat::expect_gt(length(want), 0L)

  for (ref in names(want)) {
    got <- sheet_style(wb, ref)
    w <- want[[ref]]
    if (!is.null(w$fill)) testthat::expect_equal(got$fill, w$fill, info = ref)
    if (!is.null(w$color)) testthat::expect_equal(got$color, w$color, info = ref)
    if (!is.null(w$bold)) testthat::expect_equal(got$bold, w$bold, info = ref)
    if (!is.null(w$italic)) testthat::expect_equal(got$italic, w$italic, info = ref)
    if (!is.null(w$size)) testthat::expect_equal(got$size, w$size, info = ref)
    if (!is.null(w$font)) testthat::expect_equal(got$name, w$font, info = ref)
  }
  invisible(wb)
}
