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
  list(fill = fill, font = font, numfmt = numfmt,
       bold = grepl("<b", font, fixed = TRUE),
       italic = grepl("<i ", font, fixed = TRUE),
       color = sub(".*color rgb=\"([0-9A-F]{8})\".*", "\\1", font),
       name = sub('.*name val="([^"]+)".*', "\\1", font))
}
