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

css_regions <- function(tbl, dims = "A1") {
  g <- gtxlsx_extract(tbl)
  th <- gtxlsx_theme(g$options)
  rc <- openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  p <- gtxlsx_plan(g, th, min(rc$row), min(rc$col))
  wb <- openxlsx2::wb_workbook()$add_worksheet()
  wb <- wb_add_gt(wb, tbl, dims = dims)

  html <- suppressWarnings(as.character(gt::as_raw_html(tbl, inline_css = FALSE)))
  doc <- xml2::read_html(html)
  rules <- parse_stylesheet(doc)

  body1 <- p$body_row[1L]
  spec <- list(
    list("gt_title", "th", 1L, 0L),
    list("gt_subtitle", "th", 2L, 0L),
    list("gt_column_spanner", "span", p$level_row[[1L]], 1L),
    list("gt_col_heading", "th", p$label_row, 1L),
    list("gt_group_heading", "th", p$group_head$row[1L], 0L),
    list("gt_stub", "th", body1, 0L),
    list("gt_row", "td", body1, 1L),
    list("gt_summary_row", "td", p$summaries[[1L]]$rows[1L], 1L),
    list("gt_sourcenote", "td", p$source_rows[1L], 0L),
    list("gt_footnote", "td", p$footnote_rows[1L], 0L)
  )

  out <- list()
  for (s in spec) {
    if (length(s[[3L]]) != 1L || is.na(s[[3L]])) next
    decl <- css_for(rules, s[[2L]], s[[1L]], NA_character_)
    if (!length(decl)) next
    out[[s[[1L]]]] <- list(
      want = decl_to_rec(decl, th$base_px),
      ref = ref_of(s[[3L]], min(rc$col) + s[[4L]]),
      wb = wb
    )
  }
  out
}

expect_css_matches_gt <- function(tbl, dims = "A1") {
  regions <- css_regions(tbl, dims)
  testthat::expect_gt(length(regions), 5L)

  for (nm in names(regions)) {
    r <- regions[[nm]]
    got <- sheet_style(r$wb, r$ref)
    want <- r$want
    lbl <- paste0(nm, " at ", r$ref)

    # gt writes an explicit white background where we simply leave the cell
    # unfilled; on a default sheet those render the same
    if (!is.null(want$fill) && !identical(want$fill, "FFFFFFFF")) {
      testthat::expect_equal(got$fill, want$fill, info = lbl)
    }
    if (!is.null(want$bold)) testthat::expect_equal(got$bold, want$bold, info = lbl)
    if (!is.null(want$italic)) {
      testthat::expect_equal(got$italic, want$italic, info = lbl)
    }
    if (!is.null(want$color)) testthat::expect_equal(got$color, want$color, info = lbl)
    if (!is.null(want$size)) testthat::expect_equal(got$size, want$size, info = lbl)
  }
  invisible(regions)
}

full_tbl <- function() {
  d <- data.frame(g = c("A", "A", "B"), k = paste0("r", 1:3), v = c(1, 2, 3),
                  w = c(4, 5, 6), stringsAsFactors = FALSE)
  tbl <- gt::gt(d, rowname_col = "k", groupname_col = "g")
  tbl <- gt::tab_header(tbl, title = "T", subtitle = "S")
  tbl <- gt::tab_spanner(tbl, label = "Sp", columns = c("v", "w"))
  tbl <- gt::summary_rows(tbl, columns = "v", fns = list(tot = ~ sum(.x)))
  tbl <- gt::tab_source_note(tbl, "src")
  gt::tab_footnote(tbl, "fn", locations = gt::cells_body(columns = "v", rows = 1))
}

cell_alignment <- function(wb, ref, sheet = 1L) {
  st <- wb$styles_mgr$styles
  cc <- wb$worksheets[[sheet]]$sheet_data$cc
  s <- cc$c_s[cc$r == ref]
  if (!length(s) || is.na(s)) return(NULL)
  a <- openxlsx2::xml_attr(st$cellXfs[[as.integer(s) + 1L]], "xf", "alignment")
  if (!length(a)) return(list())
  as.list(a[[1L]])
}

row_height <- function(wb, row, sheet = 1L) {
  ra <- wb$worksheets[[sheet]]$sheet_data$row_attr
  h <- ra$ht[ra$r == as.character(row)]
  if (!length(h) || !nzchar(h)) return(NA_real_)
  as.numeric(h)
}
