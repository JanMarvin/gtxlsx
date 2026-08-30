xml_attr1 <- function(xml, ...) {
  if (is.null(xml) || !length(xml)) return(NULL)
  a <- openxlsx2::xml_attr(xml, ...)
  if (!length(a)) return(NULL)
  a[[1L]]
}

attr_get <- function(x, nm) if (!is.null(x) && nm %in% names(x)) x[[nm]] else NULL

hex_to_css <- function(hex) {
  if (is.null(hex) || is.na(hex) || !nzchar(hex)) return(NULL)
  hex <- toupper(hex)
  if (nchar(hex) == 8L) hex <- substring(hex, 3L)
  if (nchar(hex) != 6L) return(NULL)
  paste0("#", hex)
}

sheet_index <- function(wb, sheet) {
  if (inherits(sheet, "openxlsx2_waiver")) {
    i <- suppressWarnings(as.integer(wb$get_active_sheet()))
    if (length(i) != 1L || is.na(i) || i < 1L) i <- length(wb$worksheets)
    return(i)
  }
  if (is.numeric(sheet)) return(as.integer(sheet))
  i <- match(as.character(sheet), unname(wb$get_sheet_names()))
  if (is.na(i)) stop("sheet '", sheet, "' not found", call. = FALSE)
  i
}

xf_style <- function(wb, id) {
  st <- wb$styles_mgr$styles
  if (is.na(id) || id + 1L > length(st$cellXfs)) return(NULL)
  xf <- xml_attr1(st$cellXfs[[id + 1L]], "xf")
  if (is.null(xf)) return(NULL)
  out <- list()

  fid <- attr_get(xf, "fillId")
  if (!is.null(fid) && identical(attr_get(xf, "applyFill") %||% "1", "1")) {
    fid <- as.integer(fid)
    if (fid + 1L <= length(st$fills)) {
      pat <- xml_attr1(st$fills[[fid + 1L]], "fill", "patternFill")
      fg <- xml_attr1(st$fills[[fid + 1L]], "fill", "patternFill", "fgColor")
      if (!is.null(fg) && !identical(attr_get(pat, "patternType") %||% "", "none")) {
        out$fill <- hex_to_css(attr_get(fg, "rgb") %||% NA_character_)
      }
    }
  }

  fid <- attr_get(xf, "fontId")
  if (!is.null(fid)) {
    fid <- as.integer(fid)
    if (fid + 1L <= length(st$fonts)) {
      fx <- st$fonts[[fid + 1L]]
      out$bold <- length(openxlsx2::xml_node(fx, "font", "b")) > 0L
      out$italic <- length(openxlsx2::xml_node(fx, "font", "i")) > 0L
      fc <- attr_get(xml_attr1(fx, "font", "color"), "rgb")
      out$color <- hex_to_css(fc %||% NA_character_)
      sz <- attr_get(xml_attr1(fx, "font", "sz"), "val")
      if (!is.null(sz)) out$size <- paste0(sz, "pt")
      out$font <- attr_get(xml_attr1(fx, "font", "name"), "val")
    }
  }

  out$align <- attr_get(xml_attr1(st$cellXfs[[id + 1L]], "xf", "alignment"), "horizontal")
  out
}

sheet_merges <- function(wb, sheet) {
  refs <- unlist(wb$worksheets[[sheet_index(wb, sheet)]]$mergeCells, use.names = FALSE)
  if (!length(refs)) return(NULL)
  refs <- unlist(regmatches(refs, regexpr("[A-Z]+[0-9]+:[A-Z]+[0-9]+", refs)))
  if (!length(refs)) return(NULL)
  rc <- lapply(refs, openxlsx2::dims_to_rowcol, as_integer = TRUE)
  data.frame(
    ref = refs,
    row_min = vapply(rc, function(z) min(z$row), numeric(1L)),
    row_max = vapply(rc, function(z) max(z$row), numeric(1L)),
    col_min = vapply(rc, function(z) min(z$col), numeric(1L)),
    col_max = vapply(rc, function(z) max(z$col), numeric(1L)),
    stringsAsFactors = FALSE
  )
}

as_numeric_col <- function(x) {
  if (all(is.na(x))) return(x)
  n <- suppressWarnings(as.numeric(x))
  if (any(is.na(n) & !is.na(x))) x else n
}

#' Turn a worksheet range back into a gt table (experimental)
#'
#' Reads a range of cells and builds a `gt` object from it: full width merged
#' rows at the top become the heading, partly merged rows above the labels
#' become spanners, full width merged rows at the bottom become source notes,
#' and per-cell fills, fonts and alignment are translated into
#' `gt::tab_style()` calls.
#'
#' @section Please read this before using it:
#' This function is a development toy, not a finished feature. It exists
#' because the reverse direction was interesting to try, and it has had only
#' light testing — a handful of sheets, no round trip guarantees. Treat its
#' output as a starting point you will edit, not as a faithful copy, and
#' expect the details to change or the function to be withdrawn.
#'
#' A worksheet simply does not record most of what a `gt` table knows. Row
#' groups, the stub, footnote marks and number formats do not come back:
#' groups arrive as ordinary rows, the stub as a column named after its
#' letter, footnote marks glued to the text they mark, and `$115,900` as the
#' bare number `115900` with no `fmt_currency()` behind it. Column names are
#' made unique, so repeated labels gain a suffix.
#'
#' @param wb A `wbWorkbook` object.
#' @param sheet The worksheet to read.
#' @param dims Range to read. Defaults to the used range of the sheet.
#' @param styles Translate cell styles into `gt::tab_style()` calls. This is
#'   done cell by cell, so it is slow on large ranges.
#' @param structure Read merged cells as heading, spanners and source notes.
#'   With `FALSE` the range is taken as a plain table.
#' @param ... Passed on to [openxlsx2::wb_to_df()].
#'
#' @return A `gt_tbl` object.
#'
#' @examples
#' library(openxlsx2)
#'
#' wb <- wb_workbook()$add_worksheet()
#' wb$add_data(x = data.frame(a = c("x", "y"), b = c(1, 2)))
#'
#' tbl <- wb_to_gt(wb, dims = "A1:B3", styles = FALSE)
#' class(tbl)
#'
#' @export
wb_to_gt <- function(wb, sheet = current_sheet(), dims = NULL, styles = TRUE,
                     structure = TRUE, ...) {
  if (!inherits(wb, "wbWorkbook")) {
    stop("`wb` must be a 'wbWorkbook' object", call. = FALSE)
  }

  args <- list(wb, sheet = sheet, col_names = FALSE, ...)
  if (!is.null(dims)) args$dims <- dims
  raw <- do.call(openxlsx2::wb_to_df, args)
  sheet_rows <- suppressWarnings(as.integer(rownames(raw)))
  sheet_cols <- openxlsx2::col2int(names(raw))
  raw[] <- lapply(raw, as.character)
  cell <- function(r, cc) raw[match(r, sheet_rows), match(cc, sheet_cols)]
  row_values <- function(r) {
    v <- as.character(raw[match(r, sheet_rows), ])
    v[!is.na(v) & nzchar(v)]
  }

  mg <- if (isTRUE(structure)) sheet_merges(wb, sheet) else NULL
  if (!is.null(mg) && nrow(mg)) {
    mg <- mg[mg$row_min >= min(sheet_rows) & mg$row_max <= max(sheet_rows) &
               mg$col_min >= min(sheet_cols) & mg$col_max <= max(sheet_cols), ,
             drop = FALSE]
  }
  full <- function(r) {
    !is.null(mg) && nrow(mg) &&
      any(mg$row_min == r & mg$col_min == min(sheet_cols) &
            mg$col_max == max(sheet_cols))
  }
  spans <- function(r) {
    if (is.null(mg) || !nrow(mg)) return(NULL)
    m <- mg[mg$row_min == r & mg$col_max > mg$col_min &
              !(mg$col_min == min(sheet_cols) & mg$col_max == max(sheet_cols)), ,
            drop = FALSE]
    if (nrow(m)) m else NULL
  }

  head_rows <- span_rows <- integer(0L)
  i <- 1L
  while (i <= length(sheet_rows) && full(sheet_rows[i])) {
    head_rows <- c(head_rows, sheet_rows[i])
    i <- i + 1L
  }
  while (i <= length(sheet_rows) && !is.null(spans(sheet_rows[i]))) {
    span_rows <- c(span_rows, sheet_rows[i])
    i <- i + 1L
  }
  label_row <- if (i <= length(sheet_rows)) sheet_rows[i] else NA_integer_
  i <- i + 1L

  note_rows <- integer(0L)
  j <- length(sheet_rows)
  while (j >= i && full(sheet_rows[j])) {
    note_rows <- c(sheet_rows[j], note_rows)
    j <- j - 1L
  }
  body_rows <- if (i <= j) sheet_rows[i:j] else integer(0L)

  body <- raw[match(body_rows, sheet_rows), , drop = FALSE]
  nms <- if (is.na(label_row)) {
    names(raw)
  } else {
    as.character(raw[match(label_row, sheet_rows), ])
  }
  bad <- is.na(nms) | !nzchar(trimws(nms))
  nms[bad] <- names(raw)[bad]
  names(body) <- make.unique(nms)
  rownames(body) <- NULL
  body[] <- lapply(body, as_numeric_col)

  out <- gt::gt(body)

  txt <- unlist(lapply(head_rows, function(r) row_values(r)[1L]))
  txt <- txt[!is.na(txt)]
  if (length(txt)) {
    out <- gt::tab_header(out, title = txt[1L],
                          subtitle = if (length(txt) > 1L) txt[2L] else NULL)
  }

  for (r in rev(span_rows)) {
    m <- spans(r)
    for (k in seq_len(nrow(m))) {
      cols <- names(body)[match(seq.int(m$col_min[k], m$col_max[k]), sheet_cols)]
      cols <- cols[!is.na(cols)]
      lbl <- cell(r, m$col_min[k])
      if (!length(cols) || is.na(lbl) || !nzchar(lbl)) next
      out <- gt::tab_spanner(out, label = lbl, columns = cols)
    }
  }

  for (r in note_rows) {
    v <- row_values(r)
    if (length(v)) out <- gt::tab_source_note(out, source_note = v[1L])
  }

  if (!isTRUE(styles) || !length(body_rows)) return(out)

  for (jj in seq_along(sheet_cols)) {
    for (ii in seq_along(body_rows)) {
      ref <- ref_of(body_rows[ii], sheet_cols[jj])
      id <- suppressWarnings(as.integer(openxlsx2::wb_get_cell_style(wb, sheet, ref)))
      if (length(id) != 1L || is.na(id)) next
      st <- xf_style(wb, id)
      if (is.null(st)) next
      sl <- list()
      if (!is.null(st$fill)) sl <- c(sl, list(gt::cell_fill(color = st$fill)))
      ct <- list(
        color = st$color, font = st$font, size = st$size,
        weight = if (isTRUE(st$bold)) "bold" else NULL,
        style = if (isTRUE(st$italic)) "italic" else NULL,
        align = st$align
      )
      ct <- ct[!vapply(ct, is.null, logical(1L))]
      if (length(ct)) sl <- c(sl, list(do.call(gt::cell_text, ct)))
      if (!length(sl)) next
      out <- gt::tab_style(out, style = sl,
                           locations = gt::cells_body(columns = names(body)[jj],
                                                      rows = ii))
    }
  }
  out
}
