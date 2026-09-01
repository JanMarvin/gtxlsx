#' Write a gt table into a worksheet
#'
#' Lays a `gt` table out as cells: the heading, the column spanners, the
#' column labels, the stub, the row groups, the body, any summary rows, the
#' footnotes and the source notes, one after another in a single rectangular
#' block starting at `dims`.
#'
#' Everything `gt` applies before rendering is already in place when the cells
#' are written, because `gtxlsx` reads the table gt has built rather than
#' repeating the work: every `fmt_*()` and `sub_*()`, the `cols_merge_*()`
#' family, `text_transform()`, `data_color()`, `summary_rows()` and the
#' footnote marks. Styling set with `gt::tab_style()` and `gt::tab_options()`
#' becomes fonts, fills, alignment and borders; markup inside a cell (bold,
#' italic, superscripts, line breaks) becomes rich text.
#'
#' Anything gt draws as a picture cannot be written to a cell.
#' `gt::fmt_image()` and `gt::cols_nanoplot()` leave the cell empty,
#' `gt::fmt_icon()` and `gt::fmt_flag()` fall back to their label text, and
#' `gt::fmt_url()` keeps the link text but not the hyperlink.
#'
#' @section Numbers versus text:
#' With `numeric = TRUE` a column is written as numbers whenever an Excel
#' number format can reproduce exactly what gt displays. `$1,234.50` becomes
#' the value `1234.5` with the format `"$"#,##0.00`, so the sheet stays
#' usable for arithmetic. Columns gt has scaled or suffixed (`1.2K` for
#' `1200`) cannot be reproduced that way and stay text; those cells are
#' marked so Excel does not flag them with its green "number stored as text"
#' indicator.
#'
#' @param wb A `wbWorkbook` object, as returned by [openxlsx2::wb_workbook()].
#' @param x A `gt_tbl` object.
#' @param sheet The worksheet to write to. Defaults to the current sheet.
#' @param dims Cell reference of the top left corner of the table, for example
#'   `"B2"`.
#' @param numeric Write numbers as numbers where the displayed format can be
#'   reproduced. Set to `FALSE` to write every cell as text.
#' @param col_widths `"auto"` measures the rendered text and sizes the columns
#'   to fit it, a numeric vector sets the widths directly, and `NULL` leaves
#'   them alone. Widths set with `gt::cols_width()` always win.
#' @param ignore_errors Mark text cells whose content looks like a number or a
#'   date, so Excel stops showing the green warning triangle on them.
#' @param ... Currently unused.
#'
#' @return The workbook, invisibly. The input workbook is not modified; a
#'   clone is returned, as elsewhere in `openxlsx2`.
#'
#' @seealso [wb_add_html()] for tables that are already HTML, and
#'   [gtxlsx_extract()] to see the pieces `wb_add_gt()` works from.
#'
#' @examples
#' library(gt)
#' library(openxlsx2)
#'
#' tbl <- gt(data.frame(item = c("Cash", "Debt"), amount = c(1204.5, -3910)))
#' tbl <- fmt_currency(tbl, columns = "amount", decimals = 2)
#' tbl <- tab_header(tbl, title = "Balance")
#'
#' wb <- wb_workbook()$add_worksheet()
#' wb <- wb_add_gt(wb, tbl, dims = "B2")
#'
#' wb_to_df(wb, col_names = FALSE)
#'
#' @export
wb_add_gt <- function(wb, x, sheet = current_sheet(), dims = "A1",
                      numeric = TRUE, col_widths = "auto",
                      ignore_errors = TRUE, ...) {
  if (!inherits(wb, "wbWorkbook")) {
    stop("`wb` must be a 'wbWorkbook' object", call. = FALSE)
  }
  wb <- wb$clone()

  g <- gtxlsx_extract(x)
  th <- gtxlsx_theme(g$options)

  rc <- openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  p <- gtxlsx_plan(g, th, row0 = min(rc$row), col0 = min(rc$col))

  cc <- new_sheet_cells()
  cols <- p$col0 - 1L + seq_len(p$w)

  gtxlsx_write_heading(cc, g, th, p, cols)
  gtxlsx_heading_marks(cc, g, th, p, cols)
  gtxlsx_write_spanners(cc, g, th, p)
  gtxlsx_write_labels(cc, g, th, p)
  gtxlsx_write_body(cc, g, th, p, numeric = numeric)
  gtxlsx_write_summaries(cc, g, th, p)
  gtxlsx_write_notes(cc, g, th, p, cols)
  gtxlsx_apply_styles(cc, g, th, p)

  flagged <- render_cells(wb, sheet, cc, th)
  gtxlsx_borders(wb, sheet, cc, g, th, p, cols)
  gtxlsx_col_widths(wb, sheet, g, th, p, col_widths)

  if (isTRUE(ignore_errors)) {
    for (rng in flagged) {
      wb$add_ignore_error(
        sheet = sheet,
        dims = rng,
        number_stored_as_text = TRUE,
        two_digit_text_year = TRUE
      )
    }
  }

  invisible(wb)
}

gtxlsx_write_heading <- function(cc, g, th, p, cols) {
  if (!is.na(p$title_row)) {
    txt <- as.character(g$heading$title)[1L]
    put_cell(cc, p$title_row, cols[1L], txt, rich = is_rich(txt),
             size = th$title_size, bold = is_bold(th$title_weight),
             font = th$font, color = th$title_color, halign = th$heading_align,
             fill = th$heading_fill, wrap = grepl("<br", txt, fixed = TRUE))
    for (j in cols[-1L]) {
      put_cell(cc, p$title_row, j, fill = th$heading_fill, size = th$title_size)
    }
    add_merge(cc, p$title_row, cols)
  }
  if (!is.na(p$subtitle_row)) {
    txt <- as.character(g$heading$subtitle)[1L]
    put_cell(cc, p$subtitle_row, cols[1L], txt, rich = is_rich(txt),
             size = th$subtitle_size, bold = is_bold(th$subtitle_weight),
             font = th$font, color = th$title_color, halign = th$heading_align,
             fill = th$heading_fill, wrap = grepl("<br", txt, fixed = TRUE))
    for (j in cols[-1L]) {
      put_cell(cc, p$subtitle_row, j, fill = th$heading_fill, size = th$subtitle_size)
    }
    add_merge(cc, p$subtitle_row, cols)
  }
}

gtxlsx_write_spanners <- function(cc, g, th, p) {
  sp <- p$spanners
  if (is.null(sp) || !nrow(sp)) return(invisible(NULL))
  for (i in seq_len(nrow(sp))) {
    vars <- sp$vars[[i]]
    j <- sort(p$col_of(vars))
    row <- p$level_row[[as.character(sp$spanner_level[i])]]
    txt <- sp$built[i]
    put_cell(cc, row, j[1L], txt, rich = is_rich(txt), size = th$label_size,
             bold = is_bold(th$label_weight), font = th$font, halign = "center",
             color = th$label_color, fill = th$label_fill)
    for (k in j[-1L]) {
      put_cell(cc, row, k, fill = th$label_fill, size = th$label_size)
    }
    add_merge(cc, row, j)
  }
  # fill the gaps on spanner rows so the header block looks contiguous
  for (row in p$level_row) {
    for (j in p$col0 - 1L + seq_len(p$w)) {
      put_cell(cc, row, j, fill = th$label_fill, size = th$label_size)
    }
  }
}

gtxlsx_write_labels <- function(cc, g, th, p) {
  if (is.na(p$label_row)) return(invisible(NULL))
  boxh <- g$boxhead
  for (v in p$col_vars) {
    i <- match(v, boxh$var)
    head_var <- c(if (p$group_as_col) p$group_var, p$stub_vars)[1L]
    lbl <- if (v %in% c(p$stub_vars, if (p$group_as_col) p$group_var)) {
      if (identical(v, head_var)) as.character(g$stubhead$label %||% "")[1L] else ""
    } else {
      as.character(boxh$column_label[[i]])[1L]
    }
    if (is.na(lbl)) lbl <- ""
    put_cell(cc, p$label_row, p$col_of(v), lbl, rich = is_rich(lbl),
             size = th$label_size, bold = is_bold(th$label_weight),
             font = th$font, color = th$label_color, fill = th$label_fill,
             halign = css_align(boxh$column_align[i], "center"),
             valign = "bottom", wrap = grepl("<br", lbl, fixed = TRUE))
  }
}

gtxlsx_write_body <- function(cc, g, th, p, numeric = TRUE) {
  body <- g$body
  if (is.null(body) || !nrow(body)) return(invisible(NULL))
  boxh <- g$boxhead
  stub <- g$stub

  for (v in c(p$stub_vars, p$body_vars)) {
    j <- p$col_of(v)
    txt <- as.character(body[[v]])
    is_stub <- v %in% p$stub_vars
    align <- if (is_stub) {
      "left"
    } else {
      css_align(boxh$column_align[match(v, boxh$var)], "right")
    }
    size <- if (is_stub) th$stub_size else th$size
    bold <- if (is_stub) is_bold(th$stub_weight) else FALSE
    fg_col <- if (is_stub) th$stub_color else th$color
    fill_col <- if (is_stub) th$stub_fill else NULL

    num <- NULL
    if (numeric && !is_stub && !is.null(g$data[[v]]) && is.numeric(g$data[[v]])) {
      num <- infer_numfmt(txt, g$data[[v]])
    }

    # Every cell in a column shares its font, size, colour and alignment; only
    # the fill and the indent vary. Building each variant once and passing it
    # by reference keeps put_cell cheap and lets render_cells group the cells
    # without deriving a signature per record.
    base <- list(size = size, font = th$font, color = fg_col, bold = bold,
                 halign = align)
    styles <- list()
    rich <- is_rich(txt)
    wrap <- grepl("<br", txt, fixed = TRUE)
    stripe <- th$stripe_fill
    stripe_on <- !is.null(stripe) &&
      ((is_stub && th$stripe_stub) || (!is_stub && th$stripe_body))
    indents <- if (is_stub && !is.null(stub$indent)) {
      suppressWarnings(as.integer(stub$indent))
    } else {
      NULL
    }

    for (i in seq_len(nrow(body))) {
      row <- p$body_row[i]
      if (is.na(row)) next
      fill <- if (stripe_on && i %% 2L == 0L) stripe else fill_col
      indent <- if (!is.null(indents) && !is.na(indents[i]) && indents[i] != 0L) {
        indents[i]
      } else {
        NULL
      }
      key <- paste0(v, "\r", fill %||% "-", "\r", indent %||% "-")
      st <- styles[[key]]
      if (is.null(st)) {
        st <- base
        if (!is.null(fill)) st$fill <- fill
        if (!is.null(indent)) st$indent <- indent
        styles[[key]] <- st
      }

      if (!is.null(num) && !is.na(num$values[i])) {
        put_cell(cc, row, j, num = num$values[i], numfmt = num$numfmt,
                 style = st, sig = key)
      } else {
        put_cell(cc, row, j, txt[i], rich = rich[i], wrap = wrap[i],
                 style = st, sig = key)
      }
    }
  }

  cols <- p$col0 - 1L + seq_len(p$w)
  if (nrow(p$group_head)) {
    for (i in seq_len(nrow(p$group_head))) {
      lbl <- p$group_head$label[i]
      put_cell(cc, p$group_head$row[i], cols[1L], lbl, rich = is_rich(lbl),
               size = th$group_size, bold = is_bold(th$group_weight),
               font = th$font, color = th$group_color, halign = "left",
               fill = th$group_fill)
      for (k in cols[-1L]) {
        put_cell(cc, p$group_head$row[i], k, fill = th$group_fill, size = th$group_size)
      }
      add_merge(cc, p$group_head$row[i], cols)
    }
  }
  if (nrow(p$group_span)) {
    j <- p$col_of(p$group_var)
    for (i in seq_len(nrow(p$group_span))) {
      lbl <- p$group_span$label[i]
      put_cell(cc, p$group_span$row_start[i], j, lbl, rich = is_rich(lbl),
               size = th$group_size, bold = is_bold(th$group_weight),
               font = th$font, color = th$group_color, halign = "left",
               valign = "top", fill = th$group_fill)
      add_merge(cc, seq.int(p$group_span$row_start[i], p$group_span$row_end[i]), j)
    }
  }
}

gtxlsx_write_summaries <- function(cc, g, th, p) {
  if (!length(p$summaries)) return(invisible(NULL))
  for (s in p$summaries) {
    df <- s$df
    grand <- identical(s$kind, "grand_summary")
    fill_col <- if (grand) th$gsummary_fill else th$summary_fill
    fg_col <- if (grand) th$gsummary_color else th$summary_color
    labels <- as.character(df[["::rowname::"]])
    stub_col <- if (length(p$stub_vars)) p$col_of(p$stub_vars[1L]) else p$col0
    for (i in seq_along(s$rows)) {
      put_cell(cc, s$rows[i], stub_col, labels[i], rich = is_rich(labels[i]),
               size = th$summary_size, font = th$font, color = fg_col,
               halign = "left", fill = fill_col)
      for (v in p$body_vars) {
        if (is.null(df[[v]])) {
          put_cell(cc, s$rows[i], p$col_of(v), fill = fill_col, size = th$summary_size)
          next
        }
        val <- as.character(df[[v]])[i]
        put_cell(cc, s$rows[i], p$col_of(v), val, rich = is_rich(val),
                 size = th$summary_size, font = th$font, color = fg_col,
                 halign = "right", fill = fill_col)
      }
    }
  }
}

gtxlsx_write_notes <- function(cc, g, th, p, cols) {
  fn <- p$footnotes
  for (i in seq_along(p$footnote_rows)) {
    row <- p$footnote_rows[i]
    txt <- paste0("<sup>", fn$mark[i], "</sup> ", fn$text[i])
    put_cell(cc, row, cols[1L], txt, rich = TRUE, size = th$footnote_size,
             font = th$font, color = th$footnote_color, halign = "left",
             fill = th$footnote_fill, wrap = TRUE)
    for (k in cols[-1L]) put_cell(cc, row, k, fill = th$footnote_fill)
    add_merge(cc, row, cols)
  }
  for (i in seq_along(p$source_rows)) {
    row <- p$source_rows[i]
    txt <- g$source_notes[i]
    put_cell(cc, row, cols[1L], txt, rich = is_rich(txt), size = th$source_size,
             font = th$font, color = th$source_color, halign = "left",
             fill = th$source_fill, wrap = TRUE)
    for (k in cols[-1L]) put_cell(cc, row, k, fill = th$source_fill)
    add_merge(cc, row, cols)
  }
}

# build_data() already injects footnote marks into the body, column labels,
# spanners and row groups; the heading is marked at render time only.
gtxlsx_heading_marks <- function(cc, g, th, p, cols) {
  fn <- g$footnotes
  if (is.null(fn) || !nrow(fn)) return(invisible(NULL))
  for (loc in c("title", "subtitle")) {
    row <- if (identical(loc, "title")) p$title_row else p$subtitle_row
    if (is.na(row)) next
    idx <- which(fn$locname == loc)
    if (!length(idx)) next
    mark <- paste0(unique(as.character(fn$fs_id[idx])), collapse = ",")
    txt <- as.character(g$heading[[loc]])[1L]
    if (is.na(txt)) txt <- ""
    put_cell(cc, row, cols[1L], paste0(txt, "<sup>", mark, "</sup>"), rich = TRUE)
  }
  invisible(NULL)
}

# openxlsx2's "auto" measures the stored value, so a number written with a
# currency or grouping format ends up too narrow. Measure the strings gt would
# have displayed instead.
measure_col <- function(v, g, th, p) {
  boxh <- g$boxhead
  acc <- new.env(parent = emptyenv())
  acc$parts <- numeric(0L)
  add <- function(x, scale) {
    x <- html_strip(as.character(x))
    x <- unlist(strsplit(x[!is.na(x)], "\n", fixed = TRUE))
    if (length(x)) acc$parts <- c(acc$parts, nchar(x) * scale)
  }

  if (!is.na(p$label_row)) {
    i <- match(v, boxh$var)
    if (!is.na(i)) add(boxh$column_label[[i]], th$label_size / 11)
  }
  if (!is.null(g$body[[v]])) add(g$body[[v]], th$size / 11)
  for (s in p$summaries) {
    if (!is.null(s$df[[v]])) add(s$df[[v]], th$summary_size / 11)
    if (identical(v, p$stub_vars[1L])) add(s$df[["::rowname::"]], th$summary_size / 11)
  }
  if (!length(acc$parts)) return(NA_real_)
  min(max(max(acc$parts) + 2.6, 4), 80)
}

gtxlsx_col_widths <- function(wb, sheet, g, th, p, col_widths) {
  if (is.null(col_widths)) return(invisible(NULL))
  cols <- p$col0 - 1L + seq_len(p$w)

  if (is.numeric(col_widths)) {
    wb$set_col_widths(sheet = sheet, cols = cols,
                      widths = rep_len(col_widths, length(cols)))
    return(invisible(NULL))
  }

  boxh <- g$boxhead
  gt_w <- if (is.null(boxh$column_width)) {
    rep(NA_real_, length(p$col_vars))
  } else {
    vapply(p$col_vars, function(v) {
      w <- boxh$column_width[[match(v, boxh$var)]]
      if (is.null(w) || !length(w) || is.na(w[1L])) return(NA_real_)
      px_to_width(css_px(as.character(w)[1L]))
    }, numeric(1L))
  }

  widths <- vapply(p$col_vars, measure_col, numeric(1L), g = g, th = th, p = p)
  widths <- ifelse(is.na(gt_w), widths, gt_w)
  sel <- !is.na(widths)
  if (any(sel)) {
    wb$set_col_widths(sheet = sheet, cols = cols[sel], widths = round(widths[sel], 2))
  }
  invisible(NULL)
}
