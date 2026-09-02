style_attrs <- function(st, th) {
  out <- list()
  fill <- st$cell_fill$color
  if (!is.null(fill)) out$fill <- css_color(fill)

  txt <- st$cell_text
  if (!is.null(txt)) {
    if (!is.null(txt$color)) out$color <- css_color(txt$color)
    if (!is.null(txt$font)) out$font <- as.character(txt$font)[1L]
    if (!is.null(txt$size)) out$size <- css_pt(txt$size, base = th$base_px)
    if (!is.null(txt$weight)) out$bold <- is_bold(txt$weight)
    if (!is.null(txt$style)) out$italic <- tolower(txt$style) %in% c("italic", "oblique")
    if (!is.null(txt$decorate)) {
      dec <- tolower(paste(txt$decorate, collapse = " "))
      out$underline <- grepl("underline", dec, fixed = TRUE)
      out$strike <- grepl("line-through", dec, fixed = TRUE)
    }
    if (!is.null(txt$align)) out$halign <- css_align(txt$align)
    if (!is.null(txt$v_align)) out$valign <- css_valign(txt$v_align)
    if (!is.null(txt$indent)) {
      n <- css_px(txt$indent, base = th$base_px)
      if (!is.na(n)) out$indent <- max(as.integer(round(n / 10)), 0L)
    }
    if (!is.null(txt$whitespace)) {
      out$wrap <- !identical(tolower(txt$whitespace), "nowrap")
    }
  }
  out
}

style_borders <- function(st) {
  out <- list()
  for (n in grep("^cell_border", names(st), value = TRUE)) {
    b <- st[[n]]
    if (is.null(b)) next
    sides <- b$side %||% b$sides %||% sub("^cell_border_?s?", "", n)
    if (any(sides %in% "all") || !nzchar(sides[1L])) {
      sides <- c("left", "right", "top", "bottom")
    }
    border <- css_border(b$style, b$width %||% b$weight)
    color <- css_color(b$color)
    for (sd in sides) {
      out[[length(out) + 1L]] <- list(side = sd, border = border, color = color)
    }
  }
  out
}

style_targets <- function(row, g, p) {
  loc <- row$locname
  cols_all <- p$col0 - 1L + seq_len(p$w)
  stub_col <- if (length(p$stub_vars)) p$col_of(p$stub_vars[1L]) else p$col0
  # gt 1.2.0 made cells_stub() target every column of a multi-column stub
  stub_cols <- if (length(p$stub_vars)) sort(p$col_of(p$stub_vars)) else p$col0
  col_of_name <- function(nm) {
    if (is.na(nm) || !nm %in% p$col_vars) NULL else p$col_of(nm)
  }
  body_row_of <- function(i) {
    if (is.na(i) || i < 1L || i > length(p$body_row)) NULL else p$body_row[i]
  }

  switch(
    loc,
    title = if (is.na(p$title_row)) NULL else list(rows = p$title_row, cols = cols_all),
    subtitle = if (is.na(p$subtitle_row)) {
      NULL
    } else {
      list(rows = p$subtitle_row, cols = cols_all)
    },
    stubhead = if (is.na(p$label_row)) {
      NULL
    } else {
      list(rows = p$label_row, cols = stub_col)
    },
    columns_columns = {
      j <- col_of_name(row$colname)
      if (is.null(j) || is.na(p$label_row)) NULL else list(rows = p$label_row, cols = j)
    },
    columns_groups = {
      sp <- p$spanners
      if (is.null(sp) || !nrow(sp)) return(NULL)
      k <- which(sp$spanner_id == row$grpname)
      if (!length(k)) return(NULL)
      list(rows = p$level_row[[as.character(sp$spanner_level[k[1L]])]],
           cols = sort(p$col_of(sp$vars[[k[1L]]])))
    },
    row_groups = {
      if (nrow(p$group_head)) {
        k <- which(p$group_head$group_id == row$grpname)
        if (!length(k)) return(NULL)
        list(rows = p$group_head$row[k[1L]], cols = cols_all)
      } else if (nrow(p$group_span)) {
        k <- which(p$group_span$group_id == row$grpname)
        if (!length(k)) return(NULL)
        list(rows = seq.int(p$group_span$row_start[k[1L]], p$group_span$row_end[k[1L]]),
             cols = p$col_of(p$group_var))
      } else {
        NULL
      }
    },
    data = {
      j <- col_of_name(row$colname)
      i <- body_row_of(row$rownum)
      if (is.null(j) || is.null(i)) NULL else list(rows = i, cols = j)
    },
    stub_column = ,
    stub = {
      i <- body_row_of(row$rownum)
      j <- if (!is.na(row$colname) && row$colname %in% p$col_vars) {
        p$col_of(row$colname)
      } else {
        stub_cols
      }
      if (is.null(i)) NULL else list(rows = i, cols = j)
    },
    summary_cells = ,
    grand_summary_cells = {
      key <- if (identical(loc, "grand_summary_cells")) "::GRAND_SUMMARY" else row$grpname
      s <- Filter(function(z) identical(z$key, key), p$summaries)
      if (!length(s)) return(NULL)
      i <- row$rownum
      if (is.na(i) || i < 1L || i > length(s[[1L]]$rows)) return(NULL)
      j <- col_of_name(row$colname)
      list(rows = s[[1L]]$rows[i], cols = j %||% stub_col)
    },
    footnotes = if (!length(p$footnote_rows)) {
      NULL
    } else {
      list(rows = p$footnote_rows, cols = cols_all)
    },
    source_notes = if (!length(p$source_rows)) {
      NULL
    } else {
      list(rows = p$source_rows, cols = cols_all)
    },
    NULL
  )
}

gtxlsx_apply_styles <- function(cc, g, th, p) {
  styles <- g$styles
  cc$borders <- list()
  if (is.null(styles) || !nrow(styles)) return(invisible(NULL))

  for (i in seq_len(nrow(styles))) {
    row <- as.list(styles[i, ])
    tgt <- style_targets(row, g, p)
    if (is.null(tgt)) next
    st <- styles$styles[[i]]
    if (is.null(st)) next

    attrs <- style_attrs(st, th)
    if (length(attrs)) {
      for (rr in tgt$rows) {
        for (jj in tgt$cols) {
          do.call(put_cell, c(list(cc, rr, jj), attrs))
        }
      }
    }

    bl <- style_borders(st)
    for (b in bl) {
      if (is.null(b$border)) next
      cc$borders[[length(cc$borders) + 1L]] <-
        list(rows = tgt$rows, cols = tgt$cols, side = b$side,
             border = b$border, color = b$color)
    }
  }
  invisible(NULL)
}

# openxlsx2 defaults every outer side to "thin", so unused sides must be
# passed as NULL explicitly
border_args <- function(...) {
  args <- list(top_border = NULL, bottom_border = NULL,
               left_border = NULL, right_border = NULL,
               top_color = NULL, bottom_color = NULL,
               left_color = NULL, right_color = NULL,
               inner_hgrid = NULL, inner_vgrid = NULL,
               inner_hcolor = NULL, inner_vcolor = NULL,
               update = TRUE)
  extra <- list(...)
  args[names(extra)] <- extra
  args[names(args) %in% c(setdiff(names(formals(openxlsx2::wb_add_border)), "..."))]
}

side_border <- function(wb, sheet, dims, side, border, color) {
  if (is.null(border) || identical(border, "none")) return(invisible(NULL))
  sides <- list()
  sides[[paste0(side, "_border")]] <- border
  sides[[paste0(side, "_color")]] <- wbc(color) %||% openxlsx2::wb_color(hex = "FF000000")
  do.call(wb$add_border,
          c(list(sheet = sheet, dims = dims), do.call(border_args, sides)))
}

# One add_border() per cell is the single most expensive thing the writers can
# do, so identical requests are collapsed into contiguous ranges first.
apply_borders <- function(wb, sheet, borders) {
  if (!length(borders)) return(invisible(NULL))
  rows <- unlist(lapply(borders, function(b) rep(b$rows, each = length(b$cols))))
  cols <- unlist(lapply(borders, function(b) rep(b$cols, times = length(b$rows))))
  sig <- unlist(lapply(borders, function(b) {
    rep(paste(b$side, b$border, b$color %||% "", sep = "\r"),
        length(b$rows) * length(b$cols))
  }))
  for (k in split(seq_along(sig), sig)) {
    parts <- strsplit(sig[k[1L]], "\r", fixed = TRUE)[[1L]]
    # openxlsx2 treats a range as a block, so a bottom border on A4:A5 lands on
    # A5 only. Runs therefore go across a row for top and bottom borders, and
    # down a column for left and right.
    along <- if (parts[1L] %in% c("top", "bottom")) "row" else "col"
    for (rng in ref_runs(rows[k], cols[k], along)) {
      side_border(wb, sheet, rng, parts[1L], parts[2L],
                  if (nzchar(parts[3L])) parts[3L] else NULL)
    }
  }
  invisible(NULL)
}

gtxlsx_borders <- function(wb, sheet, cc, g, th, p, cols) {
  ops <- g$options
  base <- th$base_px
  ob <- function(prefix) {
    css_border(opt_chr(ops, paste0(prefix, "_style")),
               opt_chr(ops, paste0(prefix, "_width")), base = base)
  }
  oc <- function(prefix) css_color(opt_chr(ops, paste0(prefix, "_color")))

  rng <- function(rows, cs = cols) openxlsx2::wb_dims(rows = rows, cols = cs)

  body_rows <- p$body_row[!is.na(p$body_row)]
  sum_rows <- unlist(lapply(p$summaries, `[[`, "rows"))
  block_rows <- c(body_rows, sum_rows, p$group_head$row, p$group_span$row_start)
  head_rows <- c(p$title_row, p$subtitle_row)
  head_rows <- head_rows[!is.na(head_rows)]

  if (length(block_rows)) {
    r0 <- min(block_rows)
    r1 <- max(block_rows)
    hg <- ob("table_body_hlines")
    vg <- ob("table_body_vlines")
    do.call(wb$add_border, c(
      list(sheet = sheet, dims = rng(seq.int(r0, r1))),
      border_args(
        # a colour without a style writes <right><color/></right>, a border side
        # with no style attribute, so drop the colour with the style
        inner_hgrid = if (identical(hg, "none")) NULL else hg,
        inner_hcolor = if (identical(hg, "none")) NULL else wbc(oc("table_body_hlines")),
        inner_vgrid = if (identical(vg, "none")) NULL else vg,
        inner_vcolor = if (identical(vg, "none")) NULL else wbc(oc("table_body_vlines"))
      )
    ))
    side_border(wb, sheet, rng(r0), "top", ob("table_body_border_top"),
                oc("table_body_border_top"))
    side_border(wb, sheet, rng(r1), "bottom", ob("table_body_border_bottom"),
                oc("table_body_border_bottom"))
  }

  if (!is.na(p$label_row)) {
    lab_rows <- c(p$level_row, p$label_row)
    side_border(wb, sheet, rng(min(lab_rows)), "top",
                ob("column_labels_border_top"), oc("column_labels_border_top"))
    side_border(wb, sheet, rng(p$label_row), "bottom",
                ob("column_labels_border_bottom"), oc("column_labels_border_bottom"))
    # gt puts column_labels.vlines on every heading cell, outer edges included
    vl <- ob("column_labels_vlines")
    if (!is.null(vl) && !identical(vl, "none")) {
      vc <- wbc(oc("column_labels_vlines"))
      wb$add_border(
        sheet = sheet, dims = rng(p$label_row), update = TRUE,
        top_border = NULL, top_color = NULL,
        bottom_border = NULL, bottom_color = NULL,
        left_border = vl, left_color = vc,
        right_border = vl, right_color = vc,
        inner_vgrid = vl, inner_vcolor = vc
      )
    }
    # gt gives every spanner the same bottom border as the column labels
    sp <- p$spanners
    if (!is.null(sp) && nrow(sp)) {
      for (i in seq_len(nrow(sp))) {
        j <- sort(p$col_of(sp$vars[[i]]))
        row <- p$level_row[[as.character(sp$spanner_level[i])]]
        side_border(wb, sheet, rng(row, j), "bottom",
                    ob("column_labels_border_bottom"),
                    oc("column_labels_border_bottom"))
      }
    }
  }

  if (length(head_rows)) {
    side_border(wb, sheet, rng(max(head_rows)), "bottom",
                ob("heading_border_bottom"), oc("heading_border_bottom"))
  }

  if (nrow(p$group_head)) {
    for (r in p$group_head$row) {
      side_border(wb, sheet, rng(r), "top", ob("row_group_border_top"),
                  oc("row_group_border_top"))
      side_border(wb, sheet, rng(r), "bottom", ob("row_group_border_bottom"),
                  oc("row_group_border_bottom"))
    }
  }

  if (length(p$stub_vars) && length(block_rows)) {
    j <- max(p$col_of(p$stub_vars))
    side_border(wb, sheet, rng(seq.int(min(block_rows), max(block_rows)), j),
                "right", ob("stub_border"), oc("stub_border"))
  }

  if (length(p$footnote_rows)) {
    side_border(wb, sheet, rng(max(p$footnote_rows)), "bottom",
                ob("footnotes_border_bottom"), oc("footnotes_border_bottom"))
  }
  if (length(p$source_rows)) {
    side_border(wb, sheet, rng(max(p$source_rows)), "bottom",
                ob("source_notes_border_bottom"), oc("source_notes_border_bottom"))
  }

  if (opt_lgl(ops, "table_border_top_include", TRUE)) {
    side_border(wb, sheet, rng(p$row0), "top", ob("table_border_top"),
                oc("table_border_top"))
  }
  if (opt_lgl(ops, "table_border_bottom_include", TRUE)) {
    side_border(wb, sheet, rng(p$last_row), "bottom", ob("table_border_bottom"),
                oc("table_border_bottom"))
  }
  side_border(wb, sheet, rng(seq.int(p$row0, p$last_row), cols[1L]), "left",
              ob("table_border_left"), oc("table_border_left"))
  side_border(wb, sheet, rng(seq.int(p$row0, p$last_row), cols[length(cols)]), "right",
              ob("table_border_right"), oc("table_border_right"))

  apply_borders(wb, sheet, cc$borders %||% list())
  invisible(NULL)
}
