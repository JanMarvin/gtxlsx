new_sheet_cells <- function() {
  e <- new.env(parent = emptyenv())
  e$n <- 0L
  e$rec <- vector("list", 256L)
  e$merges <- list()
  e$borders <- list()
  e
}

cell_key <- function(row, col) paste0(row, "_", col)

put_cell <- function(cc, row, col, text = NA_character_, ...) {
  args <- list(...)
  cc$n <- cc$n + 1L
  if (cc$n > length(cc$rec)) length(cc$rec) <- 2L * length(cc$rec)
  cc$rec[[cc$n]] <- c(list(row = row, col = col, text = text), args)
  invisible(NULL)
}

add_merge <- function(cc, rows, cols) {
  cc$merges[[length(cc$merges) + 1L]] <- list(rows = rows, cols = cols)
  invisible(NULL)
}

collect_cells <- function(cc) {
  if (!cc$n) return(list())
  cc$rec[seq_len(cc$n)]
}

# Merge attribute lists of records that target the same cell; later writes win.
# Grouping the indices first keeps this linear: growing a named list one key at
# a time is quadratic and dominates the run time on large tables.
merge_records <- function(recs) {
  if (!length(recs)) return(recs)
  keys <- paste0(vapply(recs, function(r) as.numeric(r$row), numeric(1L)), "_",
                 vapply(recs, function(r) as.numeric(r$col), numeric(1L)))
  idx <- split(seq_along(recs), keys)
  idx <- idx[order(vapply(idx, `[`, integer(1L), 1L))]

  single <- lengths(idx) == 1L
  out <- vector("list", length(idx))
  out[single] <- recs[vapply(idx[single], `[`, integer(1L), 1L)]

  for (k in which(!single)) {
    grp <- idx[[k]]
    acc <- recs[[grp[1L]]]
    for (i in grp[-1L]) {
      new <- recs[[i]]
      if (is.na(new$text %||% NA) && !is.na(acc$text %||% NA)) new$text <- acc$text
      acc <- c(new, acc[setdiff(names(acc), names(new))])
    }
    out[[k]] <- acc
  }
  out
}

ref_of <- function(rows, cols) paste0(openxlsx2::int2col(cols), rows)

# collapse scattered cells into per-column contiguous ranges. A comma separated
# dims string is not usable here: openxlsx2 expands it with
# dims_to_dataframe(fill = TRUE), which pads the bounding box with empty
# entries and those end up as attribute-less <ignoredError/> nodes.
ref_runs <- function(rows, cols) {
  if (!length(rows)) return(character(0L))
  ord <- order(cols, rows)
  rows <- rows[ord]
  cols <- cols[ord]
  brk <- c(TRUE, diff(rows) != 1L | diff(cols) != 0L)
  grp <- cumsum(brk)
  vapply(split(seq_along(rows), grp), function(i) {
    fst <- ref_of(rows[i][1L], cols[i][1L])
    lst <- ref_of(rows[i][length(i)], cols[i][1L])
    if (identical(fst, lst)) fst else paste0(fst, ":", lst)
  }, character(1L), USE.NAMES = FALSE)
}

# A record may carry its styling by reference: `style` is a shared attribute
# list and `sig` identifies it, so cells that look alike cost one key rather
# than a copy of every attribute.
fld <- function(r, nm) {
  v <- r[[nm]]
  if (is.null(v) && !is.null(r$style)) v <- r$style[[nm]]
  v
}

group_by_sig <- function(recs, fields, sigs = NULL) {
  # A record may point at a shared style through `sig`, but anything set
  # directly on it -- a tab_style() colour, a data_color() fill -- overrides
  # that per cell and has to take part in the key. Leaving the overrides out
  # would hand every cell in the group the first one's value.
  key <- vapply(seq_along(recs), function(i) {
    r <- recs[[i]]
    s <- if (!is.null(sigs) && !is.na(sigs[i])) sigs[i] else r$sig %||% ""
    base <- if (nzchar(s)) {
      s
    } else {
      paste0(vapply(fields, function(f) {
        z <- fld(r, f)
        if (is.null(z)) "" else paste0(z, collapse = "\1")
      }, character(1L)), collapse = "\2")
    }
    own <- fields[vapply(fields, function(f) !is.null(r[[f]]), logical(1L))]
    ov <- if (!length(own)) {
      ""
    } else {
      paste0(own, "=", vapply(own, function(f) paste0(r[[f]], collapse = ","),
                              character(1L)), collapse = ";")
    }
    paste0(base, "\1", isTRUE(r$wrap), "\1", ov)
  }, character(1L))
  unname(split(seq_along(recs), key))
}

# Style calls take their cells as a comma separated ref list. Chunk size makes
# little difference above a few hundred; larger chunks are marginally faster.
chunked <- function(idx, size = 1000L) {
  if (length(idx) <= size) return(list(idx))
  split(idx, ceiling(seq_along(idx) / size))
}

render_cells <- function(wb, sheet, cc, theme) {
  recs <- merge_records(collect_cells(cc))
  if (!length(recs)) return(invisible(NULL))

  # One pass over the records filling parallel vectors. Every field used below
  # was previously its own vapply over the whole set, which meant walking nine
  # thousand closures once per field.
  nr <- length(recs)
  rows <- integer(nr)
  cols <- integer(nr)
  txts <- character(nr)
  nums <- rep(NA_real_, nr)
  nfmt <- rep(NA_character_, nr)
  rich <- logical(nr)
  has_num <- logical(nr)
  has_txt <- logical(nr)
  sigs <- character(nr)
  for (i in seq_len(nr)) {
    r <- recs[[i]]
    rows[i] <- as.integer(r$row)
    cols[i] <- as.integer(r$col)
    tv <- r$text
    if (!is.null(tv) && !is.na(tv)) {
      txts[i] <- tv
      has_txt[i] <- TRUE
    } else {
      txts[i] <- NA_character_
    }
    nv <- r$num
    if (!is.null(nv) && !is.na(nv)) {
      nums[i] <- nv
      has_num[i] <- TRUE
    }
    fv <- r$numfmt
    if (!is.null(fv)) nfmt[i] <- fv
    rich[i] <- isTRUE(r$rich)
    sv <- r$sig
    sigs[i] <- if (is.null(sv)) NA_character_ else sv
  }
  refs <- ref_of(rows, cols)

  write_runs <- function(idx, values) {
    if (!length(idx)) return(invisible(NULL))
    ord <- order(cols[idx], rows[idx])
    idx <- idx[ord]
    values <- values[ord]
    brk <- c(TRUE, diff(rows[idx]) != 1L | diff(cols[idx]) != 0L)
    grp <- cumsum(brk)
    for (k in unique(grp)) {
      sel <- idx[grp == k]
      wb$add_data(sheet = sheet,
                  dims = openxlsx2::wb_dims(rows = rows[sel], cols = cols[sel][1L]),
                  x = values[grp == k], col_names = FALSE)
    }
  }

  num_idx <- which(has_num)
  write_runs(num_idx, nums[num_idx])

  plain_idx <- which(!has_num & !rich & has_txt)
  write_runs(plain_idx, html_strip_fast(txts[plain_idx]))

  for (i in which(!has_num & rich & has_txt)) {
    r <- recs[[i]]
    wb$add_data(
      sheet = sheet, dims = refs[i], col_names = FALSE,
      x = html_to_fmt(
        r$text,
        font = fld(r, "font") %||% theme$font,
        size = fld(r, "size") %||% theme$size,
        color = fld(r, "color") %||% theme$color,
        bold = isTRUE(fld(r, "bold")), italic = isTRUE(fld(r, "italic")),
        base_px = theme$base_px,
        resolver = theme$resolver
      )
    )
  }

  # One call per distinct style. Handing the cells over as ranges rather than
  # chunked ref lists was measured and is slower: openxlsx2 expands a range to
  # its cells either way, so the ref list costs nothing extra.
  for (idx in group_by_sig(recs, c("font", "size", "color", "bold", "italic",
                                   "underline", "strike"), sigs)) {
    r <- recs[[idx[1L]]]
    for (part in chunked(idx)) {
      wb$add_font(
        sheet = sheet, dims = paste0(refs[part], collapse = ","),
        name = fld(r, "font") %||% theme$font,
        size = as.character(fld(r, "size") %||% theme$size),
        color = wbc(fld(r, "color") %||% theme$color) %||%
          openxlsx2::wb_color(hex = "FF000000"),
        bold = if (isTRUE(fld(r, "bold"))) "1" else "",
        italic = if (isTRUE(fld(r, "italic"))) "1" else "",
        underline = if (isTRUE(fld(r, "underline"))) "single" else "",
        strike = if (isTRUE(fld(r, "strike"))) "1" else ""
      )
    }
  }

  filled <- which(vapply(recs, function(r) !is.null(fld(r, "fill")), logical(1L)))
  if (length(filled)) {
    for (idx in group_by_sig(recs[filled], "fill", sigs[filled])) {
      sub <- filled[idx]
      for (part in chunked(sub)) {
        wb$add_fill(sheet = sheet, dims = paste0(refs[part], collapse = ","),
                    color = wbc(fld(recs[[sub[1L]]], "fill")))
      }
    }
  }

  for (idx in group_by_sig(recs, c("halign", "valign", "wrap", "indent"), sigs)) {
    r <- recs[[idx[1L]]]
    if (is.null(fld(r, "halign")) && is.null(fld(r, "valign")) && !isTRUE(r$wrap) &&
          is.null(fld(r, "indent")) && is.null(fld(r, "rotation"))) next
    args <- list(
      horizontal = fld(r, "halign"),
      vertical = fld(r, "valign") %||% "bottom",
      wrapText = if (isTRUE(r$wrap)) "1" else NULL,
      wrap_text = if (isTRUE(r$wrap)) "1" else NULL,
      indent = if (is.null(fld(r, "indent"))) NULL else as.character(fld(r, "indent")),
      text_rotation = if (is.null(fld(r, "rotation"))) {
        NULL
      } else {
        as.character(fld(r, "rotation"))
      }
    )
    args <- known_args(openxlsx2::wb_add_cell_style, args)
    args <- args[!vapply(args, is.null, logical(1L))]
    for (part in chunked(idx)) {
      do.call(wb$add_cell_style,
              c(list(sheet = sheet, dims = paste0(refs[part], collapse = ",")), args))
    }
  }

  for (f in unique(nfmt[!is.na(nfmt)])) {
    idx <- which(!is.na(nfmt) & nfmt == f)
    for (part in chunked(idx)) {
      wb$add_numfmt(sheet = sheet, dims = paste0(refs[part], collapse = ","), numfmt = f)
    }
  }

  # Excel only flags text that it would rather have seen as a number or date,
  # so only those cells need an ignoredError entry
  looks_numeric <- function(x) {
    x <- trimws(x)
    x <- sub("^\\((.*)\\)$", "-\\1", x)
    x <- gsub("[ ,\u00a0']", "", x)
    x <- sub("^[\u00a4\u0024\u00a2\u00a3\u00a5\u20a0-\u20bf]", "", x)
    x <- sub("[\u00a4\u0024\u00a2\u00a3\u00a5\u20a0-\u20bf]$", "", x)
    grepl("^[-+]?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?%?$", x) |
      grepl("^[0-9]{1,4}[-/][0-9]{1,2}[-/][0-9]{1,4}$", x)
  }
  flag <- list()
  if (length(plain_idx)) {
    txt <- html_strip(vapply(recs[plain_idx], function(r) r$text, character(1L)))
    sel <- plain_idx[!is.na(txt) & looks_numeric(txt)]
    flag <- ref_runs(rows[sel], cols[sel])
  }

  for (m in cc$merges) {
    if (length(m$rows) * length(m$cols) < 2L) next
    wb$merge_cells(sheet = sheet, dims = openxlsx2::wb_dims(rows = m$rows, cols = m$cols))
  }

  invisible(flag)
}
