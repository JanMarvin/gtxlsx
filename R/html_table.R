xml_find <- function(x, xpath) xml2::xml_find_all(x, xpath)

# ---- stylesheet -------------------------------------------------------------

# Rules are reduced to their rightmost simple selector: gt emits "#abcd .gt_row",
# another emits ".cl-1234 td". Matching the last component on tag/class/id
# covers those without a full selector engine.
# Modern CSS nests rules, so the stylesheet is walked with a selector stack
# rather than a flat regex. Rules are then reduced to their rightmost simple
# selector: matching the last component on tag, class and id handles the scoped
# output of real generators without a full selector engine.
split_top <- function(x, sep = ",") {
  out <- character(0L)
  depth <- 0L
  buf <- ""
  for (ch in strsplit(x, "", fixed = TRUE)[[1]]) {
    if (ch == "(") depth <- depth + 1L
    if (ch == ")") depth <- depth - 1L
    if (ch == sep && depth == 0L) {
      out <- c(out, buf)
      buf <- ""
    } else {
      buf <- paste0(buf, ch)
    }
  }
  trimws(c(out, buf))
}

expand_is <- function(sel) {
  m <- regmatches(sel, regexpr(":is\\(([^()]*)\\)", sel))
  if (!length(m)) return(sel)
  inner <- split_top(gsub("^:is\\(|\\)$", "", m))
  unlist(lapply(inner, function(one) expand_is(sub(m, one, sel, fixed = TRUE))))
}

join_sel <- function(parent, child) {
  if (!length(parent) || !nzchar(parent)) return(sub("&", "", child, fixed = TRUE))
  if (grepl("&", child, fixed = TRUE)) gsub("&", parent, child, fixed = TRUE)
  else paste(parent, child)
}

walk_css <- function(txt, parent = "", acc = list()) {
  i <- 1L
  n <- nchar(txt)
  buf <- ""
  while (i <= n) {
    ch <- substr(txt, i, i)
    if (ch == "{") {
      depth <- 1L
      j <- i + 1L
      while (j <= n && depth > 0L) {
        cj <- substr(txt, j, j)
        if (cj == "{") depth <- depth + 1L
        if (cj == "}") depth <- depth - 1L
        j <- j + 1L
      }
      block <- substr(txt, i + 1L, j - 2L)
      prelude <- trimws(buf)
      buf <- ""
      i <- j
      if (grepl("^@", prelude)) {
        acc <- walk_css(block, parent, acc)
        next
      }
      sels <- unlist(lapply(split_top(prelude), function(s) {
        unlist(lapply(expand_is(s), function(e) join_sel(parent, e)))
      }))
      own <- gsub("[^{}]*\\{(?:[^{}]|\\{[^{}]*\\})*\\}", "", block, perl = TRUE)
      decl <- parse_css_decls(own)
      if (length(decl)) {
        for (sel in sels) acc[[length(acc) + 1L]] <- list(sel = sel, decl = decl)
      }
      for (sel in sels) acc <- walk_css(block, sel, acc)
      next
    }
    if (ch == "}") {
      buf <- ""
      i <- i + 1L
      next
    }
    buf <- paste0(buf, ch)
    i <- i + 1L
  }
  acc
}

parse_stylesheet <- function(doc) {
  css <- paste(xml2::xml_text(xml_find(doc, "//style")), collapse = "\n")
  if (!nzchar(css)) return(NULL)
  css <- gsub("/\\*.*?\\*/", "", css, perl = TRUE)

  flat <- walk_css(css)
  if (!length(flat)) return(NULL)

  out <- list()
  for (i in seq_along(flat)) {
    sel <- trimws(flat[[i]]$sel)
    # structural and state pseudo-classes are not evaluated here; applying such
    # a rule to every cell is worse than skipping it, so it is dropped
    skip <- paste0("::|:(nth-|first-|last-|only-|not\\(|has\\(|where\\(|hover|",
                   "focus|active|target|empty|checked|visited|link|disabled)")
    if (grepl(skip, sel, perl = TRUE)) next
    parts <- strsplit(sel, "[ >+~]+")[[1]]
    parts <- parts[nzchar(parts)]
    if (!length(parts)) next
    last <- utils::tail(parts, 1L)
    up <- utils::head(parts, -1L)
    anc <- sub("^\\.", "", unlist(regmatches(up, gregexpr("\\.[A-Za-z0-9_-]+", up))))
    anc <- c(anc, tolower(sub("[.#:\\[].*$", "", up)))
    anc <- anc[nzchar(anc)]
    tag <- sub("[.#:\\[].*$", "", last)
    cls <- sub("^\\.", "", regmatches(last, gregexpr("\\.[A-Za-z0-9_-]+", last))[[1]])
    id <- sub("^#", "", regmatches(last, regexpr("#[A-Za-z0-9_-]+", last)))
    # a component that names nothing (a bare pseudo-class) would match every
    # cell, so it is dropped rather than applied universally
    if (!nzchar(tag) && !length(cls) && !length(id)) next
    out[[length(out) + 1L]] <- list(
      tag = if (nzchar(tag)) tolower(tag) else NA_character_,
      classes = cls,
      id = if (length(id)) id else NA_character_,
      ancestors = anc,
      rank = length(cls) * 10L + length(anc) + (length(id) > 0L) * 100L,
      order = i,
      decl = flat[[i]]$decl
    )
  }
  out
}

css_for <- function(rules, tag, classes, id, ancestors = character(0L)) {
  if (!length(rules)) return(list())
  keep <- vapply(rules, function(r) {
    (is.na(r$tag) || identical(r$tag, tag)) &&
      (!length(r$classes) || all(r$classes %in% classes)) &&
      (is.na(r$id) || identical(r$id, id)) &&
      # descendant combinators are honoured to the extent of requiring the
      # ancestor classes to appear above this cell; without it a rule such as
      # ".lt-source-note td" would style every cell in the table
      (!length(r$ancestors) || all(r$ancestors %in% c(classes, ancestors)))
  }, logical(1L))
  rules <- rules[keep]
  if (!length(rules)) return(list())
  ord <- order(vapply(rules, `[[`, integer(1L), "rank"),
               vapply(rules, `[[`, integer(1L), "order"))
  decl <- list()
  imp <- list()
  for (r in rules[ord]) {
    decl[names(r$decl)] <- r$decl
    k <- attr(r$decl, "important")
    if (length(k)) imp[k] <- r$decl[k]
  }
  if (length(imp)) decl[names(imp)] <- imp
  decl
}

# ---- declarations -> cell record --------------------------------------------

decl_to_rec <- function(decl, base_px) {
  out <- list()
  g <- function(k) decl[[k]]

  bg <- g("background-color") %||% g("background")
  if (!is.null(bg)) {
    # only reduce a "background" shorthand to its first token when that cannot
    # split an rgb()/rgba() value apart
    hex <- css_color(if (grepl("(", bg, fixed = TRUE)) bg else sub("^([^ ]+).*$", "\\1", bg))
    if (!is.null(hex) && !identical(substr(hex, 1L, 2L), "00")) out$fill <- hex
  }
  if (!is.null(g("color"))) out$color <- css_color(g("color"))
  if (!is.null(g("font-family"))) out$font <- pick_font(g("font-family"))
  if (!is.null(g("font-size"))) out$size <- css_pt(g("font-size"), base = base_px)
  if (!is.null(g("font-weight"))) out$bold <- is_bold(g("font-weight"))
  if (!is.null(g("font-style"))) {
    out$italic <- tolower(trimws(g("font-style"))) %in% c("italic", "oblique")
  }
  dec <- g("text-decoration") %||% g("text-decoration-line")
  if (!is.null(dec)) {
    out$underline <- grepl("underline", dec, fixed = TRUE)
    out$strike <- grepl("line-through", dec, fixed = TRUE)
  }
  if (!is.null(g("text-align"))) out$halign <- css_align(g("text-align"))
  if (!is.null(g("text-transform"))) out$transform <- g("text-transform")
  rot <- css_rotation(g("transform"))
  if (!is.null(rot)) out$rotation <- rot
  if (!is.null(g("vertical-align"))) out$valign <- css_valign(g("vertical-align"))
  ws <- g("white-space")
  if (!is.null(ws)) out$wrap <- !identical(tolower(trimws(ws)), "nowrap")

  borders <- list()
  long <- function(side, prop) {
    g(paste0("border-", side, "-", prop)) %||%
      shorthand_part(g(paste0("border-", side)), prop) %||%
      g(paste0("border-", prop)) %||%
      shorthand_part(g("border"), prop)
  }
  if (length(decl) && any(startsWith(names(decl) %||% "", "border"))) {
    for (side in c("top", "bottom", "left", "right")) {
      b <- css_border(long(side, "style"), long(side, "width"), base = base_px)
      if (is.null(b) || identical(b, "none")) next
      borders[[side]] <- list(border = b,
                              color = css_color(long(side, "color")) %||% "FF000000")
    }
  }
  out$borders <- if (length(borders)) borders else NULL
  out
}

# Nearly every cell resolves to one of a handful of declaration sets, so the
# translation to a cell record is memoised on them.
decl_to_rec_cached <- function(decl, base_px, cache) {
  key <- paste0(names(decl), "\r", unlist(decl, use.names = FALSE), collapse = "\n")
  hit <- cache[[key]]
  if (is.null(hit)) {
    hit <- decl_to_rec(decl, base_px)
    hit$sig <- key
    cache[[key]] <- hit
  }
  hit
}

shorthand_part <- function(x, what) {
  if (is.null(x) || is.na(x)) return(NULL)
  parts <- strsplit(trimws(x), "\\s+")[[1]]
  styles <- c("none", "hidden", "solid", "dashed", "dotted", "double", "groove",
              "ridge", "inset", "outset")
  is_style <- tolower(parts) %in% styles
  is_width <- grepl("^[0-9.]+(px|pt|em|rem)?$|^(thin|medium|thick)$", parts)
  hit <- switch(what,
                style = parts[is_style],
                width = parts[is_width],
                color = parts[!is_style & !is_width],
                character(0L))
  if (length(hit)) hit[1L] else NULL
}

# ---- table grid -------------------------------------------------------------

# Walk the rows filling an occupancy matrix so that rowspan and colspan cells
# land on the column they actually occupy.
html_grid <- function(tbl) {
  # only this table's own rows: "//tr" would descend into a nested table, and
  # the DOM order of tfoot is not its rendered order
  sect <- function(path) {
    n <- xml_find(tbl, path)
    if (length(n)) as.list(n) else list()
  }
  parts <- list(head = sect("./thead/tr"),
                body = c(sect("./tbody/tr"), sect("./tr")),
                foot = sect("./tfoot/tr"))
  rows <- unlist(parts, recursive = FALSE)
  sec <- rep(names(parts), lengths(parts))
  n <- length(rows)
  if (!n) return(NULL)
  taken <- matrix(FALSE, nrow = n, ncol = 64L)
  cells <- vector("list", 4L * n)
  nc <- 0L

  grow <- function(need) {
    if (need > ncol(taken)) {
      taken <<- cbind(taken, matrix(FALSE, nrow = nrow(taken), ncol = need - ncol(taken)))
    }
  }
  span <- function(node, attr, default) {
    v <- suppressWarnings(as.integer(xml2::xml_attr(node, attr)))
    if (is.na(v)) return(1L)
    if (v == 0L) return(default)
    max(v, 1L)
  }

  for (i in seq_len(n)) {
    # xml_children() is markedly cheaper than an xpath evaluated per row
    kids <- xml2::xml_children(rows[[i]])
    if (length(kids)) {
      kids <- kids[tolower(xml2::xml_name(kids)) %in% c("td", "th")]
    }
    j <- 1L
    for (k in seq_along(kids)) {
      node <- kids[[k]]
      while (j <= ncol(taken) && taken[i, j]) j <- j + 1L
      cs <- span(node, "colspan", 1L)
      # rowspan="0" runs to the end of its section, not the end of the table
      rs <- span(node, "rowspan", max(which(sec == sec[i])) - i + 1L)
      grow(j + cs - 1L)
      taken[seq.int(i, min(i + rs - 1L, n)), seq.int(j, j + cs - 1L)] <- TRUE
      nc <- nc + 1L
      if (nc > length(cells)) length(cells) <- 2L * length(cells)
      cells[[nc]] <- list(
        node = node, row = i, col = j, rowspan = min(rs, n - i + 1L), colspan = cs,
        tag = tolower(xml2::xml_name(node)), tr = rows[[i]]
      )
      j <- j + cs
    }
  }
  cells <- cells[seq_len(nc)]
  used <- vapply(cells, function(z) z$col + z$colspan - 1L, integer(1L))
  list(cells = cells, nrow = n, ncol = if (length(used)) max(used) else 0L,
       cols = xml_find(tbl, "./colgroup/col|./col"))
}

# Legacy tables carry their styling in attributes rather than CSS, and plenty
# of real pages still do: bgcolor, align, valign, width, nowrap, border.
pres_decls <- function(node) {
  a <- function(n) {
    v <- xml2::xml_attr(node, n)
    if (is.na(v) || !nzchar(trimws(v))) NULL else trimws(v)
  }
  d <- list()
  if (!is.null(a("bgcolor"))) d[["background-color"]] <- a("bgcolor")
  if (!is.null(a("color"))) d[["color"]] <- a("color")
  if (!is.null(a("align"))) d[["text-align"]] <- a("align")
  if (!is.null(a("valign"))) d[["vertical-align"]] <- a("valign")
  if (!is.null(a("face"))) d[["font-family"]] <- a("face")
  if (!is.na(xml2::xml_attr(node, "nowrap"))) d[["white-space"]] <- "nowrap"
  w <- a("width")
  if (!is.null(w)) d[["width"]] <- if (grepl("^[0-9.]+$", w)) paste0(w, "px") else w
  d
}

node_decls <- function(node, rules, ancestors = NULL) {
  if (!length(node)) return(list())
  if (is.null(ancestors)) ancestors <- node_ancestors(node)
  d <- css_for(rules, tolower(xml2::xml_name(node)),
               strsplit(xml2::xml_attr(node, "class") %||% "", "\\s+")[[1]],
               xml2::xml_attr(node, "id"), ancestors)
  p <- pres_decls(node)
  d[names(p)] <- p
  inline <- parse_css_decls(xml2::xml_attr(node, "style"))
  d[names(inline)] <- inline
  d
}

# the classes and tag names of everything above a cell, used to approximate
# descendant combinators
node_ancestors <- function(node) {
  out <- character(0L)
  p <- xml2::xml_parent(node)
  while (length(p) && !identical(xml2::xml_name(p), "html")) {
    out <- c(out, tolower(xml2::xml_name(p)),
             strsplit(xml2::xml_attr(p, "class") %||% "", "\\s+")[[1]])
    p <- xml2::xml_parent(p)
  }
  out[nzchar(out)]
}

inner_html <- function(node) {
  s <- as.character(node)
  s <- sub("^<[^>]*>", "", s)
  s <- sub("</[a-zA-Z0-9]+>\\s*$", "", s)
  trimws(s)
}

# ---- writer -----------------------------------------------------------------

#' Write an HTML table into a worksheet
#'
#' Reads the first (or any) `<table>` of an HTML fragment or document and
#' writes it as cells. `colspan` and `rowspan` become merged ranges, `<style>`
#' rules and `style=` attributes become fills, fonts, alignment and borders,
#' and markup inside a cell becomes rich text. Titles and notes that sit
#' beside the table rather than inside it are picked up as well.
#'
#' This is the general path for tables that are already HTML, whatever
#' produced them. Old fashioned presentational markup is understood too:
#' `bgcolor`, `align`, `valign`, `width`, `nowrap` and `<table border>`.
#'
#' @section How much CSS is understood:
#' Enough for tables, not enough to call it a browser. Selectors are matched
#' on their rightmost part — tag, class and id — with the classes of ancestor
#' elements checked as well, which is what makes rules like
#' `table.report td.total` and `thead td` work. Nested rules, `:is()` and `!important` are
#' handled. What is not: `:nth-child()` and other structural or state
#' pseudo-classes are skipped rather than applied to every cell, `>` and `+`
#' behave like a plain descendant combinator, attribute selectors match on the
#' tag alone, and stylesheets pulled in with `<link>` are not fetched.
#'
#' Properties with no spreadsheet equivalent — gradients, letter spacing,
#' rounded corners — are dropped. `<img>` and `<svg>` leave an empty cell, and
#' `<a href>` keeps its text but not the link.
#'
#' @param wb A `wbWorkbook` object.
#' @param x HTML: a string, a file path, or anything with an `as.character()`
#'   method that returns HTML.
#' @param sheet The worksheet to write to. Defaults to the current sheet.
#' @param dims Cell reference of the top left corner.
#' @param which Which table in the document to write, when there is more than
#'   one.
#' @param numeric Write cells as numbers where the text is plainly a number.
#'   Only symbol prefixes and suffixes such as `$` or `%` are converted, so a
#'   label like `"458 Speciale"` stays text.
#' @param col_widths `"auto"` measures the rendered text, a numeric vector sets
#'   the widths directly, `NULL` leaves them alone.
#' @param ignore_errors Mark text cells that look numeric, so Excel does not
#'   flag them.
#' @param context Pick up block elements sitting beside the table — headings
#'   above it, notes below it — and write them as merged rows. Set to `FALSE`
#'   to write the table on its own.
#' @param ... Currently unused.
#'
#' @return The workbook, invisibly.
#'
#' @seealso [wb_add_gt()], which goes straight from a `gt` object and keeps
#'   more of the structure.
#'
#' @examples
#' library(openxlsx2)
#'
#' html <- paste0(
#'   "<style>th { background-color: #204060; color: white; }</style>",
#'   "<table><tr><th>Account</th><th>Change</th></tr>",
#'   "<tr><td>Cash</td><td>1,204.50</td></tr></table>"
#' )
#'
#' wb <- wb_workbook()$add_worksheet()
#' wb <- wb_add_html(wb, html, dims = "A1")
#'
#' wb_to_df(wb, col_names = FALSE)
#'
#' @export
wb_add_html <- function(wb, x, sheet = current_sheet(), dims = "A1", which = 1L,
                        numeric = TRUE, col_widths = "auto",
                        ignore_errors = TRUE, context = TRUE, ...) {
  if (!inherits(wb, "wbWorkbook")) {
    stop("`wb` must be a 'wbWorkbook' object", call. = FALSE)
  }
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("package 'xml2' is required to read HTML", call. = FALSE)
  }
  wb <- wb$clone()

  looks_like_path <- length(x) == 1L && is.character(x) && nchar(x) < 1024L &&
    !grepl("[<\n]", x) && file.exists(x)
  html <- if (looks_like_path) {
    xml2::read_html(x)
  } else if (inherits(x, "xml_document")) {
    x
  } else {
    xml2::read_html(paste(as.character(x), collapse = "\n"))
  }

  tables <- xml_find(html, "//table")
  if (!length(tables)) stop("no <table> found", call. = FALSE)
  if (which > length(tables)) stop("`which` is past the last table", call. = FALSE)
  tbl <- tables[[which]]

  rules <- parse_stylesheet(html)
  base_px <- 16
  root <- css_for(rules, "table", strsplit(xml2::xml_attr(tbl, "class") %||% "", "\\s+")[[1]],
                  xml2::xml_attr(tbl, "id"))
  if (!is.null(root[["font-size"]])) {
    base_px <- css_px(root[["font-size"]], base = 16)
    if (is.na(base_px)) base_px <- 16
  }
  base_size <- round(base_px * 0.75, 1)
  base_font <- pick_font(root[["font-family"]])

  # crude inheritance: the properties CSS actually inherits, taken from the
  # table rule and used as the starting point for every cell
  inherited <- root[intersect(names(root),
                              c("color", "font-family", "font-size", "font-weight",
                                "font-style", "text-align", "white-space"))]

  g <- html_grid(tbl)
  if (is.null(g)) stop("the table has no rows", call. = FALSE)

  rc <- openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  row0 <- min(rc$row)
  col0 <- min(rc$col)

  # a table-wide border="1" is a presentational shorthand for thin gridlines
  bw <- suppressWarnings(as.integer(xml2::xml_attr(tbl, "border")))
  table_border <- if (!is.na(bw) && bw > 0L) {
    list(`border-style` = "solid", `border-width` = paste0(bw, "px"),
         `border-color` = "#808080")
  } else {
    list()
  }

  # Resolving the cascade means walking up the DOM and scanning every rule, and
  # cells repeat the same tag/class/ancestor combination over and over. Both
  # halves are cached: ancestors per row, resolved declarations per signature.
  rec_cache <- new.env(parent = emptyenv())
  anc_cache <- vector("list", g$nrow)
  anc_sig_cache <- character(g$nrow)
  row_ancestors <- function(cell) {
    if (is.null(anc_cache[[cell$row]])) {
      anc_cache[[cell$row]] <<- node_ancestors(cell$node)
    }
    anc_cache[[cell$row]]
  }
  # the cascade depends on what sits above a cell, so the cache key has to
  # carry the ancestor chain as well
  row_anc_sig <- function(cell) {
    if (!nzchar(anc_sig_cache[[cell$row]])) {
      anc_sig_cache[[cell$row]] <<- paste0(c("|", row_ancestors(cell)), collapse = ",")
    }
    anc_sig_cache[[cell$row]]
  }
  col_decl <- vector("list", max(g$ncol, 1L))
  if (length(g$cols)) {
    j <- 1L
    for (k in seq_along(g$cols)) {
      cn <- suppressWarnings(as.integer(xml2::xml_attr(g$cols[[k]], "span")))
      if (is.na(cn) || cn < 1L) cn <- 1L
      d <- node_decls(g$cols[[k]], rules)
      for (jj in seq.int(j, min(j + cn - 1L, length(col_decl)))) col_decl[[jj]] <- d
      j <- j + cn
    }
  }
  row_decl <- lapply(seq_len(g$nrow), function(i) list())
  seen <- integer(0L)
  for (cell in g$cells) {
    if (cell$row %in% seen) next
    seen <- c(seen, cell$row)
    row_decl[[cell$row]] <- node_decls(cell$tr, rules)
  }

  # One pass over every cell's text instead of one call per cell: extracting,
  # stripping and number parsing are all vectorised.
  cell_txt <- vapply(g$cells, function(z) as.character(z$node), character(1L))
  cell_txt <- sub("^<[^>]*>", "", cell_txt)
  cell_txt <- trimws(sub("</[a-zA-Z0-9]+>\\s*$", "", cell_txt))
  cell_plain <- html_strip_fast(cell_txt)
  cell_blank <- !nzchar(gsub("[\\s\u00a0]", "", cell_plain, perl = TRUE))
  cell_txt[cell_blank] <- ""
  cell_plain[cell_blank] <- ""
  cell_rich <- is_rich(cell_txt)
  cell_num <- if (numeric) {
    text_as_numbers(ifelse(cell_rich, NA_character_, cell_plain))
  } else {
    list(value = rep(NA_real_, length(cell_txt)),
         numfmt = rep(NA_character_, length(cell_txt)))
  }

  # Rows and columns rarely carry their own declarations; signing them lets the
  # per-cell cache key stay identical across a uniform table.
  decl_sig <- function(d) if (!length(d)) "" else paste0(names(d), unlist(d), collapse = "")
  row_sig <- vapply(row_decl, decl_sig, character(1L))
  col_sig <- vapply(col_decl, decl_sig, character(1L))

  cc <- new_sheet_cells()
  cc$borders <- list()

  # Titles and notes are often siblings of the table (or of its wrapper) rather
  # than a <caption>, so those blocks are collected too.
  sibling_blocks <- function(where) {
    if (!isTRUE(context)) return(list())
    out <- list()
    node <- tbl
    for (lvl in 1:3) {
      p <- xml2::xml_parent(node)
      if (!length(p) || tolower(xml2::xml_name(p)) %in% c("body", "html", "")) break
      sibs <- xml_find(node, paste0(where, "-sibling::div|", where, "-sibling::p|",
                                    where, "-sibling::h1|", where, "-sibling::h2|",
                                    where, "-sibling::h3|", where, "-sibling::h4"))
      for (k in seq_along(sibs)) {
        if (length(xml_find(sibs[[k]], ".//table"))) next
        if (!nzchar(trimws(xml2::xml_text(sibs[[k]])))) next
        out[[length(out) + 1L]] <- sibs[[k]]
      }
      node <- p
    }
    out
  }

  write_block <- function(node, r) {
    txt <- inner_html(node)
    if (!nzchar(html_strip(txt) %||% "")) return(FALSE)
    decl <- inherited
    own <- node_decls(node, rules)
    decl[names(own)] <- own
    rec <- decl_to_rec(decl, base_px)
    rec$borders <- NULL
    rec$transform <- NULL
    do.call(put_cell, c(
      list(cc, r, col0, txt),
      list(rich = is_rich(txt), font = rec$font %||% base_font,
           size = rec$size %||% base_size, halign = rec$halign %||% "left"),
      rec[setdiff(names(rec), c("font", "size", "halign"))]
    ))
    for (jj in seq_len(g$ncol - 1L)) {
      do.call(put_cell, c(list(cc, r, col0 + jj), rec[intersect(names(rec), "fill")]))
    }
    add_merge(cc, r, seq.int(col0, col0 + g$ncol - 1L))
    TRUE
  }

  cap_rows <- 0L
  for (node in sibling_blocks("preceding")) {
    if (write_block(node, row0 + cap_rows)) cap_rows <- cap_rows + 1L
  }

  # <caption> sits outside the row grid; its block children become merged rows
  # above the table
  caption <- xml_find(tbl, "./caption")
  if (length(caption)) {
    blocks <- xml_find(caption[[1L]], "./div|./p|./span")
    if (!length(blocks)) blocks <- caption
    for (k in seq_along(blocks)) {
      node <- blocks[[k]]
      txt <- inner_html(node)
      if (!nzchar(html_strip(txt) %||% "")) next
      cap_rows <- cap_rows + 1L
      decl <- inherited
      own <- css_for(rules, tolower(xml2::xml_name(node)),
                     strsplit(xml2::xml_attr(node, "class") %||% "", "\\s+")[[1]],
                     xml2::xml_attr(node, "id"), node_ancestors(node))
      decl[names(own)] <- own
      inline <- parse_css_decls(xml2::xml_attr(node, "style"))
      decl[names(inline)] <- inline
      rec <- decl_to_rec(decl, base_px)
      rec$borders <- NULL
      r <- row0 - 1L + cap_rows
      do.call(put_cell, c(
        list(cc, r, col0, txt),
        list(rich = is_rich(txt), font = rec$font %||% base_font,
             size = rec$size %||% base_size,
             halign = rec$halign %||% "center"),
        rec[setdiff(names(rec), c("font", "size", "halign"))]
      ))
      for (jj in seq_len(g$ncol - 1L)) {
        do.call(put_cell, c(list(cc, r, col0 + jj), rec[intersect(names(rec), "fill")]))
      }
      add_merge(cc, r, seq.int(col0, col0 + g$ncol - 1L))
    }
  }
  row0 <- row0 + cap_rows

  for (ci in seq_along(g$cells)) {
    cell <- g$cells[[ci]]
    node <- cell$node
    # The whole style pipeline for a cell depends only on its tag, its own
    # class/style attributes, its row and column, and its wrapper chain. Cells
    # repeat those endlessly, so the finished record is cached on them.
    cls <- xml2::xml_attr(node, "class")
    sty <- xml2::xml_attr(node, "style")
    nkid <- xml2::xml_length(node, only_elements = TRUE)
    ckey <- paste(cell$tag, cls, sty, nkid, row_anc_sig(cell),
                  row_sig[cell$row], col_sig[cell$col], sep = "\r")
    rec <- rec_cache[[ckey]]

    if (is.null(rec)) {
      # browser defaults for <th>, then table, colgroup, row and cell in
      # increasing order of precedence
      decl <- if (identical(cell$tag, "th")) {
        list(`font-weight` = "bold", `text-align` = "center")
      } else {
        list()
      }
      anc <- row_ancestors(cell)

      # a cell whose content sits in a single wrapper chain (<td><p><span>text)
      # keeps its styling on those wrappers, so follow the chain down
      wrap <- list()
      inner <- node
      while (xml2::xml_length(inner, only_elements = TRUE) == 1L) {
        kid <- xml2::xml_children(inner)[[1L]]
        if (!tolower(xml2::xml_name(kid)) %in% c("p", "div", "span")) break
        inner <- kid
        w <- node_decls(inner, rules, anc)
        # a wrapper routinely declares background-color:transparent, which must
        # not wipe the background the cell itself sets
        w <- w[!vapply(w, function(v) {
          tolower(trimws(as.character(v)[1L])) %in%
            c("transparent", "none", "inherit", "initial", "unset")
        }, logical(1L))]
        if (length(w)) wrap[names(w)] <- w
      }

      for (extra in list(inherited, table_border, col_decl[[cell$col]],
                         row_decl[[cell$row]], node_decls(node, rules, anc), wrap)) {
        if (length(extra)) decl[names(extra)] <- extra
      }
      rec <- decl_to_rec(decl, base_px)
      rec$sig <- ckey
      rec_cache[[ckey]] <- rec
    }
    borders <- rec$borders

    txt <- cell_txt[[ci]]
    if (!is.null(rec$transform)) txt <- apply_transform(txt, rec$transform)

    r <- row0 - 1L + cell$row
    j <- col0 - 1L + cell$col

    num <- if (is.na(cell_num$value[ci])) {
      NULL
    } else {
      list(value = cell_num$value[ci], numfmt = cell_num$numfmt[ci])
    }

    style <- rec[setdiff(names(rec), c("transform", "sig", "borders"))]
    args <- list(
      cc, r, j, txt,
      rich = cell_rich[[ci]], style = style, sig = rec$sig,
      wrap = isTRUE(style$wrap) || grepl("<br", txt, fixed = TRUE)
    )
    if (!is.null(num)) {
      args$num <- num$value
      args$numfmt <- num$numfmt
    }
    do.call(put_cell, args)

    rows_i <- seq.int(r, r + cell$rowspan - 1L)
    cols_j <- seq.int(j, j + cell$colspan - 1L)
    if (cell$rowspan > 1L || cell$colspan > 1L) {
      for (rr in rows_i) {
        for (jj in cols_j) {
          if (rr == r && jj == j) next
          do.call(put_cell, c(list(cc, rr, jj), rec[intersect(names(rec), "fill")]))
        }
      }
      add_merge(cc, rows_i, cols_j)
    }
    for (side in names(borders)) {
      cc$borders[[length(cc$borders) + 1L]] <- list(
        rows = rows_i, cols = cols_j, side = side,
        border = borders[[side]]$border, color = borders[[side]]$color
      )
    }
  }

  theme <- list(font = base_font, size = base_size, color = NULL, base_px = base_px,
                resolver = function(tag, classes) css_for(rules, tag, classes, NA_character_))
  foot_row <- row0 + g$nrow
  for (node in sibling_blocks("following")) {
    if (write_block(node, foot_row)) foot_row <- foot_row + 1L
  }

  flagged <- render_cells(wb, sheet, cc, theme)

  apply_borders(wb, sheet, cc$borders)

  if (!is.null(col_widths)) {
    cols <- col0 - 1L + seq_len(g$ncol)
    if (is.numeric(col_widths)) {
      wb$set_col_widths(sheet = sheet, cols = cols,
                        widths = rep_len(col_widths, length(cols)))
    } else {
      w <- html_col_widths(g, cell_plain, base_size)
      for (jj in seq_along(col_decl)) {
        cw <- col_decl[[jj]][["width"]]
        if (!is.null(cw)) {
          v <- px_to_width(css_px(cw))
          if (!is.na(v)) w[jj] <- v
        }
      }
      sel <- !is.na(w)
      if (any(sel)) {
        wb$set_col_widths(sheet = sheet, cols = cols[sel], widths = round(w[sel], 2))
      }
    }
  }

  if (isTRUE(ignore_errors)) {
    for (rng in flagged) {
      wb$add_ignore_error(sheet = sheet, dims = rng,
                          number_stored_as_text = TRUE, two_digit_text_year = TRUE)
    }
  }

  invisible(wb)
}

# The rendered text is the only truth an HTML table carries, so a cell such as
# "$447,000" can be turned into a number plus a format without a round trip
# check against source data.
text_as_number <- function(x) {
  if (is.na(x) || !nzchar(x) || !grepl("[0-9]", x, fixed = FALSE)) return(NULL)
  p <- parse_num_strings(trimws(x))
  if (is.null(p)) return(NULL)
  # without source data to check against, only symbol affixes are safe: a label
  # like "458 Speciale" would otherwise become the number 458
  if (grepl("[0-9A-Za-z]", paste0(p$pre, p$suf))) return(NULL)
  neg <- nzchar(p$neg) || grepl("[-\u2212]", p$pre)
  pre <- gsub("[-\u2212]", "", p$pre)
  grp <- nzchar(p$grp)
  dec_n <- nchar(p$dec)
  v <- p$int
  if (grp) v <- gsub(p$grp, "", v, fixed = TRUE)
  if (nzchar(p$decm)) v <- paste0(v, ".", p$dec)
  v <- suppressWarnings(as.numeric(v))
  if (is.na(v)) return(NULL)
  if (neg) v <- -v
  pct <- identical(trimws(p$suf), "%")
  if (pct) v <- v / 100
  body <- if (grp) "#,##0" else "0"
  if (dec_n > 0L) body <- paste0(body, ".", strrep("0", dec_n))
  list(value = v,
       numfmt = paste0(quote_affix(trimws(pre)), body,
                       quote_affix(if (pct) "%" else trimws(p$suf))))
}

html_col_widths <- function(g, plain, base_size) {
  w <- rep(NA_real_, g$ncol)
  for (ci in seq_along(g$cells)) {
    cell <- g$cells[[ci]]
    if (cell$colspan > 1L) next
    txt <- plain[[ci]]
    if (is.na(txt) || !nzchar(txt)) next
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    if (!length(lines)) next
    v <- max(nchar(lines)) * (base_size / 11) + 2.6
    w[cell$col] <- max(w[cell$col], v, na.rm = TRUE)
  }
  pmin(pmax(w, 4), 80)
}

#' Write an lt table into a worksheet
#'
#' The `lt` package builds its HTML in JavaScript when the page is viewed, so
#' there is no table to read on the R side. This helper asks `lt` to bake the
#' table to static HTML first and then hands the result to [wb_add_html()].
#'
#' Baking needs Node.js or a Chromium based browser on the machine.
#'
#' @param wb A `wbWorkbook` object.
#' @param x An `lt_tbl` object.
#' @param sheet The worksheet to write to.
#' @param dims Cell reference of the top left corner.
#' @param method How `lt` should bake the table: `"node"` or `"browser"` to
#'   force a renderer, `"auto"` to use whichever is available.
#' @param ... Passed on to [wb_add_html()].
#'
#' @return The workbook, invisibly.
#'
#' @examples
#' # needs the lt package and a Node.js or browser install
#' \dontrun{
#' library(openxlsx2)
#'
#' tbl <- lt::lt(data.frame(a = c("x", "y"), n = c(1234.5, 67.89)))
#' tbl <- lt::lt_format(tbl, ~ n, decimals = 2, big_mark = ",")
#'
#' wb <- wb_workbook()$add_worksheet()
#' wb <- wb_add_lt(wb, tbl, dims = "B2")
#' }
#'
#' @export
wb_add_lt <- function(wb, x, sheet = current_sheet(), dims = "A1",
                      method = "auto", ...) {
  if (!requireNamespace("lt", quietly = TRUE)) {
    stop("package 'lt' is required", call. = FALSE)
  }
  if (!inherits(x, "lt_tbl")) {
    stop("`x` must be an 'lt_tbl' object", call. = FALSE)
  }
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)
  lt::lt_export(x, f, method = method, fragment = TRUE, css = TRUE)
  wb_add_html(wb, paste(readLines(f, warn = FALSE), collapse = "\n"),
              sheet = sheet, dims = dims, ...)
}
