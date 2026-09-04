# Nodes are integer indices into the parsed document; `doc` travels with them.

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

# A rule's own declarations are everything outside its nested blocks. Nesting
# goes deeper than a regex can follow, so the braces are matched by hand and
# each nested selector (the text after the last ";") is dropped with its block.
own_decls <- function(txt) {
  out <- ""
  i <- 1L
  n <- nchar(txt)
  while (i <= n) {
    ch <- substr(txt, i, i)
    if (ch == "{") {
      out <- sub("[^;]*$", "", out)
      depth <- 1L
      j <- i + 1L
      while (j <= n && depth > 0L) {
        cj <- substr(txt, j, j)
        if (cj == "{") depth <- depth + 1L
        if (cj == "}") depth <- depth - 1L
        j <- j + 1L
      }
      i <- j
      next
    }
    out <- paste0(out, ch)
    i <- i + 1L
  }
  out
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
      # declarations that precede a nested rule end in ";" and must not be
      # taken for part of that rule's selector
      prelude <- trimws(sub("^.*;", "", buf))
      buf <- ""
      i <- j
      if (grepl("^@", prelude)) {
        acc <- walk_css(block, parent, acc)
        next
      }
      sels <- unlist(lapply(split_top(prelude), function(s) {
        unlist(lapply(expand_is(s), function(e) join_sel(parent, e)))
      }))
      decl <- parse_css_decls(own_decls(block))
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

# Only the positional pseudo-classes can be judged from a table's structure;
# anything else (state, :has(), ::before) is still dropped rather than guessed.
pos_pseudos <- function(component) {
  pat <- paste0(":(not\\(:?)?(first-child|last-child|only-child|",
                "nth-child\\([^()]*\\))\\)?")
  hits <- regmatches(component, gregexpr(pat, component, perl = TRUE))[[1]]
  lapply(hits, function(h) {
    neg <- grepl(":not(", h, fixed = TRUE)
    body <- sub("^:(not\\(:?)?", "", sub("\\)$", "", h))
    if (grepl("^nth-child", body)) {
      list(kind = "nth", arg = tolower(sub("^nth-child\\(([^()]*)\\).*$", "\\1", body)),
           negate = neg)
    } else {
      list(kind = sub("\\).*$", "", body), negate = neg)
    }
  })
}

unsupported_pseudo <- function(sel) {
  pat <- paste0(":(not\\(:?)?(first-child|last-child|only-child|",
                "nth-child\\([^()]*\\))\\)?")
  cleaned <- gsub(pat, "", sel, perl = TRUE)
  grepl("::|:[a-z]", cleaned, perl = TRUE)
}

pos_ok <- function(cons, i, n) {
  if (!length(cons)) return(TRUE)
  if (is.na(i) || is.na(n)) return(FALSE)
  for (c0 in cons) {
    ok <- switch(c0$kind,
                 "first-child" = i == 1L,
                 "last-child" = i == n,
                 "only-child" = n == 1L,
                 "nth" = if (identical(c0$arg, "even")) {
                   i %% 2L == 0L
                 } else if (identical(c0$arg, "odd")) {
                   i %% 2L == 1L
                 } else {
                   k <- suppressWarnings(as.integer(c0$arg))
                   !is.na(k) && i == k
                 },
                 TRUE)
    if (isTRUE(c0$negate)) ok <- !ok
    if (!ok) return(FALSE)
  }
  TRUE
}

# A selector is kept as an ordered chain of components, each with the
# combinator that precedes it, so that "div > td" can be told apart from
# "div td". Sibling combinators are not evaluated and cause the rule to be
# dropped.
split_selector <- function(sel) {
  toks <- regmatches(sel, gregexpr("[>+~]|[^\\s>+~]+", sel, perl = TRUE))[[1]]
  toks <- trimws(toks)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(NULL)
  comb <- " "
  out <- list()
  for (tk in toks) {
    if (tk %in% c(">", "+", "~")) {
      comb <- tk
      next
    }
    out[[length(out) + 1L]] <- list(text = tk, comb = comb)
    comb <- " "
  }
  out
}

parse_component <- function(x) {
  tag <- sub("[.#:\\[].*$", "", x)
  list(
    tag = if (nzchar(tag)) tolower(tag) else NA_character_,
    classes = sub("^\\.", "", regmatches(x, gregexpr("\\.[A-Za-z0-9_-]+", x))[[1]]),
    id = {
      hit <- regmatches(x, regexpr("#[A-Za-z0-9_-]+", x))
      if (length(hit)) sub("^#", "", hit) else NA_character_
    },
    pos = pos_pseudos(x)
  )
}

component_ok <- function(cmp, tag, classes, id) {
  (is.na(cmp$tag) || identical(cmp$tag, tag)) &&
    (!length(cmp$classes) || all(cmp$classes %in% classes)) &&
    (is.na(cmp$id) || identical(cmp$id, id))
}

# Walk the chain right to left against the cell's ancestors. A child
# combinator has to match the very next level up; a descendant combinator may
# skip any number of levels.
chain_ok <- function(chain, levels, row_i, row_n) {
  if (length(chain) <= 1L) return(TRUE)
  i <- 1L
  for (k in seq(length(chain) - 1L, 1L)) {
    cmp <- chain[[k]]
    nxt <- chain[[k + 1L]]$comb
    if (identical(nxt, ">")) {
      if (i > length(levels)) return(FALSE)
      lv <- levels[[i]]
      if (!component_ok(cmp, lv$tag, lv$classes, lv$id)) return(FALSE)
      if (!pos_ok(cmp$pos, lv$pos_i, lv$pos_n)) return(FALSE)
      i <- i + 1L
    } else {
      found <- FALSE
      while (i <= length(levels)) {
        lv <- levels[[i]]
        i <- i + 1L
        if (component_ok(cmp, lv$tag, lv$classes, lv$id) &&
              pos_ok(cmp$pos, lv$pos_i, lv$pos_n)) {
          found <- TRUE
          break
        }
      }
      if (!found) return(FALSE)
    }
  }
  TRUE
}

parse_stylesheet <- function(doc) {
  css <- paste(vapply(nd_find_all(doc, "style"), function(i) nd_inner(doc, i),
                      character(1L)), collapse = "\n")
  if (!nzchar(css)) return(NULL)
  css <- gsub("/\\*.*?\\*/", "", css, perl = TRUE)

  flat <- walk_css(css)
  if (!length(flat)) return(NULL)

  out <- list()
  for (i in seq_along(flat)) {
    sel <- trimws(flat[[i]]$sel)
    # structural and state pseudo-classes are not evaluated here; applying such
    # a rule to every cell is worse than skipping it, so it is dropped
    if (unsupported_pseudo(sel)) next
    if (grepl("[+~]", sel)) next
    toks <- split_selector(sel)
    if (is.null(toks)) next
    chain <- lapply(toks, function(t) c(parse_component(t$text), list(comb = t$comb)))
    last <- chain[[length(chain)]]
    # a component that names nothing (a bare pseudo-class) would match every
    # cell, so it is dropped rather than applied universally
    if (is.na(last$tag) && !length(last$classes) && is.na(last$id)) next
    nc <- sum(vapply(chain, function(c0) length(c0$classes), integer(1L)))
    nid <- sum(vapply(chain, function(c0) !is.na(c0$id), logical(1L)))
    out[[length(out) + 1L]] <- list(
      tag = last$tag,
      classes = last$classes,
      id = last$id,
      chain = chain,
      own_pos = last$pos,
      rank = nc * 10L + length(chain) + nid * 100L,
      order = i,
      decl = flat[[i]]$decl
    )
  }
  out
}

css_for <- function(rules, tag, classes, id, ancestors = list(), pos = NULL) {
  if (!length(rules)) return(list())
  keep <- vapply(rules, function(r) {
    if (length(r$own_pos)) {
      if (is.null(pos)) return(FALSE)
      if (!pos_ok(r$own_pos, pos$cell_i, pos$cell_n)) return(FALSE)
    }
    if (!(is.na(r$tag) || identical(r$tag, tag))) return(FALSE)
    if (length(r$classes) && !all(r$classes %in% classes)) return(FALSE)
    if (!(is.na(r$id) || identical(r$id, id))) return(FALSE)
    chain_ok(r$chain, ancestors, pos$row_i, pos$row_n)
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

# CSS custom properties: --bd: 1.5px solid #888 then border-top: var(--bd).
# Values are substituted before anything tries to read them.
resolve_vars <- function(decl, vars) {
  need <- vapply(decl, function(v) grepl("var(", v, fixed = TRUE), logical(1L))
  if (!any(need)) return(decl)
  pat <- "var\\(\\s*--[A-Za-z0-9_-]+\\s*(,[^()]*)?\\)"
  for (k in names(decl)[need]) {
    v <- decl[[k]]
    for (i in seq_len(4L)) {
      m <- regmatches(v, regexpr(pat, v))
      if (!length(m)) break
      nm <- sub("^var\\(\\s*(--[A-Za-z0-9_-]+).*$", "\\1", m)
      rep <- vars[[nm]]
      if (is.null(rep)) {
        fb <- sub("\\)$", "", sub("^var\\([^,]*,\\s*", "", m))
        rep <- if (identical(fb, m)) "" else fb
      }
      v <- sub(m, rep, v, fixed = TRUE)
    }
    decl[[k]] <- trimws(v)
  }
  decl
}

css_var_defs <- function(decl) {
  if (!length(decl)) return(list())
  decl[startsWith(names(decl) %||% character(0L), "--")]
}

decl_to_rec <- function(decl, base_px) {
  out <- list()
  g <- function(k) decl[[k]]

  bg <- g("background-color") %||% g("background")
  if (!is.null(bg)) {
    # only reduce a "background" shorthand to its first token when that cannot
    # split an rgb()/rgba() value apart
    first <- if (grepl("(", bg, fixed = TRUE)) bg else sub("^([^ ]+).*$", "\\1", bg)
    hex <- css_color(first)
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
      col <- long(side, "color")
      # a transparent border is a spacer in CSS and nothing at all in a sheet
      if (!is.null(col) && tolower(trimws(col)) %in% c("transparent", "none")) next
      borders[[side]] <- list(border = b, color = css_color(col) %||% "FF000000")
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
html_grid <- function(doc, tbl) {
  # only this table's own rows: "//tr" would descend into a nested table, and
  # the DOM order of tfoot is not its rendered order
  # a <tr> with no cells is a spacer the page uses for layout; it carries
  # nothing and would land as a blank row in the sheet
  has_cells <- function(rows) {
    rows[vapply(rows, function(r) length(nd_children(doc, r, c("td", "th"))) > 0L,
                logical(1L))]
  }
  sect <- function(tags) {
    if (is.null(tags)) return(as.list(has_cells(nd_children(doc, tbl, "tr"))))
    out <- integer(0L)
    for (k in nd_children(doc, tbl, tags)) out <- c(out, nd_children(doc, k, "tr"))
    as.list(has_cells(out))
  }
  parts <- list(head = sect("thead"),
                body = c(sect("tbody"), sect(NULL)),
                foot = sect("tfoot"))
  rows <- unlist(parts, recursive = FALSE)
  sec <- rep(names(parts), lengths(parts))
  n <- length(rows)
  if (!n) return(NULL)
  grid <- new.env(parent = emptyenv())
  grid$taken <- matrix(FALSE, nrow = n, ncol = 64L)
  cells <- vector("list", 4L * n)
  nc <- 0L

  grow <- function(need) {
    have <- ncol(grid$taken)
    if (need > have) {
      grid$taken <- cbind(grid$taken,
                          matrix(FALSE, nrow = nrow(grid$taken), ncol = need - have))
    }
  }
  span <- function(node, attr, default) {
    v <- suppressWarnings(as.integer(nd_attr(doc, node, attr)))
    if (is.na(v)) return(1L)
    if (v == 0L) return(default)
    max(v, 1L)
  }

  for (i in seq_len(n)) {
    # xml_children() is markedly cheaper than an xpath evaluated per row
    kids <- nd_children(doc, rows[[i]], c("td", "th"))
    j <- 1L
    for (k in seq_along(kids)) {
      node <- kids[[k]]
      while (j <= ncol(grid$taken) && grid$taken[i, j]) j <- j + 1L
      cs <- span(node, "colspan", 1L)
      # rowspan="0" runs to the end of its section, not the end of the table
      rs <- span(node, "rowspan", max(which(sec == sec[i])) - i + 1L)
      grow(j + cs - 1L)
      grid$taken[seq.int(i, min(i + rs - 1L, n)), seq.int(j, j + cs - 1L)] <- TRUE
      nc <- nc + 1L
      if (nc > length(cells)) length(cells) <- 2L * length(cells)
      cells[[nc]] <- list(
        node = node, row = i, col = j, rowspan = min(rs, n - i + 1L), colspan = cs,
        tag = doc$nodes[[node]]$tag, tr = rows[[i]],
        cell_i = k, cell_n = length(kids),
        row_i = i - min(which(sec == sec[i])) + 1L,
        row_n = sum(sec == sec[i])
      )
      j <- j + cs
    }
  }
  cells <- cells[seq_len(nc)]
  used <- vapply(cells, function(z) z$col + z$colspan - 1L, integer(1L))
  list(cells = cells, nrow = n, ncol = if (length(used)) max(used) else 0L,
       cols = c(unlist(lapply(nd_children(doc, tbl, "colgroup"),
                              function(k) nd_children(doc, k, "col"))),
                nd_children(doc, tbl, "col")))
}

# Legacy tables carry their styling in attributes rather than CSS, and plenty
# of real pages still do: bgcolor, align, valign, width, nowrap, border.
pres_decls <- function(doc, node) {
  a <- function(n) {
    v <- nd_attr(doc, node, n)
    if (is.na(v) || !nzchar(trimws(v))) NULL else trimws(v)
  }
  d <- list()
  if (!is.null(a("bgcolor"))) d[["background-color"]] <- a("bgcolor")
  if (!is.null(a("color"))) d[["color"]] <- a("color")
  if (!is.null(a("align"))) d[["text-align"]] <- a("align")
  if (!is.null(a("valign"))) d[["vertical-align"]] <- a("valign")
  if (!is.null(a("face"))) d[["font-family"]] <- a("face")
  if (!is.na(nd_attr(doc, node, "nowrap"))) d[["white-space"]] <- "nowrap"
  w <- a("width")
  if (!is.null(w)) d[["width"]] <- if (grepl("^[0-9.]+$", w)) paste0(w, "px") else w
  d
}

node_decls <- function(doc, node, rules, ancestors = NULL, pos = NULL) {
  if (!length(node)) return(list())
  if (is.null(ancestors)) ancestors <- node_ancestors(doc, node)
  d <- css_for(rules, doc$nodes[[node]]$tag, nd_classes(doc, node),
               nd_attr(doc, node, "id"), ancestors, pos)
  p <- pres_decls(doc, node)
  d[names(p)] <- p
  inline <- parse_css_decls(nd_attr(doc, node, "style"))
  d[names(inline)] <- inline
  d
}

# the classes and tag names of everything above a cell, used to approximate
# descendant combinators
# Kept out of the writer so the cache is passed as an argument rather than
# captured: a closure over it reads as an unused variable to static checkers.
cached_ancestors <- function(doc, cell, cache) {
  key <- as.character(cell$row)
  hit <- cache[[key]]
  if (is.null(hit)) {
    hit <- node_ancestors(doc, cell$node)
    cache[[key]] <- hit
  }
  hit
}

cached_anc_sig <- function(cell, cache, ancestors) {
  key <- as.character(cell$row)
  hit <- cache[[key]]
  if (is.null(hit)) {
    hit <- paste0("|", paste0(vapply(ancestors, function(lv) {
      paste(lv$tag, paste(lv$classes, collapse = "."), lv$id, lv$pos_i, lv$pos_n,
            sep = "\r")
    }, character(1L)), collapse = "|"))
    cache[[key]] <- hit
  }
  hit
}

node_ancestors <- function(doc, node) {
  out <- list()
  p <- doc$nodes[[node]]$parent
  while (!is.na(p) && p > 1L && !identical(doc$nodes[[p]]$tag, "html")) {
    sib <- nd_sibling_index(doc, p)
    out[[length(out) + 1L]] <- list(
      tag = doc$nodes[[p]]$tag,
      classes = nd_classes(doc, p),
      id = nd_attr(doc, p, "id"),
      pos_i = sib$i,
      pos_n = sib$n
    )
    p <- doc$nodes[[p]]$parent
  }
  out
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
#' produced them. `openxlsx2` is the only thing it needs; `gt` is a suggestion
#' and nothing on this path uses it.
#'
#' Old fashioned presentational markup is understood too: `bgcolor`, `align`,
#' `valign`, `width`, `nowrap` and `<table border>`.
#'
#' @section How much CSS is understood:
#' Enough for tables, not enough to call it a browser. A selector is matched
#' by walking its components against the cell and the elements above it, so
#' `table.report td.total`, `thead td` and `div > td` all mean what they say.
#' Nested rules, `:is()`, custom properties and `!important` are handled, as
#' are the positional pseudo-classes `:first-child`, `:last-child`,
#' `:only-child` and `:nth-child()`.
#'
#' An `<a href>` inside a cell becomes a hyperlink on that cell, and the whole
#' cell is what becomes clickable: a spreadsheet has no way to link part of a
#' cell's text. The first usable anchor is taken, since a cell holds one
#' target, and a link that only points at a fragment of the source page is
#' skipped. Either of those produces a warning naming how many were dropped.
#'
#' What is not: sibling combinators (`+`, `~`), state pseudo-classes and
#' `::before` cause a rule to be skipped rather than guessed at, attribute
#' selectors match on the tag alone, `@media` conditions are ignored, and
#' stylesheets pulled in with `<link>` are not fetched.
#'
#' Properties with no spreadsheet equivalent, such as gradients, letter
#' spacing and rounded corners, are dropped. `<img>` and `<svg>` leave an empty cell, and
#' `<a href>` keeps its text but not the link.
#'
#' @param wb A `wbWorkbook` object.
#' @param x HTML: a string, a file path, an already parsed document, or
#'   anything with an `as.character()` method that returns HTML. That includes
#'   what `rvest` and `xml2` hand back, so a scraped page or a single
#'   `<table>` node can be passed straight in.
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
#' @param context Pick up block elements sitting beside the table, such as a
#'   heading above it or a note below, and write them as merged rows. Set to
#'   `FALSE` to write the table on its own.
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
  wb <- wb$clone()

  looks_like_path <- length(x) == 1L && is.character(x) && nchar(x) < 1024L &&
    !grepl("[<\n]", x) && file.exists(x)
  doc <- if (looks_like_path) {
    html_parse(paste(readLines(x, warn = FALSE), collapse = "\n"))
  } else if (is.list(x) && !is.null(x$nodes)) {
    x
  } else {
    html_parse(as.character(x))
  }

  tables <- nd_find_all(doc, "table")
  if (!length(tables)) stop("no <table> found", call. = FALSE)
  if (which > length(tables)) stop("`which` is past the last table", call. = FALSE)
  tbl <- tables[[which]]

  rules <- parse_stylesheet(doc)
  base_px <- 16
  # the table's own ancestors matter: gt scopes its rules under the wrapper
  # div, so a root lookup without them finds nothing
  root <- css_for(rules, "table", nd_classes(doc, tbl), nd_attr(doc, tbl, "id"),
                  node_ancestors(doc, tbl))
  if (!is.null(root[["font-size"]])) {
    base_px <- css_px(root[["font-size"]], base = 16)
    if (is.na(base_px)) base_px <- 16
  }
  base_size <- round(base_px * 0.75, 1)
  base_font <- pick_font(root[["font-family"]])

  # crude inheritance: the properties CSS actually inherits, taken from the
  # table rule and used as the starting point for every cell
  root_vars <- css_var_defs(root)
  inherited <- root[intersect(names(root),
                              c("color", "font-family", "font-size", "font-weight",
                                "font-style", "text-align", "white-space"))]

  g <- html_grid(doc, tbl)
  if (is.null(g)) stop("the table has no rows", call. = FALSE)

  rc <- openxlsx2::dims_to_rowcol(dims, as_integer = TRUE)
  row0 <- min(rc$row)
  col0 <- min(rc$col)

  # a table-wide border="1" is a presentational shorthand for thin gridlines
  bw <- suppressWarnings(as.integer(nd_attr(doc, tbl, "border")))
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
  anc_cache <- new.env(parent = emptyenv())
  row_ancestors <- function(cell) cached_ancestors(doc, cell, anc_cache)
  # the cascade depends on what sits above a cell, so the cache key has to
  # carry the ancestor chain as well
  anc_sig_cache <- new.env(parent = emptyenv())
  row_anc_sig <- function(cell) {
    cached_anc_sig(cell, anc_sig_cache, row_ancestors(cell))
  }
  col_decl <- vector("list", max(g$ncol, 1L))
  if (length(g$cols)) {
    j <- 1L
    for (k in seq_along(g$cols)) {
      cn <- suppressWarnings(as.integer(nd_attr(doc, g$cols[[k]], "span")))
      if (is.na(cn) || cn < 1L) cn <- 1L
      d <- node_decls(doc, g$cols[[k]], rules)
      for (jj in seq.int(j, min(j + cn - 1L, length(col_decl)))) col_decl[[jj]] <- d
      j <- j + cn
    }
  }
  row_decl <- lapply(seq_len(g$nrow), function(i) list())
  seen <- integer(0L)
  for (cell in g$cells) {
    if (cell$row %in% seen) next
    seen <- c(seen, cell$row)
    row_decl[[cell$row]] <- node_decls(doc, cell$tr, rules)
  }

  # One pass over every cell's text instead of one call per cell: extracting,
  # stripping and number parsing are all vectorised.
  cell_txt <- vapply(g$cells, function(z) nd_inner(doc, z$node), character(1L))
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
  decl_sig <- function(d) {
    if (!length(d)) "" else paste0(names(d), unlist(d), collapse = "")
  }
  row_sig <- vapply(row_decl, decl_sig, character(1L))
  col_sig <- vapply(col_decl, decl_sig, character(1L))

  cc <- new_sheet_cells()
  cc$borders <- list()

  # Titles and notes are often siblings of the table (or of its wrapper) rather
  # than a <caption>, so those blocks are collected too.
  sibling_blocks <- function(where) {
    if (!isTRUE(context)) return(list())
    out <- list()
    blocks <- c("div", "p", "h1", "h2", "h3", "h4")
    node <- tbl
    for (lvl in 1:3) {
      p <- doc$nodes[[node]]$parent
      if (is.na(p) || p <= 1L || doc$nodes[[p]]$tag %in% c("body", "html")) break
      kids <- doc$nodes[[p]]$children
      at <- match(node, kids)
      side <- if (identical(where, "preceding")) {
        if (at > 1L) kids[seq_len(at - 1L)] else integer(0L)
      } else if (at < length(kids)) {
        kids[seq.int(at + 1L, length(kids))]
      } else {
        integer(0L)
      }
      for (k in side) {
        if (!doc$nodes[[k]]$tag %in% blocks) next
        if (length(nd_find(doc, k, "table"))) next
        if (!nzchar(trimws(nd_text(doc, k)))) next
        out[[length(out) + 1L]] <- k
      }
      node <- p
    }
    out
  }

  write_block <- function(node, r) {
    txt <- nd_inner(doc, node)
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
  caption <- nd_children(doc, tbl, "caption")
  if (length(caption)) {
    blocks <- nd_children(doc, caption[[1L]], c("div", "p", "span"))
    if (!length(blocks)) blocks <- caption
    for (k in seq_along(blocks)) {
      node <- blocks[[k]]
      txt <- nd_inner(doc, node)
      if (!nzchar(html_strip(txt) %||% "")) next
      cap_rows <- cap_rows + 1L
      decl <- inherited
      own <- node_decls(doc, node, rules)
      decl[names(own)] <- own
      inline <- list()
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
    cls <- nd_attr(doc, node, "class")
    sty <- nd_attr(doc, node, "style")
    nkid <- length(doc$nodes[[node]]$children)
    pos <- list(cell_i = cell$cell_i, cell_n = cell$cell_n,
                row_i = cell$row_i, row_n = cell$row_n)
    # only the answers the pseudo-classes can give belong in the key
    pkey <- paste(pos$cell_i == 1L, pos$cell_i == pos$cell_n, pos$cell_i %% 2L,
                  pos$row_i == 1L, pos$row_i == pos$row_n, pos$row_i %% 2L,
                  pos$cell_i, pos$row_i, sep = ",")
    ckey <- paste(cell$tag, cls, sty, nkid, row_anc_sig(cell),
                  row_sig[cell$row], col_sig[cell$col], pkey, sep = "\r")
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
      while (length(doc$nodes[[inner]]$children) == 1L) {
        kid <- doc$nodes[[inner]]$children[[1L]]
        if (!doc$nodes[[kid]]$tag %in% c("p", "div", "span")) break
        inner <- kid
        w <- node_decls(doc, inner, rules, anc, pos)
        # a wrapper routinely declares background-color:transparent, which must
        # not wipe the background the cell itself sets
        w <- w[!vapply(w, function(v) {
          tolower(trimws(as.character(v)[1L])) %in%
            c("transparent", "none", "inherit", "initial", "unset")
        }, logical(1L))]
        if (length(w)) wrap[names(w)] <- w
      }

      for (extra in list(inherited, table_border, col_decl[[cell$col]],
                         row_decl[[cell$row]],
                         node_decls(doc, node, rules, anc, pos), wrap)) {
        if (length(extra)) decl[names(extra)] <- extra
      }
      decl <- resolve_vars(decl, c(root_vars, css_var_defs(decl)))
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
                resolver = function(tag, classes) {
                  css_for(rules, tag, classes, NA_character_)
                })
  foot_row <- row0 + g$nrow
  for (node in sibling_blocks("following")) {
    if (write_block(node, foot_row)) foot_row <- foot_row + 1L
  }

  # a border on the <table> itself frames the block rather than any one cell
  troot <- decl_to_rec(resolve_vars(root, root_vars), base_px)
  if (length(troot$borders)) {
    r0 <- row0 - cap_rows
    r1 <- row0 + g$nrow - 1L
    c0 <- col0
    c1 <- col0 + g$ncol - 1L
    for (side in names(troot$borders)) {
      b <- troot$borders[[side]]
      rows_j <- switch(side,
                       top = list(r0, seq.int(c0, c1)),
                       bottom = list(r1, seq.int(c0, c1)),
                       left = list(seq.int(r0, r1), c0),
                       right = list(seq.int(r0, r1), c1))
      cc$borders[[length(cc$borders) + 1L]] <-
        list(rows = rows_j[[1L]], cols = rows_j[[2L]], side = side,
             border = b$border, color = b$color)
    }
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
