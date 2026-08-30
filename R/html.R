html_entities <- c(
  amp = "&", lt = "<", gt = ">", quot = "\"", apos = "'", nbsp = "\u00a0",
  ndash = "\u2013", mdash = "\u2014", hellip = "\u2026", copy = "\u00a9",
  reg = "\u00ae", trade = "\u2122", times = "\u00d7", minus = "\u2212",
  deg = "\u00b0", plusmn = "\u00b1", lsquo = "\u2018", rsquo = "\u2019",
  ldquo = "\u201c", rdquo = "\u201d", bull = "\u2022", dagger = "\u2020",
  Dagger = "\u2021", sect = "\u00a7", para = "\u00b6", euro = "\u20ac",
  pound = "\u00a3", yen = "\u00a5", cent = "\u00a2", frac12 = "\u00bd",
  micro = "\u00b5", middot = "\u00b7", ensp = "\u2002", emsp = "\u2003",
  thinsp = "\u2009"
)

html_unescape <- function(x) {
  if (is.na(x) || !grepl("&", x, fixed = TRUE)) return(x)
  m <- gregexpr("&(#[0-9]+|#[xX][0-9a-fA-F]+|[A-Za-z][A-Za-z0-9]*);", x, perl = TRUE)[[1]]
  if (identical(as.integer(m)[1L], -1L)) return(x)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  out <- character(0L)
  pos <- 1L
  for (i in seq_along(starts)) {
    if (starts[i] > pos) out <- c(out, substr(x, pos, starts[i] - 1L))
    ent <- substr(x, starts[i] + 1L, starts[i] + lens[i] - 2L)
    rep <- if (grepl("^#[xX]", ent)) {
      intToUtf8(strtoi(substring(ent, 3L), 16L))
    } else if (grepl("^#", ent)) {
      intToUtf8(as.integer(substring(ent, 2L)))
    } else if (!is.na(html_entities[ent])) {
      unname(html_entities[ent])
    } else {
      substr(x, starts[i], starts[i] + lens[i] - 1L)
    }
    out <- c(out, rep)
    pos <- starts[i] + lens[i]
  }
  if (pos <= nchar(x)) out <- c(out, substring(x, pos))
  paste0(out, collapse = "")
}

tag_attr <- function(tag, name) {
  pat <- paste0("\\b", name, "\\s*=\\s*(\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
  m <- regmatches(tag, regexec(pat, tag, perl = TRUE, ignore.case = TRUE))[[1]]
  if (!length(m)) return(NA_character_)
  v <- m[c(3L, 4L, 5L)]
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) NA_character_ else v[1L]
}

apply_css <- function(st, decls, base_px) {
  for (k in names(decls)) {
    v <- tolower(trimws(decls[[k]]))
    switch(
      k,
      "font-weight" = {
        n <- suppressWarnings(as.numeric(v))
        st$bold <- if (!is.na(n)) n >= 600 else v %in% c("bold", "bolder")
      },
      "font-style" = st$italic <- v %in% c("italic", "oblique"),
      "text-decoration" = ,
      "text-decoration-line" = {
        st$underline <- grepl("underline", v, fixed = TRUE)
        st$strike <- grepl("line-through", v, fixed = TRUE)
      },
      "color" = st$color <- css_color(v),
      "font-size" = st$size <- css_pt(v, base = base_px),
      "font-family" = st$font <- trimws(strsplit(decls[[k]], ",")[[1]])[1L],
      "vertical-align" = {
        if (v == "super") st$vert_align <- "superscript"
        if (v == "sub") st$vert_align <- "subscript"
      },
      NULL
    )
  }
  st
}

html_runs <- function(text, base_px = 16, resolver = NULL) {
  st <- list(bold = FALSE, italic = FALSE, underline = FALSE, strike = FALSE,
             vert_align = NULL, size = NULL, color = NULL, font = NULL)
  stack <- list()
  void <- c("br", "hr", "img", "wbr", "input", "col", "meta", "link")

  m <- gregexpr("<[^>]*>", text, perl = TRUE)[[1]]
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  if (identical(starts[1L], -1L)) {
    starts <- integer(0L)
    lens <- integer(0L)
  }

  acc <- new.env(parent = emptyenv())
  acc$runs <- list()
  push <- function(s) {
    if (nzchar(s)) acc$runs[[length(acc$runs) + 1L]] <- c(list(text = s), st)
  }

  pos <- 1L
  for (i in seq_along(starts)) {
    if (starts[i] > pos) push(html_unescape(substr(text, pos, starts[i] - 1L)))
    tag <- substr(text, starts[i], starts[i] + lens[i] - 1L)
    pos <- starts[i] + lens[i]

    if (grepl("^<!", tag)) next
    closing <- grepl("^</", tag)
    name <- tolower(sub("^</?\\s*([a-zA-Z0-9]+).*$", "\\1", tag))

    if (name %in% void) {
      if (name %in% c("br", "hr")) push("\n")
      next
    }

    if (closing) {
      # only <br> and <hr> force a line break: block tags are routinely used as
      # wrappers inside a single table cell
      if (name %in% c("p", "li")) push("\n")
      if (length(stack)) {
        st <- stack[[length(stack)]]
        stack[[length(stack)]] <- NULL
      }
      next
    }

    stack[[length(stack) + 1L]] <- st

    switch(
      name,
      b = , strong = st$bold <- TRUE,
      i = , em = , cite = , var = , dfn = st$italic <- TRUE,
      u = , ins = st$underline <- TRUE,
      s = , del = , strike = st$strike <- TRUE,
      sup = st$vert_align <- "superscript",
      sub = st$vert_align <- "subscript",
      code = , kbd = , samp = , tt = st$font <- "Consolas",
      NULL
    )

    if (identical(name, "font")) {
      col <- tag_attr(tag, "color")
      if (!is.na(col)) st$color <- css_color(col)
      face <- tag_attr(tag, "face")
      if (!is.na(face)) st$font <- face
    }

    if (is.function(resolver)) {
      cls <- tag_attr(tag, "class")
      if (!is.na(cls)) {
        st <- apply_css(st, resolver(name, strsplit(trimws(cls), "\\s+")[[1]]), base_px)
      }
    }
    sty <- tag_attr(tag, "style")
    if (!is.na(sty)) st <- apply_css(st, parse_css_decls(sty), base_px)
  }
  if (pos <= nchar(text)) push(html_unescape(substring(text, pos)))
  runs <- acc$runs
  # wrappers such as <p> around a cell's content leave a stray break at either
  # end; drop those rather than pad every cell with a blank line
  while (length(runs) && identical(runs[[1L]]$text, "\n")) runs <- runs[-1L]
  while (length(runs) && identical(runs[[length(runs)]]$text, "\n")) {
    runs <- runs[-length(runs)]
  }
  runs
}

runs_to_fmt <- function(runs, font = NULL, size = NULL, color = NULL,
                        bold = FALSE, italic = FALSE) {
  if (!length(runs)) return(fmt_txt_safe("", font = font, size = size))
  parts <- lapply(runs, function(r) {
    fmt_txt_safe(
      r$text,
      bold       = isTRUE(r$bold) || isTRUE(bold),
      italic     = isTRUE(r$italic) || isTRUE(italic),
      underline  = isTRUE(r$underline),
      strike     = isTRUE(r$strike),
      size       = r$size %||% size,
      color      = wbc(r$color %||% color),
      font       = r$font %||% font,
      vert_align = r$vert_align
    )
  })
  Reduce(`+`, parts)
}

html_to_fmt <- function(x, font = NULL, size = NULL, color = NULL,
                        bold = FALSE, italic = FALSE, base_px = 16,
                        resolver = NULL) {
  if (is.na(x)) return(fmt_txt_safe("", font = font, size = size))
  runs_to_fmt(html_runs(x, base_px = base_px, resolver = resolver),
              font = font, size = size, color = color, bold = bold, italic = italic)
}

# Most cells hold no markup at all; only pay for the strip where it can matter.
html_strip_fast <- function(x) {
  need <- !is.na(x) & grepl("[<&]", x)
  if (any(need)) x[need] <- html_strip(x[need])
  x
}

html_strip <- function(x) {
  out <- vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- gsub("<br[^>]*>|</p>", "\n", s, perl = TRUE)
    trimws(html_unescape(gsub("<[^>]*>", "", s, perl = TRUE)), whitespace = "[\r\n]")
  }, character(1L), USE.NAMES = FALSE)
  out
}
