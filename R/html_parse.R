# A small HTML reader.
#
# openxlsx2 already ships an XML parser, but it is an XML parser: it rejects
# `<br>`, unquoted attribute values and unclosed `<td>`, all of which are legal
# HTML and all of which real pages contain. Rather than add a second XML
# library to the package, the markup is scanned here directly. Only what a
# table needs is built: a tree of elements with their attributes, their
# children and the raw text between their tags.

void_tags <- c("br", "hr", "img", "wbr", "input", "col", "meta", "link",
               "source", "track", "area", "base", "embed", "param")

# tags that close an open sibling of the same kind
closes_sibling <- list(
  td = c("td", "th"), th = c("td", "th"), tr = "tr", li = "li", p = "p",
  thead = c("thead", "tbody", "tfoot"), tbody = c("thead", "tbody", "tfoot"),
  tfoot = c("thead", "tbody", "tfoot"), option = "option",
  dt = c("dt", "dd"), dd = c("dt", "dd")
)

parse_attrs <- function(x) {
  if (!nzchar(x)) return(list())
  pat <- "([A-Za-z_:][-A-Za-z0-9_:.]*)\\s*(=\\s*(\"[^\"]*\"|'[^']*'|[^\\s\"'>]+))?"
  m <- gregexpr(pat, x, perl = TRUE)
  hits <- regmatches(x, m)[[1]]
  if (!length(hits)) return(list())
  out <- list()
  for (h in hits) {
    nm <- trimws(tolower(sub("\\s*=.*$", "", h)))
    if (!nzchar(nm)) next
    val <- if (grepl("=", h, fixed = TRUE)) {
      v <- trimws(sub("^[^=]*=\\s*", "", h))
      if (grepl('^".*"$', v) || grepl("^'.*'$", v)) substr(v, 2L, nchar(v) - 1L) else v
    } else {
      ""
    }
    out[[nm]] <- val
  }
  out
}

new_node <- function(tag, attrs, parent) {
  list(tag = tag, attrs = attrs, children = list(), text = character(0L),
       parent = parent, inner_from = NA_integer_, inner_to = NA_integer_)
}

# Elements are collected into a flat store; a node refers to its parent and its
# children by index, which keeps the tree cheap to build and to walk upward.
html_parse <- function(x) {
  x <- paste(as.character(x), collapse = "\n")
  x <- gsub("<!--.*?-->", "", x, perl = TRUE)

  nodes <- list(new_node("#root", list(), NA_integer_))
  open <- 1L
  # a quoted attribute value may contain ">", so the scan has to step over
  # quoted runs rather than stop at the first ">"
  m <- gregexpr("<(?:[^>\"']|\"[^\"]*\"|'[^']*')*>", x, perl = TRUE)[[1]]
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  if (identical(starts[1L], -1L)) return(list(nodes = nodes, src = x))

  add_text <- function(i, from, to) {
    if (to >= from) {
      s <- substr(x, from, to)
      if (nzchar(trimws(s))) nodes[[i]]$text <<- c(nodes[[i]]$text, s)
    }
  }

  pos <- 1L
  for (k in seq_along(starts)) {
    add_text(open, pos, starts[k] - 1L)
    tag <- substr(x, starts[k], starts[k] + lens[k] - 1L)
    pos <- starts[k] + lens[k]
    if (grepl("^<[!?]", tag)) next

    closing <- grepl("^</", tag)
    name <- tolower(sub("^</?\\s*([A-Za-z0-9:-]+).*$", "\\1", tag))
    if (!nzchar(name)) next

    if (closing) {
      # close the nearest matching ancestor, ignoring stray end tags
      j <- open
      while (!is.na(j) && j > 1L && !identical(nodes[[j]]$tag, name)) {
        j <- nodes[[j]]$parent
      }
      if (!is.na(j) && j > 1L) {
        # everything still open inside it ends here too
        z <- open
        while (!is.na(z) && z >= j && z > 1L) {
          nodes[[z]]$inner_to <- starts[k] - 1L
          if (identical(z, j)) break
          z <- nodes[[z]]$parent
        }
        open <- nodes[[j]]$parent
      }
      next
    }

    # an unclosed sibling ends where the next one begins
    sib <- closes_sibling[[name]]
    if (!is.null(sib) && open > 1L && nodes[[open]]$tag %in% sib) {
      nodes[[open]]$inner_to <- starts[k] - 1L
      open <- nodes[[open]]$parent
    }

    nodes[[length(nodes) + 1L]] <- new_node(name, parse_attrs(
      sub("^<[A-Za-z0-9:-]+", "", sub("/?>$", "", tag))
    ), open)
    idx <- length(nodes)
    nodes[[idx]]$inner_from <- pos
    nodes[[open]]$children <- c(nodes[[open]]$children, idx)

    if (name %in% void_tags || grepl("/>$", tag)) {
      nodes[[idx]]$inner_to <- pos - 1L
    } else {
      open <- idx
    }
  }
  add_text(open, pos, nchar(x))

  for (i in seq_along(nodes)) {
    if (is.na(nodes[[i]]$inner_to)) nodes[[i]]$inner_to <- nchar(x)
  }
  list(nodes = nodes, src = x)
}

# ---- accessors --------------------------------------------------------------

nd_attr <- function(doc, i, name) {
  v <- doc$nodes[[i]]$attrs[[tolower(name)]]
  if (is.null(v)) NA_character_ else v
}

nd_classes <- function(doc, i) {
  cls <- nd_attr(doc, i, "class")
  if (is.na(cls)) return(character(0L))
  out <- strsplit(trimws(cls), "\\s+")[[1]]
  out[nzchar(out)]
}

nd_children <- function(doc, i, tags = NULL) {
  kids <- doc$nodes[[i]]$children
  if (is.null(tags)) return(kids)
  kids[vapply(kids, function(k) doc$nodes[[k]]$tag %in% tags, logical(1L))]
}

nd_inner <- function(doc, i) {
  n <- doc$nodes[[i]]
  if (is.na(n$inner_from) || n$inner_to < n$inner_from) return("")
  trimws(substr(doc$src, n$inner_from, n$inner_to))
}

nd_text <- function(doc, i) {
  html_unescape(gsub("<[^>]*>", "", nd_inner(doc, i), perl = TRUE))
}

# every element under `i`, depth first, optionally filtered by tag
nd_find <- function(doc, i, tags = NULL) {
  out <- integer(0L)
  stack <- doc$nodes[[i]]$children
  while (length(stack)) {
    k <- stack[[1L]]
    stack <- stack[-1L]
    if (is.null(tags) || doc$nodes[[k]]$tag %in% tags) out <- c(out, k)
    stack <- c(doc$nodes[[k]]$children, stack)
  }
  out
}

nd_find_all <- function(doc, tags) nd_find(doc, 1L, tags)

nd_sibling_index <- function(doc, i) {
  p <- doc$nodes[[i]]$parent
  if (is.na(p)) return(list(i = NA_integer_, n = NA_integer_))
  kids <- doc$nodes[[p]]$children
  list(i = match(i, kids), n = length(kids))
}
