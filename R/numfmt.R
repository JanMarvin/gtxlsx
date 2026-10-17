num_pattern <- function(big, dec) {
  cls <- function(x) {
    x <- ifelse(grepl("[[:punct:]]", x), paste0("\\", x), x)
    paste0("[", paste(x, collapse = ""), "]")
  }
  int <- if (length(big)) {
    paste0("(?<int>[0-9]{1,3}(?:(?<grp>", cls(big), ")[0-9]{3})+|[0-9]*)")
  } else {
    "(?<int>[0-9]*)(?<grp>)"
  }
  paste0("^(?<pre>[^0-9]*?)(?<neg>-|\u2212)?", int,
         "(?:(?<decm>", cls(dec), ")(?<dec>[0-9]+))?(?<suf>[^0-9]*)$")
}

default_big <- c(",", "\u00a0", "\u202f", "\u2009", " ", ".", "'")
default_dec <- c(".", ",")
num_pat <- num_pattern(default_big, default_dec)

# `dec_mark` and `big_mark` are the characters a table uses for the decimal
# point and for grouping thousands. When both are NULL the text is read by its
# own shape and anything that could be read two ways is left as text.
num_marks <- function(dec_mark = NULL, big_mark = NULL) {
  check <- function(x, what, empty_ok) {
    if (is.null(x)) return(NULL)
    ok <- is.character(x) && length(x) == 1L && !is.na(x) &&
      (nchar(x) == 1L || (empty_ok && !nzchar(x))) && !grepl("[0-9]", x)
    if (!ok) {
      stop("`", what, "` must be a single character that is not a digit",
           if (empty_ok) " (or \"\" for none)", call. = FALSE)
    }
    x
  }
  dec_mark <- check(dec_mark, "dec_mark", FALSE)
  big_mark <- check(big_mark, "big_mark", TRUE)
  if (!is.null(dec_mark) && identical(dec_mark, big_mark)) {
    stop("`dec_mark` and `big_mark` must differ", call. = FALSE)
  }
  if (is.null(dec_mark) && is.null(big_mark)) {
    return(list(auto = TRUE, pat = num_pat))
  }
  if (is.null(big_mark)) big_mark <- setdiff(default_big, dec_mark)
  if (identical(big_mark, "")) big_mark <- character()
  if (is.null(dec_mark)) dec_mark <- setdiff(default_dec, big_mark)
  list(auto = FALSE, pat = num_pattern(big_mark, dec_mark))
}

parse_num_strings <- function(x) {
  x <- trimws(x)
  parts <- regmatches(x, regexec(num_pat, x, perl = TRUE))
  if (any(lengths(parts) == 0L)) return(NULL)
  get <- function(i) vapply(parts, function(p) p[[i]], character(1L))
  list(pre = get(2L), neg = get(3L), int = get(4L), grp = get(5L),
       decm = get(6L), dec = get(7L), suf = get(8L))
}

quote_affix <- function(x) {
  if (!nzchar(x)) return("")
  if (identical(x, "%")) return("%")
  paste0("\"", gsub("\"", "", x, fixed = TRUE), "\"")
}

#' @return `NULL` when the column cannot be represented safely, otherwise a
#'   list with `values` and `numfmt`.
#' @noRd
infer_numfmt <- function(strings, values) {
  if (!is.numeric(values)) return(NULL)
  # a cell carrying markup stays text; check before decoding, since gt renders
  # currency symbols as HTML entities and those have to be resolved to match
  if (any(is_rich(strings))) return(NULL)
  strings <- html_strip(strings)
  keep <- !is.na(strings) & nzchar(strings) & !is.na(values)
  if (!any(keep)) return(NULL)

  p <- parse_num_strings(strings[keep])
  if (is.null(p)) return(NULL)

  # gt writes a negative currency as "\u2212$700": the sign sits inside the
  # prefix, not in the sign group
  neg_pre <- grepl("[-\u2212]", p$pre)
  p$pre <- gsub("[-\u2212]", "", p$pre)
  pre <- unique(p$pre)
  suf <- unique(p$suf)
  if (length(pre) != 1L || length(suf) != 1L) return(NULL)

  grp <- unique(p$grp[nzchar(p$grp)])
  if (length(grp) > 1L) return(NULL)
  decm <- unique(p$decm[nzchar(p$decm)])
  if (length(decm) > 1L) return(NULL)
  if (length(grp) == 1L && length(decm) == 1L && identical(grp, decm)) return(NULL)

  dec_n <- unique(nchar(p$dec))
  if (length(dec_n) != 1L) return(NULL)

  num <- p$int
  if (length(grp) == 1L) num <- gsub(grp, "", num, fixed = TRUE)
  if (length(decm) == 1L) num <- paste0(num, ".", p$dec)
  num <- suppressWarnings(as.numeric(num))
  num[nzchar(p$neg) | neg_pre] <- -num[nzchar(p$neg) | neg_pre]
  if (anyNA(num)) return(NULL)

  pct <- identical(trimws(suf), "%")
  ref <- values[keep]
  if (pct) ref <- ref * 100
  # the rendered text is a rounding of the stored value, so accept anything
  # within half a unit of the last shown decimal; a scaled or suffixed column
  # (1.2K for 1200) is far outside that and falls back to text
  tol <- 0.5 * 10^(-dec_n) * (1 + 1e-9) + 1e-9
  if (any(abs(ref - num) > tol)) return(NULL)

  # gt's drop_leading_zero prints ".75"; "#" is the number format code
  # that suppresses a leading zero the same way
  lead <- if (all(!nzchar(p$int))) "#" else "0"
  body <- if (length(grp) == 1L) paste0("#,##", lead) else lead
  if (dec_n > 0L) body <- paste0(body, ".", strrep("0", dec_n))
  fmt <- paste0(quote_affix(trimws(pre)), body,
                quote_affix(if (pct) "%" else trimws(suf)))

  out <- rep(NA_real_, length(strings))
  out[keep] <- values[keep]
  list(values = out, numfmt = fmt)
}

quote_affix_v <- function(x) {
  ifelse(!nzchar(x), "",
         ifelse(x == "%", "%", paste0("\"", gsub("\"", "", x, fixed = TRUE), "\"")))
}

# What each cell says about the marks, read by the broad pattern: a group mark
# is certain when the number holds two of them, when a decimal mark follows it,
# or when it is a space (also no-break, narrow and thin) or an apostrophe; a
# decimal mark is certain when it follows digits that are not a group. A
# single "." or "," in front of exactly three digits proves nothing.
read_num_cells <- function(xs, pat) {
  parts <- regmatches(xs, regexec(pat, xs, perl = TRUE))
  hit <- lengths(parts) == 8L
  m <- matrix("", length(xs), 8L)
  if (any(hit)) m[hit, ] <- do.call(rbind, parts[hit])
  cols <- c("all", "pre", "neg", "int", "grp", "decm", "dec", "suf")
  colnames(m) <- cols
  out <- as.data.frame(m, stringsAsFactors = FALSE)[, -1L]
  out$hit <- hit
  out$n_marks <- nchar(gsub("[0-9]", "", out$int))
  out$mixed <- vapply(strsplit(gsub("[0-9]", "", out$int), ""),
                      function(z) length(unique(z)) > 1L, logical(1L))
  out$lead_zero <- out$n_marks > 0L & startsWith(out$int, "0")
  out$symbols <- hit & !grepl("[0-9A-Za-z]", paste0(out$pre, out$suf))
  out
}

mark_evidence <- function(xs) {
  r <- read_num_cells(xs, num_pat)
  r <- r[r$symbols & !r$mixed, ]
  lone_zero <- r$lead_zero & r$n_marks == 1L & !nzchar(r$decm)
  sure_big <- nzchar(r$grp) & !lone_zero &
    (r$n_marks >= 2L | nzchar(r$decm) |
       r$grp %in% c(" ", "\u00a0", "\u202f", "\u2009", "'"))
  sure_dec <- c(r$decm[nzchar(r$decm)], r$grp[lone_zero])
  big <- unique(r$grp[sure_big])
  dec <- unique(sure_dec)
  list(big = big, dec = dec,
       conflict = length(big) > 1L || length(dec) > 1L || any(big %in% dec))
}

# The digits as the text wrote them, rebuilt from the value: grouped by the mark
# found, with the decimals found. Anything that does not come back is not a
# number of that shape, so "007" or a twenty digit id stay text.
rebuild_core <- function(value, decn, grp, decm, int) {
  s <- mapply(function(v, d) sprintf("%.*f", d, abs(v)), value, decn)
  s <- as.character(s)
  ip <- sub("\\..*$", "", s)
  fp <- sub("^[^.]*\\.?", "", s)
  grouped <- ifelse(nzchar(grp),
                    mapply(function(i, g) {
                      gsub("(?<=[0-9])(?=([0-9]{3})+$)", g, i, perl = TRUE)
                    }, ip, grp),
                    ip)
  core <- paste0(grouped, ifelse(decn > 0L, paste0(decm, fp), ""))
  ifelse(nzchar(int), core, sub("^0", "", core))
}

# Vectorised sibling of text_as_number(): one regex pass for a whole column of
# cells instead of one per cell.
text_as_numbers <- function(x, dec_mark = NULL, big_mark = NULL) {
  marks <- num_marks(dec_mark, big_mark)
  n <- length(x)
  out <- list(value = rep(NA_real_, n), numfmt = rep(NA_character_, n))
  ok <- which(!is.na(x) & nzchar(x) & grepl("[0-9]", x))
  if (!length(ok)) return(out)
  xs <- trimws(x[ok])

  conflict <- FALSE
  if (marks$auto) {
    ev <- mark_evidence(xs)
    conflict <- ev$conflict
    if (conflict) {
      warning("the table uses more than one decimal or group mark, so numbers ",
              "with either are left as text; set `dec_mark` and `big_mark`",
              call. = FALSE)
    } else if (length(ev$big) || length(ev$dec)) {
      marks <- num_marks(if (length(ev$dec)) ev$dec, if (length(ev$big)) ev$big)
    }
  }

  r <- read_num_cells(xs, marks$pat)
  if (!any(r$hit)) return(out)

  # without source data to check against, only symbol affixes are safe: a label
  # such as "458 Speciale" would otherwise become the number 458
  keep <- r$symbols

  # A group mark can only sit between digit groups, never behind a leading
  # zero, and a number has one kind of it. "0.001" is therefore a decimal.
  as_decimal <- r$lead_zero & r$n_marks == 1L & !nzchar(r$decm) & !r$mixed &
    r$grp %in% default_dec
  if (marks$auto) {
    r$decm[which(as_decimal)] <- r$grp[which(as_decimal)]
    r$dec[which(as_decimal)] <- sub("^0.", "", r$int[which(as_decimal)])
    r$int[which(as_decimal)] <- "0"
    r$grp[which(as_decimal)] <- ""
  } else {
    as_decimal[] <- FALSE
  }
  keep <- keep & !r$mixed & !(r$lead_zero & !as_decimal) &
    !(nzchar(r$grp) & r$grp == r$decm)

  # "1.234" reads as one thousand two hundred thirty-four in one locale and as
  # a decimal in another; unless the table settled it, only a "," is taken as
  # the group mark there, since that is by far the commoner table
  if (marks$auto) {
    keep <- keep & !(r$n_marks == 1L & !nzchar(r$decm) & r$grp == "." & !as_decimal)
  }
  if (conflict) keep <- keep & !nzchar(r$grp) & !nzchar(r$decm)
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) return(out)

  negp <- nzchar(r$neg) | grepl("[-\u2212]", r$pre)
  pre <- gsub("[-\u2212]", "", r$pre)
  core <- paste0(r$int, ifelse(nzchar(r$decm), paste0(r$decm, r$dec), ""))
  raw <- suppressWarnings(as.numeric(paste0(gsub("[^0-9]", "", r$int),
                                            ifelse(nzchar(r$decm),
                                                   paste0(".", r$dec), ""))))
  decn <- nchar(r$dec)
  decn[is.na(decn)] <- 0L
  keep <- keep & !is.na(raw) &
    rebuild_core(raw, decn, r$grp, r$decm, r$int) == core
  keep[is.na(keep)] <- FALSE

  v <- raw
  v[negp] <- -v[negp]
  pct <- trimws(r$suf) == "%"
  v[pct] <- v[pct] / 100

  lead <- ifelse(nzchar(r$int), "0", "#")
  body <- ifelse(nzchar(r$grp), paste0("#,##", lead), lead)
  body <- ifelse(decn > 0L, paste0(body, ".", strrep("0", decn)), body)
  fmt <- paste0(quote_affix_v(trimws(pre)), body,
                quote_affix_v(ifelse(pct, "%", trimws(r$suf))))

  out$value[ok[keep]] <- v[keep]
  out$numfmt[ok[keep]] <- fmt[keep]
  out
}
