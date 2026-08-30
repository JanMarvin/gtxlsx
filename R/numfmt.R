num_pat <- paste0(
  "^(?<pre>[^0-9]*?)(?<neg>-|\u2212)?",
  "(?<int>[0-9]{1,3}(?:(?<grp>[,\u00a0 .'])[0-9]{3})+|[0-9]+)",
  "(?:(?<decm>[.,])(?<dec>[0-9]+))?",
  "(?<suf>[^0-9]*)$"
)

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

  body <- if (length(grp) == 1L) "#,##0" else "0"
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

# Vectorised sibling of text_as_number(): one regex pass for a whole column of
# cells instead of one per cell.
text_as_numbers <- function(x) {
  n <- length(x)
  out <- list(value = rep(NA_real_, n), numfmt = rep(NA_character_, n))
  ok <- which(!is.na(x) & nzchar(x) & grepl("[0-9]", x))
  if (!length(ok)) return(out)

  xs <- trimws(x[ok])
  parts <- regmatches(xs, regexec(num_pat, xs, perl = TRUE))
  hit <- lengths(parts) == 8L
  if (!any(hit)) return(out)
  ok <- ok[hit]
  m <- do.call(rbind, parts[hit])

  pre <- m[, 2L]
  neg <- m[, 3L]
  int <- m[, 4L]
  grp <- m[, 5L]
  decm <- m[, 6L]
  dec <- m[, 7L]
  suf <- m[, 8L]

  # without source data to check against, only symbol affixes are safe: a label
  # such as "458 Speciale" would otherwise become the number 458
  keep <- !grepl("[0-9A-Za-z]", paste0(pre, suf))
  if (!any(keep)) return(out)

  negp <- nzchar(neg) | grepl("[-\u2212]", pre)
  pre <- gsub("[-\u2212]", "", pre)
  num <- paste0(gsub("[,\u00a0 .\u0027]", "", int),
                ifelse(nzchar(decm), paste0(".", dec), ""))
  v <- suppressWarnings(as.numeric(num))
  v[negp] <- -v[negp]
  pct <- trimws(suf) == "%"
  v[pct] <- v[pct] / 100

  body <- ifelse(nzchar(grp), "#,##0", "0")
  decn <- nchar(dec)
  body <- ifelse(decn > 0L, paste0(body, ".", strrep("0", decn)), body)
  fmt <- paste0(quote_affix_v(trimws(pre)), body,
                quote_affix_v(ifelse(pct, "%", trimws(suf))))

  keep <- keep & !is.na(v)
  out$value[ok[keep]] <- v[keep]
  out$numfmt[ok[keep]] <- fmt[keep]
  out
}
