#' @keywords internal
"_PACKAGE"

#' @importFrom openxlsx2 current_sheet fmt_txt wb_color wb_dims
NULL

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# The only piece of gt that gtxlsx reaches into. There is no public accessor for a
# built table, and reimplementing the build would duplicate a large part of gt,
# so the access is isolated here and checked before use.
gt_build_data <- function(x, context = "html") {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("package 'gt' is required", call. = FALSE)
  }
  fun <- try(utils::getFromNamespace("build_data", "gt"), silent = TRUE)
  if (inherits(fun, "try-error") || !is.function(fun)) {
    stop("this version of 'gt' does not provide the table builder gtxlsx needs; ",
         "please report it at https://github.com/JanMarvin/gtxlsx/issues",
         call. = FALSE)
  }
  fun(data = x, context = context)
}

# Footnote and source note text may be plain, an md() object or an html()
# object. gt renders markdown with markdown::mark(), which turns on the
# superscript, subscript and strikethrough extensions, so the same engine is
# used here. Plain text is escaped exactly as gt escapes it, and html() text is
# already markup and passes through.
render_md <- function(x, context = "html") {
  if (is.null(x) || !length(x)) return(NULL)
  if (inherits(x, "from_markdown")) {
    out <- markdown::mark(text = as.character(x))
    return(gsub("^<p>|</p>$", "", trimws(out)))
  }
  if (inherits(x, "html") || inherits(x, "AsIs")) return(as.character(x))
  out <- as.character(x)
  out <- gsub("&", "&amp;", out, fixed = TRUE)
  out <- gsub("<", "&lt;", out, fixed = TRUE)
  gsub(">", "&gt;", out, fixed = TRUE)
}

# openxlsx2 renamed a few arguments between releases; drop what the installed
# version does not know instead of failing
known_args <- function(fun, args) {
  fm <- setdiff(names(formals(fun)), "...")
  args[names(args) %in% fm]
}

fmt_txt_safe <- function(x, ...) {
  do.call(openxlsx2::fmt_txt,
          c(list(x = x), known_args(openxlsx2::fmt_txt, list(...))))
}

opt_val <- function(ops, parameter) {
  i <- match(parameter, ops$parameter)
  if (is.na(i)) return(NULL)
  v <- ops$value[[i]]
  if (length(v) == 0L) NULL else v
}

opt_chr <- function(ops, parameter, default = NA_character_) {
  v <- opt_val(ops, parameter)
  if (is.null(v)) default else as.character(v)[1L]
}

opt_lgl <- function(ops, parameter, default = FALSE) {
  v <- opt_val(ops, parameter)
  if (is.null(v)) default else isTRUE(as.logical(v)[1L])
}

is_rich <- function(x) !is.na(x) & grepl("<[a-zA-Z/!]", x)

as_df <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

# flatten a possibly list-shaped column of labels to plain character
# built components already hold rendered strings; re-running gt's process_text()
# on them would escape the markup a second time
chr_col <- function(x) {
  if (is.null(x)) return(character(0L))
  if (!is.list(x)) return(as.character(x))
  vapply(x, function(e) {
    if (is.null(e) || !length(e)) NA_character_ else as.character(e)[1L]
  }, character(1L), USE.NAMES = FALSE)
}

has_text <- function(x) {
  !is.null(x) && length(x) >= 1L && !is.na(x[[1L]]) && nzchar(as.character(x)[[1L]])
}

# gt's gt_colors.scss font-color() threshold
luminance <- function(hex) {
  if (is.null(hex) || is.na(hex)) return(255)
  hex <- substring(hex, nchar(hex) - 5L)
  v <- strtoi(substring(hex, c(1L, 3L, 5L), c(2L, 4L, 6L)), 16L)
  v[1L] * 0.299 + v[2L] * 0.587 + v[3L] * 0.114
}

# CSS generic keywords are not fonts Excel can resolve; take the first real
# family from a stack, or fall back
pick_font <- function(stack, default = "Calibri") {
  f <- trimws(gsub("[\"']", "", unlist(strsplit(as.character(stack), ","))))
  generic <- c("system-ui", "-apple-system", "blinkmacsystemfont", "ui-sans-serif",
               "ui-serif", "ui-monospace", "ui-rounded", "sans-serif", "serif",
               "monospace", "cursive", "fantasy", "apple color emoji",
               "segoe ui emoji", "segoe ui symbol", "noto color emoji",
               "emoji", "math", "fangsong", "inherit", "initial")
  f <- f[nzchar(f) & !tolower(f) %in% generic]
  if (length(f)) f[1L] else default
}
