#' @keywords internal
"_PACKAGE"

#' @importFrom openxlsx2 current_sheet fmt_txt wb_color wb_dims
NULL

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# The only piece of gt that gtxlsx reaches into. There is no public accessor for a
# built table, and reimplementing the build would duplicate a large part of gt,
# so the access is isolated here and checked before use.
gt_build_data <- function(x, context = "html") {
  need_gt()
  fun <- try(utils::getFromNamespace("build_data", "gt"), silent = TRUE)
  if (inherits(fun, "try-error") || !is.function(fun)) {
    stop("this version of 'gt' does not provide the table builder gtxlsx needs; ",
         "please report it at https://github.com/JanMarvin/gtxlsx/issues",
         call. = FALSE)
  }
  built <- fun(data = x, context = context)
  check_built(built)
}

# The components read out of a built gt table. Checked at run time, not only
# in the tests, so that a gt release which moves one of them produces a clear
# error here rather than a wrong sheet somewhere downstream.
# _source_notes is left out on purpose: gt adds it only once a table has one.
built_parts <- c("_body", "_boxhead", "_data", "_footnotes", "_groups_rows",
                 "_heading", "_options", "_row_groups", "_spanners",
                 "_stub_df", "_stubhead", "_styles", "_summary_build")

# The columns read out of those components. A rename here would otherwise
# surface as a sheet full of NA rather than as an error.
built_cols <- list(
  `_boxhead` = c("var", "type", "column_label", "column_align"),
  `_stub_df` = c("rownum_i", "group_id"),
  `_spanners` = c("vars", "spanner_level", "spanner_id"),
  `_row_groups` = character(0L),
  `_styles` = c("locname", "colname", "rownum", "styles")
)

check_built <- function(built) {
  missing <- setdiff(built_parts, names(built))
  if (length(missing)) {
    stop("gtxlsx cannot read this version of 'gt' (",
         as.character(utils::packageVersion("gt")), "): ",
         "the built table has no ", paste(missing, collapse = ", "), ". ",
         "This is a gtxlsx problem, not a problem with your table or with any ",
         "package that called it. Please report it at ",
         "https://github.com/JanMarvin/gtxlsx/issues",
         call. = FALSE)
  }

  for (part in names(built_cols)) {
    want <- built_cols[[part]]
    if (!length(want)) next
    # An empty component is never read from, so its columns do not matter. A
    # gt old enough to lack one of them still works on tables that do not use
    # the feature, and is refused on the tables that do.
    if (!NROW(built[[part]])) next
    have <- names(built[[part]])
    if (is.null(have)) next
    gone <- setdiff(want, have)
    if (length(gone)) {
      stop("gtxlsx cannot read this version of 'gt' (",
           as.character(utils::packageVersion("gt")), "): ",
           part, " has no column ", paste(gone, collapse = ", "), ". ",
           "This is a gtxlsx problem, not a problem with your table or with ",
           "any package that called it. Please report it at ",
           "https://github.com/JanMarvin/gtxlsx/issues", call. = FALSE)
    }
  }
  built
}

# Footnote and source note text may be plain, an md() object or an html()
# object. gt renders markdown with markdown::mark(), which turns on the
# superscript, subscript and strikethrough extensions, so the same engine is
# used here. Plain text is escaped exactly as gt escapes it, and html() text is
# already markup and passes through.
render_md <- function(x, context = "html") {
  if (is.null(x) || !length(x)) return(NULL)
  if (inherits(x, "from_markdown")) {
    if (!requireNamespace("markdown", quietly = TRUE)) {
      stop("package 'markdown' is required to render md() text", call. = FALSE)
    }
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

# Every tab_options() name this package reads is looked up here, so this is
# also where a name gt has renamed shows up. Rather than keep a second list of
# the names in sync by hand, the misses are collected as they happen and
# reported once. All of them exist in gt 1.1.0 and in 1.3.0.9000, so a miss
# means gt moved something rather than that the option is merely newer.
opt_missed <- new.env(parent = emptyenv())

opt_val <- function(ops, parameter) {
  i <- match(parameter, ops$parameter)
  if (is.na(i)) {
    opt_missed[[parameter]] <- TRUE
    return(NULL)
  }
  v <- ops$value[[i]]
  if (length(v) == 0L) NULL else v
}

# Called once per table, after the options have been read.
report_missed_options <- function() {
  gone <- ls(opt_missed)
  rm(list = gone, envir = opt_missed)
  if (!length(gone)) return(invisible(NULL))
  warning("this version of 'gt' has no option ", paste(gone, collapse = ", "),
          ", so the corresponding formatting was left out. Please report it ",
          "at https://github.com/JanMarvin/gtxlsx/issues", call. = FALSE)
  invisible(NULL)
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

# gt is a suggestion, not a dependency: wb_add_html() needs nothing from it.
need_gt <- function() {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("package 'gt' is required for this function; ",
         "wb_add_html() works without it", call. = FALSE)
  }
  invisible(TRUE)
}
