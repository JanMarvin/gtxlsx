#' Look at the pieces of a built gt table
#'
#' Runs gt's own build step and hands back the result as plain data frames and
#' lists: the rendered body, the column definitions, the stub, the row groups,
#' the spanners, the styles, the footnotes and the table options.
#'
#' This is the input [wb_add_gt()] works from. It is exported mainly so you can
#' see why a table came out the way it did, or check what a `gt` feature leaves
#' behind before it reaches the worksheet.
#'
#' @param x A `gt_tbl` object.
#' @param context Render context handed to gt's builder. `"html"` is what
#'   [wb_add_gt()] uses and the only value that has been exercised here.
#'
#' @return A named list with the elements `body`, `data`, `boxhead`, `stub`,
#'   `groups_rows`, `row_groups`, `spanners`, `heading`, `stubhead`, `styles`,
#'   `footnotes`, `source_notes`, `summary` and `options`.
#'
#' @examples
#' library(gt)
#'
#' tbl <- gt(data.frame(a = 1:2, b = c(1.5, 2.5)))
#' tbl <- fmt_number(tbl, columns = "b", decimals = 1)
#'
#' g <- gtxlsx_extract(tbl)
#' g$body
#' g$boxhead$var
#'
#' @export
gtxlsx_extract <- function(x, context = "html") {
  if (!inherits(x, "gt_tbl")) {
    stop("`x` must be a 'gt_tbl' object", call. = FALSE)
  }

  built <- if (isTRUE(x$`_has_built`)) x else gt_build_data(x, context)

  ops <- as_df(built$`_options`)
  boxh <- as_df(built$`_boxhead`)
  stub <- as_df(built$`_stub_df`)
  grps <- as_df(built$`_groups_rows`)
  spanners <- as_df(built$`_spanners`)
  footnotes <- as_df(built$`_footnotes`)
  styles <- as_df(built$`_styles`)

  if (is.null(grps)) {
    grps <- data.frame(group_id = character(0L), row_start = integer(0L),
                       row_end = integer(0L), group_label = character(0L),
                       stringsAsFactors = FALSE)
  }
  if (nrow(grps)) {
    lbl <- grps$built_group_label %||% grps$group_label %||% grps$group_id
    grps$group_label <- chr_col(lbl)
    na <- is.na(grps$group_label)
    grps$group_label[na] <- as.character(grps$group_id)[na]
  }
  if (!is.null(spanners) && nrow(spanners)) {
    spanners$built <- chr_col(spanners$built %||% spanners$spanner_label)
  }

  if (!is.null(boxh$column_label)) {
    boxh$column_label <- chr_col(boxh$column_label)
  }

  heading <- built$`_heading` %||% list()
  for (k in c("title", "subtitle")) {
    v <- heading[[k]]
    if (!is.null(v) && (inherits(v, "from_markdown") || inherits(v, "html"))) {
      v <- as.character(render_md(v, context))[1L]
    }
    # process_text() turns an absent heading into character(0)/NA, which would
    # otherwise reserve an empty row in the layout
    heading[k] <- list(if (has_text(v)) as.character(v)[1L] else NULL)
  }

  dat <- as_df(built$`_data`)
  if (!is.null(dat) && !is.null(stub) && nrow(stub) == nrow(dat) &&
        "rownum_i" %in% names(stub)) {
    dat <- dat[stub$rownum_i, , drop = FALSE]
    rownames(dat) <- NULL
  }

  src <- built$`_source_notes`
  if (length(src)) {
    src <- chr_col(src)
  } else {
    src <- character(0L)
  }

  if (!is.null(footnotes) && nrow(footnotes)) {
    footnotes$text <- vapply(
      footnotes$footnotes,
      function(s) as.character(render_md(s, context))[1L],
      character(1L), USE.NAMES = FALSE
    )
  }

  list(
    body         = as_df(built$`_body`),
    data         = dat,
    boxhead      = boxh,
    stub         = stub,
    groups_rows  = grps,
    row_groups   = built$`_row_groups`,
    spanners     = spanners,
    heading      = heading,
    stubhead     = built$`_stubhead`,
    styles       = styles,
    footnotes    = footnotes,
    source_notes = src,
    summary      = built$`_summary_build`$summary_df_display_list,
    options      = ops
  )
}

gtxlsx_theme <- function(ops) {
  font <- pick_font(opt_val(ops, "table_font_names"))

  base_px <- css_px(opt_chr(ops, "table_font_size", "16px"), base = 16)
  if (is.na(base_px)) base_px <- 16

  scale_pt <- function(param, default = "100%") {
    css_pt(opt_chr(ops, param, default), base = base_px) %||% (base_px * 0.75)
  }

  table_bg <- css_color(opt_chr(ops, "table_background_color")) %||% "FFFFFFFF"
  fnt_dark <- css_color(opt_chr(ops, "table_font_color")) %||% "FF333333"
  fnt_light <- css_color(opt_chr(ops, "table_font_color_light")) %||% "FFFFFFFF"
  # gt picks the foreground per region from that region's background using the
  # luminance rule in its gt_colors.scss font-color() function
  fg <- function(param) {
    bg <- css_color(opt_chr(ops, param)) %||% table_bg
    if (luminance(bg) > 186) fnt_dark else fnt_light
  }

  list(
    font            = font,
    base_px         = base_px,
    size            = round(base_px * 0.75, 1),
    color           = fg("table_background_color"),
    title_color     = fg("heading_background_color"),
    label_color     = fg("column_labels_background_color"),
    group_color     = fg("row_group_background_color"),
    stub_color      = fg("stub_background_color"),
    summary_color   = fg("summary_row_background_color"),
    gsummary_color  = fg("grand_summary_row_background_color"),
    footnote_color  = fg("footnotes_background_color"),
    source_color    = fg("source_notes_background_color"),
    title_size      = scale_pt("heading_title_font_size", "125%"),
    subtitle_size   = scale_pt("heading_subtitle_font_size", "85%"),
    label_size      = scale_pt("column_labels_font_size", "100%"),
    group_size      = scale_pt("row_group_font_size", "100%"),
    stub_size       = scale_pt("stub_font_size", "100%"),
    footnote_size   = scale_pt("footnotes_font_size", "90%"),
    source_size     = scale_pt("source_notes_font_size", "90%"),
    summary_size    = round(base_px * 0.75, 1),
    title_weight    = opt_chr(ops, "heading_title_font_weight", "initial"),
    subtitle_weight = opt_chr(ops, "heading_subtitle_font_weight", "initial"),
    label_weight    = opt_chr(ops, "column_labels_font_weight", "normal"),
    group_weight    = opt_chr(ops, "row_group_font_weight", "initial"),
    stub_weight     = opt_chr(ops, "stub_font_weight", "initial"),
    heading_align   = css_align(opt_chr(ops, "heading_align", "center"), "center"),
    label_fill      = css_color(opt_chr(ops, "column_labels_background_color")),
    heading_fill    = css_color(opt_chr(ops, "heading_background_color")),
    group_fill      = css_color(opt_chr(ops, "row_group_background_color")),
    stub_fill       = css_color(opt_chr(ops, "stub_background_color")),
    summary_fill    = css_color(opt_chr(ops, "summary_row_background_color")),
    gsummary_fill   = css_color(opt_chr(ops, "grand_summary_row_background_color")),
    footnote_fill   = css_color(opt_chr(ops, "footnotes_background_color")),
    source_fill     = css_color(opt_chr(ops, "source_notes_background_color")),
    stripe_fill     = css_color(opt_chr(ops, "row_striping_background_color")),
    stripe_body     = opt_lgl(ops, "row_striping_include_table_body"),
    stripe_stub     = opt_lgl(ops, "row_striping_include_stub"),
    labels_hidden   = opt_lgl(ops, "column_labels_hidden"),
    group_as_column = opt_lgl(ops, "row_group_as_column")
  )
}

is_bold <- function(weight, default = FALSE) {
  if (is.null(weight) || is.na(weight)) return(default)
  n <- suppressWarnings(as.numeric(weight))
  if (!is.na(n)) return(n >= 600)
  w <- tolower(weight)
  if (w %in% c("initial", "inherit")) return(default)
  w %in% c("bold", "bolder")
}
