gtxlsx_plan <- function(g, th, row0, col0) {
  boxh <- g$boxhead
  group_var <- boxh$var[boxh$type == "row_group"]
  stub_vars <- boxh$var[boxh$type == "stub"]
  body_vars <- boxh$var[boxh$type == "default"]

  group_as_col <- th$group_as_column && length(group_var) == 1L
  col_vars <- c(if (group_as_col) group_var, stub_vars, body_vars)
  w <- length(col_vars)

  col_of <- function(v) col0 - 1L + match(v, col_vars)

  spanners <- g$spanners
  if (!is.null(spanners) && nrow(spanners)) {
    spanners$vars <- lapply(spanners$vars, intersect, col_vars)
    spanners <- spanners[lengths(spanners$vars) > 0L, , drop = FALSE]
  }
  if (!is.null(spanners) && nrow(spanners)) {
    spanners$spanner_level <-
      match(spanners$spanner_level, sort(unique(spanners$spanner_level)))
    n_levels <- max(spanners$spanner_level)
  } else {
    n_levels <- 0L
  }

  r <- row0
  title_row <- subtitle_row <- NA_integer_
  if (!is.null(g$heading$title)) {
    title_row <- r
    r <- r + 1L
  }
  if (!is.null(g$heading$subtitle)) {
    subtitle_row <- r
    r <- r + 1L
  }

  level_row <- integer(0L)
  if (n_levels > 0L) {
    level_row <- r + (n_levels - seq_len(n_levels))
    names(level_row) <- seq_len(n_levels)
    r <- r + n_levels
  }

  label_row <- NA_integer_
  if (!th$labels_hidden) {
    label_row <- r
    r <- r + 1L
  }

  nb <- if (is.null(g$body)) 0L else nrow(g$body)
  body_row <- rep(NA_integer_, nb)
  grp <- g$groups_rows
  group_head <- data.frame(group_id = character(0L), label = character(0L),
                           row = integer(0L), stringsAsFactors = FALSE)
  group_span <- data.frame(group_id = character(0L), label = character(0L),
                           row_start = integer(0L), row_end = integer(0L),
                           stringsAsFactors = FALSE)
  acc <- new.env(parent = emptyenv())
  acc$summaries <- list()

  add_summary <- function(key, kind, r_at) {
    sm <- g$summary[[key]]
    if (is.null(sm) || !nrow(sm)) return(r_at)
    rows <- r_at + seq_len(nrow(sm)) - 1L
    acc$summaries[[length(acc$summaries) + 1L]] <-
      list(key = key, kind = kind, df = sm, rows = rows)
    r_at + nrow(sm)
  }

  has_groups <- !is.null(grp) && nrow(grp) > 0L

  if (has_groups) {
    for (i in seq_len(nrow(grp))) {
      idx <- seq.int(grp$row_start[i], grp$row_end[i])
      if (!group_as_col) {
        group_head <- rbind(group_head, data.frame(
          group_id = grp$group_id[i], label = grp$group_label[i],
          row = r, stringsAsFactors = FALSE
        ))
        r <- r + 1L
      }
      body_row[idx] <- r + seq_along(idx) - 1L
      if (group_as_col) {
        group_span <- rbind(group_span, data.frame(
          group_id = grp$group_id[i], label = grp$group_label[i],
          row_start = r, row_end = r + length(idx) - 1L, stringsAsFactors = FALSE
        ))
      }
      r <- r + length(idx)
      r <- add_summary(grp$group_id[i], "summary", r)
    }
  } else if (nb > 0L) {
    body_row <- r + seq_len(nb) - 1L
    r <- r + nb
  }

  r <- add_summary("::GRAND_SUMMARY", "grand_summary", r)

  fnotes <- g$footnotes
  fn <- if (is.null(fnotes) || !nrow(fnotes)) {
    data.frame(mark = character(0L), text = character(0L), stringsAsFactors = FALSE)
  } else {
    d <- unique(data.frame(mark = as.character(fnotes$fs_id %||% NA_character_),
                           text = fnotes$text %||% NA_character_,
                           stringsAsFactors = FALSE))
    d[!is.na(d$mark) & nzchar(d$mark), , drop = FALSE]
  }
  footnote_rows <- if (nrow(fn)) {
    out <- r + seq_len(nrow(fn)) - 1L
    r <- r + nrow(fn)
    out
  } else {
    integer(0L)
  }

  source_rows <- if (length(g$source_notes)) {
    out <- r + seq_along(g$source_notes) - 1L
    r <- r + length(g$source_notes)
    out
  } else {
    integer(0L)
  }

  list(
    col_vars = col_vars, w = w, col0 = col0, row0 = row0, last_row = r - 1L,
    col_of = col_of, group_var = group_var, stub_vars = stub_vars,
    body_vars = body_vars, group_as_col = group_as_col,
    spanners = spanners, n_levels = n_levels, level_row = level_row,
    title_row = title_row, subtitle_row = subtitle_row, label_row = label_row,
    body_row = body_row, group_head = group_head, group_span = group_span,
    summaries = acc$summaries, footnotes = fn, footnote_rows = footnote_rows,
    source_rows = source_rows
  )
}
