test_that("the built components come back as plain frames", {
  skip_no_gt()
  tbl <- gt::tab_header(small_gt(), title = "T", subtitle = "S")
  tbl <- gt::tab_spanner(tbl, label = "V", columns = c("n", "p"))
  tbl <- gt::tab_source_note(tbl, "src")
  g <- gtxlsx_extract(tbl)

  expect_true(all(c("body", "boxhead", "spanners", "heading", "options") %in% names(g)))
  expect_s3_class(g$body, "data.frame")
  expect_equal(g$heading$title, "T")
  expect_equal(g$source_notes, "src")
  expect_equal(nrow(g$spanners), 1L)
})

test_that("built strings are not escaped a second time", {
  skip_no_gt()
  tbl <- gt::tab_footnote(small_gt(), "usd",
                          locations = gt::cells_column_labels(columns = "n"))
  g <- gtxlsx_extract(tbl)
  lbl <- g$boxhead$column_label[[match("n", g$boxhead$var)]]
  expect_false(grepl("&lt;", lbl, fixed = TRUE))
})

test_that("an absent heading stays absent", {
  skip_no_gt()
  g <- gtxlsx_extract(small_gt())
  expect_null(g$heading$title)
})

test_that("theme values are derived from the options", {
  skip_no_gt()
  th <- gtxlsx_theme(gtxlsx_extract(small_gt())$options)
  expect_true(is.numeric(th$size))
  expect_false(identical(th$font, "system-ui"))
  expect_equal(th$color, "FF333333")
})

test_that("weight tests behave", {
  expect_true(is_bold("bold"))
  expect_true(is_bold("700"))
  expect_false(is_bold("normal"))
  expect_false(is_bold(NA))
})

test_that("footnote and source text is rendered without gt internals", {
  expect_equal(render_md("a & b"), "a &amp; b")
  expect_equal(render_md(gt::md("**bold**")), "<strong>bold</strong>")
  expect_equal(render_md(gt::html("<em>x</em>")), "<em>x</em>")
  expect_null(render_md(NULL))
})

test_that("the components gtxlsx relies on are all present", {
  skip_no_gt()
  built <- gt_build_data(gt::tab_source_note(small_gt(), "src"))
  needed <- c("_body", "_data", "_boxhead", "_stub_df", "_groups_rows",
              "_spanners", "_heading", "_stubhead", "_styles", "_footnotes",
              "_source_notes", "_summary_build", "_options", "_row_groups")
  expect_true(all(needed %in% names(built)))
  expect_true(all(c("var", "type", "column_label", "column_align") %in%
                    names(built$`_boxhead`)))
  expect_true(all(c("locname", "colname", "rownum", "styles") %in%
                    names(built$`_styles`)))
})

test_that("plain text is escaped exactly as gt escapes it", {
  skip_no_gt()
  pt <- utils::getFromNamespace("process_text", "gt")
  xs <- c("a & b", "1 < 2 > 0", "say \"hi\"", "it's", "5% of $10",
          "<b>not markup</b>", "100 &amp; more", "caf\u00e9 \u2014 \u00bd")
  for (x in xs) {
    expect_equal(render_md(x), as.character(pt(x, context = "html")), info = x)
  }
})

test_that("markdown renders the same runs gt would produce", {
  skip_no_gt()
  pt <- utils::getFromNamespace("process_text", "gt")
  keep <- function(r) {
    lapply(r, function(z) z[c("text", "bold", "italic", "strike", "vert_align")])
  }
  # gt swapped its markdown engine along the way; before that its runs differ
  # from ours, which is gt changing rather than this package drifting
  skip_if_gt_cannot(stopifnot(identical(
    keep(html_runs(render_md(gt::md("sup^2^")))),
    keep(html_runs(as.character(pt(gt::md("sup^2^"), context = "html"))))
  )))

  for (x in c("**b** and _i_", "a & b", "`code`", "sup^2^ and ~sub~", "*i*")) {
    expect_equal(keep(html_runs(render_md(gt::md(x)))),
                 keep(html_runs(as.character(pt(gt::md(x), context = "html")))),
                 info = x)
  }
})

test_that("a gt whose internals moved is refused, not guessed at", {
  skip_no_gt()
  msg <- tryCatch(check_built(list(`_body` = 1)), error = conditionMessage)
  expect_match(msg, "cannot read this version of 'gt'")
  expect_match(msg, "not a problem with your table")
  expect_match(msg, "github.com/JanMarvin/gtxlsx/issues", fixed = TRUE)
  # names what is missing, so the report says something useful
  expect_match(msg, "_boxhead")
})

test_that("the guard passes on tables with and without the optional parts", {
  skip_no_gt()
  # _source_notes only exists once a table has one, so it is not required
  expect_silent(gt_build_data(gt::gt(head(gt::exibble, 2))))
  expect_silent(
    gt_build_data(gt::tab_source_note(gt::gt(head(gt::exibble, 2)), "s"))
  )
})

test_that("a renamed column in a gt component is refused", {
  skip_no_gt()
  built <- gt_build_data(gt::gt(head(gt::exibble, 2)))
  names(built$`_boxhead`)[names(built$`_boxhead`) == "column_align"] <- "align"

  msg <- tryCatch(check_built(built), error = conditionMessage)
  expect_match(msg, "_boxhead has no column column_align")
  expect_match(msg, "not a problem with your table")
})

test_that("a style location gtxlsx does not know is reported", {
  skip_no_gt()
  tbl <- gt::tab_style(gt::gt(head(gt::exibble, 2)),
                       gt::cell_fill(color = "yellow"),
                       gt::cells_body(columns = "num", rows = 1))
  g <- gtxlsx_extract(tbl)
  g$styles$locname <- "cells_something_new"

  th <- gtxlsx_theme(g$options)
  p <- gtxlsx_plan(g, th, 1L, 1L)
  cc <- new_sheet_cells()

  expect_warning(gtxlsx_apply_styles(cc, g, th, p), "cells_something_new")
  expect_warning(gtxlsx_apply_styles(cc, g, th, p), "not one gtxlsx knows")
})

test_that("the locations gt uses today are all known", {
  skip_no_gt()
  used <- c("title", "subtitle", "stubhead", "columns_columns",
            "columns_groups", "row_groups", "data", "stub", "stub_column",
            "summary_cells", "grand_summary_cells", "footnotes",
            "source_notes")
  expect_true(all(used %in% known_locnames))
})

test_that("an option gt no longer has is reported, once", {
  skip_no_gt()
  ops <- gt::gt(head(gt::exibble, 2))$`_options`

  expect_warning(gtxlsx_theme(ops[ops$parameter != "table_font_size", ]),
                 "no option table_font_size")
  # the miss is cleared, so the next table starts fresh
  expect_no_warning(gtxlsx_theme(ops))
})

test_that("every option gtxlsx reads exists in this gt", {
  skip_no_gt()
  # the accessors record misses, so reading a whole theme is the check
  expect_no_warning(gtxlsx_theme(gt::gt(head(gt::exibble, 2))$`_options`))
})
