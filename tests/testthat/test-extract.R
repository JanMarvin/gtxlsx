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
  for (x in c("**b** and _i_", "a & b", "`code`", "sup^2^ and ~sub~", "*i*")) {
    expect_equal(keep(html_runs(render_md(gt::md(x)))),
                 keep(html_runs(as.character(pt(gt::md(x), context = "html")))),
                 info = x)
  }
})
