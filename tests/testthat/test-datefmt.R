date_cc <- function(wb) wb$worksheets[[1L]]$sheet_data$cc

test_that("every whole-day style is written as a date with its own format", {
  skip_if_not_installed("gt")
  x <- data.frame(d = as.Date(c("2026-01-31", "2026-06-05")))
  want <- c("yyyy-mm-dd", "dddd, mmmm d, yyyy", "ddd, mmm d, yyyy",
            "dddd d mmmm yyyy", "mmmm d, yyyy", "mmm d, yyyy", "d mmm yyyy",
            "d mmmm yyyy")
  for (st in 1:8) {
    g <- gt::fmt_date(gt::gt(x), d, date_style = st)
    wb <- wb_add_gt(openxlsx2::wb_workbook()$add_worksheet("t"), g)
    cc <- date_cc(wb)
    expect_equal(as.numeric(cc$v[cc$r == "A2"]), 46053)
    expect_equal(as.numeric(cc$v[cc$r == "A3"]), 46178)
    expect_match(unlist(wb$styles_mgr$styles$numFmts),
                 paste0("formatCode=\"", want[st], "\""), all = FALSE,
                 fixed = TRUE)
  }
})

test_that("times, character dates and text that is no date", {
  skip_if_not_installed("gt")
  x <- data.frame(
    t = as.POSIXct(c("2026-06-30 14:05:09", "2026-01-02 00:00:00"), tz = "UTC"),
    c = c("2026-01-31", "2026-06-05"),
    s = c("a", "b")
  )
  g <- gt::gt(x) |>
    gt::fmt_datetime(t, date_style = 5, time_style = 4) |>
    gt::fmt_date(c, date_style = 8)
  cc <- date_cc(wb_add_gt(openxlsx2::wb_workbook()$add_worksheet("t"), g))
  expect_equal(as.numeric(cc$v[cc$r == "A2"]), 46203 + 14.1525 / 24,
               tolerance = 1e-6)
  expect_equal(as.numeric(cc$v[cc$r == "A3"]), 46024)
  expect_equal(as.numeric(cc$v[cc$r == "B2"]), 46053)
  expect_equal(cc$c_t[cc$r == "C2"], "inlineStr")
})

test_that("a date text gt did not produce from the value stays text", {
  expect_null(infer_datefmt("31 January 2026 ", as.Date("2026-02-01")))
  expect_null(infer_datefmt("31 Januar 2026", as.Date("2026-01-31")))
  expect_equal(infer_datefmt("2026-01-31", "2026-01-31")$numfmt, "yyyy-mm-dd")
  expect_null(infer_datefmt("2026-01-31", "not a date"))
  expect_null(infer_datefmt("2026-01-31", 5))
})

test_that("dates in html are read only when a style reproduces the text", {
  v <- function(x) text_as_dates(x)$value
  expect_equal(v(c("31 January 2026", "2026-06-05", "Sat, Jan 31, 2026")),
               c(46053, 46178, 46053))
  expect_equal(v("January 31, 2026 2:05 PM"), 46053 + 14.0833333 / 24,
               tolerance = 1e-6)
  expect_true(is.na(v("Fri, Jan 31, 2026")))
  expect_true(is.na(v("31 Feb 2026")))
  expect_true(is.na(v("2026")))
  expect_true(is.na(v("31")))
  expect_true(is.na(v("5 June")))
  expect_equal(text_as_dates("31 January 2026")$numfmt, "d mmmm yyyy")
})

test_that("convert_date = FALSE keeps the printed text", {
  skip_if_not_installed("gt")
  g <- gt::fmt_date(gt::gt(data.frame(d = as.Date("2026-01-31"),
                                      n = 1.5)), d, date_style = 8)
  cc <- function(...) {
    wb <- openxlsx2::wb_workbook()$add_worksheet("t")
    wb_add_gt(wb, g, ...)$worksheets[[1L]]$sheet_data$cc
  }
  on <- cc()
  expect_equal(on$c_t[on$r == "A2"], "")
  off <- cc(convert_date = FALSE)
  expect_equal(off$c_t[off$r == "A2"], "inlineStr")
  expect_equal(off$c_t[off$r == "B2"], "")
  no_num <- cc(numeric = FALSE)
  expect_equal(no_num$c_t[no_num$r == "A2"], "")
  expect_equal(no_num$c_t[no_num$r == "B2"], "inlineStr")
})

test_that("a gt_group passes the conversion switches on", {
  skip_if_not_installed("gt")
  skip_if_not("gt_group" %in% getNamespaceExports("gt"))
  grp <- gt::gt_group(
    gt::fmt_date(gt::gt(data.frame(d = as.Date("2026-01-31"))), d)
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet("t")
  cc <- wb_add_gt(wb, grp, convert_date = FALSE)$worksheets[[1L]]$sheet_data$cc
  expect_equal(cc$c_t[cc$r == "A2"], "inlineStr")
})
