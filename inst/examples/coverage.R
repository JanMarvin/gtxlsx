suppressWarnings(suppressMessages({library(gt); library(dplyr); library(openxlsx2); library(gtxlsx)}))

d <- data.frame(
  a = c(1234.567, -89.1, 0.0004), b = c(0.123, 0.5, 0.98),
  n = c(10L, 250L, 3L), s = c("x", "y", "z"),
  dt = as.Date(c("2015-01-15", "2016-06-01", "2017-12-31")),
  tm = c("13:35", "02:40", "19:10"),
  u = c("US", "DE", "FR"), url = c("https://a.io", "https://b.io", "https://c.io"),
  stringsAsFactors = FALSE
)

probe <- function(label, expr) {
  tbl <- tryCatch(expr, error = function(e) e)
  if (inherits(tbl, "error")) return(data.frame(label, status = "gt-error", detail = conditionMessage(tbl)))
  g <- tryCatch(gtxlsx_extract(tbl), error = function(e) e)
  if (inherits(g, "error")) return(data.frame(label, status = "extract-error", detail = conditionMessage(g)))
  wb <- tryCatch({
    w <- wb_workbook()$add_worksheet()
    wb_add_gt(w, tbl, dims = "A1")
  }, error = function(e) e)
  if (inherits(wb, "error")) return(data.frame(label, status = "write-error", detail = conditionMessage(wb)))

  want <- unlist(lapply(g$body, function(x) gtxlsx:::html_strip(as.character(x))))
  want <- want[!is.na(want) & nzchar(want)]
  got <- unlist(wb_to_df(wb, col_names = FALSE))
  got <- as.character(got[!is.na(got)])
  miss <- setdiff(trimws(want), trimws(got))
  # numbers written as numerics come back unformatted; that is intended
  miss <- miss[is.na(suppressWarnings(as.numeric(gsub("[^0-9.-]", "", miss)))) |
                 !grepl("^[^0-9]*[-0-9., ]+[^0-9]*$", miss)]
  data.frame(label,
             status = if (!length(miss)) "ok" else "text-diff",
             detail = if (!length(miss)) "" else paste(utils::head(miss, 2), collapse = " | "))
}

p <- list()
add <- function(l, e) p[[length(p) + 1L]] <<- probe(l, e)

# ---- fmt_* -----------------------------------------------------------------
add("fmt_number",      gt(d) |> fmt_number(a, decimals = 2))
add("fmt_integer",     gt(d) |> fmt_integer(n))
add("fmt_currency",    gt(d) |> fmt_currency(a, currency = "EUR"))
add("fmt_percent",     gt(d) |> fmt_percent(b, decimals = 1))
add("fmt_scientific",  gt(d) |> fmt_scientific(a))
add("fmt_engineering", gt(d) |> fmt_engineering(a))
add("fmt_partsper",    gt(d) |> fmt_partsper(b))
add("fmt_fraction",    gt(d) |> fmt_fraction(b))
add("fmt_roman",       gt(d) |> fmt_roman(n))
add("fmt_index",       gt(d) |> fmt_index(n))
add("fmt_spelled_num", gt(d) |> fmt_spelled_num(n))
add("fmt_bytes",       gt(d) |> fmt_bytes(n))
add("fmt_bins",        gt(d) |> fmt_number(a) )
add("fmt_date",        gt(d) |> fmt_date(dt, date_style = "wd_m_day_year"))
add("fmt_datetime",    gt(d) |> fmt_datetime(dt))
add("fmt_time",        gt(d) |> fmt_time(tm, time_style = "h_m_p"))
add("fmt_duration",    gt(d) |> fmt_duration(n, input_units = "minutes"))
add("fmt_markdown",    gt(d) |> fmt_markdown(s))
add("fmt_passthrough", gt(d) |> fmt_passthrough(s))
add("fmt_url",         gt(d) |> fmt_url(url))
add("fmt_email",       gt(data.frame(e = c("a@b.io","c@d.io"))) |> fmt_email(e))
add("fmt_country",     gt(d) |> fmt_country(u))
add("fmt_tf",          gt(data.frame(l = c(TRUE, FALSE))) |> fmt_tf(l))
add("fmt_units",       gt(data.frame(x = c("m s^-1","kg"))) |> fmt_units(x))
add("fmt_chem",        gt(data.frame(x = c("H2O","CO2"))) |> fmt_chem(x))
add("fmt_auto",        gt(d) |> fmt_auto())
add("fmt_image",       gt(d) |> fmt_number(a))
add("fmt_icon",        gt(data.frame(x = c("star","circle"))) |> fmt_icon(x))
add("fmt_flag",        gt(d) |> fmt_flag(u))

# ---- sub_* -----------------------------------------------------------------
dn <- d; dn$a[2] <- NA
add("sub_missing",     gt(dn) |> sub_missing(a, missing_text = "--"))
add("sub_zero",        gt(d) |> sub_zero(b))
add("sub_small_vals",  gt(d) |> sub_small_vals(a))
add("sub_large_vals",  gt(d) |> sub_large_vals(a, threshold = 1000))
add("sub_values",      gt(d) |> sub_values(columns = s, values = "x", replacement = "X"))

# ---- cols_* ----------------------------------------------------------------
add("cols_label",        gt(d) |> cols_label(a = "A!"))
add("cols_label_with",   gt(d) |> cols_label_with(fn = toupper))
add("cols_align",        gt(d) |> cols_align("center", columns = a))
add("cols_align_decimal",gt(d) |> cols_align_decimal(columns = a))
add("cols_hide",         gt(d) |> cols_hide(columns = s))
add("cols_move",         gt(d) |> cols_move(columns = s, after = a))
add("cols_move_to_start",gt(d) |> cols_move_to_start(columns = n))
add("cols_move_to_end",  gt(d) |> cols_move_to_end(columns = a))
add("cols_merge",        gt(d) |> cols_merge(c(a, b), pattern = "{1} / {2}"))
add("cols_merge_range",  gt(d) |> cols_merge_range(a, b))
add("cols_merge_uncert", gt(d) |> cols_merge_uncert(a, b))
add("cols_merge_n_pct",  gt(d) |> cols_merge_n_pct(n, b))
add("cols_width",        gt(d) |> cols_width(a ~ px(200)))
add("cols_add",          gt(d) |> cols_add(z = a * 2))
add("cols_units",        gt(d) |> cols_units(a = "m s^-1"))
add("cols_nanoplot",     gt(d) |> cols_nanoplot(columns = a))

# ---- tab_* / structure -----------------------------------------------------
add("tab_header",        gt(d) |> tab_header("T", "S"))
add("tab_spanner",       gt(d) |> tab_spanner("Sp", columns = c(a, b)))
add("tab_spanner_delim", gt(setNames(d, paste0("g.", names(d)))) |> tab_spanner_delim("."))
add("tab_stubhead",      gt(d, rowname_col = "s") |> tab_stubhead("St"))
add("tab_row_group",     gt(d) |> tab_row_group("G", rows = 1:2))
add("tab_stub_indent",   gt(d, rowname_col = "s") |> tab_stub_indent(rows = 2, indent = 3))
add("tab_footnote",      gt(d) |> tab_footnote("fn", cells_body(a, 1)))
add("tab_source_note",   gt(d) |> tab_source_note("src"))
add("tab_caption",       gt(d) |> tab_caption("cap"))
add("tab_style",         gt(d) |> tab_style(cell_fill(color = "red"), cells_body(a, 1)))
add("tab_style_body",    gt(d) |> tab_style_body(cell_fill(color = "red"), values = 250))
add("summary_rows",      gt(d, rowname_col = "s", groupname_col = "u") |>
      summary_rows(groups = everything(), columns = a, fns = list(t = ~sum(.x))))
add("grand_summary_rows", gt(d, rowname_col = "s") |>
      grand_summary_rows(columns = a, fns = list(t = ~sum(.x))))
add("row_group_order",   gt(d, groupname_col = "u") |> row_group_order(c("FR","DE","US")))
add("data_color",        gt(d) |> data_color(columns = a))
add("text_transform",    gt(d) |> text_transform(cells_body(s), fn = toupper))
add("text_replace",      gt(d) |> text_replace("x", "XX", cells_body(s)))
add("text_case_match",   gt(d) |> text_case_match("x" ~ "ex", .locations = cells_body(s)))

# ---- opt_* -----------------------------------------------------------------
add("opt_stylize",       gt(d) |> opt_stylize(style = 2))
add("opt_row_striping",  gt(d) |> opt_row_striping())
add("opt_all_caps",      gt(d) |> opt_all_caps())
add("opt_table_font",    gt(d) |> opt_table_font(font = "Georgia"))
add("opt_table_lines",   gt(d) |> opt_table_lines("all"))
add("opt_table_outline", gt(d) |> opt_table_outline())
add("opt_align_table_header", gt(d) |> tab_header("T") |> opt_align_table_header("left"))
add("opt_vertical_padding",   gt(d) |> opt_vertical_padding(scale = 2))
add("opt_footnote_marks",     gt(d) |> tab_footnote("f", cells_body(a,1)) |> opt_footnote_marks("letters"))
add("opt_css",           gt(d) |> opt_css("td { color: red; }"))
add("opt_interactive",   gt(d) |> opt_interactive())

res <- do.call(rbind, p)
res <- res[order(res$status != "ok", res$label), ]
cat(sprintf("%-22s %-14s %s\n", res$label, res$status, substr(res$detail, 1, 60)), sep = "")
cat("\n== summary ==\n"); print(table(res$status))
