library(gt)
library(dplyr)
library(openxlsx2)
library(gtxlsx)

wb <- wb_workbook()

add <- function(wb, name, tbl, dims = "B2") {
  wb$add_worksheet(name, grid_lines = FALSE)
  wb_add_gt(wb, tbl, sheet = name, dims = dims)
}

# 1 -- stub, row groups, summary rows, grand summary, mixed formatters ---------
exibble_tbl <-
  exibble |>
  gt(rowname_col = "row", groupname_col = "group") |>
  fmt_number(columns = num, decimals = 2, use_seps = TRUE) |>
  fmt_currency(columns = currency, currency = "EUR") |>
  fmt_date(columns = date, date_style = "wd_m_day_year") |>
  fmt_time(columns = time, time_style = "h_m_p") |>
  sub_missing(columns = everything(), missing_text = "--") |>
  summary_rows(
    groups = everything(),
    columns = c(num, currency),
    fns = list(avg = ~ mean(.x, na.rm = TRUE), total = ~ sum(.x, na.rm = TRUE)),
    fmt = ~ fmt_number(.x, decimals = 1)
  ) |>
  grand_summary_rows(
    columns = c(num, currency),
    fns = list(`grand total` = ~ sum(.x, na.rm = TRUE)),
    fmt = ~ fmt_number(.x, decimals = 1)
  ) |>
  tab_header(title = md("**exibble**"), subtitle = "stub, groups and summaries") |>
  tab_stubhead(label = "row") |>
  tab_source_note("Summary rows are written as their own sheet rows.")

wb <- add(wb, "summaries", exibble_tbl)

# 2 -- data_color gradients, per-cell fills -----------------------------------
pops <-
  countrypops |>
  filter(country_code_2 %in% c("DE", "FR", "IT", "ES", "PL", "NL")) |>
  filter(year %in% seq(1980, 2020, 10)) |>
  select(country_name, year, population)
pops <- reshape(
  as.data.frame(pops), idvar = "country_name", timevar = "year",
  direction = "wide"
)
names(pops) <- sub("^population\\.", "y", names(pops))

color_tbl <-
  pops |>
  gt(rowname_col = "country_name") |>
  fmt_number(columns = starts_with("y"), decimals = 1, scale_by = 1 / 1e6) |>
  data_color(
    columns = starts_with("y"),
    palette = c("#FFF7EC", "#FC8D59", "#7F0000")
  ) |>
  cols_label_with(fn = function(x) sub("^y", "", x)) |>
  tab_spanner(label = "Population (millions)", columns = starts_with("y")) |>
  tab_header(title = "data_color()", subtitle = "continuous fills per cell") |>
  tab_stubhead(label = "Country")

wb <- add(wb, "colors", color_tbl)

# 3 -- merged columns: ranges, uncertainties, patterns ------------------------
merge_df <- data.frame(
  part = c("A-100", "A-200", "B-100", "B-200"),
  lo = c(1.2, 3.4, 0.8, 5.5),
  hi = c(2.8, 4.9, 1.6, 7.1),
  est = c(12.31, 9.04, 15.77, 6.42),
  err = c(0.41, 0.22, 1.05, 0.18),
  city = c("Berlin", "Hamburg", "Munich", "Cologne"),
  cc = c("DE", "DE", "DE", "DE"),
  stringsAsFactors = FALSE
)

merge_tbl <-
  merge_df |>
  gt(rowname_col = "part") |>
  fmt_number(columns = c(lo, hi), decimals = 1) |>
  fmt_number(columns = c(est, err), decimals = 2) |>
  cols_merge_range(col_begin = lo, col_end = hi) |>
  cols_merge_uncert(col_val = est, col_uncert = err) |>
  cols_merge(columns = c(city, cc), pattern = "{1} <em>({2})</em>") |>
  cols_label(lo = "Range", est = "Estimate", city = "Site") |>
  tab_header(title = "Merged columns", subtitle = md("`cols_merge_*()` variants")) |>
  tab_footnote("Uncertainty shown as a plus/minus range.",
               locations = cells_column_labels(columns = est))

wb <- add(wb, "merges", merge_tbl)

# 4 -- multi-level spanners via delimiter splitting ---------------------------
span_df <- data.frame(
  region = c("North", "South", "East", "West"),
  `2023.Q1.rev` = c(120.5, 98.2, 143.9, 87.4),
  `2023.Q1.cost` = c(80.1, 70.4, 95.2, 60.3),
  `2023.Q2.rev` = c(131.2, 104.8, 150.1, 91.0),
  `2023.Q2.cost` = c(84.7, 73.9, 99.8, 63.5),
  check.names = FALSE, stringsAsFactors = FALSE
)

span_tbl <-
  span_df |>
  gt(rowname_col = "region") |>
  tab_spanner_delim(delim = ".") |>
  fmt_currency(columns = everything(), currency = "USD", decimals = 1) |>
  tab_header(title = "Nested spanners", subtitle = "three header levels") |>
  tab_style(
    style = cell_fill(color = "#EDF2F7"),
    locations = cells_body(columns = ends_with("cost"))
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold", color = "#1A365D")),
    locations = cells_stub()
  )

wb <- add(wb, "spanners", span_tbl)

# 5 -- heavy theming: backgrounds, borders, striping, aligned text ------------
theme_tbl <-
  sp500 |>
  filter(date >= "2015-01-05", date <= "2015-01-16") |>
  select(-adj_close) |>
  gt() |>
  fmt_currency(columns = c(open, high, low, close)) |>
  fmt_date(columns = date, date_style = "day_m_year") |>
  fmt_number(columns = volume, suffixing = TRUE) |>
  tab_header(title = md("S&P 500 &mdash; *two weeks*"), subtitle = "themed output") |>
  tab_options(
    table.font.names = "Georgia",
    table.font.size = px(15),
    heading.background.color = "#1A365D",
    heading.title.font.size = px(22),
    column_labels.background.color = "#2C5282",
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#EBF4FF",
    table_body.hlines.color = "#BEE3F8",
    table.border.top.style = "double",
    table.border.bottom.style = "double"
  ) |>
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = list(cells_title(), cells_column_labels())
  ) |>
  tab_style(
    style = list(cell_fill(color = "#C6F6D5"), cell_text(weight = "bold")),
    locations = cells_body(columns = close, rows = close > open)
  ) |>
  tab_style(
    style = list(cell_fill(color = "#FED7D7"), cell_text(color = "#822727")),
    locations = cells_body(columns = close, rows = close <= open)
  ) |>
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#2C5282",
                         style = "double", weight = px(2)),
    locations = cells_body(rows = volume == max(volume))
  )

wb <- add(wb, "theming", theme_tbl)

# 6 -- pizzaplace: grouped counts with a stub and footnote marks --------------
pizza <-
  pizzaplace |>
  count(type, size, name = "n") |>
  group_by(type) |>
  mutate(share = n / sum(n)) |>
  ungroup()

pizza_tbl <-
  pizza |>
  gt(rowname_col = "size", groupname_col = "type") |>
  fmt_integer(columns = n) |>
  fmt_percent(columns = share, decimals = 1) |>
  cols_label(n = "Sold", share = "Share") |>
  summary_rows(
    groups = everything(), columns = n,
    fns = list(total = ~ sum(.x)), fmt = ~ fmt_integer(.x)
  ) |>
  tab_header(title = "pizzaplace", subtitle = "counts by type and size") |>
  tab_footnote("Share is within the pizza type.",
               locations = cells_column_labels(columns = share)) |>
  tab_footnote("Largest single category.",
               locations = cells_body(columns = n, rows = which.max(pizza$n))) |>
  opt_stylize(style = 3, color = "blue")

wb <- add(wb, "pizza", pizza_tbl)


# 7 -- units, chemistry and markdown: sub/superscript runs --------------------
units_df <- data.frame(
  quantity = c("Speed of sound", "Water", "Carbon dioxide", "Planck constant"),
  symbol = c("m s^-1", "H2O", "CO2", "J Hz^-1"),
  note = c("in **dry air** at 20 &deg;C", "_solvent_", "greenhouse gas", "exact since **2019**"),
  value = c(343.2, 18.015, 44.009, 6.62607015e-34),
  stringsAsFactors = FALSE
)

units_tbl <-
  units_df |>
  gt(rowname_col = "quantity") |>
  fmt_units(columns = symbol) |>
  fmt_markdown(columns = note) |>
  fmt_scientific(columns = value, decimals = 3) |>
  cols_units(value = "various") |>
  cols_label(symbol = "Units", note = "Note", value = "Value") |>
  tab_header(title = md("Units and `fmt_markdown()`"),
             subtitle = "superscripts, subscripts and inline emphasis") |>
  tab_stubhead(label = "Quantity")

wb <- add(wb, "units", units_tbl)

# 8 -- row groups rendered as their own column --------------------------------
groupcol_tbl <-
  gtcars |>
  dplyr::filter(ctry_origin %in% c("Germany", "Italy")) |>
  dplyr::select(ctry_origin, mfr, model, year, hp, msrp) |>
  dplyr::slice_head(n = 8, by = ctry_origin) |>
  gt(rowname_col = "model", groupname_col = "ctry_origin") |>
  fmt_integer(columns = c(year, hp)) |>
  fmt_currency(columns = msrp, decimals = 0) |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(title = "row_group.as_column", subtitle = "groups merged down a column") |>
  tab_stubhead(label = "Model") |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups())

wb <- add(wb, "groupcol", groupcol_tbl)

# 9 -- a spread of formatters on one table ------------------------------------
fmt_df <- data.frame(
  metric = c("alpha", "beta", "gamma", "delta"),
  num = c(1234.5678, -0.0004321, 0, 98765.4),
  pct = c(0.0123, 0.5, 0.987, NA),
  bytes = c(1024, 1048576, 5e9, 17),
  mins = c(90, 45, 3600, 7),
  idx = 1:4,
  when = as.Date(c("2024-01-05", "2024-06-30", "2025-02-14", "2025-12-01")),
  stringsAsFactors = FALSE
)

fmt_tbl <-
  fmt_df |>
  gt(rowname_col = "metric") |>
  fmt_number(columns = num, decimals = 2) |>
  fmt_percent(columns = pct, decimals = 1) |>
  fmt_bytes(columns = bytes) |>
  fmt_duration(columns = mins, input_units = "minutes") |>
  fmt_roman(columns = idx) |>
  fmt_date(columns = when, date_style = "day_m_year") |>
  sub_missing(columns = pct, missing_text = md("*n/a*")) |>
  sub_zero(columns = num, zero_text = "nil") |>
  cols_label(num = "Number", pct = "Percent", bytes = "Size",
             mins = "Duration", idx = "Rank", when = "Date") |>
  tab_header(title = "Formatter spread", subtitle = "one column per fmt_* family") |>
  tab_source_note(md("`fmt_bytes()` and `fmt_duration()` stay text; the rest become numbers."))

wb <- add(wb, "formatters", fmt_tbl)

# 10 -- indented stub with a hierarchy -----------------------------------------
indent_df <- data.frame(
  item = c("Revenue", "Product", "Services", "Costs", "COGS", "Opex", "Net"),
  level = c(0, 1, 1, 0, 1, 1, 0),
  q1 = c(1200, 800, 400, -700, -450, -250, 500),
  q2 = c(1350, 890, 460, -760, -480, -280, 590),
  stringsAsFactors = FALSE
)

indent_tbl <-
  indent_df |>
  dplyr::select(-level) |>
  gt(rowname_col = "item") |>
  fmt_currency(columns = c(q1, q2), decimals = 0) |>
  tab_stub_indent(rows = c(2, 3, 5, 6), indent = 4) |>
  cols_label(q1 = "Q1", q2 = "Q2") |>
  tab_spanner(label = "2024", columns = c(q1, q2)) |>
  tab_header(title = "Indented stub", subtitle = "tab_stub_indent()") |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(rows = c(1, 4, 7))) |>
  tab_style(style = cell_borders(sides = "top", weight = px(2)),
            locations = cells_body(rows = 7)) |>
  tab_footnote("Indented rows roll up into the line above.",
               locations = cells_stub(rows = 2))

wb <- add(wb, "indent", indent_tbl)

# 11 -- the same table taken through as_raw_html() and wb_add_html() ----------
html_tbl <-
  units_tbl |>
  tab_header(title = "Same table via HTML", subtitle = "wb_add_html() on as_raw_html()")

wb$add_worksheet("html", grid_lines = FALSE)
wb <- wb_add_html(wb, as.character(as_raw_html(html_tbl, inline_css = FALSE)),
                  sheet = "html", dims = "B2")

# 12 -- an lt table, baked to static HTML and written the same way ------------
if (requireNamespace("lt", quietly = TRUE) && nzchar(Sys.which("node"))) {
  lt_df <- data.frame(
    item = c("alpha", "beta", "gamma", "delta"),
    grp = c("A", "A", "B", "B"),
    n = c(1234.5, 67.89, 0.5, 98765),
    p = c(0.12, 0.5, 0.98, 0.03),
    stringsAsFactors = FALSE
  )
  lt_tbl <-
    lt::lt(lt_df, rowname = "item", groupname = "grp") |>
    lt::lt_header("an lt table", "baked with lt_export(method = \"node\")") |>
    lt::lt_spanner("Values", c("n", "p")) |>
    lt::lt_format(~ n, decimals = 2, big_mark = ",") |>
    lt::lt_format(~ p, percent = TRUE, decimals = 1) |>
    lt::lt_style(columns = "n", rows = 4, bold = TRUE, color = "#b00020") |>
    lt::lt_note("written through wb_add_lt()")

  wb$add_worksheet("lt", grid_lines = FALSE)
  wb <- wb_add_lt(wb, lt_tbl, sheet = "lt", dims = "B2")
}


if (interactive()) wb$open() else wb$save("gtxlsx-gallery.xlsx")
