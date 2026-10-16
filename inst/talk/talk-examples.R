# gtxlsx — examples from the talk
# Every block runs on its own. Open the .xlsx after each one.

## install -----------------------------------------------------------------
# install.packages("gtxlsx")

library(gt)
library(openxlsx2)
library(gtxlsx)


## 1 minimal ---------------------------------------------------------------
# a gt table, and the same table in a worksheet

tbl <- gt(head(exibble, 4))

wb <- wb_workbook()$add_worksheet("minimal")
wb <- wb_add_gt(wb, tbl, dims = "B2")
# wb_open(wb)


## 2 one cell, every knob --------------------------------------------------
# fill, font, size, colour, alignment and a border on a single cell

tbl <- gt(data.frame(a = c("plain", "styled", "plain"), b = c(1, 2, 3))) |>
  tab_style(
    style = list(
      cell_fill(color = "#FFE9A8"),
      cell_text(color = "#7A3E00", weight = "bold", style = "italic",
                size = px(18), align = "center"),
      cell_borders(sides = "all", color = "#7A3E00", weight = px(2))
    ),
    locations = cells_body(columns = everything(), rows = 2)
  )

wb <- wb_workbook()$add_worksheet("one cell")
wb <- wb_add_gt(wb, tbl, dims = "B2")


## 3 merged cells ----------------------------------------------------------
# spanners become merged cells; each can carry its own fill

tbl <- gt(data.frame(item = c("north", "south"),
                     q1 = c(1200, 980), q2 = c(1350, 1010),
                     y1 = c(2550, 1990))) |>
  tab_spanner(label = "2024", columns = c(q1, q2)) |>
  tab_spanner(label = "total", columns = y1) |>
  tab_style(cell_fill(color = "#D9E8F5"),
            locations = cells_column_spanners(spanners = "2024")) |>
  tab_style(cell_fill(color = "#E8F5D9"),
            locations = cells_column_spanners(spanners = "total"))

wb <- wb_workbook()$add_worksheet("merged")
wb <- wb_add_gt(wb, tbl, dims = "B2")


## 4 number formats --------------------------------------------------------
# the cell holds the number; the format makes it look like gt
# (in 0.4.0 the date column is still written as text)

tbl <- gt(data.frame(
  money = c(1204.5, -700, 56),
  share = c(0.446, 0.352, 0.201),
  count = c(12000, 4300, 987),
  when  = as.Date(c("2026-01-31", "2026-06-30", "2026-12-24"))
)) |>
  fmt_currency(columns = money, currency = "EUR") |>
  fmt_percent(columns = share, decimals = 1) |>
  fmt_integer(columns = count) |>
  fmt_date(columns = when, date_style = "day_month_year")

wb <- wb_workbook()$add_worksheet("numbers")
wb <- wb_add_gt(wb, tbl, dims = "B2")

# money, share and count are numbers; when is text in 0.4.0
wb_to_df(wb, sheet = "numbers", dims = "B3:E5", col_names = FALSE)


## 5 censoring -------------------------------------------------------------
# sub_missing() and text_transform() both happen before gtxlsx sees the table,
# so a masked cell is masked in the sheet as well

salaries <- data.frame(
  team  = c("a", "b", "c", "d"),
  n     = c(14, 3, 22, 2),
  mean  = c(52000, 61000, 48500, 75000)
)

tbl <- gt(salaries) |>
  fmt_currency(columns = mean, currency = "EUR", decimals = 0) |>
  text_transform(
    locations = cells_body(columns = mean, rows = n < 5),
    fn = function(x) "&mdash;"
  ) |>
  tab_footnote("Suppressed where fewer than five observations.",
               locations = cells_column_labels(columns = mean))

wb <- wb_workbook()$add_worksheet("censored")
wb <- wb_add_gt(wb, tbl, dims = "B2")

# a column with one non-numeric cell is written as text as a whole,
# so the whole mean column is text here
wb_to_df(wb, sheet = "censored", dims = "D3:D6", col_names = FALSE)


## 6 the whole apparatus ---------------------------------------------------
# row groups, a stub, summary rows, footnotes, a theme

pizza <- data.frame(
  type = rep(c("classic", "veggie"), each = 3),
  size = rep(c("S", "M", "L"), 2),
  sold = c(6139, 4112, 4057, 2663, 3583, 5403)
)

tbl <- gt(pizza, rowname_col = "size", groupname_col = "type") |>
  fmt_integer(columns = sold) |>
  summary_rows(groups = everything(), columns = sold,
               fns = list(total = ~ sum(.x)), fmt = ~ fmt_integer(.x)) |>
  grand_summary_rows(columns = sold, fns = list(all = ~ sum(.x)),
                     fmt = ~ fmt_integer(.x)) |>
  tab_header(title = "pizzaplace", subtitle = "counts by type and size") |>
  tab_source_note("Source: the pizzaplace data set.") |>
  opt_stylize(style = 3, color = "blue") |>
  tab_options(table_body.hlines.style = "solid")

# in 0.4.0 the merged source note is not made tall enough and is cut off
wb <- wb_workbook()$add_worksheet("groups")
wb <- wb_add_gt(wb, tbl, dims = "B2")


## 7 several tables --------------------------------------------------------
# gt_group() and gt_split() are written one after another

grp <- gt_group(
  gt(head(exibble, 2)) |> tab_header("first"),
  gt(head(exibble, 3)) |> tab_header("second")
)

wb <- wb_workbook()$add_worksheet("group")
wb <- wb_add_gt(wb, grp, dims = "B2", gap = 2)


## 8 any html table --------------------------------------------------------
# no gt involved; wb_add_html() needs only openxlsx2

# needs knitr
html <- knitr::kable(head(mtcars[, 1:4]), format = "html")

wb <- wb_workbook()$add_worksheet("kable")
wb <- wb_add_html(wb, html, dims = "B2")

# with a stylesheet of your own
html <- paste0(
  "<style>",
  "  table { font-family: Georgia; }",
  "  th    { background-color: #204060; color: white; }",
  "  td    { border-bottom: 1px solid #ccc; }",
  "  .hot  { background-color: #FFD9D9; font-weight: bold; }",
  "</style>",
  "<table><tr><th>region</th><th>value</th></tr>",
  "<tr><td>north</td><td>1,204.50</td></tr>",
  "<tr><td class='hot'>south</td><td class='hot'>9,999.00</td></tr>",
  "</table>"
)

wb <- wb_workbook()$add_worksheet("styled html")
wb <- wb_add_html(wb, html, dims = "B2")


## 9 things to watch -------------------------------------------------------

# (a) the cell holds more precision than gt shows
tbl <- gt(data.frame(x = c(1/3, 2/3))) |> fmt_number(columns = x, decimals = 2)
wb  <- wb_add_gt(wb_workbook()$add_worksheet("precision"), tbl, dims = "B2")
wb_to_df(wb, sheet = "precision", dims = "B3:B4", col_names = FALSE)
# gt prints 0.33; the cell is 0.3333333333333333

# (b) gt rounds the way R does, Excel displays the way Excel does
gt(data.frame(v = c(0.5, 1.5, 2.5))) |> fmt_number(columns = v, decimals = 0)
round(c(0.5, 1.5, 2.5))
# gt shows 0, 2, 2 (half to even). The cells hold 0.5, 1.5, 2.5 with
# format "0", which Excel displays as 1, 2, 3.

# (c) a footnote mark is text inside the cell, so the whole column is text
tbl <- gt(data.frame(a = c(1, 2))) |>
  tab_footnote("note", locations = cells_body(columns = a, rows = 1))
wb  <- wb_add_gt(wb_workbook()$add_worksheet("footnote"), tbl, dims = "B2")
wb_to_df(wb, sheet = "footnote", dims = "B3:B4", col_names = FALSE)
# both cells are text; the marked one is "1 1" with a superscript run

## 10 a web table ----------------------------------------------------------
# links in the page become hyperlinks in the sheet

library(rvest)
currencies <- "https://en.wikipedia.org/wiki/List_of_circulating_currencies"
tabs <- currencies |>
  read_html() |>
  html_elements("table")

tab <- tabs[2]

wb <- wb_workbook() |>
  wb_add_worksheet() |>
  wb_add_html(x = tab, dims = "A1", freeze = TRUE)

if (interactive()) wb$open()


## 11 switches -------------------------------------------------------------
tbl <- gt(head(exibble, 4)) |> fmt_currency(columns = currency, currency = "EUR")

wb <- wb_workbook()$add_worksheet("values only")
wb <- wb_add_gt(wb, tbl, dims = "B2", features = FALSE)

wb <- wb_workbook()$add_worksheet("numfmt and merge")
wb <- wb_add_gt(wb, tbl, dims = "B2", features = c("numfmt", "merge"))

wb <- wb_workbook()$add_worksheet("frozen")
wb <- wb_add_gt(wb, tbl, dims = "B2", freeze = TRUE)


## 12 what gt hands over ---------------------------------------------------

tbl <- gt(data.frame(a = c("plain", "bold"), n = c(1.5, 2))) |>
  fmt_number(columns = n, decimals = 1) |>
  tab_footnote("note", cells_body(columns = a, rows = 1)) |>
  tab_style(cell_text(weight = "bold"), cells_body(columns = n, rows = 2))

built <- gt:::build_data(tbl, "html")
built[["_data"]]$n
built[["_body"]]$n
built[["_body"]]$a[1]
s <- as.data.frame(built[["_styles"]])
s[, c("locname", "colname", "rownum")]
