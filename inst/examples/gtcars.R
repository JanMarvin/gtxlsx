library(gt)
library(openxlsx2)
library(gtxlsx)

order_countries <- c("Germany", "Italy", "United States", "Japan")

best_mpg <- gtcars[which.max(gtcars$mpg_c), ]
best_hp  <- gtcars[which.max(gtcars$hp), ]

tab <- gtcars
tab <- tab[order(factor(tab$ctry_origin, levels = order_countries),
                 tab$mfr, -tab$msrp), ]
tab$car <- paste(tab$mfr, tab$model)
tab <- tab[setdiff(names(tab), c("mfr", "model"))]

gt_table <-
  gt(tab, rowname_col = "car", groupname_col = "ctry_origin") |>
  cols_hide(columns = c(drivetrain, bdy_style)) |>
  cols_move(columns = c(trsmn, mpg_c, mpg_h), after = trim) |>
  tab_spanner(label = "Performance",
              columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)) |>
  cols_merge(columns = c(mpg_c, mpg_h), pattern = "<<{1}c<br>{2}h>>") |>
  cols_merge(columns = c(hp, hp_rpm), pattern = "{1}<br>@{2}rpm") |>
  cols_merge(columns = c(trq, trq_rpm), pattern = "{1}<br>@{2}rpm") |>
  cols_label(mpg_c = "MPG", hp = "HP", trq = "Torque", year = "Year",
             trim = "Trim", trsmn = "Transmission", msrp = "MSRP") |>
  fmt_currency(columns = msrp, decimals = 0) |>
  cols_align(align = "center", columns = c(mpg_c, hp, trq)) |>
  tab_style(style = cell_text(size = px(12)),
            locations = cells_body(columns = c(trim, trsmn, mpg_c, hp, trq))) |>
  tab_header(title = md("The Cars of **gtcars**"),
             subtitle = "These are some fine automobiles") |>
  tab_source_note(source_note = md("Source: various pages of the Edmunds website.")) |>
  tab_footnote(footnote = md("Best gas mileage (city) of all the **gtcars**."),
               locations = cells_body(columns = mpg_c,
                                      rows = paste(best_mpg$mfr, best_mpg$model))) |>
  tab_footnote(footnote = md("The highest horsepower of all the **gtcars**."),
               locations = cells_body(columns = hp,
                                      rows = paste(best_hp$mfr, best_hp$model))) |>
  tab_footnote(footnote = "All prices in U.S. dollars (USD).",
               locations = cells_column_labels(columns = msrp))

wb <- wb_workbook() |>
  wb_add_worksheet(grid_lines = FALSE) |>
  wb_add_gt(x = gt_table, dims = "B2")

if (interactive()) wb$open()
