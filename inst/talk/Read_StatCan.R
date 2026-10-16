
library(chromote)
library(rvest)
library(openxlsx2)
library(gtxlsx)

session <- read_html_live("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901")
Sys.sleep(3)

tab <- session |>
    html_elements("table")

wb <- wb_workbook() |>
  wb_add_worksheet() |>
  wb_add_html(x = tab, dims = "A1", freeze = TRUE)

if (interactive()) wb$open()
