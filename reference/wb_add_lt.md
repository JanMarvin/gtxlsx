# Write an lt table into a worksheet

The \`lt\` package builds its HTML in JavaScript when the page is
viewed, so there is no table to read on the R side. This helper asks
\`lt\` to bake the table to static HTML first and then hands the result
to \[wb_add_html()\].

## Usage

``` r
wb_add_lt(wb, x, sheet = current_sheet(), dims = "A1", method = "auto", ...)
```

## Arguments

- wb:

  A \`wbWorkbook\` object.

- x:

  An \`lt_tbl\` object.

- sheet:

  The worksheet to write to.

- dims:

  Cell reference of the top left corner.

- method:

  How \`lt\` should bake the table: \`"node"\` or \`"browser"\` to force
  a renderer, \`"auto"\` to use whichever is available.

- ...:

  Passed on to \[wb_add_html()\].

## Value

The workbook, invisibly.

## Details

Baking needs Node.js or a Chromium based browser on the machine.

## Examples

``` r
# needs the lt package and a Node.js or browser install
if (FALSE) { # \dontrun{
library(openxlsx2)

tbl <- lt::lt(data.frame(a = c("x", "y"), n = c(1234.5, 67.89)))
tbl <- lt::lt_format(tbl, ~ n, decimals = 2, big_mark = ",")

wb <- wb_workbook()$add_worksheet()
wb <- wb_add_lt(wb, tbl, dims = "B2")
} # }
```
